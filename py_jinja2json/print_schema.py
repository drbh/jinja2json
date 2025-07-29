import sys
import json
import re
from pathlib import Path
from collections import OrderedDict

# Add the py_jinja2json module to path (it's in the src subdirectory)
sys.path.insert(0, str(Path(__file__).parent / "src"))

try:
    import py_jinja2json as j2j
except ImportError:
    sys.stderr.write("Error: py_jinja2json module not found. Please build the Python bindings first.\n")
    sys.stderr.write("Run: cd py_jinja2json && maturin develop\n")
    sys.exit(1)


class VariableSchema:
    """Structure to hold all variables found in a template (matches Rust version)."""
    def __init__(self):
        self.top_level_variables = set()
        self.nested_expressions = set()
        self.full_expressions = set()
        self.discovered_values = {}


class LoopContext:
    """Context for tracking loop variables and their relationships (matches Rust version)."""
    def __init__(self):
        self.loop_var_to_array = {}
        self.set_variables = set()
    
    def add_loop_mapping(self, loop_var, array_var):
        self.loop_var_to_array[loop_var] = array_var
    
    def get_array_for_loop_var(self, loop_var):
        return self.loop_var_to_array.get(loop_var)
    
    def add_set_variable(self, var_name):
        self.set_variables.add(var_name)
    
    def is_set_variable(self, var_name):
        return var_name in self.set_variables


def extract_variable_schema(template_analysis):
    """Extract all variables and expressions from a Jinja2 template (main entry point)."""
    schema = VariableSchema()
    loop_context = LoopContext()
    
    # First pass: collect all set variables
    collect_set_variables(template_analysis["instructions"], loop_context)
    
    # Second pass: analyze expressions knowing which variables are set variables
    analyze_stmt(template_analysis["instructions"], schema, loop_context)
    
    # Build nested structure from the collected variables
    return build_nested_structure(schema.nested_expressions, schema.top_level_variables, schema.discovered_values)


def collect_set_variables(stmt, loop_context):
    """First pass: collect all set variables to distinguish them from input variables."""
    stmt_type = stmt.get("type", "")
    
    if stmt_type == "start_template":
        for child in stmt.get("children", []):
            collect_set_variables(child, loop_context)
    elif stmt_type == "if_condition":
        for s in stmt.get("true_branch", []) + stmt.get("false_branch", []):
            collect_set_variables(s, loop_context)
    elif stmt_type == "for_loop":
        for s in stmt.get("body", []):
            collect_set_variables(s, loop_context)
    elif stmt_type == "set_variable":
        # Track the variable being set as a local variable
        target = stmt.get("target", "")
        match = re.search(r'id: "([^"]+)"', target)
        if match:
            loop_context.add_set_variable(match.group(1))


def analyze_stmt(stmt, schema, loop_context):
    """Recursively analyze AST statements to find variables."""
    stmt_type = stmt.get("type", "")
    
    if stmt_type == "start_template":
        for child in stmt.get("children", []):
            analyze_stmt(child, schema, loop_context)
    
    elif stmt_type == "emit_expr":
        expr_str = stmt.get("expression", "")
        schema.full_expressions.add(expr_str)
        extract_variables_from_expr(expr_str, stmt.get("variable_info", {}), schema, loop_context)
    
    elif stmt_type == "if_condition":
        condition_str = stmt.get("condition", "")
        schema.full_expressions.add(condition_str)
        extract_variables_from_expr(condition_str, stmt.get("condition_info", {}), schema, loop_context)
        
        for s in stmt.get("true_branch", []):
            analyze_stmt(s, schema, loop_context)
        for s in stmt.get("false_branch", []):
            analyze_stmt(s, schema, loop_context)
    
    elif stmt_type == "for_loop":
        iterator_str = stmt.get("iterator", "")
        schema.full_expressions.add(iterator_str)
        iterator_info = stmt.get("iterator_info", {})
        
        # Extract loop variable name
        target = stmt.get("target", "")
        loop_var_match = re.search(r'id: "([^"]+)"', target)
        loop_var_name = loop_var_match.group(1) if loop_var_match else "item"
        
        # Handle different iterator patterns
        if match := re.match(r'var\((\w+)\)\.(\w+)', iterator_str):
            # Handle patterns like var(message).tool_calls (nested case)
            parent_var = match.group(1)
            attr_name = match.group(2)
            
            # Nested case: for item in parent.array (e.g., for tool_call in message.tool_calls)
            full_path = build_variable_path(parent_var, attr_name, loop_context)
            loop_context.add_loop_mapping(loop_var_name, full_path)
            schema.nested_expressions.add(f"{full_path}._item_structure")
            
        elif match := re.match(r'var\((\w+)\)\[const\("(\w+)"\)\]', iterator_str):
            # Handle patterns like var(message)["tool_calls"] (item access case)
            parent_var = match.group(1)
            attr_name = match.group(2)
            
            # Nested case: for item in parent[array] (e.g., for tool in message["tool_calls"])
            full_path = build_variable_path(parent_var, attr_name, loop_context)
            loop_context.add_loop_mapping(loop_var_name, full_path)
            schema.nested_expressions.add(f"{full_path}._item_structure")
            # Also add the array property itself so it appears in the parent structure
            schema.nested_expressions.add(full_path)
            
        elif iterator_info.get("variable_name") and not iterator_info.get("attributes"):
            # Simple case: for item in array (only if no attributes)
            array_var = iterator_info["variable_name"]
            loop_context.add_loop_mapping(loop_var_name, array_var)
            schema.nested_expressions.add(f"{array_var}._item_structure")
            
            # Only add to top level if not a loop var itself
            if not loop_context.get_array_for_loop_var(array_var) and not loop_context.is_set_variable(array_var):
                schema.top_level_variables.add(array_var)
        
        # Analyze loop body
        for s in stmt.get("body", []):
            analyze_stmt(s, schema, loop_context)
    
    elif stmt_type == "set_variable":
        expr_str = describe_expression(stmt.get("expression_info", {}), stmt.get("expression", ""))
        schema.full_expressions.add(expr_str)
        # Only analyze the right-hand side expression for actual input variables
        extract_variables_from_expr(stmt.get("expression", ""), stmt.get("expression_info", {}), schema, loop_context)


def extract_variables_from_expr(expr_str, var_info, schema, loop_context):
    """Extract variables from an expression and capture constant values."""
    # First check variable_info
    if var_info.get("variable_name"):
        var_name = var_info["variable_name"]
        # Check if this is a loop variable or set variable - if so, don't add as top-level
        if loop_context.get_array_for_loop_var(var_name) is None and not loop_context.is_set_variable(var_name):
            schema.top_level_variables.add(var_name)
        
        # Add attributes - build full attribute path (but skip if base variable is a set variable)
        attributes = var_info.get("attributes", [])
        if attributes and not loop_context.is_set_variable(var_name):
            attr_path = ".".join(attributes)
            if array_name := loop_context.get_array_for_loop_var(var_name):
                schema.nested_expressions.add(f"{array_name}.{attr_path}")
            else:
                schema.nested_expressions.add(f"{var_name}.{attr_path}")
    
    # Parse expression string for additional patterns
    # Pattern: var(name)
    for match in re.finditer(r'var\((\w+)\)(?![.\[])', expr_str):
        var_name = match.group(1)
        if loop_context.get_array_for_loop_var(var_name) is None and not loop_context.is_set_variable(var_name):
            schema.top_level_variables.add(var_name)
    
    # Pattern: var(name)[const("field")]
    for match in re.finditer(r'var\((\w+)\)\[const\("(\w+)"\)\]', expr_str):
        var_name = match.group(1)
        field_name = match.group(2)
        full_path = build_getitem_path(var_name, field_name, loop_context)
        if full_path:
            schema.nested_expressions.add(full_path)
    
    # Pattern: var(name).field (but skip if base variable is a set variable)
    for match in re.finditer(r'var\((\w+)\)\.(\w+)', expr_str):
        var_name = match.group(1)
        field_name = match.group(2)
        if not loop_context.is_set_variable(var_name):
            full_path = build_variable_path(var_name, field_name, loop_context)
            schema.nested_expressions.add(full_path)
    
    # Capture comparison values
    capture_comparison_values(expr_str, schema, loop_context)


def build_variable_path(var_name, attr_name, loop_context):
    """Build a full variable path like 'message.role' or 'user.profile.name'."""
    # If this is a loop variable, map it to the array structure
    if array_name := loop_context.get_array_for_loop_var(var_name):
        return f"{array_name}.{attr_name}"
    else:
        return f"{var_name}.{attr_name}"


def build_getitem_path(var_name, key, loop_context):
    """Build a full variable path for item access like 'message.role' from message['role']."""
    # If this is a loop variable, map it to the array structure
    if array_name := loop_context.get_array_for_loop_var(var_name):
        return f"{array_name}.{key}"
    else:
        return f"{var_name}.{key}"


def describe_expression(var_info, expr_str):
    """Describe an expression in detail showing the structure."""
    if var_info:
        expr_type = var_info.get("expr_type", "")
        if expr_type == "variable" and var_info.get("variable_name"):
            return f"var({var_info['variable_name']})"
    return expr_str


def capture_comparison_values(expr_str, schema, loop_context):
    """Capture values from comparisons like var == 'value'."""
    # Pattern: var(name).field == const("value")
    for match in re.finditer(r'var\((\w+)\)\.(\w+)\s*==\s*const\("([^"]+)"\)', expr_str):
        var_name, field_name, value = match.groups()
        var_path = build_variable_path(var_name, field_name, loop_context)
        schema.discovered_values[var_path] = value
    
    # Pattern: var(name)[const("field")] == const("value")
    for match in re.finditer(r'var\((\w+)\)\[const\("(\w+)"\)\]\s*==\s*const\("([^"]+)"\)', expr_str):
        var_name, field_name, value = match.groups()
        var_path = build_getitem_path(var_name, field_name, loop_context)
        if var_path:
            schema.discovered_values[var_path] = value


def build_nested_structure(nested_expressions, top_level_variables, discovered_values):
    """Build a nested JSON structure from variable paths (matches Rust build_nested_structure)."""
    root = {}
    
    # Identify which variables are arrays (have _item_structure markers)
    array_vars = set()
    item_structures = {}
    
    # Process expressions in single pass to match Rust logic exactly (sorted for determinism) 
    for expr in sorted(nested_expressions):
        if expr.endswith("._item_structure"):
            var_name = expr.replace("._item_structure", "")
            array_vars.add(var_name)
        else:
            # Check if this is a nested expression from a loop variable
            parts = expr.split('.')
            if len(parts) >= 2:
                # Find the longest matching array variable prefix
                matched_array_var = None
                remaining_parts = []
                
                for i in range(len(parts), 0, -1):
                    potential_array = '.'.join(parts[:i])
                    if potential_array in array_vars:
                        matched_array_var = potential_array
                        remaining_parts = parts[i:]
                        break
                
                # If no nested array match, try top-level variables
                if matched_array_var is None:
                    base_var = parts[0]
                    if base_var in array_vars or base_var in top_level_variables:
                        matched_array_var = base_var
                        remaining_parts = parts[1:]
                
                if matched_array_var and remaining_parts:
                    remaining_path = '.'.join(remaining_parts)
                    if matched_array_var not in item_structures:
                        item_structures[matched_array_var] = set()
                    item_structures[matched_array_var].add(remaining_path)
    
    # First, add all top-level variables
    for var in sorted(top_level_variables):
        if var in array_vars:
            # This is an array - create array with example item
            item_props = item_structures.get(var, set())
            if item_props:
                example_item = {}
                
                # Build nested structure for array items
                # Filter out properties that are prefixes of other properties
                filtered_props = set()
                for prop in sorted(item_props):
                    is_prefix = False
                    for other_prop in item_props:
                        if other_prop != prop and other_prop.startswith(f"{prop}."):
                            is_prefix = True
                            break
                    if not is_prefix:
                        filtered_props.add(prop)
                
                for prop in sorted(filtered_props):
                    full_path = f"{var}.{prop}"
                    
                    if full_path in array_vars:
                        # This is a nested array, create an array structure
                        nested_item_props = item_structures.get(full_path, set())
                        if nested_item_props:
                            nested_example_item = {}
                            
                            # Apply the same prefix filtering for nested arrays
                            filtered_nested_props = set()
                            for nested_prop in sorted(nested_item_props):
                                is_prefix = False
                                for other_nested_prop in nested_item_props:
                                    if other_nested_prop != nested_prop and other_nested_prop.startswith(f"{nested_prop}."):
                                        is_prefix = True
                                        break
                                if not is_prefix:
                                    filtered_nested_props.add(nested_prop)
                            
                            for nested_prop in sorted(filtered_nested_props):
                                nested_full_path = f"{full_path}.{nested_prop}"
                                discovered_val = discovered_values.get(nested_full_path, "example_value")
                                # Filter out values that contain unicode or special characters
                                if (discovered_val in ["\\n", "\n", "\\t", "\t", "\\r", "\r"] or 
                                    any(ord(c) > 127 for c in str(discovered_val)) or
                                    "<" in str(discovered_val) and ">" in str(discovered_val)):
                                    nested_value = "example_value"
                                else:
                                    nested_value = discovered_val
                                insert_property_path(nested_example_item, nested_prop, nested_value)
                            
                            value = [nested_example_item]
                        else:
                            value = ["example_value"]
                    else:
                        discovered_val = discovered_values.get(full_path, "example_value")
                        # Filter out values that are just whitespace/control characters or contain unicode
                        if (discovered_val in ["\\n", "\n", "\\t", "\t", "\\r", "\r"] or 
                            any(ord(c) > 127 for c in str(discovered_val)) or
                            "<" in str(discovered_val) and ">" in str(discovered_val)):
                            value = "example_value"
                        else:
                            value = discovered_val
                    
                    # Insert this property into the example item
                    insert_property_path(example_item, prop, value)
                
                root[var] = [example_item]
            else:
                root[var] = ["example_value"]
        elif not any(expr.startswith(f"{var}.") for expr in nested_expressions):
            # Regular variable
            value = discovered_values.get(var, "example_value")
            root[var] = value
    
    # Then build nested structures from the remaining expressions (non-array items)
    for expr in nested_expressions:
        if not expr.endswith("._item_structure"):
            parts = expr.split('.')
            if len(parts) >= 2 and parts[0] not in array_vars:
                insert_nested_path(root, parts, 0, discovered_values)
    
    return root


def insert_property_path(obj, property_path, value):
    """Insert a property path into an object (e.g., 'function.name' -> value)."""
    parts = property_path.split('.')
    insert_property_path_recursive(obj, parts, 0, value)


def insert_property_path_recursive(obj, parts, index, value):
    """Recursive helper for insert_property_path."""
    if index >= len(parts):
        return
    
    part = parts[index]
    
    if index == len(parts) - 1:
        # Last part - insert the value
        obj[part] = value
    else:
        # Intermediate part - ensure there's an object to recurse into
        if part not in obj:
            obj[part] = {}
        if isinstance(obj[part], dict):
            insert_property_path_recursive(obj[part], parts, index + 1, value)


def insert_nested_path(obj, parts, index, discovered_values):
    """Insert a nested path into the JSON structure."""
    if index >= len(parts):
        return
    
    key = parts[index]
    
    if index == len(parts) - 1:
        # Last part - insert the actual value
        full_path = '.'.join(parts)
        value = discovered_values.get(full_path, "example_value")
        obj[key] = value
    else:
        # Intermediate part - ensure there's an object to recurse into
        if key not in obj:
            obj[key] = {}
        
        if isinstance(obj[key], dict):
            insert_nested_path(obj[key], parts, index + 1, discovered_values)


def print_schema(template_path):
    """Analyze a template file and print its variable schema."""
    template_path = Path(template_path)
    
    if not template_path.exists():
        sys.stderr.write(f"Error: Template file '{template_path}' not found.\n")
        return False
    
    try:
        analysis = j2j.analyze_template_file(str(template_path))
        schema = extract_variable_schema(analysis)
        print(json.dumps(schema, indent=2, sort_keys=True))
        return True
        
    except Exception as e:
        sys.stderr.write(f"Error analyzing template: {e}\n")
        return False


def main():
    if len(sys.argv) != 2:
        sys.stderr.write("Usage: python print_schema.py <template_file>\n")
        sys.exit(1)
    
    template_file = sys.argv[1]
    success = print_schema(template_file)
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()