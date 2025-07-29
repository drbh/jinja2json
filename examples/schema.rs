// use jinja2json::{analyze_template_instructions};
use minijinja::machinery::{ast, parse};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::fs;

/// Structure to hold all variables found in a template
#[derive(Debug, Serialize, Deserialize)]
pub struct VariableSchema {
    /// Top-level variable names (e.g., "message", "user", "config")
    pub top_level_variables: BTreeSet<String>,
    /// All nested variable expressions (e.g., "message.role", "user.profile.name")
    pub nested_expressions: BTreeSet<String>,
    /// All condition/expression strings showing full nesting
    pub full_expressions: BTreeSet<String>,
    /// Values found in template (variable_path -> value)
    pub discovered_values: BTreeMap<String, Value>,
    /// Nested JSON structure representing the expected input data
    pub nested_structure: Value,
}

/// Context for tracking loop variables and their relationships
#[derive(Debug)]
struct LoopContext {
    /// Maps loop variable names to their array sources (e.g., "message" -> "messages")
    loop_var_to_array: BTreeMap<String, String>,
    /// Set of variables that are created by set statements (local variables)
    set_variables: BTreeSet<String>,
}

impl LoopContext {
    fn new() -> Self {
        Self {
            loop_var_to_array: BTreeMap::new(),
            set_variables: BTreeSet::new(),
        }
    }

    fn add_loop_mapping(&mut self, loop_var: &str, array_var: &str) {
        self.loop_var_to_array
            .insert(loop_var.to_string(), array_var.to_string());
    }

    fn get_array_for_loop_var(&self, loop_var: &str) -> Option<&String> {
        self.loop_var_to_array.get(loop_var)
    }

    fn add_set_variable(&mut self, var_name: &str) {
        self.set_variables.insert(var_name.to_string());
    }

    fn is_set_variable(&self, var_name: &str) -> bool {
        self.set_variables.contains(var_name)
    }
}

/// Extract all variables and expressions from a Jinja2 template
pub fn extract_variable_schema(
    template_source: &str,
) -> Result<VariableSchema, Box<dyn std::error::Error>> {
    let ast = parse(
        template_source,
        "schema_analysis",
        Default::default(),
        Default::default(),
    )?;

    let mut schema = VariableSchema {
        top_level_variables: BTreeSet::new(),
        nested_expressions: BTreeSet::new(),
        full_expressions: BTreeSet::new(),
        discovered_values: BTreeMap::new(),
        nested_structure: Value::Null,
    };

    // Track loop variables to understand their relationship to arrays
    let mut loop_context = LoopContext::new();

    // First pass: collect all set variables
    collect_set_variables(&ast, &mut loop_context);

    // Second pass: analyze expressions knowing which variables are set variables
    analyze_stmt(&ast, &mut schema, &mut loop_context);

    // Build nested structure from the collected variables and discovered values
    schema.nested_structure = build_nested_structure(
        &schema.nested_expressions,
        &schema.top_level_variables,
        &schema.discovered_values,
    );

    Ok(schema)
}

/// First pass: collect all set variables to distinguish them from input variables
fn collect_set_variables(stmt: &ast::Stmt, loop_context: &mut LoopContext) {
    match stmt {
        ast::Stmt::Template(template) => {
            for child in &template.children {
                collect_set_variables(child, loop_context);
            }
        }
        ast::Stmt::IfCond(if_stmt) => {
            for stmt in &if_stmt.true_body {
                collect_set_variables(stmt, loop_context);
            }
            for stmt in &if_stmt.false_body {
                collect_set_variables(stmt, loop_context);
            }
        }
        ast::Stmt::ForLoop(for_stmt) => {
            for stmt in &for_stmt.body {
                collect_set_variables(stmt, loop_context);
            }
        }
        ast::Stmt::Set(set_stmt) => {
            // Track the variable being set as a local variable
            if let ast::Expr::Var(var) = &set_stmt.target {
                loop_context.add_set_variable(&var.id);
            }
        }
        _ => {}
    }
}

/// Recursively analyze AST statements to find variables
fn analyze_stmt(stmt: &ast::Stmt, schema: &mut VariableSchema, loop_context: &mut LoopContext) {
    match stmt {
        ast::Stmt::Template(template) => {
            for child in &template.children {
                analyze_stmt(child, schema, loop_context);
            }
        }
        ast::Stmt::EmitExpr(emit) => {
            let expr_str = describe_expression_detailed(&emit.expr);
            schema.full_expressions.insert(expr_str);
            extract_variables_from_expr(&emit.expr, schema, loop_context);
        }
        ast::Stmt::IfCond(if_stmt) => {
            let condition_str = describe_expression_detailed(&if_stmt.expr);
            schema.full_expressions.insert(condition_str);
            extract_variables_from_expr(&if_stmt.expr, schema, loop_context);

            for stmt in &if_stmt.true_body {
                analyze_stmt(stmt, schema, loop_context);
            }
            for stmt in &if_stmt.false_body {
                analyze_stmt(stmt, schema, loop_context);
            }
        }
        ast::Stmt::ForLoop(for_stmt) => {
            let iterator_str = describe_expression_detailed(&for_stmt.iter);
            schema.full_expressions.insert(iterator_str);
            extract_variables_from_expr(&for_stmt.iter, schema, loop_context);

            // Record the loop variable and its iterator relationship
            let loop_var_name = match &for_stmt.target {
                ast::Expr::Var(var) => var.id.to_string(),
                _ => "item".to_string(), // fallback
            };

            match &for_stmt.iter {
                ast::Expr::Var(array_var) => {
                    // Simple case: for item in array
                    loop_context.add_loop_mapping(&loop_var_name, &array_var.id);
                    schema
                        .nested_expressions
                        .insert(format!("{}._item_structure", array_var.id));
                }
                ast::Expr::GetAttr(attr) => {
                    // Nested case: for item in parent.array (e.g., for tool_call in message.tool_calls)
                    let full_path = build_variable_path(&attr.expr, &attr.name, loop_context);
                    loop_context.add_loop_mapping(&loop_var_name, &full_path);
                    schema
                        .nested_expressions
                        .insert(format!("{}._item_structure", full_path));
                }
                _ => {}
            }

            for stmt in &for_stmt.body {
                analyze_stmt(stmt, schema, loop_context);
            }
        }
        ast::Stmt::Set(set_stmt) => {
            let expr_str = describe_expression_detailed(&set_stmt.expr);
            schema.full_expressions.insert(expr_str);

            // Only analyze the right-hand side expression for actual input variables
            extract_variables_from_expr(&set_stmt.expr, schema, loop_context);
        }
        _ => {}
    }
}

/// Extract variables from an expression and capture constant values
fn extract_variables_from_expr(
    expr: &ast::Expr,
    schema: &mut VariableSchema,
    loop_context: &LoopContext,
) {
    match expr {
        ast::Expr::Var(var) => {
            // Check if this is a loop variable or set variable - if so, don't add as top-level
            if loop_context.get_array_for_loop_var(&var.id).is_none()
                && !loop_context.is_set_variable(&var.id)
            {
                schema.top_level_variables.insert(var.id.to_string());
            }
        }
        ast::Expr::GetAttr(attr) => {
            let full_path = build_variable_path(&attr.expr, &attr.name, loop_context);
            // Only add nested expression if the base variable is not a set variable
            if let ast::Expr::Var(base_var) = &attr.expr {
                if !loop_context.is_set_variable(&base_var.id) {
                    schema.nested_expressions.insert(full_path);
                }
            } else {
                schema.nested_expressions.insert(full_path);
            }
            extract_variables_from_expr(&attr.expr, schema, loop_context);
        }
        ast::Expr::GetItem(item) => {
            // Handle both array access and object key access
            let full_path = build_getitem_path(&item.expr, &item.subscript_expr, loop_context);
            if !full_path.is_empty() {
                // Only add nested expression if the base variable is not a set variable
                if let ast::Expr::Var(base_var) = &item.expr {
                    if !loop_context.is_set_variable(&base_var.id) {
                        schema.nested_expressions.insert(full_path);
                    }
                } else {
                    schema.nested_expressions.insert(full_path);
                }
            }
            extract_variables_from_expr(&item.expr, schema, loop_context);
            extract_variables_from_expr(&item.subscript_expr, schema, loop_context);
        }
        ast::Expr::BinOp(binop) => {
            // Capture comparisons to infer values
            capture_comparison_values(&binop.left, &binop.right, schema, loop_context);
            extract_variables_from_expr(&binop.left, schema, loop_context);
            extract_variables_from_expr(&binop.right, schema, loop_context);
        }
        ast::Expr::UnaryOp(unaryop) => {
            extract_variables_from_expr(&unaryop.expr, schema, loop_context);
        }
        ast::Expr::List(list) => {
            for item in &list.items {
                extract_variables_from_expr(item, schema, loop_context);
            }
        }
        ast::Expr::Filter(filter) => {
            if let Some(expr) = &filter.expr {
                extract_variables_from_expr(expr, schema, loop_context);
            }
        }
        ast::Expr::Test(test) => {
            extract_variables_from_expr(&test.expr, schema, loop_context);
        }
        ast::Expr::Call(call) => {
            extract_variables_from_expr(&call.expr, schema, loop_context);
        }
        _ => {}
    }
}

/// Capture values from comparisons like var == "value"
fn capture_comparison_values(
    left: &ast::Expr,
    right: &ast::Expr,
    schema: &mut VariableSchema,
    loop_context: &LoopContext,
) {
    match (left, right) {
        (var_expr, ast::Expr::Const(const_val)) => {
            if let Some(var_path) = extract_variable_path(var_expr, loop_context) {
                schema
                    .discovered_values
                    .insert(var_path, convert_const_to_value(const_val));
            }
        }
        (ast::Expr::Const(const_val), var_expr) => {
            if let Some(var_path) = extract_variable_path(var_expr, loop_context) {
                schema
                    .discovered_values
                    .insert(var_path, convert_const_to_value(const_val));
            }
        }
        _ => {}
    }
}

/// Extract the full path of a variable expression
fn extract_variable_path(expr: &ast::Expr, loop_context: &LoopContext) -> Option<String> {
    match expr {
        ast::Expr::Var(var) => Some(var.id.to_string()),
        ast::Expr::GetAttr(attr) => Some(build_variable_path(&attr.expr, &attr.name, loop_context)),
        _ => None,
    }
}

/// Convert AST constant to JSON value
fn convert_const_to_value(const_val: &ast::Const) -> Value {
    let val_str = const_val.value.to_string();

    // Try to parse as different types based on the string representation
    if let Some(s) = const_val.value.as_str() {
        Value::String(s.to_string())
    } else if val_str == "true" {
        Value::Bool(true)
    } else if val_str == "false" {
        Value::Bool(false)
    } else if val_str == "null" || val_str == "none" {
        Value::Null
    } else if let Ok(i) = val_str.parse::<i64>() {
        Value::Number(serde_json::Number::from(i))
    } else if let Ok(f) = val_str.parse::<f64>() {
        Value::Number(serde_json::Number::from_f64(f).unwrap_or(serde_json::Number::from(0)))
    } else {
        Value::String(val_str)
    }
}

/// Build a full variable path like "message.role" or "user.profile.name"
fn build_variable_path(expr: &ast::Expr, attr_name: &str, loop_context: &LoopContext) -> String {
    match expr {
        ast::Expr::Var(var) => {
            // If this is a loop variable, map it to the array structure
            if let Some(array_name) = loop_context.get_array_for_loop_var(&var.id) {
                format!("{}.{}", array_name, attr_name)
            } else {
                format!("{}.{}", var.id, attr_name)
            }
        }
        ast::Expr::GetAttr(attr) => {
            let base_path = build_variable_path(&attr.expr, &attr.name, loop_context);
            format!("{}.{}", base_path, attr_name)
        }
        _ => format!("({}).{}", describe_expression_detailed(expr), attr_name),
    }
}

/// Build a full variable path for item access like "message.role" from message["role"]
fn build_getitem_path(
    expr: &ast::Expr,
    subscript_expr: &ast::Expr,
    loop_context: &LoopContext,
) -> String {
    match (expr, subscript_expr) {
        (ast::Expr::Var(var), ast::Expr::Const(const_val)) => {
            if let Some(key) = const_val.value.as_str() {
                // If this is a loop variable, map it to the array structure
                if let Some(array_name) = loop_context.get_array_for_loop_var(&var.id) {
                    format!("{}.{}", array_name, key)
                } else {
                    format!("{}.{}", var.id, key)
                }
            } else {
                String::new()
            }
        }
        (ast::Expr::GetAttr(attr), ast::Expr::Const(const_val)) => {
            if let Some(key) = const_val.value.as_str() {
                let base_path = build_variable_path(&attr.expr, &attr.name, loop_context);
                format!("{}.{}", base_path, key)
            } else {
                String::new()
            }
        }
        _ => String::new(),
    }
}

/// Describe an expression in detail showing the structure
fn describe_expression_detailed(expr: &ast::Expr) -> String {
    match expr {
        ast::Expr::Var(var) => format!("var({})", var.id),
        ast::Expr::Const(const_val) => format!("const({:?})", const_val.value),
        ast::Expr::GetAttr(attr) => {
            format!("{}.{}", describe_expression_detailed(&attr.expr), attr.name)
        }
        ast::Expr::GetItem(item) => {
            format!(
                "{}[{}]",
                describe_expression_detailed(&item.expr),
                describe_expression_detailed(&item.subscript_expr)
            )
        }
        ast::Expr::Filter(filter) => {
            let expr_str = if let Some(expr) = &filter.expr {
                describe_expression_detailed(expr)
            } else {
                "?".to_string()
            };
            if filter.args.is_empty() {
                format!("{} | {}", expr_str, filter.name)
            } else {
                format!("{} | {}(...)", expr_str, filter.name)
            }
        }
        ast::Expr::Test(test) => {
            if test.args.is_empty() {
                format!(
                    "{} is {}",
                    describe_expression_detailed(&test.expr),
                    test.name
                )
            } else {
                format!(
                    "{} is {}(...)",
                    describe_expression_detailed(&test.expr),
                    test.name
                )
            }
        }
        ast::Expr::Call(call) => {
            if call.args.is_empty() {
                format!("{}()", describe_expression_detailed(&call.expr))
            } else {
                format!("{}(...)", describe_expression_detailed(&call.expr))
            }
        }
        ast::Expr::BinOp(binop) => {
            let left = describe_expression_detailed(&binop.left);
            let right = describe_expression_detailed(&binop.right);
            let debug_str = format!("{:?}", binop);

            let op = if let Some(caps) = regex::Regex::new(r"BinOp \{ left: .*, op: ([^,]+),")
                .unwrap()
                .captures(&debug_str)
            {
                match caps.get(1).map(|m| m.as_str()) {
                    Some("And") => "and",
                    Some("Or") => "or",
                    Some("Eq") => "==",
                    Some("Ne") => "!=",
                    Some("In") => "in",
                    Some("NotIn") => "not in",
                    _ => "==",
                }
            } else {
                "=="
            };
            format!("{} {} {}", left, op, right)
        }
        ast::Expr::UnaryOp(unaryop) => {
            let expr = describe_expression_detailed(&unaryop.expr);
            let debug_str = format!("{:?}", unaryop);
            let op = if debug_str.contains("Not") {
                "not "
            } else {
                "!"
            };
            format!("{}{}", op, expr)
        }
        ast::Expr::List(list) => {
            let items: Vec<String> = list
                .items
                .iter()
                .map(|i| describe_expression_detailed(i))
                .collect();
            format!("[{}]", items.join(", "))
        }
        _ => format!("{:?}", expr).chars().take(50).collect(),
    }
}

/// Build a nested JSON structure from variable paths
fn build_nested_structure(
    nested_expressions: &BTreeSet<String>,
    top_level_variables: &BTreeSet<String>,
    discovered_values: &BTreeMap<String, Value>,
) -> Value {
    let mut root = Map::new();

    // Identify which variables are arrays (have _item_structure markers)
    let mut array_vars = BTreeSet::new();
    let mut item_structures = BTreeMap::new();

    for expr in nested_expressions {
        if expr.ends_with("._item_structure") {
            let var_name = expr.replace("._item_structure", "");
            array_vars.insert(var_name.clone());
        } else {
            // Check if this is a nested expression from a loop variable
            let parts: Vec<&str> = expr.split('.').collect();
            if parts.len() >= 2 {
                // Find the longest matching array variable prefix
                let mut matched_array_var = None;
                let mut remaining_parts = Vec::new();

                for i in (1..=parts.len()).rev() {
                    let potential_array = parts[0..i].join(".");
                    if array_vars.contains(&potential_array) {
                        matched_array_var = Some(potential_array);
                        remaining_parts = parts[i..].to_vec();
                        break;
                    }
                }

                // If no nested array match, try top-level variables
                if matched_array_var.is_none() {
                    let base_var = parts[0];
                    if array_vars.contains(base_var) || top_level_variables.contains(base_var) {
                        matched_array_var = Some(base_var.to_string());
                        remaining_parts = parts[1..].to_vec();
                    }
                }

                if let Some(array_var) = matched_array_var {
                    if !remaining_parts.is_empty() {
                        let remaining_path = remaining_parts.join(".");
                        item_structures
                            .entry(array_var)
                            .or_insert_with(BTreeSet::new)
                            .insert(remaining_path);
                    }
                }
            }
        }
    }

    // First, add all top-level variables
    for var in top_level_variables {
        if array_vars.contains(var) {
            // This is an array - create array with example item
            let item_props = item_structures.get(var).cloned().unwrap_or_default();
            if !item_props.is_empty() {
                let mut example_item = Map::new();

                // Build nested structure for array items
                // Filter out properties that are prefixes of other properties
                let mut filtered_props = BTreeSet::new();
                for prop in &item_props {
                    let mut is_prefix = false;
                    for other_prop in &item_props {
                        if other_prop != prop && other_prop.starts_with(&format!("{}.", prop)) {
                            is_prefix = true;
                            break;
                        }
                    }
                    if !is_prefix {
                        filtered_props.insert(prop.clone());
                    }
                }

                for prop in &filtered_props {
                    let full_path = format!("{}.{}", var, prop);
                    let value = if array_vars.contains(&full_path) {
                        // This is a nested array, create an array structure
                        let nested_item_props =
                            item_structures.get(&full_path).cloned().unwrap_or_default();
                        if !nested_item_props.is_empty() {
                            let mut nested_example_item = Map::new();

                            // Apply the same prefix filtering for nested arrays
                            let mut filtered_nested_props = BTreeSet::new();
                            for nested_prop in &nested_item_props {
                                let mut is_prefix = false;
                                for other_nested_prop in &nested_item_props {
                                    if other_nested_prop != nested_prop
                                        && other_nested_prop
                                            .starts_with(&format!("{}.", nested_prop))
                                    {
                                        is_prefix = true;
                                        break;
                                    }
                                }
                                if !is_prefix {
                                    filtered_nested_props.insert(nested_prop.clone());
                                }
                            }

                            for nested_prop in &filtered_nested_props {
                                let nested_full_path = format!("{}.{}", full_path, nested_prop);
                                let nested_value = discovered_values
                                    .get(&nested_full_path)
                                    .cloned()
                                    .unwrap_or_else(|| Value::String("example_value".to_string()));
                                insert_property_path(
                                    &mut nested_example_item,
                                    nested_prop,
                                    nested_value,
                                );
                            }
                            Value::Array(vec![Value::Object(nested_example_item)])
                        } else {
                            Value::Array(vec![Value::String("example_value".to_string())])
                        }
                    } else {
                        discovered_values
                            .get(&full_path)
                            .cloned()
                            .unwrap_or_else(|| Value::String("example_value".to_string()))
                    };

                    // Insert this property into the example item
                    insert_property_path(&mut example_item, prop, value);
                }
                root.insert(var.clone(), Value::Array(vec![Value::Object(example_item)]));
            } else {
                root.insert(
                    var.clone(),
                    Value::Array(vec![Value::String("example_value".to_string())]),
                );
            }
        } else if !nested_expressions
            .iter()
            .any(|expr| expr.starts_with(&format!("{}.", var)))
        {
            // Regular variable
            let value = discovered_values
                .get(var)
                .cloned()
                .unwrap_or_else(|| Value::String("example_value".to_string()));
            root.insert(var.clone(), value);
        }
    }

    // Then build nested structures from the remaining expressions (non-array items)
    for expr in nested_expressions {
        if !expr.ends_with("._item_structure") {
            let parts: Vec<&str> = expr.split('.').collect();
            if parts.len() >= 2 && !array_vars.contains(parts[0]) {
                insert_nested_path(&mut root, &parts, 0, discovered_values);
            }
        }
    }

    Value::Object(root)
}

/// Insert a property path into an object (e.g., "function.name" -> value)
fn insert_property_path(obj: &mut Map<String, Value>, property_path: &str, value: Value) {
    let parts: Vec<&str> = property_path.split('.').collect();
    insert_property_path_recursive(obj, &parts, 0, value);
}

/// Recursive helper for insert_property_path
fn insert_property_path_recursive(
    obj: &mut Map<String, Value>,
    parts: &[&str],
    index: usize,
    value: Value,
) {
    if index >= parts.len() {
        return;
    }

    let part = parts[index];

    if index == parts.len() - 1 {
        // Last part - insert the value
        obj.insert(part.to_string(), value);
    } else {
        // Intermediate part - ensure there's an object to recurse into
        let entry = obj
            .entry(part.to_string())
            .or_insert_with(|| Value::Object(Map::new()));
        if let Value::Object(nested_obj) = entry {
            insert_property_path_recursive(nested_obj, parts, index + 1, value);
        }
    }
}

/// Insert a nested path into the JSON structure
fn insert_nested_path(
    obj: &mut Map<String, Value>,
    parts: &[&str],
    index: usize,
    discovered_values: &BTreeMap<String, Value>,
) {
    if index >= parts.len() {
        return;
    }

    let key = parts[index].to_string();

    if index == parts.len() - 1 {
        // Last part - insert the actual value
        let full_path = parts.join(".");
        let value = discovered_values
            .get(&full_path)
            .cloned()
            .unwrap_or_else(|| Value::String("example_value".to_string()));
        obj.insert(key, value);
    } else {
        // Intermediate part - ensure there's an object to recurse into
        let entry = obj
            .entry(key.clone())
            .or_insert_with(|| Value::Object(Map::new()));

        if let Value::Object(nested_obj) = entry {
            insert_nested_path(nested_obj, parts, index + 1, discovered_values);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let template_content = if args.len() > 1 {
        // Read template from file
        let template_path = &args[1];
        match fs::read_to_string(template_path) {
            Ok(content) => {
                // println!("Analyzing template: {}\n", template_path);
                content
            }
            Err(e) => {
                eprintln!("Error reading template file '{}': {}", template_path, e);
                std::process::exit(1);
            }
        }
    } else {
        // Use example template if no file provided
        println!("No template file provided. Using example template.\n");
        println!("Usage: cargo run --example schema <template_file>");
        println!("Example: cargo run --example schema templates/deepseekr1.j2\n");

        r#"{%- if product.status == "active" -%}
    {{ product.title }}
    Price: {{ product.price }}
{%- elif customer.preferences.notifications and settings.enabled -%}
    {{ customer.preferences.theme | default("dark") }}
    {% for order in customer.orders %}
        Order #{{ order.id }}: {{ order.total }}
        {% for line_item in order.line_items %}
            - {{ line_item.name }} ({{ line_item.quantity }})
        {% endfor %}
    {% endfor %}
{%- endif -%}
{% set analytics.page_views = analytics.page_views + 1 %}
{% if api.rate_limit.remaining > 0 %}
    {{ api.response.data }}
{% endif %}"#
            .to_string()
    };

    match extract_variable_schema(&template_content) {
        Ok(schema) => {
            // println!("=== Variable Schema Analysis ===\n");

            // println!("Top-level Variables:");
            // for var in &schema.top_level_variables {
            //     println!("  - {}", var);
            // }

            // println!("\nNested Variable Expressions:");
            // for expr in &schema.nested_expressions {
            //     println!("  - {}", expr);
            // }

            // println!("\nFull Expressions (showing structure):");
            // for expr in &schema.full_expressions {
            //     println!("  - {}", expr);
            // }

            // println!("\nDiscovered Values from Template:");
            // for (path, value) in &schema.discovered_values {
            //     println!("  {} = {}", path, value);
            // }

            // Nested structure that could be used as input
            // println!("\n=== Nested Input Structure ===");
            match serde_json::to_string_pretty(&schema.nested_structure) {
                Ok(json) => println!("{}", json),
                Err(e) => println!("Nested structure serialization error: {}", e),
            }

            // // Complete schema JSON output
            // println!("\n=== Complete Schema JSON ===");
            // match serde_json::to_string_pretty(&schema) {
            //     Ok(json) => println!("{}", json),
            //     Err(e) => println!("JSON serialization error: {}", e),
            // }
        }
        Err(e) => {
            println!("Error analyzing template: {}", e);
        }
    }
}
