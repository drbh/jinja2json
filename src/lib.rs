use minijinja::machinery::{ast, parse};
use minijinja::{Environment, Error as MinijinjaError};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::fmt;
use regex::Regex;

/// A wrapper struct for Jinja2 templates
#[derive(Debug, Clone)]
pub struct Template {
    pub template: String,
}

/// Custom error type for template rendering
#[derive(Debug)]
pub enum TemplateError {
    RenderError(MinijinjaError),
    SerializationError(String),
}

impl fmt::Display for TemplateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TemplateError::RenderError(e) => write!(f, "Template render error: {}", e),
            TemplateError::SerializationError(e) => write!(f, "Serialization error: {}", e),
        }
    }
}

impl std::error::Error for TemplateError {}

impl From<MinijinjaError> for TemplateError {
    fn from(error: MinijinjaError) -> Self {
        TemplateError::RenderError(error)
    }
}


/// VM instruction types for template analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum VmInstruction {
    #[serde(rename = "start_template")]
    StartTemplate {
        children: Vec<VmInstruction>,
    },
    #[serde(rename = "emit_raw")]
    EmitRaw {
        content: String,
        preview: String,
    },
    #[serde(rename = "emit_expr")]
    EmitExpr {
        expression: String,
        variable_info: ExpressionInfo,
    },
    #[serde(rename = "if_condition")]
    IfCondition {
        condition: String,
        condition_info: ExpressionInfo,
        true_branch: Vec<VmInstruction>,
        false_branch: Vec<VmInstruction>,
    },
    #[serde(rename = "for_loop")]
    ForLoop {
        target: String,
        iterator: String,
        iterator_info: ExpressionInfo,
        body: Vec<VmInstruction>,
    },
    #[serde(rename = "other")]
    Other {
        operation_type: String,
        description: String,
    },
}

/// Expression analysis information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpressionInfo {
    pub expr_type: String,
    pub variable_name: Option<String>,
    pub attributes: Vec<String>,
    pub filters: Vec<String>,
    pub tests: Vec<String>,
    pub is_complex: bool,
}

/// Complete VM analysis structure
#[derive(Debug, Serialize, Deserialize)]
pub struct VmAnalysis {
    pub template_info: TemplateInfo,
    pub instructions: VmInstruction,
    pub statistics: VmStatistics,
    pub rendered_output: Option<RenderedOutput>,
}

/// Template metadata
#[derive(Debug, Serialize, Deserialize)]
pub struct TemplateInfo {
    pub path: String,
    pub length: usize,
    pub preview: String,
}

/// Template processing statistics
#[derive(Debug, Serialize, Deserialize)]
pub struct VmStatistics {
    pub total_instructions: usize,
    pub raw_emissions: usize,
    pub expression_emissions: usize,
    pub conditionals: usize,
    pub loops: usize,
    pub other_operations: usize,
    pub max_nesting_depth: usize,
}

/// Template rendering test results
#[derive(Debug, Serialize, Deserialize)]
pub struct RenderedOutput {
    pub success: bool,
    pub length: usize,
    pub preview: String,
    pub error: Option<String>,
}

/// Analyzes a Jinja2 template and creates a complete VM analysis
///
/// This function parses a template string and creates a comprehensive analysis
/// including VM instructions, statistics, and template metadata.
///
/// # Arguments
///
/// * `template_source` - The Jinja2 template as a string
/// * `template_path` - Path or name for the template (used in metadata)
/// * `test_context` - Optional JSON context for testing template rendering
///
/// # Returns
///
/// * `Result<VmAnalysis, TemplateError>` - Complete analysis or error
///
/// # Example
///
/// ```
/// use jinja2json::analyze_template;
/// use serde_json::json;
///
/// let template = "Hello {{ name }}!";
/// let context = Some(json!({"name": "World"}));
/// let analysis = analyze_template(template, "greeting.j2", context).unwrap();
/// ```
pub fn analyze_template(
    template_source: &str,
    template_path: &str,
    test_context: Option<Value>,
) -> Result<VmAnalysis, TemplateError> {
    // Parse the template into AST
    let ast = parse(template_source, template_path, Default::default(), Default::default())?;
    
    // Initialize statistics
    let mut stats = VmStatistics {
        total_instructions: 0,
        raw_emissions: 0,
        expression_emissions: 0,
        conditionals: 0,
        loops: 0,
        other_operations: 0,
        max_nesting_depth: 0,
    };
    
    // Analyze VM operations
    let instructions = analyze_vm_operations(&ast, 0, &mut stats);
    
    // Test rendering if context provided
    let rendered_output = if let Some(context) = test_context {
        Some(test_template_rendering(template_source, &context))
    } else {
        None
    };
    
    // Create template info
    let template_info = TemplateInfo {
        path: template_path.to_string(),
        length: template_source.len(),
        preview: template_source[..200.min(template_source.len())].to_string(),
    };
    
    Ok(VmAnalysis {
        template_info,
        instructions,
        statistics: stats,
        rendered_output,
    })
}

/// Analyzes a template and returns just the VM instructions
///
/// This is a simpler version that returns only the instruction tree.
///
/// # Arguments
///
/// * `template_source` - The Jinja2 template as a string
///
/// # Returns
///
/// * `Result<VmInstruction, TemplateError>` - VM instruction tree or error
pub fn analyze_template_instructions(template_source: &str) -> Result<VmInstruction, TemplateError> {
    let ast = parse(template_source, "template", Default::default(), Default::default())?;
    let mut stats = VmStatistics {
        total_instructions: 0,
        raw_emissions: 0,
        expression_emissions: 0,
        conditionals: 0,
        loops: 0,
        other_operations: 0,
        max_nesting_depth: 0,
    };
    
    Ok(analyze_vm_operations(&ast, 0, &mut stats))
}

fn analyze_vm_operations(node: &ast::Stmt, depth: usize, stats: &mut VmStatistics) -> VmInstruction {
    stats.total_instructions += 1;
    stats.max_nesting_depth = stats.max_nesting_depth.max(depth);
    
    match node {
        ast::Stmt::Template(template) => {
            let mut children = Vec::new();
            
            for child in &template.children {
                children.push(analyze_vm_operations(child, depth + 1, stats));
            }
            
            VmInstruction::StartTemplate { children }
        }
        ast::Stmt::EmitRaw(raw) => {
            stats.raw_emissions += 1;
            
            let preview = if raw.raw.len() > 30 {
                format!("{}...", &raw.raw[..27])
            } else {
                raw.raw.to_string()
            };
            let escaped_preview = preview.replace(['\n', '\r'], "\\n");
            
            VmInstruction::EmitRaw {
                content: raw.raw.to_string(),
                preview: escaped_preview,
            }
        }
        ast::Stmt::EmitExpr(emit) => {
            stats.expression_emissions += 1;
            
            let expr_info = analyze_expression(&emit.expr);
            let expr_description = describe_expression(&emit.expr);
            
            VmInstruction::EmitExpr {
                expression: expr_description,
                variable_info: expr_info,
            }
        }
        ast::Stmt::IfCond(if_stmt) => {
            stats.conditionals += 1;
            
            let condition_info = analyze_expression(&if_stmt.expr);
            let condition_description = describe_expression(&if_stmt.expr);
            
            let mut true_branch = Vec::new();
            for stmt in &if_stmt.true_body {
                true_branch.push(analyze_vm_operations(stmt, depth + 1, stats));
            }
            
            let mut false_branch = Vec::new();
            for stmt in &if_stmt.false_body {
                false_branch.push(analyze_vm_operations(stmt, depth + 1, stats));
            }
            
            VmInstruction::IfCondition {
                condition: condition_description,
                condition_info,
                true_branch,
                false_branch,
            }
        }
        ast::Stmt::ForLoop(for_stmt) => {
            stats.loops += 1;
            
            let target = format!("{:?}", for_stmt.target);
            let iterator_info = analyze_expression(&for_stmt.iter);
            let iterator_description = describe_expression(&for_stmt.iter);
            
            let mut body = Vec::new();
            for stmt in &for_stmt.body {
                body.push(analyze_vm_operations(stmt, depth + 1, stats));
            }
            
            VmInstruction::ForLoop {
                target,
                iterator: iterator_description,
                iterator_info,
                body,
            }
        }
        _ => {
            stats.other_operations += 1;
            
            VmInstruction::Other {
                operation_type: std::any::type_name_of_val(&node).to_string(),
                description: format!("{:?}", node).chars().take(100).collect(),
            }
        }
    }
}

fn analyze_expression(expr: &ast::Expr) -> ExpressionInfo {
    match expr {
        ast::Expr::Var(var) => ExpressionInfo {
            expr_type: "variable".to_string(),
            variable_name: Some(var.id.to_string()),
            attributes: Vec::new(),
            filters: Vec::new(),
            tests: Vec::new(),
            is_complex: false,
        },
        ast::Expr::Const(_const_val) => ExpressionInfo {
            expr_type: "constant".to_string(),
            variable_name: None,
            attributes: Vec::new(),
            filters: Vec::new(),
            tests: Vec::new(),
            is_complex: false,
        },
        ast::Expr::GetAttr(attr) => {
            let mut base_info = analyze_expression(&attr.expr);
            base_info.expr_type = "attribute_access".to_string();
            base_info.attributes.push(attr.name.to_string());
            base_info.is_complex = true;
            base_info
        }
        ast::Expr::GetItem(item) => {
            let mut base_info = analyze_expression(&item.expr);
            base_info.expr_type = "item_access".to_string();
            base_info.is_complex = true;
            base_info
        }
        ast::Expr::Filter(filter) => {
            let mut base_info = if let Some(expr) = &filter.expr {
                analyze_expression(expr)
            } else {
                ExpressionInfo {
                    expr_type: "unknown".to_string(),
                    variable_name: None,
                    attributes: Vec::new(),
                    filters: Vec::new(),
                    tests: Vec::new(),
                    is_complex: false,
                }
            };
            base_info.expr_type = "filtered_expression".to_string();
            base_info.filters.push(filter.name.to_string());
            base_info.is_complex = true;
            base_info
        }
        ast::Expr::Test(test) => {
            let mut base_info = analyze_expression(&test.expr);
            base_info.expr_type = "test_expression".to_string();
            base_info.tests.push(test.name.to_string());
            base_info.is_complex = true;
            base_info
        }
        ast::Expr::Call(call) => {
            let mut base_info = analyze_expression(&call.expr);
            base_info.expr_type = "function_call".to_string();
            base_info.is_complex = true;
            base_info
        }
        ast::Expr::List(_) => ExpressionInfo {
            expr_type: "list".to_string(),
            variable_name: None,
            attributes: Vec::new(),
            filters: Vec::new(),
            tests: Vec::new(),
            is_complex: true,
        },
        _ => ExpressionInfo {
            expr_type: "other".to_string(),
            variable_name: None,
            attributes: Vec::new(),
            filters: Vec::new(),
            tests: Vec::new(),
            is_complex: true,
        },
    }
}

fn describe_expression(expr: &ast::Expr) -> String {
    match expr {
        ast::Expr::Var(var) => format!("var({})", var.id),
        ast::Expr::Const(const_val) => format!("const({:?})", const_val.value),
        ast::Expr::GetAttr(attr) => {
            format!("{}.{}", describe_expression(&attr.expr), attr.name)
        }
        ast::Expr::GetItem(item) => {
            format!(
                "{}[{}]",
                describe_expression(&item.expr),
                describe_expression(&item.subscript_expr)
            )
        }
        ast::Expr::Filter(filter) => {
            let arg_str = if filter.args.is_empty() {
                String::new()
            } else {
                format!(
                    "({})",
                    filter
                        .args
                        .iter()
                        .map(|a| format!("{:?}", a))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            let expr_str = if let Some(expr) = &filter.expr {
                describe_expression(expr)
            } else {
                "?".to_string()
            };
            format!("{} | {}{}", expr_str, filter.name, arg_str)
        }
        ast::Expr::Test(test) => {
            let arg_str = if test.args.is_empty() {
                String::new()
            } else {
                format!(
                    " {}",
                    test.args
                        .iter()
                        .map(|a| format!("{:?}", a))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            };
            format!(
                "{} is {}{}",
                describe_expression(&test.expr),
                test.name,
                arg_str
            )
        }
        ast::Expr::Call(call) => {
            format!(
                "{}({})",
                describe_expression(&call.expr),
                call.args
                    .iter()
                    .map(|a| format!("{:?}", a))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
        ast::Expr::List(list) => {
            format!(
                "[{}]",
                list.items
                    .iter()
                    .map(|i| describe_expression(i))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
        ast::Expr::BinOp(binop) => {
            let left = describe_expression(&binop.left);
            let right = describe_expression(&binop.right);
            // For now, we'll infer the operator from the debug string
            let debug_str = format!("{:?}", binop);
            let op = if debug_str.contains("Eq") {
                "=="
            } else if debug_str.contains("Ne") {
                "!="
            } else {
                "=="
            };
            format!("{} {} {}", left, op, right)
        }
        _ => format!("{:?}", expr).chars().take(100).collect(),
    }
}

fn test_template_rendering(template_source: &str, context: &Value) -> RenderedOutput {
    let env = Environment::new();
    match env.template_from_str(template_source) {
        Ok(template) => {
            match template.render(context) {
                Ok(result) => RenderedOutput {
                    success: true,
                    length: result.len(),
                    preview: result[..200.min(result.len())].to_string(),
                    error: None,
                },
                Err(e) => RenderedOutput {
                    success: false,
                    length: 0,
                    preview: String::new(),
                    error: Some(format!("{}", e)),
                },
            }
        }
        Err(e) => RenderedOutput {
            success: false,
            length: 0,
            preview: String::new(),
            error: Some(format!("{}", e)),
        },
    }
}

/// Renders a Jinja2 template with the provided JSON data
///
/// # Arguments
///
/// * `template` - A Template struct containing the Jinja2 template string
/// * `data` - JSON data to use as context for rendering the template
///
/// # Returns
///
/// * `Ok(String)` - The rendered template as a string
/// * `Err(TemplateError)` - An error if rendering fails
///
/// # Example
///
/// ```
/// use jinja2json::{json_to_template, Template};
/// use serde_json::json;
///
/// let template = Template {
///     template: "Hello {{ name }}!".to_string(),
/// };
///
/// let data = json!({
///     "name": "World"
/// });
///
/// match json_to_template(&template, &data) {
///     Ok(result) => println!("{}", result), // Prints: Hello World!
///     Err(e) => eprintln!("Error: {}", e),
/// }
/// ```
pub fn json_to_template(template: &Template, data: &Value) -> Result<String, TemplateError> {
    // Create a new Jinja environment
    let mut env = Environment::new();
    
    // Add the template to the environment
    env.add_template("template", &template.template)?;
    
    // Get the template from the environment
    let tmpl = env.get_template("template")?;
    
    // Render the template with the JSON data
    let rendered = tmpl.render(data)?;
    
    Ok(rendered)
}

/// Reconstructs a Jinja2 template from VM analysis JSON
///
/// This function takes a JSON structure containing VM instruction analysis
/// and reconstructs the original Jinja2 template syntax.
///
/// # Arguments
///
/// * `vm_analysis` - JSON value containing the VM analysis structure
///
/// # Returns
///
/// * `Result<String, TemplateError>` - The reconstructed template or an error
///
/// # Example
///
/// ```
/// use jinja2json::reconstruct_template;
/// use serde_json::json;
///
/// let vm_analysis = json!({
///     "instructions": {
///         "type": "start_template",
///         "children": [
///             {
///                 "type": "emit_raw",
///                 "content": "Hello "
///             },
///             {
///                 "type": "emit_expr",
///                 "expression": "var(name)"
///             }
///         ]
///     }
/// });
///
/// let template = reconstruct_template(&vm_analysis).unwrap();
/// // Returns: "Hello {{ name }}"
/// ```
pub fn reconstruct_template(vm_analysis: &Value) -> Result<String, TemplateError> {
    let mut template = String::new();
    
    if let Some(instructions) = vm_analysis.get("instructions") {
        if let Some(children) = instructions.get("children") {
            reconstruct_children(&mut template, children, 0);
        }
    }
    
    Ok(template)
}

/// Reconstructs and formats a Jinja2 template from VM analysis JSON
///
/// This function reconstructs the template and applies formatting to make it more readable,
/// including proper indentation and line breaks.
///
/// # Arguments
///
/// * `vm_analysis` - JSON value containing the VM analysis structure
///
/// # Returns
///
/// * `Result<String, TemplateError>` - The formatted reconstructed template or an error
pub fn reconstruct_template_formatted(vm_analysis: &Value) -> Result<String, TemplateError> {
    let reconstructed = reconstruct_template(vm_analysis)?;
    Ok(format_template(&reconstructed))
}

fn reconstruct_children(template: &mut String, children: &Value, indent: usize) -> () {
    if let Some(arr) = children.as_array() {
        for child in arr {
            reconstruct_instruction(template, child, indent);
        }
    }
}

fn reconstruct_instruction(template: &mut String, instruction: &Value, indent: usize) -> () {
    let _indent_str = " ".repeat(indent);
    
    match instruction.get("type").and_then(|t| t.as_str()) {
        Some("emit_raw") => {
            if let Some(content) = instruction.get("content").and_then(|c| c.as_str()) {
                template.push_str(content);
            }
        }
        Some("emit_expr") => {
            if let Some(expr_str) = instruction.get("expression").and_then(|e| e.as_str()) {
                // Parse the expression string
                let parsed = parse_expression(expr_str);
                template.push_str("{{ ");
                template.push_str(&parsed);
                template.push_str(" }}");
            } else if let Some(var_info) = instruction.get("variable_info") {
                // Fallback to variable_info
                let expr = reconstruct_expression(var_info);
                template.push_str("{{ ");
                template.push_str(&expr);
                template.push_str(" }}");
            }
        }
        Some("if_condition") => {
            let condition = if let Some(cond_str) = instruction.get("condition").and_then(|c| c.as_str()) {
                // First try to use the condition string directly if it looks complete
                if cond_str.contains(" is defined") || !cond_str.contains("BinOp") {
                    parse_simple_condition(cond_str)
                } else {
                    // Otherwise try to reconstruct from truncated BinOp
                    reconstruct_condition(instruction.get("condition_info"), instruction.get("condition"))
                }
            } else {
                "condition".to_string()
            };
            
            template.push_str("{% if ");
            template.push_str(&condition);
            template.push_str(" %}");
            
            if let Some(true_branch) = instruction.get("true_branch") {
                reconstruct_children(template, true_branch, indent);
            }
            
            if let Some(false_branch) = instruction.get("false_branch") {
                if let Some(arr) = false_branch.as_array() {
                    if !arr.is_empty() {
                        // Check if this is an elif pattern
                        let is_elif = arr.get(0)
                            .and_then(|first| first.get("type"))
                            .and_then(|t| t.as_str())
                            .map(|t| t == "if_condition")
                            .unwrap_or(false);
                        
                        if is_elif {
                            // Process the nested if as an elif
                            if let Some(first_inst) = arr.get(0) {
                                if let Some(cond_str) = first_inst.get("condition").and_then(|c| c.as_str()) {
                                    let condition = if cond_str.contains(" is defined") || !cond_str.contains("BinOp") {
                                        parse_simple_condition(cond_str)
                                    } else {
                                        reconstruct_condition(first_inst.get("condition_info"), first_inst.get("condition"))
                                    };
                                    
                                    template.push_str("{% elif ");
                                    template.push_str(&condition);
                                    template.push_str(" %}");
                                    
                                    if let Some(true_branch) = first_inst.get("true_branch") {
                                        reconstruct_children(template, true_branch, indent);
                                    }
                                    
                                    // Continue with any remaining false branch
                                    if let Some(false_branch) = first_inst.get("false_branch") {
                                        if let Some(fb_arr) = false_branch.as_array() {
                                            if !fb_arr.is_empty() {
                                                let next_is_elif = fb_arr.get(0)
                                                    .and_then(|i| i.get("type"))
                                                    .and_then(|t| t.as_str())
                                                    .map(|t| t == "if_condition")
                                                    .unwrap_or(false);
                                                
                                                if next_is_elif {
                                                    reconstruct_children(template, false_branch, indent);
                                                } else {
                                                    template.push_str("{% else %}");
                                                    reconstruct_children(template, false_branch, indent);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            template.push_str("{% else %}");
                            reconstruct_children(template, false_branch, indent);
                        }
                    }
                }
            }
            
            template.push_str("{% endif %}");
        }
        Some("for_loop") => {
            if let Some(target) = instruction.get("target").and_then(|t| t.as_str()) {
                // Extract variable name from "Var { id: \"variable\" } @ ..."
                let var_name = extract_var_name(target);
                
                let iterator = if let Some(iter) = instruction.get("iterator").and_then(|i| i.as_str()) {
                    parse_expression(iter)
                } else if let Some(iter_info) = instruction.get("iterator_info") {
                    reconstruct_expression(iter_info)
                } else {
                    "items".to_string()
                };
                
                template.push_str("{% for ");
                template.push_str(&var_name);
                template.push_str(" in ");
                template.push_str(&iterator);
                template.push_str(" %}");
                
                if let Some(body) = instruction.get("body") {
                    reconstruct_children(template, body, indent + 2);
                }
                
                template.push_str("{% endfor %}");
            }
        }
        _ => {}
    }
}

fn extract_var_name(target: &str) -> String {
    // Extract from patterns like "Var { id: \"variable\" } @ ..."
    if let Some(caps) = Regex::new(r#"Var \{ id: "(\w+)" \}"#).unwrap().captures(target) {
        return caps[1].to_string();
    }
    "item".to_string()
}

fn reconstruct_expression(var_info: &Value) -> String {
    let mut expr = String::new();
    
    if let Some(var_name) = var_info.get("variable_name").and_then(|v| v.as_str()) {
        expr.push_str(var_name);
        
        if let Some(attrs) = var_info.get("attributes").and_then(|a| a.as_array()) {
            for attr in attrs {
                if let Some(attr_str) = attr.as_str() {
                    expr.push('.');
                    expr.push_str(attr_str);
                }
            }
        }
        
        if let Some(filters) = var_info.get("filters").and_then(|f| f.as_array()) {
            for filter in filters {
                if let Some(filter_str) = filter.as_str() {
                    expr.push_str(" | ");
                    expr.push_str(filter_str);
                }
            }
        }
    }
    
    expr
}

fn parse_expression(expr: &str) -> String {
    let mut result = expr.to_string();
    
    // Handle constant strings - const("string") -> "string"
    let const_str_re = Regex::new(r#"const\("([^"]*)"\)"#).unwrap();
    result = const_str_re.replace_all(&result, "\"$1\"").to_string();
    
    // Handle simple constants - const(value) -> value
    let const_re = Regex::new(r"const\(([^)]+)\)").unwrap();
    result = const_re.replace_all(&result, "$1").to_string();
    
    // Replace var(...) with just the variable name
    let var_re = Regex::new(r"var\(([^)]+)\)").unwrap();
    result = var_re.replace_all(&result, "$1").to_string();
    
    // Handle dictionary access patterns like [const("role")]
    let dict_re = Regex::new(r#"\["([^"]+)"\]"#).unwrap();
    result = dict_re.replace_all(&result, "['$1']").to_string();
    
    // Handle filter arguments like tojson(Kwarg("indent", Const { value: 2 }), ...)
    // Extract just the filter name for now
    if result.contains(" | ") {
        let parts: Vec<&str> = result.split(" | ").collect();
        if parts.len() == 2 && parts[1].contains("(") {
            // Extract filter name and check for common filters
            let filter_part = parts[1];
            if filter_part.starts_with("tojson") {
                result = format!("{} | tojson(indent=2, sort_keys=True)", parts[0]);
            } else {
                // For other filters, just use the filter name
                let filter_name = filter_part.split('(').next().unwrap_or(filter_part);
                result = format!("{} | {}", parts[0], filter_name);
            }
        }
    }
    
    // Handle string concatenation (== should be +)
    result = result.replace(" == ", " + ");
    
    result
}

fn parse_simple_condition(cond: &str) -> String {
    parse_expression(cond)
}

fn reconstruct_condition(cond_info: Option<&Value>, raw_condition: Option<&Value>) -> String {
    if let Some(info) = cond_info {
        if let Some(test_expr) = info.get("expr_type").and_then(|e| e.as_str()) {
            if test_expr == "test_expression" {
                if let Some(var_name) = info.get("variable_name").and_then(|v| v.as_str()) {
                    let mut cond = var_name.to_string();
                    
                    if let Some(attrs) = info.get("attributes").and_then(|a| a.as_array()) {
                        for attr in attrs {
                            if let Some(attr_str) = attr.as_str() {
                                cond.push('.');
                                cond.push_str(attr_str);
                            }
                        }
                    }
                    
                    if let Some(tests) = info.get("tests").and_then(|t| t.as_array()) {
                        for test in tests {
                            if let Some(test_str) = test.as_str() {
                                cond.push_str(" is ");
                                cond.push_str(test_str);
                            }
                        }
                    }
                    
                    return cond;
                }
            }
        }
    }
    
    // Fallback: try to parse from raw condition string  
    if let Some(raw) = raw_condition.and_then(|r| r.as_str()) {
        return parse_expression(raw);
    }
    
    "condition".to_string()
}

fn format_template(template: &str) -> String {
    let mut result = template.to_string();
    
    // Add newlines after }} when followed by {{
    result = result.replace("}}{{", "}}\n{{");
    
    // Add newlines after control structures when followed by other structures
    result = result.replace("{% endif %}{%", "{% endif %}\n{%");
    result = result.replace("{% endfor %}{%", "{% endfor %}\n{%");
    
    // Add newlines before end control structures when preceded by expressions
    result = result.replace("}}{% endif %}", "}}\n{% endif %}");
    result = result.replace("}}{% endfor %}", "}}\n{% endfor %}");
    
    // Add newlines after control structures when followed by expressions
    let for_expr_re = Regex::new(r"(%}\{\{)").unwrap();
    result = for_expr_re.replace_all(&result, "%}\n{{").to_string();
    
    // Add basic indentation for expressions inside control blocks
    let lines: Vec<&str> = result.split('\n').collect();
    let mut formatted_lines = Vec::new();
    let mut indent_level: usize = 0;
    
    for line in lines {
        let trimmed = line.trim();
        
        // Decrease indent for end tags
        if trimmed.starts_with("{% endif %}") || trimmed.starts_with("{% endfor %}") {
            indent_level = indent_level.saturating_sub(1);
        }
        
        // Add indentation
        let indent = "  ".repeat(indent_level);
        formatted_lines.push(format!("{}{}", indent, trimmed));
        
        // Increase indent for start tags
        if trimmed.starts_with("{% if ") || trimmed.starts_with("{% elif ") || trimmed.starts_with("{% for ") {
            indent_level += 1;
        }
    }
    
    formatted_lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_simple_variable_substitution() {
        let template = Template {
            template: "Hello {{ name }}!".to_string(),
        };
        
        let data = json!({
            "name": "World"
        });
        
        let result = json_to_template(&template, &data).unwrap();
        assert_eq!(result, "Hello World!");
    }

    #[test]
    fn test_loop_rendering() {
        let template = Template {
            template: "{% for item in items %}{{ item }} {% endfor %}".to_string(),
        };
        
        let data = json!({
            "items": ["apple", "banana", "cherry"]
        });
        
        let result = json_to_template(&template, &data).unwrap();
        assert_eq!(result, "apple banana cherry ");
    }

    #[test]
    fn test_conditional_rendering() {
        let template = Template {
            template: "{% if show %}Visible{% else %}Hidden{% endif %}".to_string(),
        };
        
        let data = json!({
            "show": true
        });
        
        let result = json_to_template(&template, &data).unwrap();
        assert_eq!(result, "Visible");
        
        let data = json!({
            "show": false
        });
        
        let result = json_to_template(&template, &data).unwrap();
        assert_eq!(result, "Hidden");
    }

    #[test]
    fn test_missing_variable_renders_empty() {
        let template = Template {
            template: "Hello {{ missing_var }}!".to_string(),
        };
        
        let data = json!({});
        
        let result = json_to_template(&template, &data).unwrap();
        // By default, minijinja renders undefined variables as empty strings
        assert_eq!(result, "Hello !");
    }
    
    #[test]
    fn test_complex_nested_data() {
        let template = Template {
            template: "User: {{ user.name }}, Age: {{ user.age }}, City: {{ user.address.city }}".to_string(),
        };
        
        let data = json!({
            "user": {
                "name": "John",
                "age": 25,
                "address": {
                    "city": "New York"
                }
            }
        });
        
        let result = json_to_template(&template, &data).unwrap();
        assert_eq!(result, "User: John, Age: 25, City: New York");
    }
}