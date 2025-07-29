use pyo3::prelude::*;
use jinja2json::{analyze_template, reconstruct_template_formatted, json_to_template, Template};
use serde_json::Value;

#[pyfunction]
fn analyze_template_py(
    template_source: String,
    template_path: Option<String>,
    test_context: Option<String>,
) -> PyResult<String> {
    let path = template_path.unwrap_or_else(|| "template".to_string());
    let context: Option<Value> = if let Some(ctx) = test_context {
        Some(serde_json::from_str(&ctx).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Invalid JSON context: {}", e))
        })?)
    } else {
        None
    };

    let analysis = analyze_template(&template_source, &path, context).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Template analysis failed: {}", e))
    })?;

    serde_json::to_string_pretty(&analysis).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("JSON serialization failed: {}", e))
    })
}

#[pyfunction]
fn reconstruct_template_py(vm_analysis_json: String) -> PyResult<String> {
    let vm_analysis: Value = serde_json::from_str(&vm_analysis_json).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Invalid JSON: {}", e))
    })?;

    reconstruct_template_formatted(&vm_analysis).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Template reconstruction failed: {}", e))
    })
}

#[pyfunction]
fn render_template_py(template_source: String, context_json: String) -> PyResult<String> {
    let template = Template {
        template: template_source,
    };

    let context: Value = serde_json::from_str(&context_json).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Invalid JSON context: {}", e))
    })?;

    json_to_template(&template, &context).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Template rendering failed: {}", e))
    })
}

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn _core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(analyze_template_py, m)?)?;
    m.add_function(wrap_pyfunction!(reconstruct_template_py, m)?)?;
    m.add_function(wrap_pyfunction!(render_template_py, m)?)?;
    Ok(())
}
