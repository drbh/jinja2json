import json
from pathlib import Path
from typing import Optional, Dict, Any, Union
from py_jinja2json._core import analyze_template_py, reconstruct_template_py, render_template_py

__version__ = "0.1.0"

def analyze_template(
    template: str,
    template_path: Optional[str] = None,
    test_context: Optional[Dict[str, Any]] = None
) -> Dict[str, Any]:
    """
    Analyze a Jinja2 template and return a detailed VM analysis.
    
    Args:
        template: The Jinja2 template string to analyze
        template_path: Optional path/name for the template (used in metadata)
        test_context: Optional context dictionary to test template rendering
        
    Returns:
        Dictionary containing the VM analysis with instructions, statistics, and metadata
        
    Example:
        >>> analysis = analyze_template("Hello {{ name }}!", test_context={"name": "World"})
        >>> print(analysis["statistics"]["expression_emissions"])
        1
    """
    context_json = None
    if test_context is not None:
        context_json = json.dumps(test_context)
    
    result_json = analyze_template_py(template, template_path, context_json)
    return json.loads(result_json)

def reconstruct_template(vm_analysis: Dict[str, Any]) -> str:
    """
    Reconstruct a Jinja2 template from VM analysis.
    
    Args:
        vm_analysis: Dictionary containing VM analysis (from analyze_template)
        
    Returns:
        The reconstructed Jinja2 template string
        
    Example:
        >>> analysis = analyze_template("Hello {{ name }}!")
        >>> template = reconstruct_template(analysis)
        >>> print(template)
        Hello {{ name }}!
    """
    vm_analysis_json = json.dumps(vm_analysis)
    return reconstruct_template_py(vm_analysis_json)

def render_template(template: str, context: Dict[str, Any]) -> str:
    """
    Render a Jinja2 template with the provided context.
    
    Args:
        template: The Jinja2 template string
        context: Dictionary containing template variables
        
    Returns:
        The rendered template as a string
        
    Example:
        >>> result = render_template("Hello {{ name }}!", {"name": "World"})
        >>> print(result)
        Hello World!
    """
    context_json = json.dumps(context)
    return render_template_py(template, context_json)

# Convenience function for template-to-JSON workflow
def template_to_json(
    template: str,
    template_path: Optional[str] = None,
    test_context: Optional[Dict[str, Any]] = None
) -> Dict[str, Any]:
    """
    Convert a Jinja2 template to JSON VM analysis (alias for analyze_template).
    
    Args:
        template: The Jinja2 template string
        template_path: Optional path/name for the template
        test_context: Optional context dictionary to test rendering
        
    Returns:
        Dictionary containing the VM analysis
    """
    return analyze_template(template, template_path, test_context)

# Convenience function for JSON-to-template workflow  
def json_to_template(vm_analysis: Dict[str, Any]) -> str:
    """
    Convert JSON VM analysis back to Jinja2 template (alias for reconstruct_template).
    
    Args:
        vm_analysis: Dictionary containing VM analysis
        
    Returns:
        The reconstructed Jinja2 template string
    """
    return reconstruct_template(vm_analysis)

def main() -> None:
    """Command line interface - placeholder for future CLI implementation."""
    print("py-jinja2json: Python bindings for jinja2json")
    print("Use the Python API functions: analyze_template, reconstruct_template, render_template")

# File I/O convenience functions
def analyze_template_file(
    template_path: Union[str, Path],
    test_context: Optional[Dict[str, Any]] = None,
    output_path: Optional[Union[str, Path]] = None
) -> Dict[str, Any]:
    """
    Analyze a Jinja2 template file and optionally save the analysis to JSON.
    
    Args:
        template_path: Path to the Jinja2 template file
        test_context: Optional context dictionary to test template rendering
        output_path: Optional path to save the JSON analysis
        
    Returns:
        Dictionary containing the VM analysis
        
    Example:
        >>> analysis = analyze_template_file("templates/chat.j2")
        >>> # Save analysis to file
        >>> analyze_template_file("templates/chat.j2", output_path="analysis.json")
    """
    template_path = Path(template_path)
    template_content = template_path.read_text(encoding='utf-8')
    
    analysis = analyze_template(template_content, str(template_path), test_context)
    
    if output_path:
        output_path = Path(output_path)
        output_path.write_text(json.dumps(analysis, indent=2), encoding='utf-8')
        print(f"Analysis saved to: {output_path}")
    
    return analysis

def reconstruct_template_file(
    analysis_path: Union[str, Path],
    output_path: Optional[Union[str, Path]] = None
) -> str:
    """
    Reconstruct a Jinja2 template from a JSON analysis file.
    
    Args:
        analysis_path: Path to the JSON analysis file
        output_path: Optional path to save the reconstructed template
        
    Returns:
        The reconstructed Jinja2 template string
        
    Example:
        >>> template = reconstruct_template_file("analysis.json")
        >>> # Save template to file
        >>> reconstruct_template_file("analysis.json", "reconstructed.j2")
    """
    analysis_path = Path(analysis_path)
    analysis = json.loads(analysis_path.read_text(encoding='utf-8'))
    
    template = reconstruct_template(analysis)
    
    if output_path:
        output_path = Path(output_path)
        output_path.write_text(template, encoding='utf-8')
        print(f"Template saved to: {output_path}")
    
    return template

def render_template_file(
    template_path: Union[str, Path],
    context: Dict[str, Any],
    output_path: Optional[Union[str, Path]] = None
) -> str:
    """
    Render a Jinja2 template file with context and optionally save the result.
    
    Args:
        template_path: Path to the Jinja2 template file
        context: Dictionary containing template variables
        output_path: Optional path to save the rendered result
        
    Returns:
        The rendered template as a string
        
    Example:
        >>> result = render_template_file("templates/basic.j2", {"messages": [...]})
        >>> # Save result to file
        >>> render_template_file("templates/basic.j2", context, "output.txt")
    """
    template_path = Path(template_path)
    template_content = template_path.read_text(encoding='utf-8')
    
    result = render_template(template_content, context)
    
    if output_path:
        output_path = Path(output_path)
        output_path.write_text(result, encoding='utf-8')
        print(f"Rendered output saved to: {output_path}")
    
    return result

def load_context_from_file(context_path: Union[str, Path]) -> Dict[str, Any]:
    """
    Load template context from a JSON file.
    
    Args:
        context_path: Path to the JSON context file
        
    Returns:
        Dictionary containing the context data
        
    Example:
        >>> context = load_context_from_file("context.json")
        >>> render_template_file("template.j2", context)
    """
    context_path = Path(context_path)
    return json.loads(context_path.read_text(encoding='utf-8'))

__all__ = [
    "analyze_template",
    "reconstruct_template", 
    "render_template",
    "template_to_json",
    "json_to_template",
    "analyze_template_file",
    "reconstruct_template_file",
    "render_template_file",
    "load_context_from_file"
]
