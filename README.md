# jinja2json

Convert Jinja2 templates to JSON VM analysis and back. A CLI tool for analyzing Jinja2 template structure and reconstructing templates from their VM representations.

## Usage

### Convert template to JSON
```bash
# From file
jinja2json to-json templates/chat_template.j2

# From stdin
echo "Hello {{ name }}\!" | jinja2json to-json -

# Save to file
jinja2json to-json templates/chat_template.j2 -o templates/analysis.json
```

### Convert JSON back to template
```bash
# From file
jinja2json to-jinja templates/analysis.json

# From stdin
cat templates/analysis.json | jinja2json to-jinja -

# Save to file
jinja2json to-jinja templates/analysis.json -o templates/reconstructed.j2
```

### Round-trip conversion
```bash
jinja2json to-json templates/chat_template.j2 | jinja2json to-jinja - -o templates/reconstructed.j2
# Creates reconstructed template in templates/ folder
```