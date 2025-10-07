#!/usr/bin/env python3
"""
General-purpose converter for org files to Docusaurus-compatible markdown files
Can be used in any project with org-mode documentation
"""

import re
import sys
import os
import glob
import argparse
import json

def convert_org_to_docusaurus_md(org_content, link_mappings=None):
    """Convert org content to Docusaurus-compatible markdown"""

    # Start with basic conversion
    md_content = org_content

    # Protect code blocks from inline code conversion
    def protect_codeblocks(match):
        code_content = match.group(2)
        # Replace = with a placeholder to prevent inline code conversion
        code_content = code_content.replace('=', '⊡EQUALS⊡')
        return f"{match.group(1)}\n{code_content}\n{match.group(3)}"

    # Protect both #+begin_src blocks and ``` blocks
    md_content = re.sub(r'(\#\+begin_src\s*\w*\n)(.*?)(\n\#\+end_src)', protect_codeblocks, md_content, flags=re.DOTALL)
    md_content = re.sub(r'(```\w*\n)(.*?)(```)', protect_codeblocks, md_content, flags=re.DOTALL)


    # Extract and remove org title directive
    title_match = re.search(r'^\#\+title:\s*(.+?)$', md_content, flags=re.MULTILINE | re.IGNORECASE)
    extracted_title = title_match.group(1).strip() if title_match else None
    md_content = re.sub(r'^\#\+title:.*?\n', '', md_content, flags=re.MULTILINE | re.IGNORECASE)

    # Convert org headers (* -> #, ** -> ##, etc)
    md_content = re.sub(r'^\*\*\* (.+)', r'### \1', md_content, flags=re.MULTILINE)
    md_content = re.sub(r'^\*\* (.+)', r'## \1', md_content, flags=re.MULTILINE)
    md_content = re.sub(r'^\* (.+)', r'# \1', md_content, flags=re.MULTILINE)

    # Convert org links [[url][text]] to [text](url)
    md_content = re.sub(r'\[\[([^\]]+)\]\[([^\]]+)\]\]', r'[\2](\1)', md_content)

    # Apply custom link mappings if provided
    if link_mappings:
        for pattern, replacement in link_mappings.items():
            md_content = re.sub(pattern, replacement, md_content)

    # Convert org image links [[file:path]] to ![](path)
    md_content = re.sub(r'\[\[file:([^\]]+)\]\]', r'![](/\1)', md_content)

    # Convert bare URLs <https://...> to markdown links
    md_content = re.sub(r'<(https?://[^>]+)>', r'[\1](\1)', md_content)

    # Convert org code inline ~code~ and =code= to `code`
    # But be careful not to convert = in HTML attributes like href="..."
    md_content = re.sub(r'~([^~]+)~', r'`\1`', md_content)
    # Convert =text= to `text` but avoid HTML attributes
    # Match =...= patterns that are not inside HTML tags (not preceded by href or other attributes)
    md_content = re.sub(r'(?<![a-zA-Z]")=([^=\n]+)=(?![">])', r'`\1`', md_content)

    # Convert org code blocks #+begin_src to ```
    md_content = re.sub(r'^\#\+begin_src\s*(.+)', r'```\1', md_content, flags=re.MULTILINE)
    md_content = re.sub(r'^\#\+end_src', r'```', md_content, flags=re.MULTILINE)

    # Convert org quotes
    md_content = re.sub(r'^\#\+begin_quote', r'>', md_content, flags=re.MULTILINE)
    md_content = re.sub(r'^\#\+end_quote', r'', md_content, flags=re.MULTILINE)

    # Remove org directives (html, attr, etc.)
    md_content = re.sub(r'^\#\+[a-zA-Z_]+:.*?\n', '', md_content, flags=re.MULTILINE)

    # Clean up multiple newlines
    md_content = re.sub(r'\n{3,}', '\n\n', md_content)

    # Restore protected = symbols in code blocks
    md_content = md_content.replace('⊡EQUALS⊡', '=')

    return md_content.strip(), extracted_title

def split_content_by_sections(content):
    """Split content into logical sections"""
    sections = {}

    # Temporarily replace special chars in code blocks to avoid interference with section splitting and inline code conversion
    # Find all code blocks and replace # and = with placeholders
    def replace_special_chars_in_codeblocks(match):
        code_content = match.group(2)
        # Replace # at start of line with a placeholder (for section splitting)
        code_content = re.sub(r'^#', '⊡HASH⊡', code_content, flags=re.MULTILINE)
        # Replace = with a placeholder (for inline code conversion)
        code_content = code_content.replace('=', '⊡EQUALS⊡')
        return f"```{match.group(1)}\n{code_content}\n```"

    protected_content = re.sub(r'```(\w+)\n(.*?)\n```', replace_special_chars_in_codeblocks, content, flags=re.DOTALL)

    # Split by main headers (single #)
    main_sections = re.split(r'^# (.+)$', protected_content, flags=re.MULTILINE)

    if len(main_sections) > 1:
        # First section is introduction (before first #)
        intro_content = main_sections[0].strip()

        # Process remaining sections and restore special characters
        for i in range(1, len(main_sections), 2):
            if i + 1 < len(main_sections):
                section_title = main_sections[i].strip()
                section_content = main_sections[i + 1].strip()
                # Restore special characters in code blocks
                section_content = section_content.replace('⊡HASH⊡', '#')
                section_content = section_content.replace('⊡EQUALS⊡', '=')
                sections[section_title] = section_content

        # Also restore special characters in intro content
        intro_content = intro_content.replace('⊡HASH⊡', '#')
        intro_content = intro_content.replace('⊡EQUALS⊡', '=')
        return intro_content, sections

    return content, {}

def create_frontmatter(title, position):
    """Create Docusaurus frontmatter"""
    return f"""---
sidebar_position: {position}
title: {title}
---

"""

def load_config(config_file):
    """Load configuration from JSON file"""
    if not os.path.exists(config_file):
        return {}

    try:
        with open(config_file, 'r', encoding='utf-8') as f:
            return json.load(f)
    except (json.JSONDecodeError, IOError) as e:
        print(f"Warning: Could not load config file {config_file}: {e}")
        return {}

def convert_single_org_file(org_file, output_dir, title=None, position=None, link_mappings=None):
    """Convert a single org file to markdown"""
    if not os.path.exists(org_file):
        print(f"Warning: {org_file} not found, skipping")
        return False

    with open(org_file, 'r', encoding='utf-8') as f:
        org_content = f.read()

    # Convert to markdown
    md_content, extracted_title = convert_org_to_docusaurus_md(org_content, link_mappings)

    # Use extracted title if no title provided
    if not title:
        title = extracted_title or os.path.splitext(os.path.basename(org_file))[0]

    # Generate output filename
    base_name = os.path.splitext(os.path.basename(org_file))[0].lower()
    if base_name == "readme":
        output_filename = "intro.md"
        title = title or "Introduction"
    else:
        output_filename = f"{base_name}.md"

    # Create frontmatter and content
    frontmatter = create_frontmatter(title, position or 1)
    final_content = frontmatter + md_content

    # Write to output directory
    os.makedirs(output_dir, exist_ok=True)
    filepath = os.path.join(output_dir, output_filename)

    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(final_content)

    print(f"✓ Generated {filepath}")
    return True

def process_with_sections(org_file, output_dir, config):
    """Process org file with section splitting (for complex README files)"""
    if not os.path.exists(org_file):
        print(f"Warning: {org_file} not found")
        return False

    with open(org_file, 'r', encoding='utf-8') as f:
        org_content = f.read()

    # Convert to markdown
    link_mappings = config.get('link_mappings', {})
    md_content, _ = convert_org_to_docusaurus_md(org_content, link_mappings)

    # Split into sections
    intro_content, sections = split_content_by_sections(md_content)

    # Ensure output directory exists
    os.makedirs(output_dir, exist_ok=True)

    # Create introduction page
    intro_title = config.get('intro_title', 'Introduction')
    intro_frontmatter = create_frontmatter(intro_title, 1)
    intro_final = intro_frontmatter + intro_content

    intro_path = os.path.join(output_dir, "intro.md")
    with open(intro_path, 'w', encoding='utf-8') as f:
        f.write(intro_final)
    print(f"✓ Generated {intro_path}")

    # Create pages for main sections
    section_mappings = config.get('section_mappings', {})

    for section_title, section_content in sections.items():
        if section_title in section_mappings:
            filename, display_title, position = section_mappings[section_title]
            frontmatter = create_frontmatter(display_title, position)
            final_content = frontmatter + f"# {display_title}\n\n" + section_content

            filepath = os.path.join(output_dir, filename)
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(final_content)
            print(f"✓ Generated {filepath}")

def main():
    parser = argparse.ArgumentParser(
        description='Convert org files to Docusaurus-compatible markdown files',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Convert all .org files in current directory
  python org_to_docusaurus.py

  # Convert specific files
  python org_to_docusaurus.py README.org CONFIG.org

  # Use custom output directory
  python org_to_docusaurus.py -o documentation

  # Use configuration file for complex projects
  python org_to_docusaurus.py -c docusaurus-config.json

Configuration file format (JSON):
{
    "output_dir": "docs",
    "intro_title": "Introduction",
    "link_mappings": {
        "\\\\[CHINESE\\\\.org\\\\]\\\\(CHINESE\\\\.org\\\\)": "[Chinese Configuration](./chinese)"
    },
    "section_mappings": {
        "Installation": ["installation.md", "Installation", 2],
        "Usage": ["usage.md", "Usage Guide", 3]
    },
    "files": [
        {"file": "README.org", "split_sections": true},
        {"file": "CHINESE.org", "title": "Chinese Configuration", "position": 10}
    ]
}
        """
    )

    parser.add_argument('files', nargs='*', help='Org files to convert (default: all .org files)')
    parser.add_argument('-o', '--output', default='docs', help='Output directory (default: docs)')
    parser.add_argument('-c', '--config', help='Configuration file (JSON format)')
    parser.add_argument('--split-sections', action='store_true',
                       help='Split README.org into multiple files by sections')

    args = parser.parse_args()

    # Load configuration
    config = {}
    if args.config:
        config = load_config(args.config)

    output_dir = config.get('output_dir', args.output)

    # Determine files to process
    files_to_process = args.files
    if not files_to_process:
        # Auto-discover org files
        files_to_process = glob.glob("*.org")
        if not files_to_process:
            print("No .org files found in current directory")
            return

    # Process files based on configuration
    if 'files' in config:
        # Use configuration file settings
        for file_config in config['files']:
            org_file = file_config['file']
            if file_config.get('split_sections', False):
                process_with_sections(org_file, output_dir, config)
            else:
                title = file_config.get('title')
                position = file_config.get('position')
                link_mappings = config.get('link_mappings', {})
                convert_single_org_file(org_file, output_dir, title, position, link_mappings)
    else:
        # Simple mode: convert all specified files
        link_mappings = config.get('link_mappings', {})

        for i, org_file in enumerate(files_to_process, 1):
            if org_file.lower().startswith('readme') and args.split_sections:
                process_with_sections(org_file, output_dir, config)
            else:
                convert_single_org_file(org_file, output_dir, position=i*10, link_mappings=link_mappings)

if __name__ == "__main__":
    main()