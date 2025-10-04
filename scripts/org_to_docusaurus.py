#!/usr/bin/env python3
"""
Convert README.org to Docusaurus-compatible markdown
Preserves org format for GitHub rendering while creating clean MDX
"""

import re
import sys
import os

def convert_org_to_docusaurus_md(org_content):
    """Convert org content to Docusaurus-compatible markdown"""

    # Start with basic conversion
    md_content = org_content

    # Remove org title directive
    md_content = re.sub(r'^\#\+title:.*?\n', '', md_content, flags=re.MULTILINE)

    # Convert org headers (* -> #, ** -> ##, etc)
    md_content = re.sub(r'^\*\*\* (.+)', r'### \1', md_content, flags=re.MULTILINE)
    md_content = re.sub(r'^\*\* (.+)', r'## \1', md_content, flags=re.MULTILINE)
    md_content = re.sub(r'^\* (.+)', r'# \1', md_content, flags=re.MULTILINE)

    # Convert org links [[url][text]] to [text](url)
    md_content = re.sub(r'\[\[([^\]]+)\]\[([^\]]+)\]\]', r'[\2](\1)', md_content)

    # Convert org image links [[file:path]] to ![](path)
    md_content = re.sub(r'\[\[file:([^\]]+)\]\]', r'![](/\1)', md_content)

    # Convert bare URLs <https://...> to markdown links
    md_content = re.sub(r'<(https?://[^>]+)>', r'[\1](\1)', md_content)

    # Convert org code inline ~code~ to `code`
    md_content = re.sub(r'~([^~]+)~', r'`\1`', md_content)

    # Convert org code blocks #+begin_src to ```
    md_content = re.sub(r'^\#\+begin_src (.+)', r'```\1', md_content, flags=re.MULTILINE)
    md_content = re.sub(r'^\#\+end_src', r'```', md_content, flags=re.MULTILINE)

    # Convert org quotes
    md_content = re.sub(r'^\#\+begin_quote', r'>', md_content, flags=re.MULTILINE)
    md_content = re.sub(r'^\#\+end_quote', r'', md_content, flags=re.MULTILINE)

    # Remove org html directives
    md_content = re.sub(r'^\#\+html:.*?\n', '', md_content, flags=re.MULTILINE)

    # Remove org attr directives
    md_content = re.sub(r'^\#\+attr_org:.*?\n', '', md_content, flags=re.MULTILINE)

    # Fix image paths
    md_content = re.sub(r'images/', '/images/', md_content)

    # Clean up multiple newlines
    md_content = re.sub(r'\n{3,}', '\n\n', md_content)

    return md_content.strip()

def main():
    if len(sys.argv) != 3:
        print("Usage: python org_to_docusaurus.py input.org output.md")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    if not os.path.exists(input_file):
        print(f"Error: {input_file} not found")
        sys.exit(1)

    with open(input_file, 'r', encoding='utf-8') as f:
        org_content = f.read()

    # Convert to markdown
    md_content = convert_org_to_docusaurus_md(org_content)

    # Add Docusaurus frontmatter and title
    frontmatter = """---
sidebar_position: 1
title: Introduction
---

# paw (point-and-write)

"""

    final_content = frontmatter + md_content

    # Ensure output directory exists
    os.makedirs(os.path.dirname(output_file), exist_ok=True)

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(final_content)

    print(f"✓ Converted {input_file} to {output_file}")

if __name__ == "__main__":
    main()