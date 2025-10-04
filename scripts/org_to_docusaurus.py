#!/usr/bin/env python3
"""
Convert README.org to multiple Docusaurus-compatible markdown files
Splits content into logical sections for better documentation structure
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

    # Fix specific broken links
    md_content = re.sub(r'\[README_PAW_CLI\]\(README_PAW_CLI\.md\)', r'[README_PAW_CLI](https://github.com/chenyanming/paw/blob/main/README_PAW_CLI.md)', md_content)

    # Fix malformed URLs with > at the end
    md_content = re.sub(r'http://localhost:5001>', 'http://localhost:5001', md_content)

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

def split_content_by_sections(content):
    """Split content into logical sections"""
    sections = {}

    # Split by main headers (single *)
    main_sections = re.split(r'^# (.+)$', content, flags=re.MULTILINE)

    if len(main_sections) > 1:
        # First section is introduction (before first #)
        intro_content = main_sections[0].strip()

        # Process remaining sections
        for i in range(1, len(main_sections), 2):
            if i + 1 < len(main_sections):
                section_title = main_sections[i].strip()
                section_content = main_sections[i + 1].strip()
                sections[section_title] = section_content

        return intro_content, sections

    return content, {}

def create_frontmatter(title, position):
    """Create Docusaurus frontmatter"""
    return f"""---
sidebar_position: {position}
title: {title}
---

"""

def main():
    if len(sys.argv) != 2:
        print("Usage: python org_to_docusaurus.py input.org")
        sys.exit(1)

    input_file = sys.argv[1]
    docs_dir = "docs"

    if not os.path.exists(input_file):
        print(f"Error: {input_file} not found")
        sys.exit(1)

    with open(input_file, 'r', encoding='utf-8') as f:
        org_content = f.read()

    # Convert to markdown
    md_content = convert_org_to_docusaurus_md(org_content)

    # Split into sections
    intro_content, sections = split_content_by_sections(md_content)

    # Ensure docs directory exists
    os.makedirs(docs_dir, exist_ok=True)

    # Create introduction page
    intro_frontmatter = create_frontmatter("Introduction", 1)
    intro_final = intro_frontmatter + "# paw (point-and-write)\n\n" + intro_content

    with open(f"{docs_dir}/intro.md", 'w', encoding='utf-8') as f:
        f.write(intro_final)
    print("✓ Generated docs/intro.md")

    # Create pages for main sections
    section_mapping = {
        "Get started": ("getting-started.md", "Getting Started", 2),
        "Installation": ("installation.md", "Installation", 3),
        "What can paw do?": ("usage.md", "Usage Guide", 4),
        "Configuration": ("configuration.md", "Configuration", 5),
        "Database Synchronization": ("database.md", "Database & Sync", 6),
        "User Discussions": ("community.md", "Community", 7),
        "References": ("references.md", "References", 8),
    }

    for section_title, section_content in sections.items():
        if section_title in section_mapping:
            filename, display_title, position = section_mapping[section_title]
            frontmatter = create_frontmatter(display_title, position)
            final_content = frontmatter + f"# {display_title}\n\n" + section_content

            filepath = f"{docs_dir}/{filename}"
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(final_content)
            print(f"✓ Generated docs/{filename}")

if __name__ == "__main__":
    main()