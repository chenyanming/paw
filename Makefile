# Docusaurus Documentation Makefile

.PHONY: install dev build serve clean generate-docs

# Check if pandoc is available
check-pandoc:
	@which pandoc > /dev/null || (echo "Error: pandoc is not installed. Please install pandoc first." && exit 1)

# Generate docs/intro.md from README.org
generate-docs: check-pandoc
	@echo "Generating docs/intro.md from README.org..."
	@mkdir -p docs
	@echo "---" > docs/intro.md
	@echo "sidebar_position: 1" >> docs/intro.md
	@echo "title: Introduction" >> docs/intro.md
	@echo "---" >> docs/intro.md
	@echo "" >> docs/intro.md
	@echo "# paw (point-and-write)" >> docs/intro.md
	@echo "" >> docs/intro.md
	@pandoc README.org -f org -t gfm --wrap=none | \
		sed 's|images/|/images/|g' | \
		sed 's|<img src="/images/logo.jpg" width="256" height="256">|<img src="/images/logo.jpg" width="256" height="256" alt="Paw Logo" />|' | \
		sed 's|```{=org}|<!-- org directive -->|g' | \
		sed 's|```{\..*}|```|g' | \
		sed 's|{\.verbatim}||g' | \
		sed 's|<a href="#\([^"]*\)">\([^<]*\)</a>|[\2](#\1)|g' | \
		sed 's|<https://\([^>]*\)>|\1|g' | \
		sed 's|<http://\([^>]*\)>|\1|g' | \
		sed 's|<img src="\([^"]*\)">|<img src="\1" />|g' | \
		sed 's|<img src="\([^"]*\)" alt="\([^"]*\)">|<img src="\1" alt="\2" />|g' | \
		sed 's|svg -&gt;|svg →|g' | \
		sed 's|pbm -&gt;|pbm →|g' | \
		sed 's|all-the-icons -&gt;|all-the-icons →|g' | \
		sed 's|nerd-icons -&gt;|nerd-icons →|g' | \
		sed 's|text\\. The first|text. The first|g' | \
		sed 's|~~\([^~]*\)~~|\*\*\1\*\*|g' | \
		sed 's|-\\>|→|g' | \
		tail -n +2 >> docs/intro.md
	@echo "✓ Generated docs/intro.md"

# Copy static assets
copy-assets:
	@echo "Copying images to static directory..."
	@mkdir -p static
	@cp -r images static/ 2>/dev/null || true
	@echo "✓ Copied static assets"

# Install all dependencies
install:
	npm install

# Start development server (auto-generates docs)
dev: generate-docs copy-assets install
	npm run start

# Build the documentation site (auto-generates docs)
build: generate-docs copy-assets install
	npm run build

# Serve the built site locally
serve: build
	npm run serve

# Clean build artifacts and generated files
clean:
	rm -rf build dist *.egg-info node_modules .docusaurus docs/intro.md

# Full setup (for new projects)
setup: generate-docs copy-assets install
	@echo "✓ Documentation setup complete!"
	@echo "Run 'make dev' to start development server"

# Python build targets (keeping original functionality)
.PHONY: dist-python upload-python
dist-python: clean
	python3 -m build

upload-python: dist-python
	python3 -m twine upload dist/*
