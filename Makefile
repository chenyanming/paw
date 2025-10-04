# Docusaurus Documentation Makefile

.PHONY: install dev build serve clean generate-docs

# Check if python is available
check-python:
	@which python3 > /dev/null || (echo "Error: python3 is not installed. Please install python3 first." && exit 1)

# Generate multiple documentation files from README.org using Python script
generate-docs: check-python
	@echo "Generating documentation from README.org..."
	@python3 scripts/org_to_docusaurus.py README.org

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
	rm -rf build dist *.egg-info node_modules .docusaurus docs/*.md

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
