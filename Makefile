# PAW (Point-and-Write) Makefile
# Supports documentation generation and development

.PHONY: install-npm generate-docs copy-assets dev build serve clean help setup

# Generate multiple documentation files from all org files using Python script
generate-docs:
	@echo "📚 Generating documentation from org files..."
	@if which python3 > /dev/null 2>&1; then \
		python3 scripts/org_to_docusaurus.py -c docusaurus-config.json; \
	elif which python > /dev/null 2>&1; then \
		python scripts/org_to_docusaurus.py -c docusaurus-config.json; \
	else \
		echo "❌ Python not found. Please install Python to generate documentation."; \
		exit 1; \
	fi

# Documentation Management
# =======================

# Copy static assets
copy-assets:
	@echo "📁 Copying images to static directory..."
	@mkdir -p static
	@cp -r images static/ 2>/dev/null || true
	@echo "✓ Copied static assets"

# Install npm dependencies for documentation
install-npm:
	@echo "📦 Installing npm dependencies..."
	@npm install

# Development workflows
# ===================

# Start development server (auto-generates docs)
dev: generate-docs copy-assets install-npm
	@echo "🔧 Starting development server..."
	@npm run start

# Build the documentation site (auto-generates docs)
build: generate-docs copy-assets install-npm
	@echo "🏗️ Building documentation site..."
	@npm run build

# Serve the built site locally
serve: build
	@echo "🌐 Serving built site locally..."
	@npm run serve

# Utilities
# =========

# Clean all build artifacts and generated files
clean:
	@echo "🧹 Cleaning build artifacts..."
	@rm -rf build node_modules .docusaurus docs/*.md
	@echo "✓ Cleaned all artifacts"

# Show help
help:
	@echo "PAW (Point-and-Write) Emacs Module Makefile Commands"
	@echo "=================================================="
	@echo ""
	@echo "📚 Documentation:"
	@echo "  make generate-docs   - Generate docs from README.org"
	@echo "  make dev             - Start documentation dev server"
	@echo "  make build           - Build documentation site"
	@echo "  make serve           - Serve built documentation"
	@echo ""
	@echo "🔧 Setup:"
	@echo "  make install-npm     - Install npm dependencies"
	@echo "  make copy-assets     - Copy static assets"
	@echo "  make setup           - Full setup for documentation"
	@echo ""
	@echo "🧹 Utilities:"
	@echo "  make clean           - Clean all build artifacts"
	@echo "  make help            - Show this help message"
	@echo ""
	@echo "📝 Note: Python server functionality has been moved to a separate repository."

# Default target
.DEFAULT_GOAL := help

# Full setup (for new projects)
setup: generate-docs copy-assets install-npm
	@echo "✅ Full setup complete!"
	@echo "Run 'make help' to see all available commands"
