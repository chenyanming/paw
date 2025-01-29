.PHONY: clean
clean:
	rm -rf build dist *.egg-info
.PHONY: dist
dist: clean
	python3 -m build
.PHONY: upload
upload: dist
	python3 -m twine upload dist/*
