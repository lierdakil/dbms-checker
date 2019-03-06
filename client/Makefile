MY_FILES=$(shell git ls-files .)

.PHONY: all
all: dist

.PHONY: dist
dist:
	rm -r dist/ || echo
	npm install
	npm install --only=dev
	npm run build
