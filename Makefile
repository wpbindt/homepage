.PHONY: deploy-website
deploy-website: static
	ansible-playbook -i ansible/inventory.ini ansible/site.yml

.PHONY: build-html-generator-builder-image
build-html-generator-builder-image:
	dockers/html-generator-builder/build_docker_image.sh

html-generator/html-generator: build-html-generator-builder-image
	html-generator/build.sh

.PHONY: clean-html-generator
clean-html-generator:
	html-generator/build.sh clean

static: html-generator/html-generator
	python3 scripts/generate_static.py

.PHONY: clean-static
clean-static:
	-rm -rf static
