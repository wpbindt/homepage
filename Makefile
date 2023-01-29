.PHONY: deploy-website
deploy-website:
	ansible/deploy.sh

.PHONY: deploy-image
deploy-image:
	dockers/ansible/build_docker_image.sh

.PHONY: build-html-generator-builder-image
build-html-generator-builder-image:
	dockers/html-generator-builder/build_docker_image.sh

html-generator/html-generator: build-html-generator-builder-image
	html-generator/build.sh

.PHONY: clean-html-generator
clean-html-generator:
	html-generator/build.sh clean

static: html-generator/html-generator
	html-generator/html-generator content

.PHONY: clean-static
clean-static:
	-rm -rf static
