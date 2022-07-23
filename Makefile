.PHONY: deploy-website
deploy-website: static
	ansible-playbook -i ansible/inventory.ini ansible/site.yml

.PHONY: build-html-generator-builder-image
build-html-generator-builder-image:
	dockers/html-generator-builder/build_docker_image.sh

generator/html_generator/html-generator: #build-html-generator-builder-image
	generator/html_generator/build.sh

.PHONY: clean-html-generator
clean-html-generator:
	generator/html_generator/build.sh clean

static: generator/html_generator/html-generator
	python3 generator/generate.py

.PHONY: clean-static
clean-static:
	-rm -rf static
