deploy-website:
	ansible-playbook -i ansible/inventory.ini ansible/site.yml
build-html-generator-builder-image:
	dockers/html-generator-builder/build_docker_image.sh
build-html-generator:
	generator/html_generator/build.sh
