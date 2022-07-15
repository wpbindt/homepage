deploy-website:
	ansible-playbook -i ansible/inventory.ini ansible/site.yml
build-markdown-parser:
	generator/convert_markup/build.sh
