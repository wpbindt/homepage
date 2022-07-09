deploy-website:
	ansible-playbook -vvv -i ansible/inventory.ini ansible/site.yml
