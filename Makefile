deploy-website:
	ansible-playbook -i ansible/inventory.ini ansible/site.yml
