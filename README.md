# Static homepage
This repo contains the content of my homepage. It is deployed automatically on every push to to `main`, using ansible. The relevant playbooks are found in the `ansible` directory, and the deploy script in `dockers/ansible/scripts`.

The content is located in `my-homepage`, and is written in a custom markup language. This is converted to HTML using the `make static`, which runs the application in `html-generator`. It parses the content as follows. It iterates over the subdirectories (not recursively) of the `my-homepage` directory, and convert each of the files in there to HTML. Each subdirectory gets its own section in the index page, with links pointing to the converted files (which live in a generated subdirectory corresponding to the section).
