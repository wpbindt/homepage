# ADDED USING ANSIBLE, NO TOUCHY

server {
  listen 80;
  listen [::]:80;  
  
  root /var/www/www.trial.com;  

  location / {
    index index.html;
  }
}
