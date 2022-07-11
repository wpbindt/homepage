# ADDED USING ANSIBLE, NO TOUCHY

server {
  listen 80 default_server;
  listen [::]:80 default_server;  
  
  root /var/www/www.trial.com;  
  index index.html;  

  location / {
    try_files $uri $uri/ =404 www.trial.com;
  }
}
