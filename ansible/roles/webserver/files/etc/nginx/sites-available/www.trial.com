# ADDED USING ANSIBLE, NO TOUCHY

server {
  listen 80;
  listen [::]:80;  
  
  root /var/www/www.trial.com;  
  index index.html;  

  location / {
    try_files $uri $uri/ =404 www.trial.com;
  }
}
