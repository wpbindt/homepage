** 12-7-2022

To serve this website, we use the following server block config:
>server {
>  listen 80;
>  listen [::]:80;  
>  
>  root /var/www/www.trial.com;  
>
>  location / {
>    index index.html;
>  }
>}
The way nginx will parse a request is then as follows.

First it will match the hostname in the request headers to the server names specified in the `server_name` part of the server blocks, defaulting to the default server flagged by `default_server`. Here, there is only one server block, so no server name was specified. All requests will be served by the server specified above.

Then it will match the path part of the request uri to the location blocks inside the matched server block. Then, the uri path will be appended to the path specified in the root block, and nginx will serve the file located there. A special case is the empty path, which will result in the file specified in the index block.
