eproxy_server(eps)
==================

## Introduction ##

* this app use for cross the GFW
* and this is just a server
* so you must have a client([eproxy_client](https://github.com/feng19/eproxy_client)) in your computer

## Feature ##

* protocol encrypt

## Usage ##
* install [heroku](https://www.heroku.com/) toolbelt
* make & setting
```
    git clone git://github.com/feng19/eproxy_server
    cd eproxy_server
    make
    vi server.config(setting encrypt key)
    ./deploy.sh
    input your heroku app name
    wait for save the server address
```
* setting server address and the encrypt key to your client.config
* open your client and use
