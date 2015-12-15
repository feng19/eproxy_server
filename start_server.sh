#! /bin/bash

erl -pa ebin deps/*/ebin -config server +K true -s make_proxy_server start #-detached
exit 0 
