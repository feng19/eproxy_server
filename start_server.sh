#! /bin/bash

#erl -pa ebin deps/*/ebin -config server +K true -s eproxy_server start -detached
erl -pa ebin deps/*/ebin -config server +K true -s eproxy_server start

exit 0 
