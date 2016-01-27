#! /bin/bash

echo heroku name:
read name
heroku create $name -s cedar
heroku config:add BUILDPACK_URL="https://github.com/heroku/heroku-buildpack-erlang.git"
git push heroku master
