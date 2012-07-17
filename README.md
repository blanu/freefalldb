freefalldb
==========

Scalable NoSQL database for mobile apps

Usage
==========
. ./setEnv.sh
freefall new first_app
cd first_app
freefall compile
freefall deploy

Command Reference
==========
freefall new [appname] - create a new app skeleton
freefall compile - generate both client (for all languages) and server code
freefall server - generate server code
freefall deploy - deploy server code to Google App Engine
freefall client - generate client code for all languages
freefall py - generate python client code
freefall android - generate Android client code and compile it into a .jar file
