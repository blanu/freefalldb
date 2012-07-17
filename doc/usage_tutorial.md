Step 1 - Creating a new project
===============================

. ./setEnv.sh
freefall new first_app
cd first_app

Step 2 - Defining your app
===============================

See the app tutorial for more information on this step.

Step 3 - Generating and deploying server-side code
===============================

freefall server
freefall deploy

or just:

freefall deploy (automatically runs "freefall server" first)

Step 4 - Generating client-side code
===============================

freefall py
freefall android

or just:

freefall client (automatically runs client code generation for all languages)

or just:

freefall compile (automatically generates BOTH client code for all languages and server code)

The python code is in gen/client/py/ and there should be one .py file for each action and view you configured. This provides a command line interface for interacting with your backend services.

For instance, if you defined an action report(username, score) then a program report.py would be generated which you could use like this:

python gen/client/py/report.py testUsername 1000

Similarly if you have defined a view called "scores", then a program scores.py would be generated which you could use like this:

python gen/client/py/scores.py

And which might produce output like this:

[{"username": "testUsername", "score": 1000}]

The Android code is compiled into a jar file gen/android/lib/[appname].jar. You can import this .jar file as well as the included gen/android/lib/android-json-rpc-0.3.3.jar into your Android project in order to interact with your defined actions and services from your Android app.
