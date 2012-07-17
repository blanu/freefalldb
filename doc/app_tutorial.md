This tutorial is going to walk you through creating a very simple leaderboard application in which clients can submit scores and you can get a sorted list of the scores.

Step 1 - A new project
===============================

After you run "freefall new leaderboard", a directory called "leaderboard" will be created containing a skeleton for your project. The skeleton directory structure looks like this:

leaderboard/
    config/
        config.yaml
    app/
        actions/
            testAction.py
        transforms/
            identity.py
        lib/
            generic.py

Step 2 - Configuring your app
===============================

The skeleton config.yaml file looks like this:

appname: example
package: com.example
version: 1
actions:
  testAction:
  - string
transforms:
  copy:
    triggers:
      - testModel
    output: testView
views:
  - testView
models:
  - name: testModel
    type: bag
  - name: testView
    type: bag

The appname should be the name of your app. This is used in a number of places, from the name of the App Engine app which is deployed to the name of the client .jar file which is provided for Android.

The package is only used in the Android client. It is the Java package that will be used for all of the generated classes.

The version is used by the App Engine app.

Let's change this to have the appname of "leaderboard" and the package "com.example.leaderboard". We'll delete the example actions, transforms, views, and models to make room for the tutorial content. Afterwards, config.yaml should look like this:

appname: leaderboard
package: com.leaderboard
version: 1
actions:
transforms:
views:
models:

Most of the work of configuration is defining your actions, views, and models. These define the backend services which will be available to clients. Documentation on defining these is provided in further sections.

Step 3 - Defining your models
===============================

The core of your database is its models. Models represent the state stored in the database. Everything else, such as actions, transforms, and views, modify models. The models are defined in the "models" section of your config.yaml file. Each model has a name and a type. The supported types are bag, map, and list. When you first access a model, it is automatically created if it does not already exist. Once it has been created, the type cannot be changed.

The bag type for models is an unordered, unindexed collection. The supported actions on a bag are just adding items to the bag and retrieving the entire contents of the bag. There is no way to access individual items in the bag as the collection is both unordered and unindexed. Bags may not initially seem very useful, but they have one very useful and important property, which is that adding items to a bag will never caused a collision. This means that writing to bags can be parallelized very well. Bags are therefore appropriate for high-volume writes. For instance, in a leaderboard service where you want to record every score ever made you can use a bag and then later use a transform to produce a list of high scores from the scores in the bag.

The map type is an unordered, indexed collection which can take strings as keys and any type as a value, including other collections. Values can be of a mixed type. For instance, a map can have some values which are strings and some which are floats. The map supported setting the value for a given key and fetching the value associated with a given key. The entire map can also be fetched. Collisions in maps occur only when setting the value for the same key. A key can only have one value associated with it, so simultaneous modification of the same key will cause a collision.

The list type is an ordered, indexed collection which associates float keys with any type as a value, including other collections. Values can be of a mixed type. For instance, a map can have some values which are strings and some which are floats. Unlike a traditional list, the list type has some special properties which make it more parallelizable. The list type is similar to the map type except with float keys instead of string keys and with multiple values allowed per key. Values are inserted into the list with float keys and when the list is fetched the values are ordered according to their keys. Values with the same key will be in random order with respect to each other. These unique properties allow for lists to be free of collision on writes and efficient to use.

For our example application we will add a bag called "unsortedScores" to store scores reported by the client. We'll also add a list called "sortedScores" to store the sorted score list. We'll talk more about this list later. The models section of config.yaml should look like this:

models:
  - name: unsortedScores
  - type: bag
  - name: sortedScores
  - type: list

Step 4 - Defining your views
===============================

Views are part of the public API of your app. Views are read-only and are generated from models. They represent the output of your backend services. For instance, in a leaderboard service you might want a "sortedScores" service that gives you a sorted list of scores.

The first step in defining a view is to define a corresponding model. For more on this, see "Step 3 - Defining your models". In order to mark a particular model as a view, just add it to the views list. For instance, to make the "sortedScores" model into a view the config.yaml should look like this:

views:
  - sortedScores

That's all you need to do to define a view. Creating views doesn't require writing any python code.

Once you define a view, it will show up in the client API. For instance, if you were to run "freefall py" it would generate a file called "gen/client/py/sortedScores.py". If you were to run this program, it would display the JSON encoding of the contents of the sortedScores model. The Android code will also contain a method "sortedScores()" which you can call from an Android program to get the contents of the sortedScores model.

Step 5 - Defining your actions
===============================

An action is part of the public API of your app which is available to clients. Actions allow the client to make changes to the models stored by the database. In MVC terminology, actions are controllers.

In order to define an action, first add it to your config.yaml in the actions section. You need the name of the action and a list of types for the parameters. Valid types are string, float, map, and list.

For example, if you were to add an action called "report" with a string parameter and a float parameter, the actions section of your config.yaml would now look like this:

actions:
  report:
  - string
  - float

The next step is to add the action function to first_app/app/actions/[actionName].py. For instance, the "report" action would go in first_app/app/actions/report.py.

Actions are python functions. They use the parameters given by the client to make modifications to the models stored by the database. Here is the example code for the report function:

def report(id, score):
  state.add(['unsortedScores'], {'id': id, 'score': score})

Notice that the function is named the same as the name of the action. The arguments to the function can be named whatever you like, but there should be the same number of arguments as you specified in the config.yaml. All action functions also have access to a special variable called "state" which allows you to modify the database models. In this case we want to add the arguments to the bag called "unsortedScores". So we call state.add(). The first argument is the path to the model we want to modify. The second argument is the value we want to add to the database. The value should be a JSON-compatible value, so a string, float, dict with string keys and JSON-compatible values, or list with JSON-compatible values.

Step 6 - Defining your transforms
=================================

A transform is not part of the public API of your app and is not available to clients. Transforms occur internally to your backend service. They are triggered by changes in models in the database and in turn create changes in other models. The changes propagate through a data flow graph with the changes originating in actions and ending in views.

In order to define a transform, first add it to your config.yaml in the transforms section. You need the name of the transform, a list of models which act as triggers to cause the transform to run, a list of models which act as input to the transform function, and an output model.

For example, consider if you have a model containing unsorted scores, for instance in a bag, perhaps called "unsortedScores". New scores are added to the bag when they are eported. Now you want to create a sorted list of the scores for a leaderboard, so you create a new model of type "list" called "sortedScores". You need a transform which will generate sorted scores from the unsorted scores. The trigger would be "unsortedScores" as you only need to run the transform to generate sorted scores when new scores are added to the bag of unsorted scores. The output would be "sortedScores" as that's the model you created to store sortedScores. However, there is one more part to this transform, which is that the new sorted scores depends not only on the new score that was added, but also on the old set of sorted scores. You don't want to resort all scores everytime a new one is added, you just want to insert the new score into its proper place in an already mostly sorted list. Therefore, you must add "sortedScores" as an input to the "sort" function.

So, if you were to add a transform called "sort" with a trigger of "unsortedScores", an input of "sortedScores", and an output of "sortedScores", the actions section of your config.yaml would now look like this:

transforms:
  sort:
    triggers:
    - unsortedScores
    inputs:
    - sortedScores
    output: sortedScores

Also don't forget to define the models in the "models" section. For instance, if "unsortedScores" is a new model that you're introducing then you will need to decide on its type and add a section for it in "models". The input and trigger models also need to be defined, of course.

The next step is to add the transform function to first_app/app/transforms/[transformName].py. For instance, the "sort" transform would go in first_app/app/transforms/sort.py.

Transforms are python functions. They are run when the trigger models are changed and use the input models to determine changes to make to the output model. For instance, the "sort" transform would run when the unsorted scores change (i.e. a new score has been submitted), have access to the old set of sorted scores, and produce as output changes to the sorted scores model.

Here is the code for the sort transform:

def sort(unsortedScoreChanges, oldSortedScores, newSortedScores):
  item=unsortedScoreChanges[0]['args']
  playerid=item['id']
  score=item['score']

  newSortedHighScores.remove(playerid)
  newSortedHighScores.insert(score, playerid)

As you can see, the function has the same name as the transform. The arguments correspond to the triggers, inputs, and output configured in config.yaml. The first argument, "unsortedScoreChanges" corresponds to the trigger and contains a list of changes which have been made to the "unsortedScores" model. The second argument, "oldSortedScores" corresponds to the input model and contains all of the current contents of the "sortedScores" model. The third argument, "newSortedScores", corresponds to the output model and can be modified to make changes to that model.

There are several imporant things to note here. First, the trigger inputs such as "unsortedScoreChanges" contain ONLY the changes that have just been made to the model, not the rest of the contents of the model. If you want access to the whole model, you need to add it as an input. Second, the input models are read-only while the output model is write-only. If you want to both read and write a model then you need to add the model as both an input and an output. Finally, while you can have multiple input models, you can only have one output model.

Step 7 - Putting it all together
================================

Our finished config.yaml should look like this:

appname: leaderboard
package: com.leaderboard
version: 1
actions:
  report:
  - string
  - float
transforms:
  sort:
    triggers:
    - unsortedScores
    inputs:
    - sortedScores
    output: sortedScores
views:
  - sortedScores
models:
  - name: unsortedScores
  - type: bag
  - name: sortedScores
  - type: list

The app directory should look like this:

leaderboard/
    config/
        config.yaml
    app/
        actions/
            report.py
        transforms/
            sort.py
        lib/
            generic.py

If you run "freefall py", you should have these files:

leaderboard/
    gen/
        client/
            py/
                report.py
                sortedScores.py

To recap the functionality of our leaderboard application, the "report" action lets clients report new scores. These are stored in the "unsortedScores" bag. The "sort" transform is then triggered, inserting the scores into the "sortedScores" list. This model is also configured to be a view, so clients can access the "sortedScores" view to access the list of sorted scores. So now you have a very simple leaderboard applications!

Run "freefall deploy" to generate the server-side code and deploy it to Google App Engine (you will need to have configured your App Engine account and created an App Engine service with the same name as your app). Then you can run the following commands to see your leaderboard in action:

python gen/client/py/report.py user1 1000
python gen/client/py/report.py user2 3000
python gen/client/py/sortedScores.py
