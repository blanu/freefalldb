""" The transformUtils module provides utility methods for fetching objects from the database. """

import logging

from google.appengine.ext import db
from google.appengine.ext import deferred

from storage import resolveInput, resolveModel, resolveOutput
from models import Root, MapModel

def fetchRoot():
  """ fetchRoot returns the Root object for the database. """
  root=Root.all().get()
  if root:
    logging.error('old root')
    return root.value
  else:
    logging.error('new root')
    m=MapModel()
    m.put()
    root=Root(value=m)
    root.put()
    return m

def loadInput(name):
  """ loadInput returns a CollectionInput instance of the proper type for the model with the specified name. """
  root=fetchRoot()
  return resolveInput(root, [name])

def loadModel(name):
  """ loadInput returns a Collection instance of the proper type for the model with the specified name. """
  root=fetchRoot()
  return resolveModel(root, [name])

def loadOutput(name, state):
  """ loadInput returns a TransactionMonad instance of the proper type for the model with the specified name. """
  logging.error('load output: '+str(state))
  root=fetchRoot()
  return resolveOutput(root, [name], state)
