import logging

from google.appengine.ext import db
from google.appengine.ext import deferred

from storage import resolveInput, resolveModel, resolveOutput
from models import Root, MapModel

def fetchRoot():
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
  root=fetchRoot()
  return resolveInput(root, [name])

def loadModel(name):
  root=fetchRoot()
  return resolveModel(root, [name])

def loadOutput(name):
  root=fetchRoot()
  return resolveOutput(root, [name])
