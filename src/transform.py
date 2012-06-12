import logging

from google.appengine.ext import db
from google.appengine.ext import deferred

from storage import *

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
    
@db.transactional
def addToBag(root, path, args):
  logging.error('addToBag('+str(root)+','+str(path)+','+str(args)+')')
  set=Bag(root, root, path)
  set.add(args)

enabledTransforms={
  'add': addToBag
}

def applyTransforms(rootKey, transforms):
  root=db.get(rootKey)
  applyTransformsInTransaction(root, transforms)

@db.transactional
def applyTransformsInTransaction(root, transforms):
  logging.error('applyTransforms: '+str(transforms))
  for t in transforms:
    applyTransform(root, t)
    
@db.transactional
def applyTransform(root, transform):
  type=transform['type']
  path=transform['path']
  args=transform['args']
  f=enabledTransforms[type]
  if f!=None:
    f(root, path, args)

def applyAction(transforms):
  root=fetchRoot()
  rootKey=root.key()
  deferred.defer(applyTransforms, rootKey, transforms, _queue='actions')
  
#def applyTransform(transforms):
#  deferred.defer(applyTransforms, transforms, _queue='transforms')
