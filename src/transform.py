import logging

from google.appengine.ext import db
from google.appengine.ext import deferred
from google.appengine.api import memcache

from storage import *
from transformUtils import fetchRoot
from triggerInfo import triggers

@db.transactional
def addToBag(root, path, args):
  logging.error('addToBag('+str(root)+','+str(path)+','+str(args)+')')
  set=Bag(root, root, path)
  set.add(args)

@db.transactional
def putInMap(root, path, args):
  logging.error('putInMap('+str(root)+','+str(path)+','+str(args)+')')
  map=Map(root, root, path)
  for key in args:
    value=args[key]
    map.put(key, value)

@db.transactional
def appendToList(root, path, args):
  logging.error('appendToList('+str(root)+','+str(path)+','+str(args)+')')
  l=List(root, root, path)
  l.append(args)

@db.transactional
def removeFromList(root, path, args):
  logging.error('removeFromList('+str(root)+','+str(path)+','+str(args)+')')
  l=List(root, root, path)
  l.remove(args)

@db.transactional
def insertIntoList(root, path, args):
  logging.error('insertIntoList('+str(root)+','+str(path)+','+str(args)+')')
  l=List(root, root, path)
  l.insert(args['index'], args['value'])

@db.transactional
def clearList(root, path, args):
  logging.error('clearList('+str(root)+','+str(path)+','+str(args)+')')
  l=List(root, root, path)
  l.clear()

enabledTransforms={
  'add': addToBag,
  'put': putInMap,
  'append': appendToList,
  'remove': removeFromList,
  'insert': insertIntoList,
  'clear': clearList,
}

def getDirtyViews(transforms):
  views=[]
  for transform in transforms:
    views.append(transform['path'])
  return views

def processViews(rootKey, views):
  logging.info('PROCESS VIEWS')
  root=db.get(rootKey)
  for view in views:
    obj=resolveModel(root, view)
    logging.error('processing view:')
    logging.error(obj.serialize())
    viewObj=View(name=view[0], json=obj.serialize())
    viewObj.put()
    memcache.set(viewObj.name, viewObj.json)

def triggerTransforms(root, transforms):
  logging.error('triggerTransforms')
  paths={}
  for transform in transforms:
    path=transform['path'][0]
    if path in paths:
      paths[path].append(transform)
    else:
      paths[path]=[transform]
  logging.error('paths: '+str(paths))
  for path in paths:
    if path in triggers:
      f=triggers[path]
      logging.error('TRIGGER '+str(f.func_name))
      f(paths[path])

def applyTransforms(rootKey, transforms):
  logging.info('APPLY TRANSFORMS')
  root=db.get(rootKey)
  applyTransformsInTransaction(root, transforms)

  triggerTransforms(root, transforms)

  views=getDirtyViews(transforms)
  deferred.defer(processViews, rootKey, views, _queue='views')

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
  logging.info('applyActions: '+str(transforms))
  root=fetchRoot()
  rootKey=root.key()
  deferred.defer(applyTransforms, rootKey, transforms, _queue='actions')
