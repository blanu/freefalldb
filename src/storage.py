import logging

from models import *
from modelInfo import *
from model import *

def resolveInput(root, path):
  name=path[0]
  if name in modelNames:
    if modelTypes[name]=='bag':
      return BagInput(root, root, path)
    elif modelTypes[name]=='map':
      return MapInput(root, root, path)
    else:
      logging.error('Unknown resolve type '+str(modelInfo[name]))
  else:
    logging.error('Unknown model '+str(name))  

def resolveModel(root, path):
  name=path[0]
  if name in modelNames:
    if modelTypes[name]=='bag':
      return Bag(root, root, path)
    elif modelTypes[name]=='map':
      return Map(root, root, path)
    else:
      logging.error('Unknown resolve type '+str(modelInfo[name]))
  else:
    logging.error('Unknown model '+str(name))
    
def resolveOutput(root, path):
  name=path[0]
  if name in modelNames:
    if modelTypes[name]=='bag':
      return BagTransaction(path)
    elif modelTypes[name]=='map':
      return MapTransaction(path)
    else:
      logging.error('Unknown resolve type '+str(modelInfo[name]))
  else:
    logging.error('Unknown model '+str(name))

class CollectionInput:
  def __init__(self, root, container, path):
    self.root=root
    self.container=container
    self.entity=None
    
    self.path=path
    name=path[0]

    item=MapItem.all().ancestor(self.root).filter("collection =", self.container).filter('index =', name).get()
    if item:
      self.entity=item.value.value
      
class BagInput(CollectionInput):
  pass
  
class MapInput(CollectionInput):
  def get(self, key):
    value=MapItem.all().ancestor(self.root).filter("collection =", self.entity).filter("index =", key).get()
    if value:
      return value.value
    else:
      return None

class ListInput(CollectionInput):
  def get(self, index):
    value=ListItem.all().ancestor(self.root).filter("collection =", self.entity).filter("index =", index).get()
    if value:
      return value.value
    else:
      return None

class Collection:
  root=None
  container=None
  path=None
  entity=None

  def __init__(self, root, container, path):
    self.root=root
    self.container=container
    
    self.path=path
    name=path[0]

    item=MapItem.all().ancestor(self.root).filter("collection =", self.container).filter('index =', name).get()
    logging.error('item: '+str(item))
    if item:
      self.entity=item.value.value
      logging.error('old collection: '+str(self.entity))
    else:
      if name in modelNames:
        if modelTypes[name]=='bag':
          bag=BagModel(parent=self.root)
          bag.put()
          val=CollectionValue(parent=root, value=bag)
          val.put()
          item=MapItem(parent=self.root, collection=self.container, index=name, value=val)
          item.put()
          self.entity=bag
          logging.error('new bag: '+str(self.entity))
        elif modelTypes[name]=='map':
          map=MapModel(parent=self.root)
          map.put()
          val=CollectionValue(parent=root, value=map)
          val.put()
          item=MapItem(parent=self.root, collection=self.container, index=name, value=val)
          item.put()
          self.entity=map
          logging.error('new map: '+str(self.entity))
    
  def makeValue(self, root, value):
    logging.error('makeValue('+str(type(value))+':'+str(value)+')')
    if type(value)==str:
      obj=StringValue(parent=root, value=value)
      obj.put()
      return obj
    elif type(value)==float:
      obj=FloatValue(parent=root, value=value)
      obj.put()
      return obj
    elif type(value)==dict:
      colVal=self.makeMap(root, value)
      obj=CollectionValue(parent=root, value=colVal)
      obj.put()
      return obj
    elif type(value)==list:
      colVal=self.makeList(root, value)
      obj=CollectionValue(parent=root, value=colVal)
      obj.put()
      return obj
    else:
      logging.error('Unknown type')
      return None
      
  def makeMap(self, root, map):
    c=MapModel(parent=root)
    c.put()
    for key in map:
      value=map[key]
      item=MapItem(parent=root, collection=c, index=key, value=self.makeValue(root, value))
      item.put()
    return c
    
  def makeList(self, root, list):
    c=ListModel(parent=root)
    c.put()
    for index in range(len(list)):
      value=list[index]
      item=ListItem(parent=root, collection=c, index=index, value=self.makeValue(root, value))
      item.put()
    return c
    
  def serialize(self):
    return self.serializeValue(self.entity)
    
  def serializeValue(self, value):
    if type(value)==BagModel:
      s='['
      items=Item.all().ancestor(self.root).filter('collection =', value).fetch(10)
      for item in items:
        s=s+self.serializeValue(item.value)+','
      s=s[:-1]
      s=s+']'
    elif type(value)==ListModel:
      s='['
      items=ListItem.all().ancestor(self.root).filter('collection =', value).fetch(10)
      for item in items:
        s=s+self.serializeValue(item.value)+','
      s=s[:-1]
      s=s+']'
    elif type(value)==MapModel:
      s='{'
      items=MapItem.all().ancestor(self.root).filter('collection =', value).fetch(10)
      for item in items:
        s=s+'"'+str(item.index)+'"'+':'+self.serializeValue(item.value)+','
      s=s[:-1]
      s=s+'}'
    elif type(value)==StringValue:
      s='"'+str(value.value)+'"'
    elif type(value)==FloatValue:
      s=str(value.value)
    elif type(value)==CollectionValue:
      s=self.serializeValue(value.value)
    else:
      print('Unknown serialize type: '+str(type(value)))
      return ''
    return s

class Bag(Collection):
  def add(self, value):
    valueObj=self.makeValue(self.root, value)
    if valueObj:
      logging.error('entity: '+str(self.entity))
      item=Item(parent=self.root, collection=self.entity, value=valueObj)
      item.put()

class Map(Collection):
  def put(self, key, value):
    logging.error('map.put: '+str(key)+':'+str(value))
    valueObj=self.makeValue(self.root, value)
    if valueObj:
      logging.error('entity: '+str(self.entity))
      logging.error('root: '+str(self.root))
      logging.error('query: '+str(self.root.key())+' '+str(self.container.key())+' '+str(key))
      item=MapItem.all().ancestor(self.root).filter("collection =", self.entity).filter('index =', str(key)).get()
      logging.error('found map item?: '+str(item))
      if item:
        item.value=valueObj
      else:
        item=MapItem(parent=self.root, collection=self.entity, index=key, value=valueObj)
      item.put()
