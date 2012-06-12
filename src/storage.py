import logging

from models import *
from modelInfo import *

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
      logging.error('old bag: '+str(self.entity))
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
    else:
      logging.error('Unknown type')
      return None

class Bag(Collection):
  def add(self, value):
    valueObj=self.makeValue(self.root, value)
    if valueObj:
      logging.error('entity: '+str(self.entity))
      item=Item(parent=self.root, collection=self.entity, value=valueObj)
      item.put()
