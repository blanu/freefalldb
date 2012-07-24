""" The storage module provides classes and utility methods for loading objects from the App Engine database. """

import logging

from models import *
from modelInfo import *
from model import *

def resolveInput(root, path):
  """
  resolveInput returns a CollectionInput of the proper type for the given path, looked up from the given root.
  The type of the returned object is determined by the model types defined in config.yaml.
  """
  name=path[0]
  if name in modelNames:
    if modelTypes[name]=='bag':
      return BagInput(root, root, path)
    elif modelTypes[name]=='map':
      return MapInput(root, root, path)
    elif modelTypes[name]=='list':
      return ListInput(root, root, path)
    else:
      logging.error('Unknown resolve type '+str(modelTypes[name]))
  else:
    logging.error('Unknown model '+str(name))

def resolveModel(root, path):
  """
  resolveModel returns a Collection of the proper type for the given path, looked up from the given root.
  The type of the returned object is determined by the model types defined in config.yaml.
  """
  logging.info('resolveModel: '+str(path))
  name=path[0]
  if name in modelNames:
    if modelTypes[name]=='bag':
      return Bag(root, root, path)
    elif modelTypes[name]=='map':
      return Map(root, root, path)
    elif modelTypes[name]=='list':
      return List(root, root, path)
    else:
      logging.error('Unknown resolve type '+str(modelTypes[name]))
  else:
    logging.error('Unknown model '+str(name))

def resolveOutput(root, path):
  """
  resolveOutput returns a TransactionMonad of the proper type for the given path, looked up from the given root.
  The type of the returned object is determined by the model types defined in config.yaml.
  """
  name=path[0]
  if name in modelNames:
    if modelTypes[name]=='bag':
      return BagTransaction(path)
    elif modelTypes[name]=='map':
      return MapTransaction(path)
    elif modelTypes[name]=='list':
      return ListTransaction(path)
    else:
      logging.error('Unknown resolve type '+str(modelTypes[name]))
  else:
    logging.error('Unknown model '+str(name))

class CollectionInput:
  """
  CollectionInput is the base class for the different types of input classes.
  Each CollectionInput class provides read-only methods for accessing models.
  """
  def __init__(self, root, container, path, entity=None):
    """ Set the root, container, and path properties. If entity is included, use it, otherwise look up the path using the given root to find the entity. """
    self.root=root
    self.container=container

    if entity:
      self.entity=entity
    else:
      self.entity=None

      self.path=path
      name=path[0]

      item=MapItem.all().ancestor(self.root).filter("collection =", self.container).filter('index =', name).get()
      if item:
        self.entity=item.value.value

class BagInput(CollectionInput):
  """ BagInput is the CollectionInput class for providing read-only access to bag models. """
  pass

class MapInput(CollectionInput):
  """
  MapInput is the CollectionInput class for providing read-only access to map models.
  It allows for getting a value from a map given a key.
  """
  def get(self, key):
    """ Get the value from a map model given the key. """
    return self[key]

  def __getitem__(self, index):
    """ Get the value from a map model given the key. """
    value=MapItem.all().ancestor(self.root).filter("collection =", self.entity).filter("index =", index).get()
    if value:
      logging.error('Collection found??? '+str(type(value.value)))
      if type(value.value)==CollectionValue:
        logging.error('Collection found!!! '+str(type(value.value.value)))
        if type(value.value.value)==BagModel:
          return BagInput(self.root, self.entity, [index], entity=value.value.value)
        elif type(value.value.value)==MapModel:
          return MapInput(self.root, self.entity, [index], entity=value.value.value)
        elif type(value.value.value)==ListModel:
          return ListInput(self.root, self.entity, [index], entity=value.value.value)
        else:
          logging.error('Unknown collection value type: '+str(type(value.value.value)))
          return value.value.value
      else:
        return value.value
    else:
      return None

class ListInput(CollectionInput):
  """ ListInput is the CollectionInput class for providing read-only access to list models. """
  def __getitem__(self, index):
    """ Get the value from a list given the index. """
    value=ListItem.all().ancestor(self.root).filter("collection =", self.entity).filter("index =", index).get()
    if value:
      logging.error('Collection found??? '+str(type(value.value)))
      if type(value.value)==CollectionValue:
        logging.error('Collection found!!! '+str(type(value.value.value)))
        if type(value.value.value)==BagModel:
          return BagInput(self.root, self.entity, [index], entity=value.value.value)
        elif type(value.value.value)==MapModel:
          return MapInput(self.root, self.entity, [index], entity=value.value.value)
        elif type(value.value.value)==ListModel:
          return ListInput(self.root, self.entity, [index], entity=value.value.value)
        else:
          logging.error('Unknown collection value type: '+str(type(value.value.value)))
          return value.value.value
      else:
        return value.value
    else:
      return None

  def __len__(self):
    """ Return the length of the list. """
    try:
      return int(ListItem.all().ancestor(self.root).filter("collection =", self.entity).count())
    except Exception, e:
      logging.error('Length error: '+str(e))
      return 0

  def tuples(self):
    """ Return a python list where each item in the list is a tuple (index,value) containing the index of the list position and the value at that position, in the same order as the referenced list model. """
    results=[]
    logging.info('tuples root: '+str(self.root))
    logging.info('tuples coll: '+str(self.container))
    logging.info('# of tuples: '+str(ListItem.all().ancestor(self.root).filter('collection =', self.entity).count()))
    items=ListItem.all().ancestor(self.root).filter('collection =', self.entity).order('index').run()
    for item in items:
      results.append((item.index, item.value.value))
    return results

class Collection:
  """ Collection is the base class for the classes which provide direct access to modify database models. """
  root=None
  container=None
  path=None
  entity=None

  def __init__(self, root, container, path):
    """
    Set the root, container, and path properties. If entity is included, use it, otherwise look up the path using the given root to find the entity.
    If an object does not exist at the given path, create it using the type information provided in the models section of config.yaml.
    """
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
          val=CollectionValue(parent=root, collection=self.container, value=bag)
          val.put()
          item=MapItem(parent=self.root, collection=self.container, index=name, value=val)
          item.put()
          self.entity=bag
          logging.error('new bag: '+str(self.entity))
        elif modelTypes[name]=='map':
          map=MapModel(parent=self.root)
          map.put()
          val=CollectionValue(parent=root, collection=self.container, value=map)
          val.put()
          item=MapItem(parent=self.root, collection=self.container, index=name, value=val)
          item.put()
          self.entity=map
          logging.error('new map: '+str(self.entity))
        elif modelTypes[name]=='list':
          l=ListModel(parent=self.root)
          l.put()
          val=CollectionValue(parent=root, collection=self.container, value=l)
          val.put()
          item=MapItem(parent=self.root, collection=self.container, index=name, value=val)
          item.put()
          self.entity=l
          logging.error('new list: '+str(self.entity))
        else:
          logging.error('Unknown collection type: '+str(modelTypes[name]))

  def makeValue(self, root, container, value):
    """ Create and store in the database an object representing the given value, of a compatible type with the type of value and contained in the given container. """
    logging.error('makeValue('+str(type(value))+':'+str(value)+')')
    if type(value)==str:
      obj=StringValue(parent=root, collection=container, value=value)
      obj.put()
      return obj
    elif type(value)==unicode:
      obj=StringValue(parent=root, collection=container, value=str(value))
      obj.put()
      return obj
    elif type(value)==float:
      obj=FloatValue(parent=root, collection=container, value=value)
      obj.put()
      return obj
    elif type(value)==int:
      obj=FloatValue(parent=root, collection=container, value=float(value))
      obj.put()
      return obj
    elif type(value)==long:
      obj=FloatValue(parent=root, collection=container, value=float(value))
      obj.put()
      return obj
    elif type(value)==dict:
      colVal=self.makeMap(root, value)
      obj=CollectionValue(parent=root, collection=container, value=colVal)
      obj.put()
      return obj
    elif type(value)==list:
      colVal=self.makeList(root, value)
      obj=CollectionValue(parent=root, collection=container, value=colVal)
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
      item=MapItem(parent=root, collection=c, index=key, value=self.makeValue(root, c, value))
      item.put()
    return c

  def makeList(self, root, list):
    c=ListModel(parent=root)
    c.put()
    for index in range(len(list)):
      value=list[index]
      item=ListItem(parent=root, collection=c, index=index, value=self.makeValue(root, c, value))
      item.put()
    return c

  def serialize(self):
    """ Serialize this collection to JSON. """
    return self.serializeValue(self.entity)

  def serializeValue(self, value):
    """ Serialize the given value to JSON. """
    if type(value)==BagModel:
      s='['
      count=Item.all().ancestor(self.root).filter('collection =', value).count()
      if count>0:
        items=Item.all().ancestor(self.root).filter('collection =', value).run()
        for item in items:
          s=s+self.serializeValue(item.value)+','
        s=s[:-1]
      s=s+']'
    elif type(value)==ListModel:
      s='['
      count=ListItem.all().ancestor(self.root).filter('collection =', value).order('index').count()
      if count>0:
        items=ListItem.all().ancestor(self.root).filter('collection =', value).order('index').run()
        for item in items:
          s=s+self.serializeValue(item.value)+','
        s=s[:-1]
      s=s+']'
    elif type(value)==MapModel:
      s='{'
      count=MapItem.all().ancestor(self.root).filter('collection =', value).count()
      if count>0:
        items=MapItem.all().ancestor(self.root).filter('collection =', value).run()
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
  """ Bag provides direct access to modify bag type models in the database. """
  def add(self, value):
    """ Add the given value to the bag with the stored path. """
    valueObj=self.makeValue(self.root, self.entity, value)
    if valueObj:
      logging.error('entity: '+str(self.entity))
      item=Item(parent=self.root, collection=self.entity, value=valueObj)
      item.put()

class Map(Collection):
  """ Map provides direct access to modify map type models in the database. """
  def put(self, key, value):
    """ Put the given value with the specified key into the map model found at the stored path. """
    logging.error('map.put: '+str(key)+':'+str(value))
    valueObj=self.makeValue(self.root, self.entity, value)
    if valueObj:
      logging.error('entity: '+str(self.entity))
      logging.error('root: '+str(self.root))
      logging.error('query: '+str(self.root.key())+' '+str(self.container.key())+' '+str(key))
      item=MapItem.all().ancestor(self.root).filter("collection =", self.entity).filter('index =', str(key)).get()
      logging.error('found map item?: '+str(item))
      if item:
        #FIXME - Recursive delete necessary for collections
        item.value.delete()
        item.value=valueObj
      else:
        item=MapItem(parent=self.root, collection=self.entity, index=str(key), value=valueObj)
      item.put()

class List(Collection):
  """ List provides direct access to modify list type models in the database. """
  # FIXME - this is broken in the new list implementation
  def append(self, value):
    """ Append the given value to the list model found at the stored path. """
    valueObj=self.makeValue(self.root, self.entity, value)
    key=ListItem.all().ancestor(self.root).filter("collection =", self.entity).count()
    if valueObj:
      logging.error('entity: '+str(self.entity))
      item=ListItem.all().ancestor(self.root).filter("collection =", self.entity).filter('index =', int(key)).get()
      if item:
        item.value=valueObj
      else:
        logging.error('new list item: '+str(self.entity))
        item=ListItem(parent=self.root, collection=self.entity, index=int(key), value=valueObj)
      item.put()

  def remove(self, value):
    """ Remove the first instance (by index) of the given value from the list found at the stored path. """
    if type(value)==str or type(value)==unicode:
      valueObj=StringValue.all().ancestor(self.root).filter('collection =', self.entity).filter('value =', value).get()
      if not valueObj:
        logging.error('No such list item value: '+str(value))
      else:
        item=ListItem.all().ancestor(self.root).filter("collection =", self.entity).filter('value =', valueObj).get()
        if not item:
          logging.error('No such list item value: '+str(value))
        else:
          #FIXME - Recursive delete necessary for collections
          item.delete()
          valueObj.delete()
    else:
      logging.error('Unsupported remove type: '+str(type(value)))

  def insert(self, key, value):
    """ Insert the value with the given key into the list model found at the stored path. """
    logging.error('insert: '+str(key)+' '+str(value))
    valueObj=self.makeValue(self.root, self.entity, value)
    if valueObj:
      logging.error('entity: '+str(self.entity))
      item=ListItem.all().ancestor(self.root).filter("collection =", self.entity).filter('index =', int(key)).get()
      if item:
        item.value=valueObj
      else:
        logging.error('new list item: '+str(self.entity))
        item=ListItem(parent=self.root, collection=self.entity, index=int(key), value=valueObj)
      item.put()

  def clear(self):
    """ Clear such that it is empty the list found at the stored path. """
    logging.error('clear')
    items=ListItem.all().ancestor(self.root).filter("collection =", self.entity).run()
    for item in items:
      # FIXME - Recursive delete necessary for collections
      item.value.delete()
      item.delete()
