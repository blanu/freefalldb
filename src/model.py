""" The model module provides classes encapsulating transactional monads for modifying the database models. """

import logging
import json as parser

class TransactionMonad:
  """ TransactionMonad is the base class for all transactional monad classes. It stores operations without regard to type. """
  def __init__(self, transforms=None):
    """ Initialize the transform list to be empty. """
    logging.error('TM:'+str(transforms))
    if transforms!=None:
      logging.error('reusing transforms')
      self.transforms=transforms
    else:
      logging.error('new transforms')
      self.transforms=[]

  def add(self, path, item):
    """ Add an item to the bag with the specified path. """
    self.transforms.append({'path': path, 'type': 'add', 'args': item})

  def put(self, path, key, value):
    """ Put the value with the given key into the map with the specified path. """
    self.transforms.append({'path': path, 'type': 'put', 'args': {key:value}})

  def clear(self, path):
    """ Clear so that it is empty the list with the specified path. """
    self.transforms.append({'path': path, 'type': 'clear', 'args': {}})

  def serialize(self):
    """ Serialize the list of transforms to JSON. """
    return parser.dumps(self.transforms)

class GenericTransaction(TransactionMonad):
  def __init__(self, path, transforms=None):
    """ Initialize the transform list to be empty and store the path. """
    TransactionMonad.__init__(self, transforms=transforms)
    self.path=path

  def add(self, item):
    """ Add an item to the bag found at the stored path. """
    logging.error('adding to bag')
    self.transforms.append({'path': self.path, 'type': 'add', 'args': item})
    logging.error('transforms: '+str(self.transforms))

  def put(self, key, value):
    """ Put the value with the given key into the map found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'put', 'args': {key:value}})

  def __setitem__(self, key, value):
    self.put(key, value)

  def __getitem__(self, index):
    return GenericTransaction(self.path+[index], transforms=self.transforms)

  def append(self, item):
    """ Append an item to the list found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'append', 'args': item})

  def remove(self, item):
    """ Remove the first instance (by index) of the given item from the list found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'remove', 'args': item})

  def insert(self, index, item):
    """ Insert the value with the given index into the list found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'insert', 'args': {'index': index, 'value': item}})

  def clear(self):
    """ Clear such that it is empty the list found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'clear', 'args': {}})

class BagTransaction(TransactionMonad):
  """ BagTransaction is a typed transactional monad which stores operations for Bag models. """

  def __init__(self, path, transforms=None):
    """ Initialize the transform list to be empty and store the path. """
    TransactionMonad.__init__(self, transforms=transforms)
    self.path=path

  def add(self, item):
    """ Add an item to the bag found at the stored path. """
    logging.error('adding to bag')
    self.transforms.append({'path': self.path, 'type': 'add', 'args': item})
    logging.error('transforms: '+str(self.transforms))

class MapTransaction(TransactionMonad):
  """ MapTransaction is a typed transactional monad which stores operations for Map models. """

  def __init__(self, path, transforms=None):
    """ Initialize the transform list to be empty and store the path. """
    TransactionMonad.__init__(self, transforms=transforms)
    self.path=path

  def put(self, key, value):
    """ Put the value with the given key into the map found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'put', 'args': {key:value}})

  def __getitem__(self, index):
    return GenericTransaction(self.path+[index], transforms=self.transforms)

  def __setitem__(self, key, value):
    self.put(key, value)

class ListTransaction(TransactionMonad):
  """ ListTransaction is a typed transactional monad which stores operations for List models. """

  def __init__(self, path, transforms=None):
    """ Initialize the transform list to be empty and store the path. """
    TransactionMonad.__init__(self, transforms=transforms)
    self.path=path

  def append(self, item):
    """ Append an item to the list found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'append', 'args': item})

  def remove(self, item):
    """ Remove the first instance (by index) of the given item from the list found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'remove', 'args': item})

  def insert(self, index, item):
    """ Insert the value with the given index into the list found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'insert', 'args': {'index': index, 'value': item}})

  def clear(self):
    """ Clear such that it is empty the list found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'clear', 'args': {}})
