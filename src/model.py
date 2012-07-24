""" The model module provides classes encapsulating transactional monads for modifying the database models. """

from django.utils import simplejson as parser

class TransactionMonad:
  """ TransactionMonad is the base class for all transactional monad classes. It stores operations without regard to type. """
  def __init__(self):
    """ Initialize the transform list to be empty. """
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

class BagTransaction(TransactionMonad):
  """ BagTransaction is a typed transactional monad which stores operations for Bag models. """

  def __init__(self, path):
    """ Initialize the transform list to be empty and store the path. """
    TransactionMonad.__init__(self)
    self.path=path

  def add(self, item):
    """ Add an item to the bag found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'add', 'args': item})

class MapTransaction(TransactionMonad):
  """ MapTransaction is a typed transactional monad which stores operations for Map models. """

  def __init__(self, path):
    """ Initialize the transform list to be empty and store the path. """
    TransactionMonad.__init__(self)
    self.path=path

  def put(self, key, value):
    """ Put the value with the given key into the map found at the stored path. """
    self.transforms.append({'path': self.path, 'type': 'put', 'args': {key:value}})

class ListTransaction(TransactionMonad):
  """ ListTransaction is a typed transactional monad which stores operations for List models. """

  def __init__(self, path):
    """ Initialize the transform list to be empty and store the path. """
    TransactionMonad.__init__(self)
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
    """ Clear such that it is empty the list found at the stored path. """"
    self.transforms.append({'path': self.path, 'type': 'clear', 'args': {}})
