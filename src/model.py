from django.utils import simplejson as parser

class TransactionMonad:
  def __init__(self):
    self.transforms=[]

  def add(self, path, item):
    self.transforms.append({'path': path, 'type': 'add', 'args': item})

  def put(self, path, key, value):
    self.transforms.append({'path': path, 'type': 'put', 'args': {key:value}})

  def clear(self, path):
    self.transforms.append({'path': path, 'type': 'clear', 'args': {}})

  def serialize(self):
    return parser.dumps(self.transforms)

class BagTransaction(TransactionMonad):
  def __init__(self, path):
    TransactionMonad.__init__(self)
    self.path=path

  def add(self, item):
    self.transforms.append({'path': self.path, 'type': 'add', 'args': item})

class MapTransaction(TransactionMonad):
  def __init__(self, path):
    TransactionMonad.__init__(self)
    self.path=path

  def put(self, key, value):
    self.transforms.append({'path': self.path, 'type': 'put', 'args': {key:value}})

class ListTransaction(TransactionMonad):
  def __init__(self, path):
    TransactionMonad.__init__(self)
    self.path=path

  def append(self, item):
    self.transforms.append({'path': self.path, 'type': 'append', 'args': item})

  def remove(self, item):
    self.transforms.append({'path': self.path, 'type': 'remove', 'args': item})

  def insert(self, index, item):
    self.transforms.append({'path': self.path, 'type': 'insert', 'args': {'index': index, 'value': item}})

  def clear(self):
    self.transforms.append({'path': self.path, 'type': 'clear', 'args': {}})
