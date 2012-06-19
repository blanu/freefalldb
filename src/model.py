from django.utils import simplejson as parser

class TransactionMonad:
  def __init__(self):
    self.transforms=[]

  def add(self, path, item):
    self.transforms.append({'path': path, 'type': 'add', 'args': item})
    
  def put(self, path, key, value):
    self.transforms.append({'path': path, 'type': 'put', 'args': {key:value}})

  def append(self, path, item):
    self.transforms.append({'path': path, 'type': 'append', 'args': item})

  def serialize(self):
    return parser.dumps(self.transforms)

class BagTransaction(TransactionMonad):
  def __init__(self, path):
    TransactionMonad.__init__(self)
    self.path=path
    
  def add(self, item):
    TransactionMonad.add(self, self.path, item)
    
class MapTransaction(TransactionMonad):
  def __init__(self, path):
    TransactionMonad.__init__(self)
    self.path=path
    
  def put(self, key, value):
    TransactionMonad.put(self, self.path, key, value)

class ListTransaction(TransactionMonad):
  def __init__(self, path):
    TransactionMonad.__init__(self)
    self.path=path
    
  def append(self, item):
    TransactionMonad.append(self, self.path, item)
