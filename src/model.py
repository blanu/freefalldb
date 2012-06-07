from django.utils import simplejson as parser

class TransactionMonad:
  transforms=[]
  
  def __init__(self):
    pass

  def add(self, path, item):
    self.transforms.append({'path': path, 'type': 'add', 'args': item})

  def serialize(self):
    return parser.dumps(self.transforms)
