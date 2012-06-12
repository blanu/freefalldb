from django.utils import simplejson as parser

class TransactionMonad:
  def __init__(self):
    self.transforms=[]

  def add(self, path, item):
    self.transforms.append({'path': path, 'type': 'add', 'args': item})

  def serialize(self):
    return parser.dumps(self.transforms)
