from google.appengine.ext import db
from google.appengine.ext.db import polymodel

class View(db.Model):
  name=db.StringProperty(required=True)
  json=db.TextProperty(required=True)

class CollectionModel(polymodel.PolyModel):
  pass
  
class BagModel(CollectionModel):
  pass

class ListModel(CollectionModel):
  pass

class MapModel(CollectionModel):
  pass
  
class Value(polymodel.PolyModel):
  pass

class Item(polymodel.PolyModel):
  collection=db.ReferenceProperty(CollectionModel, required=True)
  value=db.ReferenceProperty(Value, required=True)

class ListItem(Item):
  index=db.IntegerProperty(required=True)

class MapItem(Item):
  index=db.StringProperty(required=True)

class CollectionValue(Value):
  value=db.ReferenceProperty(CollectionModel, required=True)
  
class StringValue(Value):
  value=db.StringProperty(required=True)

class FloatValue(Value):
  value=db.FloatProperty(required=True)

class Root(db.Model):
  value=db.ReferenceProperty(MapModel, required=True)
