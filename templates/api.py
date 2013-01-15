import logging

from google.appengine.api import memcache

from generic import JsonRpcService, PackRpcService
from model import TransactionMonad
from transform import applyAction

from transformUtils import *

from models import View, BSONView
import bson

class ActionService(JsonRpcService): #foreach($action in $actions)
  def json_${action.name}($action.jargs):
    logging.info('ACTION: $action.name')

    state=TransactionMonad()

    #foreach($input in $action.inputs)${input}=loadInput($input)
    #end

    #foreach($output in $action.outputs)${output}=loadOutput('$output', state)
    #end

    self.${action.name}($action.cargs)
    logging.error('Resulting transforms: '+str(state.transforms))
    applyAction(state.transforms)
    return None

  def ${action.name}($action.fargs): #foreach($line in $action.code)
    $line #end
#end

class ViewService(JsonRpcService): #foreach($view in $views)
  def fetchView(self, name):
    json = memcache.get(name)
    if json is not None:
      return str(json)
    else:
      view=View.all().filter('name =', name).get()
      if view is not None and view.json is not None:
        memcache.add(name, view.json, 10)
        return str(view.json)
      else:
        logging.error('No data for view '+str(name))

  def json_${view.name}(self):
    logging.info('VIEW: $view.name')
    return self.fetchView('$view.name')
#end

class PackActionService(PackRpcService): #foreach($action in $actions)
  def json_${action.name}($action.jargs):
    logging.info('ACTION: $action.name')

    state=TransactionMonad()

    #foreach($input in $action.inputs)${input}=loadInput($input)
    #end

    #foreach($output in $action.outputs)${output}=loadOutput('$output', state)
    #end

    self.${action.name}($action.cargs)
    logging.error('Resulting transforms: '+str(state.transforms))
    applyAction(state.transforms)
    return None

  def ${action.name}($action.fargs): #foreach($line in $action.code)
    $line #end
#end

class PackViewService(PackRpcService): #foreach($view in $views)
  def fetchView(self, name):
    json = memcache.get(name+'.bson')
    if json is not None:
      return bson.loads(json)['result']
    else:
      root=fetchRoot()
      obj=resolveModel(root, [name])
      logging.error('processing view')

      bsons=obj.bsonSerialize()
      memcache.add(name+'.bson', bsons, 10)
      logging.error('returning view '+str(bsons))
      return bson.loads(bsons)['result']

  def json_${view.name}(self):
    logging.info('VIEW: $view.name')
    return self.fetchView('$view.name')
#end
