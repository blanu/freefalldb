import logging

from google.appengine.api import memcache

from generic import JsonRpcService
from model import TransactionMonad
from transform import applyAction

from models import View

class ActionService(JsonRpcService): #foreach($action in $actions)
  def json_${action.name}($action.jargs):
    logging.info('ACTION: $action.name')
    state=TransactionMonad()
    logging.error('state: '+str(state.transforms))
    self.${action.name}($action.cargs)
    applyAction(state.transforms)
    return None

  def ${action.name}($action.fargs): #foreach($line in $action.code)
    $line #end
#end


def fetchView(name):
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

class ViewService(JsonRpcService): #foreach($view in $views)
  def json_${view.name}(self):
    logging.info('VIEW: $view.name')
    return fetchView('$view.name')
#end
