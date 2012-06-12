import logging

from generic import JsonRpcService
from model import TransactionMonad
from transform import applyAction

class ActionService(JsonRpcService): #foreach($action in $actions)
  def json_${action.name}($action.jargs): 
    state=TransactionMonad()   
    logging.error('state: '+str(state.transforms))
    self.${action.name}($action.cargs)
    applyAction(state.transforms)
    return None
    
  def ${action.name}($action.fargs): #foreach($line in $action.code)
    $line #end    
#end


class ViewService(JsonRpcService): #foreach($view in $views)
  def json_${view.name}($view.jargs): 
    state=TransactionMonad()   
    self.${view.name}($view.cargs)
    return None
    
  def ${view.name}($view.fargs): #foreach($line in $view.code)
    $line #end
#end

