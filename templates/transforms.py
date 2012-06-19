import logging

from models import *
from transformUtils import *

#foreach($transform in $transforms)
def _${transform.name}($transform.args):#foreach($line in $transform.code)
  $line #end    

    
def ${transform.name}(changes):
  from transform import applyAction
  #foreach($input in $transform.inputs)${input}=loadInput('$input')
  #end
  
  state=loadOutput('$transform.output')
  
  ${transform.output}=_${transform.name}($transform.cargs)
  applyAction(state.transforms)
  return None  
#end

