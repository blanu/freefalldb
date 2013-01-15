"""
transforms contains the transform functions for transforms defined in config.yaml
This file is generated using the content of files from the app/transforms/ directory.
"""

import logging

from model import TransactionMonad
from models import *
from transformUtils import *

#foreach($transform in $transforms)
def _${transform.name}($transform.args):#foreach($line in $transform.code)
  $line #end


def ${transform.name}(changes):
  from transform import applyAction
  #foreach($input in $transform.inputs)${input}=loadInput('$input')
  #end

  stateMonad=TransactionMonad()
  state=loadOutput('$transform.output', stateMonad)

  ${transform.output}=_${transform.name}($transform.cargs)
  applyAction(stateMonad.transforms)
  return None
#end

