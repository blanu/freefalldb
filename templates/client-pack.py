#!/usr/bin/python
import os
import sys

sys.path.append(os.path.abspath('lib'))

from packProxy import ServiceProxy

def convert(arg, argType):
  if argType=='string':
    return str(arg)
  elif argType=='integer':
    return int(arg)
  elif argType=='float':
    return float(arg)
  else:
    print('Unknown arg type: '+str(argType))

args=sys.argv[1:]

#if($types)
types=$types
for x in range(len(args)):
  args[x]=convert(args[x], types[x])
#end

#remote=ServiceProxy('http://${host}.appspot.com/${service}.pack')
remote=ServiceProxy('http://localhost:8080/${service}.pack')

if len(args)==0:
  result=remote.${method}()
else:
  result=remote.${method}(*args)

if result:
  print(result)
