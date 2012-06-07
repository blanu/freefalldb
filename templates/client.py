#!/usr/bin/python
import os
import sys

sys.path.append(os.path.abspath('lib'))

from jsonrpc import ServiceProxy

args=sys.argv[1:]

#remote=ServiceProxy('http://${appname}.appspot.com/${service}')
remote=ServiceProxy('http://localhost:8080/${service}')

if len(args)==0:
  result=remote.${method}()
else:
  result=remote.${method}(*args)

if result:
  print(result)
