
import os
import shutil

import yaml

from paver.easy import *
from paver.path import *

from airspeed import CachingFileLoader

def ensure(path):
  if not os.path.exists(path):
    print('mkdir '+str(path))
    os.mkdir(path)

def copy(src, dst):
  print('Create '+str(dst))
  if os.path.isdir(src):
    if not os.path.exists(dst):
      shutil.copytree(src, dst, True)
  else:
    shutil.copyfile(src, dst)

def generate(fileName, templateName, context):
  print('Generating '+str(fileName))
  loader=CachingFileLoader("../templates")
  template=loader.load_template(templateName)
  result=template.merge(context, loader=loader)
  f=open(fileName, 'w')
  f.write(result)
  f.close()

@task
@consume_args
def new(args):
  name=args[0]
  print('Creating new project '+str(name))
  ensure(name)
  with pushd(name):
    ensure('config')

    ensure('app')
    with pushd('app'):
      ensure('actions')
      ensure('transformations')
      ensure('models')
      ensure('views')
      ensure('lib')

  copy('examples/config.yaml', name+'/config/config.yaml')
  copy('src/generic.py', name+'/app/lib/generic.py')
  copy('src/util.py', name+'/app/lib/util.py')

@task
def compile():
  if not os.path.exists('config/config.yaml'):
    return

  ensure('gen')
  ensure('gen/server')

  copy('../examples/queue.yaml', 'gen/server/queue.yaml')
  copy('../lib/jsonrpc', 'gen/server/jsonrpc')
  copy('../src/model.py', 'gen/server/model.py')
  copy('../src/transform.py', 'gen/server/transform.py')
  copy('../src/storage.py', 'gen/server/storage.py')
  copy('../src/models.py', 'gen/server/models.py')
  copy('../templates/web.py', 'gen/server/web.py')

  files=os.listdir('app/lib')
  for filename in files:
    copy('app/lib/'+filename, 'gen/server/'+filename)

  f=open('config/config.yaml')
  s=f.read()
  f.close()
  data=yaml.load(s)
  appname=data['appname']
  version=data['version']
  
  generate('gen/server/app.yaml', 'app.yaml', {'appname': appname, 'version': version})  
  
  actions=data['actions']
  views=data['views']
  pages=actions+views
  models=data['models']

  actionMethods=[]
  for action in actions:
    f=open('app/actions/'+action+'.py')
    data=f.read()
    f.close()
    firstLine=data.split("\n")[0]
    name=firstLine.split(' ')[1].split('(')[0]
    args=[arg.strip() for arg in firstLine.split('(')[1].split(')')[0].split(',')]
    while len(args)>0 and args[-1]=='':
      args=args[:-1]
    if len(args)==0:
      jargs='self'
      fargs='self, state'
      cargs='state'
    else:
      jargs=', '.join(['self']+args)
      fargs=', '.join(['self', 'state']+args)
      cargs=', '.join(['state']+args)
    body=[line.strip() for line in data.split("\n")[1:]]
    while body[-1]=='':
      body=body[:-1]
    actionMethods.append({'name':name, 'jargs':jargs, 'fargs':fargs, 'cargs':cargs, 'code':body})  
  
  viewMethods=[]
  for view in views:
    f=open('app/views/'+view+'.py')
    data=f.read()
    f.close()
    firstLine=data.split("\n")[0]
    name=firstLine.split(' ')[1].split('(')[0]
    args=[arg.strip() for arg in firstLine.split('(')[1].split(')')[0].split(',')]
    while len(args)>0 and args[-1]=='':
      args=args[:-1]
    if len(args)==0:
      jargs='self'
      fargs='self, state'
      cargs='state'
    else:
      jargs=', '.join(['self']+args)
      fargs=', '.join(['self', 'state']+args)
      cargs=', '.join(['state']+args)
    body=[line.strip() for line in data.split("\n")[1:]]
    while body[-1]=='':
      body=body[:-1]
    viewMethods.append({'name':name, 'jargs':jargs, 'fargs':fargs, 'cargs':cargs, 'code':body})

  generate('gen/server/api.py', 'api.py', {'actions': actionMethods, 'views': viewMethods})
  generate('gen/server/modelInfo.py', 'modelInfo.py', {'models': models})

  ensure('gen/client')
  ensure('gen/client/py')
  ensure('gen/client/py/lib')
  
  copy('../lib/jsonrpc', 'gen/client/py/lib/jsonrpc')

  for action in actions:
    generate('gen/client/py/'+action+'.py', 'client.py', {'appname': appname, 'service': 'actions', 'method': action})
  for view in views:
    generate('gen/client/py/'+view+'.py', 'client.py', {'appname': appname, 'service': 'views', 'method': view})
    

@task
def deploy():
  sh('appcfg.py update gen/server')
