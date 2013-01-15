"""
This Paver configuration file enables command line access to Freefall tasks. You need Paver to be installed.
Install Paver with 'pip install paver'.
For help run:
  . ./setEnv.sh
  freefall help
"""

import os
import shutil

import yaml

from paver.easy import *
from paver.path import *

from airspeed import CachingFileLoader

from json import dumps

def ensure(path):
  if not os.path.exists(path):
    print('mkdir '+str(path))
    os.makedirs(path)

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
      ensure('transforms')
      ensure('lib')

  copy('examples/config.yaml', name+'/config/config.yaml')
  copy('examples/testAction.py', name+'/app/actions/testAction.py')
  copy('examples/copy.py', name+'/app/transforms/copy.py')

def parseConfig():
  f=open('config/config.yaml')
  s=f.read()
  f.close()
  data=yaml.load(s)
  appname=data['appname']
  if 'service' in data:
    service=data['service']
  else:
    service=appname
  capname=appname.capitalize()
  version=data['version']
  package=data['package']
  packagePath=package.replace('.', '/')

  generate('gen/server/app.yaml', 'app.yaml', {'appname': service, 'version': version})

  actions=data['actions']
  transforms=data['transforms']
  views=data['views']
  pages=actions.keys()+views
  models=data['models']

  actionMethods=[]
  for action in actions:
    actionValues=actions[action]
    parameters=actionValues['parameters']
    try:
      inputs=actionValues['inputs']
    except:
      inputs=[]
    outputs=actionValues['outputs']
    f=open('app/actions/'+action+'.py')
    data=f.read()
    f.close()
    firstLine=data.split("\n")[0]
    name=firstLine.split(' ')[1].split('(')[0]
    args=[arg.strip() for arg in firstLine.split('(')[1].split(')')[0].split(',')]
    while len(args)>0 and args[-1]=='':
      args=args[:-1]
    argList=args[:len(parameters)]
    jargs=', '.join(['self']+args[:len(parameters)])
    fargs=', '.join(['self']+args)
    cargs=', '.join(args)

    targs=[]
    itargs=[]
    for x in range(min(len(args),len(parameters))):
      arg=args[x]
      argType=parameters[x]
      if argType=='string':
        targs.append('String '+arg)
        itargs.append(arg+':(NSString *)'+arg)
      elif argType=='float':
        targs.append('Float '+arg)
        itargs.append(arg+':(NSNumber *)'+arg)
      elif argType=='map':
        targs.append('Map '+arg)
        itargs.append(arg+':(NSDictionary *)'+arg)
      elif argType=='list':
        targs.append('List '+arg)
        itargs.append(arg+':(NSArray *)'+arg)
      else:
        print('Unknown targs type: '+str(type(arg)))
    targs=', '.join(targs)
    itargs[0]=itargs[0].split(':')[1]
    itargs=' '.join(itargs)
    args=', '.join(args)

    body=[line.strip() for line in data.split("\n")[1:]]
    while body[-1]=='':
      body=body[:-1]
    actionMethods.append({'name':name, 'jargs':jargs, 'fargs':fargs, 'cargs':cargs, 'targs':targs, 'args':args, 'code':body, 'inputs': inputs, 'outputs': outputs, 'signature': itargs, 'argList': argList})

  triggers=[]
  transformMethods=[]
  if not transforms:
    transforms=[]
  for transform in transforms:
    transformValues=transforms[transform]
    for trigger in transformValues['triggers']:
      triggers.append({'transform': transform, 'path': trigger})
    f=open('app/transforms/'+transform+'.py')
    data=f.read()
    f.close()
    firstLine=data.split("\n")[0]
    name=firstLine.split(' ')[1].split('(')[0]
    args=[arg.strip() for arg in firstLine.split('(')[1].split(')')[0].split(',')]
    while len(args)>0 and args[-1]=='':
      args=args[:-1]
    args=', '.join(args)
    body=data.split("\n")[1:]
    while body[-1].strip()=='':
      body=body[:-1]

    if 'inputs' in transformValues:
      inputs=transformValues['inputs']
    else:
      inputs=[]
    output=transformValues['output']
    inputStr=', '.join(inputs)
    cargs=', '.join(['changes']+inputs+['state'])
    transformMethods.append({'name':name, 'args':args, 'cargs':cargs, 'code':body, 'inputs':inputs, 'inputStr':inputStr, 'output':transformValues['output']})

  viewMethods=[]
  for view in views:
    viewMethods.append({'name':view})

  return appname, service, capname, version, package, packagePath, actions, models, triggers, views, actionMethods, transformMethods, viewMethods

@task
def compile():
  call_task('server')
  call_task('client')

@task
def server():
  if not os.path.exists('config/config.yaml'):
    return

  ensure('gen')
  ensure('gen/server')

  copy('../examples/queue.yaml', 'gen/server/queue.yaml')
  copy('../src/index.yaml', 'gen/server/index.yaml')
  copy('../lib/jsonrpc', 'gen/server/jsonrpc')
  copy('../src/model.py', 'gen/server/model.py')
  copy('../src/transform.py', 'gen/server/transform.py')
  copy('../src/transformUtils.py', 'gen/server/transformUtils.py')
  copy('../src/storage.py', 'gen/server/storage.py')
  copy('../src/models.py', 'gen/server/models.py')
  copy('../templates/web.py', 'gen/server/web.py')
  copy('../src/generic.py', 'gen/server/generic.py')
  copy('../src/packRpcHandler.py', 'gen/server/packRpcHandler.py')
  copy('../lib/bson/', 'gen/server/bson')
#  copy('../lib/pytz/', 'gen/server/pytz')

  files=os.listdir('app/lib')
  for filename in files:
    copy('app/lib/'+filename, 'gen/server/'+filename)

  appname, service, capname, version, package, packagePath, actions, models, triggers, views, actionMethods, transformMethods, viewMethods=parseConfig()

  generate('gen/server/api.py', 'api.py', {'actions': actionMethods, 'views': viewMethods})
  generate('gen/server/transforms.py', 'transforms.py', {'transforms': transformMethods})
  generate('gen/server/modelInfo.py', 'modelInfo.py', {'models': models})
  generate('gen/server/triggerInfo.py', 'triggerInfo.py', {'triggers': triggers})

@task
def client():
  call_task('py')
  call_task('android')
  call_task('ios')

@task
def py():
  if not os.path.exists('config/config.yaml'):
    return

  appname, service, capname, version, package, packagePath, actions, models, triggers, views, actionMethods, transformMethods, viewMethods=parseConfig()

  print ('service: '+str(service))

  ensure('gen')
  ensure('gen/client')
  ensure('gen/client/py')
  ensure('gen/client/py/lib')
  ensure('gen/client/py/lib/bson')

  copy('../lib/jsonrpc', 'gen/client/py/lib/jsonrpc')
  copy('../src/packProxy.py', 'gen/client/py/lib/packProxy.py')
  copy('../lib/bson/__init__.py', 'gen/client/py/lib/bson/__init__.py')
  copy('../lib/bson/network.py', 'gen/client/py/lib/bson/network.py')
  copy('../lib/bson/codec.py', 'gen/client/py/lib/bson/codec.py')

  for action in actions:
    generate('gen/client/py/'+action+'.py', 'client.py', {'appname': appname, 'host': service, 'service': 'actions', 'method': action, 'types': dumps(actions[action]['parameters'])})
    generate('gen/client/py/'+action+'-pack.py', 'client-pack.py', {'appname': appname, 'host': service, 'service': 'actions', 'method': action, 'types': dumps(actions[action]['parameters'])})
  for view in views:
    generate('gen/client/py/'+view+'.py', 'client.py', {'appname': appname, 'host': service, 'service': 'views', 'method': view})
    generate('gen/client/py/'+view+'-pack.py', 'client-pack.py', {'appname': appname, 'host': service, 'service': 'views', 'method': view})

def shell(cmd):
  print('Executing '+str(cmd))
  os.system(cmd)

@task
def android():
  if not os.path.exists('config/config.yaml'):
    return

  appname, service, capname, version, package, packagePath, actions, models, triggers, views, actionMethods, transformMethods, viewMethods=parseConfig()

  ensure('gen')
  ensure('gen/client')
  ensure('gen/client/android')
  ensure('gen/client/android/lib')
  ensure('gen/client/android/src')
  ensure('gen/client/android/src/'+packagePath)
  ensure('gen/client/android/class')
  ensure('gen/client/android/class/'+packagePath)

  copy('../lib/android-json-rpc-0.3.3.jar', 'gen/client/android/lib/android-json-rpc-0.3.3.jar')

  generate('gen/client/android/src/'+packagePath+'/'+capname+'Client.java', 'Client.java', {'appname': capname, 'package': package, 'actions': actionMethods, 'views': viewMethods})
  shell('javac -d gen/client/android/class gen/client/android/src/'+packagePath+'/'+capname+'Client.java')
  with pushd('gen/client/android/class'):
    shell('jar cvf ../lib/'+appname+'.jar '+packagePath.split('/')[0])

@task
def ios():
  if not os.path.exists('config/config.yaml'):
    return

  appname, service, capname, version, package, packagePath, actions, models, triggers, views, actionMethods, transformMethods, viewMethods=parseConfig()

  ensure('gen')
  ensure('gen/client')
  ensure('gen/client/ios')
  ensure('gen/client/ios/lib')
  ensure('gen/client/ios/src')

  copy('../lib/DSJSONRPC', 'gen/client/ios/lib/DSJSONRPC')
  copy('../lib/msgpack-objectivec', 'gen/client/ios/lib/msgpack-objectivec')
  copy('../lib/PackRPC', 'gen/client/ios/lib/PackRPC')

  generate('gen/client/ios/src/'+capname+'Client.h', 'Client.h', {'appname': capname, 'host': service, 'package': package, 'actions': actionMethods, 'views': viewMethods})
  generate('gen/client/ios/src/'+capname+'Client.m', 'Client.m', {'appname': capname, 'host': service, 'package': package, 'actions': actionMethods, 'views': viewMethods})

@task
@needs(['server'])
def deploy():
  sh('appcfg.py update gen/server')
