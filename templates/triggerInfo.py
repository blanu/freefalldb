"""
This is the list of triggers. It contains a variable 'triggers' which is a map of database names to transform functions.
This list is generated using information in the transforms section of config.yaml.
"""

from transforms import *

triggers={
  #foreach($trigger in $triggers)
    '$trigger.path': ${trigger.transform},
  #end
}

