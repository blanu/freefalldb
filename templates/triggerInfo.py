from transforms import *

triggers={
  #foreach($trigger in $triggers)
    '$trigger.path': ${trigger.transform},
  #end
}

