modelNames=[
  #foreach($model in $models)
    '$model.name',
  #end
]

modelTypes={
  #foreach($model in $models)
    '$model.name': '$model.type',
  #end
}
