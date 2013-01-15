#import "PackRPC.h"

@interface ${appname}Client : NSObject
#foreach($action in $actions)
- (void) ${action.name}:${action.signature};#end

#foreach($view in $views)
- (void) ${view.name}:(void (^) (id result))callback;#end


@end
