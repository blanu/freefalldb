#import "${appname}Client.h"

@implementation ${appname}Client
#foreach($action in $actions)
- (void) ${action.name}:${action.signature}
{
//  PackRPC *jsonRPC = [[PackRPC alloc] initWithServiceEndpoint:[NSURL URLWithString:@"http://${host}.appspot.com/actions.pack"]];
  PackRPC *jsonRPC = [[PackRPC alloc] initWithServiceEndpoint:[NSURL URLWithString:@"http://localhost:8080/actions.pack"]];
  NSMutableArray *params=[[NSMutableArray alloc] init];
  #foreach($arg in $action.argList)[params addObject:$arg];
  #end
  NSLog(@"sending %@", params);

  [jsonRPC callMethod:@"${action.name}" withParameters:params onCompletion:^(NSString *methodName, NSInteger callId, id methodResult, DSJSONRPCError *methodError, NSError* error)
  {
  }];
}
#end

#foreach($view in $views)
- (void) ${view.name}:(void (^) (id result))callback
{
//  PackRPC *jsonRPC = [[PackRPC alloc] initWithServiceEndpoint:[NSURL URLWithString:@"http://${host}.appspot.com/views.pack"]];
  PackRPC *jsonRPC = [[PackRPC alloc] initWithServiceEndpoint:[NSURL URLWithString:@"http://localhost:8080/views.pack"]];
  NSDictionary *params=[[NSDictionary alloc] init];
  [jsonRPC callMethod:@"${view.name}" withParameters:params onCompletion:^(NSString *methodName, NSInteger callId, id methodResult, DSJSONRPCError *methodError, NSError* error)
  {
    if(methodError==nil && error==nil)
    {
      callback(methodResult);
    }
  }];
}
#end


@end
