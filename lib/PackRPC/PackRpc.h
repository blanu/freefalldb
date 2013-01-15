#import <Foundation/Foundation.h>
#import "DSJSONRPCError.h"

@class PackRPC;

/**
 *  Delegate used to provide information regarding web service calls made.
 *
**/
@protocol PackRPCDelegate <NSObject>
@optional
/**
 *  Invoked upon the method successfully completing without the error key being set in the response.
 *
 *  methodResult will be the appropriate Objective-C object type based on the type set as the result on the server.
 *
**/
- (void)jsonRPC:(PackRPC *)jsonRPC didFinishMethod:(NSString *)methodName forId:(NSInteger)aId withResult:(id)methodResult;

/**
 *  Invoked when the method is completed and the error key is set in the response.
 *
 *  methodError is an Objective-C object which contains all information provided by the offical JSON-RPC error response structure.
 *
**/
- (void)jsonRPC:(PackRPC *)jsonRPC didFinishMethod:(NSStream *)methodName forId:(NSInteger)aId withError:(DSJSONRPCError *)methodError;

/**
 *  Invoked when an error occurs with the connection or when the JSON payload can't be (de)serialized.
 *
 *  The error number will be set to a value defined by DSJSONRPCError. 
 *  localizedDescription is the value from the original error that was generated.
 *
**/
- (void)jsonRPC:(PackRPC *)jsonRPC didFailMethod:(NSString *)methodName forId:(NSInteger)aId withError:(NSError *)error;
@end


/**
 *  Invoked when and error occurs or upon method completion.
 *
 *  If methodError is set, the error occured on the server.
 *  methodError is an Objective-C object which contains all information provided by the offical JSON-RPC error response structure.
 *
 *  The internalError value is set when an error occurs with the connection or when the JSON payload can't be (de)serialized.
 *
 *  methodResult will be the appropriate Objective-C object type based on the type set as the result on the server.
 *
**/
typedef void (^PackRPCCompletionHandler)(NSString *methodName, NSInteger callId, id methodResult, DSJSONRPCError *methodError, NSError *internalError);


@interface PackRPC : NSObject

@property (nonatomic, DS_WEAK) id<PackRPCDelegate> delegate;

- (id)initWithServiceEndpoint:(NSURL *)serviceEndpoint;
- (id)initWithServiceEndpoint:(NSURL *)serviceEndpoint andHTTPHeaders:(NSDictionary *)httpHeaders;

#pragma mark - Web Service Invocation Methods
- (NSInteger)callMethod:(NSString *)methodName;
- (NSInteger)callMethod:(NSString *)methodName withParameters:(id)methodParams;

#pragma mark - Web Service Invocation Methods (Completion Handler Based)
- (NSInteger)callMethod:(NSString *)methodName onCompletion:(PackRPCCompletionHandler)completionHandler;
- (NSInteger)callMethod:(NSString *)methodName withParameters:(id)methodParams onCompletion:(PackRPCCompletionHandler)completionHandler;

@end
