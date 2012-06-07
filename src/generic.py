from google.appengine.ext import webapp

from jsonrpc.handler import JSONRPC
    
class JsonRpcService(webapp.RequestHandler, JSONRPC):
    def post(self):
        response, code = self.handleRequest(self.request.body, self.HTTP_POST)
        self.response.headers['Content-Type'] = 'application/json'
        self.response.set_status(code)
        self.response.out.write(response)    
