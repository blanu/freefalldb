"""
The generic module provides the base class from which all API services inherit. It provides a thin interface between App Engine and JSONRPC.
"""

from google.appengine.ext import webapp

from jsonrpc.handler import JSONRPC

class JsonRpcService(webapp.RequestHandler, JSONRPC):
    """ JsonRpcService provides an App Engine request handler which implements the JSON-RPC specification. """
    def post(self):
        """ JsonRpcService only supports HTTP POST """

        response, code = self.handleRequest(self.request.body, self.HTTP_POST)
        self.response.headers['Content-Type'] = 'application/json'
        self.response.set_status(code)
        self.response.out.write(response)
