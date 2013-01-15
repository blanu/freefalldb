"""
Pack-RPC module, written to be used with GAE. Based on JSON-RPC.
"""

import logging

from jsonrpc.handler import JSONRPC, JSONRPCError
from bson import loads, dumps

class PackRPC(JSONRPC):
    def _decodeRequest(self, data):
        logging.error('packrpc data 1: '+str(data))
        results=[]
        try:
            data = loads(data)
            logging.error('packrpc data 2: '+str(data))
            for var in ('method', 'params', 'id'):
                results.append(data[var])
        except KeyError:
            logging.error('key error')
            raise JSONRPCError('Bad Call', httpStatus=500)
        except:
            logging.error('format error')
            raise JSONRPCError('Format Error', httpStatus=500)

        return results

    def _encodeResponse(self, result, error, id):
        response = {'result': result, 'error': error, 'id': id}
        return dumps(response)
