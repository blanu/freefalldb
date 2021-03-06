"""
This is the web.py file required by App Engine to set up URL paths to handlers.
It defines two URLs, /actions and /views.
"""

from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext.webapp import template

from api import *

application = webapp.WSGIApplication([
  ('/actions', ActionService),
  ('/views', ViewService),
  ('/actions.pack', PackActionService),
  ('/views.pack', PackViewService),
], debug=True)

def main():
  run_wsgi_app(application)

if __name__ == "__main__":
  main()
