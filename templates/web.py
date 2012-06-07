from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext.webapp import template

from api import *

application = webapp.WSGIApplication([
  ('/actions', ActionService),
  ('/views', ViewService),
], debug=True)

def main():
  run_wsgi_app(application)

if __name__ == "__main__":
  main()