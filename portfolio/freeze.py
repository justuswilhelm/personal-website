"""."""
from flask_frozen import Freezer
from . import application


freezer = Freezer(application)
