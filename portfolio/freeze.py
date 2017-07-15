"""."""
from flask_frozen import Freezer
from . import application

application.config['FREEZER_DEFAULT_MIMETYPE'] = 'text/html'
application.config['FREEZER_STATIC_IGNORE'] = (
    '.DS_Store',
)
freezer = Freezer(application)
