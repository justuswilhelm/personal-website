"""."""
from flask_frozen import Freezer
from . import (
    application,
    read_blog_metadata,
)


freezer = Freezer(application)
