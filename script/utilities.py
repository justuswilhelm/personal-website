from yaml import load
from jinja2 import (
    Environment,
    FileSystemLoader,
)


def datetimeformat(value, format='%b %d, %Y'):
    """
    >>> from datetime import datetime
    >>> a = datetime(2015, 12, 24)
    >>> datetimeformat(a)
    'Dec 24, 2015'
    """
    return value.strftime(format)


def load_data(path):
    """Given a path, read a yaml file."""
    with open(path) as fd:
        return load(fd.read()) or {}


env = Environment(loader=FileSystemLoader('templates/'))
env.filters['datetimeformat'] = datetimeformat
