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


env = Environment(loader=FileSystemLoader('templates/'))
env.filters['datetimeformat'] = datetimeformat
