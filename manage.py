#!/usr/bin/env python
"""Collect management methods."""
from functools import wraps
from sys import argv

manage_methods = {}


def manage(method):
    """Wrap command, register it and make it callable."""
    @wraps(method)
    def wrapper(*args, **kwargs):
        print("Calling management task {}".format(method.__name__))
        method(*args, **kwargs)
        print("Done with {}".format(method.__name__))

    manage_methods[method.__name__] = wrapper

    return wrapper


def main():
    """Call appropriate management method."""
    manage_methods[argv[1]]()


@manage
def runserver():
    """Run a local test server."""
    from portfolio import application
    application.run(debug=True)


@manage
def freeze():
    """Freeze website into build/ folder."""
    from portfolio.freeze import freezer
    if len(argv) == 3 and argv[2] == "debug":
        freezer.run(debug=True)
    else:
        freezer.freeze()


if __name__ == "__main__":
    main()
