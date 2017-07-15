"""Jinja filters."""
import subprocess

from jinja2 import Markup


def pandoc(value):
    command = 'pandoc',
    proc = subprocess.run(
        command, input=value.encode(), stdout=subprocess.PIPE,
        check=True,
    )
    result = Markup(proc.stdout.decode())
    return result
