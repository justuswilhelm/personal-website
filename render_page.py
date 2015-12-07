#!/usr/bin/env python
import logging

from os.path import (
    basename,
    join,
    splitext,
)
from sys import argv

from yaml import load

from jinja2 import (
    Environment,
    FileSystemLoader,
)

logging.basicConfig(level='INFO')

env = Environment(loader=FileSystemLoader('templates/'))


def load_data(path):
    """Given a path, read a yaml file."""
    return load(open(join('data', '{}.yaml'.format(path))).read()) or {}


def main():
    page = argv[1]
    logging.info("Processing %s", page)
    page_name, _ = splitext(basename(page))
    metadata = load(open(page).read())
    assert metadata, "Empty metadata"
    data = {name: load_data(name) for name in metadata.get('data_names', [])}
    logging.info("Loaded metadata files %s", data.keys())
    template = env.get_template('{}.html'.format(page_name))
    out_path = join('public', '{}.html'.format(page_name))
    with open(out_path, 'w') as fd:
        fd.write(template.render(**data))


if __name__ == "__main__":
    main()
