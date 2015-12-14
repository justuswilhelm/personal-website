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


def datetimeformat(value, format='%b %d, %Y'):
    return value.strftime(format)

env.filters['datetimeformat'] = datetimeformat


def load_data(path):
    """Given a path, read a yaml file."""
    with open(path) as fd:
        return load(fd.read()) or {}


def load_metadata(path):
    logging.info("Loading metadata files")
    metadata_paths = load_data(path).get('data_names')
    assert metadata_paths is not [], "No metadata paths"
    metadata = {name: load_data(
        join('data', name + '.yaml')) for name in metadata_paths}
    return metadata


def main():
    page = argv[1]
    out_path = argv[2]

    page_name, _ = splitext(basename(page))
    logging.info("Processing %s", page)

    metadata = load_metadata(page)

    template_file = '{}.html'.format(page_name)
    logging.info("Rendering template %s.", template_file)
    template = env.get_template(template_file)
    output = template.render(**metadata)

    logging.info("Writing output to %s.", out_path)
    with open(out_path, 'w') as fd:
        fd.write(output)


if __name__ == "__main__":
    main()
