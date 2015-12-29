#!/usr/bin/env python
import logging

from os.path import (
    basename,
    join,
    splitext,
)
from sys import argv

from utilities import (
    env,
    load_data,
)

logging.basicConfig(level='INFO')


def load_metadata(path):
    logging.info("Loading metadata files")
    metadata_paths = load_data(path).get('data_names')
    assert metadata_paths is not [], "No metadata paths"
    metadata = {name: load_data(
        join('data', name + '.yaml')) for name in metadata_paths}
    return metadata


def render_template(page):
    page_name, _ = splitext(basename(page))
    template_file = '{}.html'.format(page_name)

    logging.info("Rendering template %s.", template_file)
    return env.get_template(template_file).render(**load_metadata(page))


def write_output(output, out_path):
    logging.info("Writing output to %s.", out_path)
    with open(out_path, 'w') as fd:
        fd.write(output)


def main():
    page = argv[1]
    out_path = argv[2]

    write_output(render_template(page), out_path)


if __name__ == "__main__":
    main()
