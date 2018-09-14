#!/usr/bin/env python
"""
Pandoc filter to process code blocks with class "graphviz" into
graphviz-generated images. Needs pygraphviz
"""
import os
import subprocess
import sys

from pandocfilters import (
    Image,
    Para,
    get_caption,
    get_extension,
    get_filename4code,
    get_value,
    toJSONFilter,
)


PICTURE_FORMAT = "png"


def graphviz(key, value, format, _):
    if key == 'CodeBlock':
        (ident, classes, keyvals), code = value
        if "graphviz" in classes:
            caption, typef, keyvals = get_caption(keyvals)
            prog, keyvals = get_value(keyvals, "prog", "dot")
            filetype = get_extension(format, PICTURE_FORMAT)
            dest = get_filename4code("graphviz", code, filetype)

            if not os.path.isfile(dest):
                result = subprocess.run(
                    [
                        "dot",
                        "-T{}".format(PICTURE_FORMAT),
                        "-Gdpi=300",
                    ],
                    stdout=subprocess.PIPE,
                    input=code.encode(),
                )
                with open(dest, 'wb') as fd:
                    fd.write(result.stdout)
                sys.stderr.write('Created image ' + dest + '\n')

            return Para([Image([ident, [], keyvals], caption, [dest, typef])])

if __name__ == "__main__":
    toJSONFilter(graphviz)
