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


def mscgen(key, value, format, _):
    if key == 'CodeBlock':
        (ident, classes, keyvals), code = value
        if "mscgen" in classes:
            caption, typef, keyvals = get_caption(keyvals)
            prog, keyvals = get_value(keyvals, "prog", "mscgen")
            filetype = get_extension(format, PICTURE_FORMAT)
            dest = get_filename4code("mscgen", code, filetype)

            if not os.path.isfile(dest):
                result = subprocess.run(
                    [
                        "mscgen",
                        "-T", "png",
                        "-o", dest,
                        "-",
                    ],
                    input=code.encode(),
                )
                sys.stderr.write('Created image ' + dest + '\n')

            return Para([Image([ident, [], keyvals], caption, [dest, typef])])

if __name__ == "__main__":
    toJSONFilter(mscgen)
