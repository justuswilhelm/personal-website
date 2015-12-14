#!/usr/bin/env python
import logging
from os.path import join
from sys import argv

from jinja2 import (
    Environment,
    FileSystemLoader,
)
from markdown import markdown
from yaml import load

env = Environment(loader=FileSystemLoader('templates/'))
logging.basicConfig(level='INFO')


def read_article(id):
    with open(join('blog', id) + '.md') as fd:
        return markdown(
            fd.read(),
            extensions=[
                'markdown.extensions.fenced_code',
                'markdown.extensions.codehilite',
            ], extension_configs={
                'markdown.extensions.codehilite': {
                    'linenums': True,
                    'noclasses': True,
                    'guess_lang': True,
                }
            })


def load_blog(path):
    with open(path) as fd:
        blog = load(fd.read())

    blog_with_articles = map(
        lambda b: {**b, 'content': read_article(str(b['created']))},
        blog,
    )
    return list(blog_with_articles)


def main():
    blog_path = argv[1]
    out_path = argv[2]

    template = env.get_template("blog_article.html")
    for blog_article in load_blog(blog_path):
        logging.info("Rendering %s", blog_article['title'])
        filename = join(out_path, '{date}-{title}.html'.format(
            title=blog_article['title'],
            date=blog_article['created']))
        with open(filename, 'w') as fd:
            fd.write(template.render(article=blog_article))

if __name__ == "__main__":
    main()
