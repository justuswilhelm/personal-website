#!/usr/bin/env python
from functools import lru_cache
from markdown import markdown
from yaml import load
from os.path import join


@lru_cache()
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


@lru_cache()
def load_blog():
    blog = load(open(join('blog', 'blog.yaml')))

    blog_with_articles = map(
        lambda b: {**b, 'content': read_article(str(b['created']))},
        blog,
    )
    return list(blog_with_articles)


if __name__ == "__main__":
    for article in load_blog():
        print(article)
