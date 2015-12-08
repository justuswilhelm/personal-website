#!/usr/bin/env python
from jinja2 import (
    Environment,
    FileSystemLoader,
)
from blog import load_blog
env = Environment(loader=FileSystemLoader('templates/'))


def main():
    blog_index_template = env.get_template('blog_index.html')
    with open('public/blog_index.html', 'w') as fd:
        fd.write(blog_index_template.render(
            blog_articles=load_blog()))

if __name__ == "__main__":
    main()
