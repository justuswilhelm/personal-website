#!/usr/bin/env python
from jinja2 import (
    Environment,
    FileSystemLoader,
)

from blog import load_blog
env = Environment(loader=FileSystemLoader('templates/'))


def main():
    template = env.get_template("blog_article.html")
    for blog_article in load_blog():
        with open('public/blog/{date}-{title}.html'.format(
                title=blog_article['title'],
                date=blog_article['created'],), 'w') as fd:
            fd.write(template.render(article=blog_article))

if __name__ == "__main__":
    main()
