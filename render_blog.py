#!/usr/bin/env python
from jinja2 import (
    Environment,
    FileSystemLoader,
)

from blog import load_blog


env = Environment(loader=FileSystemLoader('templates/'))


def render_blog():
    blog_index_template = env.get_template('blog_index.html')
    with open('public/blog_index.html', 'w') as fd:
        fd.write(blog_index_template.render(
            blog_articles=tuple(load_blog())))


def render_blog_articles():
    template = env.get_template("blog_article.html")
    for blog_article in load_blog():
        with open('public/blog/{date}-{title}.html'.format(
                title=blog_article['title'],
                date=blog_article['created'],), 'w') as fd:
            fd.write(template.render(article=blog_article))


def main():
    render_blog_articles()
    render_blog()


if __name__ == "__main__":
    main()
