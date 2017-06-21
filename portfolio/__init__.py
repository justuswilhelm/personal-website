"""Flask application."""
from glob import glob
from datetime import date
from os.path import split, splitext

from flask import Flask, Response, render_template
from markdown import markdown
from yaml import safe_load


application = Flask(__name__)
application.config.update(FREEZER_DESTINATION='../justus.pw')


def discover_blog_articles():
    """Return paths of all blog articles."""
    return reversed(glob('blog/*.md'))


def read_blog_metadata():
    """Read all blog articles, yield their meta data."""
    for path in discover_blog_articles():
        yield parse_blog_article(path)[0]


def parse_blog_article(path):
    """Parse a single blog article."""
    with open(path) as fd:
        try:
            meta_raw, content = fd.read().split('---')
        except ValueError:
            raise SyntaxError("Missing meta data in {}".format(path))
        meta = safe_load(meta_raw)
        fname = splitext(split(path)[-1])[0]
        meta['created'] = date(*map(int, fname.split("-")))
        return meta, content


def render_article(raw):
    """Turn raw to markdown."""
    return markdown(
        raw,
        extensions=[
            'markdown.extensions.fenced_code',
            'markdown.extensions.codehilite',
            'markdown.extensions.headerid',
        ], extension_configs={
            'markdown.extensions.codehilite': {
                'guess_lang': True,
            }
        })


def load_article(year, month, day):
    """Load an article, given year, month and day."""
    path = 'blog/{}-{:02d}-{:02d}.md'.format(year, month, day)
    meta, content = parse_blog_article(path)
    return meta, render_article(content)


def load_data(path):
    """Given a path, read a yaml file."""
    with open('data/{}.yaml'.format(path)) as fd:
        return safe_load(fd.read()) or {}


@application.template_filter()
def datetimeformat(value, format='%b %d, %Y'):
    """
    Format a datetime object.

    >>> from datetime import datetime
    >>> a = datetime(2015, 12, 24)
    >>> datetimeformat(a)
    'Dec 24, 2015'
    """
    return value.strftime(format)


@application.route('/')
def index():
    """Show index page."""
    with open("data/statement.md") as fd:
        statement = fd.read()
    return render_template('index.html',
                           statement=render_article(statement),
                           timeline=load_data('timeline'))


@application.route('/blog.html')
def blog():
    """Show blog index."""
    return render_template('blog.html', blog=tuple(read_blog_metadata()))


@application.route('/blog/<int(fixed_digits=4):year>-'
                   '<int(fixed_digits=2):month>-'
                   '<int(fixed_digits=2):day>-<title>.html')
def blog_article(year, month, day, **kwargs):
    """Render an individual blog article."""
    meta, content = load_article(year, month, day)
    return render_template('blog_article.html', meta=meta, content=content)


@application.route('/CNAME')
def CNAME():
    """Return the CNAME for github."""
    return Response("www.justus.pw",
                    headers={'Content-Type': 'application/octet-stream'})
