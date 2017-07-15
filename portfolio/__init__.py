"""Flask application."""
from glob import glob
from datetime import date
from os.path import split, splitext

from flask import Flask, render_template
from yaml import safe_load

from . import content
from . import filters


application = Flask(__name__)
application.config.update(FREEZER_DESTINATION='../justus.pw')
application.jinja_env.add_extension('pypugjs.ext.jinja.PyPugJSExtension')
application.jinja_env.filters['pandoc'] = filters.pandoc


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
            meta_raw, c = fd.read().split('---')
        except ValueError:
            raise SyntaxError("Missing meta data in {}".format(path))
        meta = safe_load(meta_raw)
        fname = splitext(split(path)[-1])[0]
        meta['created'] = date(*map(int, fname.split("-")))
        return meta, c


def load_article(year, month, day):
    """Load an article, given year, month and day."""
    path = 'blog/{}-{:02d}-{:02d}.md'.format(year, month, day)
    meta, c = parse_blog_article(path)
    return meta, c


@application.route('/')
def index():
    """Show index page."""
    entries = content.client.entries({
        'content_type': 'landingPage',
    })
    return render_template('index.pug', entries=entries)


@application.route('/method/<slug>/')
def method(slug):
    """Show method page."""
    entry = content.client.entries({
        'content_type': 'method',
        'fields.slug': slug,
    })[0]
    return render_template('method.pug', entry=entry)


@application.route('/landing/<slug>/')
def landing(slug):
    """Show landing page."""
    entry = content.client.entries({
        'content_type': 'landingPage',
        'fields.slug': slug,
    })[0]
    return render_template(
        'landing.pug', entry=entry
    )


@application.route('/blog/')
def blog():
    """Show blog index."""
    return render_template('blog.pug', blog=read_blog_metadata())


@application.route(
    '/blog/<int(fixed_digits=4):year>-<int(fixed_digits=2):month>-'
    '<int(fixed_digits=2):day>-<title>'
)
def blog_article(year, month, day, **kwargs):
    """Render an individual blog article."""
    meta, c = load_article(year, month, day)
    return render_template('blog_article.pug', meta=meta, content=c)
