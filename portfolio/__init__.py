"""Flask application."""
from glob import glob
from datetime import date
from os.path import split, splitext

from flask import Flask, render_template
from yaml import safe_load

from . import content, filters
from .content import contentful


application = Flask(__name__)
application.config.update(FREEZER_DESTINATION='../justus.pw')
application.jinja_env.filters['pandoc'] = filters.pandoc
application.jinja_env.add_extension('pypugjs.ext.jinja.PyPugJSExtension')


def discover_blog_articles():
    """Return paths of all blog articles."""
    return reversed(glob('blog/*.md'))


def read_blog_metadata():
    """Read all blog articles, yield their meta data."""
    for path in discover_blog_articles():
        yield parse_blog_article(path)


def parse_blog_article(path):
    """Parse a single blog article."""
    with open(path) as fd:
        try:
            meta_raw, c = fd.read().split('---')
        except ValueError:
            raise SyntaxError("Missing meta data in {}".format(path))
        meta = safe_load(meta_raw)
        fname, = splitext(split(path)[-1])
        return {
            **meta,
            "content": c,
            "created": date(*map(int, fname.split("-"))),
        }


def load_article(year, month, day):
    """Load an article, given year, month and day."""
    path = 'blog/{}-{:02d}-{:02d}.md'.format(year, month, day)
    return parse_blog_article(path)


@application.route('/index.html')
def index():
    """Show index page."""
    entries = contentful.entries({'content_type': 'landingPage'})
    return render_template('index.pug', entries=entries)


@application.route('/explainer/<slug>.html')
def explainer(slug):
    """Show explainer page."""
    entry = content.get_explainer(slug)
    return render_template('explainer.pug', entry=entry)


@application.route('/method/<slug>.html')
def method(slug):
    """Show method page."""
    entry = content.get_method(slug)
    return render_template('method.pug', entry=entry)


@application.route('/landing/<slug>.html')
def landing(slug):
    """Show landing page."""
    entry = content.get_landing(slug)
    return render_template(
        'landing.pug', entry=entry
    )


@application.route('/blog.html')
def blog():
    """Show blog index."""
    return render_template('blog.pug', blog=read_blog_metadata())


@application.route(
    '/blog/<int(fixed_digits=4):year>-<int(fixed_digits=2):month>-'
    '<int(fixed_digits=2):day>-<title>.html'
)
def blog_article(year, month, day, **kwargs):
    """Render an individual blog article."""
    article = load_article(year, month, day)
    return render_template('blog_article.pug', article=article)
