from collections import namedtuple
from flask import (
    Flask,
    render_template,
)

application = Flask(__name__)

BlogEntry = namedtuple('BlogEntry', ['date', 'title', 'content'])


def get_blog_entry(entry):
    return ""


def get_blog_entries():
    return []


@application.route("/")
def index():
    return render_template("index.html")


@application.route("/blog/<entry>/")
def blog_entry(entry):
    blog_entry = get_blog_entry(entry)
    return render_template("blog_entry.html", entry=blog_entry)


@application.route("/blog/")
def blog_index():
    blog_entries = get_blog_entries()
    return render_template("blog_index.html", entries=blog_entries)
