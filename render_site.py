from os import mkdir
from shutil import (
    copytree,
    rmtree,
)

from jinja2 import (
    Environment,
    FileSystemLoader,
)


env = Environment(loader=FileSystemLoader('templates/'))


def clean():
    rmtree('public')
    rmtree('blog')


def create_folders():
    mkdir('public')
    mkdir('blog')


def copy_files():
    copytree('static/', 'public/static/')


def render_templates():
    render_blog()
    render_index()


def retrieve_blog_articles():
    return []


def render_blog():
    blog_index_template = env.get_template('blog_index.html')
    blog_articles = retrieve_blog_articles()
    with open('public/blog_index.html', 'w') as fd:
        fd.write(blog_index_template.render(
            blog_articles=blog_articles
            ))


def render_index():
    index_template = env.get_template('index.html')
    with open('public/index.html', 'w') as fd:
        fd.write(index_template.render())


def main():
    clean()
    create_folders()
    copy_files()
    render_templates()


if __name__ == "__main__":
    main()
