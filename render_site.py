from os import mkdir
from shutil import (
    copy,
    copytree,
    rmtree,
)

from jinja2 import (
    Environment,
    FileSystemLoader,
)

from blog import get_blog_articles


env = Environment(
    loader=FileSystemLoader('templates/'),
    extensions=['jinja2_markdown.MarkdownExtension'],)


def clean():
    rmtree('public', ignore_errors=True)


def create_folders():
    mkdir('public')
    mkdir('public/blog')


def copy_files():
    copy('CNAME', 'public/CNAME')
    copytree('static/', 'public/static/')


def render_templates():
    render_blog_articles()
    render_blog()
    render_index()


def render_blog():
    blog_index_template = env.get_template('blog_index.html')
    with open('public/blog_index.html', 'w') as fd:
        fd.write(blog_index_template.render(
            blog_articles=get_blog_articles()))


def render_blog_articles():
    blog_article_template = env.get_template("blog_article.html")
    for blog_article in get_blog_articles():
        with open('public/blog/{date}-{title}.html'.format(
                title=blog_article['title'],
                date=blog_article['creation_date'],), 'w') as fd:
            fd.write(blog_article_template.render(
                article=blog_article))


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
