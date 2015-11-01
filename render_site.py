from json import load

from jinja2 import (
    Environment,
    FileSystemLoader,
)

from blog import get_blog_articles


env = Environment(
    loader=FileSystemLoader('templates/'),
    extensions=['jinja2_markdown.MarkdownExtension'],)


def render_templates():
    render_blog_articles()
    render_blog()
    render_index()
    render_projects()


def render_blog():
    blog_index_template = env.get_template('blog_index.html')
    with open('public/blog_index.html', 'w') as fd:
        fd.write(blog_index_template.render(
            blog_articles=tuple(get_blog_articles())))


def render_blog_articles():
    template = env.get_template("blog_article.html")
    for blog_article in get_blog_articles():
        with open('public/blog/{date}-{title}.html'.format(
                title=blog_article['title'],
                date=blog_article['creation_date'],), 'w') as fd:
            fd.write(template.render(article=blog_article))


def render_index():
    expertise = load(open('templates/expertise.json'))
    template = env.get_template('index.html')
    with open('public/index.html', 'w') as fd:
        fd.write(template.render(expertise=expertise))


def render_projects():
    projects = load(open('templates/projects.json'))
    template = env.get_template('projects.html')
    with open('public/projects.html', 'w') as fd:
        fd.write(template.render(projects=projects))


def main():
    render_templates()


if __name__ == "__main__":
    main()
