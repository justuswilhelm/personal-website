from functools import lru_cache
from datetime import date
from os.path import (
    splitext,
    split,
)
from glob import glob


def extract_blog_article_metadata(blog_article_path):
    dirname, filename = split(blog_article_path)
    title, _ = splitext(filename)
    creation_date = date(*(map(int, dirname.split("/")[1:])))
    return title, creation_date


@lru_cache()
def read_blog_article(blog_article_path):
    # blog/2015/09/10/Who To Sue If Skype Spam Kills You.md
    title, creation_date = extract_blog_article_metadata(blog_article_path)
    with open(blog_article_path) as fd:
        content = fd.read()
    return {
        'title': title,
        'creation_date': creation_date,
        'content': content, }


def get_blog_articles():
    document_paths = reversed(glob('blog/**/*.md', recursive=True))
    return map(read_blog_article, document_paths)


if __name__ == "__main__":
    for document in get_blog_articles():
        print(document)
