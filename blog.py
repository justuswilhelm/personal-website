#!/usr/bin/env python
from functools import lru_cache
from datetime import date
from os.path import (
    splitext,
    split,
)
from glob import glob
from re import sub


def extract_blog_article_metadata(blog_article_path):
    dirname, filename = split(blog_article_path)
    title, _ = splitext(filename)
    creation_date = date(*(map(int, dirname.split("/")[1:])))
    return title, creation_date


def format_documents(content):
    r"""
    >>> format_documents("#hello\n##hello\n")
    '###hello\n'
    """
    adjusted_titles = sub(r"(#+)", r"#\1", content)
    stripped_headline = sub(r"^.*\n", "", adjusted_titles)
    return stripped_headline


@lru_cache()
def read_blog_article(blog_article_path_raw):
    # blog/2015/09/10/Who To Sue If Skype Spam Kills You.md
    blog_article_path = blog_article_path_raw.replace("_", "/")
    title, creation_date = extract_blog_article_metadata(blog_article_path)
    with open(blog_article_path_raw) as fd:
        content = format_documents(fd.read())
    return {
        'title': title,
        'creation_date': creation_date,
        'content': content, }


def get_blog_articles():
    blog_article_paths = reversed(glob('blog/**/*.md', recursive=True))
    return map(read_blog_article, blog_article_paths)


if __name__ == "__main__":
    for document in get_blog_articles():
        print(document)
