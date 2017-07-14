"""Contentful access."""
from os import environ

import contentful
from dotenv import load_dotenv, find_dotenv

load_dotenv(find_dotenv())

SPACE_ID = environ['CONTENTFUL_SPACE_ID']
ACCESS_TOKEN = environ['CONTENTFUL_ACCESS_TOKEN']

client = contentful.Client(SPACE_ID, ACCESS_TOKEN)
