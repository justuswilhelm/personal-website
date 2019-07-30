---
title: Hakyll on Netlify
tags:
  - haskell
  - hakyll
  - netlify
---

If you're like me, you are more busy switching between static site generators
than actually writing posts for your blog. If you take a closer look at the
[commit history](github.com/...) for this website, you will notice that I have
switched site generators several times (including to and away from my own site
generator!) But who can resist the appeal of yet another static site generator
that promises some combination of

- easy content editing
- ridiculously simple deployment
- blazing fast build times
- a vibrant plugin ecosystem that will support every flavor of Markdown
  (including straw
- easy to extend
- sane configuration that works for everyone

This time I have chosen a fabulous site generator toolkit called [hakyll](LINK)
Hakyll is a static site generator implemented in Haskell. It uses a DSL for
configuration and allows its user to build their site in almost any way they
like. Completely custom URL schema? No problem. Compile the same file using
several different compilers, including Pandoc? No problem. Compile to one easy
to deploy static executable, including all your configuration? No problem.

Now recalling the advantages that every shiny static site generator out there
promises to have, we quickly notice that Hakyll has

- easy content editing -- only when set up correctly,
- ridiculously simple deployment -- once you manage to correctly install and
  master Haskell and [Stack](https://docs.haskellstack.org/en/stable/README/)
- blazing fast build times, if you ignore that fact that building the site
  generator itself will initially take approximately 15 minutes,
- no vibrant plugin ecosystem, but a vibrant ecosystem of tutorials and
  snippets in the lower 100's range,
- easy extensibility, but only if you are willing to put in the extra effort of
  learning a functional programming level at least enough to understand Monads,
  since they're used everywhere, and a
- sane default configuration, which only does perhaps 20 % of what Jekyll can
  do out of the box (like pagination, liquid templates, data files).

As you can see, there are a lot of places where Hakyll can only shine once you
spend a considerable amount of time configuring everything. But what might
sound like a big drawback is actually Hakyll's biggest advantage. Not many site
generators give you the ability to make something that is tailored to exactly
what you are doing. That is, unless you just implement your own generator.

This is what I've done previously by created a Flask site in Python and then
turning it into a static site using [Frozen
Flask](https://pythonhosted.org/Frozen-Flask/). If Jekyll is on one end of the
site generator spectrum thanks to it's batteries-included approach, then
building your own static site generator in Python, even with the advantage of
using powerful Jinja 2 templates inside Flask, is very clearly on the other end
of the spectrum and will force you to do everything yourself, including boring
things like traversing and watching your asset or content folders so that you
can have a local live version.

Creating your own site generator is a valuable experience of course and it can
teach you a lot about how to design good build systems. At the same time, there
are some advanced features out there that just take a lot of time to implement
from scratch by yourself. One of those advanced features is tight integration
with external tools and providing several build versions of the same post. And
this is what ultimately led me to choose Hakyll for my personal website.

My ideal workflow for blogging looks like this:

1. I write down a rough draft of what I would like to say
