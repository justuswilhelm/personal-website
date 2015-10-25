# Filtering Trees
Today we are going to look at how to filter items in tree data structures using
Python 3.

Let's define our task first:

Given a tree data structure in the form of nested lists, like the following

```
tree = ['root', [
  ['left_child', []],
  ['middle_child', [
    ['a leaf!', []]],
  ],
  ['right_child', [
    ['right_grandchild', [
      ['another leaf!']]]],
  ],
]
```

<blockquote class="imgur-embed-pub" lang="en" data-id="3XdcPdv"><a href="//imgur.com/3XdcPdv">View post on imgur.com</a></blockquote><script async src="//s.imgur.com/min/embed.js" charset="utf-8"></script>

Find out path to all nodes that have the value "Foobar". There are no
constraints on the tree, such as being sorted, so at least all nodes need to be
inspected at least once.

There are a few ways to do it, following one of many programming styles. We will
explore a few. What all methods have in common, that there needs to be some
mechanism of traversing the tree in order to visit all nodes, and to extract and
return the nodes that match "Foobar".

But before we work on trees, we need to randomly generate some. `gen_tree` will
recursively create a random tree with a maximum depth of 3.

```
def gen_tree(depth=0, max_depth=3):
    return (
        "Foobar" if randbool() else "Qux",
        tuple(gen_tree(depth + 1) for _ in range(randint(0, 3)))
        if depth < max_depth else tuple(),
    )
```
```
>>> pprint(gen_tree())
('Qux',
 (('Foobar', (('Qux', (('Foobar', ()),)),)),
  ('Foobar', ()),
  ('Qux',
   (('Qux', (('Qux', ()), ('Qux', ()), ('Foobar', ()))),
    ('Qux', (('Foobar', ()), ('Qux', ()), ('Qux', ()))),
    ('Foobar', (('Qux', ()), ('Qux', ()), ('Qux', ())))))))
```

Now, let's see how easily we can find foobar.

## Stack-based Traversal

If we're going to traverse a tree structure, we need to take note of which nodes
we've already visited and which we have not seen yet. If we traverse a tree in a
predetermined order, for example depth-first in {pre,in,post}-order, we only
need to be aware of which we need to visit in the future. Wikipedia has some
really nice illustrations showing the different kinds of tree traversals, so I
won't try to do a better job at it. Take a look [right
here](https://en.wikipedia.org/wiki/Tree_traversal#Depth-first).

```
def filter_tree(tree, keyword="Foobar"):
    # Our first goal is the tree's root
    # Additionally, we are going to store the path to the node
    # As the second item in the tuple
    goals = [(tree, [tree[0]])]
    while goals:
        node, path = goals.pop()  # pop the first item in the goal queue
        for child_node in node[1]:
            # the path is the current path plus the turn taken
            goals.append((child_node, path + [child_node[0]]))
        if node[0] == keyword:
            yield tuple(path)
```
```
>>> pprint(tuple(filter_tree(tree)))
(('Qux', 'Qux', 'Foobar'),
 ('Qux', 'Qux', 'Qux', 'Foobar'),
 ('Qux', 'Qux', 'Qux', 'Foobar'),
 ('Qux', 'Foobar'),
 ('Qux', 'Foobar'),
 ('Qux', 'Foobar', 'Qux', 'Foobar'))
```

## Recursion-based Traversal


```
def filter_tree_recursive(tree, path=None, keyword="Foobar"):
    path = path or tuple()
    if tree[0] == keyword:
        yield (path + (tree[0], ))
    for child_node in reversed(tree[1]):
        for result in filter_tree_recursive(child_node, path + (tree[0], )):
            yield result
```

```
>>> pprint(tuple(filter_tree_recursive(tree)))
(('Qux', 'Qux', 'Foobar'),
 ('Qux', 'Qux', 'Qux', 'Foobar'),
 ('Qux', 'Qux', 'Qux', 'Foobar'),
 ('Qux', 'Foobar'),
 ('Qux', 'Foobar'),
 ('Qux', 'Foobar', 'Qux', 'Foobar'))
```


Aww yiss
