from pprint import pprint
from random import randint, seed
seed(1)


def randbool():
    return bool(randint(0, 1))


def gen_tree(depth=0, max_depth=3):
    return (
        "Foobar" if randbool() else "Qux",
        tuple(gen_tree(depth + 1) for _ in range(randint(0, 3)))
        if depth < max_depth else tuple(),
    )


tree = gen_tree()
pprint(tree)


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

pprint(tuple(filter_tree(tree)))


def filter_tree_recursive(tree, path=None, keyword="Foobar"):
    path = path or tuple()
    if tree[0] == keyword:
        yield (path + (tree[0], ))
    for child_node in reversed(tree[1]):
        for result in filter_tree_recursive(child_node, path + (tree[0], )):
            yield result


pprint(tuple(filter_tree_recursive(tree)))
