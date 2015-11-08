def tokenize(s):
    """
    >>> tokenize("(quote (+ 1 2) + (1 (1)))")
    ['(', 'quote', '(', '+', 1, 2, ')', '+', '(', 1, '(', 1, ')', ')', ')']
    """
    return list(map(lambda s: int(s) if s.isnumeric() else s, filter(
        bool, s.replace('(', '( ').replace(')', ' )').split(" "))))


def parse(tokens):
    """
    >>> parse(['(', '(', '+', 1, 2, ')', '+', '(', 1, '(', 1, ')', ')', ')'])
    (('+', 1, 2), '+', (1, (1,)))
    """
    def _list():
        tokens.pop(0)
        while tokens[0] != ')':
            yield parse(tokens)
        tokens.pop(0)
    return tuple(_list()) if tokens[0] == '(' else tokens.pop(0)
