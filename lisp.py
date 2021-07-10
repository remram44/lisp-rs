class ProgramError(Exception):
    """Error running program"""


DEFAULT_ENV = {}


def eval(expr, env=None):
    if env is None:
        env = DEFAULT_ENV
    if isinstance(expr, str):
        try:
            kind, value = env[expr]
        except KeyError:
            raise ProgramError("Unbound atom %s" % expr)
        if kind == 'value':
            return value
        else:
            raise ProgramError("Can't use %s %r in this context" % (kind, expr))
    elif not isinstance(expr, list):
        raise ValueError("Expected str or list, got %s" % type(expr))
    else:
        # Try to call a macro
        if isinstance(expr[0], str):
            try:
                kind, value = env[expr[0]]
            except KeyError:
                pass
            else:
                if kind == 'macro':
                    return value(expr, env)

        # Default to function call
        return call_func(expr, env)


def call_func(expr, env):
    fn = eval(expr[0], env)
    args = [eval(a, env) for a in expr[1:]]
    return fn(args)


def register(dct, kind):
    def wrapper(func):
        dct[func.__name__.lstrip('_')] = kind, func
        return func
    return wrapper


# Macros

@register(DEFAULT_ENV, 'macro')
def _quote(expr, env):
    if len(expr) != 2:
        raise ProgramError("Wrong number of arguments to quote")
    return expr[1]


@register(DEFAULT_ENV, 'macro')
def _set(expr, env):
    if len(expr) < 4 or len(expr) % 2 != 0:
        raise ProgramError("Wrong number of arguments to set: %d" % len(expr - 1))
    new_env = dict(env)
    for i in range(1, len(expr) - 1, 2):
        new_env[expr[i]] = 'value', eval(expr[i + 1], env)
    return eval(expr[-1], new_env)


@register(DEFAULT_ENV, 'macro')
def _lambda(expr, env):
    if len(expr) != 3:
        raise ProgramError("Wrong number of arguments to lambda")
    arg_names = expr[1]
    body = expr[2]
    # We could use a Python closure here but that would mask things
    return Function(arg_names, body, env)


class Function:
    def __init__(self, arg_names, body, env):
        self.arg_names = arg_names
        self.body = body
        self.env = env

    def __call__(self, args):
        if len(args) != len(self.arg_names):
            raise ProgramError("Wrong number of arguments to lambda function")
        new_env = dict(self.env)
        for name, value in zip(self.arg_names, args):
            new_env[name] = 'value', value
        return eval(self.body, new_env)


@register(DEFAULT_ENV, 'macro')
def _defmacro(expr, env):
    if len(expr) != 5:
        raise ProgramError("Wrong number of arguments to defmacro")
    name = expr[1]
    arg_names = expr[2]
    body = expr[3]
    new_env = dict(env)
    new_env[name] = 'macro', Macro(arg_names, body, env)
    return eval(expr[4], new_env)


class Macro:
    def __init__(self, arg_names, body, env):
        self.arg_names = arg_names
        self.body = body
        self.env = env

    def __call__(self, expr, env):
        if len(expr) - 1 != len(self.arg_names):
            raise ProgramError("Wrong number of arguments to macro")

        # Call the macro body, in the definition's environment
        macro_env = dict(self.env)
        for name, value in zip(self.arg_names, expr[1:]):
            macro_env[name] = 'value', value
        code = eval(self.body, macro_env)

        # Now evaluate the result, in the caller's environment
        return eval(code, env)


# Functions

@register(DEFAULT_ENV, 'value')
def _cons(args):
    if len(args) != 2:
        raise ProgramError("Wrong number of arguments to cons")
    return [args[0]] + args[1]


# Tests

assert eval(['cons', ['quote', 'a'], ['quote', []]]) == ['a']
assert eval(['cons', ['quote', 'a'], ['quote', ['b', 'c']]]) == ['a', 'b', 'c']
assert eval([['lambda', ['a', 'b'], 'b'], ['quote', 'd'], ['quote', 'e']]) == 'e'
assert eval(['set', 'f', ['lambda', ['a', 'b'], 'b'], ['f', ['quote', 'd'], ['quote', 'e']]]) == 'e'
assert eval(['cons', ['quote', 'quote'], ['quote', ['a']]]) == ['quote', 'a']
assert eval(['defmacro', 'm', ['x'], ['cons', ['quote', 'quote'], ['quote', ['a']]], ['m', 'a']]) == 'a'
