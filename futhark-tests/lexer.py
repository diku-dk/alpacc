import json
import re

FUTHARK_TEST_HEADER = """-- ==
-- entry: lex
"""

def to_futhark_test(inp, out, futhark_type):
    def to_u8(s):
        return str(s) + futhark_type

    def to_array(arr):
        if len(arr) == 0:
            return f'empty([0]{futhark_type})'
        return f'[{", ".join(map(to_u8, arr))}]'

    def to_2d_array(arr):
        if len(arr) == 0:
            return f'empty([0]{futhark_type})'
        return f'[{", ".join(map(to_array, arr))}]'

    def to_str(arr):
        return '"' + ''.join(map(str, arr)) + '"'

    return f"""-- input {{ {to_str(inp)} }}
-- output {{ {to_2d_array(out)} }}
"""

def lexer(string, regex, pattern, mapping):
    result = []
    start = 0
    end = len(string)

    while start != end:
        match = pattern.search(string, start)

        if match is None:
            print(test)
            return None
        elif match.start() != start:
            return None
        
        groups = match.groupdict()
        
        if groups['ignore'] is not None:
            start = match.end()
            continue

        for key in regex.keys():
            if groups[key] is not None:
                result.append([mapping[key], start, match.end() - 1])
                break
        
        start = match.end()

    return result

def generate_tests_from_json(file) -> None:
    data = json.load(open(file))
    regex = data['regex']
    tests = data['tests']
    assert(regex is not None)
    assert(tests is not None)
    return generate_tests(tests, regex)

def generate_tests(inps, regex):
    mapping = {key : i for key, i in zip(regex, range(len(regex)))}
    pattern = re.compile('|'.join(f'(?P<{n}>{p})' for n, p in regex.items()))
    outs = (lexer(inp, regex, pattern, mapping) for inp in inps)
    return '\n'.join(to_futhark_test(inp, out, 'u64') for inp, out in zip(inps, outs))

def create_lexer(token_rules):
    return '\n'.join(f'{k}={v};' for k, v in token_rules.items())

if __name__ == '__main__':
    lex = generate_tests_from_json('regex/overlap.json')
    print(lex)