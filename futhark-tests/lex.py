import json
import re
import subprocess
import glob
from pathlib import Path

def to_futhark_test(inp, out, futhark_type):
    def to_u8(s):
        return str(s) + futhark_type

    def to_array(arr):
        assert(len(arr) != 0)
        return f'[{", ".join(map(to_u8, arr))}]'

    def to_2d_array(arr):
        if len(arr) == 0:
            return f'empty([0][3]{futhark_type})'
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
            return []
        elif match.start() != start:
            return []
        
        groups = match.groupdict()
        
        if groups.get('ignore') is not None:
            start = match.end()
            continue

        for key in regex.keys():
            if groups.get(key) is not None:
                result.append([mapping[key], start, match.end()])
                break
        
        start = match.end()

    return result

def generate_tests(inps, regex):
    mapping = {key : i for key, i in zip(regex, range(len(regex)))}
    pattern = re.compile('|'.join(f'(?P<{n}>{p})' for n, p in regex.items()))
    outs = (lexer(inp, regex, pattern, mapping) for inp in inps)
    test = ''.join(
        to_futhark_test(inp, out, 'i32')
        for inp, out in zip(inps, outs)
    )
    return test

def create_lexer(token_rules):
    return '\n'.join(f'{k}={v};' for k, v in token_rules.items())

FUTHARK_TEST_HEADER = """-- ==
-- entry: lex
"""

def generate_tests_from_json(json_source) -> None:
    json_file = Path(json_source)

    assert(json_file.exists())

    filename = f'{json_file.stem}_lexer.fut'

    data = json.load(open(json_file))
    regex = data['regex']
    tests = data['tests']
    assert(regex is not None)
    assert(tests is not None)
    
    p = subprocess.Popen(
        ['alpacc',
         '--stdin',
         '--lexer',
         f'--output={filename}'],
        shell=False,
        text=True,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    p.communicate(input=create_lexer(regex))
    assert(p.returncode == 0)

    tests = generate_tests(tests, regex)
    
    with open(filename, 'a') as fp:
        fp.write(FUTHARK_TEST_HEADER)
        fp.write(tests)

def generate_tests_from_jsons(path):
    for file in Path(path).glob('*.json'):
        generate_tests_from_json(file)

if __name__ == '__main__':
    generate_tests_from_json('lexer-tests/overlap.json')
