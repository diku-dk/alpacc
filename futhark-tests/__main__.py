import random
import os
import multiprocessing
import shutil
import time
import sys
import itertools
import subprocess
import argparse
from typing import Optional


class Mute():

    def __init__(self):
        self.old_stdout = None
        self.old_stderr = None
    

    def __enter__(self):
        self.old_stdout = sys.stdout
        sys.stdout = open(os.devnull, "w")
        self.old_stderr = sys.stderr
        sys.stderr = open(os.devnull, "w")
    
    
    def __exit__(self, *args, **kwargs):
        sys.stdout = self.old_stdout
        sys.stderr = self.old_stderr

class DeleteNew():

    def __init__(self):
        self.old_content = None
    

    def __enter__(self):
        self.old_content = set(os.listdir())
    
    def __exit__(self, *args, **kwargs):
        new_content = set(os.listdir()) - self.old_content
        for content in new_content:
            if os.path.isdir(content):
                shutil.rmtree(content)
            elif os.path.isfile(content):
                os.remove(content)
        self.old_content = None

class Production:

    def __init__(self, nonterminal: str, symbols: list[str]) -> None:
        self.nonterminal = nonterminal
        self.symbols = symbols

    def __hash__(self):
        return hash(self.nonterminal + "".join(self.symbols))

    def __eq__(self, other):
        return self.nonterminal + "".join(self.symbols) == other

class Grammar:

    def __init__(
        self,
        start: str,
        terminals: list[str],
        nonterminals: list[str],
        productions: list[Production]
        ) -> None:
        self.start = start
        self.terminals = terminals
        self.nonterminals = nonterminals
        self.productions = productions
        self.production_map = self.create_production_map()
        self.productions = self.renumber_productions()
    
    def remove_duplicates(self):
        self.terminals = list(dict.fromkeys(self.terminals))
        self.nonterminals = list(dict.fromkeys(self.nonterminals))
        self.productions = list(dict.fromkeys(self.productions))
        self.production_map = self.create_production_map()
        self.productions = self.renumber_productions()
    
    def create_production_map(self):
        
        result = {nt: [] for nt in self.nonterminals}
        for production in self.productions:
            result[production.nonterminal].append(''.join(production.symbols))
        return result

    def renumber_productions(self):
        # Construct productions matching the order in self.production_map.
        result = []

        def prod(nt):
            nonlocal result
            result += [Production(nt, [c for c in r]) for r in self.production_map[nt]]

        prod(self.start)
        for nt in self.nonterminals:
            if nt == self.start:
                continue
            prod(nt)

        return result
    
    def left_derive(self, idx, string):

        result = []
        for symbols in self.production_map[string[idx]]:
            temp = string[:idx] + symbols + string[idx+1:] 
            for i in range(idx, len(temp)):
                if temp[i] in self.nonterminals:
                    result.append((i, temp))
                    break
            else:
                result.append((-1, temp))
        
        return result

    def leftmost_derivations(self, k: int) -> set[str]:
        visisted = set()
        derivable = set()
        queue = [(0, self.start)]

        while 0 < len(queue):
            (idx, item) = queue.pop()

            if item[:k + 1] in visisted:
                continue

            visisted.add(item[:k + 1])

            if all(map(lambda s: s in self.terminals, item)):

                if len(item) > k:
                    continue

                derivable.add(item)
                continue


            queue.extend(self.left_derive(idx, item))
        
        return derivable

    def leftmost_derivations_index(self, k):
        left_derivations = list(self.leftmost_derivations(k))
        to_index_map = dict(zip(self.terminals, range(len(self.terminals))))
        to_index_string = lambda s : list(map(to_index_map.get, s))
        return list(map(lambda a: (a, to_index_string(a)), left_derivations))

    def __str__(self) -> str:
        s = ''
        for t in self.terminals:
            s += f'{t} = "{t}";\n'
        def prod(nt):
            nonlocal s
            s += f'{nt} = {" | ".join([" ".join([c for c in r]) for r in self.production_map[nt]])};\n'
        prod(self.start)
        for nt in self.nonterminals:
            if nt == self.start:
                continue
            prod(nt)
        return s

def concat(iterable):
    result = []
    for ls in iterable:
        result.extend(ls)
    return result

class ExtendedGrammar(Grammar):

    def __init__(self, k: int, start: str, terminals: list[str], nonterminals: list[str], productions: list[Production]) -> None:
        self.old_start = start
        self.stopper = '$' * k
        new_start = '&'
        new_terminals = concat([['$'], terminals])
        new_nonterminals = concat([['&'], nonterminals])
        new_productions = concat([[Production(new_start, concat([[start], list(self.stopper)]))], productions])

        super().__init__(
            new_start,
            new_terminals,
            new_nonterminals,
            new_productions
        )
    
    @classmethod
    def extend(ctx, k: int, grammar: Grammar):
        return ExtendedGrammar(
            k,
            grammar.start,
            grammar.terminals,
            grammar.nonterminals,
            grammar.productions
        )

def truncated_product(k: int, set_a: set[str], set_b: set[str]):
    return set([(a + b)[:k] for a in list(set_a) for b in list(set_b)])

def every_split(string: str):
    return set((string[:i], string[i:]) for i in range(1,len(string)))

def alpha_beta(k: int, grammar: Grammar, first_mapping: dict[str: set[set]], string: str):
    
    if len(string) == 0:
        return {''}
    elif len(string) == 1:
        if string in grammar.terminals:
            return {string}
        else:
            return first_mapping[string]
    
    return unions([
        truncated_product(
            k,
            alpha_beta(k, grammar, first_mapping, a),
            alpha_beta(k, grammar, first_mapping, b)
        ) for a, b in every_split(string)
    ])

def create_first(k: int, grammar: Grammar):
    final_first_map = first_map(k, grammar)
    return lambda s: alpha_beta(k, grammar, final_first_map, s)

def unions(iterable):
    return set().union(*iterable)

def first_map(k: int, grammar: Grammar):
    init_map = {nt: set() for nt in grammar.nonterminals}

    def auxiliary(first_mapping):
        new_first_set = lambda s: alpha_beta(k, grammar, first_mapping, s)
        return {
            nt: unions(map(new_first_set, grammar.production_map[nt]))
            for nt in grammar.nonterminals
        }

    old_map = dict()
    new_map = init_map
    while old_map != new_map:
        old_map = new_map
        new_map = auxiliary(new_map)
    
    return new_map

def right_symbols(grammar: Grammar, string: str):

    result = []
    for i in range(len(string)):
        if string[i] in grammar.nonterminals:
            result.append((string[i], string[1+i:]))
    return result

def follow_map(k: int, grammar: ExtendedGrammar, first_func):
    
    init_map = {nt: set() for nt in grammar.nonterminals}
    init_map[grammar.start] = {grammar.stopper}

    def auxiliary(follow_mapping):
        
        def new_follow_set(nt, pair):
            return (pair[0], truncated_product(
            k,
            first_func(pair[1]),
            follow_mapping[nt]
        ))

        def follow_sets(production):
            new_follow_set_f = lambda s: new_follow_set(production.nonterminal, s)
            return list(map(new_follow_set_f,
            right_symbols(grammar, ''.join(production.symbols))
        ))

        new_sets = concat(map(follow_sets, grammar.productions))
        result_map = dict(follow_mapping)

        for nt, follow_set in new_sets:
            result_map[nt] = result_map[nt] | (follow_set)

        return result_map

    old_map = dict()
    new_map = init_map

    while old_map != new_map:
        old_map = new_map
        new_map = auxiliary(new_map)
    
    return new_map

class LLParser:
    
    def __init__(self, k: int, grammar: Grammar) -> None:
        self.k = k
        self.grammar = ExtendedGrammar.extend(k, grammar)
        self.first = create_first(k, self.grammar)
        self.follow = follow_map(k, self.grammar, self.first)
        self.table = self.creat_ll_table()

    def creat_ll_table(self):
        
        prods = self.grammar.productions

        def auxiliary(i):
            symbols = ''.join(prods[i].symbols)
            nonterminal = prods[i].nonterminal
            sets = truncated_product(
                self.k,
                self.first(symbols),
                self.follow[nonterminal]
            )
            return {
                (nonterminal, string) : i for string in sets
            }
        
        result_table = dict()
        keys_set = set()
        for i in range(len(prods)):
            sub_table = auxiliary(i)
            new_keys = set(sub_table.keys())
            if len(new_keys & keys_set) != 0:
                return None
            keys_set |= new_keys
            result_table.update(sub_table)
        
        return result_table

    def parse(self, string):
        string += self.grammar.stopper
        prods = []
        stack = [self.grammar.start]

        while True:
            if len(string) == 0:
                return prods[1:]
            elif stack[0] == string[0]:
                string = string[1:]
                stack.pop(0)
            else:
                i = self.table[(stack.pop(0), string[:self.k])]
                stack = self.grammar.productions[i].symbols + stack
                prods.append(i - 1)

def random_production(
        terminals: list[str],
        nonterminals: list[str],
        m: int,
        nt: Optional[str] = None,
        no_direct_left_recursion: bool = True
    ) -> Production:
    all_symbols = terminals + nonterminals
    nonterminal = nt

    if nonterminal == None:
        nonterminal = random.choice(nonterminals)
    
    if no_direct_left_recursion:
        all_symbols.remove(nonterminal)
    
    symbols = random.choices(all_symbols, k=random.randint(0, m))
    return Production(nonterminal, symbols)

def generate_grammar(
        k_terminals: int,
        k_nonterminals: int,
        extra_productions: int,
        m: int,
        no_direct_left_recursion: bool = True
        ) -> Grammar:
    ts = "abcdefghijklmnopqrstuvwxyz"
    nts = ts.upper()
    terminals = random.sample(ts, k_terminals)
    nonterminals = random.sample(nts, k_nonterminals)
    sepecific_production = lambda nt: random_production(
            terminals=terminals,
            nonterminals=nonterminals,
            m=m,
            nt=nt,
            no_direct_left_recursion=no_direct_left_recursion
        )
    production = lambda: random_production(terminals, nonterminals, m)
    specific_prooducions = [sepecific_production(nt) for nt in nonterminals]
    extra_productions = [production() for _ in range(extra_productions)]
    productions = specific_prooducions + extra_productions
    return Grammar(
        nonterminals[0],
        terminals,
        nonterminals,
        productions
    )

def generate_random_llp_grammar(
        name,
        k_ter,
        k_nonter,
        extra_prod,
        m,
        q=1,
        k=1,
        no_direct_left_recursion: bool = True,
        no_duplicates: bool = True,
        quiet: bool = False):
    
    generated_count = 0
    while True:
        filename = f'{name}.fut'
        grammar = generate_grammar(
            k_terminals=k_ter,
            k_nonterminals=k_nonter,
            extra_productions=extra_prod,
            m=m,
            no_direct_left_recursion=no_direct_left_recursion
        )

        if no_duplicates:
            grammar.remove_duplicates()

        could_create = False
        p = subprocess.Popen(
            ['parallel-parser',
             '--stdin',
             f'--output={filename}',
             f'--lookback={q}',
             f'--lookahead={k}'],
            shell=False,
            text=True,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
        )
        p.communicate(input=str(grammar))
        if p.returncode == 0:
            could_create = True

        generated_count += 1
        
        if os.path.exists(f'{filename}') and could_create:
            if not quiet:
                print(f'{filename} contains a parser for the grammar: {grammar}.')
            return name, grammar, generated_count

def stuck_test(number_of_grammars: int, q: int = 1, k: int = 1):
    count = 0
    for i in range(number_of_grammars):
        with DeleteNew():
            filename = f'temp_{i}'
            _, _, generated_count = generate_random_llp_grammar(
                filename,
                3,
                3,
                3,
                3,
                q=q,
                k=k,
                no_direct_left_recursion=False,
                no_duplicates=False
            )
            count += generated_count
        time.sleep(0.05)
    acceptance_percent = 100 * number_of_grammars / count
    print(f'{acceptance_percent:.02f}% of grammars were accepted.')

def stuck_test_timed(number_of_grammars: int, q: int = 1, k: int = 1):
    p = multiprocessing.Process(
        target=stuck_test,
        name="stuck_check",
        args=(number_of_grammars, q, k)
        )
    p.start()
    p.join(18000) # 5 hours.
    p.kill()

    if p.exitcode == 0:
        print('The parser generator stuck test succeeded.')
    else:
        print('The parser generator stuck test failed.')

    return p.exitcode

FUTHARK_TEST_HEADER = """-- ==
-- entry: parse
"""

def to_futhark_test(inp, out):
    def to_u32(s):
        return str(s) + 'u32'

    def to_array(arr):
        if len(arr) == 0:
            return 'empty([0]u32)'
        return f'[{", ".join(map(to_u32, arr))}]'

    return f"""-- input {{ {to_array(inp)} }}
-- output {{ {to_array(out)} }}
"""


def generate_parser_test(
        valid_string_length: int,
        invalid_string_length: int,
        number_of_grammars: int, 
        q: int,
        k: int
    ):
    root_name = f'parser_llp{q}{k}'

    terminal_min = 2
    terminal_max = 3

    string_combinations = {
        i: list(
            itertools.chain(*(
                itertools.product(range(i), repeat=k) 
                for k in range(1, invalid_string_length+1)
                ))
            )
        for i in range(terminal_min, terminal_max+1)
    }

    grammars = (
        generate_random_llp_grammar(
            f'{root_name}_{i}',
            random.randint(2, 3),
            random.randint(2, 3),
            random.randint(2, 5),
            random.randint(2, 5),
            no_direct_left_recursion=True,
            no_duplicates=True,
            q=q,
            k=k,
            quiet=True
        ) for i in range(number_of_grammars)
    )

    for name, grammar, _ in grammars:
        valid_strings = grammar.leftmost_derivations_index(
            valid_string_length
        )
        valid_strings_set = set(map(lambda x: tuple(x[1]), valid_strings))
        ll_parser = LLParser(k, grammar)
        
        with open(f'{name}.fut', 'a') as fp:
            fp.write('\n'.join(['-- ' + l for l in str(grammar).splitlines()]))
            fp.write(f'\n{FUTHARK_TEST_HEADER}')
            for string, indices in valid_strings:
                expected = ll_parser.parse(string)
                fp.write(f'{to_futhark_test(indices, expected)}')
            
            for indices in string_combinations[len(grammar.nonterminals)]:
                if indices in valid_strings_set:
                    continue
                
                fp.write(f'{to_futhark_test(indices, [])}')

def main():
    test_dir = os.path.dirname(__file__)

    parser = argparse.ArgumentParser(
                    prog='ParallelParserTester',
                    description='Program for testing the Futhark parsers')

    parser.add_argument('-t', '--test-type')
    parser.add_argument('-q', '--lookback', type=int)
    parser.add_argument('-k', '--lookahead', type=int)
    parser.add_argument('-s', '--grammar-size', type=int)
    parser.add_argument('-v', '--valid-size', type=int)
    parser.add_argument('-i', '--invalid-size', type=int)

    args = parser.parse_args()

    assert args.test_type is not None, "test-type must be set."

    if args.test_type == 'setup':
        assert 0 == subprocess.check_call(
            'futhark pkg add github.com/diku-dk/sorts && futhark pkg sync',
            shell=True
        ), "Futharks sorts lib could not be retrieved."
    elif args.test_type == 'stuck':
        assert args.grammar_size is not None, "grammar-size must be set."
        assert args.lookback is not None, "lookback must be set."
        assert args.lookahead is not None, "lookahead must be set."
        assert 0 == stuck_test_timed(
            number_of_grammars=args.grammar_size,
            q=args.lookback,
            k=args.lookahead
        ), "The parser probably got stuck while creating some grammar."
    elif args.test_type == 'parse':
        assert args.grammar_size is not None, "grammar-size must be set."
        assert args.valid_size is not None, "valid-size must be set."
        assert args.invalid_size is not None, "invalid-size must be set."
        assert args.lookback is not None, "lookback must be set."
        assert args.lookahead is not None, "lookahead must be set."
        generate_parser_test(
            valid_string_length=args.valid_size,
            invalid_string_length=args.invalid_size,
            number_of_grammars=args.grammar_size,
            q=args.lookback,
            k=args.lookahead
        )
        print('The tests have been generated.')
    else:
        raise Exception(f'"{args.test_type}" is not a test type.')

if __name__ == '__main__':
    test_dir = os.path.dirname(__file__)
    os.chdir(test_dir)
    main()
