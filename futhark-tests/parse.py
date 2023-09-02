import random
import os
import multiprocessing
import time
import itertools
import subprocess
from typing import Optional
from util import DeleteNew


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
        # This limit is made because the u8 type is hardcoded in the tests.
        # If the number of terminals were to exceed the parser generator would
        # change the type.
        assert (len(terminals) <= 254)
        assert (len(productions) <= 255)
        self.nonterminals = nonterminals
        self.productions = productions
        self.production_map = self.create_production_map()
        self.productions = self.renumber_productions()
        self.production_index_map = self.index_map()

    def remove_duplicates(self):
        self.terminals = list(dict.fromkeys(self.terminals))
        self.nonterminals = list(dict.fromkeys(self.nonterminals))
        self.productions = list(dict.fromkeys(self.productions))
        self.production_map = self.create_production_map()
        self.productions = self.renumber_productions()
        self.production_index_map = self.index_map()

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
            result += [Production(nt, [c for c in r])
                       for r in self.production_map[nt]]

        prod(self.start)
        for nt in self.nonterminals:
            if nt == self.start:
                continue
            prod(nt)

        return result

    def index_map(self):
        return dict(zip(self.productions, range(len(self.productions))))

    def left_derive(self, idx, string, derivation):
        
        if idx < 0:
            return []

        def to_index(nt, symbols):
            prod = Production(nt, symbols)
            return self.production_index_map[prod]

        result = []
        nt = string[idx]

        for symbols in self.production_map[nt]:
            temp = string[:idx] + symbols + string[idx+1:]
            new_derivation = derivation.copy()
            new_derivation.append(to_index(nt, symbols))
            for i in range(idx, len(temp)):
                if temp[i] in self.nonterminals:
                    result.append((i, temp, new_derivation))
                    break
            else:
                result.append((-1, temp, new_derivation))

        return result

    def leftmost_derivations(self, k: int) -> dict[str, list[str]]:
        visisted = set()
        derivable = dict()
        queue = [(0, self.start, [])]

        while 0 < len(queue):
            (idx, item, derivation) = queue.pop()

            if item[:k + 1] in visisted:
                continue

            visisted.add(item[:k + 1])

            if all(map(lambda s: s in self.terminals, item)):

                if len(item) > k:
                    continue
                
                derivable[item] = derivation
                continue

            queue.extend(self.left_derive(idx, item, derivation))

        return derivable

    def leftmost_derivations_index(self, k) -> dict[tuple[int], list[int]]:
        left_derivations = self.leftmost_derivations(k)
        to_index_map = dict(zip(self.terminals, range(len(self.terminals))))
        
        def to_index_string(s):
            return tuple(map(to_index_map.get, s))
        
        return dict(
            [(to_index_string(k), v) for k, v in left_derivations.items()]
        )

    def __str__(self) -> str:
        s = ''
        for t in self.terminals:
            s += f'{t} = {t};\n'

        def prod(nt):
            nonlocal s
            s += f'{nt} = {" | ".join([" ".join([c for c in r]) for r in self.production_map[nt]])};\n'
        prod(self.start)
        for nt in self.nonterminals:
            if nt == self.start:
                continue
            prod(nt)
        return s


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

    def sepecific_production(nt): return random_production(
        terminals=terminals,
        nonterminals=nonterminals,
        m=m,
        nt=nt,
        no_direct_left_recursion=no_direct_left_recursion
    )
    def production(): return random_production(terminals, nonterminals, m)
    specific_prooducions = [sepecific_production(nt) for nt in nonterminals]
    extra_productions = [production() for _ in range(extra_productions)]
    productions = specific_prooducions + extra_productions

    # Shuffle the nonterminals as we use the first nonterminal as the
    # starting one, and it tends to be quite boring.
    random.shuffle(nonterminals)

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
            ['alpacc',
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
                print(f'{filename} contains a parser for the grammar:\n{grammar}')
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
    p.join(18000)  # 5 hours.
    p.kill()

    if p.exitcode == 0:
        print('The parser generator stuck test succeeded.')
    else:
        print('The parser generator stuck test failed.')

    return p.exitcode


FUTHARK_TEST_HEADER = """-- ==
-- entry: parse
"""

def to_futhark_test(inp, out, futhark_type):
    def to_u8(s):
        return str(s) + futhark_type

    def to_array(arr):
        if len(arr) == 0:
            return f'empty([0]{futhark_type})'
        return f'[{", ".join(map(to_u8, arr))}]'

    def to_str(arr):
        return '"' + ''.join(map(str, arr)) + '"'

    return f"""-- input {{ {to_str(inp)} }}
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

        def indices_to_terminals(indices):
            return [grammar.terminals[i] for i in indices]

        with open(f'{name}.fut', 'a') as fp:
            fp.write('\n'.join(['-- ' + l for l in str(grammar).splitlines()]))
            fp.write(f'\n{FUTHARK_TEST_HEADER}')
            for indices, expected in valid_strings.items():
                string = indices_to_terminals(indices)
                fp.write(f'{to_futhark_test(string, expected, "u8")}')

            for indices in string_combinations[len(grammar.terminals)]:
                if indices in valid_strings:
                    continue
                string = indices_to_terminals(indices)
                fp.write(to_futhark_test(string, [], 'u8'))
