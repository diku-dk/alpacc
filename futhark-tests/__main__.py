import random
import os
import multiprocessing
import shutil
import time
import sys
import numpy as np
import itertools
import subprocess
from futhark_ffi import Futhark
from typing import Optional


class Mute():

    def __init__(self):
        self.old_stdout = None
    

    def __enter__(self):
        self.old_stdout = sys.stdout
        sys.stdout = open(os.devnull, "w")
    
    
    def __exit__(self, *args, **kwargs):
        sys.stdout = self.old_stdout

class Production:

    def __init__(self, nonterminal: str, symbols: list[str]) -> None:
        self.nonterminal = nonterminal
        self.symbols = symbols
    
    def __str__(self) -> str:
        return f'{self.nonterminal} -> {" ".join(self.symbols)}'

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
    
    def remove_duplicates(self):
        self.terminals = list(dict.fromkeys(self.terminals))
        self.nonterminals = list(dict.fromkeys(self.nonterminals))
        self.productions = list(dict.fromkeys(self.productions))
        self.production_map = self.create_production_map()
    
    def create_production_map(self):
        
        result = {nt: [] for nt in self.nonterminals}
        for production in self.productions:
            result[production.nonterminal].append(''.join(production.symbols))

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

            if item[:k] in visisted:
                continue

            visisted.add(item[:k])

            if all(map(lambda s: s in self.terminals, item)):
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
        return (f'({self.start},'
                f'{{{",".join(self.terminals)}}},'
                f'{{{",".join(self.nonterminals)}}},'
                f'{{{",".join(map(str, self.productions))}}})')

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
    start = random.choice(nonterminals)
    sepecific_production = lambda nt: random_production(
            terminals,
            nonterminals,
            m,
            nt,
            no_direct_left_recursion=no_direct_left_recursion
        )
    production = lambda: random_production(terminals, nonterminals, m)
    specific_prooducions = [sepecific_production(nt) for nt in nonterminals]
    extra_productions = [production() for _ in range(extra_productions)]
    productions = specific_prooducions + extra_productions
    return Grammar(
        start,
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
        no_duplicates: bool = True):
    
    while True:
        filename = f'{name}.fut'
        grammar = generate_grammar(
            k_ter,
            k_nonter,
            extra_prod,
            m,
            no_direct_left_recursion
        )

        if no_duplicates:
            grammar.remove_duplicates()
        
        cmd = f'printf "{grammar}" | ./parallel-parser --stdin --output="{filename}" -q {q} -k {k}'

        could_create = False
        try:
            subprocess.check_call(
                cmd,
                shell=True,
                stdout=open(os.devnull, 'wb'),
                stderr=open(os.devnull, 'wb')
            )
            could_create = True
        except subprocess.CalledProcessError:
            pass

        if os.path.exists(f'{filename}') and could_create:
            print(f'{filename} contains a parser for the grammar: {grammar}.')
            return name, grammar

def stuck_test(number_of_grammars: int):
    for i in range(number_of_grammars):
        filename = f'temp_{i}'
        generate_random_llp_grammar(
            filename,
            3,
            3,
            3,
            3,
            no_direct_left_recursion=True,
            no_duplicates=True
        )
        os.remove(f'{filename}.fut')
        time.sleep(0.05)

def stuck_test_timed(number_of_grammars: int):
    p = multiprocessing.Process(
        target=stuck_test,
        name="stuck_check",
        args=(number_of_grammars,)
        )
    p.start()
    p.join(18000) # 5 hours.
    p.kill()

    if p.exitcode == 0:
        print('The parser generator stuck test succeeded.')
    else:
        print('The parser generator stuck test failed.')

    return p.exitcode

def parser_test(
        valid_string_length: int,
        invalid_string_length: int,
        number_of_grammars: int
    ):
    terminal_min = 2
    terminal_max = 3

    string_combinations = {
        i: list(itertools.product(range(i), repeat=invalid_string_length))
        for i in range(terminal_min, terminal_max+1)
    }

    grammars = [
        generate_random_llp_grammar(
            f'parser_{i}',
            random.randint(2, 3),
            random.randint(2, 3),
            random.randint(2, 6),
            random.randint(2, 6),
            no_direct_left_recursion=True,
            no_duplicates=True
        ) for i in range(number_of_grammars)
    ]
    error = False

    for name, grammar in grammars:
        assert 0 == subprocess.check_call(
            f'futhark c --library {name}.fut',
            shell=True
        ), 'The parser could not be compiled.'
        assert 0 == subprocess.check_call(
            f'build_futhark_ffi {name}',
            shell=True
        )
        parser = Futhark(__import__(f'_{name}'))
        valid_strings = grammar.leftmost_derivations_index(valid_string_length)
        valid_strings_set = set(map(lambda x: tuple(x[1]), valid_strings))
        for string, indices in valid_strings:
            futhark_result = parser.parse(np.array(list(indices)))
            result = parser.from_futhark(futhark_result)
            if len(result) == 0:
                print(
                    (f'The string {string} from the grammar {grammar} could'
                     ' not be parsed.')
                )
                error = True
        
        for indices in string_combinations[len(grammar.nonterminals)]:
            if indices in valid_strings_set:
                continue

            futhark_result = parser.parse(np.array(list(indices)))
            result = parser.from_futhark(futhark_result)
            if len(result) != 0:
                print(result)
                error = True
    
    if error:
        print("Parsing tests failed.")
    else:
        print("Parsing tests succeeded.")

    return error

def main():
    assert 0 == subprocess.check_call(
        (f'cd .. && cabal install --installdir={test_dir} '
         '--install-method=copy --enable-executable-stripping '
         '--overwrite-policy=always'),
        shell=True
    ), "Could not compile the parallel parser generator."
    assert os.path.exists(
        './parallel-parser'
    ), "The parallel-parser binaries does not exists."
    # assert 0 == stuck_test_timed(
    #     number_of_grammars=1000
    # ), "The parser probably got stuck while creating some grammar."
    assert not parser_test(
        valid_string_length=20,
        invalid_string_length=10,
        number_of_grammars=3
    ), "Not all tested strings for some grammar could be parsed."

if __name__ == '__main__':
    test_dir = os.path.dirname(__file__)
    os.chdir(test_dir)
    old_content = set(os.listdir())
    main()
    new_content = set(os.listdir()) - old_content
    for content in new_content:
        if os.path.isdir(content):
            shutil.rmtree(content)
        elif os.path.isfile(content):
            os.remove(content)