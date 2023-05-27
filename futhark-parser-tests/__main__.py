import random
import os
import multiprocessing
import shutil
import time
import numpy as np
from futhark_ffi import Futhark
from typing import Optional

class Production:

    def __init__(self, nonterminal: str, symbols: list[str]) -> None:
        self.nonterminal = nonterminal
        self.symbols = symbols
    
    def __str__(self) -> str:
        return f'{self.nonterminal} -> {" ".join(self.symbols)}'

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
        return list(map(to_index_string, left_derivations))

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
        no_left_recursion: bool = True
    ) -> Production:
    all_symbols = terminals + nonterminals
    nonterminal = nt

    if nonterminal == None:
        nonterminal = random.choice(nonterminals)
    
    if no_left_recursion:
        all_symbols.remove(nonterminal)
    symbols = random.choices(all_symbols, k=random.randint(0, m))
    return Production(nonterminal, symbols)

def generate_grammar(
        k_terminals: int,
        k_nonterminals: int,
        extra_productions: int,
        m: int,
        no_left_recursion: bool = True
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
            no_left_recursion=no_left_recursion
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

def generate_random_llp_grammar(path, k_ter, k_nonter, extra_prod, m, q=1, k=1):
    while True:
        grammar = generate_grammar(k_ter, k_nonter, extra_prod, m)
        print(f'Grammar: {grammar}')
        cmd = f'printf "{grammar}" | ./parallel-parser --stdin --output="{path}" -q {q} -k {k}'
        exitcode = os.system(cmd)
        if os.path.exists(path) and exitcode == 0:
            return (path, grammar)

def stuck_check(n: int):
    for i in range(n):
        filename = f'temp-{i}.fut'
        generate_random_llp_grammar(
            filename,
            3,
            3,
            3,
            3
        )
        os.remove(filename)
        time.sleep(0.05)

def stuck_test(n: int):
    p = multiprocessing.Process(
        target=stuck_check,
        name="stuck_check",
        args=(n,)
        )
    p.start()
    p.join(n * 5)
    p.kill()
    return p.exitcode

def can_parser_test(n: int):
    grammars = [
        generate_random_llp_grammar(
            f'parser-{i}.fut',
            random.randint(2, 5),
            random.randint(2, 5),
            random.randint(2, 5),
            random.randint(2, 6)
        ) for i in range(20)
    ]

    for path, grammar in grammars:
        assert 0 == os.system(f'futhark c --library {path}.fut'), 'The parser could not be compiled.'

def main():
    assert 0 == os.system(f'cd .. && cabal install --installdir={os.path.dirname(__file__)} --install-method=copy --enable-executable-stripping --overwrite-policy=always'), "Could not compile the parallel parser generator."
    assert os.path.exists('./parallel-parser'), "The parallel-parser binaries does not exists."
    assert 0 == stuck_test(1000), "The parser probably got stuck while creating some grammar."

# import _parser

if __name__ == '__main__':
    # parser = Futhark(_parser)
    # futhark multicore --library parser.fut
    # build_futhark_ffi parser
    # res = parser.parse(np.array([0, 0, 1, 1, 1, 2, 2, 3]))
    # print(parser.from_futhark(res))
    
    os.chdir(os.path.dirname(__file__))
    assert(0 == os.system('futhark pkg add github.com/diku-dk/sorts && futhark pkg sync'))
    old_content = set(os.listdir())
    main()
    new_content = set(os.listdir()) - old_content
    for content in new_content:
        if os.path.isdir(content):
            shutil.rmtree(content)
        elif os.path.isfile(content):
            os.remove(content)