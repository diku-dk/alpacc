import random
import os
import tempfile

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

def random_production(terminals, nonterminals, m, nt=None):
    all_symbols = terminals + nonterminals
    symbols = random.choices(all_symbols, k=random.randint(0, m))
    
    if nt != None:
        return Production(nt, symbols)
    
    nonterminal = random.choice(nonterminals)
    return Production(nonterminal, symbols)

def generate_grammar(k_terminals, k_nonterminals, extra_productions, m):
    ts = "abcdefghijklmnopqrstuvwxyz"
    nts = ts.upper()
    terminals = random.sample(ts, k_terminals)
    nonterminals = random.sample(nts, k_nonterminals)
    start = random.choice(nonterminals)
    sepecific_production = lambda nt: \
        random_production(terminals, nonterminals, m, nt)
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

def generate_random_llp_grammar(k_ter, k_nonter, extra_prod, m, q=1, k=1):
    with tempfile.TemporaryDirectory() as folder:
        while True:
            grammar = generate_grammar(k_ter, k_nonter, extra_prod, m)
            print(grammar)
            cmd = f'printf "{grammar}" | ./parallel-parser --stdin -q {q} -k {k}'
            exitcode = os.system(cmd)
            if exitcode == 0:
                return grammar

def main():
    # grammar = Grammar(
    #     'S',
    #     ['a'],
    #     ['S'],
    #     [Production('S', ['a', 'a', 'S']), Production('S', [])]
    # )
    os.system(f'cd .. && cabal install --installdir={os.path.dirname(__file__)} --install-method=copy --enable-executable-stripping --overwrite-policy=always')
    os.system('./parallel-parser')
    grammars = [generate_random_llp_grammar(2, 2, 2, 3) for _ in range(20)]
    # print(grammar.leftmost_derivations_index(3))

if __name__ == '__main__':
    os.chdir(os.path.dirname(__file__))
    main()