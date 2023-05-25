import random
import itertools
import os
import tempfile

class Production:

    def __init__(self, nonterminal, symbols) -> None:
        self.nonterminal = nonterminal
        self.symbols = symbols
    
    def __str__(self) -> str:
        return f'{self.nonterminal} -> {" ".join(self.symbols)}'

class Grammar:

    def __init__(self, start, terminals, nonterminals, productions) -> None:
        self.start = start
        self.terminals = terminals
        self.nonterminals = nonterminals
        self.productions = productions
    
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
            cmd = f'cd {folder} && printf "{grammar}" | parallel-parser --stdin -q {q} -k {k}'
            exitcode = os.system(cmd)
            if exitcode == 0:
                return grammar

def main():
    os.system('cd .. && cabal install --overwrite-policy=always')
    [generate_random_llp_grammar(2, 2, 2, 3) for _ in range(20)]
# (N,{c,x},{N,H},{N -> ,H -> ,H -> x N H,N -> x H})
# (Q,{x,n},{Q,A},{Q -> A,A -> ,A -> n Q A,A -> n Q})
# (A,{a,l},{E,A},{E -> l A A,A -> l a E,A -> ,E -> })
# (Q,{o,y},{N,Q},{N -> y Q,Q -> N N,N -> ,Q -> y})
# (E,{t,s},{V,E},{V -> t E E,E -> ,E -> s V,E -> V})
# (N,{o,m},{N,X},{N -> o N X,X -> o N,N -> ,X -> })
# (X,{g,v},{X,C},{X -> v X X,C -> ,C -> g,X -> C})
# (Z,{x,e},{Z,V},{Z -> x Z V,V -> ,Z -> V x e,Z -> })
if __name__ == '__main__':
    os.chdir(os.path.dirname(__file__))
    main()