import json
import re

class Lexer:

    def __init__(self, file) -> None:
        data = json.load(open(file))
        self.lexer_source = self.create_lexer(data['regex'])
    
    def create_lexer(self, token_rules):
        return '\n'.join(f'{k}={v};' for k, v in token_rules.items())

def lexer(string):
    pattern = re.compile(r'(?P<atom>[a-zA-Z0-9_]+)|(?P<lparen>\()|(?P<rparen>\))|(?P<space>\s+)')
    
    atom = 'atom'
    lparen = 'lparen'
    rparen = 'rparen'
    tokens = []
    terminals = []
    start = 0
    end = len(string)

    while start != end:
        match = pattern.search(string, start)

        if match is None:
            return None
        elif match.start() != start:
            return None
        
        groups = match.groupdict()
        
        if groups[atom] is not None:
            terminals.append(atom)
            tokens.append(groups[atom])
        elif groups[lparen] is not None:
            terminals.append(lparen)
            tokens.append(groups[lparen])
        elif groups[rparen] is not None:
            terminals.append(rparen)
            tokens.append(groups[rparen])
        
        start = match.end()

    return tokens, terminals


if __name__ == '__main__':
    lex = Lexer('regex/sexp.json')