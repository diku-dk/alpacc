import os
import subprocess
import argparse
from . import generate_parser_test, stuck_test_timed


def main():
    parser = argparse.ArgumentParser(
        prog='AlpaccTester',
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
            'futhark pkg add github.com/diku-dk/sorts && futhark pkg add github.com/diku-dk/containers && futhark pkg sync',
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
    elif args.test_type == 'parser':
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
