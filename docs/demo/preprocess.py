from os import path
from sys import argv
from re import compile as regex

def process(prolog_main, prefix=None):
    if prefix:
        main = prefix + read_file(prolog_main)
    else:
        main = read_file(prolog_main)
    head = path.split(prolog_main)[0]
    pattern = regex(r':-\s*consult\((?:\'|")(?P<file>.+)(?:\'|")\)\.')
    def consult(match):
        return read_file(path.join(head, match.group('file')))
    main = pattern.sub(consult, main)
    with open('demo/main.pl', 'w') as p:
        p.write(main)


def read_file(file_path):
    content = None
    with open(file_path, mode='r') as f:
        content = f.read()
    return content

TAU_PREFIX = r'''
% import lists since tau-prolog is stupid
:- use_module(library(lists)).

% define dif/2
dif(X, Y) :- nonvar(X), nonvar(Y), X \== Y.

% program starts here
'''

if __name__ == '__main__':
    if len(argv) > 1:
        src = argv[1]
    else:
        src = '../../src/8puzzle.pl'
    process(src, TAU_PREFIX)