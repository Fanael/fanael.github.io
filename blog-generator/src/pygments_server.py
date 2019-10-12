#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""
A simple pygments server, communicating over pipes, to reuse the same process
for multiple highlightings instead of spawning a new pygmentize process for
every one.

The protocol is very simple: every command is on its own line, followed by a
constant number of arguments, also one per line.

When the server is done processing a command, it sends ":done" followed by a
newline to the client.

On error, the server simply prints the stack trace to stderr and exits.
"""
import sys
import pygments as p
import pygments.lexers as lex
import pygments.formatter as fmt
import pygments.token as tok

# Dict of known token types and a boolean indicating if it should be styled
_KNOWN_TOKENS = {
    tok.Token: False,
    tok.Comment: True,
    tok.Keyword: True,
    tok.String: True,
    tok.String.Escape: True,
    tok.String.Interpol: True,
    tok.Number: True,
}

_STRING_ESCAPES = {
    ord('"'): '\\"',
    ord('\\'): '\\\\',
}

class _SexpFormatter(fmt.Formatter):
    @staticmethod
    def format_unencoded(token_source, out):
        out.write('(')
        for token_type, value in token_source:
            known_type = token_type
            while True:
                needs_styling = _KNOWN_TOKENS.get(known_type)
                if needs_styling is not None:
                    break
                known_type = known_type.parent
            if needs_styling:
                out.write(f' ((span :class \"c-{tok.STANDARD_TYPES[known_type]}\")')
            out.write(f' "{value.translate(_STRING_ESCAPES)}"')
            if needs_styling:
                out.write(')')
        out.write(')\n')

_COMMAND_MAP = {}

def _define_command(name):
    def decorator(function):
        _COMMAND_MAP[name] = function
        return function
    return decorator

@_define_command(':quit')
def _quit_server():
    print(':done')
    sys.exit(0)

@_define_command(':highlight')
def _highlight_code():
    lexer_name = input()
    source_path = input()
    destination_path = input()
    lexer = lex.get_lexer_by_name(lexer_name)
    with open(source_path, 'r') as input_file:
        source_code = input_file.read()
    with open(destination_path, 'w') as output_file:
        p.highlight(source_code, lexer, _SexpFormatter(), outfile=output_file)
    print(':done')

def _main():
    while True:
        command = input()
        _COMMAND_MAP[command]()

if __name__ == '__main__':
    _main()
