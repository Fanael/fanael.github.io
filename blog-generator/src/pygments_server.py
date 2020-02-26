#!/usr/bin/env python3
# Copyright © 2019-2020  Fanael Linithien
# SPDX-License-Identifier: GPL-3.0-or-later
'''
A simple pygments server, communicating over pipes, to reuse the same process
for multiple highlightings instead of spawning a new pygmentize process for
every one.

The protocol is very simple: there are two types, a simple string and a
multiline string:
 - A simple string is a single line of text followed by a newline.
 - A multiline string consists of multiple lines that begin with ">", terminated
   with a simple string ":done".
Command names, arguments and responses are simple strings unless noted
otherwise.

When the server is done processing a command, it sends the simple string
":done". If the response is a multiline string, the multiline string terminating
":done" and the command-finishing one are collapsed into one.

On recoverable errors, the server sends ":error" followed by a multiline string
describing the error.

List of known commands:
 - ":quit": initiate orderly shutdown of the server.
   No arguments.
   No response.
 - ":highlight": highlight the syntax of some code.
   Two arguments:
    - pygments lexer name
    - source code to highlight (multiline string)
   Reponse: A multiline string representing a lisp list of HTSL forms.
'''
import sys
import traceback
import pygments as p
import pygments.formatter as fmt
import pygments.lexers as lex
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

def _get_effective_class_name(token_type):
    known_type = token_type
    while True:
        needs_styling = _KNOWN_TOKENS.get(known_type)
        if needs_styling is not None:
            break
        known_type = known_type.parent
    return 'c-' + tok.STANDARD_TYPES[known_type] if needs_styling else None

_TOKEN_TYPE_CLASSES = {t: _get_effective_class_name(t) for t in tok.STANDARD_TYPES}

class _SexpFormatter(fmt.Formatter):
    @staticmethod
    def format_unencoded(token_source, out):
        for token_type, value in token_source:
            class_name = _TOKEN_TYPE_CLASSES[token_type]
            if class_name:
                out.write(f' ((span :class "{class_name}")')
            out.write(f' "{value.translate(_STRING_ESCAPES)}"')
            if class_name:
                out.write(')')

def _read_multiline_string():
    source_lines = []
    while True:
        line = input()
        if line.startswith('>'):
            source_lines.append(line[1:])
        elif line == ':done':
            break
        else:
            raise ValueError(f'Unexpected terminator of multiline string: "{line}"')
    source_lines.append('')
    return '\n'.join(source_lines)

def _finish_command():
    print(':done', flush=True)

def _print_multiline_string(string):
    for line in string.splitlines():
        print(f'>{line}')
    _finish_command()

def _print_exception(exception):
    print(':error')
    trace = traceback.format_exception(type(exception), exception, exception.__traceback__)
    _print_multiline_string(''.join(trace))

_COMMAND_MAP = {}

def _define_command(name):
    def decorator(function):
        _COMMAND_MAP[name] = function
        return function
    return decorator

@_define_command(':quit')
def _quit_server():
    _finish_command()
    sys.exit(0)

@_define_command(':highlight')
def _highlight_code():
    lexer_name = input()
    source_code = _read_multiline_string()
    lexer = lex.get_lexer_by_name(lexer_name)
    _print_multiline_string(p.highlight(source_code, lexer, _SexpFormatter()))

def _main():
    while True:
        command = input()
        try:
            _COMMAND_MAP[command]()
        except Exception as exception:
            _print_exception(exception)

if __name__ == '__main__':
    _main()
