#!/usr/bin/env python3
# Copyright © 2019-2021  Fanael Linithien
# SPDX-License-Identifier: AGPL-3.0-or-later
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
":done".

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
   Response: A stream of tokens.
   Each token consists of one or more strings. The first string is always a
   simple string indicating the type of the token and its format:
    - ":sc": sets the CSS class of following tokens. One simple string follows,
      indicating the name of the CSS class. Empty string indicates that the
      following tokens require no special styling. The implied initial value is
      the empty string.
    - ":nl": the token is a single literal line feed. No values follow.
    - ":s": simple text. One simple string follows, indicating the text of the
      token.
    - ":m": multiline text. One *multiline* string follows, indicating the text
      of the token.
   The simple strings ":done" and ":error" can also occur in place of a token
   type, with their usual semantics.
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
    tok.Generic.Deleted: True,
    tok.Generic.Inserted: True,
    tok.Generic.Heading: True,
    tok.Generic.Subheading: True,
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
    return 'c-' + tok.STANDARD_TYPES[known_type] if needs_styling else ''

_TOKEN_TYPE_CLASSES = {t: _get_effective_class_name(t) for t in tok.STANDARD_TYPES}

def _read_line():
    return sys.stdin.readline().rstrip('\n')

def _send_done(out):
    out.write(':done\n')

def _print_multiline_string(string, out):
    for line in string.splitlines():
        out.write(f'>{line}\n')
    _send_done(out)

class _TokenStreamFormatter(fmt.Formatter):
    @staticmethod
    def format_unencoded(token_source, out):
        current_class = ''
        for token_type, value in token_source:
            class_name = _TOKEN_TYPE_CLASSES[token_type]
            if current_class != class_name:
                current_class = class_name
                out.write(f':sc\n{class_name}\n')

            if value == '\n':
                out.write(':nl\n')
            elif '\n' not in value:
                out.write(f':s\n{value}\n')
            else:
                out.write(':m\n')
                _print_multiline_string(value, out)

def _read_multiline_string():
    source_lines = []
    while True:
        line = _read_line()
        if line.startswith('>'):
            source_lines.append(line[1:])
        elif line == ':done':
            break
        else:
            raise ValueError(f'Unexpected terminator of multiline string: "{line}"')
    source_lines.append('')
    return '\n'.join(source_lines)

def _print_exception(exception):
    print(':error')
    trace = traceback.format_exception(type(exception), exception, exception.__traceback__)
    _print_multiline_string(''.join(trace), sys.stdout)

_COMMAND_MAP = {}

def _define_command(name):
    def decorator(function):
        _COMMAND_MAP[name] = function
        return function
    return decorator

@_define_command(':quit')
def _quit_server():
    _send_done(sys.stdout)
    sys.stdout.flush()
    sys.exit(0)

@_define_command(':highlight')
def _highlight_code():
    lexer_name = _read_line()
    source_code = _read_multiline_string()
    lexer = lex.get_lexer_by_name(lexer_name)
    p.highlight(source_code, lexer, _TokenStreamFormatter(), outfile=sys.stdout)
    _send_done(sys.stdout)

def _main():
    while True:
        command = _read_line()
        try:
            _COMMAND_MAP[command]()
        except Exception as exception:
            _print_exception(exception)
        sys.stdout.flush()

if __name__ == '__main__':
    _main()
