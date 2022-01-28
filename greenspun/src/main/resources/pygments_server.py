#!/usr/bin/env python3
# Copyright © 2019-2021  Fanael Linithien
# SPDX-License-Identifier: AGPL-3.0-or-later
# pylint: disable=missing-function-docstring
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
from typing import Callable, Dict, FrozenSet, NoReturn, Optional, TextIO
import sys
import traceback
import pygments.lexer as lex
import pygments.lexers as lxs
import pygments.token as tok

TokenType = tok._TokenType # pylint: disable=protected-access

# Set of token types that we want to style.
KNOWN_TOKENS: FrozenSet[TokenType] = frozenset((
    tok.Comment,
    tok.Keyword,
    tok.String,
    tok.String.Escape,
    tok.String.Interpol,
    tok.Number,
    tok.Generic.Deleted,
    tok.Generic.Inserted,
    tok.Generic.Heading,
    tok.Generic.Subheading,
))

def get_effective_class_name(token_type: TokenType) -> str:
    current_type: Optional[TokenType] = token_type
    while current_type is not None:
        if current_type in KNOWN_TOKENS:
            return 'c-' + tok.STANDARD_TYPES[current_type]
        current_type = current_type.parent
    return ''

TOKEN_TYPE_CLASSES: Dict[TokenType, str] = {
    t: get_effective_class_name(t) for t in tok.STANDARD_TYPES
}

def read_line() -> str:
    return sys.stdin.readline().rstrip('\n')

def send_done(out: TextIO) -> None:
    out.write(':done\n')

def print_multiline_string(string: str, out: TextIO) -> None:
    for line in string.splitlines():
        out.write(f'>{line}\n')
    send_done(out)

def send_token_stream(code: str, lexer: lex.Lexer, out: TextIO) -> None:
    current_class = ''
    for token_type, value in lexer.get_tokens(code):
        class_name = TOKEN_TYPE_CLASSES[token_type]
        if current_class != class_name:
            current_class = class_name
            out.write(f':sc\n{class_name}\n')

        if value == '\n':
            out.write(':nl\n')
        elif '\n' not in value:
            out.write(f':s\n{value}\n')
        else:
            out.write(':m\n')
            print_multiline_string(value, out)

def read_multiline_string() -> str:
    source_lines = []
    while True:
        line = read_line()
        if line.startswith('>'):
            source_lines.append(line[1:])
        elif line == ':done':
            break
        else:
            raise ValueError(f'Unexpected terminator of multiline string: "{line}"')
    source_lines.append('')
    return '\n'.join(source_lines)

def print_exception(exception: Exception) -> None:
    print(':error')
    trace = traceback.format_exception(type(exception), exception, exception.__traceback__)
    print_multiline_string(''.join(trace), sys.stdout)

Command = Callable[[], None]
COMMAND_MAP: Dict[str, Command] = {}

def define_command(name: str) -> Callable[[Command], Command]:
    def decorator(function: Command) -> Command:
        COMMAND_MAP[name] = function
        return function
    return decorator

@define_command(':quit')
def quit_server() -> NoReturn:
    send_done(sys.stdout)
    sys.stdout.flush()
    sys.exit(0)

@define_command(':highlight')
def highlight_code() -> None:
    lexer_name = read_line()
    source_code = read_multiline_string()
    lexer = lxs.get_lexer_by_name(lexer_name)
    send_token_stream(source_code, lexer, sys.stdout)
    send_done(sys.stdout)

def main() -> NoReturn:
    while True:
        command = read_line()
        try:
            COMMAND_MAP[command]()
        except Exception as exception: # pylint: disable=broad-except
            print_exception(exception)
        sys.stdout.flush()

if __name__ == '__main__':
    main()
