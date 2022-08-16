#!/usr/bin/env python3
# Copyright © 2019-2022  Fanael Linithien
# SPDX-License-Identifier: AGPL-3.0-or-later
# pylint: disable=missing-function-docstring, missing-class-docstring
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
    - ":m": multiline text. One multiline string follows, indicating the text
      of the token.
    - ":mn": multiline text, *without* a new line at the end. One multiline
      string follows, indicating the text of the token.
   The simple strings ":done" and ":error" can also occur in place of a token
   type, with their usual semantics.
'''
from __future__ import annotations
from typing import Callable, NoReturn, Optional, Protocol, TypeAlias
import sys
import traceback
import pygments.lexer as lex
import pygments.lexers as lxs
import pygments.token as tok

class TextSource(Protocol): # pylint: disable=too-few-public-methods
    def readline(self) -> str: ...

class TextSink(Protocol):
    def write(self, string: str) -> int: ...
    def flush(self) -> None: ...

TokenType: TypeAlias = tok._TokenType # pylint: disable=protected-access

# Set of token types that we want to style.
KNOWN_TOKENS: frozenset[TokenType] = frozenset((
    tok.Comment,
    tok.Generic.Deleted,
    tok.Generic.Heading,
    tok.Generic.Inserted,
    tok.Generic.Subheading,
    tok.Keyword,
    tok.Name.Builtin,
    tok.Name.Class,
    tok.Name.Function,
    tok.Number,
    tok.String,
    tok.String.Escape,
    tok.String.Interpol,
))

def get_effective_class_name(token_type: TokenType) -> str:
    current_type: Optional[TokenType] = token_type
    while current_type is not None:
        if current_type in KNOWN_TOKENS:
            return 'c-' + tok.STANDARD_TYPES[current_type]
        current_type = current_type.parent
    return ''

TOKEN_TYPE_CLASSES: dict[TokenType, str] = {
    t: get_effective_class_name(t) for t in tok.STANDARD_TYPES
}

def read_line(source: TextSource) -> str:
    return source.readline().rstrip('\n')

def send_done(sink: TextSink) -> None:
    sink.write(':done\n')

def print_multiline_string(string: str, sink: TextSink) -> None:
    for line in string.splitlines():
        sink.write(f'>{line}\n')
    send_done(sink)

def send_token_stream(code: str, lexer: lex.Lexer, sink: TextSink) -> None:
    current_class = ''
    for token_type, value in lexer.get_tokens(code):
        class_name = TOKEN_TYPE_CLASSES[token_type]
        if current_class != class_name:
            current_class = class_name
            sink.write(f':sc\n{class_name}\n')

        if value == '\n':
            sink.write(':nl\n')
        elif '\n' not in value:
            sink.write(f':s\n{value}\n')
        else:
            sink.write(':m\n' if value[-1] == '\n' else ':mn\n')
            print_multiline_string(value, sink)

def read_multiline_string(source: TextSource) -> str:
    source_lines = []
    while True:
        line = read_line(source)
        if line.startswith('>'):
            source_lines.append(line[1:])
        elif line == ':done':
            break
        else:
            raise ValueError(f'Unexpected terminator of multiline string: "{line}"')
    source_lines.append('')
    return '\n'.join(source_lines)

def print_exception(exception: Exception, sink: TextSink) -> None:
    print(':error')
    trace = traceback.format_exception(type(exception), exception, exception.__traceback__)
    print_multiline_string(''.join(trace), sink)

Command: TypeAlias = Callable[[TextSource, TextSink], None]
COMMAND_MAP: dict[str, Command] = {}

def define_command(name: str) -> Callable[[Command], Command]:
    def decorator(function: Command) -> Command:
        COMMAND_MAP[name] = function
        return function
    return decorator

@define_command(':quit')
def quit_server(_source: TextSource, sink: TextSink) -> NoReturn:
    send_done(sink)
    sink.flush()
    sys.exit(0)

@define_command(':highlight')
def highlight_code(source: TextSource, sink: TextSink) -> None:
    lexer_name = read_line(source)
    source_code = read_multiline_string(source)
    lexer = lxs.get_lexer_by_name(lexer_name, stripnl=False, ensurenl=False)
    send_token_stream(source_code, lexer, sink)
    send_done(sink)

def main(source: TextSource, sink: TextSink) -> NoReturn:
    while True:
        command = read_line(source)
        try:
            COMMAND_MAP[command](source, sink)
        except Exception as exception: # pylint: disable=broad-except
            print_exception(exception, sink)
        sink.flush()

if __name__ == '__main__':
    main(sys.stdin, sys.stdout)
