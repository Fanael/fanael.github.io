#!/usr/bin/env python3
# Copyright © 2019-2024  Fanael Linithien
# SPDX-License-Identifier: AGPL-3.0-or-later
# pylint: disable=missing-function-docstring, missing-class-docstring
'''
A simple pygments server, communicating over pipes, to allow the reuse of a
process for highlighting multiple code blocks instead of spawning a new
pygmentize process for each one.

The protocol consists of three types:
 - A command: a single line of text that begins with ":", followed by a newline.
 - A simple string: a single line of text that begins with ">", followed by a
   newline.
 - A multiline string: multiple lines of text, each beginning with ">",
   terminated with the command ":done". The effective value is the concatenation
   of all lines, *separated* with newlines, i.e. there's no implied newline at
   the end.

The term "newline" refers to the character U+000A LINE FEED.

When the server is done processing a request, it sends the command ":done".

On recoverable errors, the server sends the command ":error" followed by a
multiline string describing the error.

List of known commands:
 - ":quit": initiate orderly shutdown of the server.
   No arguments.
   No response.
 - ":highlight": highlight the syntax of some code.
   Two arguments:
    - pygments lexer name (simple string)
    - source code to highlight (multiline string)
   Response: A stream of tokens.
   Each token consists of one or more strings. The first string is always a
   simple string indicating the type of the token and its format:
    - ":sc": sets the CSS class of following tokens. One simple string follows,
      indicating the name of the CSS class. The empty string indicates that the
      following tokens require no special styling. The implied initial value is
      the empty string.
    - ":nl": the token is a single literal line feed. No values follow.
    - ":s": simple text. One simple string follows, indicating the text of the
      token.
    - ":m": multiline text. One multiline string follows, indicating the text of
      the token.
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

def read_simple_string(source: TextSource) -> str:
    result = read_line(source)
    if result.startswith('>'):
        return result[1:]
    else:
        raise ValueError(f'Expected a simple string, but got "{result}"')

def send_done(sink: TextSink) -> None:
    sink.write(':done\n')

def print_multiline_string(string: str, sink: TextSink) -> None:
    for line in string.split('\n'):
        sink.write(f'>{line}\n')
    send_done(sink)

def send_token_stream(code: str, lexer: lex.Lexer, sink: TextSink) -> None:
    current_class = ''
    for token_type, value in lexer.get_tokens(code):
        class_name = TOKEN_TYPE_CLASSES[token_type]
        if current_class != class_name:
            current_class = class_name
            sink.write(f':sc\n>{class_name}\n')

        if value == '\n':
            sink.write(':nl\n')
        elif '\n' not in value:
            sink.write(f':s\n>{value}\n')
        else:
            sink.write(':m\n')
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
    return '\n'.join(source_lines)

def print_exception(exception: Exception, sink: TextSink) -> None:
    sink.write(':error\n')
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
    lexer_name = read_simple_string(source)
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
