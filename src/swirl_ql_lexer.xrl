Definitions.

D   = [0-9]
L   = [A-Za-z_][A-Za-z0-9_.]*
WS  = ([\000-\s]|%.*)
C   = (<|<=|=|>=|>|<>)
P   = [(),]

Rules.

{C}         : {token, {comp, TokenLine, atom(TokenChars)}}.
{D}+\.{D}+? : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{D}+        : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{P}         : {token, {atom(TokenChars), TokenLine}}.
'[^']*'     : {token, {string, TokenLine, quoted_bitstring(TokenChars, TokenLen)}}.
"[^"]*"     : {token, {string, TokenLine, quoted_bitstring(TokenChars, TokenLen)}}.
{L}+        : word(TokenLine, TokenChars).
{WS}+       : skip_token.

Erlang code.

atom(TokenChars) ->
    list_to_atom(string:to_lower(TokenChars)).

quoted_bitstring(TokenChars, TokenLen) ->
    list_to_bitstring(lists:sublist(TokenChars, 2, TokenLen - 2)).

reserved_word('and') -> true;
reserved_word('in') -> true;
reserved_word('is') -> true;
reserved_word('not') -> true;
reserved_word('null') -> true;
reserved_word('or') -> true;
reserved_word(_) -> false.

word(TokenLine, TokenChars) ->
    Word = atom(TokenChars),
    case reserved_word(Word) of
        true  -> {token, {Word, TokenLine}};
        false -> {token, {var, TokenLine, Word}}
    end.
