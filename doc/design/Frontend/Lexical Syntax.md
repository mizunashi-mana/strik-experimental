# Lexical Syntax
## Overview

```
lexical_program := (whitespace / lexeme)* EOS
lexeme := literal_part
        / literal
        / special_char
        / keyword_id
        / keyword_sym
        / free_id
        / var_id
        / var_sym
        / con_id
        / con_sym
```

## Identifier

```
var_id := id_small_char id_char* ! id_char
con_id := id_large_char id_char* ! id_char
var_sym := sym_normal_char sym_char* ! sym_char
con_sym := sym_sp_char sym_char* ! sym_char
free_id := kw_prefix_char string
```

## Reserved

```
special_char := "{"
              / "}"
              / "["
              / "]"
              / "("
              / ")"
              / ";"
              / "."
keyword_id := kw_prefix_char id_char* ! id_char
            / kw_prefix_char sym_char* ! sym_char
            / "#{"
            / "#["
            / "#("
            / "_" ! id_char
keyword_id_sym_char = sym_char
keyword_sym := keyword_sym_unit ! sym_char
keyword_sym_unit := "="
                   / "^"
                   / ":"
                   / '\'
```

## Literal Overview

```
literal_part := interp_string_part
literal := string
         / rational
         / integer
```

## Number Literal

```
rational := sign? decimal "." decimal
integer := sign? zero_char ("x" / "X") heximal
         / sign? (! zero_char) decimal
decimal := digit_char (digit_char / "_")* ! (digit_char / "_")
heximal := hexit_char (hexit_char / "_")* ! (hexit_char / "_")
sign := "+"
      / "-"
zero_char := "0"
hexit_char := digit_char
            / "A" / "B" / ... / "F"
            / "a" / "b" / ... / "f"
```

## String Literal

```
interp_string_part := interp_string_start
                    / interp_string_cont
                    / interp_string_end
string := str_sep_char str_graphic_char* str_sep_char

interp_string_start = str_sep_char str_graphic_char* interp_open
interp_string_cont := interp_close str_graphic_char* interp_open
interp_string_end := interp_close str_graphic_char* str_sep_char

interp_open := interp_open_char "{"
interp_close := kw_prefix_char "}"

str_sep_char := '"'
escape_open_char := '\'
interp_open_char := kw_prefix_char
str_graphic_char := uni_escape
                  / bstr_graphic 
bstr_graphic_char := byte_escape
                   / white_char
                   / ! (str_sep_char / escape_open_char / interp_open_char) graphic_char
byte_escape := escape_open_char (charesc / byteesc)
uni_escape := escape_open_char "u{" hexit+ "}"
charesc := "0" / "a" / "b" / "f" / "n" / "r" / "t" / "v" / "#"
         / escape_open_char / str_sep_char
byteesc := "x" hexit_char hexit_char
```

## Whitespace

```
whitespace := whitestuff+
whitestuff := white_char
            / comment
```

## Comment

```
comment := line_comment
         / multiline_comment

line_comment := "//" any_1l_char* (newline / EOS)
multiline_comment := comment_open anys comment_close

comment_open := "/*"
comment_close := "*/"

any_1l_char := graphic_char / space_char
anys := (! (comment_open / comment_close) any_char)*
any_char := graphic_char / white_char
```

## Base Unit

```
graphic_char := small_char
              / large_char
              / symbol_char
              / digit_char
              / other_char
              / special_char
              / other_special_char
              / other_graphic_char
id_char := small_char
         / large_char
         / digit_char
         / other_char
sym_char := symbol_char
          / other_char
white_char := "\v"
            / space_char
            / newline_char
space_char := "\t" / "\u{200E}" / "\u{200F}"
            / "\p{General_Category=Space_Separator}"
newline := "\r\n" / newline_char
newline_char := "\r" / "\n" / "\f"
         / "\p{General_Category=Line_Separator}"
         / "\p{General_Category=Paragraph_Separator}"

small_char := "\p{General_Category=Lowercase_Letter}"
            / "\p{General_category=Other_Letter}"
            / "_"
large_char := "\p{General_Category=Uppercase_Letter}"
            / "\p{General_Category=Titlecase_Letter}"
symbol_char := ! (special_char / other_special_char / "_") symbol_cat_char
symbol_cat_char := "\p{General_Category=Connector_Punctuation}"
                 / "\p{General_Category=Dash_Punctuation}"
                 / "\p{General_Category=Other_Punctuation}"
                 / "\p{General_Category=Symbol}"
digit_char := "\p{General_Category=Decimal_Number}"
other_char := ! white_char other_cat_char
other_cat_char := "\p{General_Category=Modifier_Letter}"
                / "\p{General_Category=Mark}"
                / "\p{General_Category=Letter_Number}"
                / "\p{General_Category=Other_Number}"
                / "\p{General_Category=Format}"
other_special_char := "#"
                    / '"'
                    / "'"
other_graphic_char := ! (symbol_cat_char / special_char / other_special_char) other_graphic_cat_char
other_graphic_cat_char := "\p{General_Category=Punctuation}"
```
