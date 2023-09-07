# Not Scope
* Accessibility
	* Unicode syntax
	* Internationalization
	* Consider usage and keyboard layout
* Useful number literal
	* exponent representation
	* bit and octet representation
	* character representation
* Useful string literal
	* full control escape support
	* bytes-string interpolation
	* raw string literal with custom separation
	* raw bytes-string literal with custom separation
* Support documentation comment
* Useful comments
	* nested comment
* Support attribute annotation
# Contents
## Lexical Syntax
Overview:
```
lexical_program := (whitespace / lexeme)* EOS
lexeme := literal_part
        / literal
		/ brace
		/ special
		/ reserved_id
		/ reserved_sym
		/ free_id
		/ var_id
		/ var_sym
		/ con_id
		/ con_sym
```

Identifier:
```
var_id := id_small_char id_char*
con_id := id_large_char id_char*
var_sym := sym_normal_char sym_char*
con_sym := sym_sp_char sym_char*
free_id := kw_prefix_char "id" string
```

Reserved:
```
brace := "{"
       / "}"
       / "["
       / "]"
       / "("
       / ")"
special := ","
         / ";"
         / "."
reserved_id := kw_prefix_char id_char* ! id_char
             / kw_prefix_char sym_char* ! sym_char
             / "_" ! id_char
reserved_sym := reserved_sym_unit ! sym_char
reserved_sym_unit := "="
                   / "|"
                   / "^"
                   / ":"
```

Literal overview:
```
literal_part := interp_string_part
literal := rational
         / integer
         / bytechar
         / char
```

Number literal:
```
rational := sign? decimal "." decimal
integer := sign? zero_char ("x" / "X") hexit_char (hexit_char / "_")*
         / sign? (! zero_char) decimal
decimal := digit_char (digit_char / "_")*
sign := "+"
      / "-"
zero_char := "0"
hexit_char := digit_char
       / "A" / "B" / ... / "F"
       / "a" / "b" / ... / "f"
```

String interpolation:
```
interp_string_part := string
                    / interp_string_start
                    / interp_string_cont
                    / interp_string_end

string := str_sep_char str_graphic_char* str_sep_char
interp_string_start = str_sep_char str_graphic_char* interp_open
interp_string_cont := interp_close str_graphic_char* interp_open
interp_string_end := interp_close str_graphic_char* str_sep_char

interp_open := interp_open_char "{" kw_prefix
interp_close := kw_prefix "}"

str_sep_char := '"'
escape_open_char := '\'
interp_open_char := "$"
str_graphic_char := uni_escape
             / bstr_graphic 
bstr_graphic_char := byte_escape
              / whitechar
              / ! (str_sep_char / escape_open_char / interp_open_char) graphic_char
byte_escape := escape_open_char (charesc / byteesc)
uni_escape := escape_open_char "u{" hexit+ "}"
charesc := "0" / "a" / "b" / "f" / "n" / "r" / "t" / "v" / "$"
         / escape_open_char / str_sep_char
byteesc := "x" hexit_char hexit_char
```

Whitespace:
```
whitespace := whitestuff+
whitestuff := white_char
            / comment
```

Comment:
```
comment := line_comment
         / doc_comment
         / pragma_comment
         / multiline_comment

line_comment := "//" any_1l_char* (newline / EOS)
multiline_comment := comment_open anys comment_close

comment_open := "/*"
comment_close := "*/"

any_1l_char := graphic_char / space_char
anys := ((! (comment_open / comment_close)) any_char)*
any_char := graphic_char / white_char
```

Unit:
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
            / newline
space_char := "\t" / "\u{200E}" / "\u{200F}"
       / "\p{General_Category=Space_Separator}"
newline := "\r\n" / "\r" / "\n" / "\f"
         / "\p{General_Category=Line_Separator}"
         / "\p{General_Category=Paragraph_Separator}"

small_char
```
## Grammar