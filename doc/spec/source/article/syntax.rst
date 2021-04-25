Syntax
======

Notational Conventions
----------------------

.. glossary::

    ``pattern?``
        optional

    ``pattern*``
        zero or more repetitions

    ``pattern+``
        one or more repetitions

    ``( pattern )``
        grouping

    ``pattern | pattern``
        choice

    ``pattern<pattern>``
        difference

    ``"..."``
        terminal by unicode properties

    ``'...'``
        virtual layout terminal (See `Layout`_)

    ``EOS``
        end of source

Lexical Syntax
--------------

.. productionlist::
    lexical_program: (lexeme | whitespace)* EOS?
    lexeme  : literal
            : special
            : brace
            : reserved_id
            : reserved_sym
            : var_id
            : var_sym
            : con_id
            : con_sym

.. productionlist::
    var_id: (small (small | large | digit | other)*)<reserved_id>
    con_id: (large (small | large | digit | other)*)<reserved_id>
    var_sym: (symbol<":"> (symbol | other)*)<reserved_sym>
    con_sym: (":" (symbol | other)*)<reserved_sym>

.. productionlist::
    reserved_id : "#as"
                : "#case"
                : "#data"
                : "#derive"
                : "#do"
                : "#export"
                : "#family"
                : "#foreign"
                : "#impl"
                : "#infix"
                : "#letrec"
                : "#let"
                : "#mod"
                : "#newtype"
                : "#of"
                : "#pattern"
                : "#record"
                : "#role"
                : "#sig"
                : "#static"
                : "#trait"
                : "#type"
                : "#use"
                : "#when"
                : "#where"
                : "#yield"
                : "#Default"
                : "#Self"
    reserved_sym: "_"
                : "!"
                : ".." | "…"
                : "."
                : "->" | "→"
                : "<-" | "←"
                : "=>" | "⇒"
                : "<=" | "⇐"
                : "="
                : "?"
                : "@"
                : "\\/" | "∀"
                : "\\" | "λ"
                : "|"
                : "~"
                : ":"
    special : "("
            : ")"
            : ","
            : "["
            : "]"
            : "`" -- ` for syntax highlighting issue
            : ";"
            : "#@"
    brace   : "{{" | "}}" : "❴" | "❵"
            : "{" | "}"

.. productionlist::
    literal : rational
            : integer
            : bytestring
            : string
            : bytechar
            : char
            : interp_string_part

.. productionlist::
    rational: sign? decimal "." decimal exponent?
            : sign? decimal exponent
    integer : sign? zero ("b" | "B") bit (bit | "_")*
            : sign? zero ("o" | "O") octit (octit | "_")*
            : sign? zero ("x" | "X") hexit (hexit | "_")*
            : sign? decimal
    decimal: digit (digit | "_")*
    sign: "+"
        : "-"
    zero: "0"
    exponent: ("e" | "E") sign? decimal
    bit: "0" | "1"
    octit: "0" | "1" | ... | "7"
    hexit   : digit
            : "A" | "B" | ... | "F"
            : "a" | "b" | ... | "f"

.. productionlist::
    bytestring: "#r" str_sep bstr_graphic* str_sep
    string: str_sep (bstr_graphic | uni_escape)* str_sep
    bytechar: "#r" char_sep bchar_graphic char_sep
    char: char_sep (bchar_graphic | uni_escape) char_sep
    str_sep: "\""
    char_sep: "'"
    escape_open: "\\"
    bstr_graphic: graphic<str_sep | escape_open>
                : whitechar
                : byte_escape
                : gap
    bchar_graphic   : graphic<char_sep | escape_open>
                    : " "
                    : byte_escape
    byte_escape: escape_open (charesc | asciiesc | byteesc)
    uni_escape: escape_open "u{" hexit+ "}"
    gap: escape_open "|" whitechar* "|"
    charesc : "0" | "a" | "b" | "f" | "n" | "r" | "t" | "v"
            : "$" | escape_open | str_sep | char_sep
    asciiesc: "^" cntrlesc
            : "NUL" | "SOH" | "STX" | "ETX" | "EOT" | "ENQ"
            : "ACK" | "BEL" | "BS" | "HT" | "LF" | "VT"
            : "FF" | "CR" | "SO" | "SI" | "DLE" | "DC1"
            : "DC2" | "DC3" | "DC4" | "NAK" | "SYN" | "ETB"
            : "CAN" | "EM" | "SUB" | "ESC" | "FS" | "GS"
            : "RS" | "US" | "SP" | "DEL"
    cntrlesc: "A" | "B" | ... | "Z" | "@" | "[" | "\\" | "]"
            : "^" | "_"
    byteesc: "x" hexit hexit

.. productionlist::
    interp_string_part  : interp_string_without_interp
                        : interp_string_start
                        : interp_string_cont
                        : interp_string_end
    interp_str_open: "#s" str_sep
    interp_str_graphic  : bstr_graphic<"$" | str_sep | escape_open>
                        : uni_escape
    interp_open: "$" ( "{#" | "⦃" )
    interp_close: "#}" | "⦄"
    interp_string_without_interp: interp_str_open interp_str_graphic* str_sep
    interp_string_start: interp_str_open interp_str_graphic* interp_open
    interp_string_cont: interp_close interp_str_graphic* interp_open
    interp_string_end: interp_close interp_str_graphic* str_sep

.. productionlist::
    whitespace: whitestuff+
    whitestuff  : whitechar
                : comment

.. productionlist::
    comment : line_comment
            : doc_comment
            : pragma_comment
            : multiline_comment
    line_comment: "--" "-"* (any<symbol | other> any*)? (newline | EOS)
    multiline_comment: comment_open (ANY<"!" | "#"> ANYs (nested_comment ANYs)*)? comment_close
    doc_comment: comment_open "!" (ANY*)<ANY* newline "|" comment_close ANY*> newline "|" comment_close
    pragma_comment: comment_open "#" ANYs (nested_comment ANYs)* "#" comment_close
    nested_comment: comment_open ANYs (nested_comment ANYs)* comment_close
    comment_open: "{-"
    comment_close: "-}"
    any: graphic | space
    ANYs: (ANY*)<ANY* (comment_open | comment_close) ANY*>
    ANY: graphic | whitechar

.. productionlist::
    graphic : small
            : large
            : symbol
            : digit
            : other
            : special
            : other_special
            : other_graphic
    whitechar   : "\v"
                : space
                : newline
    space   : "\t" | "\u200E" | "\u200F"
            : "\p{General_Category=Space_Separator}"
    newline : "\r\n" | "\r" | "\n" | "\f"
            : "\p{General_Category=Line_Separator}"
            : "\p{General_Category=Paragraph_Separator}"
    small   : "\p{General_Category=Lowercase_Letter}"
            : "\p{General_Category=Other_Letter}"
            : "_"
    large   : "\p{General_Category=Uppercase_Letter}"
            : "\p{General_Category=Titlecase_Letter}"
    symbol  : symbolchar<special | other_special | "_" | "'">
    symbolchar  : "\p{General_Category=Connector_Punctuation}"
                : "\p{General_Category=Dash_Punctuation}"
                : "\p{General_Category=Other_Punctuation}"
                : "\p{General_Category=Symbol}"
    digit   : "\p{General_Category=Decimal_Number}"
    other   : "\p{General_Category=Modifier_Letter}"
            : "\p{General_Category=Mark}"
            : "\p{General_Category=Letter_Number}"
            : "\p{General_Category=Other_Number}"
            : "\p{General_Category=Format}"<whitechar>
            : "'"
    other_special: "#" | "\"" | "{" | "}" | "⦃" | "⦄" | "❴" | "❵"
    other_graphic: other_graphic_char<symbolchar | special | other_special>
    other_graphic_char: "\p{General_Category=Punctuation}"

Specifications for Lexical Nonterminals
:::::::::::::::::::::::::::::::::::::::

These nonterminals must be disjoint:

* ``whitespace``
* ``var_id``
* ``var_sym``
* ``con_id``
* ``con_sym``
* ``reserved_id``
* ``reserved_sym``
* ``special``
* ``brace``
* ``literal``

These nonterminals must be disjoint:

* ``whitechar``
* ``small``
* ``large``
* ``symbol``
* ``digit``
* ``other``
* ``special``
* ``other_special``
* ``other_graphic``

These nonterminals must be disjoint:

* ``space``
* ``newline``

These expressions must be empty:

* ``((lexeme | whitespace)*)<ANY*>``
* ``reserved_id<'#' (small | large) (small | large | digit | other)*>``
* ``reserved_sym<'_' | (symbol (symbol | other)*)>``
* ``brace<other_special*>``
* ``literal<("+" | "-" | digit | "'" | other_special) ANY*>``
* ``(multiline_comment | doc_comment | pragma_comment | nested_comment)<comment_open ANY* comment_close>``
* ``(multiline_comment | doc_comment | pragma_comment)<doc_comment | nested_comment>``
* ``("\p{General_Category=Letter}" | "\p{General_Category=Mark}" | "\p{General_Category=Number}" | "\p{General_Category=Punctuation}" | "\p{General_Category=Symbol}" | "\p{General_Category=Separator}" | "\p{General_Category=Format}")<graphic | whitechar>``

Aliases
-------

.. productionlist::
    "->"    : "->" | "→"
    ".."    : ".." | "…"
    "<-"    : "<-" | "←"
    "<="    : "<=" | "⇐"
    "=>"    : "=>" | "⇒"
    "\\/"   : "\\/" | "∀"
    "\\"    : "\\" | "λ"
    "{{"    : "{{" | "❴"
    "}}"    : "}}" | "❵"

Grammar
-------

.. productionlist::
    program: decl_body

.. productionlist::
    decl_body   : "{" decl_items "}"
                : "{{" decl_items "}}"
                : '{' decl_items '}'
    decl_items  : lsemis? (decl_item lsemis)* decl_item?
    decl_item   : sig_item
                : type_decl
                : data_decl
                : val_decl

.. productionlist::
    typesig_decl: "#type" declcon ":" type
    valsig_decl: declvar ":" type
    consig_decl: declcon ":" type

.. productionlist::
    type_decl: "#type" decltype "=" type ("where" type_decl_where_body)?
    type_decl_where_body: "{" type_decl_where_items "}"
                        : "{{" type_decl_where_items "}}"
                        : '{' type_decl_where_items '}'
    type_decl_where_items: lsemis? (type_decl_where_item lsemis)* type_decl_where_item?
    type_decl_where_item: typesig_decl
                        : type_decl

.. productionlist::
    data_decl   : "#data" declcon (":" type)? ("#where" data_decl_body)?
                : "#data" decltype "=" alg_data_type ("#where" type_decl_where_body)?
                : "#newtype" decltype "=" type ("#where" type_decl_where_body)?
    data_decl_body  : "{" data_decl_items "}"
                    : "{{" data_decl_items "}}"
                    : '{' data_decl_items '}'
    data_decl_items: lsemis? (data_decl_item lsemis)* data_decl_item?
    data_decl_item  : typesig_decl
                    : consig_decl
                    : type_decl
    alg_data_type   : "(" alg_data_type_items ")"
                    : alg_data_type_items
    alg_data_type_items : "|"* (impltype "|"+)* impltype "|"*

.. productionlist::
    val_decl: declvarexpr "=" expr ("#where" val_decl_where_body)?
    val_bind: pat "=" expr ("#where" val_decl_where_body)?
    val_decl_where_body : "{" val_decl_where_items "}"
                        : "{{" val_decl_where_items "}}"
                        : '{' val_decl_where_items '}'
    val_decl_where_items: lsemis? (val_decl_where_item lsemis)* val_decl_where_item?
    val_decl_where_item: let_bind_item

.. productionlist::
    decltype    : declcon bind_var*
                : bind_var declconop bind_var
    impltype    : con_qualified type_qualified*
                : type_qualified conop type_qualified
    declvarexpr : declvar bind_var*
                : bind_var declop bind_var

.. productionlist::
    type: "\\/" bind_var* "=>" type
        : type_expr
    type_expr   : type_unit "->" type
                : type_unit
    type_unit: type_infix
    type_infix: type_apps (type_op type_apps)*
    type_op : con_sym
            : var_sym_ext
            : "`" type_qualified_op "`"
    type_qualified_op   : sym_ext
                        : type_qualified
    type_apps: type_qualified type_app*
    type_app: "@" type_qualified
            : type_qualified
    type_qualified: type_atomic
    type_atomic : "(" type (":" type)? ")"
                : con
                : var
                : type_literal
    type_literal: literal
                : "(" type_tuple_items ")"
                : "[" type_array_items "]"
                : "{" type_simplrecord_items "}"
    type_tuple_items: (type ",")+ type ","?
    type_array_items: (type ",")* type?
    type_simplrecord_items: (type_simplrecord_item ",")* type_simplrecord_item?
    type_simplrecord_item: declvar ":" type

.. productionlist::
    sig_item: typesig_decl
            : valsig_decl
            : consig_decl

.. productionlist::
    expr: expr_infix ":" type
        : expr_infix
    expr_infix: expr_apps (expr_op expr_apps)*
    expr_op : con_sym
            : var_sym_ext
            : "`" expr_qualified_op "`"
    expr_qualified_op   : sym_ext
                        : expr_qualified
    expr_apps: expr_qualified expr_app*
    expr_app: expr_qualified
            : "@" type_qualified
    expr_qualified: expr_block
    expr_block  : "\\" "#case" case_alt_body
                : "\\" lambda_body
                : "#letrec" let_body
                : "#let" let_body
                : "#case" (expr ",")* expr? "#of" case_alt_body
                : "#do" do_body
                : "#@" layout_block_body
                : expr_atomic
    expr_atomic : "(" expr ")"
                : con
                : var
                : expr_literal
    expr_literal: literal
                : expr_interp_string
                : "(" expr_tuple_items ")"
                : "[" expr_array_items "]"
                : "{" expr_simplrecord_items "}"
    expr_interp_string  : interp_string_without_interp
                        : interp_string_start expr (interp_string_cont expr)* interp_string_end
    expr_tuple_items: (expr ",")+ expr ","?
    expr_array_items: (expr ",")* expr?
    expr_simplrecord_items: (expr_simplrecord_item ",")* expr_simplrecord_item?
    expr_simplrecord_item: declvar "=" expr

.. productionlist::
    pat : pat_unit ":" type
        : pat_unit
    pat_unit: pat_infix ("|" pat_infix)*
    pat_infix: pat_univ_apps (conop_qualified pat_univ_apps)*
    pat_univ_apps   : pat_univ_apps "@" type_qualified
                    : pat_apps
    pat_apps: con_qualified pat_app*
    pat_app : pat_qualified
            : "@" type_qualified
    pat_qualified: pat_atomic
    pat_atomic  : "(" pat ")"
                : var
                : pat_literal
    pat_literal : literal
                : "(" pat_tuple_items ")"
                : "[" pat_array_items "]"
                : "{" pat_simplrecord_items "}"
    pat_tuple_items: (pat ",")+ pat ","?
    pat_array_items: (pat ",")* pat?
    pat_simplrecord_items: (pat_simplrecord_item ",")* pat_simplrecord_item?
    pat_simplrecord_item: declvar "=" pat

.. productionlist::
    let_body: let_binds "in" expr
    let_binds   : "{" let_bind_items "}"
                : "{{" let_bind_items "}}"
                : '{' let_bind_items '}'
    let_bind_items: lsemis? (let_bind_item lsemis)* let_bind_item?
    let_bind_item   : sig_item
                    : type_decl
                    : data_decl
                    : val_bind

.. productionlist::
    case_alt_body   : "{" case_alt_items "}"
                    : "{{" case_alt_items "}}"
                    : '{' case_alt_items '}'
    case_alt_items: lsemis? (case_alt_item lsemis)* case_alt_item?
    case_alt_item: (pat ",")* pat? guarded_alt
    guarded_alt : "->" expr
                : "#when" guarded_alt_body
    guarded_alt_body: "{" guarded_alt_items "}"
                    : "{{" guarded_alt_items "}}"
                    : '{' guarded_alt_items '}'
    guarded_alt_items: lsemis? (guarded_alt_item lsemis)* guarded_alt_item?
    guarded_alt_item: guard_qual "->" expr
    guard_qual: expr

.. productionlist::
    lambda_body : pat_atomic* guarded_alt

.. productionlist::
    do_body : "{" do_stmt_items "}"
            : "{{" do_stmt_items "}}"
            : '{' do_stmt_items '}'
    do_stmt_items   : lsemis? (do_stmt_item lsemis)* do_yield_item lsemis?
    do_stmt_item    : pat "<-" expr
                    : pat "=" expr
                    : "#letrec" let_binds
    do_yield_item   : "#yield" expr

.. productionlist::
    layout_block_body   : "{" layout_block_item "}"
                        : "{{" layout_block_item "}}"
                        : '{' layout_block_item '}'
    layout_block_item   : lsemis? expr lsemis?

.. productionlist::
    bind_var: "@" simple_bind_var
            : simple_bind_var
    simple_bind_var : var_id_ext
                    : "(" var_id_ext ":" type ")"
    con_qualified : con
    conop_qualified : conop
    con : con_id_ext
        : "(" con_sym_ext ")"
    conop   : con_sym_ext
            : "`" con_sym_ext "`"
            : "`" con_id_ext "`"
    var : var_id_ext
        : "(" var_sym_ext ")"
    op  : var_sym_ext
        : "`" var_sym_ext "`"
        : "`" var_id_ext "`"
    sym_ext : con_sym_ext
            : var_sym_ext
    con_id_ext  : con_id
                : "(" ")"
    con_sym_ext : con_sym
                : "->"
    var_id_ext  : var_id
                : "_"
    var_sym_ext : var_sym

.. productionlist::
    declcon : con_id
            : "(" con_sym ")"
    declconop   : con_sym
                : "`" con_sym "`"
                : "`" con_id "`"
    declvar : var_id
            : "(" var_sym ")"
    declop  : var_sym
            : "`" var_sym "`"
            : "`" var_id "`"

.. productionlist::
    lsemis: (';' | ";")+

Layout
------

.. code-block:: haskell

    data TokenWithL
        = Token Bool Int String
        | ExpectBrace

    preParse ts = go ts 0 isLayoutKeyword where
        go ts pl isL = skipWhiteSpace ts pl \(b,c,t) ts l -> case t of
            "\\" ->
                Token b c t:go ts l isLayoutKeywordLam
            _ | isL t ->
                Token b c t:ExpectBrace:go ts l isLayoutKeyword
            _ ->
                Token b c t:go ts l isLayoutKeyword

    skipWhiteSpace ts pl cont = case ts of
        [] -> []
        ((l1,c1),(l2,c2),t):ts
            | isWhiteSpace t ->
                skipWhiteSpace ts pl cont
            | pl < l1 ->
                cont (True,c1,t) ts l2
            | otherwise ->
                cont (False,c1,t) ts l2

    isWhiteSpace t =
        t match whitespace

    isLayoutKeyword t = case t of
        "#let"      -> True
        "#letrec"   -> True
        "#of"       -> True
        "#when"     -> True
        "#where"    -> True
        "#@"        -> True
        _           -> False

    isLayoutKeywordLam t = case t of
        "#case"     -> True
        _           -> isLayoutKeyword t

.. code-block:: haskell

    parseWithoutL p ts = case ts of
        [] -> []
        Token _ _ t:ts -> parse p t \r -> case r of
            ParseOk p ->
                parseWithoutL p ts
            ParseError ->
                ParseError
        ExpectBrace:ts ->
            parseWithoutL p ts

.. code-block:: haskell

    data Layout
        = NoLayout
        | ExplicitBrace
        | ExplicitDBrace Int
        | VirtualBrace Int

    parseWithL p ts = withL p ts False []

    withL p ts expB ms = case ts of
        [] -> resolveEmptyBrace p expB \p ->
            tryEnd p ms
        Token isN c t:ts
            | isN ->
                resolveNewline p c t ts expB ms
            | otherwise ->
                resolveToken p t ts expB ms
        ExpectBrace:ts ->
            withL ts True ms

    runParserL p t ts ms cont = parse p t \r -> case r of
        ParseOk p ->
            cont p ts ms
        ParseError ->
            errorRecover p t ts ms cont

    runSimpleParserL p t cont = parse p t \r -> case r of
        ParseOk p ->
            cont p
        ParseError ->
            ParseError

    errorRecover p t ts ms cont = case ms of
        VirtualBrace _:ms -> parse p '}' \r -> case r of
            ParseOk p ->
                runParserL p t ts ms cont
            ParseError ->
                ParseError
        _ ->
            ParseError

    resolveToken p t ts expB ms = case t of
        "{" | expB ->
            runParserL p "{" ts ms \p ts ms ->
                withL p ts False (ExplicitBrace:ms)
        "{{" | expB ->
            runParserL p "{{" ts ms \p ts ms ->
                let m = calcLayoutPos ts
                in withL p ts False (ExplicitDBrace m:ms)
        _ | expB ->
            runParserL p '{' ts0 ms \p ts ms ->
                let m = calcLayoutPos ts
                in resolveToken p t ts False (VirtualBrace m:ms)
        _ | isOpen t ->
            runParserL p t ts ms \p ts ms ->
                withL p ts False (NoLayout:ms)
        _ | isClose t || t match interp_string_continue ->
            tryClose p t ts ms
        _ ->
            runParserL p t ts ms \p ts ms ->
                withL p ts False ms

    resolveNewline p c t ts expB ms0 = case ms of
        ExplicitDBrace m:ms1 ->
            | c < m -> case t of
                "}}" -> resolveEmptyBrace p expB \p ->
                    runSimpleParserL p "}}" \p ->
                        withL p ts False ms1
                _ ->
                    ParseError
            | c == m -> resolveEmptyBrace p expB \p ->
                runSimpleParserL p ';' \p ->
                    resolveToken p t ts False ms0
            | otherwise ->
                resolveToken p t ts expB ms0
        VirtualBrace m:ms1
            | c < m -> resolveEmptyBrace p expB \p ->
                runSimpleParserL p '}' \p ->
                    resolveNewline p c t ts False ms1
            | c == m -> resolveEmptyBrace p expB \p ->
                runSimpleParserL p ';' \p ->
                    resolveToken p t ts False ms0
            | otherwise ->
                resolveToken p t ts expB ms0
        _ ->
            resolveToken p t ts expB ms

    resolveEmptyBrace p expB cont = case expB of
        False ->
            cont p
        True ->
            runSimpleParserL p '{' \p ->
                runSimpleParserL p '}' \p ->
                    cont p

    tryClose p t ts ms = case ms of
        []   ->
            ParseError
        m:ms -> case m of
            VirtualBrace _ -> runSimpleParserL p '}' \p ->
                tryClose p t ts ms
            ExplicitBrace -> case t of
                "}" -> runSimpleParserL p "}" \p ->
                    withL p ts ms
                _ ->
                    ParseError
            ExplicitDBrace _ -> case t of
                "}}" -> runSimpleParserL p "}}" \p ->
                    withL p ts ms
                _ ->
                    ParseError
            NoLayout
                | t match interp_string_continue -> runSimpleParserL p t \p ->
                    withL p ts (NoLayout:ms)
                | otherwise -> runSimpleParserL p t \p ->
                    withL p ts ms

    tryEnd p ms = case ms of
        [] ->
            ParseOk p
        m:ms -> case m of
            VirtualBrace _ -> runSimpleParserL p '}' \p ->
                tryEnd p ms
            _ ->
                ParseError

    calcLayoutPos ts = case ts of
        []              -> 0
        Token m _:_     -> m
        ExpectBrace:ts  -> calcLayoutPos ts

    isOpen t = case t of
        "("     -> True
        "["     -> True
        "{"     -> True
        "{{"    -> True
        _ | t match interp_string_start
                -> True
        _       -> False

    isClose t = case t of
        ")"     -> True
        "]"     -> True
        "}"     -> True
        "}}"    -> True
        _ | t match interp_string_end
                -> True
        _       -> False

Fixity Resolution
-----------------

Reference
---------

* `Unicode Identifier and Pattern Syntax <https://unicode.org/reports/tr31/>`_
* `Unicode Character Database - 5.7.1 General Category Values <http://www.unicode.org/reports/tr44/#General_Category_Values>`_
