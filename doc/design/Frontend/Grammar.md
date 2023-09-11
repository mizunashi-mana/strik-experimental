# Grammar

## Local Declaration

```
local_decl := "#let" let_body
            / "#rec" let_body
local_type_decl := "#let" let_type_body
let_body := "{" let_body_items "}"
          / let_body_item
let_body_items := lsemis? let_body_item (lsemis let_body_item)* lsemis?
let_body_item := declvar "=" expr
let_type_body := "{" let_type_body_items "}"
               / let_type_body_item
let_type_body_items := lsemis? let_type_body_item (lsemis let_type_body_item)* lsemis?
let_type_body_item := declvar "=" type
```

## Where Declaration

```
where_body := "{" where_body_items "}"
            / where_body_item
where_body_items := lsemis? where_body_item (lsemis where_body_item)* lsemis?
                  / lsemis?
where_body_item := declvar (":" type)? "=" expr
                 / local_decl
```

## Expression

```
expr := expr_ann ("#where" where_body)*
expr_ann := expr_infix ":" type
          / expr_infix
expr_infix := expr_apps (expr_op expr_apps)*
expr_op := "#op" "(" expr ")"
         / sym
expr_apps := expr_block expr_block*
expr_block := '\' expr
            / "#mch" expr_tuple_items "#in" expr
            / "#case" case_body
            / "#if" case_body
            / expr_atomic
expr_atomic := block
             / expr_literal
             / con
             / var
expr_literal := literal
              / expr_interp_string
              / expr_tuple

case_body := "{" case_items "}"
           / case_item
case_items := lsemis? case_item (lsemis case_item)* lsemis?
            / lsemis?
case_item := view "#>" expr


block := "{" block_items "}"
       / "{" block_stmts "}"
       / "{" "}"
block_items := lsemis? block_item (lsemis block_item)* lsemis?
block_item := block_pats block_guard? "#>" expr
block_pats := lsemis? pat (lsemis pat)* lsemis?
            / lsemis?
block_guard := "#if" view
block_stmts := lsemis? block_stmt (lsemis block_stmt)* lsemis?
block_stmt := expr
            / local_decl

expr_interp_string := interp_string_start expr (interp_string_cont expr)* interp_string_end

expr_tuple := "(" expr_tuple_items ")"
expr_tuple_items := lsemis? expr_tuple_item (lsemis expr_tuple_item)* lsemis?
                  / lsemis?
expr_tuple_item := declvar (":" type)? "=" expr
                 / expr
                 / local_decl
```

## Type Expression

```
type := type_ann ("#where" where_body)*
type_ann := type_infix ":" type
          / type_infix
type_infix := type_apps (type_op type_apps)*
type_op := "#op" "(" type ")"
         / sym
type_apps := type_atomic type_atomic*
type_atomic := block_type
             / type_literal
             / con
             / var
type_literal := literal
              / type_tuple

block_type := "{" block_type_stmts "}"
            / "{" "}"
block_type_stmts := lsemis? block_type_stmt (lsemis block_type_stmt)* lsemis?
block_type_stmt := type
                 / local_type_decl

type_tuple := "(" type_tuple_items ")"
type_tuple_items := lsemis? type_tuple_item (lsemis type_tuple_item)* lsemis?
                  / lsemis?
type_tuple_item := declvar "=" type
                 / type_infix
                 / local_type_decl

type_tuple_sig := "(" type_tuple_sig_items ")"
type_tuple_sig_items := lsemis? type_tuple_sig_item (lsemis type_tuple_sig_item)* lsemis?
                      / lsemis?
type_tuple_sig_item := declvar ":" type
                     / type_infix
                     / local_type_decl
```

## Pattern

```
pat := pat_ann 
pat_ann := pat_infix ":" type
         / pat_infix
pat_infix := pat_apps (pat_op pat_apps)*
pat_op := "#op" "(" con ")"
        / con_sym
pat_apps := con pat_atomic*
pat_atomic := "{" pat "}"
            / pat_literal
            / con
            / var
pat_literal := literal
             / pat_tuple

pat_tuple := "(" pat_tuple_items ")"
pat_tuple_items := lsemis? pat_tuple_item (lsemis pat_tuple_item)* lsemis?
                 / lsemis?
pat_tuple_item := declvar "=" pat
                / pat
```

## View

```
view := "{" view_and_items "}"
      / "#let" let_pat_body
      / expr
view_and_items := lsemis? view (lsemis view)* lsemis?
                / lsemis?
let_pat_body := "{" let_pat_body_items "}"
               / let_pat_body_item
let_pat_body_items := lsemis? let_pat_body_item (lsemis let_pat_body_item)* lsemis?
let_pat_body_item := pat "=" expr
```

## Base Unit

```
declvar := "#id" "(" (var_sym / var_id) ")"
         / var_id
         / free_id

sym := con_sym
     / var_sym

con := "#id" "(" (con_sym / con_id) ")"
     / con_id
var := declvar
```

## Layout Unit

```
lsemis := (';' / ";")+
```
