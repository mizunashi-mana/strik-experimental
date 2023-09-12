# Grammar

## Local Declaration

```
local_decl := "#let" let_body
            / "#rec" let_body
local_type_decl := "#let" let_type_body
let_body := lb_open let_body_items lb_close
          / let_body_item
let_body_items := lsemis? let_body_item (lsemis let_body_item)* lsemis?
let_body_item := bind_prom_type
               / bind_expr
let_type_body := lb_open let_type_body_items lb_close
               / let_type_body_item
let_type_body_items := lsemis? let_type_body_item (lsemis let_type_body_item)* lsemis?
let_type_body_item := bind_prom_type
                    /  dbind_type
```

## Where Declaration

```
where_body := lb_open where_body_items lb_close
            / where_body_item
where_body_items := lsemis? where_body_item (lsemis where_body_item)* lsemis?
                  / lsemis?
where_body_item := bind_prom_type
                 / bind_expr
                 / local_decl

bind_expr := declvar (":" type)? "=" expr
bind_type := declvar (":" type)? "=" type
bind_prom_type := "^" declvar (":" type)? "=" type
```

## Expression

```
expr := expr_ann ("#where" where_body)*
expr_ann := expr_infix ":" type
          / expr_infix
expr_infix := expr_apps (expr_op expr_apps)*
expr_op := "#op" lp_open lsemis? expr lsemis? lp_close
         / sym
expr_apps := expr_block expr_block*
expr_block := '\' expr
            / "#match" expr_tuple_items "#in" expr
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

case_body := lb_open case_items lb_close
           / case_item
case_items := lsemis? case_item (lsemis case_item)* lsemis?
            / lsemis?
case_item := view "#>" expr


block := lb_open block_items lb_close
       / lb_open block_stmts lb_close
       / lb_open lsemis? lb_close
block_items := lsemis? block_item (lsemis block_item)* lsemis?
block_item := block_pats block_guard? "#>" expr
block_pats := lsemis? pat (lsemis pat)* lsemis?
            / lsemis?
block_guard := "#if" view
block_stmts := lsemis? block_stmt (lsemis block_stmt)* lsemis?
block_stmt := expr
            / local_decl

expr_interp_string := interp_string_start block_stmts (interp_string_cont block_stmts)* interp_string_end

expr_tuple := lp_open expr_tuple_items lp_close
expr_tuple_items := lsemis? expr_tuple_item (lsemis expr_tuple_item)* lsemis?
                  / lsemis?
expr_tuple_item := bind_prom_type
                 / bind_expr
                 / "^" type
                 / expr
                 / local_decl
```

## Type Expression

```
type := type_ann ("#where" where_body)*
type_ann := type_infix ":" type
          / type_infix
type_infix := type_apps (type_op type_apps)*
type_op := "#op" lp_open lsemis? type lsemis? lp_close
         / sym
type_apps := type_atomic type_atomic*
type_atomic := block_type
             / type_literal
             / con
             / var
type_literal := literal
              / type_tuple
              / type_tuple_sig

block_type := lb_open block_type_stmts lb_close
            / lb_open lsemis? lb_close
block_type_stmts := lsemis? block_type_stmt (lsemis block_type_stmt)* lsemis?
block_type_stmt := type
                 / local_type_decl

type_tuple := lp_open type_tuple_items lp_close
type_tuple_items := lsemis? type_tuple_item (lsemis type_tuple_item)* lsemis?
                  / lsemis?
type_tuple_item := bind_prom_type
                 / bind_type
                 / "^" type_infix
                 / type_infix
                 / local_type_decl

type_tuple_sig := lp_open type_tuple_sig_items lp_close
type_tuple_sig_items := lsemis? type_tuple_sig_item (lsemis type_tuple_sig_item)* lsemis?
                      / lsemis?
type_tuple_sig_item := "^" declvar ":" type
                     / declvar ":" type
                     / "^" type_infix
                     / type_infix
                     / local_type_decl
```

## Pattern

```
pat := pat_ann 
pat_ann := pat_infix ":" type
         / pat_infix
pat_infix := pat_apps (pat_op pat_apps)*
pat_op := "#op" lp_open lsemis? con lsemis? lp_close
        / con_sym
pat_apps := con pat_atomic*
pat_atomic := lb_open lsemis? pat lsemis? lb_close
            / pat_literal
            / con
            / var
pat_literal := literal
             / pat_tuple

pat_tuple := lp_open pat_tuple_items lp_close
pat_tuple_items := lsemis? pat_tuple_item (lsemis pat_tuple_item)* lsemis?
                 / lsemis?
pat_tuple_item := declvar "=" pat
                / pat
```

## View

```
view := lb_open view_and_items lb_close
      / "#let" let_pat_body
      / expr
view_and_items := lsemis? view (lsemis view)* lsemis?
                / lsemis?
let_pat_body := lb_open let_pat_body_items lb_close
               / let_pat_body_item
let_pat_body_items := lsemis? let_pat_body_item (lsemis let_pat_body_item)* lsemis?
let_pat_body_item := pat "=" expr
```

## Base Unit

```
declvar := "#id" lp_open lsemis? (var_sym / var_id) lsemis? lp_close
         / var_id
         / free_id

sym := con_sym
     / var_sym

con := "#id" lp_open lsemis? (con_sym / con_id) lsemis? lp_close
     / con_id
var := declvar
```

## Layout Unit

```
lb_open := "{"
       / "#{"
lb_close := "}"
lp_open := "("
         / "#("
lp_close := ")"
lsemis := (';' / ";")+
```
