# Layout

## Preprocess

```
preParse(ts) = openNewImpLayout(ts, 1)

checkNewline([], n)   = [{close}]
checkNewline(t:ts, n) = checkNewline(ts, n)                           (if isWhiteSpace(t))
checkNewline(t:ts, n) = {newline(getCol(t))}:checkTokenType(t, ts, m) (if n < m, m = getRow(t))
checkNewline(t:ts, n) = checkTokenType(t, ts, n)                      (otherwise)

checkTokenType(t, ts, n) = t:openNewImpLayout(ts, n)       (if isOpenImpLayout(t))
checkTokenType(t, ts, n) = t:{openExp}:checkNewline(ts, n) (if isOpenExpLayout(t))
checkTokenType(t, ts, n) = t:{close}:checkNewline(ts, n)   (if isCloseLayout(t))
checkTokenType(t, ts, n) = t:checkNewline(ts)              (otherwise)

openNewImpLayout([], n)   = {openImp(0)}:checkNewline([])
openNewImpLayout(t:ts, n) = {openImp(getCol(t))}:checkNewline(t:ts)

isWhiteSpace(t)    = match(t, whitespace)
isOpenImpLayout(t) = match(t, lb_imp_open / lp_imp_open)
isOpenExpLayout(t) = match(t, lb_exp_open / lp_exp_open)
isCloseLayout(t)   = match(t, lb_close / lp_close)
```

## Layout Process

```
parseWithL(ts, p) = withL(ts, [], p)

withL([], [], p)                  = ParseOk
withL({openImp(m)}:ts, ms, p)     = withL(ts, {m}:ms, p)
withL({openExp}:ts, ms, p)        = withL(ts, {-}:ms, p)
withL({close}:ts, [], p)          = ParseError
withL({close}:ts, {m}:ms, p)      = withL(ts, ms, p)
withL({close}:ts, {-}:ms, p)      = withL(ts, ms, p)
withL({newline(c)}:ts, [], p)     = ParseError
withL({newline(c)}:ts, {m}:ms, p) = ParseError                   (if c < m)
withL({newline(c)}:ts, {m}:ms, p) = p(<;>, withL(ts, {m}:ms, p)) (if c = m)
withL({newline(c)}:ts, {m}:ms, p) = withL(ts, {m}:ms, p)         (if c > m)
withL({newline(c)}:ts, {-}:ms, p) = withL(ts, {-}:ms, p)
withL(t:ts, ms, p)                = p(t, withL(ts, ms, p))
```
