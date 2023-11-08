# Layout

## Preprocess

```
preParse(ts) = openNewLayout(ts, 1)

checkNewline([], n)   = [{close}]
checkNewline(t:ts, n) = checkNewline(ts, n)                          (if isWhiteSpace(t))
checkNewline(t:ts, n) = {newline(getCol(t))}:checkTokenType(t, ts, m) (if n < m, m = getRow(t))
checkNewline(t:ts, n) = checkTokenType(t, ts, n)                      (otherwise)

checkTokenType(t, ts, n) = t:openNewImpLayout(ts, n)    (if isImpLayoutOpen(t))
checkTokenType(t, ts, n) = t:openNewExpLayout(ts, n)    (if isExpLayoutOpen(t))
checkTokenType(t, ts, n) = t:{close}:checkNewline(ts)   (if isLayoutClose(t))
checkTokenType(t, ts, n) = t:checkNewline(ts)           (otherwise)

openNewImpLayout([], n)   = {impOpen(0)}:checkNewline([])
openNewImpLayout(t:ts, n) = {impOpen(getCol(t))}:checkNewline(t:ts)

openNewExpLayout([], n)   = {expOpen}:checkNewline([])
openNewExpLayout(t:ts, n) = {expOpen}:checkNewline(t:ts)

isWhiteSpace(t)    = match(t, whitespace)
isImpLayoutOpen(t) = match(t, lb_imp_open / lp_imp_open)
isExpLayoutOpen(t) = match(t, lb_exp_open / lp_exp_open)
isLayoutClose(t)   = match(t, lb_close / lp_close)
```

## Layout Process

```
parseWithL(ts, p) = withL(ts, [], p)

withL([], [], p)                  = ParseOk
withL({impOpen(m)}:ts, ms, p)     = withL(ts, {m}:ms, p)
withL({expOpen}:ts, ms, p)        = withL(ts, {-}:ms, p)
withL({close}:ts, {m}:ms, p)      = withL(ts, ms, p)
withL({close}:ts, {-}:ms, p)      = withL(ts, ms, p)
withL({newline(c)}:ts, {m}:ms, p) = ParseError                   (if c < m)
withL({newline(c)}:ts, {m}:ms, p) = p(<;>, withL(ts, {m}:ms, p)) (if c = m)
withL({newline(c)}:ts, {m}:ms, p) = withL(ts, {m}:ms, p)         (if c > m)
withL({newline(c)}:ts, {-}:ms, p) = withL(ts, {-}:ms, p)
withL(t:ts, ms, p)                = p(t, withL(ts, ms, p))
```
