# Not Scope

* Parsing optimization
* Module
* Useful pattern and view
* GADTs
* Interface
* Suspension

# Contents

## Type Declaration

```
#typ Ty = tyExp
#typ Fn1(a1: tyExp1, a2: tyExp2) = tyExp
#typ Fn2(a1: tyExp1, a2: tyExp2)(a1: tyExp1, a2: tyExp2) = tyExp
```

## Data Type Declaration

```
#dat Fn1(a1: tyExp1, a2: tyExp2) = {
	#let var = exp

	C1(k3: tyExp3, k4: tyExp4)
	C2(k5: tyExp5)
	C3
}
#dat Fn2(a1: tyExp1, a2: tyExp2) = C(k3: tyExp3)
```

## Function Declaration

```
#fun func1(a1: tyExp1, a2: tyExp2) = exp
#fun func2(^a1: tyExp1, a2: tyExp2)[^a1: tyExp1, a2: tyExp2] = exp
```

## Local Declaration

```
#let func1: (a1: tyExp1, a2: tyExp2) -> tyExp3 = \a1, a2 #> exp
#let func2 = \{
	a1 = pat1, a2 #> exp1
	_ #> exp2
}
#let var = exp

#rec var = exp
```

## Match Expression

```
#mch exp #in {
	pat1 #> exp1
	pat2 #> exp2
}

#mch k1 = exp1, k2 = exp2 #in {
	k1 = pat1, k2 #if #let pat2 = exp3 #> exp4
	_ #> exp5
}
```

## Application Expression

```
exp1 exp2
```
