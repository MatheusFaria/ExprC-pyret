import string-dict as dict
import option as opt

### Data definitions ###

# ExprC = num
# | {operator ExprC ExprC}
data ExprC:
  | numC(n :: Number)
  | idC(s :: String)
  | trueC
  | falseC
  | binopC(s :: String, l :: ExprC, r :: ExprC)
  | ifC(tes :: ExprC, tru :: ExprC, fals :: ExprC)
  | lamC(args :: List, body :: ExprC)
  | appC(funct :: ExprC, vals :: List)
end

# return values
data Value:
  | numV(n :: Number)
  | boolV(b :: Boolean)
  | closV(arg :: List, body :: ExprC, env :: List)
end

# environment bindings
data Binding:
  | bind(name :: String, val :: Value)
end


### Binary Operations ###

fun is-bool-op(op :: String) -> Boolean:
  (op == "<=") or (op == "eq?")
end

fun operator(s :: String) -> (Number, Number -> Number):
  if s == "+":
    lam(a,b): a + b end
  else if s == "-":
    lam(a,b): a - b end
  else if s == "*":
    lam(a,b): a * b end
  else if s == "/":
    lam(a,b): a / b end
  else:
    raise("Not a valid operator")
  end
end

fun bool-op(s :: String) -> (Number, Number -> Boolean):
  if s == "<=":
    lam(a, b): a <= b end
  else if s == "eq?":
    lam(a, b): a == b end
  else:
    raise("Not a valid boolean operation")
  end
where:
  bool-op("<=")(2, 3) is true
  bool-op("eq?")(2, 3) is false
  bool-op(".")(2, 4) raises "Not a valid boolean operation"
end

# Sums two values and returns the resulting one
fun binop-primitive(a :: Value, b :: Value, s :: String) -> Value:
  if is-numV(a) and is-numV(b):
    if is-bool-op(s):
      boolV(bool-op(s)(a.n, b.n))
    else if (s == "/") and (b.n == 0):
        raise("Division by zero")
    else:
      numV(operator(s)(a.n, b.n))
    end
  else if s == "eq?":
    if is-boolV(a) and is-boolV(b):
      boolV(a.b == b.b)
    else:
      boolV(false)
    end
  else:
    raise("Invalid binop primitive")
  end
where:
  binop-primitive(numV(1), numV(2), "+") is numV(3)
  binop-primitive(numV(5), numV(4), "-") is numV(1)
  binop-primitive(numV(1), numV(2), "*") is numV(2)
  binop-primitive(numV(4), numV(2), "/") is numV(2)
  binop-primitive(numV(4), numV(0), "/") raises "Division by zero"
  binop-primitive(numV(4), numV(2), "eq?") is boolV(false)
  binop-primitive(numV(2), numV(2), "eq?") is boolV(true)
  binop-primitive(boolV(true), boolV(false), "eq?") is boolV(false)
  binop-primitive(boolV(true), numV(3), "eq?") is boolV(false)
  binop-primitive(boolV(true), numV(3), "+") raises "Invalid binop primitive"
end

fun lookup(s :: String, env :: List) -> Value:
  cases(List) env:
    | empty => raise("Free identifier")
    | link(f, r) => 
      if f.name == s:
        f.val
      else:
        lookup(s, r)
      end
  end
where:
  lookup("x", [list: bind("y", numV(9)),
      bind("x", numV(8)),
      bind("z", numV(12))]) is numV(8)
  lookup("a", [list: bind("y", numV(9)),
      bind("x", numV(8)),
      bind("z", numV(12))]) raises "Free identifier"
end

### Interpreter ###

# Extends the environment with the application values
fun create-env(args :: List, vals :: List, env :: List) -> List:
  if args.length() == vals.length():
    cases(List) args:
      | empty => env
      | link(fa, ra) => create-env(ra, vals.rest, link(bind(fa, vals.first), env))
    end
  else:
    raise("Wrong arity")
  end
where:
  create-env([list: "a", "b"], [list: numV(9), numV(10)], empty)
    is [list: bind("b", numV(10)), bind("a", numV(9))]
  create-env([list: "a"], [list: numV(9), numV(10)], empty)
    raises "Wrong arity"
end

# Evaluates an Expr expression AST and produces an value
fun interp(exp :: ExprC, env :: List) -> Value:
  cases(ExprC) exp:
    | idC(s) => lookup(s, env)
    | numC(n) => numV(n)
    | trueC => boolV(true)
    | falseC => boolV(false)
    | binopC(s, l, r) => binop-primitive(interp(l, env), interp(r, env), s)
    | ifC(tes, b1, b2) => 
      interp_tes = interp(tes, env) 
      if is-boolV(interp_tes):
        if interp_tes.b:
          interp(b1, env)
        else:
          interp(b2, env)
        end
      else:
        raise("If test is not a boolean")
      end
    | lamC(a, b) => closV(a, b, env)
    | appC(f, v) =>
      f-value = interp(f, env)
      cases(Value) f-value:
        | closV(arg, body, clos-env) => 
          interp(body, create-env(arg, map(lam(a): interp(a, env) end, v), clos-env))
        | else => raise("Can only apply functions")
      end
  end
end


#interp tests
check:
  interp(numC(3), empty) is numV(3)
  interp(binopC("+", numC(3), numC(5)), empty) is numV(8)
  interp(ifC(trueC, numC(2), numC(3)), empty) is numV(2)
  interp(appC(lamC([list: "a", "b"], binopC("+", idC("a"), idC("b"))), 
      [list: numC(2), numC(3)]), empty) is numV(5)
  interp(
    appC(
      appC(
        lamC([list: "f"], lamC([list: "x"], appC(idC("f"), [list: numC(10)]))),
        [list: lamC([list: "y"], binopC("+", idC("x"), idC("y")))]),
      [list: numC(5)]), 
    empty)
    raises "Free identifier"
end
