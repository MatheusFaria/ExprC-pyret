import string-dict as dict
import option as opt

### Data definitions ###

# ExprC = num
# | {operator ExprC ExprC}
data ExprC:
  | numC(n :: Number)
  | trueC
  | falseC
  | binopC(s :: String, l :: ExprC, r :: ExprC)
  | ifC(tes :: ExprC, tru :: ExprC, fals :: ExprC)
  | lamC(args :: List, body :: ExprC)
end

# return values
data Value:
  | numV(n :: Number)
  | boolV(b :: Boolean)
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


### Interpreter ###

# Evaluates an Expr expression AST and produces an value
fun interp(exp :: ExprC) -> Value:
  cases(ExprC) exp:
    | numC(n) => numV(n)
    | trueC => boolV(true)
    | falseC => boolV(false)
    | binopC(s, l, r) => binop-primitive(interp(l), interp(r), s)
    | ifC(tes, b1, b2) => 
      interp_tes = interp(tes) 
      if is-boolV(interp_tes):
        if interp_tes.b:
          interp(b1)
        else:
          interp(b2)
        end
      else:
        raise("If test is not a boolean")
      end
  end
end


#interp tests
check:
  interp(numC(3)) is numV(3)
  interp(binopC("+", numC(3), numC(5))) is numV(8)
  interp(ifC(trueC, numC(2), numC(3))) is numV(2)
end
