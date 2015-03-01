import string-dict as dict
import option as opt

### Data definitions ###

# ExprC = num
# | {operator ExprC ExprC}
data ExprC:
  | numC(n :: Number)
  | binopC(s :: String, l :: ExprC, r :: ExprC)
  | ifC(tes :: ExprC, tru :: ExprC, fals :: ExprC)
end

# return values
data Value:
  | numV(n :: Number)
  | boolV(b :: Boolean)
end




### Binary Operations ###

# Sums two values and returns the resulting one
fun sum-primitive(a :: Value, b :: Value) -> Value:
  if is-numV(a) and is-numV(b):
    numV(a.n + b.n)
  else:
    raise("sum-primitive: Wrong argument type. Expected: two numbers.")
  end
where:
  sum-primitive(numV(3), numV(6)) is numV(9)
  sum-primitive(boolV(true), numV(6)) raises "Wrong argument type"
end


# Store all binary operations supported
binop-table = [dict.string-dict: "+", sum-primitive]

check:
  binop-table.get("+") satisfies opt.is-some
  binop-table.get-value("+")(numV(2), numV(5)) is numV(7)
end


# Checks if a string is a binary operation symbol
fun is-binop(s :: String) -> Boolean:
  binop-table.keys().member(s)
where:
  is-binop("+") is true
  is-binop("NOT BINOP") is false
end

# Apply a binop
fun binop(s :: String, l :: Value, r :: Value) -> Value:
  if is-binop(s):
    binop-table.get-value(s)(l, r)
  else:
    raise("binop: Not a binop operation")
  end
where:
  binop("+", numV(1), numV(2)) is numV(3)
  binop("NOT BINOP", numV(3), numV(5)) raises "Not a binop operation"
end




### Interpreter ###

# Evaluates an Expr expression AST and produces an value
fun interp(exp :: ExprC) -> Value:
  cases(ExprC) exp:
    | numC(n) => numV(n)
    | binopC(s, l, r) => binop(s, interp(l), interp(r))
    | ifC(tes, b1, b2) => raise("NOT IMPLEMENTED")
  end
end


#interp tests
check:
  interp(numC(3)) is numV(3)
  interp(binopC("+", numC(3), numC(5))) is numV(8)
  interp(ifC(numC(1), numC(2), numC(3))) raises "NOT IMPLEMENTED"
end