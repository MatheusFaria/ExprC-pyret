# ExprC-pyret
Implementing a basic language interpreter in pyret

# Language Syntax and Features
ExprC = num
        | true
        | false
        | id
        | {if ExprC ExprC ExprC}
        | {with {id = ExprC} ... ExprC}
        | {fn {id ...} ExprC}
        | {operator ExprC ExprC}
        | {ExprC ExprC ...}

operator = +
        | -
        | *
        | /
        | eq?
        | <=

# Host Language
- pyret

# Contributors
Colin Adams (colincadams)
Matheus de Sousa faria (matheusfaria)
Ryan Quinlan (rynq)
