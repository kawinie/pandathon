{--
    Imperative 
    Basic data types and operations. You should provide at least boolean values and integers; you may want to include floating point numbers as well.

    Conditionals.
    
    Recursion/loops.

    Names (imperative/functional languages only).
    
    Procedures/functions with arguments (or some other abstraction mechanism). You should provide a way to factor out repeated code and give it a name so that it can be reused. For imperative/functional languages, you must decide what kind of parameter passing scheme to use, which we’ll discuss in class. (Passing arguments is trivial for stack-based languages since arguments are passed on the stack!).
--}

type Var = String

data Expr = ValInt Int
          | ValStr String     
          | Op Expr Expr             
    deriving (Show, Eq)

data Op  = Add Expr Expr
         | Sub Expr Expr
         | Mul Expr Expr
         | Div Expr Expr
    deriving (Show, Eq)

data S = Do S Expr 
       | If Expr S S
       | End 
    deriving (Show, Eq)


evalOp :: Op -> Expr
evalOp (Add (ValInt i) (ValInt j)) = ValInt (i + j)
evalOp (Sub (ValInt i) (ValInt j)) = ValInt (i - j)
evalOp (Mul (ValInt i) (ValInt j)) = ValInt (i * j)
evalOp (Div (ValInt i) (ValInt j)) = ValInt (i `div` j)

evalS :: S ->
evalS (Do S e) s =



