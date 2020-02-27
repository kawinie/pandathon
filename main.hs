{--
    Imperative 
    Basic data types and operations. You should provide at least boolean values and integers; you may want to include floating point numbers as well.

    Conditionals.
    
    Recursion/loops.

    Names (imperative/functional languages only).
    
    Procedures/functions with arguments (or some other abstraction mechanism). You should provide a way to factor out repeated code and give it a name so that it can be reused. For imperative/functional languages, you must decide what kind of parameter passing scheme to use, which we’ll discuss in class. (Passing arguments is trivial for stack-based languages since arguments are passed on the stack!).
--}
import qualified Data.Map as Map


data E = Get Var
       | I Int      -- Int primitive
       | F Float    -- Float primitive
       | B Bool     -- Bool primitive (sugar for Int 0 and 1)
       | Str String -- String 
        -- Pandathlon: 
        -- Set "x" (Str "Fried")
        -- Concat (Get "x") (Str " Egg")
        --
        -- Python: 
        -- x = "Fried"
        -- x + " Egg"
       | Concat E E 
       | Func String [String] [S]
       | Add E E
       | Sub E E
       | Mul E E
       | Div E E
    deriving (Show, Eq)

instance Ord E where
    (I a) `compare` (I b) = a `compare` b
    (I a) `compare` (F b) = fromIntegral(a) `compare` b
    (F a) `compare` (I b) = a `compare` fromIntegral(b)
    (F a) `compare` (F b) = a `compare` b
        
data T = Eq  E E
       | Lt  E E
       | Lte E E
       | Gt  E E
       | Gte E E
    deriving (Show, Eq)

data S = If T S S
       | Set Var E
       | Inc Var
       | Dec Var
       | While T S
       | Call String [String]
       | Do [S]
       | End 
    deriving (Show, Eq)


type Var = String 
type Env = Map.Map Var E


-- Get literal of var v from the env 
get :: Env -> Var -> E
get env v
    | Nothing <- x = error ("ERROR: Variable " ++ show v ++ " doesn't exist") 
    | Just e  <- x = e
    where x = Map.lookup v env 

-- Update and or insert (var, val) if doesn't exist in the env
set :: Env -> Var -> E -> Env
set env v1 expr
    | Just _  <- x = Map.adjust (\x -> y) v1 env
    | Nothing <- x = Map.insert v1 y env
    where 
        x = Map.lookup v1 env
        y = val env expr


-- Get literal value of the expression 
val :: Env -> E -> E
val env (Get v)   = get env v
val env (B True)  = I 1
val env (B False) = I 0
val env (Add e1 e2) 
    | (I a, I b) <- result = I (a + b)
    | (F a, F b) <- result = F (a + b)
    | (I a, F b) <- result = F (fromIntegral(a) + b)
    | (F a, I b) <- result = F (a + fromIntegral(b))
    where result = (val env e1, val env e2)

val env (Sub e1 e2)
    | (I a, I b) <- result = I (a - b)
    | (F a, F b) <- result = F (a - b)
    | (I a, F b) <- result = F (fromIntegral(a) - b)
    | (F a, I b) <- result = F (a - fromIntegral(b))
    where result = (val env e1, val env e2)

val env (Mul e1 e2)
    | (I a, I b) <- result = I (a * b)
    | (F a, F b) <- result = F (a * b)
    | (I a, F b) <- result = F (fromIntegral(a) * b)
    | (F a, I b) <- result = F (a * fromIntegral(b))
    where result = (val env e1, val env e2)

val env (Div e1 e2)
    | right == I 0 || right == F 0 || right == B False = error "ERROR: Division by zero"
    | (I a, I b) <- result = I (a `div` b)
    | (F a, F b) <- result = F (a / b)
    | (I a, F b) <- result = F (fromIntegral(a) / b)
    | (F a, I b) <- result = F (a / fromIntegral(b))
    where 
        result = (val env e1, val env e2)
        right = snd result

-- TODO: Strings and operations (1)
val env (Concat ex1 ex2)
    | (Str ex1, Str ex2) <- result = Str(ex1 ++ ex2)
    | (Str ex1, I ex2)   <- result = Str(ex1 ++ show ex2)
    | (I ex1, Str ex2)   <- result = Str(show ex1 ++ ex2)
    | (Str ex1, F ex2)   <- result = Str(ex1 ++ show ex2)
    | (F ex1, Str ex2)   <- result = Str(show ex1 ++ ex2)
    where result = (val env ex1, val env ex2)

val env e = e

-- Valuation function for T 
test :: Env -> T -> Bool
test env (Eq e1 e2)  = val env (Sub e1 e2) == (F 0.0)
test env (Lt e1 e2)  = val env (Sub e1 e2) <  (F 0.0)
test env (Lte e1 e2) = val env (Sub e1 e2) <= (F 0.0)
test env (Gt e1 e2)  = val env (Sub e1 e2) >  (F 0.0)
test env (Gte e1 e2) = val env (Sub e1 e2) >= (F 0.0)

-- Valuation function for S
stmt :: Env -> S -> Env
stmt env (End) = env
stmt env (If t s1 s2) = if test env t then stmt env s1 else stmt env s2
stmt env (While t s) = if test env t then stmt (stmt env s) (While t s) else env
stmt env (Set v expr) = set env v expr
stmt env (Dec v) = set env v (Sub (get env v) (I 1))
stmt env (Inc v) = set env v (Add (get env v) (I 1))
stmt env (Do ss) = run env ss


-- Valuation function for a series of S ([S])
run :: Env -> [S] -> Env
run env [] = env
run env (s:ss) = run (stmt env s) ss

-- Valuation function for the prog with initially empty env 
panda :: [S] -> Env
panda = run Map.empty


p1 = [
        Set "x" (I 5),
        Set "count" (I 0),
        While (Lte (Get "x") (I 30)) (
            Do [
                Set "x" (Add (Get "x") (F 5)),
                Inc "count"
            ]
        ),
        If (Eq (Get "x") (I 35)) (
            Do [
                Set "x" (Sub (Get "x") (F 20))
            ]
        ) (
            Do [
                Set "x" (Sub (Get "x") (F 10))
            ]
        ),
        Dec "x",
        Dec "x",
        Set "s" (Str "Panda"),
        Set "s" (Concat (Get "s") (Str " Panda")),
        Set "s" (Concat (Get "s ") (F 999))
    ]

-- p2 = [
--     Func "doSomething" ["x", "y"] [
--         Set "z" ()
--     ]
-- ]

-- Set ("doSomething-x", I 10)
-- Call "doSomething" [I 5, I 10] --> 
--     Set "doSomething-x" (I 5), Set ("doSomething-y") (I 10) 