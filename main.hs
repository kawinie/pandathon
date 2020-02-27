import qualified Data.Map as Map
    
data Expr   = Get Var
            | I Int      -- Int primitive
            | F Float    -- Float primitive
            | B Bool     -- Bool primitive (sugar for Int 0 and 1)
            | Str String -- String 
            | Cat Expr Expr
            | Def String [String] [Statement]
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
    deriving (Show, Eq)

instance Ord Expr where
    (I a) `compare` (I b) = a `compare` b
    (I a) `compare` (F b) = fromIntegral(a) `compare` b
    (F a) `compare` (I b) = a `compare` fromIntegral(b)
    (F a) `compare` (F b) = a `compare` b
        
data Test   = Eq  Expr Expr
            | Lt  Expr Expr 
            | Lte Expr Expr
            | Gt  Expr Expr
            | Gte Expr Expr
    deriving (Show, Eq)

data Statement  = If Test Statement Statement
                | Set Var Expr
                | Inc Var
                | Dec Var
                | While Test Statement
                | Call String [String]
                | Do [Statement]
                | End 
        deriving (Show, Eq)


type Var = String 
type Env = Map.Map Var Expr
type StackFrame = [Env]


-- Get literal of var v from the env 
get :: Env -> Var -> Expr
get env v
    | Nothing <- value = error ("ERROR: Variable " ++ show v ++ " doesn't exist") 
    | Just e  <- value = e
    where value = Map.lookup v env 

-- Update and or insert (var, val) if doesn't exist in the env
set :: Env -> Var -> Expr -> Env
set env v expr
    | Just _  <- old = Map.adjust (\_ -> new) v env
    | Nothing <- old = Map.insert v new env
    where 
        old = Map.lookup v env
        new = val env expr

op :: (Expr, Expr) -> (Int -> Int -> Int) -> (Float -> Float -> Float) -> Expr
op (I a, I b) o _ = I (o a b)
op (F a, I b) _ o = F (o a (fromIntegral b))
op (I a, F b) _ o = F (o (fromIntegral a) b)
op (F a, F b) _ o = F (o a b)

-- Get literal value of the expression 
val :: Env -> Expr -> Expr
val env (Get v)   = get env v
val env (B True)  = I 1
val env (B False) = I 0
val env (Add e1 e2) = op (val env e1, val env e2) (+) (+)
val env (Sub e1 e2) = op (val env e1, val env e2) (-) (-)
val env (Mul e1 e2) = op (val env e1, val env e2) (*) (*)
val env (Div e1 e2)
    | elem (snd result) [I 0, F 0, B False] = error "ERROR: Division by zero"
    | otherwise = op result div (/)
    where result = (val env e1, val env e2)

val env (Cat ex1 ex2)
    | (Str ex1, Str ex2) <- result = Str(ex1 ++ ex2)
    | (Str ex1, I ex2)   <- result = Str(ex1 ++ show ex2)
    | (I ex1, Str ex2)   <- result = Str(show ex1 ++ ex2)
    | (Str ex1, F ex2)   <- result = Str(ex1 ++ show ex2)
    | (F ex1, Str ex2)   <- result = Str(show ex1 ++ ex2)
    where result = (val env ex1, val env ex2)

val env e = e

-- Valuation function for Test 
test :: Env -> Test -> Bool
test env (Eq e1 e2)  = val env (Sub e1 e2) == (F 0.0)
test env (Lt e1 e2)  = val env (Sub e1 e2) <  (F 0.0)
test env (Lte e1 e2) = val env (Sub e1 e2) <= (F 0.0)
test env (Gt e1 e2)  = val env (Sub e1 e2) >  (F 0.0)
test env (Gte e1 e2) = val env (Sub e1 e2) >= (F 0.0)

-- Valuation function for S
stmt :: Env -> Statement -> Env
stmt env (End) = env
stmt env (If t s1 s2) = if test env t then stmt env s1 else stmt env s2
stmt env (While t s)  = if test env t then stmt (stmt env s) (While t s) else env
stmt env (Set v expr) = set env v expr
stmt env (Dec v) = set env v (Sub (get env v) (I 1))
stmt env (Inc v) = set env v (Add (get env v) (I 1))
stmt env (Do ss) = run env ss


-- Valuation function for a series of S ([S])
run :: Env -> [Statement] -> Env
run env [] = env
run env (s:ss) = run (stmt env s) ss

-- Valuation function for the prog with initially empty env 
panda :: [Statement] -> Env
panda = run Map.empty


cubs = [
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
        Set "s" (Cat (Get "s") (Str " Panda")),
        Set "s" (Cat (Get "s") (Cat (Str " ") (F 999)))
    ]