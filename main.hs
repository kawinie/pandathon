--
-- ──────────────────────────────────────────────────── I ──────────
--   :::::: I M P O R T : :  :   :    :     :        :          :
-- ──────────────────────────────────────────────────────────────
--
import qualified Data.Map as Map
    

--
-- ────────────────────────────────────────────────────────────────────── II ──────────
--   :::::: A B S T R A C T   S Y N T A X : :  :   :    :     :        :          :
-- ──────────────────────────────────────────────────────────────────────────────── 
--
data Expr   = Get Var
            | I Int      -- Int 
            | F Float    -- Float 
            | B Bool     -- Bool  (sugar for Int 0 and 1)
            | Str String -- String 
            | Cat Expr Expr
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Capture Env
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


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── STATEMENT ──────────────────────────────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
-- Semantic Domain: Env -> Env
data Statement  = Set Var Expr
                | Let Var Expr
                | Inc Var
                | Dec Var
                | AddTo Var Expr
                | SubTo Var Expr
                | MulTo Var Expr
                | DivTo Var Expr
                | If Test Block Block
                | While Test Block
                | Func String [String] Block    -- Declaration 
                | Call String [String]          -- Application
                | Begin Block
                | End 
        deriving (Show, Eq)


type Block = [Statement]
type Var = String 
type Env = Map.Map Var Expr
type Stack = [Env]


--
-- ────────────────────────────────────────────────────────────────────────────── III ──────────
--   :::::: V A L U A T I O N   F U N C T I O N S : :  :   :    :     :        :          :
-- ────────────────────────────────────────────────────────────────────────────────────────
--
-- ────────────────────────────────────────────────────────────────────────────────
-- ─── RETREIVE THE LITERAL VALUE OF THE EXPRESSION FROM THE ENVIRONMENT ──────────
-- ────────────────────────────────────────────────────────────────────────────────
get :: Var -> Env -> Maybe Expr
get v env
    -- | Nothing <- value = error ("(ERROR) Variable " ++ show v ++ " doesn't exist") 
    | Nothing    <- result = Nothing
    | Just value <- result = Just value
    where result = Map.lookup v env 


getStack :: Var -> [Env] -> Maybe Expr
getStack v [] = Nothing
getStack v (env:envs) 
    | Nothing    <- result = getStack v envs
    | Just value <- result = Just value
    where result = Map.lookup v env

-- ────────────────────────────────────────────────────────────────────────────────
-- ─── UPDATE THE VALUE IF EXISTS ─────────────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
set :: Var -> Env -> Expr -> Env
set v env expr
    | Just _  <- old = Map.adjust (\_ -> new) v env
    | Nothing <- old = error ("(ERROR) Variable " ++ show v ++ " doesn't exist")
    where 
        old = Map.lookup v env
        new = val expr env

-- ────────────────────────────────────────────────────────────────────────────────
-- ─── INSERT A NEW VALUE ─────────────────────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
pandaLet :: Var -> Env -> Expr -> Env
pandaLet v env expr
    | Nothing <- old = Map.insert v new env
    | Just _  <- old = error ("(ERROR) Variable " ++ show v ++ " has already been declared")
    where 
        old = Map.lookup v env
        new = val expr env


pandaLetStack :: Var -> [Env] -> Expr -> [Env]
pandaLetStack v (env:envs) expr
    | Nothing <- old = Map.insert v new env : envs
    | Just _  <- old = error ("(ERROR) Variable " ++ show v ++ " has already been declared")
    where 
        old = Map.lookup v env
        new = val expr env


adjust :: Var -> Env -> Expr -> Env 
adjust v env new
    | Just _  <- result = Map.adjust (\_ -> new) v env
    | Nothing <- result = env
    where result = Map.lookup v env


setStack :: Var -> [Env] -> Expr -> [Env]
setStack v stack@(env:envs) expr
    | Just _  <- old = map (\e -> adjust v e new) stack
    | Nothing <- old = error ("(ERROR) Variable " ++ show v ++ " doesn't exist")
    where 
        old = getStack v stack
        new = val expr env


op :: (Expr, Expr) -> (Int -> Int -> Int) -> (Float -> Float -> Float) -> Expr
op (I a, I b) o _ = I (o a b)
op (F a, I b) _ o = F (o a (fromIntegral b))
op (I a, F b) _ o = F (o (fromIntegral a) b)
op (F a, F b) _ o = F (o a b)
op _ _ _ = error "(ERROR) Operation cannot be performed"

-- ────────────────────────────────────────────────────────────────────────────────
-- ─── GET LITERAL VALUE OF THE EXPRESSION ───────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
val :: Expr -> Env -> Expr
val (Get v) env
    | Nothing  <- result = error ("(ERROR) Variable " ++ show v ++ " doesn't exist")
    | Just (e) <- result = e
    where result = get v env
    

val (B True) env   = I 1
val (B False) env  = I 0
val (Add e1 e2) env = op (val e1 env, val e2 env) (+) (+)
val (Sub e1 e2) env = op (val e1 env, val e2 env) (-) (-)
val (Mul e1 e2) env = op (val e1 env, val e2 env) (*) (*)
val (Div e1 e2) env
    | elem (snd result) [I 0, F 0, B False] = error "(ERROR) Division by zero"
    | otherwise = op result div (/)
    where result = (val e1 env, val e2 env)


    -- ─── CONCATENATION ──────────────────────────────────────────────────────────────
    -- Perform regular concatenation if both expression are string 
    -- If one of two expressions is a string, convert the other to a string using show()
val (Cat e1 e2) env
    | (Str s1, Str s2) <- result = Str(s1 ++ s2)
    | (Str s, I a)     <- result = Str(s ++ show a)
    | (I a, Str s)     <- result = Str(show a ++ s)
    | (Str s, F a)     <- result = Str(s ++ show a)
    | (F a, Str s)     <- result = Str(show a ++ s)
    where result = (val e1 env, val e2 env)

val e env = e


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR TEST ────────────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
test :: Test -> Env -> Bool
test (Eq e1 e2) env  = val e1 env == val e2 env 
test (Lt e1 e2) env  = val e1 env <  val e2 env
test (Lte e1 e2) env = val e1 env <= val e2 env
test (Gt e1 e2) env  = val e1 env >  val e2 env
test (Gte e1 e2) env = val e1 env >= val e2 env


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR STATEMENT ───────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
stmt ::  Statement -> Env-> Env
stmt (End) env          = env
stmt (If t ss1 ss2) env = if test t env then runBlock ss1 env else runBlock ss2 env
stmt (While t ss) env   = if test t env then stmt (While t ss) (runBlock ss env) else env
stmt (Set v expr) env   = set v env expr
stmt (Let v expr) env   = pandaLet v env expr
stmt (Begin ss)   env   = runBlock ss env -- Enter a block

-- ─── SUGAR ──────────────────────────────────────────────────────────────────────
stmt (AddTo v expr) env = set v env (Add (val (Get v) env) (expr))
stmt (SubTo v expr) env = set v env (Sub (val (Get v) env) (expr))
stmt (MulTo v expr) env = set v env (Mul (val (Get v) env) (expr))
stmt (DivTo v expr) env = set v env (Div (val (Get v) env) (expr))
stmt (Dec v) env = stmt (SubTo v (I 1)) env
stmt (Inc v) env = stmt (AddTo v (I 1)) env


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR A SERIES OF STATEMENTS ──────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
-- TODO: remove the stackframe when exiting the block
-- TODO: push a stackframe on to the stack
runBlock :: [Statement] -> Env -> Env
runBlock  []  env = env   -- Exit a block
runBlock (s:ss) env = runBlock ss (stmt s env)


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR THE PROG WITH INITIALLY EMPTY ENVIRONMENT ───────────
-- ────────────────────────────────────────────────────────────────────────────────
panda :: [Statement] -> Env
panda ss = runBlock ss Map.empty


-- Fibbonaci:
-- cubs = [
--         Let "a" (I 0),
--         Let "b" (I 1),
--         Let "count" (I 0),
--         Let "c" (I 0),
--         While (Lt (Get "count") (I 10)) [
--             Set "c" (Add (Get "a") (Get "b")),
--             Set "a" (Get "b"),
--             Set "b" (Get "c"),
--             Inc "count"
--         ]
--     ]

demoStack = [
        Map.fromList [("y", I 2)],
        Map.fromList [("y", I 10)]
    ]

main = do 
    print (panda [pandaLetStack "x" demoStack (I 10)] )
     


-- cubs2 = [
--          Set "x" (I 5000),
--          Set "count" (I 0),
--          While (Gte (Get "x")(I 10))(
--              Do[
--                  Set "x" (Div (Get "x")(F 2)),
--                  Inc "count"
--                 ]
--         ) 
--     ]
-- *Main> panda cubs2
-- fromList [("count",I 9),("x",F 9.765625)]


-- Bad examples:
-- cubs3 = [("x", Int)](Set "x" (Lte (I 3) (I 4)))

-- cubs4 = [("x", Float)](Set "x" (Gte (I 3) (I 4)))
