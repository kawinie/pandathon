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
            | I Int         -- Int 
            | F Float       -- Float 
            | B Bool        -- Bool  (sugar for Int 0 and 1)
            | Str String    -- String 
            | Cat Expr Expr
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Lambda [String] Block
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
-- Semantic Domain: [Env] -> [Env]
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
                | For Statement Test Statement Block
                | Func Var [String] Block    -- Declaration 
                | Call Var [Expr]            -- Application
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
get :: Var -> [Env] -> Maybe Expr
get v [] = Nothing
get v (env:envs)
    | Nothing    <- result = get v envs
    | Just value <- result = Just value
    where result = Map.lookup v env


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── ASSIGNMENT ─────────────────────────────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
pandaLet :: Var -> Expr -> [Env]  -> [Env]
pandaLet v expr stack@(env:envs) 
    | Nothing <- old = Map.insert v new env : envs
    | Just _  <- old = error ("(ERROR) Variable " ++ show v ++ " has already been declared")
    where 
        old = Map.lookup v env
        new = val expr stack


-- -- ────────────────────────────────────────────────────────────────────────────────
-- -- ─── UPDATE THE VALUE IF EXISTS ─────────────────────────────────────────────────
-- -- ────────────────────────────────────────────────────────────────────────────────
adjust :: Var -> Env -> Expr -> Env 
adjust v env new
    | Just _  <- result = Map.adjust (\_ -> new) v env
    | Nothing <- result = env
    where result = Map.lookup v env


set :: Var -> Expr -> [Env] -> [Env]
set v expr stack@(env:envs)
    | Just _  <- old = map (\e -> adjust v e new) stack
    | Nothing <- old = error ("(ERROR) Variable " ++ show v ++ " doesn't exist")
    where 
        old = get v stack
        new = val expr stack



op :: (Expr, Expr) -> (Int -> Int -> Int) -> (Float -> Float -> Float) -> Expr
op (I a, I b) o _ = I (o a b)
op (F a, I b) _ o = F (o a (fromIntegral b))
op (I a, F b) _ o = F (o (fromIntegral a) b)
op (F a, F b) _ o = F (o a b)
op (a, b) _ _ = error ("(ERROR) Operation cannot be performed: " ++ show a ++ " " ++ show b)


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── GET LITERAL VALUE OF THE EXPRESSION ───────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
val :: Expr -> [Env] -> Expr
val (Get v) stack
    | Nothing  <- result = error ("(ERROR) Variable " ++ show v ++ " doesn't exist")
    | Just (e) <- result = e
    where result = get v stack
    

val (B True) stack   = I 1
val (B False) stack  = I 0
val (Add e1 e2) stack = op (val e1 stack, val e2 stack) (+) (+)
val (Sub e1 e2) stack = op (val e1 stack, val e2 stack) (-) (-)
val (Mul e1 e2) stack = op (val e1 stack, val e2 stack) (*) (*)
val (Div e1 e2) stack
    | elem (snd result) [I 0, F 0, B False] = error "(ERROR) Division by zero"
    | otherwise = op result div (/)
    where result = (val e1 stack, val e2 stack)


    -- ─── CONCATENATION ──────────────────────────────────────────────────────────────
    -- Perform regular concatenation if both expression are string 
    -- If one of two expressions is a string, convert the other to a string using show()
val (Cat e1 e2) stack
    | (Str s1, Str s2) <- result = Str(s1 ++ s2)
    | (Str s, I a)     <- result = Str(s ++ show a)
    | (I a, Str s)     <- result = Str(show a ++ s)
    | (Str s, F a)     <- result = Str(s ++ show a)
    | (F a, Str s)     <- result = Str(show a ++ s)
    where result = (val e1 stack, val e2 stack)

val e stack = e


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR TEST ────────────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
test :: Test -> [Env] -> Bool
test (Eq e1 e2) stack  = val e1 stack == val e2 stack 
test (Lt e1 e2) stack  = val e1 stack <  val e2 stack
test (Lte e1 e2) stack = val e1 stack <= val e2 stack
test (Gt e1 e2) stack  = val e1 stack >  val e2 stack
test (Gte e1 e2) stack = val e1 stack >= val e2 stack


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR STATEMENT ───────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
stmt ::  Statement -> [Env] -> [Env]
stmt (End) stack          = stack
stmt (If t ss1 ss2) stack = if test t stack then stmt (Begin ss1) stack else stmt (Begin ss2) stack
stmt (While t ss) stack   = if test t stack then stmt (While t ss) (stmt (Begin ss) stack) else stack
stmt (For init test update ss) stack = stmt (While test (ss ++ [update])) (stmt init stack)
stmt (Set v expr) stack   = set v expr stack
stmt (Let v expr) stack   = pandaLet v expr stack
stmt (Begin ss)   stack   = runBlock ss (Map.empty : stack) -- Enter a block

-- ─── SUGAR ──────────────────────────────────────────────────────────────────────
stmt (AddTo v expr) stack = set v (Add (val (Get v) stack) (expr)) stack
stmt (SubTo v expr) stack = set v (Sub (val (Get v) stack) (expr)) stack
stmt (MulTo v expr) stack = set v (Mul (val (Get v) stack) (expr)) stack
stmt (DivTo v expr) stack = set v (Div (val (Get v) stack) (expr)) stack
stmt (Dec v) stack = stmt (SubTo v (I 1)) stack
stmt (Inc v) stack = stmt (AddTo v (I 1)) stack

-- Functions
stmt (Func v params block) stack = pandaLet v (Lambda params block) stack 
stmt (Call v args) stack 
    | Nothing <- result = error ("(ERROR) Function (" ++ v ++ ") doesn't exist")
    | Just (Lambda params body) <- result = runBlock body (Map.fromList (zip params values) : stack)
    where 
        result = get v stack
        values = map (\x -> val x stack) args


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR A SERIES OF STATEMENTS ──────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
beginBlock :: [Statement] -> [Env] -> [Env]
beginBlock ss stack = runBlock ss (Map.empty : stack)

runBlock :: [Statement] -> [Env] -> [Env]
runBlock [] (lastFrame:[]) = [lastFrame]        -- Keep the last stackframe (global)
runBlock [] (stackFrame:rest) = rest            -- Exit a block
runBlock (s:ss) stack = runBlock ss (stmt s stack)


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR THE PROG WITH INITIALLY EMPTY ENVIRONMENT ───────────
-- ────────────────────────────────────────────────────────────────────────────────
panda :: [Statement] -> [Env]
panda ss = runBlock ss [Map.empty]


pandaStack :: [Statement] -> [Env] -> [Env]
pandaStack = runBlock


-- Fibbonaci example:
fib = [
        Let "result" (I 0),
        Func "fib" ["count"] [
            Let "a" (I 0),
            Let "b" (I 1),
            For (Let "i" (I 0)) (Lt (Get "i") (Get "count")) (Inc "i") [
                Let "c" (Add (Get "a") (Get "b")),
                Set "a" (Get "b"),
                Set "b" (Get "c")
            ],
            Set "result" (Get "a")
        ],
        Call "fib" [(I 10)]
    ]


--swaps two variables
swap = [
        Let "resultx" (I 0),
        Let "resulty" (I 1),
        Func "swap" ["x", "y"] [
            Let "temp" (Get "x"),
            Set "resultx" (Get "y"),
            Set "resulty" (Get "temp")
        ],
        Call "swap" [(Get "resultx"), (Get "resulty")]
    ]


-- example of first class functions in action
cubs = [
        Let "result" (I 0),
        Func "add" ["y"] [
            Func "+" ["x"] [AddTo "result" (Get "x")],
            Call "+" [Get "y"]
        ],
        Call "add" [(I 10)],
        Call "add" [(I 20)]
    ]

cubs2 = [
        Let "result" (F 1),
        Func "mul" ["y"][
            Func "+" ["x"] [MulTo "result" (Get "x")],
            Call "+" [Get "y"]
        ],
        Call "mul" [(I 5)],
        Call "mul" [(F 8.5)]
    ]

cubs3 = [
        Let "x" (I, 0)]
        (Set "x" (Lte(I 3)(I 4)))   
    

-- string concatenation
testing1 = [
        Let "x" (I 0),
        Let "max" (I 10),
        Let "String1" (Str "test"),
        While (Lt (Get "x") (Get "max")) [
            Set "String1" (Cat (Get "String1") (Get "String1")),
            Inc "x"
        ]
    ]

{- testing2 = [
        Let "s1" (Str "test"),
        For (Set "i" (I 0)) (Lt (Get "i") (I 10)) (Inc "i") [
            Set "s1" (Cat (Get "s1") (Get "s1"))
        ]
    ] -}

demoStack :: [Env] 
demoStack = [
        Map.fromList [("resultx", (I 0)), ("resulty", (I 1))]
        -- Map.fromList [("y", I 2)],
        -- Map.fromList [("y", I 10)]
    ]

main = do 
    print (Map.fromList (zip ["a", "b", "c"] [(I 10), (F 5), (Str "Panda")]))
     