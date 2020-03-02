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

data Statement  = Set Var Expr
                | Inc Var
                | Dec Var
                | AddTo Var Expr
                | SubTo Var Expr
                | MulTo Var Expr
                | DivTo Var Expr
                | If Test Block Block
                | While Test Block
                | Func String [String] Block
                | Call String [String]
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
get :: Env -> Var -> Expr
get env v
    | Nothing <- value = error ("(ERROR) Variable " ++ show v ++ " doesn't exist") 
    | Just e  <- value = e
    where value = Map.lookup v env 


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── UPDATE THE VALUE IF EXISTS OR INSERT A NEW ONE OTHERWISE ───────────────────
-- ────────────────────────────────────────────────────────────────────────────────
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
op _ _ _ = error "(ERROR) Operation cannot be performed"


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── GET LITERAL VALUE OF THE EXPRESSION ───────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
val :: Env -> Expr -> Expr
val env (Get v)   = get env v
val env (B True)  = I 1
val env (B False) = I 0
val env (Add e1 e2) = op (val env e1, val env e2) (+) (+)
val env (Sub e1 e2) = op (val env e1, val env e2) (-) (-)
val env (Mul e1 e2) = op (val env e1, val env e2) (*) (*)
val env (Div e1 e2)
    | elem (snd result) [I 0, F 0, B False] = error "(ERROR) Division by zero"
    | otherwise = op result div (/)
    where result = (val env e1, val env e2)


    -- ─── CONCATENATION ──────────────────────────────────────────────────────────────
    -- Perform regular concatenation if both expression are string 
    -- If one of two expressions is a string, convert the other to a string using show()
val env (Cat e1 e2)
    | (Str s1, Str s2) <- result = Str(s1 ++ s2)
    | (Str s, I a)     <- result = Str(s ++ show a)
    | (I a, Str s)     <- result = Str(show a ++ s)
    | (Str s, F a)     <- result = Str(s ++ show a)
    | (F a, Str s)     <- result = Str(show a ++ s)
    where result = (val env e1, val env e2)

val env e = e


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR TEST ────────────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
test :: Env -> Test -> Bool
test env (Eq e1 e2)  = val env e1 == val env e2
test env (Lt e1 e2)  = val env e1 <  val env e2
test env (Lte e1 e2) = val env e1 <= val env e2
test env (Gt e1 e2)  = val env e1 >  val env e2
test env (Gte e1 e2) = val env e1 >= val env e2


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR STATEMENT ───────────────────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
stmt :: Env -> Statement -> Env
stmt env (End) = env
stmt env (If t ss1 ss2) = if test env t then runBlock env ss1 else runBlock env ss2
stmt env (While t ss)   = if test env t then stmt (runBlock env ss) (While t ss) else env
stmt env (Set v expr)   = set env v expr
stmt env (Begin ss)     = runBlock env ss -- Enter a block

-- ─── SUGAR ──────────────────────────────────────────────────────────────────────
stmt env (AddTo v expr) = set env v (Add (get env v) (expr))
stmt env (SubTo v expr) = set env v (Sub (get env v) (expr))
stmt env (MulTo v expr) = set env v (Mul (get env v) (expr))
stmt env (DivTo v expr) = set env v (Div (get env v) (expr))
stmt env (Dec v) = stmt env (SubTo v (I 1))
stmt env (Inc v) = stmt env (AddTo v (I 1))


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR A SERIES OF STATEMENTS ──────────────────────────────
-- ────────────────────────────────────────────────────────────────────────────────
-- TODO: remove the stackframe when exiting the block
-- TODO: push a stackframe on to the stack
runBlock :: Env -> [Statement] -> Env
runBlock env [] = env   -- Exit a block
runBlock env (s:ss) = runBlock (stmt env s) ss


-- ────────────────────────────────────────────────────────────────────────────────
-- ─── VALUATION FUNCTION FOR THE PROG WITH INITIALLY EMPTY ENVIRONMENT ───────────
-- ────────────────────────────────────────────────────────────────────────────────
panda :: [Statement] -> Env
panda = runBlock Map.empty

-- Fibbonaci:
cubs = [
        Set "a" (I 0),
        Set "b" (I 1),
        Set "count" (I 0),
        While (Lt (Get "count") (I 10)) [
            Set "c" (Add (Get "a") (Get "b")),
            Set "a" (Get "b"),
            Set "b" (Get "c"),
            Inc "count"
        ]
    ]


main = do 
    print (panda cubs)
     
-- *Main> panda cubs
-- fromList [("count",I 6),("s",Str "Panda Panda 999.0"),("x",F 13.0)]

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
