module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

-- variable state
type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st name value = aux
  where aux x
          | x == name = value
          | otherwise = st x

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var name)         = st name
evalE st (Val v)            = v
evalE st (Op e1 Plus e2)    = evalE st e1 + evalE st e2
evalE st (Op e1 Minus e2)   = evalE st e1 - evalE st e2
evalE st (Op e1 Times e2)   = evalE st e1 * evalE st e2
evalE st (Op e1 Divide e2)  = evalE st e1 `div` evalE st e2
evalE st (Op e1 Gt e2)      = fromEnum (evalE st e1 > evalE st e2)
evalE st (Op e1 Ge e2)      = fromEnum (evalE st e1 >= evalE st e2)
evalE st (Op e1 Lt e2)      = fromEnum (evalE st e1 < evalE st e2)
evalE st (Op e1 Le e2)      = fromEnum (evalE st e1 <= evalE st e2)
evalE st (Op e1 Eql e2)     = fromEnum (evalE st e1 == evalE st e2)

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

-- incr is sytanx sugar, for is while's sugar
desugar :: Statement -> DietStatement
desugar (Incr name)      = (DAssign name (Op (Var name) Plus (Val 1)))
desugar (For s1 e s2 s3) = (DSequence (desugar s1) (DWhile e (DSequence (desugar s3) (desugar s2))))
desugar (Assign name e)  = (DAssign name e)
desugar (If e s1 s2)     = (DIf e (desugar s1) (desugar s2))
desugar (While e s)      = (DWhile e (desugar s))
desugar (Sequence s1 s2) = (DSequence (desugar s1) (desugar s2))
desugar Skip             = DSkip


-- Exercise 4 -----------------------------------------

-- different with expression，statement change state instead of change value
evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign name e)  = extend st name (evalE st e)
evalSimple st (DIf e s1 s2)     = case (evalE st e) of
  1 -> evalSimple st s1
  0 -> evalSimple st s2
evalSimple st (DWhile e s)      = case (evalE st e) of
  1 -> evalSimple (evalSimple st s) (DWhile e s)
  0 -> st
evalSimple st (DSequence s1 s2) = evalSimple (evalSimple st s1) s2
evalSimple st DSkip             = st

--
run :: State -> Statement -> State
run st s = evalSimple st (desugar s)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input
   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input
   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number
   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]