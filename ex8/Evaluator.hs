module Evaluator
where

infixl 6 :+:
infixl 7 :*:
infixr 1 :?:

data Expr
  = Lit Integer    -- a literal
  | Expr :+: Expr  -- addition
  | Expr :*: Expr  -- multiplication
  | Div Expr Expr  -- integer division
  | Expr :?: Expr  -- non-deterministic choice

-- Evaluator from the lecture (slide 421)
evalA :: (Applicative f) => Expr -> f Integer
evalA (Lit i)     = pure i
evalA (e1 :+: e2) = pure (+)  <*> evalA e1 <*> evalA e2
evalA (e1 :*: e2) = pure (*)  <*> evalA e1 <*> evalA e2
evalA (Div e1 e2) = pure div  <*> evalA e1 <*> evalA e2
evalA (_ :?: _)   = error "not implemented"

-- Example for non-deterministic expression
toss :: Expr
toss = Lit 0 :?: Lit 1

-- Your task: Implement this function
evalN :: Expr -> [Integer]
evalN (Lit i)     = pure i
evalN (e1 :+: e2) = pure (+)  <*> evalN e1 <*> evalN e2
evalN (e1 :*: e2) = pure (*)  <*> evalN e1 <*> evalN e2
evalN (Div e1 e2) = pure div  <*> evalN e1 <*> evalN e2
evalN (e1 :?: e2)   = evalN e1 ?? evalN e2

(??) :: [Integer] -> [Integer] -> [Integer]
m ?? n = do 
         x <- concat([m,n])
         return x
