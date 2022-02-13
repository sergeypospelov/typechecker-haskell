# Typechecker
Typechecker for simply typed lambda calculus in Curry form.

## Installation
1. Download 
2. Use Cabal or Stack:
```
$ cabal build
$ stack build
```
3. Run tests:
```
$ cabal test
$ stack test
```
## Api
* Api classes:
```haskell
infixl 4 :@
infixr 3 :->

type Symb = String 

data Expr = Var Symb 
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

data Type = TVar Symb 
          | Type :-> Type
  deriving (Eq,Show)

newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)
```
* Type inference:
```haskell
principlePair :: MonadError String m => Expr -> m (Env, Type) -- finds a context and a type for a term or throws an exception if the term is untyped.
```
