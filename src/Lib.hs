{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}


module Lib
    ( principlePair
    , equations
    , Expr(..)
    , Type(..)
    , Env(..)
    ) where



import Control.Monad.Except
import Data.List
import Data.Semigroup
import Control.Monad.State
import Control.Monad.Identity


infixl 4 :@
infixr 3 :->

type Symb = String 

-- Терм
data Expr = Var Symb 
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb 
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)


-- Свободные переменные
freeVars :: Expr -> [Symb] 
freeVars (Var x) = [x]
freeVars (x :@ y) = freeVars x `union` freeVars y
freeVars (Lam v x) = freeVars x \\ [v]

freeTVars :: Type -> [Symb]
freeTVars (TVar t) = [t]
freeTVars (t1 :-> t2) = freeTVars t1 `union` freeTVars t2

-- Вспомогательные функции для работы с контекстом
extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env xs) s t = Env $ (s, t) : xs

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env e) = nub (e >>= (freeTVars . snd))

appEnv :: (MonadError String m) => Env -> Symb -> m Type
appEnv (Env xs) v = case lookup v xs of
                      Nothing -> throwError $ "There is no variable \"" ++ v ++ "\" in the enviroment."
                      Just t -> return t
                      

-- Вспомогательные функции для работы с подстановкой
appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy xs) (TVar x) = case lookup x xs of
                                  Nothing -> TVar x
                                  Just t -> t
appSubsTy xs (a :-> b) = appSubsTy xs a :-> appSubsTy xs b


appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv xs (Env e) = Env $ (\(x, y) -> (x, appSubsTy xs y)) <$> e

erase :: [(Symb, Type)] -> [(Symb, Type)] -> [(Symb, Type)]
erase [] ys = []
erase (x:xs) ys = case lookup (fst x) ys of
                    Nothing -> x : (erase xs ys)
                    otherwise -> (erase xs ys)


-- Композиция подстановок
composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy (SubsTy xs1) (SubsTy xs2) = SubsTy $ (xs1 `erase` xs2) ++ ((\(x, y) -> (x, appSubsTy (SubsTy xs1) y)) <$> xs2)

instance Semigroup SubsTy where
    a <> b = composeSubsTy a b

instance Monoid SubsTy where
  mempty = SubsTy []

unify :: MonadError String m => Type -> Type -> m SubsTy
unify v1@(TVar t1) t2 | v1 == t2               = return $ SubsTy []
                      | elem t1 (freeTVars t2) = throwError $ "Can't unify (" ++ show v1 ++ ") with (" ++ show t2 ++ ")!"
                      | otherwise              = return $ SubsTy [(t1, t2)]
unify t1 v2@(TVar t2) = unify v2 t1
unify (a1 :-> a2) (b1 :-> b2) = do
                                  u2 <- unify a2 b2
                                  u1 <- unify (appSubsTy u2 a1) (appSubsTy u2 b1)
                                  return $ u1 `mappend` u2


getNew :: MonadState Integer m => m Integer
getNew = do
  var <- get
  modify (+1)
  return var

equations' :: MonadError String m => Env -> Expr -> Type -> StateT Integer m [(Type, Type)]
equations' env (Var x) sig = do
                              t2 <- env `appEnv` x
                              return [(sig, t2)]
equations' env (m :@ n) sig = do
                              var <- getNew
                              let nvar = TVar $ "x" ++ show var
                              t1 <- equations' env n (nvar)
                              t2 <- equations' env m (nvar :-> sig)
                              return $ t1 `union` t2
                              
equations' env (Lam symb expr) sig = do
                              var1 <- getNew
                              let nvar1 = TVar $ "x" ++ show var1
                              var2 <- getNew
                              let nvar2 = TVar $ "x" ++ show var2
                              t1 <- equations' (extendEnv env symb nvar1) expr nvar2
                              let t2 = [(nvar1 :-> nvar2, sig)]
                              return $ t1 `union` t2

-- Построение системы уравнений на типы для контекста, терма и начального типа для терма:
equations :: MonadError String m => Env -> Expr -> Type -> m [(Type,Type)]
equations env x sig = evalStateT (equations' env x sig) 0

unite2 :: (Type, Type) -> (Type, Type) -> (Type, Type)
unite2 (a, b) (c, d) = (a :-> c, b :-> d)

unite :: [(Type, Type)] -> (Type, Type)
unite (x : xs) = foldl unite2 x xs
unite [] = undefined

construct :: [Symb] -> Int -> [(Symb, Type)]
construct (x : xs) t = (x, TVar $ "y" ++ show t) : construct xs (t + 1)
construct [] t = []

-- Поиск главной пары для терма
principlePair :: MonadError String m => Expr -> m (Env,Type)
principlePair expr = do
                        eq <- (equations env0 expr sigma)
                        let (e1, e2) = unite eq
                        s <- unify e1 e2
                        return (appSubsEnv s env0, appSubsTy s sigma)
                     where
                        sigma = TVar "init_type"
                        env0 = Env $ construct (freeVars expr) 0