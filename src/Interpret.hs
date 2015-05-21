module Interpret where

import AbsRWHILE
import ErrM
import Control.Monad(when)
import Data.List(nub)
import Debug.Trace

type Store = [(Ident,Val)]

emptyS :: Store
emptyS = []

insertS :: Store -> Ident -> Val -> Store
insertS st x v = (x, v) : st

updateS :: Store -> Ident -> Val -> Store
updateS []         x' v' = [(x', v')]
updateS ((x,v):st) x' v' = if x==x' then (x,v') : st
                           else (x,v) : updateS st x' v'

lookupS :: Store -> Ident -> Err Val
lookupS st x = case lookup x st of
                 Nothing -> fail ("lookupS: " ++ show x)
                 Just v  -> return v

type Result = Err Store

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

-- Whether all the values are set to nil except out
check :: Store -> Ident -> Err ()
check []           out = return ()
check ((x,Nil):st) out = check st out
check ((x,v):st)   out = if x==out then check st out else fail "not zero cleared"

execProgram :: Val -> Program -> Err Val
execProgram val prog@(PDefs inn cs out) =
    do let initS = map (\x -> (x,Nil)) (nub (vars prog))
       st <- foldl execCmd (return (updateS initS inn val)) cs
       check st out
       lookupS st out

execCmds :: Result -> [Cmd] -> Result
execCmds = foldl execCmd

execCmd :: Result -> Cmd -> Result
execCmd res cmd =
    do st <- res
       case cmd of
         SAss x e  -> do v <- lookupS st x
                         v_e <- evalExp res e
                         return $ updateS st x $
                                if (v /= Nil) && (v_e == v)
                                  then Nil
                                  else v_e
         SIf e1 c1 c2 e2 -> do v1 <- evalExp res e1
                               let res2 = execCmds res (if v1 /= Nil then c1 else c2)
                               v2 <- evalExp res2 e2
                               if (v1 == Nil && v2 /= Nil) || (v1 /= Nil && v2 == Nil)
                                  then fail ("Assertion of conditional " ++ show cmd ++ " failed on exit")
                                  else res2
         SLoop e1 c1 c2 e2  -> do v1 <- evalExp res e1
                                  when (v1 == Nil) $ fail ("Assertion of loop " ++ show cmd ++ " failed on entry")
                                  execCmds res c1
                                  doloop res
                                    where doloop res = do v2 <- evalExp res e2
                                                          if v2 /= Nil then res else doloop2 res
                                          doloop2 res = let res2 = execCmds res c2
                                                        in do v1 <- evalExp res2 e1
                                                              if v1 /= Nil then fail "Assertion of loop failed (in loop)"
                                                              else let res3 = execCmds res2 c1
                                                                   in doloop res3
         SStatus -> traceShow st res

evalExp :: Result -> Exp -> Err Val
evalExp res exp = case exp of
  AEq e1 e2  -> do v1 <- evalExp res e1
                   v2 <- evalExp res e2
                   return (if v1==v2 then Cons Nil Nil else Nil)
  ATl e  -> do v <- evalExp res e
               case v of
                 Nil -> fail "evaluating tl nil"
                 Cons _ v2 -> return v2
  AHd e  -> do v <- evalExp res e
               case v of
                 Nil -> fail "evaluating tl nil"
                 Cons v1 _ -> return v1
  ACons e1 e2  -> do v1 <- evalExp res e1
                     v2 <- evalExp res e2
                     return $ Cons v1 v2
  AVar x  -> do st <- res
                lookupS st x
  ANil  -> return Nil


-- List all variables
class Variables a where
    vars :: a -> [Ident]

instance Variables Program where
    vars (PDefs x cs y) = x : y : concatMap vars cs

instance Variables Cmd where
    vars (SAss x e) = x : vars e
    vars (SIf e1 c1 c2 e2) = vars e1 ++ concatMap vars c1 ++ concatMap vars c2 ++ vars e2
    vars (SLoop e1 c1 c2 e2) = vars e1 ++ concatMap vars c1 ++ concatMap vars c2 ++ vars e2
    vars SStatus = []

instance Variables Exp where
    vars (AEq e1 e2) = vars e1 ++ vars e2
    vars (ATl e) = vars e
    vars (AHd e) = vars e
    vars (ACons e1 e2) = vars e1 ++ vars e2
    vars (AVar x) = [x]
    vars ANil = []

