module AbsRWHILE where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
data Program =
   PDefs Ident [Cmd] Ident
  deriving (Eq,Ord,Show)

data Cmd =
   SAss Ident Exp
 | SIf Exp [Cmd] [Cmd] Exp
 | SLoop Exp [Cmd] [Cmd] Exp
 | SStatus
  deriving (Eq,Ord,Show)

data Exp =
   AEq Exp Exp
 | ATl Exp
 | AHd Exp
 | ACons Exp Exp
 | AVar Ident
 | ANil
  deriving (Eq,Ord,Show)

data Val =
   Cons Val Val
 | Nil
  deriving (Eq,Ord,Show)

