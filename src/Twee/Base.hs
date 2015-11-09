{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, CPP, ConstraintKinds, UndecidableInstances, DeriveFunctor, StandaloneDeriving #-}
module Twee.Base(
  Symbolic(..), TermOf, TermListOf, SubstOf,
  vars, funs, canonicalise,
  Minimal(..), minimalTerm, isMinimal,
  Skolem(..), skolemConst, skolemise,
  Arity(..), Sized(..), Ordered(..), Strictness(..), Function, Extended(..),
  module Twee.Term, module Twee.Pretty) where

#include "errors.h"
import Prelude hiding (lookup)
import Control.Monad
import qualified Data.DList as DList
import Data.DList(DList)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Twee.Term hiding (subst, canonicalise)
import qualified Twee.Term as Term
import Twee.Pretty
import Text.PrettyPrint.HughesPJClass hiding (empty)
import Data.Ord
import Data.Monoid hiding ((<>))
import Data.Either
import Data.Reflection
import {-# SOURCE #-} Twee.Constraints
import System.IO.Unsafe
import Data.IORef
import Twee.Array

-- Generalisation of term functionality to things that contain terms.
class Symbolic a where
  type ConstantOf a

  term    :: a -> TermOf a
  symbols :: Monoid w => (Fun (ConstantOf a) -> w) -> (Var -> w) -> a -> w
  subst   :: Subst (ConstantOf a) -> a -> a

type TermOf a = Term (ConstantOf a)
type TermListOf a = TermList (ConstantOf a)
type SubstOf a = Subst (ConstantOf a)

instance Symbolic (Term f) where
  type ConstantOf (Term f) = f
  term            = id
  symbols fun var = symbols fun var . singleton
  subst           = Term.subst

instance Symbolic (TermList f) where
  type ConstantOf (TermList f) = f
  term    = __
  symbols = termListSymbols
  subst   = Term.substList

{-# INLINE termListSymbols #-}
termListSymbols :: Monoid w => (Fun f -> w) -> (Var -> w) -> TermList f -> w
termListSymbols fun var = aux
  where
    aux Empty = mempty
    aux (ConsSym (Fun f _) t) = fun f `mappend` aux t
    aux (ConsSym (Var x) t) = var x `mappend` aux t

instance (ConstantOf a ~ ConstantOf b,
          Symbolic a, Symbolic b) => Symbolic (a, b) where
  type ConstantOf (a, b) = ConstantOf a
  term (x, _) = term x
  symbols fun var (x, y) = symbols fun var x `mappend` symbols fun var y
  subst sub (x, y) = (subst sub x, subst sub y)

instance Symbolic a => Symbolic [a] where
  type ConstantOf [a] = ConstantOf a
  term _ = __
  symbols fun var ts = mconcat (map (symbols fun var) ts)
  subst sub = map (subst sub)

{-# INLINE vars #-}
vars :: Symbolic a => a -> [Var]
vars = DList.toList . symbols (const mzero) return

{-# INLINE funs #-}
funs :: Symbolic a => a -> [Fun (ConstantOf a)]
funs = DList.toList . symbols return (const mzero)

canonicalise :: Symbolic a => a -> a
canonicalise t = subst sub t
  where
    sub = Term.canonicalise (map (singleton . var) (vars t))

class Minimal a where
  minimal :: a

instance (Numbered f, Minimal f) => Minimal (Fun f) where
  minimal = toFun minimal

isMinimal :: (Numbered f, Minimal f) => Term f -> Bool
isMinimal (Fun f Empty) | f == minimal = True
isMinimal _ = False

minimalTerm :: (Numbered f, Minimal f) => Term f
minimalTerm = fun minimal []

class Skolem f where
  skolem  :: Var -> f

instance (Numbered f, Skolem f) => Skolem (Fun f) where
  skolem = toFun . skolem

skolemConst :: (Numbered f, Skolem f) => Var -> Term f
skolemConst x = fun (skolem x) []

skolemise :: (Symbolic a, Numbered (ConstantOf a), Skolem (ConstantOf a)) => a -> SubstOf a
skolemise t =
  flattenSubst [(x, skolemConst x) | x <- vars t]

class Arity f where
  arity :: f -> Int

instance (Numbered f, Arity f) => Arity (Fun f) where
  arity = arity . fromFun

class Sized a where
  size  :: a -> Int

instance (Sized f, Numbered f) => Sized (Fun f) where
  size = size . fromFun

instance (Sized f, Numbered f) => Sized (TermList f) where
  size = aux 0
    where
      aux n Empty = n
      aux n (ConsSym (Fun f _) t) = aux (n+size f) t
      aux n (Cons (Var _) t) = aux (n+1) t

instance (Sized f, Numbered f) => Sized (Term f) where
  size = size . singleton

class Ord f => Ordered f where
  orientTerms :: Term f -> Term f -> Maybe Ordering
  orientTerms t u
    | t == u = Just EQ
    | lessEq t u = Just LT
    | lessEq u t = Just GT
    | otherwise = Nothing

  lessEq :: Term f -> Term f -> Bool
  lessIn :: Model f -> Term f -> Term f -> Maybe Strictness

data Strictness = Strict | Nonstrict deriving (Eq, Show)

class    (Numbered f, Ordered f, Arity f, Sized f, Minimal f, Skolem f, PrettyTerm f) => Function f
instance (Numbered f, Ordered f, Arity f, Sized f, Minimal f, Skolem f, PrettyTerm f) => Function f

data Extended f =
    Minimal
  | Function f
  | Skolem Int
  deriving (Eq, Ord, Show, Functor)

instance Minimal (Extended f) where
  minimal = Minimal

instance Skolem (Extended f) where
  skolem (MkVar x) = Skolem x

instance Numbered f => Numbered (Extended f) where
  fromInt 0 = Minimal
  fromInt n
    | odd n     = Skolem ((n-1) `div` 2)
    | otherwise = Function (fromInt ((n-2) `div` 2))

  toInt Minimal = 0
  toInt (Skolem n) = 2*n+1
  toInt (Function f) = 2*toInt f+2

instance Pretty f => Pretty (Extended f) where
  pPrintPrec _ _ Minimal = text "⊥"
  pPrintPrec _ _ (Skolem n) = text "sk" <> pPrint n
  pPrintPrec l p (Function f) = pPrintPrec l p f

instance PrettyTerm f => PrettyTerm (Extended f) where
  termStyle (Function f) = termStyle f
  termStyle _ = uncurried

instance Sized f => Sized (Extended f) where
  size (Function f) = size f
  size _ = 1

instance Arity f => Arity (Extended f) where
  arity (Function f) = arity f
  arity _ = 0

deriving instance Pretty f     => Pretty (AutoNumbered f)
deriving instance PrettyTerm f => PrettyTerm (AutoNumbered f)
deriving instance Sized f      => Sized (AutoNumbered f)
deriving instance Arity f      => Arity (AutoNumbered f)
deriving instance Minimal f    => Minimal (AutoNumbered f)
deriving instance Skolem f     => Skolem (AutoNumbered f)