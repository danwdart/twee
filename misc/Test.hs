{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
module Test where

import           Control.Monad
import           Data.Int
import           Data.List
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Ord
import           Data.Typeable
import           GHC.Generics
import           Test.QuickCheck     hiding (Fun, Function)
import           Test.QuickCheck.All
import           Text.PrettyPrint
import           Twee.Base           hiding (F)
import           Twee.Constraints
import           Twee.CP
import           Twee.Equation
import qualified Twee.Index          as Index
import qualified Twee.KBO            as Ord
import           Twee.Pretty
import           Twee.Proof
import           Twee.Rule
import           Twee.Term           hiding (F, canonicalise, subst)
import           Twee.Term.Core      hiding (F)
import           Twee.Utils

data Func = F Int Integer deriving (Eq, Ord, Show)
  deriving Labelled via (AutoLabel Func)

instance Pretty Func where pPrint (F f _) = text "f" <#> int f
instance PrettyTerm Func
instance Arbitrary (Subst Func) where
  arbitrary = fmap (fromJust . listToSubst) (liftM2 zip (fmap nub arbitrary) (infiniteListOf arbitrary))
instance Arbitrary Func where
  arbitrary = F <$> choose (1, 1) <*> choose (1, 3)
instance Minimal Func where
  minimal = fun (F 0 1)
instance Ord.Sized Func where size (F _ n) = n
instance Ord.Weighted Func where argWeight _ = 1
instance Arity Func where
  arity (F 0 _) = 0
  arity (F 1 _) = 2
instance EqualsBonus Func

instance Arbitrary Var where arbitrary = fmap V (choose (0, 3))
instance (Labelled f, Ord f, Typeable f, Arbitrary f) => Arbitrary (Fun f) where
  arbitrary = fmap fun arbitrary

instance (Labelled f, Ord f, Typeable f, Arbitrary f, Arity f) => Arbitrary (Term f) where
  arbitrary =
    sized $ \n ->
      oneof $
        (build . var <$> arbitrary) :
        [ do { f <- arbitrary; build . app f <$> vectorOf (arity f) (resize ((n-1) `div` arity f) arbitrary :: Gen (Term f)) } | n > 0 ]
  shrink (App f ts0) =
    ts ++ (build . app f <$> shrinkOne ts)
    where
      ts = unpack ts0
      shrinkOne [] = []
      shrinkOne (x:xs) =
        [ y:xs | y <- shrink x ] ++
        [ x:ys | ys <- shrinkOne xs ]
  shrink _ = []

data Pair f = Pair (Term f) (Term f) deriving Show

instance (Labelled f, Ord f, Typeable f, Arbitrary f, Arity f) => Arbitrary (Pair f) where
  arbitrary = liftM2 Pair arbitrary arbitrary
  shrink (Pair x y) =
    [ Pair x' y  | x' <- shrink x ] ++
    [ Pair x y'  | y' <- shrink y ] ++
    [ Pair x' y' | x' <- shrink x, y' <- shrink y ]

instance (Labelled f, Ord f, Typeable f, Arbitrary f, Arity f) => Arbitrary (Equation f) where
  arbitrary = do
    Pair t u <- arbitrary
    return (t :=: u)
  shrink (t :=: u) = [t' :=: u' | Pair t' u' <- shrink (Pair t u)]

instance Ordered Func where
  lessIn = Ord.lessIn
  lessEq = Ord.lessEq
  lessEqSkolem = Ord.lessEqSkolem

instance Function f => Arbitrary (Model f) where
  arbitrary = fmap (modelFromOrder . map Variable . nub) arbitrary
  shrink = weakenModel

{-
prop_1 :: Model Func -> Pair Func -> Subst Func -> Property
prop_1 model (Pair t u) sub =
  counterexample ("Model: " ++ prettyShow model) $
  counterexample ("Subst: " ++ prettyShow sub) $
  conjoin $ do
    let cp = CriticalPair (t :=: u) 0 Nothing (axiom (Axiom 0 "dummy" (t :=: u)))
    r@(Rule _ t' u') <- map orient (map cp_eqn (split cp))
    return $
      counterexample ("LHS:   " ++ prettyShow t') $
      counterexample ("RHS:   " ++ prettyShow u') $
      counterexample ("Rule:  " ++ prettyShow r) $
      counterexample ("Inst:  " ++ prettyShow (Rule Oriented (subst sub t') (subst sub u'))) $
      counterexample ("Res:   " ++ show (lessIn model (subst sub u') (subst sub t'))) $
      not (reducesInModel model r sub) || isJust (lessIn model (subst sub u') (subst sub t'))
-}

prop_2 :: Model Func -> Pair Func -> Bool
prop_2 model (Pair t u) =
  not (lessIn model t u == Just Strict && isJust (lessIn model u t))

prop_3 :: Pair Func -> Bool
prop_3 (Pair t u) =
  not (lessThan t u && lessEq u t)

prop_4 :: Pair Func -> Property
prop_4 (Pair t u) =
  t /= u ==>
  not (lessEq t u && lessEq u t)

prop_5 :: Term Func -> Property
prop_5 t =
  lessEq t t .&&. not (lessThan t t)

prop_paths :: Term Func -> Property
prop_paths t =
  forAllShrink (choose (0, len t-1)) shrink $ \n ->
    counterexample (show (positionToPath t n)) $
    pathToPosition t (positionToPath t n) === n

prop_index :: [Term Func] -> Term Func -> Property
prop_index ts u =
  counterexample (show ts') $
  counterexample (show idx) $
  sort (catMaybes [fmap (,t) (match t u) | t <- ts']) ===
  sort (Index.matches u idx)
  where
    idx = foldr (\t -> Index.insert t t) Index.empty ts
    ts' = map canonicalise ts

newtype Terms f = Terms [Term f] deriving Show
instance (Labelled f, Ord f, Typeable f, Arbitrary f, Arity f) => Arbitrary (Terms f) where
  arbitrary = Terms <$> arbitrary
  shrink (Terms ts) =
    map Terms $
      filter (/= ts) $
      shrink ts ++ [canonicalise ts] ++ shrinkList (return . canonicalise) ts

newtype IndexOps f = IndexOps [IndexOp f] deriving Show
data IndexOp f = Add (Term f) | Delete (Term f) deriving Show

instance (Labelled f, Ord f, Typeable f, Arbitrary f, Arity f) => Arbitrary (IndexOps f) where
  arbitrary =
    sized $ \n -> IndexOps . take n <$> arbOps []
    where
      arbOps ts =
        frequency $
          (2, do { t <- arbitrary; ops <- arbOps (t:ts); return (Add t:ops) }) :
          [(1, do { t <- elements ts; ops <- arbOps (delete t ts); return (Delete t:ops) }) | not (null ts)]
  shrink (IndexOps ops) =
    IndexOps <$> shrinkList shr ops
    where
      shr (Add t)    = Add <$> shrink t
      shr (Delete t) = Delete <$> shrink t


prop_index_invariant :: IndexOps Func -> Property
prop_index_invariant (IndexOps ops) =
  flip (foldr (counterexample . show)) idxs $
  property $ Index.invariant (last idxs)
  where
    idxs = scanl (flip applyIndex) Index.empty ops
    applyIndex (Add t)    = Index.insert t t
    applyIndex (Delete t) = Index.delete t t

deriving instance Eq Symbol
deriving instance Generic Symbol

instance Arbitrary Symbol where
  arbitrary =
    Symbol <$>
      arbitrary <*>
      fmap getLarge arbitrary <*>
      (fmap (fromIntegral . getLarge) (arbitrary :: Gen (Large Int32)) `suchThat` (> 0) `suchThat` (< 2^31))
  shrink s =
    filter ok (genericShrink s)
    where
      ok s = Twee.Term.Core.size s > 0

prop_symbol_1 :: Symbol -> Property
prop_symbol_1 s =
  withMaxSuccess 100000 $
  counterexample ("fun/index/size = " ++ show (isFun s, index s, Twee.Term.Core.size s)) $
  counterexample ("n = " ++ show (fromSymbol s)) $
  toSymbol (fromSymbol s) === twiddle s
  where
    twiddle s =
      s { index = fromIntegral (fromIntegral (index s) :: Int32) }

prop_symbol_2 :: Int64 -> Property
prop_symbol_2 n =
  withMaxSuccess 100000 $
  fromSymbol (toSymbol n) === n

prop_canonorder :: Equation Func -> Property
prop_canonorder eqn@(t :=: u) =
  let vs = usort (vars eqn) in
  forAll (shuffle vs) $ \ws swap (NonNegative n) ->
    let
      Just sub = listToSubst (zip vs [build (var (V (w + n))) | V w <- ws])
      eqn' = subst sub (if swap then u :=: t else t :=: u)
    in
      canonicalise (order eqn) === canonicalise (order eqn')

prop_canonorder2 :: Equation Func -> Equation Func -> Bool
prop_canonorder2 eqn1 eqn2 =
  eqn1 `simplerThan` eqn2 || eqn2 `simplerThan` eqn1 || order eqn1 == order eqn2

prop_canonorder3 :: Equation Func -> Property
prop_canonorder3 eq =
  let eq' = order eq in
  counterexample (show eq) $
  Ord.size (eqn_lhs eq') >= Ord.size (eqn_rhs eq')

return []
main = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 1000000 })

--t :: Term Func
--t = build (app (fun (F 0)) [app (fun (F 1)) [var (V 0), var (V 1)], var (V 2)])
