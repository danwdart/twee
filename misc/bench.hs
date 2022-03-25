{-# LANGUAGE FlexibleInstances #-}
import           Criterion.Main
import           Data.Int
import           Data.Maybe
import           Test.QuickCheck hiding (Fun)
import           Twee.Term       hiding (F, isFun)
import qualified Twee.Term
import           Twee.Term.Core  hiding (subst)

instance Num (Fun Int) where fromInteger n = F (fromInteger n)
instance Num Var where fromInteger = V . fromInteger

t0, t1, u0, u1, t2, t, u :: Term Int
t0 = build $ app 0 [var 0, app 0 [var 0, app 0 [app 0 [var 0, var 1], var 2]]]
u0 = build $ app 0 [app 0 [app 2 [app 2 [var 2, var 2], var 1], app 0 [app 2 [var 2, var 2], var 3]], app 0 [app 0 [app 2 [app 2 [var 2, var 2], var 1], app 0 [app 2 [var 2, var 2], var 3]], app 0 [app 0 [app 0 [app 2 [app 2 [var 2, var 2], var 1], app 0 [app 2 [var 2, var 2], var 3]], app 2 [app 2 [var 2, var 2], var 1]], app 2 [var 2, var 2]]]]

t1 = build $ app 0 [app 1 [var 0], app 1 [var 1]]
u1 = build $ app 0 [app 1 [app 0 [app 2 empty, app 3 empty]], app 1 [app 0 [app 4 empty, app 5 empty]]]

t2 = build $ app 0 [var 0, app 1 [var 1, app 1 [var 1, var 1]]]
u2 = build $ app 0 [app 0 [var 2, var 2], var 2]

t = t0
u = u0

Just sub = match t u

mgu1 t u = let Just sub = unifyTri t u in build (subst sub t)
mgu2 t u = let Just sub = unify t u in build (subst sub t)

Just sub' = unifyTri t2 u2
Just csub' = unify t2 u2

main = do
  print t
  print u
  print (match t u)
  print (build (subst sub t))
  print (unifyTri t2 u2)
  print (close sub')
  print (build (subst sub' t2))
  print (build (subst sub' u2))
  print (mgu1 t2 u2)
  print (mgu2 t2 u2)
  print (t == t)
  print (build (subst sub t) == u)
  print (build (subst sub' t2) == build (subst sub' u2))
  print (build (subst csub' t1) == build (subst sub' t1))
  print (mgu1 t2 u2 == mgu2 t2 u2)
  print (build (subst csub' t2) == build (subst sub' t2))
  defaultMain [
    bench "eq-t" (whnf (uncurry (==)) (t, t)),
    bench "eq-u" (whnf (uncurry (==)) (u, u)),
    bench "match" (whnf (fromJust . uncurry match) (t, u)),
    bench "subst" (whnf (build . uncurry subst) (sub, t)),
    bench "unifyTri" (whnf (fromJust . uncurry unifyTri) (t2, u2)),
    bench "unify-close" (whnf (uncurry unify) (t2, u2)),
    bench "unify-subst-iter1" (whnf (build . uncurry subst) (sub', t2)),
    bench "unify-subst-iter2" (whnf (build . uncurry subst) (sub', u2)),
    bench "unify-subst-closed1" (whnf (build . uncurry subst) (csub', t2)),
    bench "unify-subst-closed2" (whnf (build . uncurry subst) (csub', u2)),
    bench "mgu-tri" (whnf (uncurry mgu1) (t2, u2)),
    bench "mgu-close" (whnf (uncurry mgu2) (t2, u2)),
    bench "make-constant" (whnf (build . uncurry app) (F 0, empty)),
    bench "baseline" (whnf (uncurry (+)) (0 :: Int, 0))]

prop :: Bool -> NonNegative (Small Int) -> NonNegative (Small Int) -> Property
prop fun_ (NonNegative (Small index_)) (NonNegative (Small size_)) =
  (isFun x, index x, size x) === (fun_, index_, size_)
  where
    x = toSymbol (fromSymbol (Symbol fun_ index_ size_))

prop2 :: Int64 -> Property
prop2 x = fromSymbol (toSymbol x) === x
