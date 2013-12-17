{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Vinyl.Classes where

import           Control.Applicative
import           Data.Functor.Identity


-- | This class is a generalized version of 'Functor'.
-- 
class Category' (ApMorphism f) => ApFunctor (f :: (k -> *) -> *) where
    type ApMorphism f :: ((k -> *) -> (k -> *) -> k -> *)
    (<<$>>) :: forall (g :: k -> *) (h :: k -> *). 
              (forall (x :: k). (ApMorphism f) g h x) -> f g -> f h

-- | This class is a generalized version of 'Traversable'.
class (ApFunctor f) => ApTraversable (f :: (k -> *) -> *) where
    apTraverse :: forall (g :: k -> *) (h :: k -> *) (e :: * -> *).
                  (Applicative e) =>
                  (forall (x :: k). g x -> e (h x)) -> f g -> e (f h)

-- This class would impose a dependency on `semigroupoids`
-- class (ApTraversable f) => ApTraversable1 (f :: (k -> *) -> *) where
--    apTraverse1 :: forall (g :: k -> *) (h :: k -> *) (e :: * -> *). 
--                   (Apply e) => 
--                   (forall (x :: k). g x -> e (h x)) -> f g -> e (f h)

-- | To accumulate effects distributed over a data type, you can 'run' it.
-- For records, you can think of the type signature as
--
-- >>> run :: (Applicative f) => Rec rs f -> f (PlainRec rs)

run :: (Applicative f, ApTraversable t) => t f -> f (t Identity)
run = apTraverse (Identity <$>)

-- | This class is a generalized version of `Pointed`.
--
-- Like `Pointed`, this class has no laws. But if `f` is also an 
-- instance of `ApApply`, then `apPure` and `<<*>>` should together follow the Applicative 
-- laws (for some sensible definition of `id`).
class ApFunctor f => ApPointed (f :: (k -> *) -> *) where
    apPure :: forall (g :: k -> *). (forall x. g x) -> f g

-- | This class is a generalized, but non-pointed version of 'Applicative'. This
-- is useful for types which range over functors rather than sets.
--
-- If `f` is also an instance of `ApApply`, then `apPure` and `<<*>>` should together 
-- follow the Applicative laws (for some sensible definition of `id`).
class ApFunctor f => ApApply (f :: (k -> *) -> *) where
    (<<*>>) :: forall (g :: k -> *) (h :: k -> *). f (ApMorphism f g h) -> f g -> f h

type ApApplicative arr f = (ApPointed f, ApApply f)

class Category' (cat :: ((k -> *) -> (k -> *) -> (k -> *)))  where
    -- | the identity morphism
    id' :: cat a a x
    -- | morphism composition
    dot :: ((b `cat` c) `cat` ((a `cat` b) `cat` (a `cat` c))) x

-- | If a record is homogenous, you can fold over it.
class FoldRec r a where
  foldRec :: (a -> b -> b) -> b -> r -> b

-- | '(~>)' is a morphism between functors.
newtype (f ~> g) x = NT { runNT :: f x -> g x }
infixr ~>
instance Category' (~>) where
    dot = NT $ \(NT f) -> NT $ \(NT g) -> NT $ \x -> f (g x)
    id' = NT id


-- | `ApApplicative`s containing `Alternative` functors can be created and combined
-- using `apEmpty` and `<<|>>`.
apEmpty :: (Alternative g, ApPointed f) => f g
apEmpty = apPure empty

(<<|>>) :: (ApMorphism f ~ (~>), Alternative g, ApApply f) => f g -> f g -> f g
a <<|>> b = (NT $ (\x -> NT $ (x <|>))) <<$>> a <<*>> b      
