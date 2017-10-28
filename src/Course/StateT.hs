{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    forall a b.
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  fn <$> StateT sfa = StateT res where
    res :: s -> f (b, s)
    res s = (\(a, s') -> (fn a, s')) <$> (sfa s)

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure ::
    forall a.
    a -> StateT s f a
  pure x = StateT (\s -> pure (x, s))
  (<*>) ::
    forall a b.
    StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  StateT sfn <*> StateT sfa = StateT res where
    mapFas :: (a -> b) -> f (a, s) -> f (b, s)
    mapFas fn fas = (\(x, s) -> (fn x, s)) <$> fas

    mapAbs :: ((a -> b), s) -> f (b, s)
    mapAbs (fn, s) = mapFas fn (sfa s)

    res :: s -> f (b, s)
    res s = (sfn s) >>= mapAbs

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    forall a b.
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  fn =<< StateT sfa = StateT res where
    res :: s -> f (b, s)
    res s = helper1 (sfa s)

    helper1 :: f (a, s) -> f (b, s)
    helper1 fas = helper2 =<< fas

    helper2 :: (a, s) -> f (b, s)
    helper2 (x, s) = helper3 s (fn x)

    helper3 :: s -> StateT s f b -> f (b, s)
    helper3 s (StateT sfb) = sfb s

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' ::
  forall a s.
  (s -> (a, s))
  -> State' s a
state' fn = StateT helper where
  helper :: s -> ExactlyOne (a, s)
  helper s = ExactlyOne (fn s)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT fs) s = runExactlyOne (fs s)

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  forall s f a.
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT sfa) s = helper (sfa s) where
  helper :: f (a, s) -> f s
  helper fas = snd <$> fas

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' st s = runExactlyOne (execT st s)

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  forall s f a.
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT sfa) s = helper (sfa s) where
  helper :: f (a, s) -> f a
  helper fas = fst <$> fas

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' st s = runExactlyOne (evalT st s)

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  forall s f.
  Applicative f =>
  StateT s f s
getT = StateT helper where
  helper :: s -> f (s, s)
  helper s = pure (s, s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  forall s f.
  Applicative f =>
  s
  -> StateT s f ()
putT s = StateT helper where
  helper :: s -> f ((), s)
  helper _ = pure ((), s)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  forall a.
  (Ord a, Num a) =>
  List a
  -> List a
distinct' xs = eval' (helper1 xs) S.empty where
  helper1 :: List a -> State' (S.Set a) (List a)
  helper1 = filtering helper2

  helper2 :: a -> State' (S.Set a) Bool
  helper2 x = state' (helper3 x)

  helper3 :: a -> S.Set a -> (Bool, S.Set a)
  helper3 x s = if S.member x s
    then (False, s)
    else (True, S.insert x s)

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  forall a.
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF = distinctF' lte100 where
  lte100 :: a -> Bool
  lte100 x = x <= 100

  distinctF' :: (a -> Bool) -> List a -> Optional (List a)
  distinctF' validator xs = evalT (helper1 xs) S.empty where
    helper1 :: List a -> StateT (S.Set a) Optional (List a)
    helper1 = filtering helper2

    helper2 :: a -> StateT (S.Set a) Optional Bool
    helper2 x = StateT (helper3 x)

    helper3 :: a -> S.Set a -> Optional (Bool, S.Set a)
    helper3 x s = if not (validator x) then Empty else
      if S.member x s
      then Full (False, s)
      else Full (True, S.insert x s)



-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) ::
    forall a b.
    (a -> b) ->
    OptionalT f a ->
    OptionalT f b
  f <$> (OptionalT foa) = OptionalT (helper <$> foa) where
    helper :: Optional a -> Optional b
    helper = ((<$>) f)

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Applicative f => Applicative (OptionalT f) where
  pure :: a -> OptionalT f a
  pure x = OptionalT (pure (pure x))

  (<*>) :: forall a b. OptionalT f (a -> b) -> OptionalT f a -> OptionalT f b
  (OptionalT f) <*> (OptionalT x) = OptionalT (helper1 f x) where
    helper1 :: f (Optional (a -> b)) -> f (Optional a) -> f (Optional b)
    helper1 f' x' = pure helper2 <*> f' <*> x' where

    helper2 :: Optional (a -> b) -> Optional a -> Optional b
    helper2 = (<*>)

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) :: forall a b. (a -> OptionalT f b) -> OptionalT f a -> OptionalT f b
  f =<< OptionalT foa = OptionalT (helper1 foa) where
    helper1 :: f (Optional a) -> f (Optional b)
    helper1 x = helper2 =<< x

    helper2 :: Optional a -> f (Optional b)
    helper2 Empty = pure Empty
    helper2 (Full x') = runOptionalT (f x')

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) :: (a -> b) -> Logger l a -> (Logger l b)
  f <$> (Logger xs x) = Logger xs (f x)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure :: a -> Logger l a
  pure x = Logger Nil x

  (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
  (Logger fs f) <*> (Logger xs x) = Logger (fs ++ xs) (f x)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) :: forall a b. (a -> Logger l b) -> Logger l a -> Logger l b
  f =<< (Logger xs x) = let Logger ys y = f x in
    Logger (xs ++ ys) y

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 xs x = Logger (xs :. Nil) x

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  forall a.
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG xs = runOptionalT helper1 where
  helper1 :: OptionalT (Logger Chars) (List a)
  helper1 = evalT helper2 S.empty

  helper2 :: StateT (S.Set a) (OptionalT (Logger Chars)) (List a)
  helper2 = filtering helper3 xs

  helper3 :: a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
  helper3 x = StateT helper4 where
    helper4 :: S.Set a -> OptionalT (Logger Chars) (Bool, S.Set a)
    helper4 s = if x > 100
      then OptionalT helper5
      else OptionalT helper6
      where
        helper5 :: Logger Chars (Optional (Bool, S.Set a))
        helper5 = log1 ("aborting > 100: " ++ (show' x)) Empty

        helper6 :: Logger Chars (Optional (Bool, S.Set a))
        helper6 = if even x
          then log1 ("even number: " ++ (show' x)) (Full helper7)
          else Logger Nil (Full helper7)

        helper7 :: (Bool, S.Set a)
        helper7 = if S.member x s
          then (False, s)
          else (True, S.insert x s)
