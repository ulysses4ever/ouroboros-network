{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Intended for qualified import
--
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Telescope as Telescope
module Ouroboros.Consensus.HardFork.Combinator.Util.Telescope (
    -- * Telescope
    Telescope(..)
    -- ** Conversions
  , toNS
  , toAtMost
    -- ** Type utilities
  , nonEmpty
  , empty
    -- ** Bifunctor analogues of SOP functions
  , bihap
  , bihctraverse'
  , bihtraverse'
  , bihsequence'
  , bihctraverse_
  , bihtraverse_
  , bihliftA
  , bihmap
  , hfirst
  , hsecond
    -- * SimpleTelescope
  , SimpleTelescope(..)
    -- * Telescope specific operations
  , Extend(..)
  , extend
  , Align(..)
  , align
  , UpdateTip(..)
  , RestoreTip(..)
  , retract
  ) where

import           Prelude hiding (sequence, zipWith)

import           Control.Applicative ((<|>))
import           Data.Kind
import           Data.SOP
import           Data.SOP.Constraint
import           Data.Void
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     allNoUnexpectedThunks)
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))

{-------------------------------------------------------------------------------
  Telescope
-------------------------------------------------------------------------------}

data Telescope (g :: k -> Type) (f :: k -> Type) (xs :: [k]) where
  TZ :: f x ->                     Telescope g f (x ': xs)
  TS :: g x -> Telescope g f xs -> Telescope g f (x ': xs)

{-------------------------------------------------------------------------------
  SOP class instances for 'Telescope'
-------------------------------------------------------------------------------}

type instance Prod    (Telescope g)   = NP
type instance SListIN (Telescope g)   = SListI
type instance AllN    (Telescope g) c = All c

instance HAp (Telescope g) where
  hap = flip go
    where
      -- We could define this in terms of 'bimap' but we lack 'SListI'
      go :: Telescope g f xs -> NP (f -.-> f') xs -> Telescope g f' xs
      go (TZ fx)   (f :* _)  = TZ (apFn f fx)
      go (TS gx t) (_ :* fs) = TS gx (go t fs)

instance HTraverse_ (Telescope g) where
  hctraverse_ p = bihctraverse_ p (\_ -> pure ())
  htraverse_    = bihtraverse_    (\_ -> pure ())

instance HSequence (Telescope g) where
  hsequence'    = bihsequence' . bihliftA (Comp . pure) id
  hctraverse' p = bihctraverse' p pure
  htraverse'    = bihtraverse'    pure

{-------------------------------------------------------------------------------
  Bifunctor analogues of class methods
-------------------------------------------------------------------------------}

-- | Bifunctor analogue of 'hap'
bihap :: NP (g -.-> g') xs
      -> NP (f -.-> f') xs
      -> Telescope g f xs -> Telescope g' f' xs
bihap = \gs fs t -> go t gs fs
  where
    go :: Telescope g f xs
       -> NP (g -.-> g') xs
       -> NP (f -.-> f') xs
       -> Telescope g' f' xs
    go (TZ fx)   _         (f :* _)  = TZ (apFn f fx)
    go (TS gx t) (g :* gs) (_ :* fs) = TS (apFn g gx) (go t gs fs)

-- | Bifunctor analogue of 'hctraverse''
bihctraverse' :: forall proxy c m g g' f f' xs. (All c xs, Applicative m)
              => proxy c
              -> (forall x. c x => g x -> m (g' x))
              -> (forall x. c x => f x -> m (f' x))
              -> Telescope g f xs -> m (Telescope g' f' xs)
bihctraverse' _ g f = go
  where
    go :: All c xs' => Telescope g f xs' -> m (Telescope g' f' xs')
    go (TZ fx)   = TZ <$> f fx
    go (TS gx t) = TS <$> g gx <*> go t

-- | Bifunctor analogue of 'htraverse''
bihtraverse' :: (SListI xs, Applicative m)
             => (forall x. g x -> m (g' x))
             -> (forall x. f x -> m (f' x))
             -> Telescope g f xs -> m (Telescope g' f' xs)
bihtraverse' = bihctraverse' (Proxy @Top)

-- | Bifunctor analogue of 'hsequence''
bihsequence' :: forall m g f xs. (SListI xs, Applicative m)
             => Telescope (m :.: g) (m :.: f) xs -> m (Telescope g f xs)
bihsequence' = bihtraverse' unComp unComp

-- | Bifunctor analogue of 'hctraverse_'
bihctraverse_ :: forall proxy c xs m g f. (All c xs, Applicative m)
              => proxy c
              -> (forall x. c x => g x -> m ())
              -> (forall x. c x => f x -> m ())
              -> Telescope g f xs -> m ()
bihctraverse_ _ g f = go
  where
    go :: All c xs' => Telescope g f xs' -> m ()
    go (TZ fx)   = f fx
    go (TS gx t) = g gx *> go t

bihtraverse_ :: (SListI xs, Applicative m)
             => (forall x. g x -> m ())
             -> (forall x. f x -> m ())
             -> Telescope g f xs -> m ()
bihtraverse_ = bihctraverse_ (Proxy @Top)

{-------------------------------------------------------------------------------
  Bifunctor analogues of derived functions
-------------------------------------------------------------------------------}

-- | Bifunctor analogue of 'hliftA' (== 'hmap')
bihliftA :: SListI xs
         => (forall x. g x -> g' x)
         -> (forall x. f x -> f' x)
         -> Telescope g f xs -> Telescope g' f' xs
bihliftA g f = bihap (hpure (fn g)) (hpure (fn f))

-- | Another name for 'bihliftA'
--
-- SOP provides 'hmap' as an alias for 'hliftA'. For consistency we do the same.
bihmap :: SListI xs
       => (forall x. g x -> g' x)
       -> (forall x. f x -> f' x)
       -> Telescope g f xs -> Telescope g' f' xs
bihmap = bihliftA

-- | Higher order equivalent of 'Data.Bifunctor.first'
hfirst :: SListI xs
       => (forall x. g x -> g' x) -> Telescope g f xs -> Telescope g' f xs
hfirst f = bihmap f id

-- | Higher order equivalent of 'Data.Bifunctor.second'
hsecond :: SListI xs
        => (forall x. f x -> f' x) -> Telescope g f xs -> Telescope g f' xs
hsecond = bihmap id

{-------------------------------------------------------------------------------
  Simple telescope

  Primarily useful as a sanity check of our bifunctor generalizations.
-------------------------------------------------------------------------------}

-- | 'Telescope' with both functors set to the same @f@
newtype SimpleTelescope f xs = SimpleTelescope {
      getSimpleTelescope :: Telescope f f xs
    }

{-------------------------------------------------------------------------------
  SOP class instances for 'SimpleTelescope'
-------------------------------------------------------------------------------}

type instance Prod    SimpleTelescope   = NP
type instance SListIN SimpleTelescope   = SListI
type instance AllN    SimpleTelescope c = All c

instance HAp SimpleTelescope where
  hap fs = SimpleTelescope . bihap fs fs . getSimpleTelescope

instance HTraverse_ SimpleTelescope where
  hctraverse_ p f = bihctraverse_ p f f . getSimpleTelescope
  htraverse_    f = bihtraverse_    f f . getSimpleTelescope

instance HSequence SimpleTelescope where
  hsequence'      = fmap SimpleTelescope . bihsequence'        . getSimpleTelescope
  hctraverse' p f = fmap SimpleTelescope . bihctraverse' p f f . getSimpleTelescope
  htraverse'    f = fmap SimpleTelescope . bihtraverse'    f f . getSimpleTelescope

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

toNS :: Telescope g f xs -> NS f xs
toNS (TZ   l) = Z l
toNS (TS _ r) = S (toNS r)

toAtMost :: Telescope (K a) (K (Maybe a)) xs -> AtMost xs a
toAtMost = go
  where
    go :: Telescope (K a) (K (Maybe a)) xs -> AtMost xs a
    go (TZ (K ma))  = maybe AtMostNil atMostOne ma
    go (TS (K a) t) = AtMostCons a (go t)

{-------------------------------------------------------------------------------
  Type utilities
-------------------------------------------------------------------------------}

nonEmpty :: Telescope g f xs -> (forall x xs'. xs ~ (x ': xs') => a) -> a
nonEmpty TZ{} k = k
nonEmpty TS{} k = k

empty :: Telescope g f '[] -> Void
empty t = case t of {}

{-------------------------------------------------------------------------------
  Extending the telescope
-------------------------------------------------------------------------------}

data Extend g f x y = Extend { extendWith :: f x -> Maybe (g x, f y) }

extend :: InPairs (Extend g f) xs -> Telescope g f xs -> Telescope g f xs
extend = go
  where
    go :: InPairs (Extend g f) xs' -> Telescope g f xs' -> Telescope g f xs'
    go PNil         (TZ fx)   = TZ fx
    go (PCons _ fs) (TS gx t) = TS gx (go fs t)
    go (PCons f _)  (TZ fx)   = case extendWith f fx of
                                  Nothing       -> TZ fx
                                  Just (gx, fy) -> TS gx (TZ fy)
    go PNil         (TS _ t)  = absurd (empty t)

{-------------------------------------------------------------------------------
  Aligning
-------------------------------------------------------------------------------}

data Align g f x y = Align { alignWith :: f x -> (g x, f y) }

-- | Align 'Telescope' with an 'NS'
--
-- Compare
--
-- > hap :: NP (f -.-> f') xs -> Telescope g f xs -> Telescope g f' xs
--
-- Precondition: the 'Telescope' cannot be ahead of the 'NS'.
align :: HasCallStack
      => InPairs (Align g f) xs
      -> NS (f -.-> f') xs -> Telescope g f xs -> Telescope g f' xs
align = flip . go
  where
    go :: InPairs (Align g f) xs
       -> Telescope g f xs
       -> NS (f -.-> f') xs
       -> Telescope g f' xs
    go _            (TZ fx)   (Z f) = TZ (apFn f fx)
    go (PCons _ as) (TS gx t) (S f) = TS gx $ go as t f
    go (PCons a as) (TZ fx)   (S f) = let (gx, fy) = alignWith a fx
                                      in TS gx $ go as (TZ fy) f
    go _            (TS _ _)  (Z _) = error "align: precondition violated"
    go PNil         _         (S f) = absurd (emptySum f)

{-------------------------------------------------------------------------------
  Retraction
-------------------------------------------------------------------------------}

newtype UpdateTip    f x = UpdateTip  { updateTipWith  :: f x -> Maybe (f x) }
newtype RestoreTip g f x = RestoreTip { restoreTipWith :: g x -> Maybe (f x) }

retract :: NP (UpdateTip f) xs
        -> NP (RestoreTip g f) xs
        -> Telescope g f xs -> Maybe (Telescope g f xs)
retract = \upd re t -> go t upd re
  where
    go :: Telescope g f xs
       -> NP (UpdateTip f) xs
       -> NP (RestoreTip g f) xs
       -> Maybe (Telescope g f xs)
    go (TZ fx)   (f :* _)  _         = TZ <$> updateTipWith f fx
    go (TS gx t) (f :* fs) (g :* gs) = (TS gx <$> go t fs gs)
                                   <|> (do restored <- restoreTipWith g gx
                                           TZ <$> updateTipWith f restored)

{-------------------------------------------------------------------------------
  Standard type class instances
-------------------------------------------------------------------------------}

deriving stock instance ( All (Compose Eq g) xs
                        , All (Compose Eq f) xs
                        ) => Eq (Telescope g f xs)

deriving stock instance ( All (Compose Eq  g) xs
                        , All (Compose Ord g) xs
                        , All (Compose Eq  f) xs
                        , All (Compose Ord f) xs
                        ) => Ord (Telescope g f xs)

deriving stock instance ( All (Compose Show g) xs
                        , All (Compose Show f) xs
                        ) => Show (Telescope g f xs)

instance ( All (Compose NoUnexpectedThunks g) xs
         , All (Compose NoUnexpectedThunks f) xs
         ) => NoUnexpectedThunks (Telescope g f xs) where
  showTypeOf _ = "Telescope"
  whnfNoUnexpectedThunks ctxt = \case
      TZ f   -> noUnexpectedThunks ("TZ" : ctxt) f
      TS g t -> allNoUnexpectedThunks [
                    noUnexpectedThunks ("g" : "TS" : ctxt) g
                  , noUnexpectedThunks ("t" : "TS" : ctxt) t
                  ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

emptySum :: NS f '[] -> Void
emptySum ns = case ns of {}
