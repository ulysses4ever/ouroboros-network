{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Ouroboros.Consensus.HardFork.Combinator.SingleEra.DerivingVia (
    LiftNS(..)
  , LiftNP(..)
  , LiftTelescope(..)
  , LiftNamedNS(..)
  , LiftNamedNP(..)
  , LiftNamedTelescope(..)
  ) where

import           Data.List (intercalate)
import           Data.Proxy
import           Data.SOP
import           Data.SOP.Dict
import           Data.SOP.NP
import           Data.Typeable
import           GHC.TypeLits

import           Cardano.Prelude (NoUnexpectedThunks (..), ThunkInfo (..),
                     allNoUnexpectedThunks)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

proofAll :: SListI xs
         => (forall x . Dict c x -> Dict d x)
         -> Dict (All c) xs -> Dict (All d) xs
proofAll f dict = all_NP (map_NP f (unAll_NP dict))

proofLift :: (SingleEraBlock x => c (f x))
          => Dict SingleEraBlock x -> Dict (Compose c f) x
proofLift Dict = Dict

liftEras :: (All SingleEraBlock xs, forall x. SingleEraBlock x => c (f x))
         => Proxy xs -> Proxy c -> Proxy f -> Dict (All (Compose c f)) xs
liftEras _ _ _ = proofAll proofLift Dict

{-------------------------------------------------------------------------------
  LiftNS
-------------------------------------------------------------------------------}

newtype LiftNS f xs = LiftNS (NS f xs)

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Eq (f x))
      => Eq (LiftNS f xs) where
  LiftNS x == LiftNS y =
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @f) of { Dict ->
          x == y
        }

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Ord (f x))
      => Ord (LiftNS f xs) where
  LiftNS x `compare` LiftNS y =
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @f) of { Dict ->
          x `compare` y
        }}

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Show (f x))
      => Show (LiftNS f xs) where
  show (LiftNS x) =
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @f) of { Dict ->
          show x
        }

{-------------------------------------------------------------------------------
  LiftNP
-------------------------------------------------------------------------------}

newtype LiftNP f xs = LiftNP (NP f xs)

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Eq (f x))
      => Eq (LiftNP f xs) where
  LiftNP x == LiftNP y =
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @f) of { Dict ->
          x == y
        }

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Ord (f x))
      => Ord (LiftNP f xs) where
  LiftNP x `compare` LiftNP y =
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @f) of { Dict ->
          x `compare` y
        }}

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Show (f x))
      => Show (LiftNP f xs) where
  show (LiftNP x) =
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @f) of { Dict ->
          show x
        }

{-------------------------------------------------------------------------------
  LiftTelescope
-------------------------------------------------------------------------------}

newtype LiftTelescope g f xs = LiftTelescope (Telescope g f xs)

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => Eq (g x)
         , forall x. SingleEraBlock x => Eq (f x)
         ) => Eq (LiftTelescope g f xs) where
  LiftTelescope x == LiftTelescope y =
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @g) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @f) of { Dict ->
          x == y
        }}

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => Ord (f x)
         , forall x. SingleEraBlock x => Ord (g x)
         ) => Ord (LiftTelescope g f xs) where
  compare (LiftTelescope x) (LiftTelescope y) =
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @g) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @g) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @f) of { Dict ->
          compare x y
        }}}}

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => Show (g x)
         , forall x. SingleEraBlock x => Show (f x)
         ) => Show (LiftTelescope g f xs) where
  show (LiftTelescope x) =
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @g) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @f) of { Dict ->
          show x
        }}

{-------------------------------------------------------------------------------
  LiftNamedNS
-------------------------------------------------------------------------------}

newtype LiftNamedNS (name :: Symbol) f xs = LiftNamedNS (NS f xs)

liftNamedNS :: Proxy name -> NS f xs -> LiftNamedNS name f xs
liftNamedNS _ = LiftNamedNS

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => NoUnexpectedThunks (f x)
         , KnownSymbol name
         ) => NoUnexpectedThunks (LiftNamedNS name f xs) where
  showTypeOf _ = symbolVal (Proxy @name) ++ " " ++ showBlockTypes (sList :: SList xs)

  whnfNoUnexpectedThunks ctxt = \(LiftNamedNS x) -> go x
    where
      go :: NS f xs -> IO ThunkInfo
      go (Z l) = noUnexpectedThunks ("Z" : ctxt) l
      go (S r) = noUnexpectedThunks ("S" : ctxt) (liftNamedNS (Proxy @name) r)

{-------------------------------------------------------------------------------
  LiftNamedNP
-------------------------------------------------------------------------------}

newtype LiftNamedNP (name :: Symbol) f xs = LiftNamedNP (NP f xs)

liftNamedNP :: Proxy name -> NP f xs -> LiftNamedNP name f xs
liftNamedNP _ = LiftNamedNP

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => NoUnexpectedThunks (f x)
         , KnownSymbol name
         ) => NoUnexpectedThunks (LiftNamedNP name f xs) where
  showTypeOf _ = symbolVal (Proxy @name) ++ " " ++ showBlockTypes (sList :: SList xs)

  whnfNoUnexpectedThunks ctxt = \(LiftNamedNP x) -> go x
    where
      go :: NP f xs -> IO ThunkInfo
      go Nil       = return NoUnexpectedThunks
      go (x :* xs) = allNoUnexpectedThunks [
                          noUnexpectedThunks ("fst" : ctxt) x
                        , noUnexpectedThunks ("snd" : ctxt) (liftNamedNP (Proxy @name) xs)
                        ]

{-------------------------------------------------------------------------------
  LiftNamedTelescope
-------------------------------------------------------------------------------}

newtype LiftNamedTelescope (name :: Symbol) f g xs = LiftNamedTelescope (Telescope f g xs)

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => NoUnexpectedThunks (f x)
         , forall x. SingleEraBlock x => NoUnexpectedThunks (g x)
         , KnownSymbol name
         ) => NoUnexpectedThunks (LiftNamedTelescope name f g xs) where
  showTypeOf _ = symbolVal (Proxy @name) ++ " " ++ showBlockTypes (sList :: SList xs)

  whnfNoUnexpectedThunks ctxt (LiftNamedTelescope x) =
      case liftEras (Proxy @xs) (Proxy @NoUnexpectedThunks) (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @NoUnexpectedThunks) (Proxy @g) of { Dict ->
          whnfNoUnexpectedThunks ctxt x
        }}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

showBlockTypes :: All SingleEraBlock xs => SList xs -> String
showBlockTypes =
    (\names -> "[" ++ intercalate "," names ++ "]") . hcollapse . go
  where
    go :: All SingleEraBlock xs' => SList xs' -> NP (K String) xs'
    go SNil  = Nil
    go SCons = typeRep' :* go sList

    typeRep' :: forall blk. SingleEraBlock blk => K String blk
    typeRep' = K . show $ typeRep (Proxy @blk)
