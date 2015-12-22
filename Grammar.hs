{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Grammar where

import Generics.SOP
import Generics.SOP.Dict
import Data.Singletons.Prelude hiding (All)
import List (Find)

class (Generic t, All2 (Find ts) (Code t)) => Contained ts t
instance (Generic t, All2 (Find ts) (Code t)) => Contained ts t

type Complete p g = Dict (All (Contained (p :++ g))) g

--blah :: Complete ts (p :++ g) -> Dict (All c) p -> (forall t. Dict (Contained g (p :++ g)) t -> Dict c t) -> Dict (All c) g
--blah complete allP f = all_NP $ _
