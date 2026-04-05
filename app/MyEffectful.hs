{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module MyEffectful
  ( ($$),
    ($.),
    withR,
  )
where

import Effectful
import Effectful.Reader.Static

($$) :: Eff (Reader r : es) a -> r -> Eff es a
m $$ r = runReader r m

($.) :: (Reader r1 :> es) => Eff (Reader r2 : es) a -> (r1 -> r2) -> Eff es a
m $. f = withR f m

withR :: (Reader r1 :> es) => (r1 -> r2) -> Eff (Reader r2 : es) a -> Eff es a
withR f = subsume . withReader f
