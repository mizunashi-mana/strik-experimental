module Language.Strik.Prelude.Debug (
    debugTrace,
    debugTraceShow,
    debugTraceShowId,
    require,
    requireM,
) where

import           Language.Strik.Prelude.Core
import           Language.Strik.Prelude.Literal

import qualified Control.Exception              as Exception
import qualified Debug.Trace                    as Debug


debugTrace :: StringLit -> a -> a
debugTrace msg x = Debug.trace msg x

debugTraceShow :: Show a => a -> b -> b
debugTraceShow v x = Debug.traceShow v x

debugTraceShowId :: Show a => a -> a
debugTraceShowId x = Debug.traceShowId x

require :: Bool -> a -> a
require t x = Exception.assert t x
{-# INLINE require #-}

requireM :: Monad m => Bool -> m ()
requireM t = require t do pure ()
{-# INLINE requireM #-}
