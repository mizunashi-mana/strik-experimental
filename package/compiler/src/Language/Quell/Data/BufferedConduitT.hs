module Language.Quell.Data.BufferedConduitT (
    BufferedConduitT (..),
    runConduitT,
    Context (..),
    BufferMode (..),
    await,
    yield,
    getCurrentPosition,
    seekToPosition,
    setBufferMode,
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Data.Monad.MonadST           as MonadST
import qualified Language.Quell.Data.STBuffer                as STBuffer


newtype BufferedConduitT s i o m a = BufferedConduitT
    { unBufferedConduitT :: Conduit.ConduitT i o (StateT (Context s i) m) a
    }
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadIO
    ) via Conduit.ConduitT i o (StateT (Context s i) m)

instance MonadST.T s m => MonadST.MonadST s (BufferedConduitT s i o m) where
    type Marker (BufferedConduitT s i o m) = MonadST.Marker m
    liftST mx = BufferedConduitT do Conduit.lift do MonadST.liftST mx

runConduitT :: MonadST.T s m => BufferedConduitT s i o m a -> Conduit.ConduitT i o m a
runConduitT m = do
    ictx <- Conduit.lift do MonadST.liftST buildInitialContext
    Conduit.evalStateC ictx do unBufferedConduitT m
    where
        buildInitialContext :: ST s (Context s i)
        buildInitialContext = do
            emptyBuffer <- STBuffer.new []
            pure do
                Context
                    { bufferMode = NoBack
                    , buffer = emptyBuffer
                    , currentPosition = 0
                    }

-- | Buffer Context
--
-- required:
-- * @@bufferMode == NoBack@@ implies the head of @@buffer@@ is the current item.
-- * @@bufferMode == NeedBack i && currentPosition < i@@ implies the head of @@buffer@@ is the current item.
-- * @@bufferMode == NeedBack i && i <= currentPosition@@ implies the head of @buffer@@ is the item at @@i@@.
--
data Context s i = Context
    { bufferMode :: BufferMode
    , buffer :: STBuffer.T s i
    , currentPosition :: Int
    }

data BufferMode
    = NoBack
    | NeedBack Int
    deriving (Eq, Show)

await :: forall s i o m. MonadST.T s m => BufferedConduitT s i o m (Maybe i)
await = consumeBufferItem >>= \case
    mitem@Just{} ->
        pure mitem
    Nothing -> do
        ctx <- getContext
        case bufferMode ctx of
            NoBack ->
                awaitNewItem
            NeedBack needBack
                | currentPosition ctx < needBack ->
                    awaitNewItem
                | otherwise -> do
                    awaitAndPushBufferItem
    where
        awaitNewItem :: BufferedConduitT s i o m (Maybe i)
        awaitNewItem = do
            mitem <- BufferedConduitT Conduit.await
            case mitem of
                Nothing ->
                    pure mitem
                Just{} -> modifyContext \ctx -> do
                    let newCtx = ctx
                            { currentPosition = currentPosition ctx + 1
                            }
                    pure (newCtx, mitem)

        awaitAndPushBufferItem :: BufferedConduitT s i o m (Maybe i)
        awaitAndPushBufferItem = do
            mitem <- BufferedConduitT Conduit.await
            case mitem of
                Nothing ->
                    pure mitem
                Just item -> modifyContext \ctx -> do
                    STBuffer.appendLast item
                        do buffer ctx
                    let newCtx = ctx
                            { currentPosition = currentPosition ctx + 1
                            }
                    pure (newCtx, mitem)

yield :: Monad m => o -> BufferedConduitT s i o m ()
yield x = BufferedConduitT do Conduit.yield x

getCurrentPosition :: Monad m => BufferedConduitT s i o m Int
getCurrentPosition = do
    ctx <- getContext
    pure do currentPosition ctx

seekToPosition :: MonadST.T s m => Int -> BufferedConduitT s i o m ()
seekToPosition i = do
    ctx <- getContext
    case bufferMode ctx of
        NoBack
            | i < currentPosition ctx ->
                error "Cannot back on no back mode."
            | otherwise -> do
                consumeInputs do i - currentPosition ctx
                setPosition
        NeedBack needBack
            | i < currentPosition ctx -> if
                | i < needBack ->
                    error "Cannot back before buffered position."
                | otherwise ->
                    setPosition
            | otherwise -> do
                consumeInputs do needBack - currentPosition ctx
                bufferLength <- MonadST.liftST do STBuffer.length do buffer ctx
                consumeAndBufferInputs do i - (needBack + bufferLength)
                setPosition
    where
        consumeInputs c = if
            | c <= 0 ->
                pure ()
            | otherwise -> BufferedConduitT Conduit.await >>= \case
                Nothing ->
                    pure ()
                Just{} ->
                    consumeInputs do c - 1

        consumeAndBufferInputs c = if
            | c <= 0 ->
                pure ()
            | otherwise -> BufferedConduitT Conduit.await >>= \case
                Nothing ->
                    pure ()
                Just item -> do
                    modifyContext \ctx -> do
                        STBuffer.appendLast item do buffer ctx
                        pure (ctx, ())
                    consumeAndBufferInputs do c - 1

        setPosition = modifyContext \ctx -> do
            let newCtx = ctx
                    { currentPosition = i
                    }
            pure (newCtx, ())

setBufferMode :: MonadST.T s m => BufferMode -> BufferedConduitT s i o m ()
setBufferMode mode = case mode of
    NoBack -> modifyContext \ctx -> case bufferMode ctx of
        NoBack ->
            pure (ctx, ())
        NeedBack needBack
            | needBack <= currentPosition ctx -> do
                let consumeCount = currentPosition ctx - needBack
                _ <- MonadST.liftST
                    do STBuffer.consumeHeads ()
                        do \_ _ -> ()
                        do buffer ctx
                        do consumeCount
                let newCtx = ctx
                        { bufferMode = mode
                        }
                pure (newCtx, ())
            | otherwise -> do
                let newCtx = ctx
                        { bufferMode = mode
                        }
                pure (newCtx, ())
    NeedBack newNeedBack -> modifyContext \ctx -> case bufferMode ctx of
        NoBack
            | newNeedBack < currentPosition ctx ->
                error "Cannot back before buffered position."
            | otherwise -> do
                let newCtx = ctx
                        { bufferMode = mode
                        }
                pure (newCtx, ())
        NeedBack needBack
            | needBack <= currentPosition ctx -> if
                | newNeedBack < needBack ->
                    error "Cannot back before buffered position."
                | otherwise -> do
                    let consumeCount = if
                            | currentPosition ctx < newNeedBack ->
                                currentPosition ctx - needBack
                            | otherwise ->
                                newNeedBack - needBack
                    _ <- MonadST.liftST
                        do STBuffer.consumeHeads ()
                            do \_ _ -> ()
                            do buffer ctx
                            do consumeCount
                    let newCtx = ctx
                            { bufferMode = mode
                            }
                    pure (newCtx, ())
            | newNeedBack < currentPosition ctx ->
                error "Cannot back before buffered position."
            | otherwise -> do
                let newCtx = ctx
                        { bufferMode = mode
                        }
                pure (newCtx, ())


consumeBufferItem :: forall s i o m. MonadST.T s m => BufferedConduitT s i o m (Maybe i)
consumeBufferItem = do
    ctx <- getContext
    case bufferMode ctx of
        NoBack ->
            consumeHeadItem
        NeedBack needBack
            | currentPosition ctx < needBack ->
                consumeHeadItem
            | otherwise -> do
                let bufferPosition = currentPosition ctx - needBack
                refBufferItem bufferPosition
    where
        consumeHeadItem :: BufferedConduitT s i o m (Maybe i)
        consumeHeadItem = modifyContext \ctx -> do
            mitem <- STBuffer.consumeHead do buffer ctx
            case mitem of
                Nothing ->
                    pure (ctx, mitem)
                Just{} -> do
                    let newCtx = ctx
                            { currentPosition = currentPosition ctx + 1
                            }
                    pure (newCtx, mitem)

        refBufferItem :: Int -> BufferedConduitT s i o m (Maybe i)
        refBufferItem bufferPosition = modifyContext \ctx -> do
            mitem <- buffer ctx `STBuffer.index` bufferPosition
            case mitem of
                Nothing ->
                    pure (ctx, mitem)
                Just{} -> do
                    let newCtx = ctx
                            { currentPosition = currentPosition ctx + 1
                            }
                    pure (newCtx, mitem)

getContext :: Monad m => BufferedConduitT s i o m (Context s i)
getContext = BufferedConduitT do Conduit.lift get

modifyContext :: MonadST.T s m => (Context s i -> ST s (Context s i, r)) -> BufferedConduitT s i o m r
modifyContext f = BufferedConduitT do
    oldCtx <- Conduit.lift get
    (newCtx, res) <- Conduit.lift do MonadST.liftST do f oldCtx
    Conduit.lift do put newCtx
    pure res
