module Language.Quell.Model.Error where

import Language.Quell.Prelude

import qualified Language.Quell.Model.ErrorCode as ErrorCode

type T = Error

data Error = Error
    {
        getCode :: ErrorCode.T,
        getDetail :: Text,
        getPosition :: Maybe (), -- TODO
        getCallStack :: Maybe CallStack
    }
    deriving Show

build :: HasCallStack => ErrorCode.T -> StringLit -> Error
build code detail = Error
    {
        getCode = code,
        getDetail = text detail,
        getPosition = Nothing,
        getCallStack = Just callStack
    }
