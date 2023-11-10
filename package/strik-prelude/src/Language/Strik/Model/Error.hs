module Language.Strik.Model.Error where

import           Language.Strik.Prelude

import qualified Language.Strik.Model.ErrorCode as ErrorCode

type T = Error

data Error = Error
    {
        getCode      :: ErrorCode.T,
        getDetail    :: Text,
        getPosition  :: Maybe (), -- TODO
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
