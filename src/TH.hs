{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}


module TH where

import Control.Monad
import Data.Proxy
import Language.Haskell.TH as TH
import Language.Haskell.TH.Datatype


studyFamily :: Name -> Q [Dec]
studyFamily name = do
  info <- reify name
  case info of
    family@(FamilyI {}) -> reportWarning ("Got family: " <> show family)
    x -> reportWarning ("Got something else: " <> show x)

  return []


-- showTH th = putStrLn $(stringE . pprint =<< th)

getConstructorTypes :: Name -> Q [Dec]
getConstructorTypes gadt = do
  TyConI (DataD _ _ _ _ cons _) <- reify gadt
  reportWarning ("Got cons: " <> show cons)
  return []


getImageOfGADTUnderTypeFamily :: Name -> Name -> Q Exp
getImageOfGADTUnderTypeFamily gadt typeFamily = do
  DatatypeInfo {datatypeCons} <- reifyDatatype gadt

  exprs <- forM (fmap constructorName datatypeCons) $ \consName -> do
    [|TSType (Proxy :: Proxy ($(return $ PromotedT typeFamily) $(return $ PromotedT consName)))|]

  return $ ListE exprs
