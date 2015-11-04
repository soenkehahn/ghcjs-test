{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-name-shadowing #-}

module Main where

import           Control.Monad
import           Data.Map
import           Data.Typeable
import           Generics.Eot
import           Reflex.Dom
import           Safe

main :: IO ()
main = do
  mainWidget $ el "div" $ do
    value <- htmlForm (Proxy :: Proxy Foo) -- (A "foo" 42 True)
    el "p" $ do
      dynText =<< mapDyn show value

data Foo
  = Foo {
    sel1 :: String,
    sel2 :: Bool
  }
  | Bar Double
  deriving (Show, Generic, HtmlForm)

-- * HtmlForm

class HtmlForm a where
  htmlForm :: MonadWidget t m => Proxy a -> m (Dynamic t (Maybe a))
  default htmlForm :: (MonadWidget t m, HasEot a, GHtmlForm Datatype (Eot a)) =>
    Proxy a -> m (Dynamic t (Maybe a))
  htmlForm Proxy = do
    eot <- gHtmlForm (datatype (Proxy :: Proxy a))
    mapDyn (fmap fromEot) eot

  defaultInitialValue :: Maybe a
  defaultInitialValue = Nothing

instance HtmlForm String where
  htmlForm Proxy = do
    t <- _textInput_value <$> textInput def
    mapDyn Just t

instance HtmlForm Int where
  htmlForm Proxy = readHtmlForm
  defaultInitialValue = Just 1

instance HtmlForm Double where
  htmlForm Proxy = readHtmlForm
  defaultInitialValue = Just 1

readHtmlForm :: forall a t m . (MonadWidget t m, Show a, Read a, HtmlForm a) =>
  m (Dynamic t (Maybe a))
readHtmlForm = mdo
  let valid = singleton "style" "border-color: blue"
      invalid = singleton "style" "border-color: red"
  t <- textInput $ def
    & textInputConfig_attributes .~ attributes
    & maybe id (\ def -> textInputConfig_initialValue .~ show def)
      (defaultInitialValue :: Maybe a)
  result <- mapDyn readMay $ _textInput_value t
  attributes <- mapDyn (maybe invalid (const valid)) result
  return result

instance HtmlForm Bool where
  htmlForm Proxy = do
    b <- _checkbox_value <$> checkbox False def
    mapDyn Just b

-- * GHtmlForm

class GHtmlForm meta eot where
  gHtmlForm :: MonadWidget t m => meta -> m (Dynamic t (Maybe eot))

instance GConstructorList (Either fields restFields) =>
  GHtmlForm Datatype (Either fields restFields) where
    gHtmlForm (Datatype name constructors) = do
      let choices = constDyn $ fromList $
            zip [0 :: Int ..] (fmap constructorName constructors)
          forms = gConstructorList constructors
      el "p" $ text ("type: " ++ name)
      index <- el "p" $ do
        text "constructor: "
        _dropdown_value <$> dropdown 0 choices def
      dynForm <- mapDyn (forms !!) index
      r <- dyn dynForm
      joinDyn <$> holdDyn (constDyn Nothing) r

class GConstructorList eot where
  gConstructorList :: MonadWidget t m =>
    [Constructor] -> [m (Dynamic t (Maybe eot))]

instance (GHtmlForm Fields a, GConstructorList r) =>
  GConstructorList (Either a r) where
    gConstructorList (Constructor _ fields : r) = this : rest
      where
        this = do
          mapDyn (fmap Left) =<< gHtmlForm fields
        rest :: forall t m . MonadWidget t m => [m (Dynamic t (Maybe (Either a r)))]
        rest = fmap (\ m -> m >>= mapDyn (fmap Right)) $ (gConstructorList r :: [m (Dynamic t (Maybe r))])
    gConstructorList [] = error "impossible"

instance GConstructorList Void where
  gConstructorList _ = []

-- * fields

instance (Typeable a, HtmlForm a, GHtmlForm Fields r) => GHtmlForm Fields (a, r) where
  gHtmlForm fields = do
    a <- el "p" $ do
      forM_ (fieldsHead fields) $ \ label ->
        text (label ++ " = ")
      htmlForm (Proxy :: Proxy a) <*
        text (" :: " ++ showType (Proxy :: Proxy a))
    r :: Dynamic t (Maybe r) <- gHtmlForm (fieldsTail fields)
    combineDyn (liftM2 (,)) a r

showType :: Typeable a => Proxy a -> String
showType p = case show (typeRep p) of
  "[Char]" -> "String"
  x -> x

fieldsHead :: Fields -> Maybe String
fieldsHead = \ case
  Selectors (a : _) -> Just a
  NoSelectors _ -> Nothing
  -- impossible cases:
  Selectors [] -> Nothing
  NoFields -> Nothing

fieldsTail :: Fields -> Fields
fieldsTail = \ case
  Selectors (_ : r) -> Selectors r
  NoSelectors n -> NoSelectors (pred n)
  -- impossible cases:
  Selectors [] -> Selectors []
  NoFields -> NoFields

instance GHtmlForm Fields () where
  gHtmlForm _ = return $ constDyn (Just ())
