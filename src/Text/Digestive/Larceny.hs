{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Larceny where

import           Control.Monad.State (evalStateT, get)
import           Control.Monad.Trans (liftIO)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, listToMaybe)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Lucid
import           Lucid.Base
import           Text.Digestive
import           Web.Larceny         (a, (%))
import qualified Web.Larceny

tshow :: Show a => a -> Text
tshow = T.pack . show

type Fill = Web.Larceny.Fill ()
type Library = Web.Larceny.Library ()
type Substitutions = Web.Larceny.Substitutions ()


formFills :: View Text -> Substitutions
formFills view = Web.Larceny.subs $
                 [("dfPlainText", dfPlainText view)
                 ,("dfInput", dfInput view)
                 ,("dfInputText", dfInputText view)
                 ,("dfInputTextArea", dfInputTextArea view)
                 ,("dfInputPassword", dfInputPassword view)
                 ,("dfInputHidden", dfInputHidden view)
                 ,("dfInputSelect", dfInputSelect view)
                 ,("dfInputSelectGroup", dfInputSelectGroup view)
                 ,("dfInputRadio", dfInputRadio view)
                 ,("dfInputCheckbox", dfInputCheckbox view)
                 ,("dfInputFile", dfInputFile view)
                 ,("dfInputSubmit", dfInputSubmit view)
                 ,("dfLabel", dfLabel view)
                 ,("dfForm", dfForm view)
                 ,("dfErrorList", dfErrorList view)
                 ,("dfChildErrorList", dfChildErrorList view)
                 ,("dfList", dfList view)
                 ,("dfSubView", dfSubView view)
                 ,("dfIfChildErrors", dfIfChildErrors view)]

lucidText :: Html a -> Text
lucidText = LT.toStrict . renderText

attrsToLucid :: M.Map Text Text -> [Attribute]
attrsToLucid attrs = map (\(k,v) -> makeAttribute k v) (M.toList attrs)

addAttrs :: Text ->
            View Text ->
            M.Map Text Text ->
            [(Text, Text)] ->
            M.Map Text Text
addAttrs ref view originalAttrs dfAttrs=
  (M.fromList $ setDisabled ref view $ dfAttrs)
  `M.union` (M.delete "ref" originalAttrs)

addStdAttrs :: Text ->
            View Text ->
            M.Map Text Text ->
            [(Text, Text)] ->
            M.Map Text Text
addStdAttrs ref view originalAttrs dfAttrs=
  let ref' = absoluteRef ref view in
  (M.fromList $ setDisabled ref view $ dfAttrs <>
                          [("id", ref')
                          ,("name", ref')])
  `M.union` (M.delete "ref" originalAttrs)


dfInput :: View Text -> Fill
dfInput view =
  Web.Larceny.useAttrs
    (a"ref" % a"type")
    (\ref type' -> Web.Larceny.Fill $ \attrs _ _ ->
        do let value = fieldInputText ref view
               allAttrs = addStdAttrs ref view attrs
                          [("value", value)
                          ,("type", fromMaybe "text" type')]
           return $ lucidText $
             input_ (attrsToLucid allAttrs))

dfInputText :: View Text -> Fill
dfInputText view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
                do let value = fieldInputText ref view
                       allAttrs = addStdAttrs ref view attrs
                                    [("value", value)
                                    ,("type", "text")]
                   return $ lucidText $
                     input_ (attrsToLucid allAttrs))

dfInputTextArea :: View Text -> Fill
dfInputTextArea view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
                do let value = toHtml $ fieldInputText ref view
                       allAttrs = addStdAttrs ref view attrs []
                   return $ lucidText $ do
                     textarea_ (attrsToLucid allAttrs) value)

dfInputPassword :: View Text -> Fill
dfInputPassword view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
                do let value = fieldInputText ref view
                       allAttrs = addStdAttrs ref view attrs
                                    [("value", value)
                                    ,("type", "password")]
                   return $ lucidText $
                     input_ (attrsToLucid allAttrs))

dfInputHidden :: View Text -> Fill
dfInputHidden view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
                do let value = fieldInputText ref view
                       allAttrs = addStdAttrs ref view attrs
                                    [("value", value)
                                    ,("type", "hidden")]
                   return $ lucidText $
                     input_ (attrsToLucid allAttrs))

dfInputSelect :: View Text -> Fill
dfInputSelect view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
                do let ref' = absoluteRef ref view
                       choices = fieldInputChoice ref view
                       kids = map (mkOption ref') choices
                       selectAttrs = addAttrs ref view attrs
                                       [("id", ref')
                                       ,("name", ref')]
                   return $ lucidText $
                     select_ (attrsToLucid selectAttrs) $
                       mconcat kids)

maybeSelected :: Bool -> [Attribute] -> [Attribute]
maybeSelected sel attrs = if sel then [selected_ "selected"] <> attrs
                                 else attrs

mkOption :: Text -> (Text, Text, Bool) -> Html ()
mkOption ref (i, c, sel) =
  option_ (maybeSelected sel $
            attrsToLucid $ M.fromList
            [("value", value i)])
      (toHtml c)
  where value i' = ref <> "." <> i'

dfInputSelectGroup :: View Text -> Fill
dfInputSelectGroup view = Web.Larceny.useAttrs
  (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
           do let ref' = absoluteRef ref view
                  groups = fieldInputChoiceGroup ref view
                  kids = map mkGroup groups

                  mkGroup :: (Text, [(Text, Text, Bool)]) -> Html ()
                  mkGroup (name, options) =
                    optgroup_ (attrsToLucid $
                               M.fromList [("label", name)])
                    (mconcat $ map (mkOption ref') options)

                  selectAttrs = addAttrs ref view attrs
                                [("id", ref')
                                ,("name", ref')]
              return $ lucidText $
                select_ (attrsToLucid selectAttrs) $
                mconcat kids)

dfInputRadio :: View Text -> Fill
dfInputRadio view = Web.Larceny.useAttrs
  (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
             do let ref' = absoluteRef ref view
                    choices = fieldInputChoice ref view
                    kids = mconcat $ map mkRadio choices
                    value i = ref' <> "." <> i

                    mkRadio :: (Text, Text, Bool) -> Html ()
                    mkRadio (i, c, sel) = do
                      label_ [for_ (value i)] $ do
                        input_ (maybeChecked sel $
                                attrsToLucid $
                                addAttrs ref' view attrs
                                 [("type", "radio")
                                 ,("value", value i)
                                 ,("id", value i)
                                 ,("name", ref')])
                        (toHtml c)
                return $ lucidText $ kids)

maybeChecked :: Bool -> [Attribute] -> [Attribute]
maybeChecked sel attrs = if sel then [checked_] <> attrs
                                else attrs

dfInputCheckbox :: View Text -> Fill
dfInputCheckbox view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
                do let ref' = absoluteRef ref view
                       value = fieldInputBool ref view
                       allAttrs = maybeChecked value $
                                  attrsToLucid $
                                  addAttrs ref view attrs
                                    [("type", "checkbox")
                                    ,("id", ref')
                                    ,("name", ref')]
                   return $ lucidText $
                     input_ allAttrs)

dfPlainText :: View Text -> Fill
dfPlainText view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs (pth, tpl) lib ->
                do let ref' = absoluteRef ref view
                       value = fieldInputText ref view
                   return value)

dfInputFile :: View Text -> Fill
dfInputFile view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
                do let ref' = absoluteRef ref view
                       value = maybe "" T.pack $ listToMaybe $ fieldInputFile ref view
                       allAttrs = addAttrs ref view attrs
                                    [("type", "file")
                                    ,("id", ref')
                                    ,("name", ref')
                                    ,("value", value)]
                   return $ lucidText $
                     input_ (attrsToLucid allAttrs))


dfInputSubmit :: View Text -> Fill
dfInputSubmit _ =
   Web.Larceny.Fill $ \attrs _ _ -> do
   let allAttrs = M.fromList [("type", "submit")]  `M.union` attrs
   return $ lucidText $ input_  (attrsToLucid allAttrs)


dfLabel :: View Text -> Fill
dfLabel view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs (pth, tpl) lib ->
                do let ref' = absoluteRef ref view
                       allAttrs = addAttrs ref view attrs
                                    [("for", ref')]
                   ctxt <- get
                   content <- liftIO $ evalStateT
                              (Web.Larceny.runTemplate tpl pth mempty lib)
                              ctxt
                   return $ lucidText $
                     label_ (attrsToLucid allAttrs)
                       (toHtmlRaw content))

-- this seems extra wrong
dfForm :: View Text -> Fill
dfForm view =
   Web.Larceny.Fill $ \attrs (pth, tpl) lib -> do
   let allAttrs = M.fromList [("type", "submit")
                             ,("method", "POST")
                             ,("enctype", tshow (viewEncType view))]
                  `M.union` attrs
   ctxt <- get
   content <- liftIO $ evalStateT
              (Web.Larceny.runTemplate tpl pth mempty lib)
              ctxt
   return $ lucidText $
     form_  (attrsToLucid allAttrs) (toHtmlRaw content)

errorList :: [Text] -> [Attribute] -> Html ()
errorList [] _ = mempty
errorList errs attrs = ul_ attrs $ mconcat $ map makeError errs
  where makeError :: Text -> Html ()
        makeError e = li_ [] (toHtmlRaw e)

dfErrorList :: View Text -> Fill
dfErrorList view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
                   return $ lucidText $ errorList (errors ref view)
                                        (attrsToLucid attrs))

dfChildErrorList :: View Text -> Fill
dfChildErrorList view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> Web.Larceny.Fill $ \attrs _ _ ->
                   return $ lucidText $ errorList (childErrors ref view)
                                        (attrsToLucid attrs))

dfIfChildErrors :: View Text -> Fill
dfIfChildErrors view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> if null (childErrors ref view)
                     then Web.Larceny.textFill ""
                     else Web.Larceny.fillChildren)

--- ???????
dfSubView :: View Text -> Fill
dfSubView view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> let view' = subView ref view in
                     Web.Larceny.fillChildrenWith (formFills view'))

dfList :: View Text -> Fill
dfList view =
  Web.Larceny.useAttrs
    (a"ref") (\ref -> let views = listSubViews ref view in
                      Web.Larceny.mapSubs (\view' -> formFills view') views)


setDisabled :: Text -> View v -> [(Text, Text)] -> [(Text, Text)]
setDisabled ref view = if viewDisabled ref view then (("disabled",""):) else id

disableOnclick :: Text -> View v -> [(Text, Text)] -> [(Text, Text)]
disableOnclick ref view =
    if viewDisabled ref view then const [("disabled","")] else id
