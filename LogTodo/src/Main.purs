module Main where

import Prelude
import Control.Monad.Eff (Eff)
import React (ReactClass, Render, createClass, getProps, spec)
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Text (text_)

render :: forall props state eff. Render props state eff
render ctx = do
            _ <- getProps ctx  -- get props from context if needed
            pure(text_ "Hello World")

app :: forall p. ReactClass p
app = createClass $ spec {} render

main :: forall e. Eff (register:: REGISTER | e) Unit
main = do
  registerComponent "LogTodo" app
