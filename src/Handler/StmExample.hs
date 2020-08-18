{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.StmExample where

import Import


data StmFormData = StmFormData
  { baseValue :: Int
  , sumValue :: Int
  , textValue :: Text
  , textareaValue :: Textarea
  , htmlValue :: Html
  }



stmForm :: Form StmFormData
stmForm = renderDivs $ StmFormData
    <$> areq intField "Base value" Nothing
    <*> areq intField "add this int to the base" Nothing
    <*> areq textField "Text value" Nothing
    <*> areq textareaField "Textarea value" Nothing
    <*> areq htmlField "Html text value" Nothing


getStmExampleR :: Handler Html
getStmExampleR = do
    App {..} <- getYesod
    baseValue' <- atomically $ readTVar stmBaseValue
    sumValue  <- atomically $ readTVar stmSumValue
    textValue <- atomically $ readTVar stmTextValue
    textareaValue <- atomically $ readTVar stmTextareaValue
    htmlValue <- atomically $ readTVar stmHtmlValue
    let newValue = baseValue' + sumValue
    atomically $ writeTVar stmBaseValue newValue
    baseValue <- atomically $ readTVar stmBaseValue
    defaultLayout $ do
        setTitle "STM Example!"
        $(widgetFile "stm-example")


getStmFormR :: Handler Html
getStmFormR = do
    App {..} <- getYesod
    baseValue <- atomically $ readTVar stmBaseValue
    sumValue  <- atomically $ readTVar stmSumValue
    textValue <- atomically $ readTVar stmTextValue
    textareaValue <- atomically $ readTVar stmTextareaValue
    htmlValue <- atomically $ readTVar stmHtmlValue
    (formWidget, formEnctype) <- generateFormPost stmForm

    defaultLayout $ do
        setTitle "STM Example!"
        $(widgetFile "stm-form")

postStmFormR :: Handler Html
postStmFormR = do
    ((result, formWidget), formEnctype) <- runFormPost stmForm
    case result of
      -- Yay! Form is valid
      FormSuccess entry -> defaultLayout $ do
        App {..} <- getYesod
        atomically $ do
          writeTVar stmBaseValue (baseValue entry)
          writeTVar stmSumValue  (sumValue entry)
          writeTVar stmTextValue (textValue entry)
          writeTVar stmTextareaValue (textareaValue entry)
          writeTVar stmHtmlValue (htmlValue entry)

        baseValue <- atomically $ readTVar stmBaseValue
        sumValue  <- atomically $ readTVar stmSumValue
        textValue <- atomically $ readTVar stmTextValue
        textareaValue <- atomically $ readTVar stmTextareaValue
        htmlValue <- atomically $ readTVar stmHtmlValue

        setTitle "STM Example!"
        $(widgetFile "stm-form")

      -- utoh! Form failed validation
      _ -> defaultLayout $ do
        App {..} <- getYesod
        baseValue <- atomically $ readTVar stmBaseValue
        sumValue  <- atomically $ readTVar stmSumValue
        textValue <- atomically $ readTVar stmTextValue
        textareaValue <- atomically $ readTVar stmTextareaValue
        htmlValue <- atomically $ readTVar stmHtmlValue
        setTitle "STM Example!"
        $(widgetFile "stm-form")
