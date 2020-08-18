{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.StmExample where

import Import


getStmExampleR :: Handler Html
getStmExampleR = do
    App {..} <- getYesod
    stmValue <- atomically $ readTMVar stmExample
    defaultLayout $ do
        setTitle "STM Example!"
        $(widgetFile "stm-example")


