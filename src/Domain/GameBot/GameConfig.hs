{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Domain.GameBot.GameConfig where

import Reexport
import ClassyPrelude


data GameConfig = GameConfig{ 
      ballSpeed :: Float
    , ballDiameter :: Float
    , loseHeightLevel :: Float
    , initialRows :: Int
    , numberOfBallsInChainToDelete :: Int
    , nearestBallDiameterFactor :: Float -- used in findNearesBalls and findChainOfColor for search in range =  diameter * nearestBallDiameterFactor
    , addNewRowsTimeInterval :: Float -- im milliseconds
    , ballsInSmallRow :: Int  -- ballsInBigRow = ballsInSmallRow + 1
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)



defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig{ 
      ballSpeed = 0.8
    , ballDiameter = 74.0
    , loseHeightLevel = 100.0
    , initialRows = 3
    , numberOfBallsInChainToDelete = 3
    , nearestBallDiameterFactor = 1.05 -- used in findNearesBalls and findChainOfColor for search in range =  diameter * nearestBallDiameterFactor
    , addNewRowsTimeInterval = 15000 -- im milliseconds
    , ballsInSmallRow = 7  -- ballsInBigRow = ballsInSmallRow + 1
    }
