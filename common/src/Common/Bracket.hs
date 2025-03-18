{-# LANGUAGE OverloadedStrings #-}

module Common.Bracket where

import           Data.Text (Text)
import qualified Data.Text as T

bracket2018 :: [Text]
bracket2018 = [
  "Virginia", "UMBC", "Creighton", "Kansas State", "Kentucky", "Davidson", "Arizona", "Buffalo",
  "Miami (FL)", "Loyola (IL)", "Tennessee", "Wright State", "Nevada", "Texas", "Cincinnati", "Georgia State",
  "Xavier", "Texas Southern", "Missouri", "Florida State", "Ohio State", "South Dakota State", "Gonzaga", "UNC Greensboro",
  "Houston", "San Diego State", "Michigan", "Montana", "Texas A", "Providence", "North Carolina", "Lipscomb",
  "Villanova", "Radford", "Virginia Tech", "Alabama", "West Virginia", "Murray State", "Wichita State", "Marshall",
  "Florida", "St. Bonaventure", "Texas Tech", "Stephen F. Austin", "Arkansas", "Butler", "Purdue", "Cal State Fullerton",
  "Kansas", "Pennsylvania", "Seton Hall", "NC State", "Clemson", "New Mexico State", "Auburn", "Charleston",
  "TCU", "Syracuse", "Michigan State", "Bucknell", "Rhode Island", "Oklahoma", "Duke", "Iona" 
  ]

bracket2025Playin :: [Text]
bracket2025Playin = [
  "Alabama State", "St. Francis (PA)", "San Diego State", "North Carolina", "American University", "Mount St. Mary's", "Texas", "Xavier"
  ]

bracket2025 :: [Text]
bracket2025 = bracket2025Playin ++ [
  "Auburn", "Louisville", "Creighton", "Michigan", "UC San Diego", "Texas A&M", "Yale",
  "Ole Miss", "Iowa State", "Lipscomb", "Marquette", "New Mexico", "Michigan State", "Bryant",
  "Florida", "Norfolk State", "UConn", "Oklahoma", "Memphis", "Colorado State", "Maryland", "Grand Canyon",
  "Missouri", "Drake", "Texas Tech", "UNC Wilmington", "Kansas", "Arkansas", "St. John's", "Omaha",
  "Duke", "Mississippi State", "Baylor", "Oregon", "Liberty", "Arizona", "Akron",
  "Brigham Young", "VCU", "Wisconsin", "Montana", "Saint Mary's", "Vanderbilt", "Alabama", "Robert Morris",
  "Houston", "SIU-Edwardsville", "Gonzaga", "Georgia", "Clemson", "McNeese State", "Purdue", "High Point",
  "Illinois", "Kentucky", "Troy", "UCLA", "Utah State", "Tennessee", "Wofford"
  ]