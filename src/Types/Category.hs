module Types.Category where

data Category a = Empty | CatName a (Category a) deriving Show

exCategory :: Category String
exCategory = CatName "Anime" (CatName "Action/dramma" (CatName "Titan Attack" Empty))