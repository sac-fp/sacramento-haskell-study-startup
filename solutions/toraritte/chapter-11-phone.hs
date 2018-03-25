module Phone where

import Data.Char

-- validButtons = "1234567890*#"
type Digit = Char

type Button = (Digit, String)

data DaPhone = DaPhone [Button] deriving (Show, Eq)

-- Valid presses: 1 and up
type Presses = Int

phone :: DaPhone
phone = DaPhone [ ('1', "1"    ),  ('2', "abc2"), ('3', "def3")
                , ('4', "ghi4" ),  ('5', "jkl5"), ('6', "mno6")
                , ('7', "pqrs7"),  ('8', "tuv8"), ('9', "wxyz9")
                , ('*', "*^"   ),  ('0', " +_0" ), ('#', "#.,")
                ]

presses :: Button -> Char -> [(Digit, Presses)]
presses (digit, xs) c = ifUpper ++ [(digit, (getPresses xs (toLower c) 1))]
  where
    ifUpper = case isUpper c of
                True  -> [('*', 1)]
                False -> []
    getPresses (x:xs) c acc =
      case (x == c) of
        True  -> acc
        False -> getPresses xs c (acc+1)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c = presses (button phone) c
 where
    -- button :: DaPhone -> Char -> Button
    button (DaPhone p) = head $ filter getButton p
      where
        getButton (_, xs) = elem (toLower c) xs

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone str =
  foldr (\char list -> (reverseTaps phone char) ++ list) [] str
