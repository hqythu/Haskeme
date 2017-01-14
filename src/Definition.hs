module Definition where

data SchemeVal 
    = Atom String
    | List [SchemeVal]
    | Bool Bool
    | Number Double
    | String String