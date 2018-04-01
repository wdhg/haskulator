data Token 
  = TokNum Float
  | TokAdd
  | TokSub
  | TokMul
  | TokDiv
  | TokLParen
  | TokRParen
  deriving (Show)

isDigit :: Char -> Bool
isDigit char = char `elem` '.':['0'..'9']

classify :: String -> Token
classify item
  | all isDigit item = TokNum (read item :: Float)
  | item == "+"      = TokAdd
  | item == "-"      = TokSub
  | item == "*"      = TokMul
  | item == "/"      = TokDiv
  | item == "("      = TokLParen
  | item == ")"      = TokRParen

-- gets the contents of the iterable after the position
takeAfter :: Int -> [a] -> [a]
takeAfter pos iter = snd $ splitAt pos iter

splitCalc :: String -> [String]
splitCalc calc = do
  let item = if isDigit $ head calc
             then takeWhile isDigit calc -- get the entire number
             else [head calc] -- get the opperator
  if item == calc
    then [item] -- we've hit the end of the calculation
    else item : splitCalc (takeAfter (length item) calc) -- more to go

tokenize :: String -> [Token]
tokenize calc = map classify $ splitCalc $ filter (/=' ') calc

main = print $ tokenize "123* 4.31"