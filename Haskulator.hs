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

-- Gets the contents of the iterable after the position
iterAfter :: Int -> [a] -> [a]
iterAfter pos iter = snd $ splitAt pos iter

splitCalc :: String -> [String]
splitCalc calc = do
  let item = if isDigit $ head calc
             then takeWhile isDigit calc -- Get the entire number
             else [head calc] -- Get the opperator
  if item == calc
    then [item] -- We've hit the end of the calculation
    else item : splitCalc (iterAfter (length item) calc) -- More to go

tokenize :: String -> [Token]
tokenize calc = map classify $ splitCalc $ filter (/=' ') calc

main = print $ tokenize "123* 4.31"