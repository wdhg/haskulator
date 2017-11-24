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
  let item = if isDigit $ calc !! 0 then takeWhile isDigit calc else [calc !! 0]
  if item == calc
    then [item]
    else item : splitCalc (iterAfter (length item) calc)

tokenize :: String -> [Token]
tokenize calc = map classify $ splitCalc $ filter (/=' ') calc

main = do
  print $ tokenize "123* 4.31"