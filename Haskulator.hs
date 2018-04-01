-- in order of precedence
data Token 
  = TokNum Float
  | TokSub
  | TokAdd
  | TokDiv
  | TokMul
  deriving (Show, Eq, Ord)

isDigit :: Char -> Bool
isDigit char = char `elem` '.':['0'..'9']

classify :: String -> Token
classify item
  | all isDigit item = TokNum (read item :: Float)
  | item == "+"      = TokAdd
  | item == "-"      = TokSub
  | item == "*"      = TokMul
  | item == "/"      = TokDiv

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
    else item:splitCalc (takeAfter (length item) calc) -- more to go

tokenize :: String -> [Token]
tokenize calc = map classify $ splitCalc $ filter (/=' ') calc

-- shunting-yard algorithm
sortTokens :: [Token] -> [Token] -> [Token]
sortTokens (token:rest) opperators = case token of
  TokNum {} -> token:sortTokens rest opperators -- is number
  opperator -> if opperators == null || opperator > head opperators
    then sortTokens rest $ token:opperators
    else head opperators:sortTokens rest (tail opperators)

--parse :: [Token] -> Float
--parse tokens = do
  

-- calculate :: String -> Float
-- calculate calc
--   parse $ sortTokens $ tokenize calc+

main = do
  let tokens = tokenize "123 * 4.31"
  print $ sortTokens tokens []