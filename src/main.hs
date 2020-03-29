module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"" <|> (char '\\' >> oneOf "\"nrt\\"))
                char '"'
                return $ String x


parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom


parseNumber' :: Parser LispVal
parseNumber' = liftM (Number . read) $ many1 digit


-- Ex 1.1 With do notation
parseNumber :: Parser LispVal
parseNumber = do
                first <- digit <|> (char '#' >> oneOf "h")
                rest <- many1 (digit <|> oneOf ['a'..'f'])
                return $ case first of
                           'h' -> Number . fst . head . readHex $ rest
                           _   -> Number . read $ first : rest


-- Ex 1.2 With explicit sequencing
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= (\x -> return $ (Number . read) x)


parseExpr :: Parser LispVal
parseExpr = parseString
         <|> parseNumber
         <|> parseAtom


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value -> " ++ show val


main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn $ readExpr expr