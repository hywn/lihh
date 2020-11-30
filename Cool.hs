import System.Environment
import Data.List
import Control.Monad
import Text.Parsec
import Text.Parsec.String

{- Parsec-learning resources
	https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
	https://stackoverflow.com/questions/9220986/
	https://jakewheat.github.io/intro_to_parsing/
-}

data Parsed
	= Symbol String
	| List [Parsed]
instance Show Parsed where
	show (Symbol a) = a
	show (List a) = "(" ++ (concat $ intersperse " " $ map show a) ++ ")"

pH :: Parser Char
pH = oneOf " \t\n"

pSymbol :: Parser Parsed
pSymbol = liftM Symbol $ many1 (letter <|> char '=' <|> char '_')

pApp :: Parser Parsed
pApp = liftM List $ sepEndBy pExpr $ many1 pH

pLapp :: Parser Parsed
pLapp = do
	char '>'
	many pH
	stuff <- sepEndBy pExpr $ many1 pH
	return $ foldl1 (\a b -> List [a, b]) stuff

pRapp :: Parser Parsed
pRapp = do
	char '<'
	many pH
	stuff <- sepEndBy pExpr $ many1 pH
	return $ foldr1 (\a b -> List [a, b]) stuff

pFunc :: Parser Parsed
pFunc = do
	char '\\'
	many pH
	args <- sepEndBy pSymbol $ many1 pH
	char '.'
	many pH
	expr <- pExpr
	return $ foldr (\a b -> List [(Symbol "\\"), a, b]) expr args

pDefine = do
	char '!'
	many pH
	symbol <- pSymbol
	many1 pH
	expr <- pExpr
	return $ List [Symbol "!", symbol, expr]

pExpr :: Parser Parsed
pExpr = pSymbol <|> pFunc <|> do
	char '('
	many pH
	x <- pDefine <|> pLapp <|> pRapp <|> pApp <|> pExpr
	many pH
	char ')'
	return x

readExpr :: String -> String
readExpr input = case parse pExpr "test" input of
	Left err -> "Parser error: " ++ show err
	Right val -> show val

main :: IO()
main = do
	text <- getContents
	putStr $ readExpr text