import Data.Maybe
import qualified Data.Map.Lazy as Map

type Token = String

-- probably very bad and slow
space_parens :: String -> String
space_parens = concatMap replace
	where replace '(' = " ( "
	      replace ')' = " ) "
	      replace x   = [x]

tokenise :: String -> [String]
tokenise = words . space_parens

data Parsed
	= ParseString String
	| ParseList [Parsed]
	deriving Show

takeList :: [Token] -> ([Parsed], [Token])
takeList (")":tokens) = ([], tokens)
takeList any = (got_val:lst, rest2)
	where (got_val, rest_tokens) = parse any
	      (lst, rest2) = takeList rest_tokens

parse :: [Token] -> (Parsed, [Token])
parse ("(":rest) = (ParseList lst, remainder)
	where (lst, remainder) = takeList rest
parse (")":rest) = undefined
parse (x:rest) = (ParseString x, rest)

-- ideally everything is a function
data Value
	= Function (Value -> Value)
	| Str String
	| Void

instance Show Value where
	show (Function _) = "function"
	show (Str s) = s
	show Void = "void"
instance Eq Value where
	(==) (Str a) (Str b) = a == b
	(==) _ _ = False

type Scope = Map.Map String Value

global :: Scope
global = Map.fromList
	[
	]

-- ((\ a (\ b a)) hello bro) -> hello
-- ((\ a (a)) hello)

eval :: Scope -> Parsed -> (Value, Scope)

eval scope (ParseList lst) = case lst of
	((ParseString "\\"):(ParseString argname):expr:[]) ->
		( Function (\x -> fst $ eval (Map.insert argname x scope) expr)
		, scope
		)
	((ParseString "!") : (ParseString bindname):expr:[]) ->
		( Void
		, Map.insert bindname (fst $ eval scope expr) scope
		)
	(a:(ParseString "="):b:[]) ->
		if   x == y
		then eval scope (ParseString "true")
		else eval scope (ParseString "false")
		where (x, _) = eval scope a
		      (y, _) = eval scope b
	((ParseString "env"):rest) ->
		foldl (\(_, scop) thing -> eval scop thing) (Void, scope) rest
	(a:b:[]) -> (val, scope)
		where val = case eval scope a of
			(Function f, _) -> f x
			_ -> undefined
			where (x, _) = eval scope b

eval scope (ParseString s) = case Map.lookup s scope of
	Just val -> (val, scope)
	Nothing  -> (Str s, scope)

repl :: String -> (Value, Scope)
repl input = (val, env)
	where (parseTree, _) = parse $ tokenise input
	      (val, env) = eval global parseTree