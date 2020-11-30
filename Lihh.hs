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
	[ ("_nl", Str "\n")
	, ("_space", Str " ")
	]

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

eval :: Scope -> Parsed -> (Value, Scope, IO ())

eval scope (ParseList lst) = case lst of
	-- evaluated singleton lists as the contained list
	nested@(ParseList _):[] ->
		eval scope nested

	-- print the argument
	((ParseString "_print"):x:[]) ->
		( Void, scope,putStr $ show res)
		where (res, _, _) = eval scope x

	-- make a function
	((ParseString "\\"):(ParseString argname):expr:[]) ->
		( Function (\x -> fst3 $ eval (Map.insert argname x scope) expr)
		, scope
		, return ()
		)

	-- bind something to a symbol in current scope
	((ParseString "!") : (ParseString bindname):expr:[]) ->
		( Void
		, Map.insert bindname (fst3 $ eval scope expr) scope
		, return ()
		)

	-- check equality
	(a:(ParseString "="):b:[]) ->
		if   x == y
		then eval scope (ParseString "true")
		else eval scope (ParseString "false")
		where (x, _, _) = eval scope a
		      (y, _, _) = eval scope b

	((ParseString "_concat"):a:b:[]) ->
		(Str $ (show x) ++ (show y), scope, return ())
		where (x, _, _) = eval scope a
		      (y, _, _) = eval scope b

	-- gather scope modifications and IO effects
	((ParseString "_env"):rest) ->
		foldl after (Void, scope, return ()) rest
		where after (_, oldscope, oldio) thing = (newval, newscope, oldio >> newio)
			where (newval, newscope, newio) = eval oldscope thing

	-- evaluate a function
	(a:b:[]) -> (val, scope, return ())
		where val = case eval scope a of
			(Function f, _, _) -> f x
			_ -> undefined
			where (x, _, _) = eval scope b

eval scope (ParseString s) = case Map.lookup s scope of
	Just val -> (val, scope, return ())
	Nothing  -> (Str s, scope, return ())

repl :: String -> (Value, Scope, IO ())
repl input = eval global parseTree
	where (parseTree, _) = parse $ tokenise input

main :: IO ()
main = do
	text <- getContents
	let (val, scope, io) = repl text
	io