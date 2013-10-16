module F95VarDeclParser where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

run_parser_print :: Show a => Parser a -> String -> IO ()
run_parser_print p str = do
      case parse p "" str of
           Left err -> do
               putStr "parse error at "
               print err
           Right x  -> putStrLn $ "    "++(show x)++","
                                                                                                                                                         
run_parser :: Parser a -> String -> a
run_parser p str =  case parse p "" str of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val  

f95_var_decl_parser :: Parser VarDecl
f95_var_decl_parser = return dummyVarDecl
      
type_parser :: Parser VarType
type_parser = return dummyVarType
      
dim_parser :: Parser [Range]
dim_parser = return [dummyRange]

range_parser :: Parser Range
range_parser = return dummyRange

single_var_range :: Parser Range    
single_var_range = return dummyRange

single_const_range :: Parser Range
single_const_range = return dummyRange

single_expr_range :: Parser Range
single_expr_range = return dummyRange

range_expr :: Parser Range    
range_expr =  return dummyRange

intent_parser :: Parser Intent    
intent_parser = return dummyIntent
   
arglist_parser :: Parser [VarName]    
arglist_parser = return [dummyVarName]

ocl_argmode_parser :: Parser OclArgMode    
ocl_argmode_parser = return dummyArgMode

term :: Parser Expr
term = parens expr_parser <|> const_expr <|> var_expr <?> "simple expression"
      
expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"

const_expr :: Parser Expr
const_expr = return dummyConstExpr

var_expr :: Parser Expr
var_expr = return dummyVarExpr

optable =
    let
        binop name assoc   = Infix ( do {  reservedOp name; return (\x y ->(Op (MkOpExpr name x y))) } ) assoc
        prefix name     = Prefix  ( reservedOp  name >> return (\x ->(Pref (MkPrefixOpExpr name x))) ) 
    in
        [
          [ binop "*"  AssocLeft, binop "/"  AssocLeft, binop "%" AssocLeft ]
        , [ binop "+"  AssocLeft, binop "-"  AssocLeft ]
        , [ prefix "-" ]
        ]

lexer       = P.makeTokenParser emptyDef    

parens          = P.parens lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
word            = P.identifier lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
comma           = P.comma lexer
semi            = P.semi lexer
natural         = P.natural lexer

