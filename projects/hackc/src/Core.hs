module Core where

import Text.Megaparsec.String

xmlPrint :: ClassDecl -> String
xmlPrint c = "<ClassDecl></ClassDecl>"

compile :: ClassDecl -> String
compile = const ""

pClassDecl :: Parser ClassDecl
pClassDecl = return $ ClassDecl "Foo" [] []

type ClassName = String
type VarName = String

data ClassDecl = ClassDecl {
  _classDeclName :: ClassName,
  _classVarDecls :: [VarDecl],
  _classSubroutineDecls :: [SubroutineDecl]
  }

data VarDecl = VarDecl {
  _varDeclStatic :: Bool,
  _varDeclType :: Type,
  _varDeclName :: VarName
  }

data Type = TypeInt
          | TypeChar
          | TypeBoolean
          | TypeClass ClassName

data SubroutineSort = Constructor | Function | Method

type SubroutineName = String

data SubroutineDecl = SubroutineDecl {
  _subroutineDeclSort :: SubroutineSort,
  _subroutineDeclReturns :: Maybe Type,
  _subroutineDeclName :: SubroutineName,
  _subroutineDeclParams :: [(Type, VarName)],
  _subroutineDeclLocals :: [VarDecl],
  _subroutineDeclBody :: [Statement]
  }

data Statement = Let VarName (Maybe Expression) Expression
               | If Expression [Statement] (Maybe [Statement])
               | While Expression [Statement]
               | Do SubroutineCall
               | Return (Maybe Expression)

data Expression = Expression Term [(Op, Term)]

data Term = IntConst Int
          | StringConst String
          | BoolConst Bool
          | Null
          | This
          | Var VarName
          | Index VarName Expression
          | Call SubroutineCall
          | Parens Expression
          | Neg Term
          | Not Term

data Op = Plus
        | Minus
        | Mult
        | Div
        | And
        | Or
        | LT
        | GT
        | EQ

data SubroutineCall = FunctionCall SubroutineName [Expression]
                    | MethodCall VarName SubroutineName [Expression]
                    | StaticCall ClassName SubroutineName [Expression]
