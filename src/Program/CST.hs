module Program.CST where


data BinaryOp = Plus | Minus | OpOr | LessThan deriving (Show, Eq)

data UnaryOp = Negate | LogicalNot deriving (Show, Eq) 

data VConst = VNum Int | VStr String deriving (Show, Eq)
data Value = VConst VConst | VTime deriving (Show, Eq)

data Expr = 
    Val Value | 
    BinOp BinaryOp Expr Expr | 
    UnOp UnaryOp Expr |
    MTLExpr MTLElement MTLBound Expr
    
    deriving (Show, Eq)

data MTLElement = Always | Eventually deriving (Show, Eq)
data MTLBound = None | Range Integer Integer deriving (Show, Eq)

data Property = Prop MTLElement MTLBound Expr | Comp Property Property

type Program = Property 