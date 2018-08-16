data PF = POSTFIX deriving (Show, Eq)
data Command = Int 
               | ADD
               | DIV
               | EQ
               | EXEC
               | GT
               | LT
               | MUL 
               | NGET
               | POP
               | REM
               | SEL
               | SUB
               | SWAP 
               | SEC [Command] deriving (Show, Eq)

type Stack = [Command]

type Program = (PF, Int, [Command])

execSec :: [Command] -> Stack -> Stack
execSec [] s = s
execSec (c:cs) s -> case c of
    ADD -> execSec cs s:c


 
