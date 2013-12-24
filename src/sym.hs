import System.IO (BufferMode(..), hSetBuffering, putStr, stdout)
import System.IO.Error (catchIOError)

import Sym.SyntaxTree (Expr(..), evalExpr)
import Sym.Parser (parseStmt)

main = do
    hSetBuffering stdout NoBuffering
    mainLoop

mainLoop :: IO ()
mainLoop = do
    putStr "> "
    catchIOError (getLine >>= mainLoop') (\_ -> return ())

mainLoop' :: String -> IO ()
mainLoop' s = do
    case parseStmt s of
        Left e     -> print e
        Right expr -> print $ evalExpr expr
    mainLoop
