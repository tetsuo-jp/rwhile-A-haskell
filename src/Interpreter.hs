module Main where

import System.Environment ( getArgs )

import ParRWHILE
import PrintRWHILE
import Interpret
import ErrM

main :: IO ()
main = do args <- getArgs
          case args of
            [file, val_str]
                        -> do prog_str <- readFile file
                              case (pVal (myLexer val_str), pProgram (myLexer prog_str)) of
                               (Ok val, Ok prog) -> 
                                 case execProgram val prog of
                                   Bad s  -> putStrLn ("error: " ++ s)
                                   Ok res -> putStrLn (printTree res)
                               _ -> print "parse error"
            _ -> error "Error A1"
