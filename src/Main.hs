module Main where

import Language.PureScript.CST.Parser (parse)
import Language.PureScript.CST.Convert (convertModule)
import Data.Text (pack)
import Text.Pretty.Simple (pPrint)
import Language.PureScript.Linter (lint)
import Control.Monad.Writer (runWriter)

-- lint' :: Module -> []

main :: IO ()
main = do
  source <- pack <$> readFile "TestModule.purs"

  let parseResult = parse source

  case parseResult of
    (_, Right mod) -> do
      let astMod = convertModule "TestModule.purs" mod

      pPrint astMod

      -- let errors = runWriter $ lint astMod

      -- print "lint errors:"
      -- print errors
    _ -> mempty

  -- pPrint parseResult
