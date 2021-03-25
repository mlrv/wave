{-# LANGUAGE OverloadedStrings #-}

module Wave.Parser where

import Control.Arrow (left)
import Control.Monad
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Wave.Ast

type Parser = P.Parsec Void T.Text

type Ann = P.SourcePos

runParser :: Parser a -> FilePath -> T.Text -> Either T.Text a
runParser p file src =
  let withEof = p <* P.eof
      parsed = P.runParser withEof file src
   in left (T.pack . P.errorBundlePretty) parsed

parseLit :: Parser Lit
parseLit =
  P.choice
    [ LString <$> stringLiteral,
      numberLiteral
    ]

sc :: Parser ()
sc =
  L.space
    P.hspace1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser ()
symbol = void . L.symbol sc

stringLiteral :: Parser T.Text
stringLiteral =
  fmap T.pack $ P.char '\"' *> P.manyTill L.charLiteral (P.char '\"')

numberLiteral :: Parser Lit
numberLiteral = do
  sign <- P.optional $ P.char '-'
  int <- P.some P.digitChar
  dec <- P.optional $ P.char '.' *> P.some P.digitChar
  case dec of
    Nothing -> pure $ LInt (read $ maybe [] pure sign <> int)
    Just n -> pure $ LFloat (read $ maybe [] pure sign <> int <> "." <> n)

-- Utils

getAnn :: Parser Ann
getAnn = P.getSourcePos