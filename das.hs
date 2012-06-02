module Main where
import System.IO
import Text.ParserCombinators.Parsec
import Numeric
import Data.Char
import Data.Word
import Data.Bits
import Data.Map hiding (map)
import Data.Maybe
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL

type Label = String

data Val =
	  Register String
	| DerefRegister String
	| DerefRegisterOffset String Int
	| Pop
	| Peek
	| Push
	| SP
	| PC
	| Overflow
	| DerefLiteral Int
	| Literal Int
	| Addr Label
	| DerefAddr Label
	deriving Show

data Op =
	  Set Val Val
	| Add Val Val
	| Sub Val Val
	| Mul Val Val
	| Div Val Val
	| Mod Val Val
	| Shl Val Val
	| Shr Val Val
	| And Val Val
	| Bor Val Val
	| Xor Val Val
	| Ife Val Val
	| Ifn Val Val
	| Ifg Val Val
	| Ifb Val Val
	| Jsr Val
	| Putc Val
	| Getc Val
	| StringLiteral String
	deriving Show

dasFile = do
	whiteSpace
	result <- many op
	whiteSpace
	eof
	return result

lexeme p = do { x <- p ; whiteSpace ; return x }

op :: GenParser Char st ([Label], Op)
op = do
	l <- many addrLabel
	o <- try op2 <|> try op1 <|> try literalString <?> "instruction"
	whiteSpace
	return (l, o)
 where
	op2 = do { n <- op2name ; a <- val ; b <- val ; return (n a b) }
	op2name = anyLex [
			("set", Set),
			("add", Add),
			("sub", Sub),
			("mul", Mul),
			("div", Div),
			("mod", Mod),
			("shl", Shl),
			("shr", Shr),
			("and", And),
			("bor", Bor),
			("xor", Xor),
			("ife", Ife),
			("ifn", Ifn),
			("ifg", Ifg),
			("ifb", Ifb)
		]
	op1 = do { n <- op1name ; a <- val ; return (n a) }
	op1name = anyLex [
			("jsr", Jsr),
			("putc", Putc),
			("getc", Getc)
		]

	literalString = do
		char '"'
		content <- many quotedChar
		char '"'
		return $ StringLiteral content
	quotedChar = noneOf "\\\""
	         <|> try (string "\\\"" >> return '"')
	         <|> try (string "\\\\" >> return '\\')
	         <|> (try $ do
	           char '\\'
	           ds <- count 2 hexDigit
	           let num = fst . (!! 0) . readHex $ ds
	           return $ chr num
	         )

anyLex ((s,f):[]) = try (reserved s >> return f) <?> "instruction"
anyLex ((s,f):ss) = try (reserved s >> return f) <|> anyLex ss

val = try register
  <|> try derefRegisterOffset
  <|> try derefRegister
  <|> try (derefAddr >>= return . DerefAddr)
  <|> try (reserved "%pop" >> return Pop)
  <|> try (reserved "%peek" >> return Peek)
  <|> try (reserved "%push" >> return Push)
  <|> try (reserved "%sp" >> return SP)
  <|> try (reserved "%pc" >> return PC)
  <|> try (reserved "%o" >> return Overflow)
  <|> try (addrName >>= return . Addr)
  <|> try (literalNum >>= return . Literal)
  <?> "value"
register = foldr1 (<|>) $ map r ["a","b","c","x","y","z","i","j"]
	where r s = reserved ('%':s) >> return (Register s)
derefRegister = brackets $ do
	(Register r) <- register
	return (DerefRegister r)
derefRegisterOffset = brackets $ do
	(Register r) <- register
	symbol "+"
	offset <- literalNum
	return (DerefRegisterOffset r offset)

derefAddr = brackets addrName
addrName = identifier

literalNum = do
	digits <- many1 hexDigit
	let offset = fst . (!! 0) $ readHex digits
	return offset

addrLabel = try $ lexeme $ do
	name <- identifier
	char ':'
	return name

identifier = lexeme $ do
	first <- letter
	rest <- many identLetter
	return (first:rest)

identLetter = alphaNum <|> char '_'
symbol s = lexeme $ string s
reserved s = lexeme $ try $
	do { string s ; notFollowedBy identLetter <?> ("end of " ++ show s) }
brackets = between (symbol "[") (symbol "]")

whiteSpace = do
	skipMany (simpleSpace <|> oneLineComment <?> "")
 where
	simpleSpace = skipMany1 (satisfy isSpace)
	oneLineComment = do
		try $ string "#"
		skipMany $ satisfy (/= '\n')
		return ()

parseDAS input = parse dasFile "(unknown)" input

assemble os = resolveLabels look preops
	where (m, preops) = (assemble' os 0 empty)
	      look = fromJust . (flip Data.Map.lookup m)

-- if Label, should be replaced during label resolution.
assemble' :: [([Label],Op)] -> Word16 -> Map Label Word16 ->
             ((Map Label Word16), [Either Word16 Label])
assemble' [] a m = (m,[])
assemble' ((ls,o):os) a m = (finalM, thisOp ++ rest)
	where (finalM, rest) = assemble' os (a+(fromIntegral $ length thisOp)) m'
	      thisOp = assembleOp o
	      m' = foldr id m (map (\l -> insert l a) ls)

assembleOp :: Op -> [Either Word16 Label]
assembleOp (Set a b) = emit 0x1 a b
assembleOp (Add a b) = emit 0x2 a b
assembleOp (Sub a b) = emit 0x3 a b
assembleOp (Mul a b) = emit 0x4 a b
assembleOp (Div a b) = emit 0x5 a b
assembleOp (Mod a b) = emit 0x6 a b
assembleOp (Shl a b) = emit 0x7 a b
assembleOp (Shr a b) = emit 0x8 a b
assembleOp (And a b) = emit 0x9 a b
assembleOp (Bor a b) = emit 0xa a b
assembleOp (Xor a b) = emit 0xb a b
assembleOp (Ife a b) = emit 0xc a b
assembleOp (Ifn a b) = emit 0xd a b
assembleOp (Ifg a b) = emit 0xe a b
assembleOp (Ifb a b) = emit 0xf a b
assembleOp (StringLiteral s) = map (Left . fromIntegral . ord) s
assembleOp (Jsr a)   = emit0 0x1 a
assembleOp (Putc a)  = emit0 0x10 a
assembleOp (Getc a)  = emit0 0x11 a

emit :: Word16 -> Val -> Val -> [Either Word16 Label]
emit opcode a b =
	[Left $ opcode .|. (shift (valFor a) 4) .|. (shift (valFor b) 10)] ++
		auxFor a ++ auxFor b

emit0 opcode a =
	[Left $ (shift opcode 4) .|. (shift (valFor a) 10)] ++ auxFor a

valFor :: Val -> Word16
valFor (Register s) = regToInt s
valFor (DerefRegister s) = 0x08 + regToInt s
valFor (DerefRegisterOffset s _) = 0x10 + regToInt s
valFor Pop = 0x18
valFor Peek = 0x19
valFor Push = 0x1a
valFor SP = 0x1b
valFor PC = 0x1c
valFor Overflow = 0x1d
valFor (DerefLiteral i) = 0x1e
valFor (Literal i) | i <= 0x1f = 0x20 + fromIntegral i
valFor (Literal _) | otherwise = 0x1f
valFor (Addr _) = 0x1f
valFor (DerefAddr _) = 0x1e

auxFor :: Val -> [Either Word16 Label]
auxFor (DerefRegisterOffset _ o) = [Left $ fromIntegral o]
auxFor (DerefLiteral i) = [Left $ fromIntegral i]
auxFor (Literal i) | i > 0x1f = [Left $ fromIntegral i]
auxFor (Addr s) = [Right s]
auxFor (DerefAddr s) = [Right s]
auxFor _ = []

resolveLabels :: (Label -> Word16) -> [Either Word16 Label] -> [Word16]
resolveLabels m [] = []
resolveLabels m ((Right label):rs) = m label : resolveLabels m rs
resolveLabels m ((Left w):rs)      = w : resolveLabels m rs

regToInt "a" = 0
regToInt "b" = 1
regToInt "c" = 2
regToInt "x" = 3
regToInt "y" = 4
regToInt "z" = 5
regToInt "i" = 6
regToInt "j" = 7

main = do
	c <- getContents
	case parse dasFile "(stdin)" c of
		Left e -> do hPutStrLn stderr "Error parsing input:"
		             hPutStrLn stderr (show e)
		Right r -> do
			let words = assemble r
			BL.putStr $ runPut $ mapM_ putWord16be words
