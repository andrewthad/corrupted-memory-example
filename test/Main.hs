import Packed.Bytes.Parser (Parser)
import Data.Word
import Packed.Bytes (Bytes)
import GHC.Exts (RealWorld)
import Packed.Bytes.Stream.ST (ByteStream)
import Control.Monad.ST (ST,runST)
import qualified Packed.Bytes as B
import qualified Data.Char
import qualified Packed.Bytes.Parser as P
import qualified Packed.Bytes.Stream.ST as Stream

main :: IO ()
main = do
  let r = runST $ runExampleParser
        (P.takeBytesUntilEndOfLineConsume)
        (foldMap Stream.singleton (map charToWord8 "emporium"))
  print r

runExampleParser :: Parser e () a -> ByteStream s -> ST s (Maybe a, Maybe String)
runExampleParser parser stream = do
  P.Result mleftovers r _ <- P.parseStreamST stream () parser
  mextra <- case mleftovers of
    Nothing -> return Nothing
    Just (P.Leftovers chunk remainingStream) -> do
      bs <- Stream.unpack remainingStream
      return (Just (map word8ToChar (B.unpack chunk ++ bs)))
  return (either (const Nothing) Just r,mextra)

word8ToChar :: Word8 -> Char
word8ToChar = Data.Char.chr . fromIntegral

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . Data.Char.ord

s2b :: String -> Bytes
s2b = B.pack . map charToWord8

c2w :: Char -> Word8
c2w = charToWord8
