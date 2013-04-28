import System.Environment
import Codec.Picture
import Data.Word

data Colour = Colour !Float !Float !Float -- ¡Float!

clamp :: Float -> Float -> Float -> Float -- Float! Float! Float!
clamp low high x = max low (min high x)

colourToPixel :: Colour -> PixelRGB8
colourToPixel (Colour r g b) = PixelRGB8 (frob r) (frob g) (frob b)
  where
    frob :: Float -> Word8
    frob x = round $ clamp 0 255 (255 * (x + 1) / 2)

genPoint :: Float -> Float -> Colour
genPoint x y = Colour x y z

genPixel :: Int -> Int -> Int -> Int -> PixelRGB8
genPixel w h x y = colourToPixel $ genPoint (fI x / fI w) (fI y / fI h) where fI = fromIntegral

genImage :: Int -> Int -> String -> IO ()
genImage width height path = savePngImage path (ImageRGB8 $ generateImage (genPixel width height) width height)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [w, h, path] -> genImage (read w) (read h) path
    _            -> putStrLn "You're doing it wrong."
