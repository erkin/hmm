import System.Environment
import Codec.Picture
genPixel :: Int -> Int -> PixelRGB8
genPixel _ _ = PixelRGB8 0 0 0
genImage :: Int -> Int -> String -> IO ()
genImage width height path = savePngImage path (ImageRGB8 $ generateImage genPixel width height)
main = getArgs >>= genImage 512 512 . head
