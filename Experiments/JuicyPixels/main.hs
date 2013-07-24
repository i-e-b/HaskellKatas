
import Codec.Picture
import Codec.Picture.Types (dynamicMap)
import qualified Data.Vector.Storable as V
import Data.Word

main = do
	img <- readImage "./test.png"
	let info = myInfoImage img
	putStrLn info

myInfoImage :: Either String DynamicImage -> String
myInfoImage (Left err) = "Error: "++err
myInfoImage (Right img) = "Got an image: " ++ (show $ width img) ++ " x " ++ (show $ height img) ++ ", type is " ++ (kind img)
	where
		width = dynamicMap imageWidth
		height = dynamicMap imageHeight

whatever :: Either String DynamicImage -> Maybe DynamicImage
whatever (Right img) = Just img
whatever _ = Nothing

rgbVect :: Maybe DynamicImage -> Maybe (V.Vector Word8) 
rgbVect (Just (ImageRGB8 a)) = Just (imageData a)
rgbVect _ = Nothing

kind :: DynamicImage -> String
kind (ImageY8 a) = "Greyscale"
kind (ImageRGB8 a) = "True colour"
kind (ImageRGBA8 a) = "True with alpha"
kind _ = "Other"

