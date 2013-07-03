
import Codec.Picture
import Codec.Picture.Types (dynamicMap)

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

kind :: DynamicImage -> String
kind (ImageY8 a) = "Greyscale"
kind (ImageRGB8 a) = "True colour"
kind (ImageRGBA8 a) = "True with alpha"
kind _ = "Other"

