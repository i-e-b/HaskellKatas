
import Codec.Picture

main = do
	img <- readImage "./test.png"
	let info = myInfoImage img
	putStrLn info

myInfoImage :: Either String DynamicImage -> String
myInfoImage (Left err) = "Error: "++err
myInfoImage (Right img) = "Got an image"

