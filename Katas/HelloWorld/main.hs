{- |
Module      :  Katas.HelloWorld
Copyright   :  Who cares?
License     :  Public domain
Maintainer  :  person@example.com
Stability   :  experimental
Portability :  GHC

A simple little hello world in Haskell.

This introductory header comment is over the top here, but seems
to be more-or-less the norm in all the projects I see.
Maybe there is some package manager/doc reader tool that uses it?
-}

-- If you put a module declaration in, all you get is an object file. This is probably for libs only
--module Katas.HelloWorld.Main (main) where

main = do
	putStrLn "Hi there! What's your name?"
	name <- getLine
	putStrLn ("Hello, " ++ name ++ "!")
