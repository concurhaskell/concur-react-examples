{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<|>))

-- import Control.Monad (forever)

import           Concur.Core
import           Concur.React

import           Text.Read           (readMaybe)

-- Your first 8 Concur programs!
-- http://www.altocumulus.org/Fudgets/Intro/

-------------------------------------------------------------------------------
-- 1. Hello World!
-- `text` is a widget that displays static text
main1 :: Widget HTML a
main1 = text "Hello, world!"

-------------------------------------------------------------------------------
-- 2. Hello World with a Quit button
-- `button` takes a list of dom attributes and a child widget and wraps that in a button
-- `<|>` is a widget combinator that combines two widgets in parallel
-- In Concur, widgets have a definite lifecycle. The button widget ends as soon as it has clicked.
-- Parallel composition of widgets ends as soon as any one of the child widgets ends.
--   So the entire program ends when the button is clicked.
main2 :: Widget HTML ()
main2 = text "Hello, world!" <|> button [] (text "Quit")

-------------------------------------------------------------------------------
-- 3. Factorial function, version 1

-- The module Concur.Core.Pipe provides some combinators which can act as processing pipelines.
-- Before we go further, lets create a few more generic pipe widgets

-- A Producer is just an ordinary widget
-- Inputbox that produces integers
intInput :: Widget HTML Int
intInput = readMaybe <$> inputEnter [] >>= maybe intInput return

-- Lets create a Pipe that displays all Integers that go through it
intOutput :: Pipe (Widget HTML) Int x
intOutput = pipe (text "") (text . show)

-- Okay lets go ahead

-- A Simple Factorial function
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- We lift the factorial function into a Pipe
main3 :: Widget HTML a
main3 = intInput >>- purePipe fac >>- intOutput

-------------------------------------------------------------------------------
-- 4. Factorial function, version 2, improved layout
main4 :: Widget HTML a
main4 = el_ "div" [] intInput >>- purePipe fac >>- el_ "div" [] intOutput


-------------------------------------------------------------------------------
-- 5. Up Counter

-- A counting function
count :: Int -> () -> (Int, Int)
count n () = (n+1, n+1)

main5 :: Widget HTML a
main5 = button [] (text "Up") >>- pureStatefulPipe count 0 >>- intOutput

main :: IO ()
main = runWidgetInBody main5
