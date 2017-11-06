{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<|>))
import           Control.Monad       (void, forever)
import           Control.Monad.State (StateT, execStateT, get, modify, put)
import           Data.Void           (Void)
import           Text.Read           (readMaybe)

import           Concur.Core
import           Concur.Core.Pipe
import           Concur.React


-- Your first 8 Concur Pipe programs!
-- Inspired from the mighty Fudgets' - http://www.altocumulus.org/Fudgets/Intro/

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

-- A Producer is just an ordinary widget lifted to a pipe
-- Inputbox that produces integers
intInput :: Producer (Widget HTML) Int
intInput = produce go
  where
    go = readMaybe <$> inputEnter [] >>= maybe go return

-- A Button that produces ()
buttonInput :: String -> Producer (Widget HTML) ()
buttonInput s = produce $ button [] (text s)

-- Lets create a Pipe that displays all Integers that go through it
intOutput :: Consumer (Widget HTML) Int
intOutput = pipe (text . show)

-- Test
main3a :: Widget HTML x
main3a = runPipe $ intInput >-> intOutput

-- Okay lets go ahead

-- A Simple Factorial function
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- We lift the factorial function into a Pipe
main3 :: Widget HTML x
main3 = runPipe $ intInput >-> purePipe fac >-> intOutput

-------------------------------------------------------------------------------
-- 4. Factorial function, version 2, improved layout
main4 :: Widget HTML x
main4 = runPipe $ el_ "div" [] intInput >-> purePipe fac >-> el_ "div" [] intOutput

-------------------------------------------------------------------------------
-- 5. Up Counter

-- Just like Widgets, Pipes don't hold state directly. When we need state, we can use the state monad.
-- Here we can keep a count of the number of times the button was clicked
counter :: Monad m => () -> StateT Int m Int
counter () = modify (+1) >> get

-- Now we can pipe everything together
counterPipe :: Pipe (StateT Int (Widget HTML)) Void Void
counterPipe = liftPipe (buttonInput "Up") >-> pipe counter >-> liftPipe intOutput

-- We can even plug in the factorial pipe so that we display successive factorials when the button is clicked
counterFactorialPipe :: Pipe (StateT Int (Widget HTML)) Void Void
counterFactorialPipe = liftPipe (buttonInput "Up") >-> pipe counter >-> purePipe fac >-> liftPipe intOutput

main5 :: Widget HTML Int
main5 = flip execStateT 0 $ runPipe counterPipe

main5a :: Widget HTML Int
main5a = flip execStateT 0 $ runPipe counterFactorialPipe

-------------------------------------------------------------------------------
-- 6. Up/Down Counter

-- Up/Down counting function
countUpDown :: Monad m => Bool -> StateT Int m Int
countUpDown False = modify (+ (-1)) >> get
countUpDown True  = modify (+1) >> get

-- Let's create a widget that has "Up" and "Down" buttons
-- and emits True or False respectively
upDownPipe :: Producer (Widget HTML) Bool
upDownPipe = el "div" []
  [ True  <$ buttonInput "Up"
  , False <$ buttonInput "Down"
  ]

-- Pipe everything together
countUpDownPipe :: Pipe (StateT Int (Widget HTML)) Void Void
countUpDownPipe = liftPipe upDownPipe >-> pipe countUpDown >-> liftPipe intOutput

-- Similar example as the previous, except here we can count up or down
main6 :: Widget HTML Int
main6 = flip execStateT 0 $ runPipe $ countUpDownPipe

-------------------------------------------------------------------------------
-- 7. Up/Down/Reset Counter

-- To handle more states, we can use a custom data type
data CountAction = Up | Down | Reset

-- Up/Down/Reset counting function
countUpDownReset :: Monad m => CountAction -> StateT Int m Int
countUpDownReset Up    = modify (+1) >> get
countUpDownReset Down  = modify (+ (-1)) >> get
countUpDownReset Reset = put 0 >> get

-- Our producer widget is a combination of three widgets
upDownResetPipe :: Producer (Widget HTML) CountAction
upDownResetPipe = el "div" []
  [ Up    <$ buttonInput "Up"
  , Down  <$ buttonInput "Down"
  , Reset <$ buttonInput "Reset"
  ]

-- Pipe everything together
countUpDownResetPipe :: Pipe (StateT Int (Widget HTML)) Void Void
countUpDownResetPipe = liftPipe upDownResetPipe >-> pipe countUpDownReset >-> liftPipe intOutput

-- Similar example as the previous, except here we can count up or down
main7 :: Widget HTML Int
main7 = flip execStateT 0 $ runPipe countUpDownResetPipe

-------------------------------------------------------------------------------
-- EVERYTHING BELOW THIS IS DEBUG

statefulPipe :: a
statefulPipe = undefined

-------------------------------------------------------------------------------
-- 8. Simple (Postfix) Calculator

-- The Calculator has the same basic structure as our previous examples
-- We generate events with composing a set of button widgets
-- , we maintain an internal state (the running total)
-- , and we display the internal state in an output label

-- Our possible actions
data CalculatorAction = Plus | Minus | Times | Div | Enter | Clear | Digit Int

-- Button pad control panel, that produces these actions
calcButtonsPipe :: Producer (Widget HTML) CalculatorAction
calcButtonsPipe = el "div" []
  [ el "div" [] [d 7, d 8, d 9, opDiv]
  , el "div" [] [d 4, d 5, d 6, opTimes]
  , el "div" [] [d 1, d 2, d 3, opMinus]
  , el "div" [] [d 0, ent, cls, opPlus]
  ]
  where
    d n     = but (Digit n) (show n)
    ent     = but Enter "⏎"
    cls     = but Clear "C"
    opDiv   = but Div "/"
    opTimes = but Times "*"
    opMinus = but Minus "-"
    opPlus = but Plus "+"
    but x s = x <$ buttonInput s

-- Our calculator calculation
calc :: Monad m => CalculatorAction -> StateT [Int] m Int
calc (Digit d) = do
  s' <- get
  case s' of
    []    -> new d []
    (n:s) -> new (n*10+d) s
calc Clear = new 0 []
calc Enter = modify (0:) >> return 0
calc Plus = op (+)
calc Minus = op (-)
calc Times = op (*)
calc Div = op div
op :: Monad m => (Int -> Int -> Int) -> StateT [Int] m Int
op f = do
  s' <- get
  case s' of
    (y:x:s) -> new (f x y) s
    _       -> new 0 []
new :: Monad m => Int -> [Int] -> StateT [Int] m Int
new n s = put (n:s) >> return n

-- Pipe everything together as usual
calcPipe :: Pipe (StateT [Int] (Widget HTML)) Void Void
calcPipe = liftPipe calcButtonsPipe >-> pipe calc >-> liftPipe intOutput

-- Again similar main as the previous
main8 :: Widget HTML [Int]
main8 = flip execStateT [] $ runPipe calcPipe

-------------------------------------------------------------------------------
-- Main function
-- Show all widgets
main :: IO ()
main = runWidgetInBody $ forever $ do
  el "div" []
    [ h1 "Your first 8 Concur Pipe Programs"
    , fudgetLink
    , "Hello World" #> main1
    , "Hello World with Quit button" #> main2
    , "Cat" #> main3a
    , "Factorial" #> main3
    , "Factorial + Layout" #> main4
    , "Click Counter" #> void main5
    , "Click + Factorial" #> void main5a
    , "Up/Down Counter" #> void main6
    , "Up/Down/Reset Counter" #> void main7
    , "Postfix Calculator" #> void main8
    ]
  h1 "Application exited" <|> button [] (text "Restart")
  where
    fudgetLink = el "h4" []
      [ text "Inspired from the mighty Fudgets' - "
      , el "a" [vattr "href" "http://www.altocumulus.org/Fudgets/Intro/"] [text "Your first 8 Fudgets Programs"]
      ]
    h1 s = el "h1" [] [text s]
    s #> m = orr [el "h2" [] [text s], m, hr]
    hr = elLeaf "hr" []
