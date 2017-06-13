module Tests where

import IC.TestSuite

import MP hiding (main)

lookUpTestCases
  = [ ("K", [("K", 8), ("B", 12), ("K", 6), ("A", 33)]) ==> [8,6]
    , ("A", [("A", 8), ("B",9), ("C",5), ("A",7)]) ==> [8,7]
    , ("a", []) ==> []
    , ("a", [("a", 9)]) ==> [9]
    , ("a", [("b", 9)]) ==> []
    ]

splitTestCases
  = [ ("a", "whatever")
        ==> ("a",["wh","tever"])
    , (" .,", "A comma, then some words.")
        ==> (" ,   .",["A","comma","","then","some","words",""])
    , ("", "")
        ==> ("", [""])
    , (".", "A.B")
        ==> (".", ["A","B"])
    , (" ", " A")
        ==> (" ", ["", "A"])
    ]

combineTestCases
  = [ ("a",["wh","tever"])
        ==> ["wh","a","tever"]

    , (" ,   .", ["A","comma","","then","some","words",""])
        ==> ["A"," ","comma",",",""," ","then"," ","some"," ","words",".",""]

    , ("", [""])
        ==> [""]
    , (".", ["A","B"])
        ==> ["A",".","B"]
    , (" ", ["", "A"])
        ==> [""," ","A"]
    ]

getKeywordDefsTestCases
  = [ ["$sports basketball, tennis, hockey"]
        ==> [("$sports","basketball, tennis, hockey")]
    , ["$name Jennifer"]
        ==> [("$name","Jennifer")]
    , ["$rule Reproduce this precisely -- or else!!"]
        ==> [("$rule","Reproduce this precisely -- or else!!")]
    , ["$x Define x", "$y 55"]
        ==> [("$x","Define x"),("$y","55")]
    , ["$a A", "$b B", "$c C"]
        ==> [("$a","A"),("$b","B"),("$c","C")]
    , []
        ==> []
    , ["$x-y-z $$$"]
        ==> [("$x-y-z","$$$")]
    , ["$$ something to think about"]
        ==> [("$$","something to think about")]
    , ["$ meanie!"]
        ==> [("$","meanie!")]
    , ["$var  Tristan Allwood"]
        ==> [("$var", " Tristan Allwood")]
    ]

expandTestCases
  = [ ("The winner of $1 is $2", "$1 the marathon\n$2 John Smith.")
        ==> "The winner of the marathon is John Smith."
    ,("The highest $1 in the world is $2", "$1 building\n$2 the Burj Khalifa.")
        ==> "The highest building in the world is the Burj Khalifa."
    , ("The capital of $1 is $2", "$1 Peru\n$2 Lima.")
        ==> "The capital of Peru is Lima."
    , ("The time is $a", "$a now.")
        ==> "The time is now."
    , ("Keywords (e.g. $x, $y, $z...) may appear anywhere, e.g. <$here>.",
       "$x $a\n$y $b\n$z $c\n$here $this-is-one")
        ==> "Keywords (e.g. $a, $b, $c...) may appear anywhere, e.g. <$this-is-one>."
    ]

allTestCases
  = [ TestCase "lookUp"  (uncurry lookUp)
                         lookUpTestCases
    , TestCase "split"   (uncurry split)
                         splitTestCases
    , TestCase "combine" (uncurry combine)
                         combineTestCases

    , TestCase "getKeywordDefs" getKeywordDefs
                                getKeywordDefsTestCases

    , TestCase "expand"  (uncurry expand)
                         expandTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
