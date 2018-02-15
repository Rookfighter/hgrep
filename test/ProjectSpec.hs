-- ProjectSpec.hs
--
--     Author: Fabian Meyer
-- Created on: 15 Feb 2018

module ProjectSpec where

import RegexTestLib
import Regex.Internal
import ParserCon

runTests = withFeatures myBasics myFeatures

myFeatures :: Features Regex
myFeatures = [
    myParsing,
    mySet,
    myAny,
    myRep,
    myMany,
    myMatch
    ]

myParsing :: Feature Regex
myParsing = Parsing show compile

mySet :: Feature Regex
mySet = Set compSet
    where compSet s = compileTrusted compilerBracketInner s

myAny :: Feature Regex
myAny = Any (Regex regexAny ".")

myRep :: Feature Regex
myRep = Rep genRep
    where genRep n1 mn2 (Regex reg s) = Regex (regexGenRep reg n1 mn2) (s ++ "{" ++ show n1 ++ "," ++ (m2s mn2) ++ "}")
          m2s (Just n) = show n
          m2s Nothing  = ""

myMany :: Feature Regex
myMany = Many g
    where g (Regex reg s) = Regex (regexSome reg) s

myMatch :: Feature Regex
myMatch = Match match2bool

match2bool :: Regex -> String -> Bool
match2bool reg = maybe2bool . matchExact reg

maybe2bool :: Maybe a -> Bool
maybe2bool (Just _) = True
maybe2bool Nothing  = False

myBasics :: Basics Regex
myBasics = Basics myAtom mySeq myAlt myStar

myAtom :: Char -> Regex
myAtom c = Regex (return <$> lit c) [c]

mySeq :: Regex -> Regex -> Regex
mySeq (Regex re1 s1) (Regex re2 s2) = Regex (regexSeq re1 re2) (s1 ++ s2)

myAlt :: Regex -> Regex -> Regex
myAlt (Regex re1 s1) (Regex re2 s2) = Regex (re1 <|> re2) ("(" ++ s1 ++ ")|(" ++ s2 ++ ")")

myStar :: Regex -> Regex
myStar (Regex re s) = Regex (regexMany re) ("(" ++ s ++ ")*")
