-- ProjectSpec.hs
--
--     Author: Fabian Meyer
-- Created on: 15 Feb 2018

module RegexTest where

import RegexTestLib
import Regex.Internal
import ParserCon

runTests = withFeatures myBasics myFeatures

myFeatures :: Features
myFeatures = [
    myParsing,
    mySet,
    myAny,
    myRep,
    myMany,
    myMatch
    ]

myParsing :: Feature
myParsing = Parsing show compile

mySet :: Feature
mySet = Set compSet
    where compSet s = compileTrusted compilerBracket s

myAny :: Feature
myAny = Any regexAny

myRep :: Feature
myRep = Rep genRep
    where genRep n1 mn2 reg = regexGenRep reg n1 mn2

myMany :: Feature
myMany = Many regexSome

myMatch :: Feature
myMatch = Match match2bool

match2bool :: Regex -> String -> Bool
match2bool reg = maybe2bool . matchExact reg

maybe2bool :: Maybe a -> Bool
maybe2bool (Just _) = True
maybe2bool Nothing  = False

myBasics :: Basics
myBasics = Basics myAtom mySeq myAlt myStar

myAtom :: Char -> Regex
myAtom c = compileTrusted compilerAtom [c]

mySeq :: Regex -> Regex -> Regex
mySeq re1 re2 = (++) <$> re1 <*> re2

myAlt :: Regex -> Regex -> Regex
myAlt re1 re2 = re1 <|> re2

myStar :: Regex -> Regex
myStar re = concat <$> pmany re
