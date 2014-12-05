-- Like HXT but not a mess
module XMLArrow where

import Control.Arrow

import qualified Text.XML.Light.Input as Input
import Text.XML.Light.Types

type LA = Kleisli []
type ContentLA = LA Content Content

-- Predicates
isElem :: Content -> Bool
isElem x = case x of Elem _ -> True
                     _      -> False

hasName :: String -> Content -> Bool
hasName s x = case x of Elem Element{elName = QName{qName = t}} -> s == t
                        _                                       -> False

hasAttr :: String -> Content -> Bool
hasAttr s x = runKleisli (getAttr s) x /= []

-- Arrows
filterBy :: (a -> Bool) -> LA a a
filterBy p = Kleisli f where
    f x = [x | p x]

orElse :: ContentLA -> ContentLA -> ContentLA
orElse a b = Kleisli f where
    f x = let res = runKleisli a x in
              case res of [] -> runKleisli b x
                          _  -> res

deep :: ContentLA -> ContentLA
deep f = f `orElse` (getChildren >>> deep f)

atTag :: String -> ContentLA
atTag s = deep (filterBy isElem >>> filterBy (hasName s))

getChildren :: ContentLA
getChildren = Kleisli f where
    f x = case x of Elem Element{elContent = children} -> children
                    _                                  -> []

getAttr :: String -> LA Content String
getAttr s = Kleisli f where
  f x = case x of Elem Element{elAttribs = attrs} -> 
                      [attrVal a | a<-attrs, qName (attrKey a) == s]
                  _                               -> []

text :: LA Content String
text = getChildren >>> Kleisli f where
    f x = case x of Text CData{cdData = s} -> [s]
                    _                      -> []

parseXML :: LA String Content
parseXML = Kleisli Input.parseXML

findOne :: LA a b -> LA a b
findOne (Kleisli f) = Kleisli (take 1 . f)
