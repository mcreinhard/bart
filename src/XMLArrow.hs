-- Like HXT but not a mess
module XMLArrow where

import Text.XML.Light.Types
import qualified Text.XML.Light.Input as Input
import Control.Arrow

type XMLArrow = Kleisli [] Content Content

filterBy :: (Content -> Bool) -> XMLArrow
filterBy p = Kleisli $ \c -> [c | p c]

getChildren :: XMLArrow
getChildren = Kleisli $ \c -> case c of
                                Elem Element{elContent = children} -> children
                                _                                  -> []

orElse :: XMLArrow -> XMLArrow -> XMLArrow
orElse f g = Kleisli $ \c -> let res = runKleisli f c in
                               case res of [] -> runKleisli g c
                                           _  -> res

deep :: XMLArrow -> XMLArrow
deep f = f `orElse` (getChildren >>> deep f)

isElem :: XMLArrow
isElem = filterBy $ \c -> case c of
                            Elem _ -> True
                            _      -> False

hasName :: String -> XMLArrow
hasName s = filterBy $ \c -> case c of
                               Elem Element{elName = QName{qName = t}} -> s == t
                               _ -> False

atTag :: String -> XMLArrow
atTag s = deep (isElem >>> hasName s)

text :: Kleisli [] Content String
text = getChildren >>> (Kleisli $ \c -> case c of
                                            Text CData{cdData = s} -> [s]
                                            _                      -> [])

parseXML :: Kleisli [] String Content
parseXML = Kleisli Input.parseXML
