{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Metrology.SI.Prefixes.Attoparsec.Text
  ( decaP
  , hectoP
  , kiloP
  , megaP
  , gigaP
  , teraP
  , petaP
  , exaP
  , zettaP
  , yottaP
  , deciP
  , centiP
  , milliP
  , microP
  , nanoP
  , picoP
  , femtoP
  , attoP
  , zeptoP
  , yoctoP
  ) where

import Control.Applicative
import Data.Attoparsec.Text hiding (Number)
import Data.Metrology
import Data.Metrology.SI.Dims
import Data.Metrology.SI.Prefixes
  ( Deca(..)
  , Hecto(..)
  , Kilo(..)
  , Mega(..)
  , Giga(..)
  , Tera(..)
  , Peta(..)
  , Exa(..)
  , Zetta(..)
  , Yotta(..)
  , Deci(..)
  , Centi(..)
  , Milli(..)
  , Micro(..)
  , Nano(..)
  , Pico(..)
  , Femto(..)
  , Atto(..)
  , Zepto(..)
  , Yocto(..)
  )
import Data.Metrology.SI.Units

a >~  b = char a   >> return b
a >>~ b = string a >> return b

decaP :: Parser Deca
decaP = "da" >>~ Deca

hectoP :: Parser Hecto
hectoP = 'h' >~ Hecto

kiloP :: Parser Kilo
kiloP = 'k' >~ Kilo

megaP :: Parser Mega
megaP = 'M' >~ Mega

gigaP :: Parser Giga
gigaP = 'G' >~ Giga

teraP :: Parser Tera
teraP = 'T' >~ Tera

petaP :: Parser Peta
petaP = 'P' >~ Peta

exaP :: Parser Exa
exaP = 'E' >~ Exa

zettaP :: Parser Zetta
zettaP = 'Z' >~ Zetta

yottaP :: Parser Yotta
yottaP = 'Y' >~ Yotta

deciP :: Parser Deci
deciP = 'd' >~ Deci

centiP :: Parser Centi
centiP = 'c' >~ Centi

milliP :: Parser Milli
milliP = 'm' >~ Milli

microP :: Parser Micro
microP = (char 'μ' <|> char 'u') >> return Micro

nanoP :: Parser Nano
nanoP = 'n' >~ Nano

picoP :: Parser Pico
picoP = 'p' >~ Pico

femtoP :: Parser Femto
femtoP = 'f' >~ Femto

attoP :: Parser Atto
attoP = 'a' >~ Atto

zeptoP :: Parser Zepto
zeptoP = 'z' >~ Zepto

yoctoP :: Parser Yocto
yoctoP = 'y' >~ Yocto

