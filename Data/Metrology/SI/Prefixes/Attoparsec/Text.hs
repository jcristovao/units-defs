{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Metrology.SI.Prefixes.Attoparsec.Text
  ( deca
  , hecto
  , kilo
  , mega
  , giga
  , tera
  , peta
  , exa
  , zetta
  , yotta
  , deci
  , centi
  , milli
  , micro
  , nano
  , pico
  , femto
  , atto
  , zepto
  , yocto
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

deca :: Parser Deca
deca = "da" >>~ Deca

hecto :: Parser Hecto
hecto = 'h' >~ Hecto

kilo :: Parser Kilo
kilo = 'k' >~ Kilo

mega :: Parser Mega
mega = 'M' >~ Mega

giga :: Parser Giga
giga = 'G' >~ Giga

tera :: Parser Tera
tera = 'T' >~ Tera

peta :: Parser Peta
peta = 'P' >~ Peta

exa :: Parser Exa
exa = 'E' >~ Exa

zetta :: Parser Zetta
zetta = 'Z' >~ Zetta

yotta :: Parser Yotta
yotta = 'Y' >~ Yotta

deci :: Parser Deci
deci = 'd' >~ Deci

centi :: Parser Centi
centi = 'c' >~ Centi

milli :: Parser Milli
milli = 'm' >~ Milli

micro :: Parser Micro
micro = (char 'μ' <|> char 'u') >> return Micro

nano :: Parser Nano
nano = 'n' >~ Nano

pico :: Parser Pico
pico = 'p' >~ Pico

femto :: Parser Femto
femto = 'f' >~ Femto

atto :: Parser Atto
atto = 'a' >~ Atto

zepto :: Parser Zepto
zepto = 'z' >~ Zepto

yocto :: Parser Yocto
yocto = 'y' >~ Yocto

{-pars :: UnitPrefix a => Parser a-}
{-pars = yocto <|> zepto-}
