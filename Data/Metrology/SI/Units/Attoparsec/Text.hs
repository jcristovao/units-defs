{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, MultiParamTypeClasses,
             ConstraintKinds, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ScopedTypeVariables, TypeOperators, PolyKinds #-}

module Data.Metrology.SI.Units.Attoparsec.Text
 ( ParseUnit(..)
 , gramP
 , meterP
 , metreP
 , secondP
 , minuteP
 , hourP
 , dayP
 , parseTime
 )where

import Control.Applicative
import Data.Attoparsec.Text hiding (Number)
import Data.Metrology
import Data.Metrology.SI
import qualified Data.Metrology.SI.Dims as Dims
import Data.Metrology.SI.Prefixes.Attoparsec.Text

class ParseUnit g i where
  parseUnit :: Parser g -> Parser i


build :: ( Subset (CanonicalUnitsOfFactors (UnitFactorsOf unit)) (CanonicalUnitsOfFactors (LookupList (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU))
        , Subset (CanonicalUnitsOfFactors (LookupList (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU)) (CanonicalUnitsOfFactors (UnitFactorsOf unit))
        , Unit unit
        , UnitFactor (LookupList (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU)
        , Fractional n)
      => n
      -> Parser a
      -> (a -> unit)
      -> Parser (Qu (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU n)
build v g p = (\u' -> v %% p u') <$> (skipSpace >> g)

{-parseUnit' :: ( Subset (CanonicalUnitsOfFactors (UnitFactorsOf unit)) (CanonicalUnitsOfFactors (LookupList (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU))-}
              {-, Subset (CanonicalUnitsOfFactors (LookupList (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU)) (CanonicalUnitsOfFactors (UnitFactorsOf unit))-}
              {-, Dimension (DimOfUnit unit)-}
              {-, Unit unit-}
              {-, UnitFactor (LookupList (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU)-}
              {-, Fractional n-}
              {-, Data.Type.Equality.EqStar unit Canonical ~ 'False)-}
            {-=> Parser unit-}
            {--> n-}
            {--> Parser (Qu (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU n)-}
parseUnit' g = skipSpace >> double >>= (\v ->
   (  (skipSpace >> decaP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> hectoP >>= return . (:@) >>= build v g)
  <|> (skipSpace >> kiloP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> megaP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> gigaP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> teraP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> petaP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> exaP   >>= return . (:@) >>= build v g)
  <|> (skipSpace >> zettaP >>= return . (:@) >>= build v g)
  <|> (skipSpace >> yottaP >>= return . (:@) >>= build v g)
  <|> (skipSpace >> deciP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> centiP >>= return . (:@) >>= build v g)
  <|> (skipSpace >> milliP >>= return . (:@) >>= build v g)
  <|> (skipSpace >> microP >>= return . (:@) >>= build v g)
  <|> (skipSpace >> nanoP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> picoP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> femtoP >>= return . (:@) >>= build v g)
  <|> (skipSpace >> attoP  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> zeptoP >>= return . (:@) >>= build v g)
  <|> (skipSpace >> yoctoP >>= return . (:@) >>= build v g)
  -- No prefix
  <|> (skipSpace >> (\u' -> v %% u') <$> (skipSpace >> g))
   ))


-- | Since the parser code is highly repetitive, let save some characters
(>~) :: Char -> b -> Parser b
a >~  b = char a   >> return b
{-(>>~):: Text -> b  -> Parser b-}
a >>~ b = string a >> return b

meterP :: Parser Meter
meterP = 'm' >~ Meter

metreP :: Parser Meter
metreP = meterP

instance ParseUnit Meter (Qu '[F Dims.Length One] 'DefaultLCSU Double) where
  parseUnit g = parseUnit' g

gramP :: Parser Gram
gramP = 'g' >~ Gram

instance ParseUnit Gram (Qu '[F Dims.Mass One] 'DefaultLCSU Double) where
  parseUnit g = parseUnit' g

secondP :: Parser Second
secondP = 's' >~ Second

instance ParseUnit Second (Qu '[F Dims.Time One] 'DefaultLCSU Double) where
  parseUnit g = parseUnit' g

minuteP :: Parser Minute
minuteP = "min" >>~ Minute

instance ParseUnit Minute (Qu '[F Dims.Time One] 'DefaultLCSU Double) where
  parseUnit g = parseUnit' g

hourP :: Parser Hour
hourP = 'h' >~ Hour

instance ParseUnit Hour (Qu '[F Dims.Time One] 'DefaultLCSU Double) where
  parseUnit g = parseUnit' g

dayP :: Parser Day
dayP = 'd' >~ Day

instance ParseUnit Day (Qu '[F Dims.Time One] 'DefaultLCSU Double) where
  parseUnit g = parseUnit' g

parseTime :: Parser Time
parseTime =  parseUnit secondP
         <|> parseUnit minuteP
         <|> parseUnit hourP
         <|> parseUnit dayP

ampereP :: Parser Ampere
ampereP = 'A' >~ Ampere

kelvinP :: Parser Kelvin
kelvinP = 'k' >~ Kelvin

moleP :: Parser Mole
moleP = "mol" >>~ Mole

candelaP :: Parser Candela
candelaP = "cd" >>~ Candela

hertzP :: Parser Hertz
hertzP = "Hz" >>~ Hertz

literP :: Parser Liter
literP = 'l' >~ Liter

litreP :: Parser Liter
litreP = literP

newtonP :: Parser Newton
newtonP = 'N' >~ Newton

pascalP :: Parser Pascal
pascalP = "Pa" >>~ Pascal

jouleP :: Parser Joule
jouleP = 'J' >~ Joule

wattP :: Parser Watt
wattP = 'W' >~ Watt

coloumbP :: Parser Coulomb
coloumbP = 'C' >~ Coulomb

voltP :: Parser Volt
voltP = 'V' >~ Volt

faradP :: Parser Farad
faradP = 'F' >~ Farad

ohmP :: Parser Ohm
ohmP = 'Ω' >~ Ohm

siemensP :: Parser Siemens
siemensP = 'S' >~ Siemens

weberP :: Parser Weber
weberP = "Wb" >>~ Weber

teslaP :: Parser Tesla
teslaP = 'T' >~ Tesla

henryP :: Parser Henry
henryP = 'H' >~ Henry

lumenP :: Parser Lumen
lumenP = "lm" >>~ Lumen

luxP :: Parser Lux
luxP = "lx" >>~ Lux

becquerelP :: Parser Becquerel
becquerelP = "Bq" >>~ Becquerel

grayP :: Parser Gray
grayP = "Gy" >>~ Gray

sievertP :: Parser Sievert
sievertP = "Sv" >>~ Sievert

katalP :: Parser Katal
katalP = "kat" >>~ Katal

hectareP :: Parser Hectare
hectareP = "ha" >>~ Hectare

tonP :: Parser Ton
tonP = 't' >~ Ton

tonneP :: Parser Ton
tonneP = tonP
