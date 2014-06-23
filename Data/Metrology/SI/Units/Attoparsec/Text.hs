{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, MultiParamTypeClasses,
             ConstraintKinds, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ScopedTypeVariables, TypeOperators, PolyKinds #-}

{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving,
             RoleAnnotations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Metrology.SI.Units.Attoparsec.Text
 ( ParseUnit(..)

 )where

import Control.Applicative
import Data.Attoparsec.Text hiding (Number)
import Data.Metrology
import Data.Metrology.SI
import qualified Data.Metrology.SI.Dims as Dims
import qualified Data.Metrology.SI.Prefixes.Attoparsec.Text as PAT

class ParseUnit g i where
  parseUnit :: Parser g -> Parser i

instance ParseUnit Gram (Qu '[F Dims.Mass One] 'DefaultLCSU Double) where
  parseUnit g = skipSpace >> double >>= parseUnit' g

instance ParseUnit Second (Qu '[F Dims.Time One] 'DefaultLCSU Double) where
  parseUnit g = skipSpace >> double >>= parseUnit' g


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
parseUnit' g v =
      (skipSpace >> PAT.deca  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.hecto >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.kilo  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.mega  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.giga  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.tera  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.peta  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.exa   >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.zetta >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.yotta >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.deci  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.centi >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.milli >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.micro >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.nano  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.pico  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.femto >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.atto  >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.zepto >>= return . (:@) >>= build v g)
  <|> (skipSpace >> PAT.yocto >>= return . (:@) >>= build v g)
  -- No prefix
  <|> (skipSpace >> (\u' -> v %% u') <$> (skipSpace >> g))


-- | Since the parser code is highly repetitive, let save some characters
(>~) :: Char -> b -> Parser b
a >~  b = char a   >> return b
{-(>>~):: Text -> b  -> Parser b-}
a >>~ b = string a >> return b

meter :: Parser Meter
meter = 'm' >~ Meter

metre :: Parser Meter
metre = meter

gram :: Parser Gram
gram = 'g' >~ Gram

second :: Parser Second
second = 's' >~ Second

minute :: Parser Minute
minute = "min" >>~ Minute

hour :: Parser Hour
hour = 'h' >~ Hour

day :: Parser Day
day = 'd' >~ Day

ampere :: Parser Ampere
ampere = 'A' >~ Ampere

kelvin :: Parser Kelvin
kelvin = 'k' >~ Kelvin

mole :: Parser Mole
mole = "mol" >>~ Mole

candela :: Parser Candela
candela = "cd" >>~ Candela

hertz :: Parser Hertz
hertz = "Hz" >>~ Hertz

liter :: Parser Liter
liter = 'l' >~ Liter

litre :: Parser Liter
litre = liter

newton :: Parser Newton
newton = 'N' >~ Newton

pascal :: Parser Pascal
pascal = "Pa" >>~ Pascal

joule :: Parser Joule
joule = 'J' >~ Joule

watt :: Parser Watt
watt = 'W' >~ Watt

coloumb :: Parser Coulomb
coloumb = 'C' >~ Coulomb

volt :: Parser Volt
volt = 'V' >~ Volt

farad :: Parser Farad
farad = 'F' >~ Farad

ohm :: Parser Ohm
ohm = 'Ω' >~ Ohm

siemens :: Parser Siemens
siemens = 'S' >~ Siemens

weber :: Parser Weber
weber = "Wb" >>~ Weber

tesla :: Parser Tesla
tesla = 'T' >~ Tesla

henry :: Parser Henry
henry = 'H' >~ Henry

lumen :: Parser Lumen
lumen = "lm" >>~ Lumen

lux :: Parser Lux
lux = "lx" >>~ Lux

becquerel :: Parser Becquerel
becquerel = "Bq" >>~ Becquerel

gray :: Parser Gray
gray = "Gy" >>~ Gray

sievert :: Parser Sievert
sievert = "Sv" >>~ Sievert

katal :: Parser Katal
katal = "kat" >>~ Katal

hectare :: Parser Hectare
hectare = "ha" >>~ Hectare

ton :: Parser Ton
ton = 't' >~ Ton

tonne :: Parser Ton
tonne = ton
