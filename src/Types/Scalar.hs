module Types.Scalar where

import Data.Number.Fixed
import Data.Ratio
import Utils

data SscNumber = SscInteger Integer | SscRational Rational | SscReal (Fixed Prec50)

instance Show SscNumber where
  show (SscInteger i) = show i
  show (SscRational i) = concat [show $ numerator i, "/", show $ denominator i]
  show (SscReal i) = (trimEnd (== '.') . trimEnd (== '0') . show) i

instance Num SscNumber where
  (SscInteger a) + (SscInteger b) = SscInteger $ a + b
  (SscInteger a) + (SscRational b) = SscRational $ toRational a + b
  (SscInteger a) + (SscReal b) = SscReal $ fromIntegral a + b
  (SscRational a) + (SscRational b) = SscRational $ a + b
  (SscRational a) + (SscReal b) = SscReal $ fromRational a + b
  (SscReal a) + (SscReal b) = SscReal $ a + b
  a + b = b + a

  negate (SscInteger a) = SscInteger $ negate a
  negate (SscRational a) = SscRational $ negate a
  negate (SscReal a) = SscReal $ negate a

  (SscInteger a) * (SscInteger b) = SscInteger $ a * b
  (SscInteger a) * (SscRational b) = SscRational $ toRational a * b
  (SscInteger a) * (SscReal b) = SscReal $ fromIntegral a * b
  (SscRational a) * (SscRational b) = SscRational $ a * b
  (SscRational a) * (SscReal b) = SscReal $ fromRational a * b
  (SscReal a) * (SscReal b) = SscReal $ a * b
  a * b = b * a

  abs (SscInteger a) = SscInteger $ abs a
  abs (SscRational a) = SscRational $ abs a
  abs (SscReal a) = SscReal $ abs a

  signum (SscInteger a) = SscInteger $ signum a
  signum (SscRational a) = SscRational $ signum a
  signum (SscReal a) = SscReal $ signum a

  fromInteger i = SscInteger i
