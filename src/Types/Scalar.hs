module Types.Scalar where

import Data.Number.Fixed
import Data.Ratio
import Utils

data SscNumber = SscInteger Integer | SscRational Rational | SscReal (Fixed Prec50)

instance Show SscNumber where
  show (SscInteger i) = show i
  show (SscRational i) = concat [show $ numerator i, "/", show $ denominator i]
  show (SscReal i) = trimEnd (== '0') $ show i

instance Num SscNumber where
  (SscInteger a) + (SscInteger b) = SscInteger $ a + b

  (SscInteger a) + (SscRational b) = SscRational $ toRational a + b
  (SscRational a) + (SscInteger b) = SscInteger b + SscRational a

  (SscInteger a) + (SscReal b) = SscReal $ fromIntegral a + b
  (SscReal a) + (SscInteger b) = SscInteger b + SscReal a

  (SscRational a) + (SscRational b) = SscRational $ a + b

  (SscRational a) + (SscReal b) = SscReal $ fromRational a + b
  (SscReal a) + (SscRational b) = SscRational b + SscReal a

  (SscReal a) + (SscReal b) = SscReal $ a + b

  (SscInteger a) - (SscInteger b) = SscInteger $ a - b

  (SscInteger a) - (SscRational b) = SscRational $ toRational a - b
  (SscRational a) - (SscInteger b) = SscInteger b - SscRational a

  (SscInteger a) - (SscReal b) = SscReal $ fromIntegral a - b
  (SscReal a) - (SscInteger b) = SscInteger b - SscReal a

  (SscRational a) - (SscRational b) = SscRational $ a - b

  (SscRational a) - (SscReal b) = SscReal $ fromRational a - b
  (SscReal a) - (SscRational b) = SscRational b - SscReal a

  (SscReal a) - (SscReal b) = SscReal $ a - b

  (SscInteger a) * (SscInteger b) = SscInteger $ a * b

  (SscInteger a) * (SscRational b) = SscRational $ toRational a * b
  (SscRational a) * (SscInteger b) = SscInteger b * SscRational a

  (SscInteger a) * (SscReal b) = SscReal $ fromIntegral a * b
  (SscReal a) * (SscInteger b) = SscInteger b * SscReal a

  (SscRational a) * (SscRational b) = SscRational $ a * b

  (SscRational a) * (SscReal b) = SscReal $ fromRational a * b
  (SscReal a) * (SscRational b) = SscRational b * SscReal a

  (SscReal a) * (SscReal b) = SscReal $ a * b

  abs (SscInteger a) = SscInteger $ abs a
  abs (SscRational a) = SscRational $ abs a
  abs (SscReal a) = SscReal $ abs a

  signum (SscInteger a) = SscInteger $ signum a
  signum (SscRational a) = SscRational $ signum a
  signum (SscReal a) = SscReal $ signum a

  fromInteger i = SscInteger i

data SscComplex = SscComplex { real :: SscNumber, complex :: SscNumber }
