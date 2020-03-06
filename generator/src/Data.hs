module Data (Slider, MyRetro (..), PastMonths (..), Project (..)) where

import Text.Pandoc (Pandoc (..))

newtype Slider = Slider Int
  deriving stock (Show)
  deriving newtype (Ord, Eq)

instance Bounded Slider where
  minBound = Slider 1
  maxBound = Slider 10

instance Enum Slider where
  toEnum x | x >= 1 && x <= 10 = Slider x
  toEnum _ = error "Enum out of bounds"

  fromEnum (Slider x) = x

data MyRetro = MkMyRetro PastMonths
  deriving stock (Show)

data PastMonths
  = MkPastMonths
      { numMonths :: Int,
        projects :: [Project],
        sliderProject :: Slider,
        sliderImpact :: Slider,
        sliderRole :: Slider,
        sliderGrowth :: Slider,
        sliderAppreciation :: Slider,
        sliderLife :: Slider
      }
  deriving stock (Show)

data Project
  = MkProject
      { done :: Pandoc,
        happiness :: Pandoc,
        yourImpact :: Pandoc,
        learnt :: Pandoc,
        feedback :: Pandoc
      }
  deriving stock (Show)
