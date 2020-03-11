module Data (Slider, MyRetro (..), PastMonths (..), Project (..)) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

newtype Slider = MkSlider Int
  deriving stock (Show)
  deriving newtype (Ord, Eq, ToJSON)

instance Bounded Slider where
  minBound = MkSlider 1
  maxBound = MkSlider 10

instance Enum Slider where
  toEnum x | x >= 1 && x <= 10 = MkSlider x
  toEnum _ = error "Enum out of bounds"

  fromEnum (MkSlider x) = x

data MyRetro a
  = MkMyRetro
      {pastMonths :: PastMonths a}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data PastMonths a
  = MkPastMonths
      { numMonths :: Int,
        projects :: [Project a],
        sliderProjects :: Slider,
        sliderImpact :: Slider,
        sliderRole :: Slider,
        sliderGrowth :: Slider,
        sliderAppreciation :: Slider,
        sliderLife :: Slider
      }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Project a
  = MkProject
      { done :: a,
        happiness :: a,
        yourImpact :: a,
        learnt :: a,
        feedback :: a
      }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
