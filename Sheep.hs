module Sheep
where
import Control.Monad
import Data.Maybe

data Sheep = Sheep { 
                        father :: Maybe Sheep,
                        mother :: Maybe Sheep
                   } deriving (Show)
                    
clonedSheep :: Sheep
clonedSheep = Sheep Nothing Nothing

sheepWithAFather :: Sheep
sheepWithAFather = Sheep { father = Just clonedSheep, mother = Nothing }

sheepWithAMother :: Sheep
sheepWithAMother = Sheep { mother = Just clonedSheep, father = Nothing }

sheepWithIdenticalParents :: Sheep
sheepWithIdenticalParents = Sheep (Just clonedSheep) (Just clonedSheep)

sheepWithDifferentParents :: Sheep
sheepWithDifferentParents = Sheep { mother = Just sheepWithAFather, father = Just sheepWithAMother }

sheepWithAncestry :: Sheep
sheepWithAncestry = Sheep { mother = Just sheepWithDifferentParents, father = Just sheepWithIdenticalParents }

descendentSheep :: Sheep
descendentSheep = Sheep { mother = Just sheepWithAncestry, father = Nothing }

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather sheep = return sheep >>= mother >>= father

fathersMaternalGrandfather :: Sheep -> Maybe Sheep
fathersMaternalGrandfather sheep = return sheep >>= father >>= maternalGrandfather

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather sheep = return sheep >>= mother >>= father >>= father

parent :: Sheep -> Maybe Sheep
parent sheep = (mother sheep) `mplus` (father sheep)

grandparent :: Sheep -> Maybe Sheep
grandparent sheep = ((mother sheep) >>= parent) `mplus` ((father sheep) >>= parent)

parents :: Sheep -> [Sheep]
parents sheep = (maybeToList (father sheep)) `mplus` (maybeToList (mother sheep))

grandparents :: Sheep -> [Sheep]
grandparents sheep = return sheep >>= parents >>= parents

parent' :: (MonadPlus m) => Sheep -> m Sheep
parent' sheep = (toMonad (father sheep)) `mplus` (toMonad (mother sheep))

grandparent' :: (MonadPlus m) => Sheep -> m Sheep
grandparent' sheep = ((toMonad mother sheep) >>= parent') `mplus` (toMonad (father sheep)) >>= parent'

toMonad :: (MonadPlus m) => Maybe Sheep -> m Sheep
toMonad (Just sheep) = return sheep
toMonad Nothing = mzero


