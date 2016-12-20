module Sheep
where

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


