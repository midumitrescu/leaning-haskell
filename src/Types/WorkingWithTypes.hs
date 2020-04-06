module Types.WorkingWithTypes where

data Customer = Customer String String Double
                deriving (Show, Eq)

-- Define Customer and its values
tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main St" 20.50

nameOf :: Customer -> String
nameOf (Customer name _ _)  = name