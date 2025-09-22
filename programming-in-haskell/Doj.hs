module Doj where
import Test.QuickCheck


test :: Bool -> Bool
test x = x == x

prop_test = test True
