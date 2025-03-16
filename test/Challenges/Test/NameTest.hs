module Challenges.Test.NameTest where

import Hedgehog (TestT, (===))

import Challenges (name)

test_name :: TestT IO ()
test_name = "challenges" === name
