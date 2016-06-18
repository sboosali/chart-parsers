{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Chart.Example where
import Chart()

import qualified Data.Dependent.Map as D
import Data.Dependent.Map (DMap,DSum(..))
import Data.Dependent.Sum (ShowTag(..),(==>))
import Data.GADT.Show
import Data.GADT.Compare

import Data.Functor.Identity

--------------------------------------------------------------------------------

data Tag a where
   String :: Tag String
   Int    :: Tag Int

instance GEq Tag where
    geq String String       = Just Refl
    geq Int    Int          = Just Refl
    geq _       _           = Nothing

instance GCompare Tag where
    gcompare String String  = GEQ
    gcompare String _       = GLT
    gcompare _       String = GGT

    gcompare Int Int        = GEQ
    gcompare Int   _        = GLT
    gcompare _     Int      = GGT

instance GShow Tag where
    gshowsPrec = showsPrec

instance Eq (Tag a) where
    (==) = defaultEq

instance Show (Tag a) where
    showsPrec _ String      = showString "String"
    showsPrec _ Int         = showString "Int"

instance ShowTag Tag Identity where
    showTaggedPrec String p = showsPrec p . runIdentity
    showTaggedPrec Int    p = showsPrec p . runIdentity

string x = String :=> (Identity x)

int x = Int :=> (Identity x)

--------------------------------------------------------------------------------

d :: DMap Tag Identity
d = D.fromList
 [ String ==> "hello!"
 , Int    ==> 42
 ]

toString :: DSum Tag Identity -> String
toString (String :=> (Identity s)) = s
toString (Int    :=> (Identity i)) = show i
--TODO pattern t :> x = t :=> (Identity x)

--------------------------------------------------------------------------------

{-|
@
stack build && stack exec -- example-chart-parsers
@
-}
main :: IO ()
main = do
 putStrLn "(Chart.Example...)"
 print d


--------------------------------------------------------------------------------
