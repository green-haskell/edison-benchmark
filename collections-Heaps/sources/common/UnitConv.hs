module UnitConv where

import Data.List

data Unit =
      Yocto
    | Zepto
    | Atto
    | Femto
    | Pico
    | Nano
    | Micro
    | Milli
    | Centi
    | Deci
    | Unit
    | Deca
    | Hecto
    | Kilo
    | Mega
    | Giga
    | Tera
    | Peta
    | Exa
    | Zetta
    | Yotta
    deriving ( Show , Eq , Ord , Enum )


conversionsTable :: [ ( ( Unit , Unit ) , Int ) ]
conversionsTable = [
-- ( inputUnit , outputUnit ) , powerOfTen )
      ( ( Zepto , Yocto ) , 3 )
    , ( ( Atto  , Zepto ) , 3 )
    , ( ( Femto , Atto  ) , 3 )
    , ( ( Pico  , Femto ) , 3 )
    , ( ( Nano  , Pico  ) , 3 )
    , ( ( Micro , Nano  ) , 3 )
    , ( ( Milli , Micro ) , 3 )
    , ( ( Centi , Milli ) , 1 )
    , ( ( Deci  , Centi ) , 1 )
    , ( ( Unit  , Deci  ) , 1 )

    , ( ( Deca  , Unit  ) , 1 )
    , ( ( Hecto , Deca  ) , 1 )
    , ( ( Kilo  , Hecto ) , 1 )
    , ( ( Mega  , Kilo  ) , 3 )
    , ( ( Giga  , Mega  ) , 3 )
    , ( ( Tera  , Giga  ) , 3 )
    , ( ( Peta  , Tera  ) , 3 )
    , ( ( Exa   , Peta  ) , 3 )
    , ( ( Zetta , Exa   ) , 3 )
    , ( ( Yotta , Zetta ) , 3 )
    ]


convert :: Fractional a => a -> Unit -> Unit -> a
convert value inputUnit outputUnit
    | inputUnit == outputUnit = value
    | otherwise = foldl f value ( numConvL ( conversions inputUnit outputUnit ) )
    where
        f ac ( Just exp ) =  if inputUnit < outputUnit then ac * ( 10 ^^ negate exp ) else ac * ( 10 ^ exp )


numConvL :: [ ( Unit , Unit ) ] -> [ Maybe Int ]
numConvL cList = map ( flip lookup conversionsTable ) . map f $ cList
    where
        f uP@( iU , oU ) = if iU > oU then uP else ( oU , iU )


conversions :: ( Enum t , Ord t ) => t -> t -> [ ( t , t ) ]
conversions inputUnit outputUnit
    | inputUnit == outputUnit = []
    | otherwise = convsList inputUnit ( unitsList inputUnit outputUnit )


convsList :: t -> [ t ] -> [ ( t , t ) ]
convsList iU [] = []
convsList iU [ x ] = [ ( iU , x ) ]
convsList iU ( x : xs ) = ( iU , x ) : convsList x xs


unitsList :: ( Enum a , Ord a ) => a -> a -> [ a ]
unitsList inputUnit outputUnit
    | inputUnit == outputUnit = []
    | otherwise = if inputUnit < outputUnit then
            tail [ inputUnit .. outputUnit ]
        else
            tail . reverse $ [ outputUnit .. inputUnit ]


