module Units exposing
    ( Prefix, UnitSymbol, Unit, Error
    , createUnitSymbol
    , emptyUnit, u, pu, fromList, times, per, toThe, square, cubic, inverse, paren, simplifyUnitExpression
    , ValidatedUnitSymbols, emptyUnitSymbols, defaultUnitSymbols, addUnitSymbols
    , toParser, failureParser, parse, Parser, DeadEnd
    , toString
    , DisplayOption, defaultDisplayOption, defaultDisplayOptionForTex, inAscii, inTex, inUnicode, setSeparator, setSlash, setHeadingSlash, setParen, setEmptyUnitString, displayTopNumeratorOne, deleteTopNumeratorOne, useTopLevelTexFrac, doNotUseTopLevelTexFrac, useCustomSpacing, doNotUseCustomSpacing
    , hasSameDimension, Quantity, convertInto, normalize, add, subtractBy, multiplyBy, divideBy, applySequentially
    , meter, gram, second, kelvin, ampere, mole, candela, ton, minute, hour, day, year, newton, pascal, atmosphere, millimeterOfMercury, torr, bar, joule, watt, degreeCelsius, hertz, coulomb, volt, ohm, siemens, farad, weber, tesla, henry, lumen, lux, becquerel, gray, sievert, liter, degree, radian, steradian, inch, foot, yard, pound
    , quecto, ronto, yocto, zepto, atto, femto, pico, nano, micro, milli, centi, deci, noPrefix, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta, quetta
    )

{-| This module provides functionalities for parsing and displaying SI units
(and some other daily-use units).
This module has 4 groups of functionalities that is for

1.  creating `Unit` values
2.  parsing String into `Unit`
3.  displaying Unit as String in various form.
4.  simple arithmetics of `Quantity = (Float, Unit)`.


# Types

@docs Prefix, UnitSymbol, Unit, Error


## Defining New Unit Symbol

@docs createUnitSymbol


## Creation of Unit

@docs emptyUnit, u, pu, fromList, times, per, toThe, square, cubic, inverse, paren, simplifyUnitExpression


# Parsing

Unit parsing with this module proceeds in following pathway.

  - Defining set of units used in parser.
      - To avoid unit name collision, this process goes through `ValidatedUnitSymbols` type,
        whose creation may fail with name-conflicting units.
  - Converting `ValidatedUnitSymbols` into parser by `toParser` function.
      - This should be done only once at initialization process,
        because growing `ValidatedUnitSymbols` and creating parser is relatively heavy operation.
  - Using that parser with `parse` function.


## ValidatedUnitSymbols

You can:

  - use the default set of unit symbols (`defaultUnitSymbols`) provided by this module,
  - define new unit symbols as needed with `createUnitSymbol` and add them to the default set using `addUnitSymbols`,
  - or build your own set of unit symbols from scratch by folding `addUnitSymbols` over a list, starting from `emptyUnitSymbols`.

Some units (e.g. minute, inch, year, atm) are defined as **non-prefixable units** to avoid ambiguity with SI prefixes.

@docs ValidatedUnitSymbols, emptyUnitSymbols, defaultUnitSymbols, addUnitSymbols


## Getting and Using Parser

@docs toParser, failureParser, parse, Parser, DeadEnd


# Display

@docs toString


## Display Settings

@docs DisplayOption, defaultDisplayOption, defaultDisplayOptionForTex, inAscii, inTex, inUnicode, setSeparator, setSlash, setHeadingSlash, setParen, setEmptyUnitString, displayTopNumeratorOne, deleteTopNumeratorOne, useTopLevelTexFrac, doNotUseTopLevelTexFrac, useCustomSpacing, doNotUseCustomSpacing


# Arithmetics


## Note on unit growth

Units are combined syntactically when multiplying or dividing quantities.
This means repeated multiplications or divisions may result in large unit expressions
(e.g. `m * s * s`).

This is by design.

If you want to keep unit expressions compact or convert them into a specific unit,
call `convertInto` or `normalize` explicitly at appropriate points in your calculation.

@docs hasSameDimension, Quantity, convertInto, normalize, add, subtractBy, multiplyBy, divideBy, applySequentially


# Predefined Unit Symbols

The following sections list predefined unit symbols and prefixes.
They are primarily reference material and are not required
for understanding the core API.

Some units (e.g. minute, inch, year, atm) are defined as **non-prefixable units** to avoid ambiguity with SI prefixes.

@docs meter, gram, second, kelvin, ampere, mole, candela, ton, minute, hour, day, year, newton, pascal, atmosphere, millimeterOfMercury, torr, bar, joule, watt, degreeCelsius, hertz, coulomb, volt, ohm, siemens, farad, weber, tesla, henry, lumen, lux, becquerel, gray, sievert, liter, degree, radian, steradian, inch, foot, yard, pound


# Prefix

@docs quecto, ronto, yocto, zepto, atto, femto, pico, nano, micro, milli, centi, deci, noPrefix, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta, quetta

-}

--

import Dict exposing (Dict)
import Parser exposing (..)



-- Unit Definition --{{{
--
-- -- Prefix --{{{
--
-- -- -- Type Def --{{{


{-| Type representing SI prefix ("kilo" in "kilometer" etc.)
-}
type Prefix
    = Quecto
    | Ronto
    | Yocto
    | Zepto
    | Atto
    | Femto
    | Pico
    | Nano
    | Micro
    | Milli
    | Centi
    | Deci
    | NoPrefix
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
    | Ronna
    | Quetta



-- -- -- }}}
--
-- -- -- Prefix Handling Functions --{{{


applyPrefix : Prefix -> Float -> Float
applyPrefix prefix =
    (*)
        (case prefix of
            Quecto ->
                10 ^ -30

            Ronto ->
                10 ^ -27

            Yocto ->
                10 ^ -24

            Zepto ->
                10 ^ -21

            Atto ->
                10 ^ -18

            Femto ->
                10 ^ -15

            Pico ->
                10 ^ -12

            Nano ->
                10 ^ -9

            Micro ->
                10 ^ -6

            Milli ->
                10 ^ -3

            Centi ->
                10 ^ -2

            Deci ->
                10 ^ -1

            NoPrefix ->
                10 ^ 0

            Deca ->
                10 ^ 1

            Hecto ->
                10 ^ 2

            Kilo ->
                10 ^ 3

            Mega ->
                10 ^ 6

            Giga ->
                10 ^ 9

            Tera ->
                10 ^ 12

            Peta ->
                10 ^ 15

            Exa ->
                10 ^ 18

            Zetta ->
                10 ^ 21

            Yotta ->
                10 ^ 24

            Ronna ->
                10 ^ 27

            Quetta ->
                10 ^ 30
        )



-- -- -- }}}
--
-- -- -- All SI Prefixes --{{{


prefixes : List ( String, Prefix )
prefixes =
    [ ( "q", Quecto )
    , ( "r", Ronto )
    , ( "y", Yocto )
    , ( "z", Zepto )
    , ( "a", Atto )
    , ( "f", Femto )
    , ( "p", Pico )
    , ( "n", Nano )
    , ( "μ", Micro )
    , ( "u", Micro )
    , ( "m", Milli )
    , ( "c", Centi )
    , ( "d", Deci )
    , ( "", NoPrefix )
    , ( "da", Deca )
    , ( "h", Hecto )
    , ( "k", Kilo )
    , ( "M", Mega )
    , ( "G", Giga )
    , ( "T", Tera )
    , ( "P", Peta )
    , ( "E", Exa )
    , ( "Z", Zetta )
    , ( "Y", Yotta )
    , ( "R", Ronna )
    , ( "Q", Quetta )
    ]


{-| Prefix that represents 10^(-30)
-}
quecto : Prefix
quecto =
    Quecto


{-| Prefix that represents 10^(-27)
-}
ronto : Prefix
ronto =
    Ronto


{-| Prefix that represents 10^(-24)
-}
yocto : Prefix
yocto =
    Yocto


{-| Prefix that represents 10^(-21)
-}
zepto : Prefix
zepto =
    Zepto


{-| Prefix that represents 10^(-18)
-}
atto : Prefix
atto =
    Atto


{-| Prefix that represents 10^(-15)
-}
femto : Prefix
femto =
    Femto


{-| Prefix that represents 10^(-12)
-}
pico : Prefix
pico =
    Pico


{-| Prefix that represents 10^(-9)
-}
nano : Prefix
nano =
    Nano


{-| Prefix that represents 10^(-6)
-}
micro : Prefix
micro =
    Micro


{-| Prefix that represents 10^(-3)
-}
milli : Prefix
milli =
    Milli


{-| Prefix that represents 10^(-2)
-}
centi : Prefix
centi =
    Centi


{-| Prefix that represents 10^(-1)
-}
deci : Prefix
deci =
    Deci


{-| Prefix type value that shows nothing is prefixed.
-}
noPrefix : Prefix
noPrefix =
    NoPrefix


{-| Prefix that represents 10^(1)
-}
deca : Prefix
deca =
    Deca


{-| Prefix that represents 10^(2)
-}
hecto : Prefix
hecto =
    Hecto


{-| Prefix that represents 10^(3)
-}
kilo : Prefix
kilo =
    Kilo


{-| Prefix that represents 10^(6)
-}
mega : Prefix
mega =
    Mega


{-| Prefix that represents 10^(9)
-}
giga : Prefix
giga =
    Giga


{-| Prefix that represents 10^(12)
-}
tera : Prefix
tera =
    Tera


{-| Prefix that represents 10^(15)
-}
peta : Prefix
peta =
    Peta


{-| Prefix that represents 10^(18)
-}
exa : Prefix
exa =
    Exa


{-| Prefix that represents 10^(21)
-}
zetta : Prefix
zetta =
    Zetta


{-| Prefix that represents 10^(24)
-}
yotta : Prefix
yotta =
    Yotta


{-| Prefix that represents 10^(27)
-}
ronna : Prefix
ronna =
    Ronna


{-| Prefix that represents 10^(30)
-}
quetta : Prefix
quetta =
    Quetta



-- -- -- }}}
--
-- -- }}}
--
-- -- UnitSymbol --{{{


{-| Type that represents unit before prefixed, like "meter" in "millimeter".
-}
type UnitSymbol
    = UnitSymbol UnitSymbolContent


type alias UnitSymbolContent =
    { parserInput : List String
    , outputInAscii : String
    , outputInTex : String
    , outputInUnicode : String
    , baseSIExpression : NormalizedUnit
    , allowPrefix : Bool
    }


{-| Creates a `UnitSymbol`, which represents an unprefixed unit
such as "meter" in "millimeter".

This function is mainly intended for defining custom unit symbols
that can later be added to `ValidatedUnitSymbols` for parsing.

-}
createUnitSymbol :
    { parserInput : List String
    , display : String
    , baseSIExpression :
        { meterExponent : Int
        , kilogramExponent : Int
        , secondExponent : Int
        , kelvinExponent : Int
        , ampereExponent : Int
        , moleExponent : Int
        , candelaExponent : Int
        , numericalValue : Result String Float
        }
    , allowPrefix : Bool
    }
    -> UnitSymbol
createUnitSymbol r =
    UnitSymbol
        { parserInput = r.parserInput
        , outputInAscii = r.display
        , outputInTex = r.display
        , outputInUnicode = r.display
        , baseSIExpression = r.baseSIExpression
        , allowPrefix = r.allowPrefix
        }


{-| Unit for length denoted as `m`.
-}
meter : UnitSymbol
meter =
    UnitSymbolContent [ "m" ] "m" "m" "m" (NormalizedUnit 1 0 0 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for mass denoted as `g`.
-}
gram : UnitSymbol
gram =
    UnitSymbolContent [ "g" ] "g" "g" "g" (NormalizedUnit 0 1 0 0 0 0 0 (Ok (1 / 1000))) True
        |> UnitSymbol


{-| Unit for time denoted as `s`.
-}
second : UnitSymbol
second =
    UnitSymbolContent [ "s" ] "s" "s" "s" (NormalizedUnit 0 0 1 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for absolute temperature denoted as `K`.
-}
kelvin : UnitSymbol
kelvin =
    UnitSymbolContent [ "K" ] "K" "K" "K" (NormalizedUnit 0 0 0 1 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for electric current denoted as `A`.
-}
ampere : UnitSymbol
ampere =
    UnitSymbolContent [ "A" ] "A" "A" "A" (NormalizedUnit 0 0 0 0 1 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for amount of substance denoted as `mol`.
-}
mole : UnitSymbol
mole =
    UnitSymbolContent [ "mol" ] "mol" "mol" "mol" (NormalizedUnit 0 0 0 0 0 1 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for luminous intensity denoted as `cd`.
-}
candela : UnitSymbol
candela =
    UnitSymbolContent [ "cd" ] "cd" "cd" "cd" (NormalizedUnit 0 0 0 0 0 0 1 (Ok 1)) True
        |> UnitSymbol


{-| Unit for mass denoted as `ton`. Equals to `10^6 kg`.
-}
ton : UnitSymbol
ton =
    UnitSymbolContent [ "ton" ] "ton" "ton" "ton" (NormalizedUnit 0 1 0 0 0 0 0 (Ok 1000)) False
        |> UnitSymbol


{-| Unit for time denoted as `min`. Equals to `60 s`.
-}
minute : UnitSymbol
minute =
    UnitSymbolContent [ "minute", "min" ] "min" "min" "min" (NormalizedUnit 0 0 1 0 0 0 0 (Ok 60)) False
        |> UnitSymbol


{-| Unit for time denoted as `h`. Equals to `3600 s`.
-}
hour : UnitSymbol
hour =
    UnitSymbolContent [ "h", "hr", "hour" ] "h" "h" "h" (NormalizedUnit 0 0 1 0 0 0 0 (Ok 3600)) False
        |> UnitSymbol


{-| Unit for time denoted as `day`. Equals to `86400 s`.
-}
day : UnitSymbol
day =
    UnitSymbolContent [ "d", "day" ] "day" "day" "day" (NormalizedUnit 0 0 1 0 0 0 0 (Ok 86400)) False
        |> UnitSymbol


{-| Unit for time denoted as `year`. The exact definition may vary depending on context.
In this package, conversion involving this unit is not supported.
-}
year : UnitSymbol
year =
    UnitSymbolContent [ "y", "year" ] "year" "year" "year" (NormalizedUnit 0 0 1 0 0 0 0 (Err "1 year has multiple interpretation: 365 days = 31536000 s or 365.2422 days = 3.155693e7 s")) False
        |> UnitSymbol


{-| Unit for force denoted as `N`. Equals to `kg m s^-2`.
-}
newton : UnitSymbol
newton =
    UnitSymbolContent [ "N" ] "N" "N" "N" (NormalizedUnit 1 1 -2 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for pressure denoted as `Pa`. Equals to `N m^-2`.
-}
pascal : UnitSymbol
pascal =
    UnitSymbolContent [ "Pa" ] "Pa" "Pa" "Pa" (NormalizedUnit -1 1 -2 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for pressure denoted as `atm`. Equals to `1.01325×10^5 Pa`.
-}
atmosphere : UnitSymbol
atmosphere =
    UnitSymbolContent [ "atm" ] "atm" "atm" "atm" (NormalizedUnit -1 1 -2 0 0 0 0 (Ok 101325)) False
        |> UnitSymbol


{-| Unit for pressure denoted as `mmHg`. Equals to `1/760 atm`.
-}
millimeterOfMercury : UnitSymbol
millimeterOfMercury =
    UnitSymbolContent [ "mmHg" ] "mmHg" "mmHg" "mmHg" (NormalizedUnit -1 1 -2 0 0 0 0 (Ok 133.322)) False
        |> UnitSymbol


{-| Unit for pressure denoted as `torr`. Equals to `mmHg`.
-}
torr : UnitSymbol
torr =
    UnitSymbolContent [ "torr" ] "torr" "torr" "torr" (NormalizedUnit -1 1 -2 0 0 0 0 (Ok 133.322)) False
        |> UnitSymbol


{-| Unit for pressure denoted as `bar`. Equals to `10^5 Pa`.
-}
bar : UnitSymbol
bar =
    UnitSymbolContent [ "bar" ] "bar" "bar" "bar" (NormalizedUnit -1 1 -2 0 0 0 0 (Ok 100000)) True
        |> UnitSymbol


{-| Unit for work and energy denoted as `J`. Equals to `N・m`.
-}
joule : UnitSymbol
joule =
    UnitSymbolContent [ "J" ] "J" "J" "J" (NormalizedUnit 2 1 -2 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for power denoted as `W`. Equals to `J / s`.
-}
watt : UnitSymbol
watt =
    UnitSymbolContent [ "W" ] "W" "W" "W" (NormalizedUnit 2 1 -3 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for temperature denoted as `℃`. A temperature of `t ℃` is equal to `(t + 273.15) K`.
In this package, conversion involving this unit is not supported.
-}
degreeCelsius : UnitSymbol
degreeCelsius =
    UnitSymbolContent [ "degC", "℃" ] "degC" "^\\circ C" "℃" (NormalizedUnit 0 0 0 1 0 0 0 (Err "x ℃ = (x + 273.15) K")) False
        |> UnitSymbol


{-| Unit for frequency denoted as `Hz`. Equals to s^-1
-}
hertz : UnitSymbol
hertz =
    UnitSymbolContent [ "Hz" ] "Hz" "Hz" "Hz" (NormalizedUnit 0 0 -1 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for electricity denoted as `C`. Equals to `A・s`.
-}
coulomb : UnitSymbol
coulomb =
    UnitSymbolContent [ "C" ] "C" "C" "C" (NormalizedUnit 0 0 1 0 1 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for voltage denoted as `V`. Equals to `J / C`.
-}
volt : UnitSymbol
volt =
    UnitSymbolContent [ "V" ] "V" "V" "V" (NormalizedUnit 2 1 -3 0 -1 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for resistance denoted as `Ω`. Equals to `V / A`.
-}
ohm : UnitSymbol
ohm =
    UnitSymbolContent [ "ohm", "Ω" ] "ohm" "\\Omega" "Ω" (NormalizedUnit 2 1 -3 0 -2 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for conductivity denoted as `S`. Equals to `A / V`.
-}
siemens : UnitSymbol
siemens =
    UnitSymbolContent [ "S" ] "S" "S" "S" (NormalizedUnit -2 -1 3 0 2 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for capacity denoted as `F`. Equals to `C / V`.
-}
farad : UnitSymbol
farad =
    UnitSymbolContent [ "F" ] "F" "F" "F" (NormalizedUnit -2 -1 4 0 2 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for magnetic flux denoted as `Wb`. Equals to `J / A`.
-}
weber : UnitSymbol
weber =
    UnitSymbolContent [ "Wb" ] "Wb" "Wb" "Wb" (NormalizedUnit 2 1 -2 0 -1 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for magnetic flux density denoted as `T`. Equals to `Wb / m^2`.
-}
tesla : UnitSymbol
tesla =
    UnitSymbolContent [ "T" ] "T" "T" "T" (NormalizedUnit 0 1 -2 0 -1 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for self or mutual inductance denoted as `H`. Equals to `V / (A s^-1)`.
-}
henry : UnitSymbol
henry =
    UnitSymbolContent [ "H" ] "H" "H" "H" (NormalizedUnit 2 1 -2 0 -2 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for luminous flux denoted as `Lm`. Equals to `cd・sr`.
-}
lumen : UnitSymbol
lumen =
    UnitSymbolContent [ "Lm" ] "Lm" "Lm" "Lm" (NormalizedUnit 0 0 0 0 0 0 1 (Ok 1)) True
        |> UnitSymbol


{-| Unit for illuminance denoted as `Lx`. Equals to `Lm / m^2`.
-}
lux : UnitSymbol
lux =
    UnitSymbolContent [ "Lx" ] "Lx" "Lx" "Lx" (NormalizedUnit -2 0 0 0 0 0 1 (Ok 1)) True
        |> UnitSymbol


{-| Unit for radio activity denoted as `Bq`. Equals to `1 / s`.
-}
becquerel : UnitSymbol
becquerel =
    UnitSymbolContent [ "Bq" ] "Bq" "Bq" "Bq" (NormalizedUnit 1 0 -1 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for absorbed dose denoted as `Gy`. Equals to `J / kg`.
-}
gray : UnitSymbol
gray =
    UnitSymbolContent [ "Gy" ] "Gy" "Gy" "Gy" (NormalizedUnit 2 0 -2 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for exposure dose denoted as `Sv`. The dimension equals to `J / kg`,
but with different weight coefficient applied to different radio beam kind.
-}
sievert : UnitSymbol
sievert =
    UnitSymbolContent [ "Sv" ] "Sv" "Sv" "Sv" (NormalizedUnit 2 0 -2 0 0 0 0 (Err "1 Sv in Gy differs by particle kind")) True
        |> UnitSymbol


{-| Unit for volume denoted as `L`. Equals to `10^(-3) m^3`.
-}
liter : UnitSymbol
liter =
    UnitSymbolContent [ "L" ] "L" "L" "L" (NormalizedUnit 3 0 0 0 0 0 0 (Ok (1 / 1000))) True
        |> UnitSymbol


{-| Unit for angle denoted as `°`. Equals to `(π/180) rad`.
-}
degree : UnitSymbol
degree =
    UnitSymbolContent [ "deg", "degree", "°" ] "deg" "^\\circ" "°" (NormalizedUnit 0 0 0 0 0 0 0 (Ok (pi / 180))) False
        |> UnitSymbol


{-| Unit for angle denoted as `rad`.
-}
radian : UnitSymbol
radian =
    UnitSymbolContent [ "rad" ] "rad" "rad" "rad" (NormalizedUnit 0 0 0 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for stereo angle denoted as `sr`.
-}
steradian : UnitSymbol
steradian =
    UnitSymbolContent [ "sr" ] "sr" "sr" "sr" (NormalizedUnit 0 0 0 0 0 0 0 (Ok 1)) True
        |> UnitSymbol


{-| Unit for length denoted as `in`.
-}
inch : UnitSymbol
inch =
    UnitSymbolContent [ "in" ] "in" "in" "in" (NormalizedUnit 1 0 0 0 0 0 0 (Ok 0.0254)) False
        |> UnitSymbol


{-| Unit for length denoted as `ft`.
-}
foot : UnitSymbol
foot =
    UnitSymbolContent [ "ft" ] "ft" "ft" "ft" (NormalizedUnit 1 0 0 0 0 0 0 (Ok 0.3048)) False
        |> UnitSymbol


{-| Unit for length denoted as `yd`.
-}
yard : UnitSymbol
yard =
    UnitSymbolContent [ "yd" ] "yd" "yd" "yd" (NormalizedUnit 1 0 0 0 0 0 0 (Ok 0.9144)) False
        |> UnitSymbol


{-| Unit for mass denoted as `lb`.
-}
pound : UnitSymbol
pound =
    UnitSymbolContent [ "lb" ] "lb" "lb" "lb" (NormalizedUnit 0 1 0 0 0 0 0 (Ok 0.4536)) False
        |> UnitSymbol



-- -- }}}
--
-- -- Unit --{{{


{-| Type that represents unit
-}
type Unit
    = One
    | Atom Prefix UnitSymbol
    | Parened Bool Unit -- Bool is "Forced Flag". (When True, it should never be eliminated.)
    | Pow Int Unit
    | Mul Unit Unit
    | Div Unit Unit


{-| Unit with no dimension. Equals to 1.
-}
emptyUnit : Unit
emptyUnit =
    One


toPrefixedSymbolWithExponent : ( Prefix, UnitSymbol, Int ) -> Unit
toPrefixedSymbolWithExponent ( p, s, e ) =
    case e of
        1 ->
            Atom p s

        _ ->
            Pow e (Atom p s)



-- -- }}}
--
-- }}}
--
-- Unit Operation --{{{
--
-- Creation --{{{


{-| Creates `Unit` value from `UnitSymbol` with no prefix added.

    u pascal
    |> toString defaultDisplayOption
    --> "Pa"

-}
u : UnitSymbol -> Unit
u s =
    Atom NoPrefix s


{-| Creates `Unit` value from `Prefix` and `UnitSymbol`.

    pu kilo pascal
    |> toString defaultDisplayOption
    --> "kPa"

-}
pu : Prefix -> UnitSymbol -> Unit
pu p s =
    Atom p s


{-| Creates `Unit` value from `List ( Prefix, UnitSymbol, Int )`,
where the `Int` value represents exponent.
The order in the list is preserved and used as the displayed order.
In complex case where you want to use `/` or parened exponentiation
like `kg (m / s)^2`, other functions may be more suited.

    fromList [(centi, meter, 1), (noPrefix, second, -1)]
    |> toString defaultDisplayOption
    --> "cm s^-1"

-}
fromList : List ( Prefix, UnitSymbol, Int ) -> Unit
fromList l =
    case List.map toPrefixedSymbolWithExponent l of
        [] ->
            One

        h :: t ->
            List.foldl (\crr acc -> Mul acc crr) h t



-- -- }}}
--
-- Modification --{{{


{-| Represents division with slash (not with negative exponent).
Intended to be used with `|>` operator.
Note that `|>` is left associative.

    u joule |> per (u kelvin) |> times (u gram)
    |> toString defaultDisplayOption
    --> "(J / K) g"

    u joule |> per ((u kelvin) |> times (u gram))
    |> toString defaultDisplayOption
    --> "J / (K g)"

    u joule |> per (u kelvin) |> per (u gram)
    |> toString defaultDisplayOption
    --> "(J / K) / g"

    u joule |> times (inverse (u kelvin)) |> per (u gram)
    |> toString defaultDisplayOption
    --> "J K^-1 / g"

-}
per : Unit -> Unit -> Unit
per denominator numerator =
    Div numerator denominator


{-| Represents multiplication of units.

    u meter |> times (inverse (u second))
    |> toString defaultDisplayOption
    --> "m s^-1"

-}
times : Unit -> Unit -> Unit
times b a =
    Mul a b


{-| Represents exponentiation of units.
Intended to be used with `|>` operator.
Note that `|>` is left associative.

    u mole |> per (pu deci meter |> toThe 3) |> toThe 2
    |> toString defaultDisplayOption
    --> "(mol / dm^3)^2"

-}
toThe : Int -> Unit -> Unit
toThe =
    Pow


{-| Syntax suger for `toThe 2`.

    square (u meter)
    |> toString defaultDisplayOption
    --> "m^2"

-}
square : Unit -> Unit
square =
    Pow 2


{-| Syntax suger for `toThe 3`.

    cubic (pu deci meter)
    |> toString defaultDisplayOption
    --> "dm^3"

-}
cubic : Unit -> Unit
cubic =
    Pow 3


{-| Syntax suger for `toThe -1`.
Note that this is different from `power` at display.

    u mole |> times (inverse (u liter)) |> per (u second)
    |> toString defaultDisplayOption
    --> "mol L^-1 / s"

-}
inverse : Unit -> Unit
inverse =
    Pow -1


{-| Adds paren to unit. This affects displayed string.
-}
paren : Unit -> Unit
paren =
    Parened True


{-| Simplifies unit expressions by applying semantic normalization, such as:

  - `(s^-1)^2` is converted into `s^-2`.
  - `m / 1`, `m 1` or `m^1` are all converted into `m`.

```
    litterInSI : Unit
    litterInSI = cubic (pu deci meter)

    square litterInSI
    |> toString defaultDisplayOption
    --> "(dm^3)^2"

    square litterInSI
    |> simplifyUnitExpression
    |> toString defaultDisplayOption
    --> "dm^6"
```

-}
simplifyUnitExpression : Unit -> Unit
simplifyUnitExpression =
    deleteAllParen >> simplifyUnitExpressionHelp >> addNeededParen


simplifyUnitExpressionHelp : Unit -> Unit
simplifyUnitExpressionHelp unit =
    case unit of
        -- `simplifyUnitExpression` deletes all parens,
        -- so there must be no `Parened` variant.
        Parened _ _ ->
            One

        One ->
            unit

        Atom _ _ ->
            unit

        Pow _ One ->
            One

        Pow 0 u1 ->
            One

        Pow 1 u1 ->
            simplifyUnitExpressionHelp u1

        Pow e1 (Pow e2 u1) ->
            simplifyUnitExpressionHelp (Pow (e1 * e2) u1)

        Pow e u1 ->
            Pow e (simplifyUnitExpressionHelp u1)

        Mul u1 One ->
            simplifyUnitExpressionHelp u1

        Mul One u2 ->
            simplifyUnitExpressionHelp u2

        Mul u1 u2 ->
            Mul
                (simplifyUnitExpressionHelp u1)
                (simplifyUnitExpressionHelp u2)

        Div u1 One ->
            simplifyUnitExpressionHelp u1

        Div u1 u2 ->
            Div
                (simplifyUnitExpressionHelp u1)
                (simplifyUnitExpressionHelp u2)



-- -- }}}
--
-- }}}
--
-- Parsing --{{{
--
-- -- ValidatedUnitSymbols --{{{


{-| Represents unit symbols usables in parsing purpose,
in which no name collision could occurs.
This type is exposed so that users can control
which unit symbols are accepted by the parser.
-}
type ValidatedUnitSymbols
    = ValidatedUnitSymbols (Dict String PrefixedSymbol)


{-| Represents vacant `ValidatedUnitSymbols` value
-}
emptyUnitSymbols : ValidatedUnitSymbols
emptyUnitSymbols =
    ValidatedUnitSymbols Dict.empty


defaultUnitSymbols_ : List UnitSymbol
defaultUnitSymbols_ =
    [ meter
    , gram
    , second
    , kelvin
    , ampere
    , mole
    , candela
    , ton
    , minute
    , hour
    , day
    , year
    , newton
    , pascal
    , atmosphere
    , millimeterOfMercury
    , torr
    , bar
    , joule
    , watt
    , degreeCelsius
    , hertz
    , coulomb
    , volt
    , ohm
    , siemens
    , farad
    , weber
    , tesla
    , henry
    , lumen
    , lux
    , becquerel
    , gray
    , sievert
    , liter
    , degree
    , radian
    , steradian
    , inch
    , foot
    , yard
    , pound
    ]


{-| Represents `ValidatedUnitSymbols` value
with SI standarized units and some other daily use unit.
-}
defaultUnitSymbols : ValidatedUnitSymbols
defaultUnitSymbols =
    emptyUnitSymbols
        |> addUnitSymbols defaultUnitSymbols_
        |> Result.withDefault emptyUnitSymbols


type alias PrefixedSymbol =
    { prefix : Prefix
    , symbol : UnitSymbol
    , prefixString : String
    , symbolString : String
    }


{-| The only way to add a user-defined unit symbol for parsing purpose.

This function fails if the new unit symbol conflicts with existing
symbols or their prefixed forms.

        eV : UnitSymbol
        eV =
            createUnitSymbol
                { display = "eV"
                , parserInput = ["eV"]
                , baseSIExpression =
                    { meterExponent = 2
                    , kilogramExponent = 1
                    , secondExponent = -2
                    , kelvinExponent = 0
                    , ampereExponent = 0
                    , moleExponent = 0
                    , candelaExponent = 0
                    , numericalValue =
                        Ok (1.602176634e-19)
                    }
                , allowPrefix = True
                }

        parser : Parser
        parser =
             defaultUnitSymbols
                |> addUnitSymbols [eV]
                |> Result.map toParser
                |> Result.withDefault failureParser

        parsedUnit : Unit
        parsedUnit =
            case parse parser "keV" of
                Ok unit ->
                    unit
                Err _ ->
                    emptyUnit


        (200, parsedUnit)
            |> convertInto (u eV)
            --> Ok (toFloat (round 200000), u eV)

-}
addUnitSymbols : List UnitSymbol -> ValidatedUnitSymbols -> Result Error ValidatedUnitSymbols
addUnitSymbols unitSymbols (ValidatedUnitSymbols sofar) =
    unitSymbols
        |> List.concatMap
            (\((UnitSymbol smbl) as unitSymbolBody) ->
                if not smbl.allowPrefix then
                    smbl.parserInput
                        |> List.map
                            (\symbolString ->
                                { key = symbolString
                                , value =
                                    { prefix = NoPrefix
                                    , symbol = unitSymbolBody
                                    , prefixString = ""
                                    , symbolString = symbolString
                                    }
                                }
                            )

                else
                    smbl.parserInput
                        |> List.concatMap
                            (\symbolString ->
                                List.map
                                    (\( prefixString, prefixBody ) ->
                                        { key = prefixString ++ symbolString
                                        , value =
                                            { prefix = prefixBody
                                            , symbol = unitSymbolBody
                                            , prefixString = prefixString
                                            , symbolString = symbolString
                                            }
                                        }
                                    )
                                    prefixes
                            )
            )
        |> addUnitSymbolsHelp sofar


addUnitSymbolsHelp : Dict String PrefixedSymbol -> List { key : String, value : PrefixedSymbol } -> Result Error ValidatedUnitSymbols
addUnitSymbolsHelp sofar kvs =
    case kvs of
        [] ->
            Ok (ValidatedUnitSymbols sofar)

        head :: tail ->
            case Dict.get head.key sofar of
                Nothing ->
                    addUnitSymbolsHelp
                        (Dict.insert head.key head.value sofar)
                        tail

                Just conflicted ->
                    Err
                        (UnitSymbolConflict
                            { conflictedString = head.key
                            , added =
                                { prefix = clarifyNoPrefix head.value.prefixString
                                , symbol = head.value.symbolString
                                , baseSIExpressionOfSymbol =
                                    case head.value.symbol of
                                        UnitSymbol us ->
                                            us.baseSIExpression
                                }
                            , existing =
                                { prefix = clarifyNoPrefix conflicted.prefixString
                                , symbol = conflicted.symbolString
                                , baseSIExpressionOfSymbol =
                                    case conflicted.symbol of
                                        UnitSymbol us ->
                                            us.baseSIExpression
                                }
                            }
                        )


clarifyNoPrefix : String -> String
clarifyNoPrefix s =
    case s of
        "" ->
            "<No Prefix>"

        _ ->
            s


{-| This parser always fails.
Intended to be used as a fallback when `addUnitSymbols` fails,
which indicates a configuration error rather than a runtime parsing error.
-}
failureParser : Parser
failureParser =
    problem "Failed to add new unit symbol."



-- -- }}}
--
-- -- Parser --{{{


{-| Type alias for `Parser.Parser Unit` from `elm/parser`.
-}
type alias Parser =
    Parser.Parser Unit


{-| Type alias for `Parser.DeadEnd` from `elm/parser`.
-}
type alias DeadEnd =
    Parser.DeadEnd


{-| Runs the parser onto given string.
-}
parse : Parser -> String -> Result (List DeadEnd) Unit
parse p s =
    run p s


possiblySignedInt : Parser.Parser Int
possiblySignedInt =
    oneOf
        [ symbol "-"
            |> andThen (\() -> int)
            |> map ((*) -1)
        , symbol "+"
            |> andThen (\() -> int)
        , int
        ]


unicodeSuperScriptParser : Parser.Parser Int
unicodeSuperScriptParser =
    succeed (++)
        |= oneOf
            [ succeed "-"
                |. symbol "⁻"
            , succeed ""
            ]
        |= loop ""
            (\state ->
                oneOf
                    [ succeed (\next -> Loop (state ++ next))
                        |= oneOf
                            [ succeed "0" |. symbol "⁰"
                            , succeed "1" |. symbol "¹"
                            , succeed "2" |. symbol "²"
                            , succeed "3" |. symbol "³"
                            , succeed "4" |. symbol "⁴"
                            , succeed "5" |. symbol "⁵"
                            , succeed "6" |. symbol "⁶"
                            , succeed "7" |. symbol "⁷"
                            , succeed "8" |. symbol "⁸"
                            , succeed "9" |. symbol "⁹"
                            ]
                    , succeed (Done state)
                    ]
            )
        |> andThen
            (\str ->
                case String.toInt str of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem "failed to parse superscript number."
            )


exponentParser : Unit -> Parser.Parser Unit
exponentParser unit =
    oneOf
        [ succeed identity
            |. symbol "^{"
            |= possiblySignedInt
            |. symbol "}"
        , succeed identity
            |. symbol "^"
            |= possiblySignedInt
        , succeed identity
            |= unicodeSuperScriptParser
        ]
        |> map (\e -> Pow e unit)


separatorParser : Parser.Parser ()
separatorParser =
    oneOf
        [ backtrackable spaces |. token "." |. spaces
        , backtrackable spaces |. token "*" |. spaces
        , token " " |. spaces
        ]


oneParser : Parser.Parser Unit
oneParser =
    succeed One
        |. symbol "1"


withParen : Parser.Parser Unit -> Parser.Parser Unit
withParen p =
    succeed (Parened True)
        |. backtrackable spaces
        |. symbol "("
        |. spaces
        |= p
        |. spaces
        |. symbol ")"


slashParser : Parser.Parser ()
slashParser =
    backtrackable spaces |. symbol "/" |. spaces


withOrWithoutExponent : Parser.Parser Unit -> Parser.Parser Unit
withOrWithoutExponent =
    andThen
        (\unit ->
            oneOf
                [ exponentParser unit
                , succeed unit
                ]
        )


isUnitAtomChar : Char -> Bool
isUnitAtomChar c =
    c
        /= ' '
        && c
        /= '*'
        && c
        /= '.'
        && c
        /= '/'
        && c
        /= '^'
        && c
        /= '('
        && c
        /= ')'
        && c
        /= '1'


chompUnitAtom : Parser.Parser String
chompUnitAtom =
    chompWhile isUnitAtomChar
        |> getChompedString


toParser_ : Dict String PrefixedSymbol -> Parser.Parser Unit
toParser_ dict =
    let
        atomParser : Parser.Parser Unit
        atomParser =
            chompUnitAtom
                |> andThen
                    (\a ->
                        case Dict.get a dict of
                            Just r ->
                                succeed (Atom r.prefix r.symbol)

                            Nothing ->
                                if a == "" then
                                    problem "Some unit should be given here."

                                else
                                    problem ("String \"" ++ a ++ "\" is not recognized as unit.")
                    )

        unitParserInner : () -> Parser.Parser Unit
        unitParserInner () =
            oneOf
                [ withParen (lazy unitParserInner)
                , atomParser
                , oneParser
                ]
                |> withOrWithoutExponent
                |> andThen
                    (\leftUnit ->
                        loop leftUnit
                            (\u1 ->
                                oneOf
                                    [ oneOf
                                        [ succeed (Div u1)
                                            |. slashParser
                                        , succeed (Mul u1)
                                            |. backtrackable separatorParser
                                        ]
                                        |= (oneOf
                                                [ withParen (lazy unitParserInner)
                                                , atomParser
                                                , oneParser
                                                ]
                                                |> withOrWithoutExponent
                                           )
                                        |> map Loop
                                    , succeed (Done u1)
                                    ]
                            )
                    )

        unitParser : Parser.Parser Unit
        unitParser =
            succeed identity
                |. spaces
                |= oneOf
                    [ succeed One
                        |. end
                    , succeed (Div One)
                        |. slashParser
                        |= (oneOf
                                [ withParen (lazy unitParserInner)
                                , atomParser
                                , oneParser
                                ]
                                |> withOrWithoutExponent
                           )
                        |> andThen
                            (\leftUnit ->
                                loop leftUnit
                                    (\u1 ->
                                        oneOf
                                            [ oneOf
                                                [ succeed (Div u1)
                                                    |. slashParser
                                                , succeed (Mul u1)
                                                    |. backtrackable separatorParser
                                                ]
                                                |= (oneOf
                                                        [ withParen (lazy unitParserInner)
                                                        , atomParser
                                                        , oneParser
                                                        ]
                                                        |> withOrWithoutExponent
                                                   )
                                                |> map Loop
                                            , succeed (Done u1)
                                            ]
                                    )
                            )
                    , unitParserInner ()
                    ]
    in
    unitParser


{-| Converts a set of unit symbols into a `Parser`.

If you want to build a parser that accepts **only SI base units**
(with or without prefixes), you can write:

    baseUnitsParser : Parser
    baseUnitsParser =
        emptyUnitSymbols
            |> addUnitSymbols
                [ meter, gram, second, kelvin, ampere, mole, candela ]
            |> Result.map toParser
            |> Result.withDefault failureParser

This gives you a parser that rejects any non-SI or user-defined unit symbols.

    parse baseUnitsParser "cm s^-1"
        |> Result.map (toString defaultDisplayOption)
        --> Ok "cm s^-1"

    parse baseUnitsParser "km / h"
        |> Result.map (toString defaultDisplayOption)
        |> Result.withDefault "failed to parse!"
        --> "failed to parse!"

-}
toParser : ValidatedUnitSymbols -> Parser
toParser (ValidatedUnitSymbols symbols) =
    toParser_ symbols



-- -- }}}
--
-- }}}
--
-- Arithmetics --{{{


{-| Checks if two arguments have the same dimension.

    u liter |> hasSameDimension (cubic (u meter))
    --> True

    u liter |> hasSameDimension (square (u meter))
    --> False

-}
hasSameDimension : Unit -> Unit -> Bool
hasSameDimension u1 u2 =
    let
        n1 =
            normalizeUnit u1

        n2 =
            normalizeUnit u2
    in
    (n1.meterExponent == n2.meterExponent)
        && (n1.kilogramExponent == n2.kilogramExponent)
        && (n1.secondExponent == n2.secondExponent)
        && (n1.kelvinExponent == n2.kelvinExponent)
        && (n1.ampereExponent == n2.ampereExponent)
        && (n1.moleExponent == n2.moleExponent)
        && (n1.candelaExponent == n2.candelaExponent)


{-| Representing phisical quantity that is unit multiplied by numerical coefficient.
-}
type alias Quantity =
    ( Float, Unit )


{-| Converts expression of physical quantity with unit into another unit.
This fails when:

  - The targeted unit and start unit has different dimensions.

  - Units like `℃`, `Sv` or `year` is involved.

```
    (3, cubic (u meter))
        |> convertInto (u liter)
        |> Result.map
            (\(numericalValue, unit) ->
                    String.fromInt (round numericalValue)
                    ++ " "
                    ++ toString defaultDisplayOption unit
            )
        --> Ok "3000 L"
```

-}
convertInto : Unit -> Quantity -> Result Error Quantity
convertInto toUnit ( num, fromUnit ) =
    if not (fromUnit |> hasSameDimension toUnit) then
        Err
            (DifferentDimensionValue
                { operation = Conversion
                , left = fromUnit
                , right = toUnit
                }
            )

    else
        let
            n0 =
                normalizeUnit fromUnit

            n1 =
                normalizeUnit toUnit
        in
        case n0.numericalValue of
            Err str ->
                Err (CannotConvertToSI str)

            Ok v0 ->
                case n1.numericalValue of
                    Err str ->
                        Err (CannotConvertToSI str)

                    Ok v1 ->
                        Ok ( num * v0 / v1, toUnit )


{-| Converts quantity into normalized unit in the form of `m^a kg^b s^c K^d A^e mol^f cd^g`,
exponent 1 and unit with 0 exponent being omitted.
The numerical value is also converted accordingly.
-}
normalize : Quantity -> Result Error Quantity
normalize (( num, unit ) as q) =
    let
        un =
            normalizeUnit unit
    in
    q
        |> convertInto
            (fromList
                [ ( noPrefix, meter, un.meterExponent )
                , ( kilo, gram, un.kilogramExponent )
                , ( noPrefix, second, un.secondExponent )
                , ( noPrefix, kelvin, un.kelvinExponent )
                , ( noPrefix, ampere, un.ampereExponent )
                , ( noPrefix, mole, un.moleExponent )
                , ( noPrefix, candela, un.candelaExponent )
                ]
                |> simplifyUnitExpression
            )


{-| Enumerated type of calculation conducted. Used in Error representation.
-}
type ArithmeticOperation
    = Conversion
    | Addition
    | Subtraction
    | Multiplication
    | Division


{-| Represents all recoverable errors that may occur during
unit parsing, conversion, or arithmetic operations.
-}
type Error
    = UnitSymbolConflict
        { conflictedString : String
        , added :
            { prefix : String
            , symbol : String
            , baseSIExpressionOfSymbol :
                { meterExponent : Int
                , kilogramExponent : Int
                , secondExponent : Int
                , kelvinExponent : Int
                , ampereExponent : Int
                , moleExponent : Int
                , candelaExponent : Int
                , numericalValue : Result String Float
                }
            }
        , existing :
            { prefix : String
            , symbol : String
            , baseSIExpressionOfSymbol :
                { meterExponent : Int
                , kilogramExponent : Int
                , secondExponent : Int
                , kelvinExponent : Int
                , ampereExponent : Int
                , moleExponent : Int
                , candelaExponent : Int
                , numericalValue : Result String Float
                }
            }
        }
    | CannotConvertToSI String
    | DifferentDimensionValue
        { operation : ArithmeticOperation
        , left : Unit
        , right : Unit
        }


{-| Adds two quantities.
Fails if unit for either of addend contains inconvertible unit like ℃,
or two addeds have different dimensions.
-}
add : Quantity -> Quantity -> Result Error Quantity
add ( r, rightUnit ) ( l, leftUnit ) =
    let
        nr =
            normalizeUnit rightUnit

        nl =
            normalizeUnit leftUnit
    in
    if not (rightUnit |> hasSameDimension leftUnit) then
        Err
            (DifferentDimensionValue
                { operation = Addition
                , left = leftUnit
                , right = rightUnit
                }
            )

    else
        case nl.numericalValue of
            Err str ->
                Err (CannotConvertToSI str)

            Ok vl ->
                case nr.numericalValue of
                    Err str ->
                        Err (CannotConvertToSI str)

                    Ok vr ->
                        Ok ( l + r * vr / vl, leftUnit )


{-| Subtracts one quantity from another.
Fails if unit for either subtrahend or minuend contains inconvertible unit like ℃,
or subtrahend and minuend have different dimensions.
-}
subtractBy : Quantity -> Quantity -> Result Error Quantity
subtractBy ( r, rightUnit ) ( l, leftUnit ) =
    let
        nr =
            normalizeUnit rightUnit

        nl =
            normalizeUnit leftUnit
    in
    if not (rightUnit |> hasSameDimension leftUnit) then
        Err
            (DifferentDimensionValue
                { operation = Subtraction
                , left = leftUnit
                , right = rightUnit
                }
            )

    else
        case nl.numericalValue of
            Err str ->
                Err (CannotConvertToSI str)

            Ok vl ->
                case nr.numericalValue of
                    Err str ->
                        Err (CannotConvertToSI str)

                    Ok vr ->
                        Ok ( l - r * vr / vl, leftUnit )


{-| Multiply one quantity by another.
Fails if unit for either of two contains inconvertible unit like ℃.
-}
multiplyBy : Quantity -> Quantity -> Result Error Quantity
multiplyBy ( r, rightUnit ) ( l, leftUnit ) =
    let
        nr =
            normalizeUnit rightUnit

        nl =
            normalizeUnit leftUnit

        n =
            nl |> times_ nr
    in
    case n.numericalValue of
        Err str ->
            Err (CannotConvertToSI str)

        Ok _ ->
            case nr.numericalValue of
                Err str ->
                    Err (CannotConvertToSI str)

                Ok _ ->
                    Ok ( l * r, Mul leftUnit rightUnit )


{-| Divides one quantity by another.
Fails if unit for either of two contains inconvertible unit like ℃.
-}
divideBy : Quantity -> Quantity -> Result Error Quantity
divideBy ( r, rightUnit ) ( l, leftUnit ) =
    let
        nr =
            normalizeUnit rightUnit

        nl =
            normalizeUnit leftUnit

        n =
            nl |> times_ (nr |> toThePowerOf -1)
    in
    case n.numericalValue of
        Err str ->
            Err (CannotConvertToSI str)

        Ok _ ->
            Ok ( l / r, Div leftUnit rightUnit )


{-| Applies transformations sequentially, stopping at the first error.
-}
applySequentially :
    List (Quantity -> Result Error Quantity)
    -> Quantity
    -> Result Error Quantity
applySequentially l q =
    List.foldl Result.andThen (Ok q) l



--
-- -- Arithmetics Helper --{{{
--


type alias NormalizedUnit =
    -- INTERNAL:
    -- Canonical representation of units used only for computation.
    -- Not exposed to users. (in order to prevent record constructor being used.)
    { meterExponent : Int
    , kilogramExponent : Int
    , secondExponent : Int
    , kelvinExponent : Int
    , ampereExponent : Int
    , moleExponent : Int
    , candelaExponent : Int
    , numericalValue : Result String Float
    }


normalizePrefixedSymbol : Prefix -> UnitSymbol -> NormalizedUnit
normalizePrefixedSymbol prefix (UnitSymbol unit) =
    let
        u0 =
            unit.baseSIExpression
    in
    { u0 | numericalValue = Result.map (applyPrefix prefix) u0.numericalValue }


times_ : NormalizedUnit -> NormalizedUnit -> NormalizedUnit
times_ u2 u1 =
    { meterExponent = u1.meterExponent + u2.meterExponent
    , kilogramExponent = u1.kilogramExponent + u2.kilogramExponent
    , secondExponent = u1.secondExponent + u2.secondExponent
    , kelvinExponent = u1.kelvinExponent + u2.kelvinExponent
    , ampereExponent = u1.ampereExponent + u2.ampereExponent
    , moleExponent = u1.moleExponent + u2.moleExponent
    , candelaExponent = u1.candelaExponent + u2.candelaExponent
    , numericalValue =
        case u1.numericalValue of
            Ok v1 ->
                case u2.numericalValue of
                    Ok v2 ->
                        Ok (v1 * v2)

                    Err e2 ->
                        Err e2

            Err e1 ->
                Err e1
    }


toThePowerOf : Int -> NormalizedUnit -> NormalizedUnit
toThePowerOf p n =
    { meterExponent = p * n.meterExponent
    , kilogramExponent = p * n.kilogramExponent
    , secondExponent = p * n.secondExponent
    , kelvinExponent = p * n.kelvinExponent
    , ampereExponent = p * n.ampereExponent
    , moleExponent = p * n.moleExponent
    , candelaExponent = p * n.candelaExponent
    , numericalValue =
        case n.numericalValue of
            Ok v ->
                Ok (v ^ toFloat p)

            Err _ ->
                n.numericalValue
    }


normalizeUnit : Unit -> NormalizedUnit
normalizeUnit unit =
    case unit of
        One ->
            NormalizedUnit 0 0 0 0 0 0 0 (Ok 1)

        Atom p s ->
            normalizePrefixedSymbol p s

        Parened _ u0 ->
            normalizeUnit u0

        Pow e u0 ->
            normalizeUnit u0 |> toThePowerOf e

        Mul u1 u2 ->
            times_ (normalizeUnit u1) (normalizeUnit u2)

        Div u1 u2 ->
            times_ (normalizeUnit u1) (normalizeUnit u2 |> toThePowerOf -1)



-- -- }}}
--
-- }}}
--
-- Display --{{{
--
-- -- Display Options --{{{


{-| Type that represents configurations for displaying units as strings.
This includes options about:

  - output format
      - for now, 3 options are available.
          - ascii characters like `um s^-1` (This is default value)
          - unicode characters like `μm s⁻¹`
          - tex string like `{\\rm{{\\mu}{m}}\\,/\\,{s^3}}`.
  - unit separator in arbitrary string ( default value is " ")
  - how dimensionless unit should be displayed ( default value is "[-]")
  - In which way `one |> per (u second)` should be expressed, `1 / s` or `/ s`.
  - (for tex user) the outermost slash symbol could be expressed in `\\frac{<numerator>}{<denominator>}` style.

-}
type DisplayOption
    = DisplayOption DisplayOptionContent


type alias DisplayOptionContent =
    { outputFormat : OutputFormat
    , separator : String
    , separatorAfterCloseParen : String
    , separatorBeforeOpenParen : String
    , separatorBetweenParens : String
    , slash : String
    , slashAfterCloseParen : String
    , slashBeforeOpenParen : String
    , slashBetweenParens : String
    , headingSlash : String
    , headingSlashBeforeOpenParen : String
    , beforeNumerator : String
    , afterDenominator : String
    , openParen : String
    , closeParen : String
    , emptyUnitString : String
    , displayTopNumeratorOne : Bool
    , useTopLevelTexFrac : Bool
    , useCustomSpacing : Bool
    }


{-| default values for `DisplayOption`
-}
defaultDisplayOption : DisplayOption
defaultDisplayOption =
    DisplayOption
        { outputFormat = Ascii
        , separator = " "
        , separatorAfterCloseParen = " "
        , separatorBeforeOpenParen = " "
        , separatorBetweenParens = ""
        , slash = " / "
        , slashAfterCloseParen = "/ "
        , slashBeforeOpenParen = " /"
        , slashBetweenParens = "/"
        , headingSlash = "/ "
        , headingSlashBeforeOpenParen = "/"
        , beforeNumerator = ""
        , afterDenominator = ""
        , openParen = "("
        , closeParen = ")"
        , emptyUnitString = "[-]"
        , displayTopNumeratorOne = True
        , useTopLevelTexFrac = False
        , useCustomSpacing = False
        }


{-| default values for `DisplayOption` tuned for TeX output.
-}
defaultDisplayOptionForTex : DisplayOption
defaultDisplayOptionForTex =
    DisplayOption
        { outputFormat = Tex
        , separator = "\\,"
        , separatorAfterCloseParen = "\\,"
        , separatorBeforeOpenParen = "\\,"
        , separatorBetweenParens = ""
        , slash = "\\,\\middle/\\,"
        , slashAfterCloseParen = "\\middle/\\,"
        , slashBeforeOpenParen = "\\,\\middle/"
        , slashBetweenParens = "\\middle/"
        , headingSlash = "\\middle/\\,"
        , headingSlashBeforeOpenParen = "\\middle/"
        , beforeNumerator = "\\left."
        , afterDenominator = "\\right."
        , openParen = "\\left("
        , closeParen = "\\right)"
        , emptyUnitString = "[-]"
        , displayTopNumeratorOne = True
        , useTopLevelTexFrac = False
        , useCustomSpacing = False
        }


{-| Change the output format into tex string
-}
inTex : DisplayOption -> DisplayOption
inTex (DisplayOption d) =
    DisplayOption
        { d | outputFormat = Tex }


{-| Change the output format into ascii string
-}
inAscii : DisplayOption -> DisplayOption
inAscii (DisplayOption d) =
    DisplayOption
        { d | outputFormat = Ascii }


{-| Change the output format into unicode string
-}
inUnicode : DisplayOption -> DisplayOption
inUnicode (DisplayOption d) =
    DisplayOption
        { d | outputFormat = Unicode }


{-| Change the unit separator into arbitrary string.
-}
setSeparator : String -> DisplayOption -> DisplayOption
setSeparator s (DisplayOption d) =
    DisplayOption
        { d | separator = s }


{-| Change the slash into arbitrary string.
-}
setSlash : String -> DisplayOption -> DisplayOption
setSlash s (DisplayOption d) =
    DisplayOption
        { d | slash = s }


{-| display like "K /(mol / kg)". Note that space between "/" and "(" are removed.
-}
useCustomSpacing : DisplayOption -> DisplayOption
useCustomSpacing (DisplayOption d) =
    DisplayOption
        { d | useCustomSpacing = True }


{-| cancels `useCustomSpacing`.
-}
doNotUseCustomSpacing : DisplayOption -> DisplayOption
doNotUseCustomSpacing (DisplayOption d) =
    DisplayOption
        { d | useCustomSpacing = False }


{-| modifies effect of `useCustomSpacing`.
-}
setCustomSeparatorAndSlash :
    { separator : String
    , separatorAfterCloseParen : String
    , separatorBeforeOpenParen : String
    , separatorBetweenParens : String
    , slash : String
    , slashAfterCloseParen : String
    , slashBeforeOpenParen : String
    , slashBetweenParens : String
    }
    -> DisplayOption
    -> DisplayOption
setCustomSeparatorAndSlash r (DisplayOption d) =
    DisplayOption
        { d
            | separator = r.separator
            , separatorAfterCloseParen = r.separatorAfterCloseParen
            , separatorBeforeOpenParen = r.separatorBeforeOpenParen
            , separatorBetweenParens = r.separatorBetweenParens
            , slash = r.slash
            , slashAfterCloseParen = r.slashAfterCloseParen
            , slashBeforeOpenParen = r.slashBeforeOpenParen
            , slashBetweenParens = r.slashBetweenParens
            , useCustomSpacing = True
        }


{-| Change the heading slash (like "/ " in "6.02×10²³ / mol") into arbitrary string.
-}
setHeadingSlash : String -> DisplayOption -> DisplayOption
setHeadingSlash s (DisplayOption d) =
    DisplayOption
        { d | headingSlash = s }


{-| Change the paren into arbitrary string.
-}
setParen : { open : String, close : String } -> DisplayOption -> DisplayOption
setParen { open, close } (DisplayOption d) =
    DisplayOption
        { d | openParen = open, closeParen = close }


{-| Change the expression of empty unit(`1`) into arbitrary string.
-}
setEmptyUnitString : String -> DisplayOption -> DisplayOption
setEmptyUnitString s (DisplayOption d) =
    DisplayOption
        { d | emptyUnitString = s }


{-| Display like `1 / s`.
-}
displayTopNumeratorOne : DisplayOption -> DisplayOption
displayTopNumeratorOne (DisplayOption d) =
    DisplayOption
        { d | displayTopNumeratorOne = True }


{-| Display like `/ s`.
-}
deleteTopNumeratorOne : DisplayOption -> DisplayOption
deleteTopNumeratorOne (DisplayOption d) =
    DisplayOption
        { d | displayTopNumeratorOne = False }


{-| Display like `\\frac{{\\rm{mol}}\\,/\\,{\\rm{L}}}{{\\rm{s}}}`.
This enforces `displayTopNumeratorOne` to be applied.
-}
useTopLevelTexFrac : DisplayOption -> DisplayOption
useTopLevelTexFrac (DisplayOption d) =
    DisplayOption
        { d
            | useTopLevelTexFrac = True
            , displayTopNumeratorOne = True
        }


{-| Display like `\\left({\\rm{mol}}\\,/\\,{\\rm{L}}\\right)\\,/\\,{\\rm{s}}`
-}
doNotUseTopLevelTexFrac : DisplayOption -> DisplayOption
doNotUseTopLevelTexFrac (DisplayOption d) =
    DisplayOption
        { d | useTopLevelTexFrac = False }


type OutputFormat
    = Tex
    | Ascii
    | Unicode



-- -- }}}
--
-- -- Display Helper --{{{


deleteAllParen : Unit -> Unit
deleteAllParen u0 =
    case u0 of
        One ->
            u0

        Atom _ _ ->
            u0

        Parened _ u1 ->
            deleteAllParen u1

        Pow e0 u1 ->
            Pow e0 (deleteAllParen u1)

        Mul ul ur ->
            Mul (deleteAllParen ul) (deleteAllParen ur)

        Div ul ur ->
            Div (deleteAllParen ul) (deleteAllParen ur)


addNeededParen : Unit -> Unit
addNeededParen u0 =
    case u0 of
        One ->
            u0

        Atom _ _ ->
            u0

        Parened b u1 ->
            Parened b (addNeededParen u1)

        Pow e0 u1 ->
            Pow e0
                (case u1 of
                    Pow e1 u2 ->
                        Parened False (Pow e1 (addNeededParen u2))

                    Mul u2 u3 ->
                        Parened False (Mul (addNeededParen u2) (addNeededParen u3))

                    Div u2 u3 ->
                        Parened False (Div (addNeededParen u2) (addNeededParen u3))

                    _ ->
                        addNeededParen u1
                )

        Mul ul ur ->
            Mul
                (case ul of
                    Div ull ulr ->
                        Parened False (Div (addNeededParen ull) (addNeededParen ulr))

                    _ ->
                        addNeededParen ul
                )
                (addNeededParen ur)

        Div ul ur ->
            case ul of
                Div ull ulr ->
                    addNeededParen
                        (Div
                            (Parened False (Div (addNeededParen ull) (addNeededParen ulr)))
                            ur
                        )

                _ ->
                    Div
                        (addNeededParen ul)
                        (case ur of
                            Mul url urr ->
                                Parened False (Mul (addNeededParen url) (addNeededParen urr))

                            Div url urr ->
                                Parened False (Div (addNeededParen url) (addNeededParen urr))

                            _ ->
                                addNeededParen ur
                        )



-- -- }}}
--


{-| Converts unit into string with specified display options.
-}
toString : DisplayOption -> Unit -> String
toString ((DisplayOption dr) as d) rowUnit =
    let
        unit =
            addNeededParen rowUnit
    in
    (case unit of
        One ->
            dr.emptyUnitString

        Div nU dU ->
            if dr.useTopLevelTexFrac then
                "\\frac{"
                    ++ (case nU of
                            One ->
                                "1"

                            _ ->
                                toStringInner dr nU
                       )
                    ++ "}{"
                    ++ (case dU of
                            Parened False dU_ ->
                                toStringInner dr dU_

                            _ ->
                                toStringInner dr dU
                       )
                    ++ "}"

            else if not dr.displayTopNumeratorOne then
                case nU of
                    One ->
                        if dr.useCustomSpacing then
                            if startsWithParen dU then
                                dr.beforeNumerator
                                    ++ dr.headingSlashBeforeOpenParen
                                    ++ toStringInner dr dU
                                    ++ dr.afterDenominator

                            else
                                dr.beforeNumerator
                                    ++ dr.slash
                                    ++ toStringInner dr dU
                                    ++ dr.afterDenominator

                        else
                            dr.beforeNumerator
                                ++ dr.slash
                                ++ toStringInner dr dU
                                ++ dr.afterDenominator

                    _ ->
                        toStringInner dr unit

            else
                toStringInner dr unit

        _ ->
            toStringInner dr unit
    )
        |> (\s ->
                case dr.outputFormat of
                    Tex ->
                        "{\\rm{" ++ s ++ "}}"

                    _ ->
                        s
           )


toStringInner : DisplayOptionContent -> Unit -> String
toStringInner d unit =
    case unit of
        One ->
            "1"

        Atom p s ->
            prefixToString d.outputFormat p
                ++ symbolToString d.outputFormat s

        Parened b u1 ->
            d.openParen
                ++ toStringInner d u1
                ++ d.closeParen

        Pow e u1 ->
            toStringInner d u1
                ++ exponentToString d.outputFormat e

        Mul u1 u2 ->
            toStringInner d u1
                ++ (if not d.useCustomSpacing then
                        d.separator

                    else
                        case ( endsWithParen u1, startsWithParen u2 ) of
                            ( True, True ) ->
                                d.separatorBetweenParens

                            ( True, False ) ->
                                d.separatorAfterCloseParen

                            ( False, True ) ->
                                d.separatorBeforeOpenParen

                            ( False, False ) ->
                                d.separator
                   )
                ++ toStringInner d u2

        Div u1 u2 ->
            d.beforeNumerator
                ++ toStringInner d u1
                ++ (if not d.useCustomSpacing then
                        d.slash

                    else
                        case ( endsWithParen u1, startsWithParen u2 ) of
                            ( True, True ) ->
                                d.slashBetweenParens

                            ( True, False ) ->
                                d.slashAfterCloseParen

                            ( False, True ) ->
                                d.slashBeforeOpenParen

                            ( False, False ) ->
                                d.slash
                   )
                ++ toStringInner d u2
                ++ d.afterDenominator


startsWithParen : Unit -> Bool
startsWithParen u0 =
    case u0 of
        Parened _ _ ->
            True

        Pow _ ub ->
            startsWithParen ub

        Mul ul _ ->
            startsWithParen ul

        Div ul _ ->
            startsWithParen ul

        _ ->
            False


endsWithParen : Unit -> Bool
endsWithParen u0 =
    case u0 of
        Parened _ _ ->
            True

        Pow _ ub ->
            endsWithParen ub

        Mul _ ur ->
            endsWithParen ur

        Div _ ur ->
            endsWithParen ur

        -- impossible after `addNeededParen`
        _ ->
            False



--
-- -- Display Helpers --{{{


prefixToString : OutputFormat -> Prefix -> String
prefixToString outputFormat prefix =
    case outputFormat of
        Tex ->
            case prefix of
                Quecto ->
                    "{q}"

                Ronto ->
                    "{r}"

                Yocto ->
                    "{y}"

                Zepto ->
                    "{z}"

                Atto ->
                    "{a}"

                Femto ->
                    "{f}"

                Pico ->
                    "{p}"

                Nano ->
                    "{n}"

                Micro ->
                    "{\\mu}"

                Milli ->
                    "{m}"

                Centi ->
                    "{c}"

                Deci ->
                    "{d}"

                NoPrefix ->
                    ""

                Deca ->
                    "{da}"

                Hecto ->
                    "{h}"

                Kilo ->
                    "{k}"

                Mega ->
                    "{M}"

                Giga ->
                    "{G}"

                Tera ->
                    "{T}"

                Peta ->
                    "{P}"

                Exa ->
                    "{E}"

                Zetta ->
                    "{Z}"

                Yotta ->
                    "{Y}"

                Ronna ->
                    "{R}"

                Quetta ->
                    "{Q}"

        Ascii ->
            case prefix of
                Quecto ->
                    "q"

                Ronto ->
                    "r"

                Yocto ->
                    "y"

                Zepto ->
                    "z"

                Atto ->
                    "a"

                Femto ->
                    "f"

                Pico ->
                    "p"

                Nano ->
                    "n"

                Micro ->
                    "u"

                Milli ->
                    "m"

                Centi ->
                    "c"

                Deci ->
                    "d"

                NoPrefix ->
                    ""

                Deca ->
                    "da"

                Hecto ->
                    "h"

                Kilo ->
                    "k"

                Mega ->
                    "M"

                Giga ->
                    "G"

                Tera ->
                    "T"

                Peta ->
                    "P"

                Exa ->
                    "E"

                Zetta ->
                    "Z"

                Yotta ->
                    "Y"

                Ronna ->
                    "R"

                Quetta ->
                    "Q"

        Unicode ->
            case prefix of
                Quecto ->
                    "q"

                Ronto ->
                    "r"

                Yocto ->
                    "y"

                Zepto ->
                    "z"

                Atto ->
                    "a"

                Femto ->
                    "f"

                Pico ->
                    "p"

                Nano ->
                    "n"

                Micro ->
                    "μ"

                Milli ->
                    "m"

                Centi ->
                    "c"

                Deci ->
                    "d"

                NoPrefix ->
                    ""

                Deca ->
                    "da"

                Hecto ->
                    "h"

                Kilo ->
                    "k"

                Mega ->
                    "M"

                Giga ->
                    "G"

                Tera ->
                    "T"

                Peta ->
                    "P"

                Exa ->
                    "E"

                Zetta ->
                    "Z"

                Yotta ->
                    "Y"

                Ronna ->
                    "R"

                Quetta ->
                    "Q"


symbolToString : OutputFormat -> UnitSymbol -> String
symbolToString outputFormat (UnitSymbol symbol) =
    case outputFormat of
        Tex ->
            symbol.outputInTex

        Ascii ->
            symbol.outputInAscii

        Unicode ->
            symbol.outputInUnicode


exponentToString : OutputFormat -> Int -> String
exponentToString outputFormat n =
    if n == 0 then
        ""

    else
        case outputFormat of
            Tex ->
                "^{" ++ String.fromInt n ++ "}"

            Ascii ->
                "^" ++ String.fromInt n

            Unicode ->
                intToSuperscript n


superScriptTable : Char -> Char
superScriptTable char =
    case char of
        '0' ->
            '⁰'

        '1' ->
            '¹'

        '2' ->
            '²'

        '3' ->
            '³'

        '4' ->
            '⁴'

        '5' ->
            '⁵'

        '6' ->
            '⁶'

        '7' ->
            '⁷'

        '8' ->
            '⁸'

        '9' ->
            '⁹'

        '-' ->
            '⁻'

        _ ->
            'x'


intToSuperscript : Int -> String
intToSuperscript n =
    String.fromInt n
        |> String.toList
        |> List.map superScriptTable
        |> String.fromList
        |> (\s ->
                if s == "¹" then
                    ""

                else
                    s
           )



-- -- }}}
--
-- }}}
--
