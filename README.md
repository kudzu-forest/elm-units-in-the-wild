# elm-units-in-the-wild
_Units as written and read by humans -- found in the wild._

## What This Package Is For

The main purpose of this package is to provide functionality for working with units, such as:

1. defining units,
2. displaying units as strings with various formats and options,
3. parsing strings into units,
4. performing arithmetic on physical quantities represented as floating-point numbers paired with units.

Below is a code example that outlines the main features of this package.

```elm
import Units as U exposing
    ( Unit, u, pu
    , centi, kilo, mega
    , meter, gram, minute, hour
    , per, inverse, cubic
    )

-- You can define units as follows.

unitForDensity : Unit
unitForDensity =
    u gram |> per (cubic (pu centi meter))
    -- `u` and `pu` stand for "unit" and "prefixed unit".

-- If the units you need are not defined in this package,
-- you can define new ones.

atomicMassUnit : U.UnitSymbol
atomicMassUnit =
    U.createUnitSymbol
        { parserInput = [ "u", "amu" ] -- Can be `[]` if parsing is not needed.
        , allowPrefix = False -- Prefixes like "ku" or "Mu" are not allowed.
        , display = "u"
        , baseSIExpression =
            { meterExponent = 0
            , kilogramExponent = 1
            , secondExponent = 0
            , kelvinExponent = 0
            , ampereExponent = 0
            , moleExponent = 0
            , candelaExponent = 0
            , numericalValue =
                Ok <| 1 / (1000 * 6.02214076e23)
                -- Note: the SI base unit for mass is kilogram, not gram.
            }
        }

electronVolt : U.UnitSymbol
electronVolt =
    U.createUnitSymbol
        { parserInput = [ "eV" ]
        , allowPrefix = True
        -- Prefixes like "keV" and "MeV" are automatically added.
        , display = "eV"
        , baseSIExpression =
            { meterExponent = 2
            , kilogramExponent = 1
            , secondExponent = -2
            , kelvinExponent = 0
            , ampereExponent = 0
            , moleExponent = 0
            , candelaExponent = 0
            , numericalValue = Ok 1.60217663e-19
            }
        }

-- You can stringify units using the `toString` function.

densityUnitInAscii : String
densityUnitInAscii =
    U.toString U.defaultDisplayOption unitForDensity

densityUnitInAscii
    --> "g / cm^3"
    -- doctest


-- You can also use other output formats such as TeX.
-- If you are not familiar with TeX or MathJax,
-- you may safely ignore the backslash-heavy output below.

densityUnitInTex : String
densityUnitInTex =
    U.toString U.defaultDisplayOptionForTex unitForDensity

densityUnitInTex
    --> "{\\rm{\\left.g\\,\\middle/\\,{c}m^{3}\\right.}}"
    -- doctest


-- You can define a parser to convert dynamically obtained strings into `Unit` values.

unitParser : U.Parser
unitParser =
    U.toParser U.defaultUnitSymbols

parsedUnit : Result (List U.DeadEnd) Unit
parsedUnit =
    "km / h"
        |> U.parse unitParser

parsedUnit
    --> Ok (pu kilo meter |> per (u hour))


-- You can add your own `UnitSymbol` values to a parser definition.

myUnitParser : U.Parser
myUnitParser =
    U.defaultUnitSymbols
        |> U.addUnitSymbols
            [ atomicMassUnit
            , electronVolt
            ]
        -- This returns a `Result` to guard against name collisions.
        |> Result.map U.toParser
        |> Result.withDefault U.failureParser


parsedUnit2 : Result (List U.DeadEnd) Unit
parsedUnit2 =
    U.parse myUnitParser "MeV u^-1"

parsedUnit2
    --> Ok (pu mega electronVolt |> U.times (inverse (u atomicMassUnit)))
    -- doctest


-- Simple arithmetic can also be performed with this package.

travelDistance : Result U.Error U.Quantity
travelDistance =
    ( 90, pu kilo meter |> per (u hour) )
        |> U.applySequentially
            [ U.multiplyBy ( 20, u minute )
            , U.convertInto ( u meter )
            ]
        |> Result.map (Tuple.mapFirst (round >> toFloat))

travelDistance
    --> Ok ( 30000, u meter )
```




### Main Philosophy
In this package, _units_ are treated as expressions written and read by humans.

For example, `J`, `N·m`, `kg·m/s^2`, and `kg·m·s^{-2}` all have the same physical dimension,
but they are considered distinct units because they convey different intentions.

For this reason, this package does not perform automatic normalization of unit expressions.
```elm
import Units as U exposing (pu, u, kilo, meter, minute, hour, per, multiplyBy)

travelDistanceString : Result U.Error String
travelDistanceString =
    ( 90, pu kilo meter |> per (u hour) )
        |> multiplyBy ( 20, u minute)
        |> Result.map
            (\(numericalValue, unit) ->
                String.fromInt (round numericalValue)
                    ++ " "
                    ++ U.toString U.defaultDisplayOption unit
            )

travelDistanceString
        --> Ok "1800 (km / h) min"
```
This behavior is intentional.
If you want to keep unit expressions compact or convert them into a specific unit,
call `convertInto` or `normalize` explicitly at appropriate points in your calculation.

### Displaying Units

Several display options are configurable:

- output format (currently three options: Ascii, Unicode, and TeX)
- whether to narrow the space between ")", "/" and "("
- whether to use `1` as the numerator (`1 / s` or `/ s`)
- the format of slashes, separators between multiplied units, and parentheses
- what is shown when the unit is dimensionless (`""`, `"1"`, `"[-]"` or whatever `String` you can choose).
- (for TeX output) whether to use the `\frac{<numerator>}{<denominator>}` form at the top level

Try these formats in [Ellie]()

### Parsing Units

Unit parsing with this package proceeds through the following steps:

1. First, define the set of units that should be parsable in your application.
2. Next, attempt to create a parser.
3. If it fails, check for name collisions and review which units should be included and whether they should allow prefixes.
4. If it succeeds, use the parser as needed.

The `Parser` exposed by the `Units` module is just an alias for
`Parser.Parser Units` from the `elm/parser` package.
This means you can easily combine it with other parsers as needed.
For example:

```elm
import Parser as P exposing ((|.), (|=))
import Units as U

unitParser : P.Parser U.Unit
unitParser =
    U.toParser U.defaultUnitSymbols

quantityParser : P.Parser U.Quantity
quantityParser =
    P.succeed (\num unit -> (num, unit))
        |= P.float
        |. P.spaces
        |= unitParser

parsed : Result (List P.DeadEnd) U.Quantity
parsed =
    P.run quantityParser "8.314e3 Pa L/(mol K)"

parsed
    --> Ok (8314, U.fromList [(U.noPrefix, U.pascal, 1), (U.noPrefix, U.liter, 1) ] |> U.per (U.paren (U.fromList [(U.noPrefix, U.mole, 1), (U.noPrefix, U.kelvin, 1) ])))
```


### Simple Arithmetic

Currently, this package supports addition, subtraction, multiplication, division,
and unit conversion as arithmetic operations.

Adding a function like
`apply : (Float -> Float) -> Quantity -> Quantity`
would be convenient, but functions such as `sqrt` or `sin`
require additional restrictions on the units of their arguments.

All arithmetic operations return a `Result`,
because some units (such as `℃`) are treated as inconvertible in this package.

If you find yourself repeatedly mapping over `Result`,
you can use
`applySequentially : List (Quantity -> Result Error Quantity) -> Quantity -> Result Error Quantity`.
(See the example above.)


## What This Package Is Not For

This package is not intended for:

- advanced dimensional analysis or fully type-safe physical calculations.
    - If you need stronger compile-time guarantees, consider using packages such as `elm-units`.
- use as an exhaustive reference of units.
    - This package is designed to be extensible rather than exhaustive.
    - You can add only what you need.
