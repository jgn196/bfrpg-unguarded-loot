module Unguarded exposing (Loot, lootGeneratorForLevel)

import Random exposing (Generator)
import Random.Extra


type alias Loot =
    { description : String, value : Float }


lootGeneratorForLevel : Int -> Generator (List Loot)
lootGeneratorForLevel level =
    lootGeneratorsForLevel level |> Random.Extra.sequence |> Random.map List.concat


lootGeneratorsForLevel : Int -> List (Generator (List Loot))
lootGeneratorsForLevel level =
    case level of
        1 ->
            [ coinsGenerator 75 d8 copperCoins
            , coinsGenerator 50 d6 silverCoins
            , coinsGenerator 25 d4 electrumCoins
            , coinsGenerator 7 d4 goldCoins
            , coinsGenerator 1 d4 platinumCoins
            , gemsGenerator 7 d4
            , jeweleriesGenerator 3 d4
            , magicItemsGenerator 2
            ]

        2 ->
            [ coinsGenerator 50 d10 copperCoins
            , coinsGenerator 50 d8 silverCoins
            , coinsGenerator 25 d6 electrumCoins
            , coinsGenerator 20 d6 goldCoins
            , coinsGenerator 2 d4 platinumCoins
            , gemsGenerator 10 d6
            , jeweleriesGenerator 7 d4
            , magicItemsGenerator 5
            ]

        3 ->
            [ coinsGenerator 30 (rollX_dY 2 6) copperCoins
            , coinsGenerator 50 d10 silverCoins
            , coinsGenerator 25 d8 electrumCoins
            , coinsGenerator 50 d6 goldCoins
            , coinsGenerator 4 d4 platinumCoins
            , gemsGenerator 15 d6
            , jeweleriesGenerator 7 d6
            , magicItemsGenerator 8
            ]

        4 ->
            [ coinsGenerator 20 (rollX_dY 3 6) copperCoins
            , coinsGenerator 50 (rollX_dY 2 6) silverCoins
            , coinsGenerator 25 d10 electrumCoins
            , coinsGenerator 50 (rollX_dY 2 6) goldCoins
            , coinsGenerator 8 d4 platinumCoins
            , gemsGenerator 20 d8
            , jeweleriesGenerator 10 d6
            , magicItemsGenerator 12
            ]

        5 ->
            lootGeneratorsForLevel 4

        6 ->
            [ coinsGenerator 15 (rollX_dY 4 6) copperCoins
            , coinsGenerator 50 (rollX_dY 3 6) silverCoins
            , coinsGenerator 25 d12 electrumCoins
            , coinsGenerator 70 (rollX_dY 2 8) goldCoins
            , coinsGenerator 15 d4 platinumCoins
            , gemsGenerator 30 d8
            , jeweleriesGenerator 15 d6
            , magicItemsGenerator 16
            ]

        7 ->
            lootGeneratorsForLevel 6

        8 ->
            [ coinsGenerator 10 (rollX_dY 5 6) copperCoins
            , coinsGenerator 50 (rollX_dY 5 6) silverCoins
            , coinsGenerator 25 (rollX_dY 2 8) electrumCoins
            , coinsGenerator 75 (rollX_dY 4 6) goldCoins
            , coinsGenerator 30 d4 platinumCoins
            , gemsGenerator 40 d8
            , jeweleriesGenerator 30 d8
            , magicItemsGenerator 20
            ]

        _ ->
            if level > 8 then
                lootGeneratorsForLevel 8

            else
                []


coinsGenerator : Int -> Generator Int -> (Generator Int -> Generator Loot) -> Generator (List Loot)
coinsGenerator chance roll toCoins =
    Random.Extra.maybe (percentageChance chance) (roll |> hundredsOf |> toCoins)
        |> Random.map maybeToList


maybeToList : Maybe a -> List a
maybeToList a =
    a
        |> Maybe.map List.singleton
        |> Maybe.withDefault []


percentageChance : Int -> Generator Bool
percentageChance chance =
    Random.map (\r -> r <= chance) d100


rollX_dY : Int -> Int -> Generator Int
rollX_dY count sides =
    let
        roll =
            rollD sides
    in
    if count == 1 then
        roll

    else
        Random.map2 (\x y -> x + y) roll (rollX_dY (count - 1) sides)


rollD : Int -> Generator Int
rollD sides =
    Random.int 1 sides


d100 =
    rollD 100


d12 =
    rollD 12


d10 =
    rollD 10


d8 =
    rollD 8


d6 =
    rollD 6


d4 =
    rollD 4



-- Coins


hundredsOf : Generator Int -> Generator Int
hundredsOf =
    Random.map (\r -> r * 100)


copperCoins : Generator Int -> Generator Loot
copperCoins =
    coins "cp" 0.01


coins : String -> Float -> Generator Int -> Generator Loot
coins metal value =
    Random.map
        (\count ->
            { description = String.fromInt count ++ " " ++ metal, value = toFloat count * value }
        )


silverCoins : Generator Int -> Generator Loot
silverCoins =
    coins "sp" 0.1


electrumCoins =
    coins "ep" 0.5


goldCoins =
    coins "gp" 1


platinumCoins =
    coins "pp" 10



-- Gems


gemsGenerator : Int -> Generator Int -> Generator (List Loot)
gemsGenerator chance countRoll =
    percentageChance chance
        |> Random.andThen
            (\b ->
                if b then
                    countRoll |> Random.andThen (\count -> Random.list count gemGenerator)

                else
                    Random.constant []
            )


gemGenerator : Generator Loot
gemGenerator =
    Random.map2 (\value gemType -> { description = gemDescription value gemType, value = toFloat value.value })
        gemValueGenerator
        gemTypeGenerator


gemDescription : GemValue -> String -> String
gemDescription value gemType =
    value.description ++ " " ++ gemType ++ " (" ++ String.fromInt value.value ++ " gp)"


type alias GemValue =
    { description : String, value : Int }


gemValueGenerator : Generator GemValue
gemValueGenerator =
    Random.weighted ( 20, { description = "Ornamental", value = 10 } )
        [ ( 25, { description = "Semiprecious", value = 50 } )
        , ( 30, { description = "Fancy", value = 100 } )
        , ( 20, { description = "Precious", value = 500 } )
        , ( 5, { description = "Gem", value = 1000 } )
        ]


gemTypeGenerator : Generator String
gemTypeGenerator =
    Random.weighted ( 10, "Greenstone" )
        [ ( 10, "Malachite" )
        , ( 8, "Aventurine" )
        , ( 9, "Phenalope" )
        , ( 6, "Amethyst" )
        , ( 9, "Flurospar" )
        , ( 6, "Garnet" )
        , ( 5, "Alexandrite" )
        , ( 5, "Topaz" )
        , ( 5, "Bloodstone" )
        , ( 4, "Sapphire" )
        , ( 10, "Diamond" )
        , ( 4, "Fire Opal" )
        , ( 3, "Ruby" )
        , ( 3, "Emerald" )
        ]



-- Jewelery


jeweleriesGenerator : Int -> Generator Int -> Generator (List Loot)
jeweleriesGenerator chance roll =
    percentageChance chance
        |> Random.andThen
            (\b ->
                if b then
                    Random.andThen (\c -> Random.list c jeweleryGenerator) roll

                else
                    Random.constant []
            )


jeweleryGenerator : Generator Loot
jeweleryGenerator =
    Random.map2 (\jeweleryType value -> { description = jeweleryDescription jeweleryType value, value = toFloat value })
        jeweleryTypeGenerator
        (rollX_dY 2 8 |> hundredsOf)


jeweleryDescription : String -> Int -> String
jeweleryDescription jeweleryType value =
    "Jewelled " ++ jeweleryType ++ " (" ++ String.fromInt value ++ " gp)"


jeweleryTypeGenerator : Generator String
jeweleryTypeGenerator =
    Random.weighted ( 6, "Anklet" )
        [ ( 6, "Belt" )
        , ( 2, "Bowl" )
        , ( 7, "Bracelet" )
        , ( 6, "Brooch" )
        , ( 5, "Buckle" )
        , ( 5, "Chain" )
        , ( 3, "Choker" )
        , ( 2, "Circlet" )
        , ( 5, "Clasp" )
        , ( 4, "Comb" )
        , ( 1, "Crown" )
        , ( 3, "Cup" )
        , ( 7, "Earring" )
        , ( 3, "Flagon" )
        , ( 3, "Goblet" )
        , ( 5, "Knife" )
        , ( 4, "Letter Opener" )
        , ( 3, "Locket" )
        , ( 2, "Medal" )
        , ( 7, "Necklace" )
        , ( 1, "Plate" )
        , ( 5, "Pin" )
        , ( 1, "Sceptre" )
        , ( 3, "Statuette" )
        , ( 1, "Tiara" )
        ]



-- Magic items


magicItemsGenerator : Int -> Generator (List Loot)
magicItemsGenerator chance =
    Random.Extra.maybe (percentageChance chance) magicItemGenerator
        |> Random.map maybeToList


magicItemGenerator : Generator Loot
magicItemGenerator =
    Random.constant { description = "Any 1 magic item", value = 0.0 }
