module ASTUtil.Imports exposing (FunctionReference, buildImportInformation, findImportWithRange, naiveStringifyImport, removeRangeFromImport)

import AST.Ranges as Ranges
import Elm.Syntax.Base exposing (ModuleName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..), ValueConstructorExpose)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Import)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)
import List.Extra as List


type alias FunctionReference =
    { moduleName : ModuleName
    , exposesRegex : Bool
    }


{-| Will look for an import within the file that includes a range. Will return a `Nothing` when no such import exists.
-}
findImportWithRange : File -> Range -> Maybe Import
findImportWithRange ast range =
    ast.imports
        |> List.filter (.range >> Ranges.containsRange range)
        |> List.head


buildImportInformation : ModuleName -> String -> File -> Maybe FunctionReference
buildImportInformation moduleName function file =
    file.imports
        |> List.filter (\i -> i.moduleName == moduleName)
        |> List.head
        |> Maybe.map
            (\i ->
                { moduleName = Maybe.withDefault i.moduleName i.moduleAlias
                , exposesRegex = Maybe.map (exposesFunction function) i.exposingList |> Maybe.withDefault False
                }
            )


{-| TODO
-}
exposesFunction : String -> Exposing -> Bool
exposesFunction s exposure =
    case exposure of
        All _ ->
            True

        Explicit l ->
            List.any
                (\( _, x ) ->
                    case x of
                        FunctionExpose fun ->
                            fun == s

                        _ ->
                            False
                )
                l


naiveStringifyImport : Import -> String
naiveStringifyImport imp =
    String.concat <|
        [ "import "
        , String.join "." imp.moduleName
        , Maybe.withDefault "" <| Maybe.map (String.join "." >> (++) " as ") imp.moduleAlias
        , stringifyExposingList imp.exposingList
        ]


stringifyExposingList : Maybe Exposing -> String
stringifyExposingList exp =
    case exp of
        Nothing ->
            ""

        Just (All _) ->
            " exposing (..)"

        Just (Explicit explicits) ->
            " exposing "
                ++ (case explicits of
                        [] ->
                            ""

                        xs ->
                            let
                                areOnDifferentLines =
                                    rangesOnDifferentLines (List.map Tuple.first xs)

                                seperator =
                                    if areOnDifferentLines then
                                        "\n    , "
                                    else
                                        ", "
                            in
                            "(" ++ (List.map stringifyExpose explicits |> String.join seperator) ++ ")"
                   )


rangesOnDifferentLines : List Range -> Bool
rangesOnDifferentLines ranges =
    let
        starts =
            List.map (.start >> .row) ranges
    in
    List.length (List.unique starts) == List.length starts


stringifyExpose : Ranged TopLevelExpose -> String
stringifyExpose ( _, expose ) =
    case expose of
        InfixExpose s ->
            "(" ++ s ++ ")"

        FunctionExpose s ->
            s

        TypeOrAliasExpose s ->
            s

        TypeExpose exposedType ->
            stringifyExposedType exposedType


stringifyExposedType : ExposedType -> String
stringifyExposedType { name, open } =
    name
        ++ (case open of
                Nothing ->
                    ""

                Just _ ->
                    "(..)"
           )


removeRangeFromImport : Range -> Import -> Import
removeRangeFromImport range imp =
    { imp | exposingList = Maybe.andThen (removeRangeFromExposingList range) imp.exposingList }


removeRangeFromExposingList : Range -> Exposing -> Maybe Exposing
removeRangeFromExposingList range exp =
    case exp of
        All r ->
            if r == range then
                Nothing
            else
                Just (All r)

        Explicit exposedTypes ->
            case List.filterMap (removeRangeFromExpose range) exposedTypes of
                [] ->
                    Nothing

                x ->
                    Just (Explicit x)


removeRangeFromExpose : Range -> Ranged TopLevelExpose -> Maybe (Ranged TopLevelExpose)
removeRangeFromExpose range ( r, expose ) =
    Maybe.map (Tuple.pair r) <|
        case expose of
            InfixExpose x ->
                if r == range then
                    Nothing
                else
                    Just (InfixExpose x)

            FunctionExpose x ->
                if r == range then
                    Nothing
                else
                    Just (FunctionExpose x)

            TypeOrAliasExpose x ->
                if r == range then
                    Nothing
                else
                    Just (TypeOrAliasExpose x)

            TypeExpose exposedType ->
                Just
                    (TypeExpose
                        { exposedType
                            | open =
                                if exposedType.open == Just range then
                                    Nothing
                                else
                                    exposedType.open
                        }
                    )


removeRangeFromConstructors : Range -> Exposing -> Maybe Exposing
removeRangeFromConstructors range exp =
    case exp of
        All r ->
            if r == range then
                Nothing
            else
                Just (All r)

        Explicit pairs ->
            case List.filter (Tuple.first >> (/=) range) pairs of
                [] ->
                    Nothing

                x ->
                    Just (Explicit x)
