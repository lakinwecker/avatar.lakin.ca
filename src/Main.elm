-- Copyright 2019 Lakin Wecker
-- Licensed under the GPL v3.0
-- See LICENSE for more information.


module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Browser.Events as Events
import Html exposing (Html, Attribute, div, text)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Html.Attributes exposing (style, attribute)
import Dict
import Time
import Url


---- Base Types ----


type alias Size =
    { width : Int
    , height : Int
    }


type alias Flags =
    Size



---- Utils ----


shouldDebug : Bool
shouldDebug =
    False


boardSize : Int
boardSize =
    32


identity : a -> a
identity a =
    a



---- Cell ----


type alias NeighbourCounts =
    { white : Int
    , black : Int
    }


type Color
    = White
    | Black


type State
    = Spawning
    | Alive
    | Dying
    | Dead


type alias Cell =
    { state : State
    , color : Color
    , neighbours : NeighbourCounts
    }


totalNeighbors : Cell -> Int
totalNeighbors cell =
    cell.neighbours.white + cell.neighbours.black


defaultNeighbours : NeighbourCounts
defaultNeighbours =
    { white = 0, black = 0 }


defaultCell : Cell
defaultCell =
    { state = Dead, color = White, neighbours = defaultNeighbours }



---- Cells ----


type alias Index =
    ( Int, Int )


type alias Cells =
    Dict.Dict Index Cell


getBoardCell : Index -> Cells -> Cell
getBoardCell idx board =
    Maybe.withDefault
        defaultCell
        (Dict.get idx board)


neighbourOffsets : List Index
neighbourOffsets =
    [ ( -1, -1 )
    , ( 1, 1 )
    , ( -1, 1 )
    , ( 1, -1 )
    , ( -1, 0 )
    , ( 0, -1 )
    , ( 1, 0 )
    , ( 0, 1 )
    ]


neighboursIndices : Index -> List Index
neighboursIndices ( iX, iY ) =
    List.map (\( oX, oY ) -> ( iX + oX, iY + oY )) neighbourOffsets


updateNeighbour : ( Int, Int ) -> Index -> Cells -> Cells
updateNeighbour i idx board =
    Dict.insert
        idx
        (modifyNeighbourCount i (getBoardCell idx board))
        board


modifyNeighbourCount : ( Int, Int ) -> Cell -> Cell
modifyNeighbourCount ( w, b ) c =
    let
        n =
            { white = c.neighbours.white + w, black = c.neighbours.black + b }
    in
        { c | neighbours = n }


modifyCellType : Color -> State -> Cell -> Cell
modifyCellType color state c =
    { c | state = state, color = color }


changeCell : Color -> State -> ( Int, Int ) -> Index -> Cells -> Cells
changeCell color state i idx board =
    let
        updatedNeighbours =
            List.foldl
                (updateNeighbour i)
                board
                (neighboursIndices idx)

        newBoard =
            Dict.insert
                idx
                (modifyCellType color state (getBoardCell idx board))
                updatedNeighbours
    in
        newBoard


spawn : Color -> Index -> Cells -> Cells
spawn color =
    changeCell color
        Spawning
        (case color of
            White ->
                ( 1, 0 )

            Black ->
                ( 0, 1 )
        )


spawnToAlive : Index -> Cells -> Cells
spawnToAlive idx board =
    let
        c =
            getBoardCell idx board
    in
        changeCell c.color Alive ( 0, 0 ) idx board


die : Index -> Cells -> Cells
die idx board =
    let
        c =
            getBoardCell idx board

        ( modifiers, newState ) =
            case ( c.color, c.state ) of
                ( White, Alive ) ->
                    ( ( -1, 0 ), Dying )

                ( White, Spawning ) ->
                    ( ( -1, 0 ), Dying )

                ( Black, Alive ) ->
                    ( ( 0, -1 ), Dying )

                ( Black, Spawning ) ->
                    ( ( 0, -1 ), Dying )

                ( _, _ ) ->
                    ( ( 0, 0 ), Dead )
    in
        changeCell c.color newState modifiers idx board


toIndices : Cells -> List Index
toIndices cells =
    List.map Tuple.first (Dict.toList cells)


indicesThatShouldDie : Cells -> List Index
indicesThatShouldDie board =
    let
        shouldBeDying _ cell =
            (isAlive cell && shouldDie cell) || isDying cell
    in
        board
            |> Dict.filter shouldBeDying
            |> Dict.keys


indicesThatSpawnColor : Cells -> (Int -> Int -> Bool) -> List Index
indicesThatSpawnColor board colorMatch =
    let
        shouldSpawn _ cell =
            let
                blackN =
                    cell.neighbours.black

                whiteN =
                    cell.neighbours.white
            in
                (not (isAlive cell)) && (blackN + whiteN) == 3 && (colorMatch whiteN blackN)
    in
        board
            |> Dict.filter shouldSpawn
            |> Dict.keys


indicesThatSpawnWhite : Cells -> List Index
indicesThatSpawnWhite board =
    indicesThatSpawnColor board (\w _ -> w < 2)


indicesThatSpawnBlack : Cells -> List Index
indicesThatSpawnBlack board =
    indicesThatSpawnColor board (\_ b -> b < 2)


shouldDie : Cell -> Bool
shouldDie cell =
    let
        neighbourCount =
            cell.neighbours.white + cell.neighbours.black
    in
        neighbourCount < 2 || neighbourCount > 3


isSpawning : Cell -> Bool
isSpawning c =
    c.state == Spawning


isAlive : Cell -> Bool
isAlive c =
    c.state == Alive || c.state == Spawning


isDying : Cell -> Bool
isDying c =
    c.state == Dying


isDead : Cell -> Bool
isDead c =
    c.state == Dead


evolve : Cells -> Cells
evolve board =
    let
        clearDead b =
            indicesThatShouldDie board
                |> List.foldl die b

        changeSpawningToAlive b =
            b
                |> Dict.filter (\_ c -> isSpawning c)
                |> Dict.keys
                |> List.foldl spawnToAlive b

        spawnNewWhite b =
            indicesThatSpawnWhite board
                |> List.foldl (spawn White) b

        spawnNewBlack b =
            indicesThatSpawnBlack board
                |> List.foldl (spawn Black) b

        relevantCells _ cell =
            not (isDead cell) || (totalNeighbors cell) > 0

        removeIrrelevantCells b =
            Dict.filter relevantCells b
    in
        board
            |> clearDead
            |> changeSpawningToAlive
            |> spawnNewWhite
            |> spawnNewBlack
            |> removeIrrelevantCells


glider : Cells -> Cells
glider board =
    List.foldl
        (spawn White)
        board
        [ ( 9, 9 )
        , ( 10, 9 )
        , ( 11, 9 )
        , ( 11, 8 )
        , ( 10, 7 )
        ]


avatarL : List ( Int, Int )
avatarL =
    [ ( -3, -2 )
    , ( -3, 0 )
    , ( -3, 1 )
    , ( -2, 1 )
    ]


avatarW : List ( Int, Int )
avatarW =
    [ ( 0, -2 )
    , ( 0, 0 )
    , ( 0, 1 )
    , ( 1, 1 )
    , ( 2, -2 )
    , ( 2, 0 )
    , ( 3, 1 )
    , ( 4, -2 )
    , ( 4, 1 )
    , ( 4, 0 )
    ]


offsetCoords : ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
offsetCoords ( oX, oY ) =
    List.map
        (\( vX, vY ) -> ( vX + oX, vY + oY ))


lwOffset : ( Int, Int ) -> Cells -> Cells
lwOffset o board =
    let
        offsetCells spawnColor coords b =
            coords
                |> offsetCoords o
                |> List.foldl spawnColor b
    in
        board
            |> offsetCells (spawn White) avatarW
            |> offsetCells (spawn Black) avatarL


lw : Cells -> Cells
lw =
    lwOffset ( 0, 0 )



---- MODEL ----


type AnimationState
    = LoadingAnimation
    | Paused
    | Evolving


type alias Model =
    { board : Cells
    , loadingQueue : List ( ( Int, Int ), Color )
    , size : Size
    , state : AnimationState
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ _ =
    ( { board = Dict.empty
      , loadingQueue =
            List.concat
                [ (List.map (\idx -> ( idx, Black )) avatarL)
                , (List.map (\idx -> ( idx, White )) avatarW)
                ]
      , size = { width = flags.width, height = flags.height }
      , state = LoadingAnimation
      }
    , Cmd.none
    )



---- UPDATE ----


loadNextCell : Model -> Model
loadNextCell model =
    case model.loadingQueue of
        ( idx, color ) :: rest ->
            { model
                | loadingQueue = rest
                , board = spawn color idx model.board
                , state =
                    if rest == [] then
                        Paused
                    else
                        LoadingAnimation
            }

        [] ->
            { model
                | loadingQueue = []
                , state = Paused
            }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | BrowserResize Int Int
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.state of
                LoadingAnimation ->
                    ( loadNextCell model, Cmd.none )

                Paused ->
                    ( { model | state = Evolving }, Cmd.none )

                Evolving ->
                    let
                        newBoard =
                            evolve model.board
                    in
                        ( { model | board = newBoard }, Cmd.none )

        BrowserResize w h ->
            let
                newSize =
                    { width = w, height = h }
            in
                ( { model | size = newSize }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


blueBG : Attribute Msg
blueBG =
    style "background-color" "rgb(0, 127, 177)"


whiteBG : Attribute Msg
whiteBG =
    style "background-color" "rgb(255, 255, 255)"


blackBG : Attribute Msg
blackBG =
    style "background-color" "rgb(0, 0, 0)"


greyBG : Attribute Msg
greyBG =
    style "background-color" "rgb(128, 128, 128)"


lightGreyBG : Attribute Msg
lightGreyBG =
    style "background-color" "rgb(200, 200, 200)"


whiteFG : Attribute Msg
whiteFG =
    style "color" "rgb(255, 255, 255)"


blackFG : Attribute Msg
blackFG =
    style "color" "rgb(255, 255, 255)"


cellSize : Size -> Int
cellSize size =
    min size.height size.width // boardSize


translate : Size -> Int -> Int -> Int -> String
translate size idx offset wrapSize =
    let
        val =
            modBy wrapSize (((cellSize size) * idx) + offset)
    in
        (String.fromInt val) ++ "px"


cellTranslate : Size -> Index -> String
cellTranslate size ( x, y ) =
    let
        xOffset =
            size.width // 2

        yOffset =
            size.height // 2
    in
        (translate size x xOffset size.width)
            ++ ", "
            ++ (translate size y yOffset size.height)


cellPosition : Size -> Index -> List (Attribute Msg)
cellPosition size idx =
    [ style "top" "0"
    , style "left" "0"
    , style "position" "absolute"
    , style "transform" ("translate(" ++ (cellTranslate size idx) ++ ")")
    ]


cellSizeStyle : Size -> List (Attribute Msg)
cellSizeStyle size =
    let
        sizepx =
            (String.fromInt (cellSize size)) ++ "px"
    in
        [ style "height" sizepx
        , style "width" sizepx
        ]


cellBG : Cell -> Attribute Msg
cellBG c =
    case c.color of
        White ->
            whiteBG

        Black ->
            blackBG


cellFG : Cell -> Attribute Msg
cellFG c =
    case c.color of
        Black ->
            whiteFG

        _ ->
            blackFG


aliveClass : Cell -> Attribute Msg
aliveClass cell =
    attribute "class"
        (case cell.state of
            Dying ->
                "dying"

            Dead ->
                "dead"

            Spawning ->
                "spawning"

            Alive ->
                "alive"
        )


viewAlive : Size -> Index -> Cell -> Html Msg
viewAlive size idx c =
    div (cellPosition size idx)
        [ div ((aliveClass c) :: (cellBG c) :: (cellSizeStyle size)) [ text "" ] ]


deadDisplay : Attribute Msg
deadDisplay =
    style "display"
        (if shouldDebug then
            "block"
         else
            "none"
        )


viewDead : Size -> Index -> Cell -> Html Msg
viewDead size idx c =
    div (deadDisplay :: (List.concat [ cellPosition size idx, cellSizeStyle size ]))
        [ text (String.fromInt (totalNeighbors c)) ]


viewCell : Size -> Index -> Cell -> Html Msg
viewCell size idx c =
    case c.state of
        Dead ->
            viewDead size idx c

        _ ->
            viewAlive size idx c


cellKey : Index -> String
cellKey ( i, j ) =
    String.fromInt i ++ "," ++ String.fromInt j


viewBoard : Model -> List ( String, Html Msg )
viewBoard model =
    let
        maybeFilterDead =
            if shouldDebug then
                identity
            else
                Dict.filter
                    (\_ cell -> not (cell.state == Dead))
    in
        model.board
            |> maybeFilterDead
            |> Dict.toList
            |> List.map
                (\( idx, cell ) ->
                    ( cellKey idx, Lazy.lazy3 viewCell model.size idx cell )
                )


view : Model -> Browser.Document Msg
view model =
    { title = "Lakin Wecker's Avatar"
    , body =
        [ Keyed.node "div"
            [ style "position" "relative"
            , style "display" "block"
            , style "height" "100%"
            , style "width" "100%"
            , blueBG
            ]
            (viewBoard model)
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize BrowserResize
        , Time.every
            (case model.state of
                Evolving ->
                    1000

                Paused ->
                    1500

                _ ->
                    30
            )
            Tick
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
