-- Copyright 2019 Lakin Wecker
-- Licensed under the GPL v3.0
-- See LICENSE for more information.


module Main exposing (main)

import Basics exposing (modBy)
import Browser
import Browser.Navigation as Nav
import Browser.Events as Events
import Html exposing (Html, Attribute, div, text)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Html.Attributes exposing (style, attribute)
import Dict
import List
import Tuple
import Time
import Url
import String


---- Base Types ----


type alias Size =
    { width : Int
    , height : Int
    }


type alias Flags =
    Size



---- Utils ----


wrap : Int -> Int -> Int
wrap max val =
    modBy max val


shouldDebug : Bool
shouldDebug =
    False


boardSize : Int
boardSize =
    32



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


mapNeighbour : (NeighbourCounts -> a) -> Cell -> a
mapNeighbour m cell =
    m cell.neighbours


totalNeighbors : Cell -> Int
totalNeighbors =
    mapNeighbour (\n -> n.white + n.black)


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


neighbours : Index -> List Index
neighbours ( iX, iY ) =
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
                (neighbours idx)

        newBoard =
            Dict.insert
                idx
                (modifyCellType color state (getBoardCell idx board))
                updatedNeighbours
    in
        newBoard


spawnWhite : Index -> Cells -> Cells
spawnWhite =
    changeCell White Spawning ( 1, 0 )


spawnBlack : Index -> Cells -> Cells
spawnBlack =
    changeCell Black Spawning ( 0, 1 )


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
    toIndices
        (Dict.filter
            (\_ cell ->
                let
                    n =
                        totalNeighbors cell
                in
                    (isAlive cell && (n < 2 || n > 3)) || (isDying cell)
            )
            board
        )


indicesThatSpawnColor : Cells -> (Int -> Int -> Bool) -> List Index
indicesThatSpawnColor board colorMatch =
    toIndices
        (Dict.filter
            (\_ cell ->
                let
                    blackN =
                        mapNeighbour .black cell

                    whiteN =
                        mapNeighbour .white cell
                in
                    (not (isAlive cell)) && (blackN + whiteN) == 3 && (colorMatch whiteN blackN)
            )
            board
        )


indicesThatSpawnWhite : Cells -> List Index
indicesThatSpawnWhite board =
    indicesThatSpawnColor board (\w _ -> w < 2)


indicesThatSpawnBlack : Cells -> List Index
indicesThatSpawnBlack board =
    indicesThatSpawnColor board (\_ b -> b < 2)


isSpawning : Cell -> Bool
isSpawning c =
    case c.state of
        Spawning ->
            True

        _ ->
            False


isAlive : Cell -> Bool
isAlive c =
    case c.state of
        Alive ->
            True

        Spawning ->
            True

        _ ->
            False


isDying : Cell -> Bool
isDying c =
    case c.state of
        Dying ->
            True

        _ ->
            False


isDead : Cell -> Bool
isDead c =
    case c.state of
        Dead ->
            True

        _ ->
            False


evolve : Cells -> Cells
evolve board =
    let
        clearedDead =
            List.foldl die board (indicesThatShouldDie board)

        onlyAlive =
            List.foldl
                spawnToAlive
                clearedDead
                (List.map
                    (\( idx, _ ) -> idx)
                    (Dict.toList
                        (Dict.filter (\_ c -> isSpawning c) clearedDead)
                    )
                )

        newWhite =
            List.foldl spawnWhite onlyAlive (indicesThatSpawnWhite board)

        evolvedBoard =
            List.foldl spawnBlack newWhite (indicesThatSpawnBlack board)
    in
        (Dict.filter
            (\_ cell -> (not (isDead cell)) || (totalNeighbors cell) > 0)
            evolvedBoard
        )


glider : Cells -> Cells
glider board =
    List.foldl
        spawnWhite
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
        justL =
            List.foldl
                spawnWhite
                board
                (offsetCoords o avatarW)

        justLW =
            List.foldl
                spawnBlack
                justL
                (offsetCoords o avatarL)
    in
        justLW


lw : Cells -> Cells
lw =
    lwOffset ( 0, 0 )



---- MODEL ----


type AnimationState
    = BuildingL
    | BuildingW
    | Pause
    | Going


type alias Model =
    { board : Cells
    , l : List ( Int, Int )
    , w : List ( Int, Int )
    , size : Size
    , state : AnimationState
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ _ =
    ( { board = Dict.empty
      , l = avatarL
      , w = avatarW
      , size = { width = flags.width, height = flags.height }
      , state = BuildingL
      }
    , Cmd.none
    )



---- UPDATE ----


updateL : Model -> Model
updateL model =
    case model.l of
        first :: [] ->
            { model | l = [], board = (spawnBlack first model.board), state = BuildingW }

        first :: rest ->
            { model | l = rest, board = (spawnBlack first model.board) }

        [] ->
            { model | l = [], state = BuildingW }


updateW : Model -> Model
updateW model =
    case model.w of
        first :: [] ->
            { model | w = [], board = (spawnWhite first model.board), state = Pause }

        first :: rest ->
            { model | w = rest, board = (spawnWhite first model.board) }

        [] ->
            { model | w = [], state = Pause }


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
                BuildingL ->
                    ( updateL model, Cmd.none )

                BuildingW ->
                    ( updateW model, Cmd.none )

                Pause ->
                    ( { model | state = Going }, Cmd.none )

                Going ->
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
            wrap wrapSize (((cellSize size) * idx) + offset)
    in
        (String.fromInt val) ++ "px"


cellTranslate : Size -> Index -> String
cellTranslate size idx =
    let
        xOffset =
            size.width // 2

        yOffset =
            size.height // 2
    in
        (translate size (Tuple.first idx) xOffset size.width)
            ++ ", "
            ++ (translate size (Tuple.second idx) yOffset size.height)


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

            _ ->
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


cellKey : Index -> String
cellKey ( i, j ) =
    String.fromInt i ++ "," ++ String.fromInt j


viewBoard : Model -> List ( String, Html Msg )
viewBoard model =
    List.map
        (\( idx, cell ) ->
            let
                v =
                    case cell.state of
                        Dead ->
                            viewDead

                        _ ->
                            viewAlive

                size =
                    model.size
            in
                ( cellKey idx, Lazy.lazy3 v size idx cell )
        )
        (Dict.toList
            (if shouldDebug then
                model.board
             else
                Dict.filter
                    (\_ cell -> not (cell.state == Dead))
                    model.board
            )
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
        , case model.state of
            Going ->
                Time.every 1000 Tick

            Pause ->
                Time.every 1500 Tick

            _ ->
                Time.every 30 Tick
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
