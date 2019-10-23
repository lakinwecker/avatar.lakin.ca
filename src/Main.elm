module Main exposing (..)

import Animation
import Browser
import Browser.Navigation as Nav
import Browser.Events as Events
import Html exposing (Html, Attribute, div, text)
import Html.Keyed as Keyed
import Html.Attributes exposing (style, attribute)
import Dict
import Maybe
import List
import Tuple
import Time
import Url
import String
import Debug


---- Base Types ----


type alias Flags =
    { width : Int, height : Int }


type alias Size =
    { width : Int, height : Int }


type alias Session =
    { key : Nav.Key
    , size : Size
    }



---- Utils ----


wrap : Int -> Int -> Int
wrap max val =
    case val < 0 of
        True ->
            wrap max (val + max)

        False ->
            case val >= max of
                True ->
                    wrap max (val - max)

                False ->
                    val


shouldDebug : Bool
shouldDebug =
    False



---- Cell ----


type alias NeighbourCounts =
    { white : Int, black : Int }


type Cell
    = White NeighbourCounts
    | Black NeighbourCounts
    | DyingWhite NeighbourCounts
    | DyingBlack NeighbourCounts
    | Dead NeighbourCounts


type alias CellConstructor =
    NeighbourCounts -> Cell


mapNeighbour : (NeighbourCounts -> a) -> Cell -> a
mapNeighbour m cell =
    case cell of
        White n ->
            m n

        Black n ->
            m n

        DyingWhite n ->
            m n

        DyingBlack n ->
            m n

        Dead n ->
            m n


totalNeighbors : Cell -> Int
totalNeighbors =
    mapNeighbour (\n -> n.white + n.black)


blackNeighbours : Cell -> Int
blackNeighbours =
    mapNeighbour .black


whiteNeighbours : Cell -> Int
whiteNeighbours =
    mapNeighbour .white


defaultNeighbours : NeighbourCounts
defaultNeighbours =
    { white = 0, black = 0 }


defaultCell : Cell
defaultCell =
    Dead defaultNeighbours



---- Board ----


type alias Index =
    ( Int, Int )


type alias Cells =
    Dict.Dict Index Cell


type alias Board =
    { size : Int, cells : Cells }


x : Index -> Int
x idx =
    Tuple.first idx


y : Index -> Int
y idx =
    Tuple.second idx


getBoardCell : Index -> Board -> Cell
getBoardCell idx board =
    Maybe.withDefault
        defaultCell
        (Dict.get idx board.cells)


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


neighbours : Index -> Board -> List Index
neighbours idx board =
    List.map
        (\offset ->
            ( (x idx) + (x offset), (y idx) + (y offset) )
        )
        neighbourOffsets


updateNeighbour : ( Int, Int ) -> Index -> Board -> Board
updateNeighbour i idx board =
    let
        c =
            getBoardCell idx board
    in
        { board
            | cells =
                Dict.insert
                    idx
                    (modifyNeighbourCount i c)
                    board.cells
        }


addToNeighbour : ( Int, Int ) -> NeighbourCounts -> NeighbourCounts
addToNeighbour ( w, b ) n =
    { white = n.white + w, black = n.black + b }


modifyNeighbourCount : ( Int, Int ) -> Cell -> Cell
modifyNeighbourCount i c =
    case c of
        White n ->
            White (addToNeighbour i n)

        Black n ->
            Black (addToNeighbour i n)

        DyingWhite n ->
            DyingWhite (addToNeighbour i n)

        DyingBlack n ->
            DyingBlack (addToNeighbour i n)

        Dead n ->
            Dead (addToNeighbour i n)


modifyCellType : CellConstructor -> Cell -> Cell
modifyCellType new c =
    case c of
        White n ->
            new n

        Black n ->
            new n

        DyingWhite n ->
            new n

        DyingBlack n ->
            new n

        Dead n ->
            new n


changeCell : CellConstructor -> ( Int, Int ) -> Index -> Board -> Board
changeCell newType i idx board =
    let
        c =
            getBoardCell idx board

        updatedNeighbours =
            (List.foldl
                (updateNeighbour i)
                board
                (neighbours idx board)
            )

        newBoard =
            { updatedNeighbours
                | cells =
                    Dict.insert
                        idx
                        (modifyCellType newType c)
                        updatedNeighbours.cells
            }
    in
        newBoard


spawnWhite : Index -> Board -> Board
spawnWhite =
    changeCell White ( 1, 0 )


spawnBlack : Index -> Board -> Board
spawnBlack =
    changeCell Black ( 0, 1 )


die : Index -> Board -> Board
die idx board =
    let
        c =
            getBoardCell idx board

        ( modifiers, newState ) =
            case c of
                White _ ->
                    ( ( -1, 0 ), DyingWhite )

                Black _ ->
                    ( ( 0, -1 ), DyingBlack )

                _ ->
                    ( ( 0, 0 ), Dead )
    in
        changeCell newState modifiers idx board


toIndices : Cells -> List Index
toIndices cells =
    List.map
        Tuple.first
    <|
        Dict.toList
            cells


indicesThatShouldDie : Board -> List Index
indicesThatShouldDie board =
    toIndices
        (Dict.filter
            (\idx cell ->
                let
                    n =
                        totalNeighbors cell
                in
                    (isAlive cell && (n < 2 || n > 3)) || (isDying cell)
            )
            board.cells
        )


indicesThatSpawnColor : Board -> (Int -> Int -> Bool) -> List Index
indicesThatSpawnColor board colorMatch =
    toIndices
        (Dict.filter
            (\idx cell ->
                let
                    blackN =
                        blackNeighbours cell

                    whiteN =
                        whiteNeighbours cell
                in
                    (not (isAlive cell)) && (blackN + whiteN) == 3 && (colorMatch whiteN blackN)
            )
            board.cells
        )


indicesThatSpawnWhite : Board -> List Index
indicesThatSpawnWhite board =
    indicesThatSpawnColor board (\w b -> w < 2)


indicesThatSpawnBlack : Board -> List Index
indicesThatSpawnBlack board =
    indicesThatSpawnColor board (\w b -> b < 2)


isAlive : Cell -> Bool
isAlive c =
    case c of
        White _ ->
            True

        Black _ ->
            True

        _ ->
            False


isDying : Cell -> Bool
isDying c =
    case c of
        DyingWhite _ ->
            True

        DyingBlack _ ->
            True

        _ ->
            False


isDead : Cell -> Bool
isDead c =
    case c of
        Dead _ ->
            True

        _ ->
            False


evolve : Board -> Board
evolve board =
    let
        clearedDead =
            List.foldl die board (indicesThatShouldDie board)

        newWhite =
            List.foldl spawnWhite clearedDead (indicesThatSpawnWhite board)

        evolvedBoard =
            List.foldl spawnBlack newWhite (indicesThatSpawnBlack board)
    in
        { evolvedBoard
            | cells =
                (Dict.filter
                    (\idx cell -> (not (isDead cell)) || (totalNeighbors cell) > 0)
                    evolvedBoard.cells
                )
        }


glider : Board -> Board
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
offsetCoords o =
    let
        f =
            Tuple.first

        s =
            Tuple.second
    in
        List.map
            (\v -> ( (f v) + (f o), (s v) + (s o) ))


lwOffset : ( Int, Int ) -> Board -> Board
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


lw : Board -> Board
lw =
    lwOffset ( 0, 0 )



---- MODEL ----


type AnimationState
    = BuildingL
    | BuildingW
    | Pause
    | Going


type alias Model =
    { board : Board
    , l : List ( Int, Int )
    , w : List ( Int, Int )
    , session : Session
    , generations : Int
    , state : AnimationState
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { board = { size = 32, cells = Dict.empty }
      , l = avatarL
      , w = avatarW
      , session =
            Session
                key
                { width = flags.width, height = flags.height }
      , generations = 0
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
        Tick t ->
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
                        ( { model | board = newBoard, generations = model.generations + 1 }, Cmd.none )

        BrowserResize w h ->
            let
                oldSession =
                    model.session

                size =
                    { width = w, height = h }

                newSession =
                    { oldSession | size = size }
            in
                ( { model | session = newSession }, Cmd.none )

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


minSize : Model -> Int
minSize model =
    let
        size =
            model.session.size
    in
        min size.height size.width


cellSize : Model -> Int
cellSize model =
    (minSize model) // (model.board.size)


translate : Model -> Int -> Int -> Int -> String
translate model idx offset wrapSize =
    let
        val =
            wrap wrapSize (((cellSize model) * idx) + offset)
    in
        (String.fromInt val) ++ "px"


cellTranslate : Model -> Index -> String
cellTranslate model idx =
    let
        size =
            model.session.size

        xOffset =
            size.width // 2

        yOffset =
            size.height // 2
    in
        (translate model (Tuple.first idx) xOffset size.width)
            ++ ", "
            ++ (translate model (Tuple.second idx) yOffset size.height)


cellPosition : Model -> Cell -> Index -> List (Attribute Msg)
cellPosition model cell idx =
    [ style "top" "0"
    , style "left" "0"
    , style "position" "absolute"
    , style "transform" ("translate(" ++ (cellTranslate model idx) ++ ")")
    ]


cellSizeStyle : Model -> Cell -> Index -> List (Attribute Msg)
cellSizeStyle model cell idx =
    let
        size =
            cellSize model

        sizepx =
            (String.fromInt size) ++ "px"
    in
        [ style "height" sizepx
        , style "width" sizepx
        ]


cellBG : Cell -> Attribute Msg
cellBG c =
    case c of
        White _ ->
            whiteBG

        Black _ ->
            blackBG

        DyingWhite _ ->
            whiteBG

        DyingBlack _ ->
            blackBG

        Dead _ ->
            greyBG


cellFG : Cell -> Attribute Msg
cellFG c =
    case c of
        Black _ ->
            whiteFG

        _ ->
            blackFG


aliveClass : Cell -> Attribute Msg
aliveClass cell =
    attribute "class"
        (case cell of
            DyingBlack _ ->
                "dying"

            DyingWhite _ ->
                "dying"

            Dead _ ->
                "dead"

            _ ->
                "alive"
        )


viewAlive : Model -> Index -> Cell -> Html Msg
viewAlive model idx c =
    div (cellPosition model c idx)
        [ div ((aliveClass c) :: (cellBG c) :: (cellSizeStyle model c idx)) [ text "" ] ]


deadDisplay : Attribute Msg
deadDisplay =
    case shouldDebug of
        True ->
            style "display" "block"

        False ->
            style "display" "none"


viewDead : Model -> Index -> Cell -> Html Msg
viewDead model idx c =
    div (deadDisplay :: (List.concat [ cellPosition model c idx, cellSizeStyle model c idx ]))
        [ text (String.fromInt (totalNeighbors c)) ]


cellKey : Index -> String.String
cellKey ( i, j ) =
    String.fromInt i ++ String.fromInt j


viewBoard : Model -> List ( String.String, Html Msg )
viewBoard model =
    List.map
        (\( idx, cell ) ->
            case cell of
                Dead _ ->
                    ( cellKey idx, viewDead model idx cell )

                _ ->
                    ( cellKey idx, viewAlive model idx cell )
        )
        (Dict.toList model.board.cells)


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
