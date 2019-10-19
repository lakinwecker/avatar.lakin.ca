module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (style)
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


type alias Neighbours =
    { white : Int, black : Int }


type Cell
    = White Neighbours
    | Black Neighbours
    | Dead Neighbours


type alias CellCtor =
    Neighbours -> Cell


mapNeighbour : (Neighbours -> a) -> Cell -> a
mapNeighbour m cell =
    case cell of
        White n ->
            m n

        Black n ->
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


defaultNeighbours : Neighbours
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


addToNeighbour : ( Int, Int ) -> Neighbours -> Neighbours
addToNeighbour ( w, b ) n =
    { white = n.white + w, black = n.black + b }


modifyNeighbourCount : ( Int, Int ) -> Cell -> Cell
modifyNeighbourCount i c =
    case c of
        White n ->
            White (addToNeighbour i n)

        Black n ->
            Black (addToNeighbour i n)

        Dead n ->
            Dead (addToNeighbour i n)


modifyCellType : CellCtor -> Cell -> Cell
modifyCellType new c =
    case c of
        White n ->
            new n

        Black n ->
            new n

        Dead n ->
            new n


changeCell : CellCtor -> ( Int, Int ) -> Index -> Board -> Board
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

        modifiers =
            case c of
                White _ ->
                    ( -1, 0 )

                _ ->
                    ( 0, -1 )
    in
        changeCell Dead modifiers idx board


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
                    (n < 2 || n > 3)
            )
            (aliveCells board.cells)
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
                    (blackN + whiteN) == 3 && (colorMatch whiteN blackN)
            )
            (deadCells board.cells)
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

        Dead _ ->
            False


deadCells : Cells -> Cells
deadCells cells =
    Dict.filter
        (\idx c -> not (isAlive c))
        cells


aliveCells : Cells -> Cells
aliveCells cells =
    Dict.filter
        (\idx c -> isAlive c)
        cells


evolve : Board -> Board
evolve board =
    let
        shouldDie =
            indicesThatShouldDie board

        shouldLiveBlack =
            indicesThatSpawnBlack board

        shouldLiveWhite =
            indicesThatSpawnWhite board

        clearedDead =
            List.foldl die board shouldDie

        newWhite =
            List.foldl spawnWhite clearedDead shouldLiveWhite

        evolvedBoard =
            List.foldl spawnBlack newWhite shouldLiveBlack
    in
        { evolvedBoard
            | cells =
                (Dict.filter
                    (\idx cell -> (isAlive cell) || (totalNeighbors cell) > 0)
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


lwOffset : ( Int, Int ) -> Board -> Board
lwOffset ( ox, oy ) board =
    let
        justL =
            List.foldl
                spawnWhite
                board
                [ ( 0 + ox, 0 + oy )
                , ( 0 + ox, 1 + oy )
                , ( 1 + ox, 1 + oy )
                , ( 2 + ox, 0 + oy )
                , ( 3 + ox, 1 + oy )
                , ( 4 + ox, 1 + oy )
                , ( 4 + ox, 0 + oy )
                , ( 0 + ox, -2 + oy )
                , ( 2 + ox, -2 + oy )
                , ( 4 + ox, -2 + oy )
                ]

        justLW =
            List.foldl
                spawnBlack
                justL
                [ ( -2 + ox, 1 + oy )
                , ( -3 + ox, 1 + oy )
                , ( -3 + ox, 0 + oy )
                , ( -3 + ox, -2 + oy )
                ]
    in
        justLW


lw : Board -> Board
lw =
    lwOffset ( 0, 0 )



---- MODEL ----


type alias Model =
    { board : Board
    , session : Session
    , generations : Int
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { board = lw ({ size = 32, cells = Dict.empty })
      , session =
            Session
                key
                { width = flags.width, height = flags.height }
      , generations = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | BrowserResize Int Int
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
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


blue : Attribute Msg
blue =
    style "background-color" "rgb(0, 127, 177)"


white : Attribute Msg
white =
    style "background-color" "rgb(255, 255, 255)"


black : Attribute Msg
black =
    style "background-color" "rgb(0, 0, 0)"


grey : Attribute Msg
grey =
    style "background-color" "rgb(128, 128, 128)"


whiteFont : Attribute Msg
whiteFont =
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


cellPosition : Model -> Index -> String
cellPosition model idx =
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


cellStyle : Model -> Index -> List (Attribute Msg)
cellStyle model idx =
    let
        size =
            cellSize model
    in
        [ style "height" ((String.fromInt size) ++ "px")
        , style "width" ((String.fromInt size) ++ "px")
        , style "top" "0"
        , style "left" "0"
        , style "position" "absolute"
        , style "transform" ("translate(" ++ (cellPosition model idx) ++ ")")
        ]


cellColor : Cell -> Attribute Msg
cellColor c =
    case c of
        White _ ->
            white

        Black _ ->
            black

        Dead _ ->
            grey


viewAlive : Model -> Index -> Cell -> Html Msg
viewAlive model idx c =
    div ((cellColor c) :: (cellStyle model idx))
        [ text "" ]


deadDisplay : Attribute Msg
deadDisplay =
    case shouldDebug of
        True ->
            style "display" "block"

        False ->
            style "display" "none"


viewDead : Model -> Index -> Cell -> Html Msg
viewDead model idx c =
    div (deadDisplay :: (cellColor c) :: whiteFont :: (cellStyle model idx))
        [ text (String.fromInt (totalNeighbors c)) ]


viewBoard : Model -> List (Html Msg)
viewBoard model =
    List.map
        (\( idx, cell ) ->
            case cell of
                White _ ->
                    viewAlive model idx cell

                Black _ ->
                    viewAlive model idx cell

                Dead _ ->
                    viewDead model idx cell
        )
        (Dict.toList model.board.cells)


view : Model -> Browser.Document Msg
view model =
    { title = "Lakin Wecker's Avatar"
    , body =
        [ div
            [ style "position" "relative"
            , style "display" "block"
            , style "height" "100%"
            , style "width" "100%"
            , blue
            ]
            (viewBoard model)
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick



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
