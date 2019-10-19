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


---- Utils ----


wrap : Int -> Int -> Int
wrap max val =
    case val < 0 of
        True ->
            val + max

        False ->
            case val >= max of
                True ->
                    val - max

                False ->
                    val



---- Base Types ----


type alias Flags =
    { width : Int, height : Int }


type alias Size =
    { width : Int, height : Int }


type alias Session =
    { key : Nav.Key
    , size : Size
    }



---- Cell ----


type alias Neighbours =
    { white : Int, black : Int }


type Cell
    = White Neighbours
    | Black Neighbours
    | Dead Neighbours


type alias CellCtor =
    Neighbours -> Cell


neighbourCount : (Neighbours -> Int) -> Cell -> Int
neighbourCount getCount cell =
    case cell of
        White n ->
            getCount n

        Black n ->
            getCount n

        Dead n ->
            getCount n


totalNeighbors : Cell -> Int
totalNeighbors =
    neighbourCount (\n -> n.white + n.black)


blackNeighbours : Cell -> Int
blackNeighbours =
    neighbourCount .black


whiteNeighbours : Cell -> Int
whiteNeighbours =
    neighbourCount .white


type alias Index =
    ( Int, Int )


type alias Cells =
    Dict.Dict Index Cell


type alias Board =
    { size : Int, cells : Cells }


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


x : Index -> Int
x idx =
    Tuple.first idx


y : Index -> Int
y idx =
    Tuple.second idx


shouldDebug : Bool
shouldDebug =
    False


defaultNeighbours : Neighbours
defaultNeighbours =
    { white = 0, black = 0 }


defaultCell : Cell
defaultCell =
    Dead defaultNeighbours


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
            Maybe.withDefault
                defaultCell
                (Dict.get idx board.cells)
    in
        { board
            | cells =
                Dict.insert
                    idx
                    (modifyNeighbourCount i c)
                    board.cells
        }


modifyNeighbourCount : ( Int, Int ) -> Cell -> Cell
modifyNeighbourCount ( w, b ) c =
    case c of
        White n ->
            White { white = n.white + w, black = n.black + b }

        Black n ->
            Black { white = n.white + w, black = n.black + b }

        Dead n ->
            Dead { white = n.white + w, black = n.black + b }


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
            Maybe.withDefault
                defaultCell
                (Dict.get idx board.cells)

        updatedNeighbours =
            (List.foldl
                (updateNeighbour i)
                board
                (neighbours idx board)
            )
    in
        { updatedNeighbours
            | cells =
                Dict.insert
                    idx
                    (modifyCellType newType c)
                    updatedNeighbours.cells
        }


liveWhite : Index -> Board -> Board
liveWhite =
    changeCell White ( 1, 0 )


liveBlack : Index -> Board -> Board
liveBlack =
    changeCell Black ( 0, 1 )


die : Index -> Board -> Board
die idx board =
    let
        c =
            Maybe.withDefault
                defaultCell
                (Dict.get idx board.cells)

        modifiers =
            case c of
                White _ ->
                    ( -1, 0 )

                _ ->
                    ( 0, -1 )
    in
        changeCell Dead modifiers idx board


indicesThatShouldDie : Board -> List Index
indicesThatShouldDie board =
    List.map
        Tuple.first
        (Dict.toList
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
        )


indicesThatShouldBeColor : Board -> (Int -> Int -> Bool) -> List Index
indicesThatShouldBeColor board colorMatch =
    List.map
        Tuple.first
        (Dict.toList
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
        )


indicesThatShouldLiveWhite : Board -> List Index
indicesThatShouldLiveWhite board =
    indicesThatShouldBeColor board (\w b -> w < 2)


indicesThatShouldLiveBlack : Board -> List Index
indicesThatShouldLiveBlack board =
    indicesThatShouldBeColor board (\w b -> b < 2)


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
            indicesThatShouldLiveBlack board

        shouldLiveWhite =
            indicesThatShouldLiveWhite board

        clearedDead =
            List.foldl die board shouldDie

        newWhite =
            List.foldl liveWhite clearedDead shouldLiveWhite

        evolvedBoard =
            List.foldl liveBlack newWhite shouldLiveBlack
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
        liveWhite
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
                liveWhite
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
                liveBlack
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


translate : Model -> Int -> Int -> String
translate model idx offset =
    (String.fromInt (((cellSize model) * idx) + offset)) ++ "px"


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
        (translate model (Tuple.first idx) xOffset) ++ ", " ++ (translate model (Tuple.second idx) yOffset)


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
    Time.every 1000 Tick



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
