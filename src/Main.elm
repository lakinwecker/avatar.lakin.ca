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


---- Logic ----


type alias Flags =
    { width : Int, height : Int }


type alias Size =
    { width : Int, height : Int }


type alias Session =
    { key : Nav.Key
    , size : Size
    }


type CellState
    = Alive
    | Dead


type alias Cell =
    { aliveNeighbours : Int, state : CellState }


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


x : Index -> Int
x idx =
    Tuple.first idx


y : Index -> Int
y idx =
    Tuple.second idx


shouldDebug : Bool
shouldDebug =
    False


defaultCell : Cell
defaultCell =
    { aliveNeighbours = 0, state = Dead }


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


neighbours : Index -> Board -> List Index
neighbours idx board =
    List.map
        (\offset ->
            ( (x idx) + (x offset), (y idx) + (y offset) )
        )
        neighbourOffsets


modifyNeighbourCount : Int -> Index -> Board -> Board
modifyNeighbourCount i idx board =
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
                    ({ c | aliveNeighbours = (c.aliveNeighbours + i) })
                    board.cells
        }


changeCell : CellState -> Int -> Index -> Board -> Board
changeCell state i idx board =
    let
        c =
            Maybe.withDefault
                defaultCell
                (Dict.get idx board.cells)

        newBoard =
            (List.foldl
                (modifyNeighbourCount i)
                board
                (neighbours idx board)
            )
    in
        { newBoard
            | cells =
                Dict.insert
                    idx
                    { c | state = state }
                    newBoard.cells
        }


live : Index -> Board -> Board
live =
    changeCell Alive 1


die : Index -> Board -> Board
die =
    changeCell Dead -1


shouldDie : Board -> List Index
shouldDie board =
    Dict.keys
        (Dict.filter
            (\idx cell ->
                cell.state == Alive && (cell.aliveNeighbours < 2 || cell.aliveNeighbours > 3)
            )
            board.cells
        )


shouldLive : Board -> List Index
shouldLive board =
    Dict.keys
        (Dict.filter
            (\idx cell ->
                cell.state == Dead && cell.aliveNeighbours == 3
            )
            board.cells
        )


evolve : Board -> Board
evolve board =
    let
        newBoard =
            List.foldl
                live
                (List.foldl
                    die
                    board
                    (shouldDie board)
                )
                (shouldLive board)
    in
        { newBoard
            | cells =
                (Dict.filter
                    (\idx cell -> cell.state == Alive || cell.aliveNeighbours > 0)
                    newBoard.cells
                )
        }


glider : Board -> Board
glider board =
    List.foldl
        live
        board
        [ ( 9, 9 )
        , ( 10, 9 )
        , ( 11, 9 )
        , ( 11, 8 )
        , ( 10, 7 )
        ]


lw : Board -> Board
lw board =
    List.foldl
        live
        board
        (List.map
            (\( lwX, lwY ) -> ( lwX, lwY ))
            [ ( 0, 0 )
            , ( 0, 1 )
            , ( 1, 1 )
            , ( 2, 0 )
            , ( 3, 1 )
            , ( 4, 1 )
            , ( 4, 0 )
            , ( 0, -2 )
            , ( 2, -2 )
            , ( 4, -2 )
            , ( -2, 1 )
            , ( -3, 1 )
            , ( -3, 0 )
            , ( -3, -2 )
            ]
        )



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
        , white
        ]


viewCell : Model -> Index -> Cell -> Html Msg
viewCell model idx c =
    case c.state of
        Alive ->
            viewAlive model idx c

        Dead ->
            viewDead model idx c


viewAlive : Model -> Index -> Cell -> Html Msg
viewAlive model idx c =
    div (cellStyle model idx)
        [ text "" ]


viewDead : Model -> Index -> Cell -> Html Msg
viewDead model idx c =
    div (cellStyle model idx)
        [ text
            (case c.aliveNeighbours > 0 of
                True ->
                    case shouldDebug of
                        True ->
                            String.fromInt c.aliveNeighbours

                        False ->
                            " "

                False ->
                    " "
            )
        ]


viewBoard : Model -> List (Html Msg)
viewBoard model =
    List.map
        (\( idx, cell ) ->
            viewCell model idx cell
        )
        (Dict.toList
            (Dict.filter
                (\idx cell ->
                    cell.state == Alive
                )
                model.board.cells
            )
        )


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
