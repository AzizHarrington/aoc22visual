module Day14 exposing (..)


import Array exposing (..)
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time


-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { instructions : Instructions
    , grid : Grid
    , fallingSand : Point
    , numSand : Int
    , reachedAbyss : Bool
    , showGrid : Bool
    , fileRequested : Bool
    }


type alias Instructions =
    List Instruction


type alias Instruction =
    List Point


type alias Point =
    (Int, Int)


type alias Grid =
    Array (Array Cell)


type Cell
    = Air
    | Rock
    | Sand SandStatus


type SandStatus
    = Falling
    | Settled


type Direction
    = Down
    | Right
    | Left


init : () -> (Model, Cmd Msg)
init _ =
    (
        { instructions = [[]]
        , grid = initGrid
        , fallingSand = (100, 0)
        , numSand = 1
        , reachedAbyss = False
        , showGrid = False
        , fileRequested = False
        }
        , Cmd.none
    )


initGrid : Grid
initGrid =
    Array.initialize 200 ( \_ ->
        Array.initialize 200 (\_ -> Air ))


-- UPDATE


type Msg
    = Start
    | FileSelected File
    | FileLoaded String
    | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start ->
            ({ model | fileRequested = True } , selectFile )
        FileSelected file ->
            ( model, read file)
        FileLoaded fileContent ->
            let
                instructions =
                    parseFile fileContent

                populatedGrid =
                    populateGrid initGrid instructions
            in

                (
                    { model
                    | instructions = instructions
                    , grid = populatedGrid
                    , showGrid = True
                    }
                    , Cmd.none
                )
        Tick _ ->
            if model.showGrid then
                let
                    (
                        updatedGrid
                        , fallingSand
                        , reachedAbyss
                        )
                        = updateGrid model.grid model.fallingSand

                in
                    (
                        { model
                        | grid = updatedGrid
                        , fallingSand = fallingSand
                        , reachedAbyss = reachedAbyss
                        }
                        , Cmd.none
                    )
            else
                ( model, Cmd.none )


parseFile : String -> Instructions
parseFile fileContent =
    let
        instructions =
            fileContent
            |> String.split "\n"
            |> List.map ( \line ->
                (String.split " -> " line)
                |> List.map ( \instruction ->
                    (String.split "," instruction)
                    |> List.filterMap (String.toInt))
                |> List.filterMap listToPoint)
    in
        instructions |> List.sort


listToPoint : List Int -> Maybe Point
listToPoint pointList =
    case pointList of
        (x :: y :: _) -> Just (x - 400, y)
        _ -> Nothing


populateGrid : Grid -> Instructions -> Grid
populateGrid grid instructions =
    case instructions of
        (instruction::rest) ->
            populateGrid (processInstruction grid instruction) rest
        _ -> grid


processInstruction : Grid -> Instruction -> Grid
processInstruction grid instruction =
    case instruction of
        (a::b::rest) ->
            processInstruction (addToGrid grid a b) (b::rest)
        _ -> grid


addToGrid : Grid -> Point -> Point -> Grid
addToGrid grid (ax, ay) (bx, by) =
    if ax == bx then
        case (List.sort [ay, by]) of
            [low, high] ->
                List.foldl
                    (\y g ->
                        doAdd g (ax, y) Rock)
                    grid
                    (List.range low high)
            _ -> grid
    else if (ay == by) then
        case (List.sort [ax, bx]) of
            [low, high] ->
                List.foldl
                    (\x g ->
                        doAdd g (x, ay) Rock)
                    grid
                    (List.range low high)
            _ -> grid
    else
        grid


doAdd : Grid -> Point -> Cell -> Grid
doAdd grid (x, y) cell =
    let
        updatedRow =
            Array.get y grid
            |> Maybe.map (\r -> Array.set x cell r)

        updatedGrid =
            updatedRow
            |> Maybe.map (\r -> Array.set y r grid)
    in
        case updatedGrid of
            Nothing ->
                grid
            Just g ->
                g


updateGrid : Grid -> Point -> (Grid, Point, Bool)
updateGrid grid (x, y) =
    let
        lower = x - 1

        upper = x + 2

        maybeRow =
            Array.get (y + 1) grid


    in
        case maybeRow of
            Nothing ->
                (grid, (x, y), True)
            Just row ->
                let
                    possibleMoves =
                        Array.slice lower upper row
                in

                    Debug.log ("upgrading grid")
                    Debug.log (Debug.toString possibleMoves)
                    moveSand grid (x, y) (Array.toList possibleMoves)


moveSand : Grid -> Point -> List Cell -> (Grid, Point, Bool)
moveSand grid (x, y) possibleMoves =
    case possibleMoves of
        [_, Air, _] ->
            let
                (g, pos) = (move grid (x, y) Down)
            in
                Debug.log (Debug.toString possibleMoves)
                Debug.log ("move down")
                (g, pos, False)
        [Air, _, _] ->
            let
                (g, pos) = (move grid (x, y) Left)
            in
                Debug.log (Debug.toString possibleMoves)
                Debug.log ("move left")
                (g, pos, False)
        [_, _, Air] ->
            let
                (g, pos) = (move grid (x, y) Right)
            in
                Debug.log (Debug.toString possibleMoves)
                Debug.log ("move right")
                (g, pos, False)
        [_, _, _] ->
            Debug.log (Debug.toString possibleMoves)
            Debug.log ("settle")
            (
                (doAdd grid (x, y) (Sand Settled))
                , (100, 0)
                , False
                )
        _ ->
            Debug.log ("no move")
            (grid, (x, y), True)


move : Grid -> Point -> Direction -> (Grid, Point)
move grid (x, y) direction =
    let
        newPos =
            case direction of
                Down ->
                    (x, y + 1)
                Left ->
                    (x - 1, y + 1)
                Right ->
                    (x + 1, y + 1)

        updatedGrid =
            List.foldl
                (\(point, cell) g ->
                    doAdd g point cell)
                grid
                [ ((x, y), Air)
                , (newPos, Sand Falling)
                ]

    in
        (updatedGrid, newPos)


selectFile : Cmd Msg
selectFile =
    Select.file ["text/plain"] FileSelected


read : File -> Cmd Msg
read file =
    Task.perform FileLoaded (File.toString file)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 0.0001 Tick


-- VIEW


view : Model -> Html Msg
view model =
    let
        renderCell cell =
            case cell of
                Air -> "."
                Rock -> "#"
                Sand Falling -> "+"
                Sand Settled -> "o"

        renderRow row =
            Array.map (\cell ->
                renderCell cell
            ) row
            |> Array.toList

        renderedGrid =
            Array.map (\row ->
                renderRow row
                |> String.join ""

            ) model.grid
            |> Array.toList
            |> String.join "\n"
    in
        div []
            [
                controls
                -- , div [] [ text (Debug.toString model.instructions) ]
                , pre []
                    [
                        if model.showGrid then
                            text renderedGrid
                        else
                            text "Click start to choose an instruction file and begin simulation."
                    ]
            ]


controls : Html Msg
controls =
    div [ ]
        [ button [ onClick Start ] [ text "Start" ]
        ]