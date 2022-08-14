module Components.Calendar exposing (..)

import Calendar exposing (CalendarDate, fromTime)
import Css exposing (..)
import Date exposing (Date)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput, onMouseDown, onMouseEnter, onMouseLeave)
import Task
import Time exposing (Month(..))


init : ( Model, Cmd Msg )
init =
    ( { today = Date.fromCalendarDate 2022 Jul 14
      , calendar = [ [] ]
      , selection = SingleDate Nothing
      , hovering = Nothing
      }
    , Task.perform GotTodaysDate Date.today
    )


type SelectionType d r
    = SingleDate d
    | Range r


type alias SelectionDate =
    Maybe Date


type SelectionRange
    = SelectFirst
    | SelectSecond Date
    | RangeSelected Date Date


type alias Model =
    { today : Date
    , calendar : List (List CalendarDate)
    , selection : SelectionType SelectionDate SelectionRange
    , hovering : Maybe Date
    }


currentMonth : List (List CalendarDate) -> Date
currentMonth calendar =
    case Maybe.withDefault [] (List.tail calendar) of
        d :: _ ->
            {--
            This looks very scary, but I will talk you through it, be calm... (its not that complicated)

            To determine what the current month is being displayed (so we can increment it) we need to access one
            CalendarDate held in the 2 dminesional calendar List -> https://package.elm-lang.org/packages/abradley2/elm-calendar/latest/Calendar

            calendar = [week1, week2, ...] ; week1 = [day1, day2, day3, day4, day5, day6, day7]

            The first and last weeks might contain dates from the previous or next month. To be sure we are acessing a date associated
            with the active month, we consider the first day of the second week.

            ... And thats what that case statement means.
             --}
            let
                date =
                    Maybe.withDefault (Date.fromCalendarDate 2000 Jan 1) (Maybe.map .date (List.head d))

                month =
                    Date.month date

                year =
                    Date.year date
            in
            Date.fromCalendarDate year month 1

        _ ->
            Date.fromCalendarDate 2000 Jan 1


type Msg
    = GotTodaysDate Date
    | Hover (Maybe Date)
    | Select Date
    | LastMonth
    | NextMonth


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTodaysDate today ->
            ( { model | today = today, calendar = Calendar.fromDate Nothing today }, Cmd.none )

        Hover date ->
            ( { model | hovering = date }, Cmd.none )

        Select date ->
            case model.selection of
                SingleDate d ->
                    ( { model | selection = SingleDate (Just date) }, Cmd.none )

                Range SelectFirst ->
                    ( { model | selection = Range (SelectSecond date) }, Cmd.none )

                Range (SelectSecond d1) ->
                    ( { model | selection = Range (RangeSelected (Date.min d1 date) (Date.max d1 date)) }, Cmd.none )

                Range (RangeSelected _ _) ->
                    ( { model | selection = Range SelectFirst }, Cmd.none )

        LastMonth ->
            let
                date =
                    currentMonth model.calendar

                deincrementMonth m =
                    case m of
                        Jan ->
                            Dec

                        Feb ->
                            Jan

                        Mar ->
                            Feb

                        Apr ->
                            Mar

                        May ->
                            Apr

                        Jun ->
                            May

                        Jul ->
                            Jun

                        Aug ->
                            Jul

                        Sep ->
                            Aug

                        Oct ->
                            Sep

                        Nov ->
                            Oct

                        Dec ->
                            Nov

                lastMonth =
                    case Date.month date of
                        Jan ->
                            Date.fromCalendarDate (Date.year date - 1) Dec 1

                        m ->
                            Date.fromCalendarDate (Date.year date) (deincrementMonth m) 1
            in
            ( { model | calendar = Calendar.fromDate Nothing lastMonth }, Cmd.none )

        NextMonth ->
            let
                date =
                    currentMonth model.calendar

                incrementMonth m =
                    case m of
                        Jan ->
                            Feb

                        Feb ->
                            Mar

                        Mar ->
                            Apr

                        Apr ->
                            May

                        May ->
                            Jun

                        Jun ->
                            Jul

                        Jul ->
                            Aug

                        Aug ->
                            Sep

                        Sep ->
                            Oct

                        Oct ->
                            Nov

                        Nov ->
                            Dec

                        Dec ->
                            Jan

                nextMonth =
                    case Date.month date of
                        Dec ->
                            Date.fromCalendarDate (Date.year date + 1) Jan 1

                        m ->
                            Date.fromCalendarDate (Date.year date) (incrementMonth m) 1
            in
            ( { model | calendar = Calendar.fromDate Nothing nextMonth }, Cmd.none )


view : Model -> Html Msg
view { today, calendar, selection, hovering } =
    let
        row r =
            let
                col { dayDisplay, weekdayNumber, date } =
                    {--If the day in question is not part of this month, i.e the monday at the start of the calendar is part of the previous month
                    then we use td [] [] so it holds its space but has no content.
                    --}
                    case ( dayDisplay, Date.compare date today ) of
                        ( "  ", _ ) ->
                            td [] []

                        ( _, LT ) ->
                            td [ class "text-secondary" ] [ text dayDisplay ]

                        ( _, _ ) ->
                            let
                                default =
                                    td
                                        [ onMouseEnter (Hover (Just date))
                                        , onClick (Select date)
                                        ]
                                        [ text dayDisplay ]

                                hover =
                                    td
                                        [ onMouseEnter (Hover (Just date))
                                        , onMouseLeave (Hover Nothing)
                                        , onClick (Select date)
                                        , class "bg-primary text-white"
                                        , css [ cursor pointer ]
                                        , onClick (Select date)
                                        ]
                                        [ text dayDisplay ]

                                selectedDate =
                                    td [ onClick (Select date), class "bg-warning", css [ cursor pointer ] ] [ text dayDisplay ]

                                isHovering =
                                    let
                                        test d =
                                            date == d
                                    in
                                    Maybe.withDefault False (Maybe.map test hovering)

                                isBetween d1 =
                                    let
                                        test d =
                                            Date.isBetween (Date.min d1 d) (Date.max d1 d) date
                                    in
                                    Maybe.withDefault False (Maybe.map test hovering)
                            in
                            case selection of
                                -- Single dates
                                SingleDate (Just d) ->
                                    if d == date then
                                        selectedDate

                                    else if isHovering then
                                        hover

                                    else
                                        default

                                SingleDate Nothing ->
                                    if isHovering then
                                        hover

                                    else
                                        default

                                -- Range
                                Range SelectFirst ->
                                    if isHovering then
                                        hover

                                    else
                                        default

                                Range (SelectSecond d1) ->
                                    if d1 == date then
                                        selectedDate

                                    else if isBetween d1 then
                                        hover

                                    else
                                        default

                                Range (RangeSelected d1 d2) ->
                                    if Date.isBetween d1 d2 date then
                                        selectedDate

                                    else
                                        default
            in
            tr []
                (List.map col r)
    in
    div [ class "p-3" ]
        [ div [ class "d-flex justify-content-between" ]
            [ i [ class "bi bi-arrow-left", onMouseDown LastMonth, css [ cursor pointer ] ] []
            , b [] [ text (Date.format "MMMM yy" (currentMonth calendar)) ]
            , i [ class "bi-arrow-right", onMouseDown NextMonth, css [ cursor pointer ] ] []
            ]
        , Html.Styled.table [ class "table m-0" ]
            [ thead []
                [ tr []
                    [ th [] [ text "M" ]
                    , th [] [ text "T" ]
                    , th [] [ text "W" ]
                    , th [] [ text "T" ]
                    , th [] [ text "F" ]
                    , th [] [ text "S" ]
                    , th [] [ text "S" ]
                    ]
                ]
            , tbody []
                (List.map
                    row
                    calendar
                )
            ]
        ]
