port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Set exposing (Set)
import String


port setInputValue : String -> Cmd msg


port onTab : (() -> msg) -> Sub msg


port onUndo : (() -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onUndo (\_ -> Undo)
                    , onTab (\_ -> TabComplete)
                    ]
        }



-- MODEL


type ViewMode
    = CardView
    | TableView


type alias AppState =
    { query : String
    , activatedVideos : Set String
    , viewMode : ViewMode
    }


type alias Model =
    { current : AppState
    , history : List AppState
    , typing : Bool
    }


initState : AppState
initState =
    { query = ""
    , activatedVideos = Set.empty
    , viewMode = CardView
    }


init : Model
init =
    { current = initState
    , history = []
    , typing = False
    }


pushHistory : Model -> Model
pushHistory model =
    { model | history = model.current :: model.history }


type Difficulty
    = Easy
    | Medium
    | Hard


type Content
    = YouTube String
    | Vimeo String
    | Bandcamp { src : String, linkUrl : String, linkText : String }
    | ImageContent String
    | BackgroundImage { url : String, height : String, bgSize : String, bgPosition : String }
    | LinkOnly { url : String, label : String }


type alias Item =
    { content : Content
    , title : String
    , difficulty : Maybe Difficulty
    , description : List (Html Msg)
    , tags : List String
    , isNima : Bool
    }


type alias Section =
    { name : String
    , items : List Item
    }



-- AUTOCOMPLETE


allTags : List String
allTags =
    List.concatMap
        (\section -> List.concatMap .tags section.items)
        allSections
        |> Set.fromList
        |> Set.toList


allCompletions : List String
allCompletions =
    [ "difficulty:easy"
    , "difficulty:medium"
    , "difficulty:hard"
    , "section:cooking"
    , "section:lifestyle"
    , "section:billiards"
    , "section:synth"
    , "section:orchestral"
    , "section:behind"
    , "section:live"
    , "section:computers"
    , "type:youtube"
    , "type:vimeo"
    , "type:bandcamp"
    , "type:image"
    , "type:link"
    , "is:nima"
    ]
        ++ List.map (\t -> "tag:" ++ t) allTags


lastToken : String -> String
lastToken query =
    case List.reverse (String.words query) of
        tok :: _ ->
            tok

        [] ->
            ""


replaceLastToken : String -> String -> String
replaceLastToken query completion =
    let
        tokens =
            String.words query

        prefix =
            List.take (List.length tokens - 1) tokens
    in
    String.join " " (prefix ++ [ completion ])


commonPrefix : List String -> String
commonPrefix strings =
    case strings of
        [] ->
            ""

        first :: rest ->
            List.foldl commonPrefixOf first rest


commonPrefixOf : String -> String -> String
commonPrefixOf a b =
    let
        go i =
            if i >= String.length a || i >= String.length b then
                String.left i a

            else if String.slice i (i + 1) a == String.slice i (i + 1) b then
                go (i + 1)

            else
                String.left i a
    in
    go 0


getCompletions : String -> List String
getCompletions query =
    let
        tok =
            String.toLower (lastToken query)
    in
    if String.isEmpty tok then
        []

    else
        List.filter (\c -> String.startsWith tok c && c /= tok) allCompletions



-- UPDATE


type Msg
    = UpdateQuery String
    | ActivateVideo String
    | ToggleViewMode
    | CompleteToken String
    | Undo
    | TabComplete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        state =
            model.current
    in
    case msg of
        UpdateQuery q ->
            let
                base =
                    if model.typing then
                        model

                    else
                        pushHistory model
            in
            ( { base | current = { state | query = q }, typing = True }, Cmd.none )

        ActivateVideo videoId ->
            let
                pushed =
                    pushHistory model
            in
            ( { pushed | current = { state | activatedVideos = Set.insert videoId state.activatedVideos }, typing = False }, Cmd.none )

        ToggleViewMode ->
            let
                pushed =
                    pushHistory model
            in
            ( { pushed
                | current =
                    { state
                        | viewMode =
                            case state.viewMode of
                                CardView ->
                                    TableView

                                TableView ->
                                    CardView
                    }
                , typing = False
              }
            , Cmd.none
            )

        CompleteToken completion ->
            let
                pushed =
                    pushHistory model

                newQuery =
                    replaceLastToken state.query completion ++ " "
            in
            ( { pushed | current = { state | query = newQuery }, typing = False }, setInputValue newQuery )

        Undo ->
            case model.history of
                prev :: rest ->
                    ( { model | current = prev, history = rest, typing = False }
                    , setInputValue prev.query
                    )

                [] ->
                    ( model, Cmd.none )

        TabComplete ->
            let
                completions =
                    getCompletions state.query
            in
            case completions of
                [] ->
                    ( model, Cmd.none )

                [ single ] ->
                    let
                        pushed =
                            pushHistory model

                        newQuery =
                            replaceLastToken state.query single ++ " "
                    in
                    ( { pushed | current = { state | query = newQuery }, typing = False }, setInputValue newQuery )

                _ ->
                    let
                        prefix =
                            commonPrefix completions

                        currentToken =
                            lastToken state.query
                    in
                    if String.length prefix > String.length currentToken then
                        let
                            pushed =
                                pushHistory model

                            newQuery =
                                replaceLastToken state.query prefix
                        in
                        ( { pushed | current = { state | query = newQuery }, typing = False }, setInputValue newQuery )

                    else
                        ( model, Cmd.none )



-- DATA


allSections : List Section
allSections =
    [ { name = "Cooking: Persian cuisine"
      , items =
            [ { content = YouTube "JQkAILo9gtM"
              , title = "Fesenjoon"
              , difficulty = Just Hard
              , description = []
              , tags = [ "cooking", "food", "persian", "fesenjoon" ]
              , isNima = False
              }
            , { content = YouTube "4riphWzBpuA"
              , title = "Macaroni (Makaroni) Persian Style Spaghetti Recipe"
              , difficulty = Just Easy
              , description = []
              , tags = [ "cooking", "food", "persian", "macaroni", "pasta" ]
              , isNima = False
              }
            , { content = YouTube "K7xihvdDBxE"
              , title = "Double-onionized potatoe"
              , difficulty = Just Easy
              , description = []
              , tags = [ "cooking", "food", "persian", "potato" ]
              , isNima = False
              }
            ]
      }
    , { name = "Cooking: Pasta"
      , items =
            [ { content = YouTube "NrJrYhwSBpc"
              , title = "Chicken Tequila Fettuccini"
              , difficulty = Just Medium
              , description = []
              , tags = [ "cooking", "food", "pasta", "chicken" ]
              , isNima = False
              }
            ]
      }
    , { name = "Cooking: Soup"
      , items =
            [ { content = YouTube "Q7bTh8h9fGU"
              , title = "Lentil soup"
              , difficulty = Just Easy
              , description = []
              , tags = [ "cooking", "food", "soup", "lentil" ]
              , isNima = False
              }
            ]
      }
    , { name = "DX7, Dexed and FM Synthesis"
      , items =
            [ { content = YouTube "_tcVH2KJYMo"
              , title = "Visualizing \"Take Off\" preset on dexed (Yamaha DX7 emulator)"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "synth", "dexed", "dx7", "fm synthesis", "sound" ]
              , isNima = True
              }
            , { content = YouTube "5rZ2H2YGAgI"
              , title = "Visualizing \"dentistry!\" preset on dexed (Yamaha DX7 emulator)"
              , difficulty = Just Easy
              , description =
                    [ text "Demo of me playing with \"Dentistry!\" preset and visualizing its funky spectrogram" ]
              , tags = [ "music", "synth", "dexed", "dx7", "fm synthesis", "spectrogram", "sound" ]
              , isNima = True
              }
            ]
      }
    , { name = "Pure Data"
      , items =
            [ { content = YouTube "1m9DWCjIgU4"
              , title = "Arpeggiator in Pure Data"
              , difficulty = Just Medium
              , description =
                    [ text "I made a demo of a very basic arpeggiator in Pure Data playing "
                    , a [ href "https://www.youtube.com/watch?v=48xlgXqQKLA" ] [ text "Divers" ]
                    , text " by Joanna Newsom. I'd like to expand on this video and experiment with different configuration of oscillators and envelopes for amplitude and pitch."
                    ]
              , tags = [ "music", "synth", "pure data", "arpeggiator", "sound" ]
              , isNima = True
              }
            ]
      }
    , { name = "Lifestyle"
      , items =
            [ { content = YouTube "yh_697necjI"
              , title = "Truth Be Told: Irving Norman and the Human Predicament"
              , difficulty = Nothing
              , description =
                    [ text "This is a documentary about a \"social surrealist\" painter called Irving Norman. One of Norman's paintings ("
                    , a [ href "https://youtu.be/yh_697necjI?si=zsDVXsXUKoEfZtz5&t=2230" ] [ text "My World and Yours (And the Gods Created the World in Their Own Image), 1954" ]
                    , text ") is in Crocker Museum in Sacramento. It is a very tall, and is very detailed. Irving Norman's use of painting as a language is semantically very rich."
                    ]
              , tags = [ "lifestyle", "art", "painting", "documentary", "irving norman", "sacramento" ]
              , isNima = False
              }
            , { content = YouTube "s-CTkbHnpNQ"
              , title = "10 Bullets, #8: \"ALWAYS BE KNOLLING\". By Tom Sachs"
              , difficulty = Just Easy
              , description =
                    [ text "Knoll /nōl/ vb. (1989 USA) to arrange like objects in parallel or 90 degree angles as a method of organization." ]
              , tags = [ "lifestyle", "organization", "tom sachs", "knolling" ]
              , isNima = False
              }
            , { content = YouTube "eoOUBETTyMI"
              , title = "Discipline of Do Easy by Gus Van Sant"
              , difficulty = Just Easy
              , description = []
              , tags = [ "lifestyle", "gus van sant" ]
              , isNima = False
              }
            ]
      }
    , { name = "Hybrid of Dynamicland and Billiards"
      , items =
            [ { content = YouTube "dqNeZ-6ywQ8"
              , title = "OpenPoolCC (2014)"
              , difficulty = Just Hard
              , description = []
              , tags = [ "billiards", "pool", "dynamicland", "projection", "interactive" ]
              , isNima = False
              }
            , { content = ImageContent "http://www.openpool.cc/wp-content/uploads/howitworks-new-700x602.png"
              , title = "Pool table and a projector"
              , difficulty = Just Medium
              , description = []
              , tags = [ "billiards", "pool", "projector" ]
              , isNima = False
              }
            , { content = ImageContent "https://i0.wp.com/projectionprobilliards.com/wp-content/uploads/2020/03/projector-top.png?resize=1024%2C576&ssl=1"
              , title = "Pool table and a projector"
              , difficulty = Just Medium
              , description = []
              , tags = [ "billiards", "pool", "projector" ]
              , isNima = False
              }
            , { content = ImageContent "https://projectionprobilliards.com/wp-content/uploads/2020/03/redtable-with-grid.jpg"
              , title = "Projection Pro Billiards"
              , difficulty = Just Medium
              , description = []
              , tags = [ "billiards", "pool", "projection" ]
              , isNima = False
              }
            , { content = Vimeo "https://player.vimeo.com/video/209591449?h=583a1f54a6&title=0&byline=0&portrait=0#t=7s"
              , title = "La Tabla"
              , difficulty = Just Medium
              , description = []
              , tags = [ "billiards", "pool", "la tabla", "interactive" ]
              , isNima = False
              }
            ]
      }
    , { name = "Synth sound demos and other instruments"
      , items =
            [ { content = YouTube "2hk-kOPdHAY"
              , title = "Pocket Miku: A unique vocal synthesizer from Japan"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "synth", "synthesizer", "miku", "japan", "instrument" ]
              , isNima = False
              }
            , { content = YouTube "kHVTuAc1mI4"
              , title = "Music Synthesizer for Android (demo by Raph Levien)"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "synth", "synthesizer", "android", "raph levien", "instrument" ]
              , isNima = False
              }
            , { content = YouTube "9eWouzXO7_M?si=hOuz0KZ1kG4Zj7EH&amp;start=74"
              , title = "Arp Odyssey 2800 playing a segment of Dr. Who's theme"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "synth", "synthesizer", "arp odyssey", "dr who", "instrument" ]
              , isNima = False
              }
            , { content = YouTube "-XHJekCcxUw?si=umhYR19KZmeBHKb8&amp;start=40"
              , title = "Arp Odyssey playing a segment of Dr. Who's theme"
              , difficulty = Just Medium
              , description = []
              , tags = [ "music", "synth", "synthesizer", "arp odyssey", "dr who", "instrument" ]
              , isNima = False
              }
            , { content = YouTube "lCwTj5ZrKIE"
              , title = "Dr. Who's theme played on vintage synths"
              , difficulty = Just Hard
              , description = []
              , tags = [ "music", "synth", "synthesizer", "dr who", "vintage", "instrument" ]
              , isNima = False
              }
            , { content = BackgroundImage { url = "./dr-who-spectrogram.png", height = "400px", bgSize = "1300px", bgPosition = "80% 80%" }
              , title = "Dr. Who's opening theme Spectrogram"
              , difficulty = Just Easy
              , description =
                    [ text "I created this using "
                    , a [ href "https://developer.apple.com/documentation/accelerate/visualizing_sound_as_an_audio_spectrogram" ] [ text "Apple's spectrogram sample code" ]
                    , text " while listening to "
                    , a [ href "https://www.youtube.com/watch?v=lCwTj5ZrKIE" ] [ text "this recreation of Dr. Who's theme" ]
                    , text " by Luke Million"
                    ]
              , tags = [ "music", "synth", "dr who", "spectrogram" ]
              , isNima = True
              }
            , { content = Bandcamp { src = "https://bandcamp.com/EmbeddedPlayer/album=2598635858/size=large/bgcol=ffffff/linkcol=0687f5/tracklist=false/transparent=true/", linkUrl = "https://tonyharrismusic.bandcamp.com/album/peaceful-atmosphere", linkText = "Peaceful Atmosphere by Tony Harris" }
              , title = "Kalimba played by Tony Harris from Sacramento"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "kalimba", "tony harris", "sacramento", "instrument" ]
              , isNima = True
              }
            , { content = YouTube "ajM4vYCZMZk"
              , title = "Ennio Morricone - The Ecstasy of Gold (Theremin & Voice by Carolina Eyck)"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "theremin", "ennio morricone", "carolina eyck", "instrument" ]
              , isNima = False
              }
            ]
      }
    , { name = "Orchestral"
      , items =
            [ { content = YouTube "enuOArEfqGo"
              , title = "The Good, the Bad and the Ugly - The Danish National Symphony Orchestra (Live)"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "orchestral", "live", "danish symphony", "ennio morricone" ]
              , isNima = False
              }
            , { content = YouTube "jtGL2Tqfdws"
              , title = "Twin Peaks // The Danish National Symphony Orchestra (Live)"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "orchestral", "live", "danish symphony", "twin peaks" ]
              , isNima = False
              }
            , { content = YouTube "MjxsZa3Ylao?si=5hA1rHh5ryvJAyeB"
              , title = "Fantasia Apocalyptica"
              , difficulty = Just Medium
              , description = []
              , tags = [ "music", "orchestral", "fantasia apocalyptica", "knuth" ]
              , isNima = False
              }
            ]
      }
    , { name = "Behind the scenes: Sound synthesis and music"
      , items =
            [ { content = YouTube "mam6o9O7oyE?si=-fSWz4tS27oJuThp&amp;clip=UgkxPdl3XkwN6wwMpTZ5_Vbd0zPSmOtYIF2r&amp;clipt=EJDlIhigsyM"
              , title = "\"Because it's hard... man!\""
              , difficulty = Just Hard
              , description =
                    [ text "Creators of OP1 (Teenage Engineering) responding to a question from the interviewer: "
                    , em [] [ text "\"I don't know why more people aren't making beautiful things?\"" ]
                    ]
              , tags = [ "music", "behind the scenes", "teenage engineering", "op1", "synthesis" ]
              , isNima = False
              }
            , { content = YouTube "_gRD9k_impU"
              , title = "Jan Overduin Invites You to Knuth's \"Fantasia Apocalyptica\""
              , difficulty = Just Medium
              , description =
                    [ text "I like the literal interpretations in this video" ]
              , tags = [ "music", "behind the scenes", "knuth", "fantasia apocalyptica", "organ" ]
              , isNima = False
              }
            , { content = YouTube "2ZpwbXDleDw"
              , title = "Recipe for Musique Concrete"
              , difficulty = Just Medium
              , description =
                    [ text "I love the sauce pan sounds towards the end of the video" ]
              , tags = [ "music", "behind the scenes", "musique concrete", "synthesis" ]
              , isNima = False
              }
            , { content = YouTube "e-eqgr_gn4k"
              , title = "Angelo Badalamenti explains how he wrote Laura Palmer's Theme"
              , difficulty = Just Medium
              , description = []
              , tags = [ "music", "behind the scenes", "twin peaks", "angelo badalamenti", "laura palmer" ]
              , isNima = False
              }
            ]
      }
    , { name = "Live performances"
      , items =
            [ { content = YouTube "mawK6iVwlMA"
              , title = "\"Law and Order\" Theme Song"
              , difficulty = Just Easy
              , description =
                    [ em [] [ text "\"It's so good!\"" ]
                    , text ".. Here's a "
                    , a [ href "https://www.youtube.com/watch?v=i92Ixk3-d1o" ] [ text "good piano tutorial" ]
                    , text " of it."
                    ]
              , tags = [ "music", "live", "law and order", "performance" ]
              , isNima = False
              }
            ]
      }
    , { name = "Computers and Programming"
      , items =
            [ { content = Vimeo "https://player.vimeo.com/video/72501076?h=fd3e338a90#t=21m22s"
              , title = "Hyperland (1990) by Douglas Adams"
              , difficulty = Just Hard
              , description = []
              , tags = [ "computers", "programming", "douglas adams", "hyperland", "hypertext" ]
              , isNima = False
              }
            , { content = YouTube "YDvbDiJZpy0"
              , title = "Meet the inventor of electronic spreadsheets"
              , difficulty = Just Medium
              , description = []
              , tags = [ "computers", "programming", "spreadsheet", "visicalc" ]
              , isNima = False
              }
            , { content = YouTube "Ow9AtuIuMLw"
              , title = "Eval/Apply"
              , difficulty = Just Hard
              , description =
                    [ text "\"You take the blue pill... the story ends, you wake up in your bed and believe whatever you want to believe. You take "
                    , a [ href "https://www.youtube.com/watch?v=2Op3QLzMgSY&list=PLE18841CABEA24090" ] [ text "the red pill" ]
                    , text "... you stay in Wonderland, and I show you how deep the rabbit hole goes.\""
                    ]
              , tags = [ "computers", "programming", "sicp", "eval", "apply", "lisp" ]
              , isNima = False
              }
            , { content = YouTube "STVdCJaG0bY"
              , title = "The Tech Model Railroad Club of MIT"
              , difficulty = Just Medium
              , description = []
              , tags = [ "computers", "programming", "mit", "hackers", "model railroad" ]
              , isNima = False
              }
            ]
      }
    ]



-- SEARCH


type alias ParsedQuery =
    { freeText : List String
    , difficulty : Maybe String
    , section : Maybe String
    , contentType : Maybe String
    , isNima : Maybe Bool
    , tag : Maybe String
    }


parseQuery : String -> ParsedQuery
parseQuery query =
    let
        tokens =
            String.words (String.toLower query)

        fold token acc =
            case String.split ":" token of
                [ "difficulty", val ] ->
                    { acc | difficulty = Just val }

                [ "section", val ] ->
                    { acc | section = Just val }

                [ "type", val ] ->
                    { acc | contentType = Just val }

                [ "is", "nima" ] ->
                    { acc | isNima = Just True }

                [ "tag", val ] ->
                    { acc | tag = Just val }

                _ ->
                    { acc | freeText = acc.freeText ++ [ token ] }
    in
    List.foldl fold
        { freeText = [], difficulty = Nothing, section = Nothing, contentType = Nothing, isNima = Nothing, tag = Nothing }
        tokens


difficultyToString : Difficulty -> String
difficultyToString d =
    case d of
        Easy ->
            "easy"

        Medium ->
            "medium"

        Hard ->
            "hard"


contentTypeString : Content -> String
contentTypeString content =
    case content of
        YouTube _ ->
            "youtube"

        Vimeo _ ->
            "vimeo"

        Bandcamp _ ->
            "bandcamp"

        ImageContent _ ->
            "image"

        BackgroundImage _ ->
            "image"

        LinkOnly _ ->
            "link"


matchesItem : ParsedQuery -> Item -> Bool
matchesItem pq item =
    let
        searchable =
            String.toLower (item.title ++ " " ++ String.join " " item.tags)

        textMatch =
            List.all (\w -> String.contains w searchable) pq.freeText

        diffMatch =
            case pq.difficulty of
                Nothing ->
                    True

                Just d ->
                    case item.difficulty of
                        Nothing ->
                            False

                        Just diff ->
                            difficultyToString diff == d

        typeMatch =
            case pq.contentType of
                Nothing ->
                    True

                Just t ->
                    contentTypeString item.content == t

        nimaMatch =
            case pq.isNima of
                Nothing ->
                    True

                Just _ ->
                    item.isNima

        tagMatch =
            case pq.tag of
                Nothing ->
                    True

                Just t ->
                    List.any (\tag -> String.contains t (String.toLower tag)) item.tags
    in
    textMatch && diffMatch && typeMatch && nimaMatch && tagMatch


matchesSection : ParsedQuery -> Section -> Bool
matchesSection pq section =
    case pq.section of
        Nothing ->
            True

        Just s ->
            String.contains s (String.toLower section.name)


filterSections : String -> List Section -> List Section
filterSections query sections =
    if String.isEmpty (String.trim query) then
        sections

    else
        let
            pq =
                parseQuery query
        in
        List.filterMap
            (\section ->
                if not (matchesSection pq section) then
                    Nothing

                else
                    let
                        matchingItems =
                            List.filter (matchesItem pq) section.items
                    in
                    if List.isEmpty matchingItems then
                        Nothing

                    else
                        Just { section | items = matchingItems }
            )
            sections



-- VIEW


view : Model -> Html Msg
view model =
    let
        state =
            model.current

        filtered =
            filterSections state.query allSections

        suggestions =
            [ "dr who"
            , "twin peaks"
            , "law and order"
            , "difficulty:easy"
            , "difficulty:hard"
            , "section:orchestral"
            , "type:youtube synth"
            , "is:nima"
            ]
    in
    div []
        [ h1 [] [ text "Things that inspire me (", a [ href "./index.html" ] [ text "back to home" ], text ")" ]
        , viewSearchBar state.query state.viewMode suggestions (getCompletions state.query)
        , case state.viewMode of
            CardView ->
                div [] (List.map (viewSection state.activatedVideos) filtered)

            TableView ->
                viewTable filtered
        , viewBackToHome
        ]


viewSearchBar : String -> ViewMode -> List String -> List String -> Html Msg
viewSearchBar query viewMode suggestions completions =
    div [ class "css-masonry", id "awesome-bar" ]
        [ div [ class "item" ]
            [ div [ style "display" "flex", style "align-items" "center", style "gap" "8px" ]
                [ button
                    [ onClick ToggleViewMode
                    , style "font-size" "1.4em"
                    , style "padding" "4px 12px"
                    , style "cursor" "pointer"
                    , style "border" "1px solid #999"
                    , style "border-radius" "4px"
                    , style "background" "#fff"
                    , title
                        (case viewMode of
                            CardView ->
                                "Switch to table view"

                            TableView ->
                                "Switch to card view"
                        )
                    ]
                    [ text
                        (case viewMode of
                            CardView ->
                                "\u{2637}"

                            TableView ->
                                "\u{2630}"
                        )
                    ]
                , input
                    [ type_ "text"
                    , placeholder "\u{1F50D} narrow down (e.g. 'dr who', 'synth', 'live')"
                    , onInput UpdateQuery
                    , style "flex" "1"
                    , id "search-input"
                    ]
                    []
                ]
            , if not (List.isEmpty completions) then
                ul [ style "list-style" "none", style "padding" "4px 0", style "margin" "4px 0 0 0" ]
                    (List.map
                        (\c ->
                            li
                                [ onClick (CompleteToken c)
                                , style "cursor" "pointer"
                                , style "padding" "4px 8px"
                                , style "border-radius" "4px"
                                , style "display" "inline-block"
                                , style "margin" "2px 4px"
                                , style "background" "#eef"
                                , style "border" "1px solid #ccd"
                                , style "font-family" "monospace"
                                ]
                                [ text c ]
                        )
                        completions
                    )

              else if String.isEmpty (String.trim query) then
                ul []
                    (List.map
                        (\s -> li [] [ text s ])
                        suggestions
                    )

              else
                text ""
            ]
        ]


viewSection : Set String -> Section -> Html Msg
viewSection activatedVideos section =
    div []
        [ h2 [] [ text section.name ]
        , div [ class "css-masonry" ]
            (List.map (viewItem activatedVideos) section.items)
        ]


viewItem : Set String -> Item -> Html Msg
viewItem activatedVideos item =
    div
        [ classList
            [ ( "item", True )
            , ( "nima", item.isNima )
            ]
        ]
        [ viewContent activatedVideos item.content
        , viewTitle item
        ]


youtubeBaseId : String -> String
youtubeBaseId videoId =
    case String.split "?" videoId of
        base :: _ ->
            base

        [] ->
            videoId


viewContent : Set String -> Content -> Html Msg
viewContent activatedVideos content =
    case content of
        YouTube videoId ->
            if Set.member videoId activatedVideos then
                iframe
                    [ width 100
                    , style "width" "100%"
                    , style "min-height" "400px"
                    , src ("https://www.youtube.com/embed/" ++ videoId ++ (if String.contains "?" videoId then "&autoplay=1" else "?autoplay=1"))
                    , attribute "frameborder" "0"
                    , attribute "allowfullscreen" ""
                    , attribute "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
                    ]
                    []

            else
                div
                    [ style "position" "relative"
                    , style "cursor" "pointer"
                    , style "min-height" "400px"
                    , style "background-image" ("url('https://img.youtube.com/vi/" ++ youtubeBaseId videoId ++ "/hqdefault.jpg')")
                    , style "background-size" "cover"
                    , style "background-position" "center"
                    , onClick (ActivateVideo videoId)
                    ]
                    [ div
                        [ style "position" "absolute"
                        , style "top" "50%"
                        , style "left" "50%"
                        , style "transform" "translate(-50%, -50%)"
                        , style "width" "68px"
                        , style "height" "48px"
                        , style "background-color" "rgba(204, 0, 0, 0.9)"
                        , style "border-radius" "14px"
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "justify-content" "center"
                        ]
                        [ div
                            [ style "width" "0"
                            , style "height" "0"
                            , style "border-style" "solid"
                            , style "border-width" "10px 0 10px 20px"
                            , style "border-color" "transparent transparent transparent white"
                            , style "margin-left" "4px"
                            ]
                            []
                        ]
                    ]

        Vimeo embedUrl ->
            div [ style "padding" "56.25% 0 0 0", style "position" "relative" ]
                [ iframe
                    [ src embedUrl
                    , style "position" "absolute"
                    , style "top" "0"
                    , style "left" "0"
                    , style "width" "100%"
                    , style "height" "100%"
                    , attribute "frameborder" "0"
                    , attribute "allow" "autoplay; fullscreen; picture-in-picture"
                    , attribute "allowfullscreen" ""
                    ]
                    []
                ]

        Bandcamp info ->
            iframe
                [ style "border" "0"
                , style "width" "100%"
                , style "height" "470px"
                , src info.src
                , attribute "seamless" ""
                ]
                [ a [ href info.linkUrl ] [ text info.linkText ] ]

        ImageContent url ->
            img [ src url, style "width" "100%" ] []

        BackgroundImage info ->
            div
                [ style "width" "100%"
                , style "background-image" ("url('" ++ info.url ++ "')")
                , style "display" "block"
                , style "height" info.height
                , style "background-size" info.bgSize
                , style "background-position" info.bgPosition
                ]
                []

        LinkOnly info ->
            text ""


viewTitle : Item -> Html Msg
viewTitle item =
    case item.content of
        LinkOnly info ->
            div [ class "title" ]
                [ text "See "
                , a [ href info.url ] [ code [] [ text info.label ] ]
                , viewTags item.tags
                ]

        _ ->
            div [ class "title" ]
                (List.concat
                    [ case item.difficulty of
                        Just d ->
                            [ viewDifficulty d, text " " ]

                        Nothing ->
                            []
                    , [ text item.title ]
                    , if List.isEmpty item.description then
                        []

                      else
                        [ br [] []
                        , br [] []
                        , p [ class "french" ] item.description
                        ]
                    , [ viewTags item.tags ]
                    ]
                )


viewTags : List String -> Html Msg
viewTags tags =
    div [ style "margin-top" "6px", style "display" "flex", style "flex-wrap" "wrap", style "gap" "4px" ]
        (List.map
            (\t ->
                span
                    [ onClick (CompleteToken ("tag:" ++ t))
                    , style "background" "#e8e8e8"
                    , style "color" "#555"
                    , style "padding" "2px 7px"
                    , style "border-radius" "3px"
                    , style "font-size" "0.75em"
                    , style "cursor" "pointer"
                    , style "font-family" "monospace"
                    ]
                    [ text t ]
            )
            tags
        )


viewDifficulty : Difficulty -> Html Msg
viewDifficulty d =
    let
        ( label, cls ) =
            case d of
                Easy ->
                    ( "easy", "easy" )

                Medium ->
                    ( "medium", "medium" )

                Hard ->
                    ( "hard", "hard" )
    in
    div [ class ("difficulty-level " ++ cls) ] [ text label ]


contentUrl : Content -> Maybe String
contentUrl content =
    case content of
        YouTube videoId ->
            Just ("https://www.youtube.com/watch?v=" ++ youtubeBaseId videoId)

        Vimeo embedUrl ->
            Just embedUrl

        Bandcamp info ->
            Just info.linkUrl

        ImageContent url ->
            Just url

        BackgroundImage info ->
            Just info.url

        LinkOnly info ->
            Just info.url


contentType : Content -> String
contentType content =
    case content of
        YouTube _ ->
            "YouTube"

        Vimeo _ ->
            "Vimeo"

        Bandcamp _ ->
            "Bandcamp"

        ImageContent _ ->
            "Image"

        BackgroundImage _ ->
            "Image"

        LinkOnly _ ->
            "Link"


viewTable : List Section -> Html Msg
viewTable sections =
    div [ style "margin" "1.5em" ]
        (List.map viewTableSection sections)


viewTableSection : Section -> Html Msg
viewTableSection section =
    div [ style "margin-bottom" "2em" ]
        [ h2 [] [ text section.name ]
        , table
            [ style "width" "100%"
            , style "border-collapse" "collapse"
            , style "background" "#fff"
            , style "font-size" "0.85em"
            ]
            (thead []
                [ tr []
                    [ th [ style "text-align" "left", style "padding" "8px", style "border-bottom" "2px solid #333" ] [ text "Title" ]
                    , th [ style "text-align" "left", style "padding" "8px", style "border-bottom" "2px solid #333" ] [ text "Type" ]
                    , th [ style "text-align" "left", style "padding" "8px", style "border-bottom" "2px solid #333" ] [ text "Difficulty" ]
                    , th [ style "text-align" "left", style "padding" "8px", style "border-bottom" "2px solid #333" ] [ text "Tags" ]
                    ]
                ]
                :: List.map viewTableRow section.items
            )
        ]


viewTableRow : Item -> Html Msg
viewTableRow item =
    tr []
        [ td [ style "padding" "8px", style "border-bottom" "1px solid #ddd" ]
            [ case contentUrl item.content of
                Just url ->
                    a [ href url, target "_blank" ] [ text item.title ]

                Nothing ->
                    text item.title
            ]
        , td [ style "padding" "8px", style "border-bottom" "1px solid #ddd" ]
            [ text (contentType item.content) ]
        , td [ style "padding" "8px", style "border-bottom" "1px solid #ddd" ]
            [ case item.difficulty of
                Just d ->
                    viewDifficulty d

                Nothing ->
                    text ""
            ]
        , td [ style "padding" "8px", style "border-bottom" "1px solid #ddd" ]
            [ viewTags item.tags ]
        ]


viewBackToHome : Html Msg
viewBackToHome =
    div []
        [ h2 [] [ text "Back to home" ]
        , div [ class "css-masonry css-nimasonry" ]
            [ div [ class "item" ]
                [ div [ class "title" ]
                    [ a [ href "./index.html" ]
                        [ text "\u{2190} Back to "
                        , code [] [ text "./index.html" ]
                        ]
                    ]
                ]
            ]
        ]
