port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)
import String


port setInputValue : String -> Cmd msg


port onTab : (() -> msg) -> Sub msg


port onUndo : (() -> msg) -> Sub msg


port onPinClick : (String -> msg) -> Sub msg


port onBoundsSelect : (Encode.Value -> msg) -> Sub msg


port updateMap : Encode.Value -> Cmd msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, syncMap "" )
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onUndo (\_ -> Undo)
                    , onTab (\_ -> TabComplete)
                    , onPinClick PinClick
                    , onBoundsSelect DecodeBounds
                    ]
        }



-- MODEL


type ViewMode
    = CardView
    | TableView


type alias Bounds =
    { south : Float
    , west : Float
    , north : Float
    , east : Float
    }


type alias AppState =
    { query : String
    , activatedVideos : Set String
    , viewMode : ViewMode
    , mapBounds : Maybe Bounds
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
    , mapBounds = Nothing
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


type alias MapMarker =
    { lat : Float
    , lng : Float
    , label : String
    , thumbnail : Maybe String
    }


collectMarkers : List Section -> List MapMarker
collectMarkers sections =
    List.concatMap
        (\section ->
            List.filterMap
                (\item ->
                    Maybe.map
                        (\loc ->
                            { lat = loc.lat
                            , lng = loc.lng
                            , label = loc.label
                            , thumbnail = itemThumbnail item.content
                            }
                        )
                        item.location
                )
                section.items
        )
        sections


itemThumbnail : Content -> Maybe String
itemThumbnail content =
    case content of
        YouTube videoId ->
            Just ("https://img.youtube.com/vi/" ++ youtubeBaseId videoId ++ "/default.jpg")

        _ ->
            Nothing


encodeMarkers : List MapMarker -> Encode.Value
encodeMarkers markers =
    Encode.list
        (\m ->
            Encode.object
                ([ ( "lat", Encode.float m.lat )
                 , ( "lng", Encode.float m.lng )
                 , ( "label", Encode.string m.label )
                 ]
                    ++ (case m.thumbnail of
                            Just url ->
                                [ ( "thumbnail", Encode.string url ) ]

                            Nothing ->
                                []
                       )
                )
        )
        markers


syncMap : String -> Cmd Msg
syncMap query =
    updateMap (encodeMarkers (collectMarkers (filterSections query Nothing allSections)))


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


type alias Location =
    { lat : Float
    , lng : Float
    , label : String
    }


type alias Item =
    { content : Content
    , title : String
    , difficulty : Maybe Difficulty
    , description : List (Html Msg)
    , tags : List String
    , isNima : Bool
    , location : Maybe Location
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


allLocations : List String
allLocations =
    List.concatMap
        (\section ->
            List.filterMap
                (\item -> Maybe.map .label item.location)
                section.items
        )
        allSections
        |> Set.fromList
        |> Set.toList


quoteIfNeeded : String -> String
quoteIfNeeded s =
    if String.contains " " s then
        "\"" ++ s ++ "\""

    else
        s


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
        ++ List.map (\t -> "tag:" ++ quoteIfNeeded t) allTags
        ++ List.map (\l -> "location:" ++ quoteIfNeeded l) allLocations


tokenize : String -> List String
tokenize query =
    tokenizeHelp (String.toList query) "" [] False


tokenizeHelp : List Char -> String -> List String -> Bool -> List String
tokenizeHelp chars current tokens inQuote =
    case chars of
        [] ->
            if String.isEmpty current then
                List.reverse tokens

            else
                List.reverse (current :: tokens)

        '"' :: rest ->
            tokenizeHelp rest (current ++ "\"") tokens (not inQuote)

        ' ' :: rest ->
            if inQuote then
                tokenizeHelp rest (current ++ " ") tokens True

            else if String.isEmpty current then
                tokenizeHelp rest "" tokens False

            else
                tokenizeHelp rest "" (current :: tokens) False

        c :: rest ->
            tokenizeHelp rest (current ++ String.fromChar c) tokens inQuote


lastToken : String -> String
lastToken query =
    case List.reverse (tokenize query) of
        tok :: _ ->
            tok

        [] ->
            ""


replaceLastToken : String -> String -> String
replaceLastToken query completion =
    let
        tokens =
            tokenize query

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
        List.filter (\c -> String.startsWith tok (String.toLower c) && String.toLower c /= tok) allCompletions



-- UPDATE


type Msg
    = UpdateQuery String
    | ActivateVideo String
    | ToggleViewMode
    | CompleteToken String
    | Undo
    | TabComplete
    | PinClick String
    | DecodeBounds Encode.Value
    | ClearBounds


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
            ( { base | current = { state | query = q }, typing = True }, syncMap q )

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
            ( { pushed | current = { state | query = newQuery }, typing = False }
            , Cmd.batch [ setInputValue newQuery, syncMap newQuery ]
            )

        Undo ->
            case model.history of
                prev :: rest ->
                    ( { model | current = prev, history = rest, typing = False }
                    , Cmd.batch [ setInputValue prev.query, syncMap prev.query ]
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
                    ( { pushed | current = { state | query = newQuery }, typing = False }
                    , Cmd.batch [ setInputValue newQuery, syncMap newQuery ]
                    )

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
                        ( { pushed | current = { state | query = newQuery }, typing = False }
                        , Cmd.batch [ setInputValue newQuery, syncMap newQuery ]
                        )

                    else
                        ( model, Cmd.none )

        PinClick _ ->
            ( model, Cmd.none )

        DecodeBounds value ->
            let
                decoder =
                    Decode.map4 Bounds
                        (Decode.field "south" Decode.float)
                        (Decode.field "west" Decode.float)
                        (Decode.field "north" Decode.float)
                        (Decode.field "east" Decode.float)
            in
            case Decode.decodeValue decoder value of
                Ok bounds ->
                    let
                        pushed =
                            pushHistory model
                    in
                    ( { pushed | current = { state | mapBounds = Just bounds }, typing = False }
                    , Cmd.none
                    )

                Err _ ->
                    -- null or invalid → clear bounds
                    if state.mapBounds /= Nothing then
                        let
                            pushed =
                                pushHistory model
                        in
                        ( { pushed | current = { state | mapBounds = Nothing }, typing = False }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        ClearBounds ->
            let
                pushed =
                    pushHistory model
            in
            ( { pushed | current = { state | mapBounds = Nothing }, typing = False }
            , syncMap state.query
            )



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
              , location = Just { lat = 29.5918, lng = 52.5837, label = "Shiraz, Iran" }
              }
            , { content = YouTube "4riphWzBpuA"
              , title = "Macaroni (Makaroni) Persian Style Spaghetti Recipe"
              , difficulty = Just Easy
              , description = []
              , tags = [ "cooking", "food", "persian", "macaroni", "pasta" ]
              , isNima = False
              , location = Just { lat = 35.8120, lng = 51.4260, label = "Darband, Tehran" }
              }
            , { content = YouTube "K7xihvdDBxE"
              , title = "Double-onionized potatoe"
              , difficulty = Just Easy
              , description = []
              , tags = [ "cooking", "food", "persian", "potato" ]
              , isNima = False
              , location = Just { lat = 31.3183, lng = 48.6706, label = "Ahvaz, Iran" }
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
              , location = Nothing
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
              , location = Nothing
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
              , location = Just { lat = 47.0451, lng = -122.8957, label = "Capitol Theater, Olympia" }
              }
            , { content = YouTube "5rZ2H2YGAgI"
              , title = "Visualizing \"dentistry!\" preset on dexed (Yamaha DX7 emulator)"
              , difficulty = Just Easy
              , description =
                    [ text "Demo of me playing with \"Dentistry!\" preset and visualizing its funky spectrogram" ]
              , tags = [ "music", "synth", "dexed", "dx7", "fm synthesis", "spectrogram", "sound" ]
              , isNima = True
              , location = Just { lat = 47.0477, lng = -122.9010, label = "Obsidian, Olympia" }
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
              , location = Just { lat = 47.0387, lng = -122.8908, label = "Le Voyeur, Olympia" }
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
              , location = Just { lat = 38.5816, lng = -121.5090, label = "Crocker Art Museum, Sacramento" }
              }
            , { content = YouTube "s-CTkbHnpNQ"
              , title = "10 Bullets, #8: \"ALWAYS BE KNOLLING\". By Tom Sachs"
              , difficulty = Just Easy
              , description =
                    [ text "Knoll /nōl/ vb. (1989 USA) to arrange like objects in parallel or 90 degree angles as a method of organization." ]
              , tags = [ "lifestyle", "organization", "tom sachs", "knolling" ]
              , isNima = False
              , location = Nothing
              }
            , { content = YouTube "eoOUBETTyMI"
              , title = "Discipline of Do Easy by Gus Van Sant"
              , difficulty = Just Easy
              , description = []
              , tags = [ "lifestyle", "gus van sant" ]
              , isNima = False
              , location = Nothing
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
              , location = Just { lat = 35.6295, lng = 139.7745, label = "Odaiba, Tokyo" }
              }
            , { content = ImageContent "http://www.openpool.cc/wp-content/uploads/howitworks-new-700x602.png"
              , title = "Pool table and a projector"
              , difficulty = Just Medium
              , description = []
              , tags = [ "billiards", "pool", "projector" ]
              , isNima = False
              , location = Nothing
              }
            , { content = ImageContent "https://i0.wp.com/projectionprobilliards.com/wp-content/uploads/2020/03/projector-top.png?resize=1024%2C576&ssl=1"
              , title = "Pool table and a projector"
              , difficulty = Just Medium
              , description = []
              , tags = [ "billiards", "pool", "projector" ]
              , isNima = False
              , location = Nothing
              }
            , { content = ImageContent "https://projectionprobilliards.com/wp-content/uploads/2020/03/redtable-with-grid.jpg"
              , title = "Projection Pro Billiards"
              , difficulty = Just Medium
              , description = []
              , tags = [ "billiards", "pool", "projection" ]
              , isNima = False
              , location = Nothing
              }
            , { content = Vimeo "https://player.vimeo.com/video/209591449?h=583a1f54a6&title=0&byline=0&portrait=0#t=7s"
              , title = "La Tabla"
              , difficulty = Just Medium
              , description = []
              , tags = [ "billiards", "pool", "la tabla", "interactive" ]
              , isNima = False
              , location = Nothing
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
              , location = Just { lat = 35.7023, lng = 139.7745, label = "Akihabara, Tokyo" }
              }
            , { content = YouTube "kHVTuAc1mI4"
              , title = "Music Synthesizer for Android (demo by Raph Levien)"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "synth", "synthesizer", "android", "raph levien", "instrument" ]
              , isNima = False
              , location = Nothing
              }
            , { content = YouTube "9eWouzXO7_M?si=hOuz0KZ1kG4Zj7EH&amp;start=74"
              , title = "Arp Odyssey 2800 playing a segment of Dr. Who's theme"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "synth", "synthesizer", "arp odyssey", "dr who", "instrument" ]
              , isNima = False
              , location = Nothing
              }
            , { content = YouTube "-XHJekCcxUw?si=umhYR19KZmeBHKb8&amp;start=40"
              , title = "Arp Odyssey playing a segment of Dr. Who's theme"
              , difficulty = Just Medium
              , description = []
              , tags = [ "music", "synth", "synthesizer", "arp odyssey", "dr who", "instrument" ]
              , isNima = False
              , location = Nothing
              }
            , { content = YouTube "lCwTj5ZrKIE"
              , title = "Dr. Who's theme played on vintage synths"
              , difficulty = Just Hard
              , description = []
              , tags = [ "music", "synth", "synthesizer", "dr who", "vintage", "instrument" ]
              , isNima = False
              , location = Nothing
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
              , location = Just { lat = 38.5441, lng = -121.7381, label = "Downtown Davis" }
              }
            , { content = Bandcamp { src = "https://bandcamp.com/EmbeddedPlayer/album=2598635858/size=large/bgcol=ffffff/linkcol=0687f5/tracklist=false/transparent=true/", linkUrl = "https://tonyharrismusic.bandcamp.com/album/peaceful-atmosphere", linkText = "Peaceful Atmosphere by Tony Harris" }
              , title = "Kalimba played by Tony Harris from Sacramento"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "kalimba", "tony harris", "sacramento", "instrument" ]
              , isNima = True
              , location = Just { lat = 38.5816, lng = -121.4944, label = "Sacramento" }
              }
            , { content = YouTube "ajM4vYCZMZk"
              , title = "Ennio Morricone - The Ecstasy of Gold (Theremin & Voice by Carolina Eyck)"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "theremin", "ennio morricone", "carolina eyck", "instrument" ]
              , isNima = False
              , location = Nothing
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
              , location = Just { lat = 55.6736, lng = 12.5681, label = "Tivoli Gardens, Copenhagen" }
              }
            , { content = YouTube "jtGL2Tqfdws"
              , title = "Twin Peaks // The Danish National Symphony Orchestra (Live)"
              , difficulty = Just Easy
              , description = []
              , tags = [ "music", "orchestral", "live", "danish symphony", "twin peaks" ]
              , isNima = False
              , location = Just { lat = 55.6797, lng = 12.5916, label = "Nyhavn, Copenhagen" }
              }
            , { content = YouTube "MjxsZa3Ylao?si=5hA1rHh5ryvJAyeB"
              , title = "Fantasia Apocalyptica"
              , difficulty = Just Medium
              , description = []
              , tags = [ "music", "orchestral", "fantasia apocalyptica", "knuth" ]
              , isNima = False
              , location = Nothing
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
              , location = Just { lat = 59.3293, lng = 18.0686, label = "Stockholm, Sweden" }
              }
            , { content = YouTube "_gRD9k_impU"
              , title = "Jan Overduin Invites You to Knuth's \"Fantasia Apocalyptica\""
              , difficulty = Just Medium
              , description =
                    [ text "I like the literal interpretations in this video" ]
              , tags = [ "music", "behind the scenes", "knuth", "fantasia apocalyptica", "organ" ]
              , isNima = False
              , location = Nothing
              }
            , { content = YouTube "2ZpwbXDleDw"
              , title = "Recipe for Musique Concrete"
              , difficulty = Just Medium
              , description =
                    [ text "I love the sauce pan sounds towards the end of the video" ]
              , tags = [ "music", "behind the scenes", "musique concrete", "synthesis" ]
              , isNima = False
              , location = Nothing
              }
            , { content = YouTube "e-eqgr_gn4k"
              , title = "Angelo Badalamenti explains how he wrote Laura Palmer's Theme"
              , difficulty = Just Medium
              , description = []
              , tags = [ "music", "behind the scenes", "twin peaks", "angelo badalamenti", "laura palmer" ]
              , isNima = False
              , location = Nothing
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
              , location = Nothing
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
              , location = Nothing
              }
            , { content = YouTube "RfVZtPk46rI"
              , title = "Git training promo (in Farsi)"
              , difficulty = Just Easy
              , description = []
              , tags = [ "computers", "programming", "git", "farsi", "training" ]
              , isNima = True
              , location = Just { lat = 35.6997, lng = 51.3380, label = "Azadi Tower, Tehran" }
              }
            , { content = YouTube "xeVyOUWOuB4"
              , title = "Automerge local-first attributed tree demo (realtime sync with CRDTs)"
              , difficulty = Just Hard
              , description = []
              , tags = [ "computers", "programming", "automerge", "crdt", "local-first", "sync" ]
              , isNima = True
              , location = Just { lat = 52.2053, lng = 0.1218, label = "Cambridge, UK" }
              }
            , { content = YouTube "IvY4KcGaxms"
              , title = "The purple box: Easy PDF bookmarks via highlighting"
              , difficulty = Just Medium
              , description = []
              , tags = [ "computers", "programming", "pdf", "bookmarks" ]
              , isNima = True
              , location = Just { lat = 37.8590, lng = -122.4852, label = "Sausalito Houseboats" }
              }
            , { content = YouTube "NZo4cGzcSK0"
              , title = "Rich Spreadsheets (Minicell demo for Andy)"
              , difficulty = Just Medium
              , description =
                    [ text "0:50 Motivating example. 2:45 Code-free interaction model to pluck dynamic graphs out of spreadsheets. 3:50 Calculating the shortest path. 5:14 Minicell's graph primitives. 6:05 Shapes. 7:43 Combining shapes. 8:24 Integers that flow in time. Arithmetic on integers that flow in time. 9:08 Making a 5-frame animation." ]
              , tags = [ "computers", "programming", "spreadsheet", "minicell", "graphs", "animation" ]
              , isNima = True
              , location = Just { lat = 47.6423, lng = -122.1391, label = "Building 36, Microsoft, Redmond" }
              }
            , { content = YouTube "YDvbDiJZpy0"
              , title = "Meet the inventor of electronic spreadsheets"
              , difficulty = Just Medium
              , description = []
              , tags = [ "computers", "programming", "spreadsheet", "visicalc" ]
              , isNima = False
              , location = Nothing
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
              , location = Nothing
              }
            , { content = YouTube "STVdCJaG0bY"
              , title = "The Tech Model Railroad Club of MIT"
              , difficulty = Just Medium
              , description = []
              , tags = [ "computers", "programming", "mit", "hackers", "model railroad" ]
              , isNima = False
              , location = Just { lat = 42.3601, lng = -71.0942, label = "MIT, Cambridge, MA" }
              }
            ]
      }
    , { name = "Nature"
      , items =
            [ { content = YouTube "S6c5g22YNEw"
              , title = "Birds and Water sound. :)"
              , difficulty = Nothing
              , description = []
              , tags = [ "nature", "birds", "water", "sound" ]
              , isNima = True
              , location = Just { lat = 38.5254, lng = -121.7628, label = "Putah Creek Riparian Reserve, Davis" }
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
    , location : Maybe String
    }


stripQuotes : String -> String
stripQuotes s =
    if String.startsWith "\"" s && String.endsWith "\"" s then
        String.slice 1 (String.length s - 1) s

    else
        s


splitOperator : String -> Maybe ( String, String )
splitOperator token =
    case String.indexes ":" token of
        i :: _ ->
            let
                key =
                    String.left i token

                val =
                    stripQuotes (String.dropLeft (i + 1) token)
            in
            if String.isEmpty val then
                Nothing

            else
                Just ( key, val )

        [] ->
            Nothing


parseQuery : String -> ParsedQuery
parseQuery query =
    let
        tokens =
            tokenize (String.toLower query)

        fold token acc =
            case splitOperator token of
                Just ( "difficulty", val ) ->
                    { acc | difficulty = Just val }

                Just ( "section", val ) ->
                    { acc | section = Just val }

                Just ( "type", val ) ->
                    { acc | contentType = Just val }

                Just ( "tag", val ) ->
                    { acc | tag = Just val }

                Just ( "location", val ) ->
                    { acc | location = Just val }

                Just ( "is", "nima" ) ->
                    { acc | isNima = Just True }

                _ ->
                    { acc | freeText = acc.freeText ++ [ token ] }
    in
    List.foldl fold
        { freeText = [], difficulty = Nothing, section = Nothing, contentType = Nothing, isNima = Nothing, tag = Nothing, location = Nothing }
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

        locationMatch =
            case pq.location of
                Nothing ->
                    True

                Just l ->
                    case item.location of
                        Nothing ->
                            False

                        Just loc ->
                            String.contains l (String.toLower loc.label)
    in
    textMatch && diffMatch && typeMatch && nimaMatch && tagMatch && locationMatch


matchesSection : ParsedQuery -> Section -> Bool
matchesSection pq section =
    case pq.section of
        Nothing ->
            True

        Just s ->
            String.contains s (String.toLower section.name)


itemInBounds : Bounds -> Item -> Bool
itemInBounds bounds item =
    case item.location of
        Nothing ->
            False

        Just loc ->
            loc.lat >= bounds.south && loc.lat <= bounds.north && loc.lng >= bounds.west && loc.lng <= bounds.east


filterSections : String -> Maybe Bounds -> List Section -> List Section
filterSections query maybeBounds sections =
    let
        hasQuery =
            not (String.isEmpty (String.trim query))

        hasBounds =
            maybeBounds /= Nothing

        pq =
            parseQuery query

        filterItems section =
            let
                queryFiltered =
                    if hasQuery then
                        List.filter (matchesItem pq) section.items

                    else
                        section.items

                boundsFiltered =
                    case maybeBounds of
                        Just bounds ->
                            List.filter (itemInBounds bounds) queryFiltered

                        Nothing ->
                            queryFiltered
            in
            boundsFiltered
    in
    if not hasQuery && not hasBounds then
        sections

    else
        List.filterMap
            (\section ->
                if hasQuery && not (matchesSection pq section) then
                    Nothing

                else
                    let
                        matchingItems =
                            filterItems section
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
            filterSections state.query state.mapBounds allSections

        suggestions =
            [ "dr who"
            , "twin peaks"
            , "law and order"
            , "difficulty:easy"
            , "section:orchestral"
            , "type:youtube synth"
            , "is:nima"
            , "location:\"Davis, CA\""
            , "tag:\"dr who\""
            ]
    in
    div []
        [ h1 [] [ text "Things that inspire me (", a [ href "./index.html" ] [ text "back to home" ], text ")" ]
        , div [ style "display" "flex", style "gap" "1em", style "margin" "0 1.5em 1em 1.5em", style "align-items" "start" ]
            [ div [ id "map", style "flex" "1", style "height" "calc(100vh - 120px)", style "position" "sticky", style "top" "10px", style "border" "2px solid black", style "border-radius" "6px" ] []
            , div [ style "flex" "1", style "min-width" "0", style "overflow-y" "auto", style "max-height" "calc(100vh - 120px)" ]
                [ viewSearchBar state.query state.viewMode state.mapBounds suggestions (getCompletions state.query)
                , case state.viewMode of
                    CardView ->
                        div [] (List.map (viewSection state.activatedVideos) filtered)

                    TableView ->
                        viewTable filtered
                , viewBackToHome
                ]
            ]
        ]


viewSearchBar : String -> ViewMode -> Maybe Bounds -> List String -> List String -> Html Msg
viewSearchBar query viewMode mapBounds suggestions completions =
    div [ id "awesome-bar" ]
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
            , button
                [ onClick ClearBounds
                , style "cursor" "pointer"
                , style "border" "1px solid #999"
                , style "border-radius" "4px"
                , style "background" "#fff"
                , style "padding" "4px 10px"
                , style "font-size" "0.85em"
                , style "margin-top" "6px"
                ]
                [ text "Reset zoom" ]
            ]
        ]


viewSection : Set String -> Section -> Html Msg
viewSection activatedVideos section =
    div []
        [ h2 [] [ text section.name ]
        , div [ class "css-masonry", style "grid-template-columns" "repeat(2, 1fr)" ]
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
                    , style "aspect-ratio" "16/9"
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
                    , style "aspect-ratio" "16/9"
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
