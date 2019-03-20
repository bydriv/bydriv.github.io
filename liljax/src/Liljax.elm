import Browser
import Html exposing (div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)

type alias Model = String
type alias Msg = String

main : Program () Model Msg
main = Browser.sandbox {init = init, update = update, view = view}

init : Model
init = ""

update : Msg -> Model -> Model
update msg _ = msg

view : Model -> Html.Html Msg
view model = div [class "liljax"] [input [ placeholder "what's your name?", value model, onInput (\x -> x) ] [], text "hello ", text (if model == "" then "world" else model)]
