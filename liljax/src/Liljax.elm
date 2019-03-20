import Browser
import Html exposing (div, text)
import Html.Attributes exposing (class)

type alias Model = ()
type alias Msg = ()

main : Program () Model Msg
main = Browser.sandbox {init = init, update = update, view = view}

init : Model
init = ()

update : Msg -> Model -> Model
update () () = ()

view : Model -> Html.Html Msg
view () = div [class "liljax"] [text "hello world"]
