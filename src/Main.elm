module Main exposing (main)

import Browser exposing (Document)
import Dict exposing (get)
import Html exposing (Html, img, div, text, p, a)
-- import Html.Attributes exposing (href, src, width, height, style)
import Html.Attributes as Attr
import Html.Events
import Http
import List exposing (map, map2, foldl, drop, sortWith, length, concatMap)
import Maybe exposing (withDefault)
import Random exposing (Generator)
import String exposing (concat)
import Task
import Time
import Tuple exposing (pair, first, second)
import Yaml.Decode exposing (fromString, list, dict, string, field)
import Markdown.Parser as Markdown
import Markdown.Renderer

overhead = 7

type alias Link        = String
type alias Icon        = String
type alias Description = String

type Msg = YamlLoaded (Result Http.Error String) | MarkdownLoaded (Result Http.Error String) | None

type alias Model = { post: String, post_addr: String, email: List String }

tuple_extractor a b = \v -> (withDefault "" <| get a v, withDefault "" <| get b v)

decoder : Model -> Yaml.Decode.Decoder Model
decoder model =
  Yaml.Decode.map (\addr -> { model | post_addr = addr }) (field "index" string)

main : Program () Model Msg
main =
  Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

init : () -> (Model, Cmd Msg)
init _ =
  let
    initialState = { post = "", post_addr = "", email = ["sobol", ".", "daniil", "@", "gmail", ".", "com"] }
    commands = Cmd.batch [
      Http.get {url = "../contents.yaml", expect = Http.expectString YamlLoaded}
      ]
  in
    (initialState, commands)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    YamlLoaded result ->
      case result of
        Ok yamlStr ->
          case fromString (decoder model) yamlStr of
            Ok updatedModel ->
              (model, Cmd.batch [Http.get {url = updatedModel.post_addr, expect = Http.expectString MarkdownLoaded}])
            Err e -> (model, Cmd.none)
        Err e -> (model, Cmd.none)
    MarkdownLoaded result ->
      case result of
        Ok post_txt ->
          ({ model | post = post_txt }, Cmd.none)
        Err e -> (model, Cmd.none)
    None -> (model, Cmd.none)

view : Model -> Document Msg
view model = {title = "libportalnet", body = body model}

body : Model -> List (Html msg)
body model =
  [
    Html.div bg_style [],
    Html.div body_style
      [
        case
          model.post
              |> Markdown.parse
              |> Result.mapError deadEndsToString
              |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
        of
          Ok rendered ->
              div [] rendered

          Err errors ->
              text errors
      ]
    ]

deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"

bg_style = [
  Attr.style "background-color" "#f0f0f0",
  Attr.style "padding" "0",
  Attr.style "margin" "0",
  Attr.style "width" "100%",
  Attr.style "height" "100%",
  Attr.style "display" "block",
  Attr.style "position" "static",
  Attr.style "z-index" "-1"
  ]
body_style = [
  Attr.style "padding" "20px",
  Attr.style "font-family" "monospace",
  Attr.style "display" "inline-block",
  Attr.style "z-index" "1"
  ]
