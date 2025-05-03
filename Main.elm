module Main exposing (main)

import Browser
import Html exposing (Html, div, text, h2, ul, li, input, label, span, img, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL

type alias Product =
    { id : Int
    , name : String
    , price : String
    , imageUrl : String
    , category : String
    , description : String
    , colors : List String
    , badge : Maybe String
    }

type alias Model =
    { products : List Product
    , selectedCategories : List String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { products = productList, selectedCategories = [] }
    , Cmd.none
    )


-- MESSAGES

type Msg
    = ToggleCategory String
    | ClearFilters


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCategory cat ->
            let
                updated =
                    if List.member cat model.selectedCategories then
                        List.filter ((/=) cat) model.selectedCategories
                    else
                        cat :: model.selectedCategories
            in
            ( { model | selectedCategories = updated }, Cmd.none )

        ClearFilters ->
            ( { model | selectedCategories = [] }, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    div [ style "display" "flex" ]
        [ sidebar model
        , productGrid model
        ]

sidebar : Model -> Html Msg
sidebar model =
    div [ style "width" "250px", style "padding" "1rem", style "border-right" "1px solid #ddd" ]
        [ h2 [] [ text "Category" ]
        , ul [] (List.map (categoryCheckbox model.selectedCategories) allCategories)
        , button [ onClick ClearFilters, style "margin-top" "1rem" ] [ text "Clear All Filters" ]
        ]

categoryCheckbox : List String -> String -> Html Msg
categoryCheckbox selected labelStr =
    let
        isChecked = List.member labelStr selected
    in
    li []
        [ label []
            [ input
                [ type_ "checkbox"
                , checked isChecked
                , onClick (ToggleCategory labelStr)
                ]
                []
            , text (" " ++ labelStr)
            ]
        ]

productGrid : Model -> Html Msg
productGrid model =
    let
        filtered =
            if List.isEmpty model.selectedCategories then
                model.products
            else
                List.filter (\p -> List.member p.category model.selectedCategories) model.products
    in
    div
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(auto-fit, minmax(250px, 1fr))"
        , style "gap" "1rem"
        , style "padding" "1rem"
        , style "width" "100%"
        ]
        (List.map productCard filtered)

productCard : Product -> Html Msg
productCard product =
    div [ style "border" "1px solid #ccc", style "padding" "1rem", style "text-align" "center" ]
        ([ img [ src product.imageUrl, alt product.name, style "width" "100%", style "height" "200px", style "object-fit" "cover" ] []
         , h2 [] [ text product.name ]
         , span [ style "font-weight" "bold" ] [ text product.price ]
         , div [] (List.map renderColor product.colors)
         ]
            ++ (case product.badge of
                    Just b -> [ div [ style "color" "red", style "margin-top" "0.5rem" ] [ text b ] ]
                    Nothing -> []
               )
            ++ [ div [ style "margin-top" "0.5rem" ] [ text product.description ] ]
        )

renderColor : String -> Html msg
renderColor color =
    span
        [ style "display" "inline-block"
        , style "width" "20px"
        , style "height" "20px"
        , style "border-radius" "50%"
        , style "background-color" color
        , style "margin" "0 2px"
        ]
        []

-- STATIC DATA

allCategories : List String
allCategories =
    [ "Crossbody bag", "Backpack", "Duffel", "Messenger", "Luggage", "Tote bag", "Bucket Bag", "Cooler bag", "Pouches" ]

productList : List Product
productList =
    [ { id = 1, name = "Venture Ready Sling 2.5L", price = "$89", imageUrl = "public/tokyo-tote.jpg", category = "Crossbody bag", description = "2.5L / A rugged little sling", colors = ["black", "green", "orange"], badge = Just "Bestseller" }
    , { id = 2, name = "Lite Travel Pack 30L", price = "$199", imageUrl = "public/classic-backpack.jpg", category = "Backpack", description = "30L / Compact for carry-on", colors = ["black", "white", "red"], badge = Just "Bestseller" }
    , { id = 3, name = "Venture Ready Sling 2.5L", price = "$89", imageUrl = "public/transit-workpack.jpg", category = "Duffel", description = "2.5L / A rugged little sling", colors = ["black", "green", "orange"], badge = Just "Bestseller" }
    , { id = 4, name = "Lite Travel Pack 30L", price = "$199", imageUrl = "public/via-work-bag.jpg", category = "Messenger", description = "30L / Compact for carry-on", colors = ["black", "white", "red"], badge = Just "Bestseller" }
    , { id = 5, name = "Venture Ready Sling 2.5L", price = "$89", imageUrl = "public/tokyo-tote.jpg", category = "Luggage", description = "2.5L / A rugged little sling", colors = ["black", "green", "orange"], badge = Just "Bestseller" }
    , { id = 6, name = "Lite Travel Pack 30L", price = "$199", imageUrl = "public/transit-workpack.jpg", category = "Tote bag", description = "30L / Compact for carry-on", colors = ["black", "white", "red"], badge = Just "Bestseller" }
    , { id = 7, name = "Venture Ready Sling 2.5L", price = "$89", imageUrl = "public/tokyo-tote.jpg", category = "Bucket Bag", description = "2.5L / A rugged little sling", colors = ["black", "green", "orange"], badge = Just "Bestseller" }
    , { id = 8, name = "Lite Travel Pack 30L", price = "$199", imageUrl = "public/classic-backpack.jpg", category = "Cooler bag", description = "30L / Compact for carry-on", colors = ["black", "white", "red"], badge = Just "Bestseller" }
    , { id = 9, name = "Venture Ready Sling 2.5L", price = "$89", imageUrl = "public/tokyo-tote.jpg", category = "Pouches", description = "2.5L / A rugged little sling", colors = ["black", "green", "orange"], badge = Just "Bestseller" }
    , { id = 10, name = "Lite Travel Pack 30L", price = "$199", imageUrl = "public/via-work-bag.jpg", category = "Backpack", description = "30L / Compact for carry-on", colors = ["black", "white", "red"], badge = Just "Bestseller" }
    , { id = 11, name = "Venture Ready Sling 2.5L", price = "$89", imageUrl = "public/tokyo-tote.jpg", category = "Crossbody bag", description = "2.5L / A rugged little sling", colors = ["black", "green", "orange"], badge = Just "Bestseller" }
    , { id = 12, name = "Lite Travel Pack 30L", price = "$199", imageUrl = "public/classic-backpack.jpg", category = "Duffel", description = "30L / Compact for carry-on", colors = ["black", "white", "red"], badge = Just "Bestseller" }
    , { id = 13, name = "Transit Workpack 20L", price = "$179", imageUrl = "public/transit-workpack.jpg", category = "Backpack", description = "20L / For work and laptop", colors = ["black", "navy", "beige"], badge = Nothing }
    ]
    
-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
