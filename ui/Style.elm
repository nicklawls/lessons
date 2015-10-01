module Style ( topStyle
             , bottomStyle
             , contentStyle
             , containerStyle
             , selectorStyle
             , buttonRowStyle
             , checkoutButtonStyle
             , confirmationBoxStyle
             , userInputStyle
             , inputStyle
             , buttonStyle
             , decreaseMargin
             , errorStyle
             ) where

import Html
import Html.Attributes exposing (id, style)
import Css exposing (Styles, setViewport)
import Css.Flex as Flex
import Css.Display as Display exposing (display)
import Css.Background as Background
import Css.Text as Text
import Css.Dimension as Dimension
import Css.Margin as Margin
import Color exposing (Color, rgba, complement)


topStyle : Html.Attribute
topStyle = style <| end 10 []


bottomStyle : Html.Attribute
bottomStyle = style <| end 5 []

inputStyle : Html.Attribute
inputStyle = style []

contentItem : Int -> Styles -> Styles
contentItem n styles =
    styles
        |> centered
        |> column
        |> Flex.grow n

checkoutButtonStyle : Html.Attribute
checkoutButtonStyle =
    style
        -- <| Background.color (rgba 98 64 204 1)
        <| contentItem 1 []

-- TODO: add visibility-hidden on bool arg, or equivalent
confirmationBoxStyle : Html.Attribute
confirmationBoxStyle =
    style
        <| contentItem 1 []

userInputStyle : Html.Attribute
userInputStyle =
    style
        -- <| Background.color (rgba 98 185 150 1)
        <| contentItem 1 []

buttonStyle : Html.Attribute
buttonStyle =
    style
        <| Dimension.width 60 []

decreaseMargin : Html.Attribute
decreaseMargin =
    style
        <| Margin.bottom 0
        <| Margin.top 0 []


buttonRowStyle : Html.Attribute
buttonRowStyle =
    style
        <| Flex.alignItems Flex.AICenter
        <| Flex.justifyContent Flex.JCBetween
        <| flex
        <| Dimension.width 200 []


selectorStyle : Html.Attribute
selectorStyle =
    style
        -- <| Background.color (rgba 98 185 204 1)
        <| contentItem 1 []


contentStyle : Html.Attribute
contentStyle = style <| content []


containerStyle : Html.Attribute
containerStyle =
    style
        <| column
        <| Text.color (rgba 215 216 219 1 )
        <| Dimension.height 900 []

errorStyle : Html.Attribute
errorStyle =
    style
        <| column
        <| centered []

flex : Styles -> Styles
flex styles =
  display Display.Flex styles


end : Int -> Styles -> Styles
end n styles =
  styles
    -- |> Dimension.height 50
    |> Background.color (rgba 39 73 107 1)
    |> centered
    |> Flex.grow n
    |> Flex.shrink 0


content : Styles -> Styles
content styles =
  styles
    |> Background.color (rgba 39 73 107 1)
    |> centered
    |> Flex.grow 80
    |> column
    |> Flex.alignItems Flex.AIStretch


column : Styles -> Styles
column styles =
  styles
    |> flex
    |> Flex.direction Flex.Column


centered : Styles -> Styles
centered styles =
  styles
    |> flex
    |> Flex.alignItems Flex.AICenter
    |> Flex.justifyContent Flex.JCCenter
