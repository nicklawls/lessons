module Style ( midStyle
             , topStyle
             , bottomStyle
             , contentStyle
             , containerStyle
             , selectorStyle
             , buttonRowStyle
             , checkoutButtonStyle
             , confirmationBoxStyle
             ) where

import Html
import Html.Attributes exposing (id, style)
import Css exposing (Styles, setViewport)
import Css.Flex as Flex
import Css.Display as Display exposing (display)
import Css.Shadow as Shadow
import Css.Background as Background
import Css.Text as Text
import Css.Font as Font
import Css.Padding as Padding
import Css.Dimension as Dimension
import Css.Margin as Margin
import Color exposing (Color, rgba, complement)


topStyle : Html.Attribute
topStyle = style <| end 10 []

midStyle : Html.Attribute
midStyle =
    style
        <| Flex.grow 80
        <| flex []

bottomStyle : Html.Attribute
bottomStyle = style <| end 5 []


contentItem : Int -> Styles -> Styles
contentItem n styles =
    styles
        |> centered
        |> column
        |> Flex.grow n

checkoutButtonStyle : Html.Attribute
checkoutButtonStyle =
    style
        <| contentItem 1 []

-- TODO: add visibility-hidden on bool arg, or equivalent
confirmationBoxStyle : Html.Attribute
confirmationBoxStyle =
    style
        <| contentItem 1 []


buttonRowStyle : Html.Attribute
buttonRowStyle = style []

selectorStyle : Html.Attribute
selectorStyle =
    style
        -- <| column moved it into content item
        <| contentItem 1 []


contentStyle : Html.Attribute
contentStyle = style <| content []


containerStyle : Html.Attribute
containerStyle =
    style
        <| column
        <| Text.color (rgba 255 255 255 1 )
        <| Dimension.height 900 []



flex : Styles -> Styles
flex styles =
  display Display.Flex styles


end : Int -> Styles -> Styles
end n styles =
  styles
    -- |> Dimension.height 50
    |> Background.color (rgba 26 188 156 1)
    |> centered
    |> Flex.grow n
    |> Flex.shrink 0


content : Styles -> Styles
content styles =
  styles
    -- |> Dimension.width 200
    -- |> Dimension.height 200
    |> Background.color (rgba 52 73 94 1)
    |> centered
    |> Flex.grow 1
    |> column


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
