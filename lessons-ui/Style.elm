module Style ( midStyle
             , topStyle
             , bottomStyle
             , sideStyle
             , contentStyle
             , containerStyle
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

sideStyle : Html.Attribute
sideStyle = style <| side []



contentStyle : Html.Attribute
contentStyle = style <| content []


containerStyle : Html.Attribute
containerStyle =
    style
        <| column
        <| Text.color (rgba 255 255 255 1 )
        <| Dimension.height 1000 []



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


side : Styles -> Styles
side styles =
  styles
    -- |> Dimension.height 50
    |> Background.color (rgba 52 152 219 1)
    |> centered
    |> Flex.grow 1


content : Styles -> Styles
content styles =
  styles
    -- |> Dimension.width 200
    -- |> Dimension.height 200
    |> Background.color (rgba 52 73 94 1)
    |> centered
    |> Flex.grow 3
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
