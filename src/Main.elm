module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Color
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events.Extra.Mouse as Mouse
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random as Random
import Task as Task
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (..)



-- CONFIG


minDistance : Float
minDistance =
    150


qtyParticlesMin : Int
qtyParticlesMin =
    50


logoUrl : String
logoUrl =
    "assets/img/logo.png"


mapUrl : String
mapUrl =
    "assets/img/map.png"



-- TYPES


type alias Window =
    { width : Float
    , height : Float
    }


type alias Position =
    Vec2


type alias Direction =
    Vec2


type alias Radius =
    Float


type Particle
    = Particle Position Direction Radius


type alias Model =
    { particles : List Particle
    , window : Window
    , logo : Maybe Texture
    , map : Maybe Texture
    }


type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        width =
            flags.windowWidth

        height =
            flags.windowHeight
    in
    ( { particles = []
      , window =
            { width = flags.windowWidth
            , height = flags.windowHeight
            }
      , logo = Nothing
      , map = Nothing
      }
    , Cmd.batch
        [ Task.perform GotViewport getViewport
        , Random.generate Populate <|
            Random.list qtyParticlesMin (tupleInitGenerator width height)
        ]
    )



-- UPDATE


type Msg
    = Frame Float
    | GotViewport Viewport
    | WindowResized
    | Populate (List ( Position, Direction, Radius ))
    | CanvasClick ( Float, Float )
    | LogoLoaded (Maybe Texture)
    | MapLoaded (Maybe Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame delta ->
            let
                newParticles =
                    processParticles model.window delta model.particles

                qty =
                    List.length newParticles

                cmdGenerateParticle =
                    if qty >= qtyParticlesMin then
                        Cmd.none

                    else
                        Random.generate Populate <|
                            Random.list (qtyParticlesMin - qty)
                                (tupleGenerator
                                    model.window.width
                                    model.window.height
                                )
            in
            ( { model | particles = newParticles }
            , cmdGenerateParticle
            )

        GotViewport viewport ->
            ( { model
                | window =
                    { width = viewport.viewport.width
                    , height = viewport.viewport.height
                    }
              }
            , Cmd.none
            )

        Populate listTuples ->
            ( { model
                | particles =
                    List.append model.particles <|
                        List.map createFromTuple listTuples
              }
            , Cmd.none
            )

        WindowResized ->
            ( model, Task.perform GotViewport getViewport )

        CanvasClick ( x, y ) ->
            ( model, Random.generate Populate <| Random.list 3 <| tupleGeneratorAt x y )

        LogoLoaded maybeLogo ->
            ( { model | logo = maybeLogo }, Cmd.none )

        MapLoaded maybeMap ->
            ( { model | map = maybeMap }, Cmd.none )


processParticles : Window -> Float -> List Particle -> List Particle
processParticles window delta =
    List.map (updatePosition delta) << filterOutOffscreenParticles window


filterOutOffscreenParticles : Window -> List Particle -> List Particle
filterOutOffscreenParticles { height, width } =
    let
        filterOut (Particle pos _ radius) =
            not <|
                (getX pos - radius - minDistance)
                    > width
                    || (getX pos + radius + minDistance)
                    < 0
                    || (getY pos - radius - minDistance)
                    > height
                    || (getY pos + radius + minDistance)
                    < 0
    in
    List.filter filterOut


updatePosition : Float -> Particle -> Particle
updatePosition delta (Particle pos dir radius) =
    let
        -- Pixels per sec
        velPerSec =
            100

        deltaPos =
            normalize dir |> scale ((velPerSec / 1000) * delta)
    in
    Particle (add pos deltaPos) dir radius


createFromTuple : ( Position, Direction, Radius ) -> Particle
createFromTuple ( position, direction, radius ) =
    Particle position direction radius



-- VIEW


view : Model -> Html Msg
view model =
    let
        width =
            model.window.width

        height =
            model.window.height
    in
    WebGL.toHtml
        [ Mouse.onDown (\event -> CanvasClick event.offsetPos)
        , Attrs.width <| round width
        , Attrs.height <| round height
        ]
    <|
        List.map (drawParticle model.window) model.particles



{- ++ drawConnections model.particles
   ++ viewMap model.window model.map
   ++ List.map drawParticle model.particles
   ++ drawLogo model.window model.logo
-}
{-
   viewMap : Window -> Maybe Texture -> List Renderable
   viewMap window maybeMap =
       case maybeMap of
           Nothing ->
               []

           Just map ->
               let
                   { width, height } =
                       dimensions map

                   widthScale =
                       min 1 (window.width / width)

                   heightScale =
                       (window.height / height) * widthScale

                   newHeight =
                       height * heightScale
               in
               [ texture
                   [ Advanced.transform
                       [ Advanced.scale
                           widthScale
                           heightScale
                       ]
                   ]
                   ( 0, ((window.height / 2) - (newHeight / 2)) / heightScale )
                   map
               ]
-}
{-
   drawLogo : Window -> Maybe Texture -> List Renderable
   drawLogo window maybeLogo =
       case maybeLogo of
           Nothing ->
               []

           Just logo ->
               let
                   { width, height } =
                       dimensions logo

                   aspectRatio =
                       width / height

                   widthScale =
                       min 1 (window.width / width)

                   heightScale =
                       (widthScale * width) / (aspectRatio * height)

                   newWidth =
                       width * widthScale
               in
               [ texture
                   [ Advanced.transform
                       [ Advanced.scale
                           widthScale
                           heightScale
                       ]
                   ]
                   ( ((window.width / 2) - (newWidth / 2)) / widthScale
                   , ((window.height / 2) - (height / 2)) / heightScale
                   )
                   logo
               ]

-}


colorParticle : Vec3
colorParticle =
    -- vec3 178 178 178
    vec3 0 0 0


modifyAlpha : Float -> Color.Color -> Color.Color
modifyAlpha alpha =
    Color.fromRgba << (\r -> { r | alpha = alpha }) << Color.toRgba


drawParticle : Window -> Particle -> Entity
drawParticle window (Particle pos _ radius) =
    WebGL.entity
        vertexShader
        fragmentShader
        particleMesh
        (uniforms window pos radius)


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    }


uniforms : Window -> Position -> Float -> Uniforms
uniforms window pos radius =
    let
        x =
            getX pos

        y =
            getY pos
    in
    { perspective =
        Mat4.makePerspective
            90
            (window.width / window.height)
            -1
            1
    , camera =
        Mat4.makeLookAt
            (vec3 x y (100 * radius))
            (vec3 x y 0)
            (vec3 0 1 0)
    }



-- Mesh


type alias Attribute =
    { color : Vec3
    , position : Vec3
    }


particleMesh : Mesh Attribute
particleMesh =
    let
        qtyTriangles =
            15

        baseRad =
            (2 * pi) / qtyTriangles

        vertex position =
            Attribute (Vec3.scale (1 / 255) colorParticle) position

        go n =
            let
                rotation =
                    Mat4.makeRotate
                        (toFloat n * baseRad)
                        (vec3 0 0 1)
            in
            case n of
                0 ->
                    []

                _ ->
                    ( vertex (vec3 0 0 0)
                    , vertex (Mat4.transform rotation (vec3 0 1 0))
                    , vertex (Mat4.transform rotation (vec3 (sin baseRad) (cos baseRad) 0))
                    )
                        :: go (n - 1)
    in
    go qtyTriangles
        |> WebGL.triangles



{- shapes [ fill colorParticle ]
   [ circle ( getX pos, getY pos ) radius
   ]
-}
{-
   drawConnections : List Particle -> List Renderable
   drawConnections particles =
       let
           drawPath from to =
               path ( getX from, getY from )
                   [ lineTo ( getX to, getY to )
                   ]

           drawConnection from to =
               shapes
                   [ stroke <|
                       modifyAlpha (1 - distance from to / minDistance) colorParticle
                   , lineWidth 1
                   ]
                   [ drawPath from to ]

           folder (Particle pos _ _) acc =
               acc
                   ++ (List.filter
                           (\(Particle pos2 _ _) -> distance pos pos2 < minDistance)
                           particles
                           |> List.map (\(Particle pos2 _ _) -> drawConnection pos pos2)
                      )
       in
       List.foldr folder [] particles
-}
-- Shaders


vertexShader : Shader Attribute Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onResize (\_ _ -> WindowResized)
        ]



-- MISC


vec2FromTuple : ( Float, Float ) -> Vec2
vec2FromTuple ( x, y ) =
    vec2 x y


genRadius : Random.Generator Radius
genRadius =
    Random.float 1 3


tupleInitGenerator : Float -> Float -> Random.Generator ( Position, Direction, Radius )
tupleInitGenerator width height =
    let
        genX =
            Random.float 0 width

        genY =
            Random.float 0 height

        genPos =
            Random.map vec2FromTuple <| Random.pair genX genY

        genSingleDir =
            Random.float -1 1

        genDir =
            Random.map vec2FromTuple <| Random.pair genSingleDir genSingleDir
    in
    randomPair3 genPos genDir genRadius


tupleGenerator : Float -> Float -> Random.Generator ( Position, Direction, Radius )
tupleGenerator width height =
    let
        borderPosGen =
            Random.uniform
                (Random.pair
                    (Random.uniform 0 [ width ])
                    (Random.float 0 height)
                )
                [ Random.pair (Random.float 0 width) (Random.uniform 0 [ height ]) ]

        genGenPos =
            (Random.map << Random.map) vec2FromTuple <| borderPosGen

        genSingleDir =
            Random.float -1 1

        genDir =
            Random.map vec2FromTuple <| Random.pair genSingleDir genSingleDir
    in
    genGenPos
        |> Random.andThen (\genPos -> randomPair3 genPos genDir genRadius)


tupleGeneratorAt : Float -> Float -> Random.Generator ( Position, Direction, Radius )
tupleGeneratorAt x y =
    let
        genX =
            Random.constant x

        genY =
            Random.constant y

        genPos =
            Random.map vec2FromTuple <| Random.pair genX genY

        genSingleDir =
            Random.float -1 1

        genDir =
            Random.map vec2FromTuple <| Random.pair genSingleDir genSingleDir
    in
    randomPair3 genPos genDir genRadius


randomPair3 :
    Random.Generator a
    -> Random.Generator b
    -> Random.Generator c
    -> Random.Generator ( a, b, c )
randomPair3 genA genB genC =
    Random.map3 (\a b c -> ( a, b, c )) genA genB genC



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
