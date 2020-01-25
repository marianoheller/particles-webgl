module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events.Extra.Mouse as Mouse
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random as Random
import Task as Task
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings
import WebGL.Settings.Blend
import WebGL.Texture exposing (..)



-- CONFIG


minDistance : Float
minDistance =
    150


qtyParticlesMin : Int
qtyParticlesMin =
    50


radiusMin : Float
radiusMin =
    2


radiusMax : Float
radiusMax =
    4


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
        , Task.attempt LogoLoaded <| WebGL.Texture.loadWith nonPowerOfTwoOptions logoUrl
        , Task.attempt MapLoaded <| WebGL.Texture.loadWith nonPowerOfTwoOptions mapUrl
        ]
    )



-- UPDATE


type Msg
    = Frame Float
    | GotViewport Viewport
    | WindowResized
    | Populate (List ( Position, Direction, Radius ))
    | CanvasClick ( Float, Float )
    | LogoLoaded (Result Error Texture)
    | MapLoaded (Result Error Texture)


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

        LogoLoaded (Ok logo) ->
            ( { model | logo = Just logo }, Cmd.none )

        LogoLoaded (Err _) ->
            ( model, Cmd.none )

        MapLoaded (Ok map) ->
            ( { model | map = Just map }, Cmd.none )
        MapLoaded (Err _) ->
            ( model, Cmd.none )


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

        invertY y =
            model.window.height - y
    in
    WebGL.toHtmlWith
        [ WebGL.clearColor 0.4 0.4 0.4 1
        , WebGL.alpha False
        , WebGL.antialias
        ]
        [ Mouse.onDown (\event -> CanvasClick <| Tuple.mapSecond invertY event.offsetPos)
        , Attrs.width <| round width
        , Attrs.height <| round height
        ]
    <|
        (drawConnections model.window model.particles
            ++ List.map (drawParticle model.window) model.particles
        )



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
    vec3 178 178 178


drawParticle : Window -> Particle -> Entity
drawParticle window (Particle pos _ radius) =
    WebGL.entity
        vertexShader
        fragmentShader
        particleMesh
        (particleUniforms window pos radius)


drawConnections : Window -> List Particle -> List Entity
drawConnections window particles =
    let
        drawConnection from to =
            WebGL.entity
                vertexShader
                fragmentShader
                connectionMesh
                (connectionUniforms window ( from, to ))

        folder (Particle pos _ _) acc =
            acc
                ++ (List.filter
                        (\(Particle pos2 _ _) -> distance pos pos2 < minDistance)
                        particles
                        |> List.map (\(Particle pos2 _ _) -> drawConnection pos pos2)
                   )
    in
    List.foldr folder
        []
        particles


type alias Uniforms =
    { projection : Mat4
    , camera : Mat4
    , model : Mat4
    , scale : Mat4
    , rotate : Mat4
    , intensity : Float
    }


particleUniforms : Window -> Position -> Float -> Uniforms
particleUniforms window pos radius =
    { projection =
        Mat4.makeOrtho2D
            0
            window.width
            0
            window.height
    , camera =
        Mat4.makeLookAt
            (vec3 0 0 0.99)
            (vec3 0 0 0)
            (vec3 0 1 0)
    , model =
        Mat4.makeTranslate3 (getX pos) (getY pos) 0
    , scale = Mat4.makeScale3 radius radius 0
    , rotate = Mat4.makeRotate 0 (vec3 0 0 1)
    , intensity = 1
    }


connectionUniforms : Window -> ( Position, Position ) -> Uniforms
connectionUniforms window ( from, to ) =
    let
        diff =
            sub to from

        phi =
            atan2 (getY diff) (getX diff)
    in
    { projection =
        Mat4.makeOrtho2D
            0
            window.width
            0
            window.height
    , camera =
        Mat4.makeLookAt
            (vec3 0 0 0.99)
            (vec3 0 0 0)
            (vec3 0 1 0)
    , model =
        Mat4.makeTranslate3 (getX from) (getY from) 0
    , scale = Mat4.makeScale3 (length diff) 0.3 0
    , rotate = Mat4.makeRotate phi (vec3 0 0 1)
    , intensity = (minDistance - distance from to) / minDistance
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


connectionMesh : Mesh Attribute
connectionMesh =
    let
        vertex position =
            Attribute (Vec3.scale (1 / 255) colorParticle) position
    in
    [ ( vertex (vec3 0 0 0)
      , vertex (vec3 0 1 0)
      , vertex (vec3 1 0 0)
      )
    , ( vertex (vec3 1 1 0)
      , vertex (vec3 0 1 0)
      , vertex (vec3 1 0 0)
      )
    ]
        |> WebGL.triangles



-- Shaders


vertexShader : Shader Attribute Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 projection;
        uniform mat4 camera;
        uniform mat4 model;
        uniform mat4 scale;
        uniform mat4 rotate;
        uniform float intensity;

        varying vec3 vcolor;

        void main () {
            gl_Position =  projection * camera * model * rotate * scale * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 vcolor;
        uniform float intensity;
        
        void main () {
            gl_FragColor =  vec4(vcolor, intensity);
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
    Random.float radiusMin radiusMax


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
