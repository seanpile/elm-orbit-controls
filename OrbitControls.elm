module OrbitControls exposing (OrbitEvent, State, Options, defaultState, defaultOptions, listeners, apply, applyWithOptions)

{-| OrbitControls allows you to rotate and zoom around a target in 3D.


# Definition

@docs OrbitEvent, State, Options


# Initialization

@docs defaultState, defaultOptions


# Listeners

@docs listeners


# Updating

@docs apply, applyWithOptions

-}

import Html
import Html.Events as Events
import Json.Decode as Decode
import Math.Matrix4 as Matrix4
import Math.Vector2 as Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3)
import Math.Quaternion as Quaternion exposing (Quat)


{-| Defines the bridge type that will allow the main program to communicate with the OrbitControls. These events are emitted
by OrbitControls and then handled by the main program.
-}
type OrbitEvent
    = InternalEvent { event : OrbitEventType }


type OrbitEventType
    = Zoom Int
    | Rotate MouseEvent
    | MouseDown MouseEvent
    | MouseUp MouseEvent


type alias WheelEvent =
    { deltaY : Int
    , deltaX : Int
    }


type alias MouseEvent =
    { clientX : Float
    , clientY : Float
    , width : Float
    , height : Float
    }


{-| The state that needs to be maintained by OrbitControls. This is intentionally opaque to the user
-}
type State
    = InternalState { isMouseDown : Bool, mouse : Vec2 }


{-| The available options that can be used to customize the controls.
-}
type alias Options =
    { minDistance : Float
    , maxDistance : Float
    , minPolarAngle : Float
    , maxPolarAngle : Float
    , minAzimuthAngle : Float
    , maxAzimuthAngle : Float
    , rotateSpeed : Float
    , zoomSpeed : Float
    , up : Vec3
    , target : Vec3
    }



-- Constants


{-| Initialize the OrbitControl state that you will need to pass to OrbitControls
-}
defaultState : State
defaultState =
    InternalState
        { isMouseDown = False
        , mouse = Vector2.vec2 0 0
        }


{-| The set of options with mostly sensible defaults. To override:

    defaults =
        OrbitControls.defaultOptions

    options =
        { defaults
            | minDistance = 1
            , maxDistance = 10
            , ...
        }

-}
defaultOptions : Options
defaultOptions =
    let
        bigNumber : Float
        bigNumber =
            1.0e64
    in
        { minDistance = 0
        , maxDistance = bigNumber
        , minPolarAngle = 0
        , maxPolarAngle = pi
        , minAzimuthAngle = -bigNumber
        , maxAzimuthAngle = bigNumber
        , rotateSpeed = 1.0
        , zoomSpeed = 1.0
        , up = Vector3.vec3 0 0 1
        , target = Vector3.vec3 0 0 0
        }



-- Methods


{-| The list of listeners that should be included in the attributes of the HTML element you are
monitoring. For example:

    type Msg =
        OnOrbit OrbitEvent
        | ...

    state = OrbitControls.defaultState

    div [ listeners state OnOrbit ]
        [ WebGL.toHtml ... ]

-}
listeners : State -> (OrbitEvent -> msg) -> List (Html.Attribute msg)
listeners state message =
    case state of
        InternalState { isMouseDown } ->
            let
                mouseEventDecoder : (MouseEvent -> OrbitEventType) -> Decode.Decoder msg
                mouseEventDecoder msg =
                    (Decode.map
                        (\evt -> message (InternalEvent { event = msg evt }))
                        (Decode.map4 MouseEvent
                            (Decode.field "clientX" Decode.float)
                            (Decode.field "clientY" Decode.float)
                            (Decode.at [ "target", "width" ] Decode.float)
                            (Decode.at [ "target", "height" ] Decode.float)
                        )
                    )
            in
                List.append
                    [ Events.on
                        "wheel"
                        (Decode.map
                            (\event -> message (InternalEvent { event = Zoom event.deltaY }))
                            (Decode.map2 WheelEvent
                                (Decode.field "deltaY" Decode.int)
                                (Decode.field "deltaX" Decode.int)
                            )
                        )
                    ]
                    (if isMouseDown then
                        [ Events.on "mousemove" (mouseEventDecoder Rotate)
                        , Events.on "mouseup" (mouseEventDecoder MouseUp)
                        ]
                     else
                        [ Events.on "mousedown" (mouseEventDecoder MouseDown)
                        ]
                    )


{-| Apply the orbit event to the given position and state. This is a shortcut for `applyWithOptions` using `defaultOptions`
-}
apply : OrbitEvent -> ( Vec3, State ) -> ( Vec3, State )
apply =
    applyWithOptions defaultOptions


{-| Apply the orbit event to the given position and state. This function applies the orbiting controls that are triggered
by an underlying event.

    type Msg =
        OnOrbit OrbitEvent
        | ...

    type alias Model = {
        state: OrbitControls.State
    }

    options = OrbitControls.defaultOptions

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            OnOrbit event ->
                let
                    -- Client logic
                    currentPosition = ...

                    (updatedPosition, updatedState) =
                        OrbitControls.applyWithOptions
                            options
                            event
                            (currentPosition, model.state)

                    -- Process the new position
                    ...
                in
                    -- Keep track of the new state
                    ( { model | state = updatedState })

-}
applyWithOptions : Options -> OrbitEvent -> ( Vec3, State ) -> ( Vec3, State )
applyWithOptions options internalEvent ( position, internalState ) =
    case ( internalEvent, internalState ) of
        ( InternalEvent event, InternalState state ) ->
            case event.event of
                Zoom dy ->
                    let
                        zoomFactor =
                            0.98 ^ (options.zoomSpeed + (logBase e (1.0 + toFloat (abs dy))))

                        radius =
                            Vector3.length position

                        scaleFactor =
                            if dy > 0 then
                                min options.maxDistance (radius / zoomFactor)
                            else
                                max options.minDistance (radius * zoomFactor)
                    in
                        ( Vector3.scale (scaleFactor / radius) position, InternalState state )

                Rotate event ->
                    let
                        up =
                            options.up

                        upQuat =
                            quatFromUnitVec3 up (Vector3.vec3 0 1 0)

                        upQuatInverse =
                            Quaternion.normalize (Quaternion.conjugate upQuat)

                        offset =
                            Matrix4.transform (Quaternion.toMatrix upQuat) (Vector3.sub position options.target)

                        offsetX =
                            Vector3.getX offset

                        offsetY =
                            Vector3.getY offset

                        offsetZ =
                            Vector3.getZ offset

                        deltaX =
                            2 * pi * ((Vector2.getX state.mouse) - event.clientX) / event.width * options.rotateSpeed

                        deltaY =
                            2 * pi * ((Vector2.getY state.mouse) - event.clientY) / event.height * options.rotateSpeed

                        epsilon =
                            2.0e-23

                        radius =
                            Vector3.length position

                        theta =
                            clamp
                                options.minAzimuthAngle
                                options.maxAzimuthAngle
                                ((atan2 offsetX offsetZ) + deltaX)

                        phi =
                            clamp
                                epsilon
                                (pi - epsilon)
                                (clamp
                                    options.minPolarAngle
                                    options.maxPolarAngle
                                    ((atan2 (sqrt (offsetX ^ 2 + offsetZ ^ 2)) offsetY) + deltaY)
                                )

                        coords =
                            Matrix4.transform (Quaternion.toMatrix upQuatInverse)
                                (Vector3.vec3
                                    (radius * sin (phi) * sin (theta))
                                    (radius * cos (phi))
                                    (radius * sin (phi) * cos (theta))
                                )
                    in
                        ( Vector3.add options.target coords
                        , InternalState
                            { state
                                | mouse = Vector2.vec2 event.clientX event.clientY
                            }
                        )

                -- Keep track of whether the mouse is depressed (drag and drop)
                MouseDown event ->
                    ( position, InternalState { state | isMouseDown = True, mouse = Vector2.vec2 event.clientX event.clientY } )

                -- Keep track of whether the mouse button is released (drag and drop)
                MouseUp event ->
                    ( position, InternalState { state | isMouseDown = False, mouse = Vector2.vec2 event.clientX event.clientY } )


{-| Create a quaternion given two vectors. By definition, these vectors must be normalized
-}
quatFromUnitVec3 : Vec3 -> Vec3 -> Quat
quatFromUnitVec3 u v =
    let
        w =
            Vector3.cross u v

        q =
            Quaternion.quat (Vector3.getX w) (Vector3.getY w) (Vector3.getZ w) (1.0 + Vector3.dot u v)
    in
        Quaternion.normalize q
