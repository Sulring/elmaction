port module Math2d exposing (..)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Mouse exposing (Position)
import Window exposing (Size)

--import Math.Vector4 exposing (..)

import Math.Matrix4 exposing (..)
import Types exposing (..)

vlength : Vec3 -> Float
vlength v =
    Basics.sqrt ((Math.Vector3.getX v) ^ 2 + (Math.Vector3.getY v) ^ 2)


vnormalize : Vec3 -> Vec3
vnormalize v =
    let
        len =
            vlength v
    in
        vec3 ((Math.Vector3.getX v) / len) ((Math.Vector3.getY v) / len) (Math.Vector3.getZ v)


vscale : Float -> Vec3 -> Vec3
vscale f v =
    vec3 (f * (Math.Vector3.getX v)) (f * (Math.Vector3.getY v)) (Math.Vector3.getZ v)


vadd : Vec3 -> Vec3 -> Vec3
vadd v w =
    vec3 ((Math.Vector3.getX v) + (Math.Vector3.getX w)) ((Math.Vector3.getY v) + (Math.Vector3.getY w)) (Math.Vector3.getZ v)


vsub : Vec3 -> Vec3 -> Vec3
vsub v w =
    vec3 ((Math.Vector3.getX v) - (Math.Vector3.getX w)) ((Math.Vector3.getY v) - (Math.Vector3.getY w)) (Math.Vector3.getZ v)


cl : Float -> Float -> Float -> Float
cl a x b =
    Basics.min (Basics.max a x) b

angleBetween : Vec3 -> Vec3 -> Float
angleBetween d v =
    atan2 ((getX d) * (getY v) - (getX v) * (getY d)) ((getX d) * (getX v) + (getY d) * (getY v))


makeRotateBetween : Vec3 -> Vec3 -> Mat4
makeRotateBetween v1 v2 =
    let
        d =
            normalize v1

        v =
            normalize v2

        angle =
            angleBetween d v
    in
        makeRotate angle (vec3 0 0 1)


calculateDirection : Position -> Size -> Mat4
calculateDirection pos wsize =
    let
        center =
            vec3 (toFloat (wsize.width) / 2.0) (toFloat (wsize.height) / 2.0) 0

        target =
            vec3 (toFloat (pos.x)) (toFloat (pos.y)) 0

        d =
            normalize (Math.Vector3.sub target center)

        v =
            vec3 0 1 0
    in
        makeRotateBetween d v


makeWorldTranslation : Float -> Vec3 -> Float -> { x : Int, y : Int } -> Mat4 -> Mat4
makeWorldTranslation gameSpeed pos dt { x, y } lookAt =
    let
        d =
            vec3 (toFloat x) (toFloat y) 0

        dir =
            if length d > 1 then
                normalize d
            else
                d
    in
        makeTranslate (add pos (Math.Vector3.scale ((gameSpeed * dt) / 10000) (transform lookAt dir)))

directions : Keys -> { x : Int, y : Int }
directions { left, right, up, down } =
    let
        direction a b =
            case ( a, b ) of
                ( True, False ) ->
                    1

                ( False, True ) ->
                    -1

                _ ->
                    0
    in
        { x = direction left right
        , y = direction down up
        }
