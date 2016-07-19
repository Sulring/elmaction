port module Textures exposing (..)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)


--import Math.Vector4 exposing (..)

import Math.Matrix4 exposing (..)
import Task
import Time exposing (Time)
import WebGL exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import AnimationFrame
import Debug exposing (log)
import Html.Attributes exposing (width, height)
import Window exposing (Size)
import Task exposing (Task, andThen, succeed)
import Dict
import Keyboard
import Mouse exposing (Position)
import Random
import String
import Json.Decode as Json exposing ((:=))
import Http
import Types exposing (..)

getAnimationDuration : String -> Int
getAnimationDuration s =
    let
        animation =
            Dict.get s animationDict
    in
        case animation of
            Nothing ->
                0

            Just x ->
                x.duration


animateSprite : Bool -> Float -> Maybe Vec3 -> String -> SpriteAnimation -> SpriteAnimation
animateSprite shooting dtf moves key anim =
    let
        new =
            if shooting then
                "fire"
            else
                case moves of
                    Nothing ->
                        "idle"

                    Just m ->
                        "move"

        dt =
            floor dtf

        a =
            anim
    in
        if anim.name == new || (anim.name == "fire" && anim.end < (anim.current + dt)) then
            { anim | current = (anim.current + dt) % anim.end }
        else
            { name = new
            , current = (anim.current + dt) % anim.end
            , end = getAnimationDuration (key ++ "-" ++ new)
            }


fetchTexture : ( String, String ) -> Task Error ( String, Texture )
fetchTexture ( name, texture ) =
    loadTexture texture
        `Task.andThen` \tex ->
                        Task.succeed ( name, tex )


fetchTextures : Task Error (List ( String, Texture ))
fetchTextures =
    Task.sequence
        (List.map fetchTexture
            [ ( "ground", "texture/ground512.png" )
            , ( "assault", "texture/acharacter.png" )
            , ( "grenadeCrate", "texture/grenadeCrate.png" )
            , ( "healthCrate", "texture/healthCrate.png" )
            , ( "slug", "texture/bullet.png" )
            , ( "rocket", "texture/rocket.png" )
            , ( "healthCrate", "texture/healthCrate.png" )
            , ( "rocketCrate", "texture/grenadeCrate.png" )
            , ( "explosion", "texture/explosion.png" )
            , ( "death", "texture/death.png" )
            , ( "clone", "texture/enemy.png")
            ]
        )


framedTextures : Dict.Dict String ( Int, Bool )
framedTextures =
    Dict.fromList
        [ ( "assault", ( 4, False ) )
        , ( "clone", ( 1, False ) )
        , ( "ground", ( 20, True ) )
        , ( "slug", ( 1, False ) )
        , ( "healthCrate", ( 1, False ) )
        , ( "rocketCrate", ( 1, False ) )
        , ( "rocket", ( 1, False ) )
        , ( "explosion", ( 16, False ) )
        , ( "death", ( 16, False ) )
        ]


animationDict : AnimationDictionary
animationDict =
    Dict.fromList
        [ ( "assault-idle", { duration = 100, frames = [ ( 1, 100 ) ] } )
        , ( "assault-move", { duration = 200, frames = [ ( 1, 100 ), ( 0, 100 ) ] } )
        , ( "assault-fire", { duration = 300, frames = [ ( 1, 100 ), ( 3, 50 ), ( 2, 100 ), ( 3, 50 ) ] } )
        , ( "clone-idle", { duration = 100, frames = [ ( 0, 100 ) ] } )
        , ( "clone-move", { duration = 200, frames = [ ( 0, 100 ), ( 0, 100 ) ] } )
        , ( "clone-fire", { duration = 400, frames = [ ( 0, 100 ), ( 0, 100 ), ( 0, 100 ), ( 0, 100 ) ] } )
        , ( "ground-idle", { duration = 100, frames = [ ( 0, 100 ) ] } )
        , ( "slug-idle", { duration = 100, frames = [ ( 0, 100 ) ] } )
        , ( "healthCrate-idle", { duration = 100, frames = [ ( 0, 100 ) ] } )
        , ( "rocketCrate-idle", { duration = 100, frames = [ ( 0, 100 ) ] } )
        , ( "rocket-idle", { duration = 100, frames = [ ( 0, 100 ) ] } )
        , ( "rocket-move", { duration = 100, frames = [ ( 0, 100 ) ] } )
        , ( "explosion-idle"
          , { duration = 800
            , frames =
                [ ( 0, 50 )
                , ( 1, 50 )
                , ( 2, 50 )
                , ( 3, 50 )
                , ( 4, 50 )
                , ( 5, 50 )
                , ( 6, 50 )
                , ( 7, 50 )
                , ( 8, 50 )
                , ( 9, 50 )
                , ( 10, 50 )
                , ( 11, 50 )
                , ( 12, 50 )
                , ( 13, 50 )
                , ( 14, 50 )
                , ( 15, 50 )
                ]
            }
          )
        , ( "death-idle"
          , { duration = 400
            , frames =
                [ ( 0, 25 )
                , ( 1, 25 )
                , ( 2, 25 )
                , ( 3, 25 )
                , ( 4, 25 )
                , ( 5, 25 )
                , ( 6, 25 )
                , ( 7, 25 )
                , ( 8, 25 )
                , ( 9, 25 )
                , ( 10, 25 )
                , ( 11, 25 )
                , ( 12, 25 )
                , ( 13, 25 )
                , ( 14, 25 )
                , ( 15, 25 )
                ]
            }
          )
        , ( "slug-move", { duration = 100, frames = [ ( 0, 100 ) ] } )
        ]
