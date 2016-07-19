port module Types exposing (..)

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


type Status
    = MainMenu
    | Credits
    | Options
    | Speed
    | Difficulty
    | EnterName
      -- Entering Player name
    | Highscore
      -- showing Highscores
    | WaitingForHighscore
    | Game
      -- Game is running
    | GameOver
      -- No Continues Left :)
    | Won
    | Help


type alias Textures =
    Dict.Dict String Texture


type alias Model =
    { textures :
        Textures
        -- loaded textures
    , wsize :
        Size
        -- screen size
    , mousePosition : Position
    , keys :
        Keys
        -- currently pressed keys
    , lookAt :
        Mat4
        -- matrix which holds rotation of player character towards mouse cursor
    , status :
        Status
        -- game status
    , actorManager :
        ActorManager
        -- objects/characters/decals/collisions
    , last_i : Int
    , counter : Float
    , gameSpeed : Float
    , gameDifficulty : Bool
    , playerName : String
    , scoreList : Scores
    , score : Int
    , waiting : Float
    , startCounter : Float
    , waveCounter : Int
    , enemyCounter : Int
    }


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , minus : Bool
    , plus : Bool
    , shift : Bool
    }


type alias SpriteAnimation =
    { name :
        String
        -- subanimation title
    , current :
        Int
        -- current timing
    , end :
        Int
        -- duration of current animation
    }


type alias SpriteFrames =
    { duration : Int
    , frames : List ( Int, Int )
    }


type alias AnimationDictionary =
    Dict.Dict String SpriteFrames


type OnActorKilled
    = OnActorKilled
        { onActorKilled : Actor -> Cmd Msg
        }


type OnAction
    = OnAction
        { func : Actor -> ( Actor, Maybe (Cmd Msg) )
        }


type Msg
    = TexturesError Error
    | SoundError Error
    | StatusError Error
    | TexturesLoaded (List ( String, Texture ))
      -- loading texture and saving it to the model
    | WindowSizeError Error
    | WindowSizeSuccess Size
      -- resizing WebGL canvas when loaded or window resized
    | Animate Time
      -- main loop
    | UpdateMouse Position
      -- saving new mouse position
    | KeyChange ( Keys -> Keys, Maybe Bool )
      -- marking pressed button
    | PlayMusic String
      -- play music
    | PlaySound String
      -- play sound clip
    | Fire ( Vec3, Mat4, Actor )
    | MouseClicks Position
    | ChangeStatus Status
    | GetRandomDirectionVector Actor
    | RandomDirectionVector ( Actor, Vec3 )
    | GetRandomFireRate
    | RandomFireRate (List Int)
    | AddScore Int
    | SetGameSpeed Float
    | SetGameDifficulty Bool
    | ExitButton
    | EnterButton
    | SetName String
    | FetchSucceed Scores
    | FetchScore
    | GenerateDropCrate ( Int, Vec3 )
    | Explosion Vec3
    | GetRandomEnemyPositions (List Int)
    | RandomEnemyPositions (List Int) (List ( Float, Float ))


type Collision
    = Collision
        { blocking :
            Bool
            -- collision type. True = Blocking / False = Overlapping
        , effectOnTarget :
            Actor -> Actor
            -- effects on collision for the target Actor
        , effectOnSelf :
            Actor -> Actor
            -- effects on collision for the source Actor
        }


type alias ScoreItem =
    ( String, Int )


type alias Scores =
    List ScoreItem


type ActorType
    = Player
      -- Player character controlled by Input. Highlander Format
    | NPC
      -- Other characters (players or AI)
    | Object



-- Decals, Crates and other things.
--    | Camera                                          -- maybe TODO: Camera


type ActorSubType
    = Bullet
      -- Bullets/Rockets
    | Collectable
      -- Crates
    | Obstacle



-- Blockable obstacles


type alias CharacterAttributes =
    { health : Int
    , rockets : Int
    , score : Int
    , rateOfFire : Int
    , timeSinceLastFire : Int
    }



-- type Children = Children ( List Actor )              -- maybe TODO: Children Actors


type alias Actor =
    { key : String
    , actorType : ActorType
    , actorSubType : Maybe ActorSubType
    , characterAttributes :
        Maybe CharacterAttributes
        -- only Players and NPC have Attributes
        -- , children : Children                            -- maybe TODO: possible Children Actors
    , index :
        Int
        -- Render priority (for alpha textures)
    , size :
        Float
        -- size of Actor
    , speed : Float
    , texture :
        String
        -- texture
    , randomVal : Int
    , position :
        Vec3
        -- Actor position
    , renderPosition :
        Vec3
        -- Actor start position
    , spriteCentering : Vec3
    , rotation :
        Mat4
        -- Actor rotation
    , worldTransformationMatrix : Mat4
    , movesTo :
        Maybe Vec3
        -- AI character waypoints / multiplayer waypoints
    , lastChange : Float
    , moves :
        Bool
        -- Actor moves (see rotation for direction)
    , timeToLive :
        Maybe Int
        -- for temporary objects like decals. Nothing: lives forever. Just x: dies in x milliseconds
    , collision :
        Maybe Collision
        -- type of collisions of the Actor.
    , animation :
        SpriteAnimation
        -- Actor animations
    , affectsCamera :
        Maybe Mat4
        -- Player only : Camera transformation
    , onDeath : Maybe OnAction
    , onDestination : Maybe OnAction
    , onActorKilled :
        Maybe OnActorKilled
        -- fires Msg when killed
    , fireCommand :
        Maybe (Cmd Msg)
        -- fires Msg
    }

type alias ActorManager =
    Dict.Dict String Actor
