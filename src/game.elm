port module Game exposing (..)

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
    | EnterName                                          -- Entering Player name
    | Highscore                                          -- showing Highscores
    | WaitingForHighscore
    | Game                                               -- Game is running
    | GameOver                                           -- No Continues Left :)
    | Won
    | Help

type alias Textures =
    Dict.Dict String Texture

type alias Model =
    { textures : Textures                               -- loaded textures
    , wsize : Size                                      -- screen size
    , mousePosition : Position
    , keys : Keys                                       -- currently pressed keys
    , lookAt : Mat4                                     -- matrix which holds rotation of player character towards mouse cursor
    , status : Status                                   -- game status
    , actorManager : ActorManager                       -- objects/characters/decals/collisions
    , last_i : Int
    , counter : Float
    , gameSpeed : Float
    , gameDifficulty : Bool
    , playerName : String
    , scoreList : Scores
    , score : Int
    , waiting : Float
    , startCounter : Float
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
    { name: String                                      -- subanimation title
    , current: Int                                      -- current timing
    , end: Int                                          -- duration of current animation
    }

type alias SpriteFrames =
    { duration: Int
    , frames: List (Int, Int)
    }
type alias AnimationDictionary =
    Dict.Dict String SpriteFrames

type OnActorKilled = OnActorKilled
    {  onActorKilled : Actor -> Cmd Msg
    }
type OnAction = OnAction
    { func : Actor -> (Actor, Maybe (Cmd Msg))
    }

type Msg
    = TexturesError Error
    | SoundError Error
    | StatusError Error
    | TexturesLoaded (List ( String, Texture ))         -- loading texture and saving it to the model
    | WindowSizeError Error
    | WindowSizeSuccess Size                            -- resizing WebGL canvas when loaded or window resized
    | Animate Time                                      -- main loop
    | UpdateMouse Position                              -- saving new mouse position
    | KeyChange ((Keys -> Keys),(Maybe Bool))           -- marking pressed button
    | PlayMusic String                                  -- play music
    | PlaySound String                                  -- play sound clip
    | Fire (Vec3,Mat4,Actor)
    | MouseClicks Position
    | ChangeStatus Status
    | GetRandomDirectionVector Actor
    | RandomDirectionVector (Actor,Vec3)
    | GetRandomFireRate Int
    | RandomFireRate (List Int)
    | AddScore Int
    | SetGameSpeed Float
    | SetGameDifficulty Bool
    | ExitButton
    | EnterButton
    | SetName String
    | FetchSucceed Scores
    | FetchScore
    | GenerateDropCrate (Int,Vec3)
    | Explosion Vec3

type Collision = Collision
    { blocking : Bool                                   -- collision type. True = Blocking / False = Overlapping
    , effectOnTarget : (Actor -> Actor)                 -- effects on collision for the target Actor
    , effectOnSelf : (Actor -> Actor)                 -- effects on collision for the source Actor
    }
type alias ScoreItem = (String,Int)
type alias Scores = List ScoreItem

type ActorType
    = Player                                            -- Player character controlled by Input. Highlander Format
    | NPC                                               -- Other characters (players or AI)
    | Object                                            -- Decals, Crates and other things.
--    | Camera                                          -- maybe TODO: Camera

type ActorSubType
    = Bullet                                            -- Bullets/Rockets
    | Collectable                                       -- Crates
    | Obstacle                                          -- Blockable obstacles

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
    , characterAttributes : Maybe CharacterAttributes   -- only Players and NPC have Attributes
    -- , children : Children                            -- maybe TODO: possible Children Actors
    , index : Int                                       -- Render priority (for alpha textures)
    , size : Float                                      -- size of Actor
    , speed : Float
    , texture : String                                  -- texture
    , randomVal : Int
    , position : Vec3                                   -- Actor position
    , renderPosition : Vec3                              -- Actor start position
    , spriteCentering : Vec3
    , rotation : Mat4                                   -- Actor rotation
    , worldTransformationMatrix : Mat4
    , movesTo : Maybe Vec3                              -- AI character waypoints / multiplayer waypoints
    , moves : Bool                                      -- Actor moves (see rotation for direction)
    , timeToLive : Maybe Int                            -- for temporary objects like decals. Nothing: lives forever. Just x: dies in x milliseconds
    , collision : Maybe Collision                       -- type of collisions of the Actor.
    , animation : SpriteAnimation                       -- Actor animations
    , affectsCamera : Maybe Mat4                        -- Player only : Camera transformation
    , onDeath : Maybe OnAction
    , onDestination : Maybe OnAction
    , onActorKilled : Maybe OnActorKilled                         -- fires Msg when killed
    , fireCommand : Maybe ( Cmd Msg )                   -- fires Msg
    }

type alias ActorManager =
    Dict.Dict String Actor

port music : String -> Cmd msg

port sound : String -> Cmd msg

port setBloodOpacity : Int -> Cmd msg

port getScoreBoard : (String, Int) -> Cmd msg



update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TexturesError err ->
            ( { model | textures = Dict.empty }, Cmd.none )

        StatusError err ->
            ( model, Cmd.none )

        WindowSizeError err ->
            ( model, Cmd.none )

        WindowSizeSuccess s ->
            ( { model | wsize = { width = s.width - 3
                                , height = s.height - 3} }, Cmd.none )

        TexturesLoaded textures ->
            ( { model | textures = Dict.fromList textures }, Cmd.none)

        Animate dt ->
            if model.status == Game
                then
                    if model.startCounter > 0
                      then
                          let
                            numerize f = Basics.round (f / 1000)
                            changed = (numerize model.startCounter) /= (numerize (model.startCounter-dt))
                          in
                            if changed
                                then update (PlaySound "beep2") {model | startCounter = model.startCounter - dt}
                                else ({model | startCounter = model.startCounter - dt}, Cmd.none)

                      else
                        let
                            dirs = directions model.keys

                            characterAttributes = getCharacterAttributes model playerActor

                            actorManager = updateActorManager dt model
                            actorsAlive = fireCommandsAndkillActors actorManager
                            counter = model.counter + 1
                            last_i = if counter > 5000 then 1 else model.last_i
                        in
                            ( { model | actorManager = fst actorsAlive
                                      , counter = counter
                                      , last_i = last_i }, Cmd.batch [ (snd actorsAlive ), setBloodOpacity characterAttributes.health ] )
                else
                    if model.status == WaitingForHighscore
                        then
                            if List.isEmpty (log "LIST" model.scoreList) && model.waiting < 5000
                                then ({model | waiting = model.waiting + dt}, Cmd.none)
                                else update (ChangeStatus Highscore) { model | waiting = 0 }
                        else
                            (model, Cmd.none)

        UpdateMouse pos ->
            ( { model | mousePosition =  pos, lookAt = (calculateDirection pos model.wsize) }, Cmd.none )

        KeyChange (keyfunc,plus) ->
            ( { model | keys = keyfunc model.keys }, Cmd.none )
        MouseClicks pos ->
            if model.status == Game
                then
                    let
                        player = getPlayerActor model
                        attr = getCharacterAttributes model playerActor
                        renderPosition = (Math.Vector3.add player.renderPosition (vec3 0 1 0))
                        rotation = player.rotation
                        animation = animateSprite True 1 Nothing player.texture player.animation
                        newPlayer = { player | animation = animation }
                        am = updateActorManagerDict playerActor newPlayer model.actorManager
                    in
                        if model.keys.shift && attr.rockets > 0
                            then ( { model | actorManager = am }, ( Cmd.batch [ (succeed (player.position, rotation, player)) |> Task.perform SoundError Fire
                                                                              , (succeed "rocket") |> Task.perform SoundError PlaySound ] ))
                            else if Basics.not model.keys.shift
                                    then
                                        ( { model | actorManager = am }, ( Cmd.batch [ (succeed (player.position, rotation, player)) |> Task.perform SoundError Fire
                                                                                     , (succeed "shot") |> Task.perform SoundError PlaySound ] ))
                                    else (model,Cmd.none)
                else
                    (model, Cmd.none)
        PlayMusic s ->
            (model, music s)
        FetchScore ->
            ({ model | status = WaitingForHighscore }, getScoreBoard (model.playerName,model.score))
        SetName name ->
            ({model | playerName = String.trim name }, Cmd.none)
        FetchSucceed list ->
            update (ChangeStatus WaitingForHighscore) {model | scoreList = log "LIST>" list}
        AddScore i ->
            let
                player = getPlayerActor model
                ca = getCharacterAttributes  model playerActor

                newModel = { model | actorManager =
                                        updateActorManagerDict playerActor
                                            { player | characterAttributes
                                                     =  (Just { ca | score = ca.score + i}) }
                                            model.actorManager
                                   , score = ca.score + i
                            }
            in if ca.score >= 300
                    then update  (ChangeStatus Won) newModel
                    else (newModel,Cmd.none)
        GetRandomFireRate i ->
            (model, Random.generate RandomFireRate intList )

        RandomFireRate list ->
                let
                    actorManager = setRandoms (log "RANDOM" list) (Dict.keys model.actorManager) model.actorManager
                in
                    ( { model | actorManager = actorManager}, Cmd.none)
        GetRandomDirectionVector actor ->
            (model, Random.generate RandomDirectionVector (randomVectorForActor actor) )
        RandomDirectionVector (actor,dir) ->
            let
                player = getPlayerActor model
                newActor = { actor | movesTo = Just (Math.Vector3.add player.position  (Math.Vector3.scale 3 (Math.Vector3.normalize ( vec3 (Math.Vector3.getX dir) (Math.Vector3.getY dir) 0)))) }
            in
                ({ model | actorManager = updateActorManagerDict actor.key newActor model.actorManager } , Cmd.none)
        SoundError err ->
            ( model, Cmd.none )

        PlaySound s ->
            (model, sound s)
        Fire (position,direction,actor) ->
            let
                    player = getPlayerActor model
                    worldTranslation = makeTranslate <| vec3 -(Math.Vector3.getX player.position) -(Math.Vector3.getY player.position) 0

                    ndsx =(toFloat model.mousePosition.x)/(toFloat model.wsize.width)
                    ndsy =(toFloat model.mousePosition.y)/(toFloat model.wsize.height)

                    dy = (ndsy * 2 - 1)
                    dx = (ndsx * 2 - 1)

                    my = dy * (-2)
                    mx = dx * 2 * ((toFloat model.wsize.width) / (toFloat model.wsize.height))

                    vm = vec3 mx my 0

                    mousePosition = vec3 dx dy -4.99
                    target = log "TEST" (Math.Vector3.add player.position vm)
                    --target = rayCast model
                    size = actor.size
                    slugint = model.last_i + 1
                    slugname = "slug-" ++ ( toString ( model.last_i + 1 ) )
                    newPosition = Math.Vector3.add position  (transform direction (vec3 -(size*0.057) -0.3 0))

                    am = case actor.characterAttributes of
                            Nothing -> model.actorManager
                            Just x ->
                                if model.keys.shift == True && actor.actorType == Player && x.rockets > 0
                                    then
                                        let
                                            tam = updateActorManagerDict actor.key { actor | characterAttributes = Just { x | rockets = x.rockets - 1} } model.actorManager
                                        in
                                            Dict.insert slugname (templateRocketActor  model.gameSpeed ( slugint ) newPosition target direction) tam
                                    else Dict.insert slugname (templateSlugActor model.gameSpeed ( slugint ) newPosition direction) model.actorManager

            in
                ({ model | last_i = model.last_i + 1
                         , actorManager = am
                 }, Cmd.none)
        Explosion position ->
            let

                collide v1 r1 v2 r2 = ( Math.Vector3.getX v2 - Math.Vector3.getX v1 ) ^ 2 + ( Math.Vector3.getY v2 - Math.Vector3.getY v1 ) ^ 2 <= ( (r1/2 + r2) ) ^ 2

                checkActor s actor =
                    if actor.actorType == NPC && collide actor.position actor.size position 1
                        then { actor | timeToLive = Just 200}
                        else  actor
                newActorManager = Dict.map checkActor model.actorManager
            in
                ({ model | actorManager = newActorManager }, Cmd.none)

        GenerateDropCrate (rand,position) ->
            let
                    slugint = model.last_i + 1
                    am =
                        if rand < 50
                            then
                                let
                                    slugname =
                                        "health-" ++ ( toString ( model.last_i + 1 ) )
                                in
                                    Dict.insert slugname ( templateHealthActor model.gameSpeed ( slugint ) position ) model.actorManager
                            else
                                let
                                    slugname = "rocketCrate-" ++ ( toString ( model.last_i + 1 ) )
                                in
                                    Dict.insert slugname ( templateRocketCrateActor model.gameSpeed ( slugint ) position ) model.actorManager

            in
                ({ model | last_i = model.last_i + 1
                         , actorManager = am }, Cmd.none)
        ChangeStatus s ->
            case s of
                MainMenu ->
                    let
                        newModel = fst init
                    in
                        update (PlaySound "beep2")
                            { newModel | textures = model.textures
                                       , wsize = model.wsize
                            }
                EnterName ->
                    ({model | status = s}, Cmd.batch[Task.perform SoundError PlaySound (succeed "beep2"),Task.perform SoundError GetRandomFireRate (succeed 1)])
                Game ->
                    ({model | status = s}, Cmd.batch[Task.perform SoundError PlaySound (succeed "beep2"),Task.perform SoundError PlayMusic (succeed "actofwar")])
                Won ->
                    ({model | status = s}, Cmd.batch[Task.perform SoundError PlaySound (succeed "beep2"),Task.perform SoundError PlayMusic (succeed "intro")])
                GameOver ->
                    ({model | status = s}, Cmd.batch[Task.perform SoundError PlaySound (succeed "beep2"),Task.perform SoundError PlayMusic (succeed "intro")])

                _ -> ({model | status = s}, Task.perform SoundError PlaySound (succeed "beep2"))

        SetGameSpeed i ->
            ({model | gameSpeed = i}, Task.perform SoundError ChangeStatus (succeed Options))

        SetGameDifficulty i ->
            ({model | gameDifficulty = i}, Task.perform SoundError ChangeStatus (succeed Options))
        EnterButton ->
            if model.status == EnterName
                then
                    (model, if String.isEmpty (log "STRING" model.playerName) then Cmd.none else Task.perform SoundError ChangeStatus (succeed Help))
                else
                    (model, if String.isEmpty (log "STRING"model.playerName) then Cmd.none else Task.perform SoundError ChangeStatus (succeed Game))
        ExitButton ->
            case model.status of
                Won -> update FetchScore model
                GameOver -> update FetchScore model
                Highscore-> update (ChangeStatus MainMenu) model
                Speed -> (model, Task.perform SoundError ChangeStatus (succeed Options))
                Difficulty -> (model, Task.perform SoundError ChangeStatus (succeed Options))
                _ -> (model, Task.perform SoundError ChangeStatus (succeed MainMenu))


setRandoms : List Int -> List String -> ActorManager -> ActorManager
setRandoms xs enemies am =
        case xs of
            (i :: l) ->
                case enemies of
                    (enemy :: elist) ->
                        case Dict.get (log "ENEMY" enemy) am of
                            Nothing -> setRandoms xs elist am
                            Just x ->
                                if x.actorType == NPC
                                    then
                                        let
                                            ca = case x.characterAttributes of
                                                    Just y -> Just { y | rateOfFire = log ("RATEOF " ++x.key) (i*2 +   y.rateOfFire) }
                                                    Nothing -> Nothing
                                            newAm = updateActorManagerDict x.key
                                                        { x | randomVal = clamp 0 (Basics.round (((toFloat i) + 100.0) / 2.0)) 100
                                                            , characterAttributes = ca
                                                        }
                                                        am
                                        in
                                            setRandoms l elist newAm

                                    else
                                        setRandoms xs elist am
                    [] -> am

            [] -> am






scoreBoardUrl : Model -> String
scoreBoardUrl model =
    "http://example.com/gamescore.php?name="
           ++ model.playerName
           ++ "&score="
           ++ (toString model.score)

intList : Random.Generator (List Int)
intList = Random.list 4 (Random.int -100 100 )

setActorVectorPair : Actor -> Float -> Float -> Float -> (Actor, Vec3)
setActorVectorPair actor x y z = (actor, vec3 x y z)

randomVectorForActor : Actor -> Random.Generator (Actor,Vec3)
randomVectorForActor actor  = Random.map3 (setActorVectorPair actor) (Random.float -1 1) (Random.float -1 1) (Random.float -1 1)

getPlayerActor : Model -> Actor
getPlayerActor model =
    case Dict.get playerActor model.actorManager of
        Nothing -> templatePlayerActor playerActor
        Just x ->  x

getCharacterAttributes : Model -> String -> CharacterAttributes
getCharacterAttributes model actorName =
    let
        player = getPlayerActor model
    in
        case player.characterAttributes of
                    Nothing -> { health = 100
                               , rockets = 0
                               , score = 0
                               , rateOfFire = 300
                               , timeSinceLastFire = 0 }
                    Just x -> x

-- very primitive AI for enemies. Moves around the player
ai : Actor -> Model -> (Vec3, Cmd Msg)
ai actor model =

    case Dict.get playerActor model.actorManager of
        Nothing -> (vec3 0 0 0, Cmd.none)
        Just x -> case actor.movesTo of
                    Nothing -> (x.position, Task.perform SoundError GetRandomDirectionVector (succeed actor))
                    Just mov -> if Math.Vector3.length (Math.Vector3.sub x.position mov) < 0.5 || Math.Vector3.length (Math.Vector3.sub actor.position mov) < 0.5
                        then (mov, Task.perform SoundError GetRandomDirectionVector (succeed actor))
                        else (mov, Cmd.none)

updateActorManagerDict : String -> Actor -> ActorManager ->  ActorManager
updateActorManagerDict key target am =
    let
        newAm = Dict.remove key am
    in
        Dict.insert key target newAm

affectActorPairs : String -> List String -> ActorManager ->  ActorManager
affectActorPairs key targetKeys am  =
    case targetKeys of
        ( tkey :: list ) ->
            case Dict.get key am of
                Nothing -> am
                Just actor ->
                    case Dict.get tkey am of
                        Nothing -> am
                        Just target ->
                            let
                                affectedPair = case actor.collision of
                                    Nothing -> (actor, target)
                                    Just x -> case target.collision of
                                                Nothing -> (actor, target)
                                                Just y -> affectActorPair (x, y) (actor, target )

                                newAm = updateActorManagerDict key (fst affectedPair) am

                            in
                                affectActorPairs key list (updateActorManagerDict tkey (snd affectedPair) newAm)

        []            -> am

affectActorPair : (Collision, Collision) -> (Actor, Actor) -> (Actor, Actor)
affectActorPair (Collision c1, Collision c2) (source, target) =
    let
        collide v1 r1 v2 r2 = ( Math.Vector3.getX v2 - Math.Vector3.getX v1 ) ^ 2 + ( Math.Vector3.getY v2 - Math.Vector3.getY v1 ) ^ 2 <= ( (r1 + r2)/3 ) ^ 2
        effect = collide source.position source.size target.position target.size
        a1 = if effect
                then c2.effectOnTarget (c1.effectOnSelf source)
                else source
        a2 = if effect
                then c1.effectOnTarget (c2.effectOnSelf target)
                else target
    in ( a1, a2 )

checkCollisions : String -> Actor -> Vec3 -> ActorManager -> ActorManager
checkCollisions actorKey actor position am =
    let
        collide v1 r1 v2 r2 = ( Math.Vector3.getX v2 - Math.Vector3.getX v1 ) ^ 2 + ( Math.Vector3.getY v2 - Math.Vector3.getY v1 ) ^ 2 <= ( (r1 + r2)/3 ) ^ 2  -- two circles collision
        collisionFilter key a                                                             -- check if path is blocked
            =  case a.collision of
                    Nothing -> False
                    Just x -> let (Collision y) = x
                              in y.blocking
                              && collide actor.position actor.size a.position a.size    -- does it collide with first Actor?

        dict = Dict.remove actorKey am                                                    -- removing Actor from list (don't need to check collisions on self)
        dicts = Dict.partition collisionFilter dict                                       -- partitioning Dict into two Dicts: fst - blocking Actors, snd - overlapping Actor
        blocked =
            case actor.collision of
                Nothing -> False
                Just x ->
                    let (Collision c) = x
                    in (Dict.size ( fst dicts ) ) /= 0 && c.blocking                      -- is the path blocked?
        nactor = { actor | position = if blocked then actor.position else position }      -- path is blocked -> old position, otherwise -> new position
        blockingDict = ( fst dicts )
    in
        affectActorPairs actorKey (Dict.keys (snd dicts)) (updateActorManagerDict actorKey nactor ( am ) )

updateActorManagerList : Float -> Model -> ActorManager -> List ( String, Actor ) -> ActorManager
updateActorManagerList dt model am amList =
    case amList of
        ( a :: list ) ->
            let
                act = snd a
                key = log "ACTS" (fst a)
                player =
                    case ( Dict.get playerActor am ) of
                        Nothing -> act
                        Just x -> x
                getCA p =
                  case p.characterAttributes of
                    Nothing -> { health = 100
                               , rockets = 0
                               , score = 0
                               , rateOfFire = 300
                               , timeSinceLastFire = 0 }
                    Just x -> x
            in
                case act.actorType of
                    Player ->
                        let
                            actor =
                                case ( Dict.get key am ) of
                                    Nothing -> act
                                    Just x -> x
                            dirs = directions model.keys
                            moving = if dirs.x /= 0 || dirs.y /=0
                                        then Just ( normalize ( vec3 (toFloat dirs.x) (toFloat dirs.y) 0 ) )
                                        else Nothing
                            worldTransformationMatrix = makeWorldTranslation model.gameSpeed actor.position dt dirs model.lookAt
                            newPosition = transform worldTransformationMatrix (vec3 0 0 0)
                            newAm = checkCollisions key actor newPosition am
                            checkedActor =
                                case ( Dict.get key newAm ) of
                                    Nothing -> actor
                                    Just x -> x
                            checkedCharacterAttributes = getCA checkedActor
                            newCharacterAttributes = { checkedCharacterAttributes | timeSinceLastFire = checkedCharacterAttributes.timeSinceLastFire + ( floor dt ) }
                            newActor = { actor | animation = animateSprite False (dt*(model.gameSpeed/10)) moving checkedActor.texture checkedActor.animation
                                               , timeToLive = if newCharacterAttributes.health <= 0 then Just 0 else checkedActor.timeToLive
                                               , characterAttributes = Just newCharacterAttributes
                                               , rotation = model.lookAt
                                               , position = newPosition
                                               , worldTransformationMatrix = worldTransformationMatrix
                                       }

                        in
                            updateActorManagerList dt model (updateActorManagerDict key newActor newAm) list
                    NPC ->
                        let
                            actor =
                                (case ( Dict.get key am ) of
                                    Nothing -> act
                                    Just x -> x)

                            dirs = { x = 0, y = 1 }
                            lookAt = makeRotateBetween ( vec3 0 -1 0 ) ( Math.Vector3.sub player.position actor.position )

                            (v,cmd) = ai actor model

                            translationVector = case actor.movesTo of
                                Nothing -> Math.Vector3.scale ((actor.speed * model.gameSpeed * dt) / 10000) (Math.Vector3.normalize (Math.Vector3.sub v actor.position))
                                Just x -> Math.Vector3.scale ((actor.speed * model.gameSpeed * dt) / 10000) (Math.Vector3.normalize (Math.Vector3.sub x actor.position))
                            translation = makeTranslate translationVector
                            tPosition = transform translation actor.position

                            worldTranslation = makeTranslate <| vec3 -(Math.Vector3.getX player.position) -(Math.Vector3.getY player.position) 0


                            difflen = length ( Math.Vector3.sub tPosition actor.position )



                            (movesTo, newPosition) =
                                case actor.movesTo of
                                    Nothing -> (Just v,actor.position)
                                    Just x -> if difflen > ( length ( Math.Vector3.sub x tPosition) )
                                                 then (Just v,x)
                                                 else (Just x,tPosition)


                            renderPosition = transform worldTranslation tPosition



                            distance = length (Math.Vector3.sub newPosition player.position)

                            newAm = checkCollisions key actor newPosition am
                            checkedActor =
                                case ( Dict.get key newAm ) of
                                    Nothing -> { actor | movesTo = movesTo }
                                    Just x -> { x | renderPosition = renderPosition }

                            characterAttributes =
                                case checkedActor.characterAttributes of
                                    Nothing -> Nothing
                                    Just x -> Just { x | timeSinceLastFire = x.timeSinceLastFire + ( floor dt ) }

                            fireCommand =
                                case characterAttributes of
                                    Nothing -> Nothing
                                    Just x -> if distance < 5 && x.rateOfFire <= x.timeSinceLastFire
                                                    then Just ( Cmd.batch [ cmd
                                                                          , (succeed (checkedActor.position, checkedActor.rotation, checkedActor)) |> Task.perform SoundError Fire
                                                                          , (succeed "shot") |> Task.perform SoundError PlaySound
                                                                          ] )
                                                    else Nothing

                            newCharacterAttributes = case characterAttributes of
                                Nothing -> Nothing
                                Just x -> case fireCommand of
                                    Nothing -> characterAttributes
                                    Just t -> Just { x | timeSinceLastFire = 0}
                            isShooting = case fireCommand of
                                            Nothing -> False
                                            _       -> True
                            newActor = { checkedActor | animation = animateSprite isShooting (dt*(model.gameSpeed/10)) movesTo checkedActor.texture checkedActor.animation
                                                      , rotation = lookAt
                                                      , movesTo = movesTo
                                                      , characterAttributes = newCharacterAttributes }

                            (tActor, fireCommand2) =
                                case newCharacterAttributes of
                                    Nothing -> (newActor, Nothing)
                                    Just x ->
                                        if x.health<=0
                                            then
                                                case checkedActor.onDeath of
                                                    Nothing -> (newActor, Nothing)
                                                    Just (OnAction f) -> f.func newActor
                                            else
                                                (newActor, Nothing)
                            cmds =
                                case fireCommand of
                                    Nothing ->
                                        case fireCommand2 of
                                            Nothing -> Nothing
                                            _ ->  fireCommand2
                                    Just x ->
                                        case fireCommand2 of
                                            Nothing -> fireCommand
                                            Just k -> Just (Cmd.batch [k,x])

                            outActor = { tActor | timeToLive =
                                                    case tActor.timeToLive of
                                                       Nothing -> Nothing
                                                       Just x -> Just (Basics.max 0 (x - (Basics.round dt)))

                                                , fireCommand = cmds
                                     }
                        in
                            updateActorManagerList dt model (updateActorManagerDict key outActor newAm) list

                    Object ->
                        let
                            actor =
                                (case ( Dict.get key am ) of
                                    Nothing -> act
                                    Just x -> x)
                            dirs = { x = 0, y = 1 }


                            (destination,translationVector) =
                                case actor.movesTo of
                                    Nothing ->
                                        if actor.moves
                                            then (False, Math.Vector3.scale ((actor.speed * model.gameSpeed * dt) / 10000) (Math.Vector3.normalize (transform actor.rotation (vec3 0 -1 0))))
                                            else (False, vec3 0 0 0)
                                    Just x ->
                                        let
                                            direction = Math.Vector3.normalize (Math.Vector3.sub x actor.position)
                                            dest =  Math.Vector3.length (Math.Vector3.sub x actor.position) <= ( ( actor.speed * model.gameSpeed * dt ) / 10000 )
                                        in
                                            ( dest, Math.Vector3.scale ((actor.speed * model.gameSpeed * dt) / 10000) direction)
                            translation = makeTranslate translationVector
                            tPosition = transform translation actor.position

                            worldTranslation = makeTranslate <| vec3 -(Math.Vector3.getX player.position) -(Math.Vector3.getY player.position) 0
                            renderPosition = transform worldTranslation tPosition

                            (tActor,fireCommand) = if log "DESTIO" destination then case actor.onDestination of
                                                                         Nothing -> (actor,Nothing)
                                                                         Just (OnAction f) ->  f.func actor
                                                                  else (actor,Nothing)

                            newActor = { tActor | renderPosition =  renderPosition
                                                , animation = animateSprite False (dt*(model.gameSpeed/10)) Nothing tActor.texture tActor.animation
                                                , position =  tPosition
                                                , timeToLive = case tActor.timeToLive of
                                                                    Nothing -> Nothing
                                                                    Just x -> Just (Basics.max 0 (x - (Basics.round dt)))
                                                , fireCommand = fireCommand
                                       }
                       in
                            updateActorManagerList dt model (updateActorManagerDict key newActor am)  list

        []            -> am


updateActorManager : Float -> Model -> ActorManager
updateActorManager dt model =
    let
        amlist = Dict.toList model.actorManager
    in
        updateActorManagerList dt model model.actorManager amlist

fireCommandsAndkillActors : ActorManager -> ( ActorManager, Cmd Msg )
fireCommandsAndkillActors am =
    let
        getOnActorKilled (key, actor) =
                ( case actor.fireCommand of
                    Nothing -> []
                    Just x -> [x] )
             ++ ( case actor.timeToLive of
                    Nothing -> []
                    Just x -> if x == 0 then
                                            case actor.onActorKilled of
                                                Nothing -> []
                                                Just (OnActorKilled x) -> [ x.onActorKilled actor ]
                                        else [] )
        filterKilled key actor =
            case actor.timeToLive of
                Nothing -> True
                Just x -> x /= 0

        cmd = Cmd.batch ( List.concat (List.map getOnActorKilled ( Dict.toList am ) ) )
        newam = Dict.filter filterKilled am

    in
        (newam, cmd)

playerActor : String
playerActor = "Player1"

templatePlayerActor : String -> Actor
templatePlayerActor name =     { key = name
                               , actorType = Player
                               , actorSubType = Nothing
                               , characterAttributes = Just { health = 100
                                                            , rockets = 3
                                                            , score = 0
                                                            , rateOfFire = 10000
                                                            , timeSinceLastFire = 0 }
                                     --, children = Children []
                               , index= 7
                               , size = 0.35
                               , speed = 0.2
                               , randomVal = 0
                               , texture = "assault"
                               , animation =      { name = "idle"
                                                  , current = 0
                                                  , end = 100
                                                  }
                               , position = vec3 0 0 -4.99
                               , renderPosition = vec3 0 0 -4.99
                               , spriteCentering = vec3 -0.02 -0.17 0
                               , rotation = Math.Matrix4.identity
                               , worldTransformationMatrix = Math.Matrix4.identity
                               , movesTo = Nothing
                               , moves = True
                               , timeToLive = Nothing
                               , collision = Just ( Collision { blocking = True
                                                              , effectOnTarget = Basics.identity
                                                              , effectOnSelf = Basics.identity })
                               , affectsCamera = Nothing
                               , onDeath = Nothing
                               , onDestination = Nothing
                               , onActorKilled = Just ( OnActorKilled { onActorKilled = gameOver} )
                               , fireCommand = Nothing
                               }

gameOver : Actor -> Cmd Msg
gameOver actor = succeed GameOver |> Task.perform StatusError ChangeStatus

addScoreAndDropCrate : Actor -> Cmd Msg
addScoreAndDropCrate actor =
    Cmd.batch [ Task.perform SoundError AddScore (succeed 100)
              , Task.perform SoundError GenerateDropCrate (succeed (actor.randomVal,  actor.position))]

explode : Actor -> (Actor, Maybe(Cmd Msg))
explode act = ( { act | timeToLive = Just 800
                                          , texture = "explosion"
                                          , animation = { name = "idle"
                                                                , current = 0
                                                                , end = 800}
                                          , moves = False
                                          , movesTo = Nothing
                                          , size = 1.5}
                                    , Just (Cmd.batch [Task.perform SoundError Explosion (succeed act.position), (succeed "explosion") |> Task.perform SoundError PlaySound] ))

death : Actor -> (Actor, Maybe (Cmd Msg))
death act = ( { act |                       timeToLive = Just 400
                                          , actorType = Object
                                          , texture = "death"
                                          , animation = { name = "idle"
                                                                , current = 0
                                                                , end = 400}
                                          , moves = False
                                          , movesTo = Nothing
                                          , size = 0.2}
                                    , Nothing )

templateEnemyActor : Float ->String -> Vec3  -> Actor
templateEnemyActor gameSpeed name p =     { key = name
                               , actorType = NPC
                               , actorSubType = Nothing
                               , characterAttributes = Just { health = 100
                                                            , rockets = 0
                                                            , score = 0
                                                            , rateOfFire = round (10000/gameSpeed)
                                                            , timeSinceLastFire = 0 }
                               , index= 8
                               , size = 0.25
                               , speed = 0.8
                               , randomVal = 0
                               , texture = "assault"
                               , animation =      { name = "idle"
                                                  , current = 0
                                                  , end = 100
                                                  }
                               , position = p
                               , renderPosition = vec3 2 2 -4.99
                               , spriteCentering = vec3 0 -0.2 0.15
                               , rotation = Math.Matrix4.identity
                               , worldTransformationMatrix = Math.Matrix4.identity
                               , movesTo = Nothing
                               , moves = False
                               , timeToLive = Nothing
                               , collision = Just ( Collision { blocking = True
                                                              , effectOnTarget = Basics.identity
                                                              , effectOnSelf = Basics.identity })
                               , affectsCamera = Nothing
                               , onDeath = Just (OnAction { func = death })
                               , onDestination = Nothing
                               , onActorKilled = Just ( OnActorKilled { onActorKilled = addScoreAndDropCrate} )
                               , fireCommand = Nothing }

killBulletCollectable: Actor -> Actor
killBulletCollectable actor = case actor.actorSubType of
                                Nothing -> actor
                                Just x -> if x == Collectable || x == Bullet
                                             then { actor | timeToLive = (Just 0) }
                                             else actor

bulletHit: Actor -> Actor
bulletHit actor =
    let
        newCharacterAttributes = case actor.characterAttributes of
            Nothing -> Nothing
            Just x -> Just { x | health = Basics.clamp 0 (x.health - 20) 100 }

    in
        { actor | characterAttributes = newCharacterAttributes }

die : Actor -> Actor
die actor ={ actor | timeToLive = Just 0 }

templateSlugActor : Float -> Int -> Vec3 -> Mat4 -> Actor
templateSlugActor gameSpeed i p r  =
    let
        name = toString i
    in
                               { key = "slug-" ++ name
                               , actorType = Object
                               , actorSubType = Just Bullet
                               , characterAttributes = Just { health = 100
                                                            , rockets = 0
                                                            , score = 0
                                                            , rateOfFire = 600
                                                            , timeSinceLastFire = 0 }
                                     --, children = Children []
                               , index= 9
                               , size = 0.2
                               , texture = "slug"
                               , randomVal = 0
                               , animation =      { name = "idle"
                                                  , current = 0
                                                  , end = 100
                                                  }
                               , position = p
                               , renderPosition = p
                               , spriteCentering = vec3 0 0 0
                               , rotation = r
                               , worldTransformationMatrix = Math.Matrix4.identity
                               , speed = 4
                               , movesTo = Nothing
                               , moves = True
                               , timeToLive = Just (round (10000/gameSpeed))
                               , collision = Just (Collision { blocking = False
                                                             , effectOnTarget = bulletHit
                                                             , effectOnSelf = die })
                               , affectsCamera = Nothing
                               , onDeath = Nothing
                               , onDestination = Nothing
                               , onActorKilled = Nothing
                               , fireCommand = Nothing }
templateRocketActor : Float -> Int -> Vec3 -> Vec3 -> Mat4 -> Actor
templateRocketActor gameSpeed i p t r  =
    let
        name = toString i
    in
                               { key = "slug-" ++ name
                               , actorType = Object
                               , actorSubType = Just Bullet
                               , characterAttributes = Just { health = 100
                                                            , rockets = 0
                                                            , score = 0
                                                            , rateOfFire = 600
                                                            , timeSinceLastFire = 0 }
                                     --, children = Children []
                               , index= 9
                               , size = 0.3
                               , texture = "rocket"
                               , randomVal = 0
                               , animation =      { name = "idle"
                                                  , current = 0
                                                  , end = 100
                                                  }
                               , position = p
                               , renderPosition = p
                               , spriteCentering = vec3 0 0 0
                               , rotation = r
                               , worldTransformationMatrix = Math.Matrix4.identity
                               , speed = 3
                               , movesTo = Just t
                               , moves = False
                               , timeToLive = Just (round (30000/gameSpeed))
                               , collision = Nothing
                               , affectsCamera = Nothing
                               , onDeath = Nothing
                               , onDestination = Just ( OnAction { func = explode })
                               , onActorKilled = Nothing
                               , fireCommand = Nothing }
templateHealthActor : Float -> Int -> Vec3 -> Actor
templateHealthActor gameSpeed i p  =
    let
        name = toString i
    in
                               { key = "health-" ++ name
                               , actorType = Object
                               , actorSubType = Just Collectable
                               , characterAttributes = Just { health = 100
                                                            , rockets = 0
                                                            , score = 0
                                                            , rateOfFire = 600
                                                            , timeSinceLastFire = 0 }
                               , index= 6
                               , size = 0.1
                               , texture = "healthCrate"
                               , randomVal = 0
                               , animation =      { name = "idle"
                                                  , current = 0
                                                  , end = 100
                                                  }
                               , position = p
                               , renderPosition = p
                               , spriteCentering = vec3 0 0 0
                               , rotation = Math.Matrix4.identity
                               , worldTransformationMatrix = Math.Matrix4.identity
                               , speed = 4
                               , movesTo = Nothing
                               , moves = False
                               , timeToLive = Just (round (100000/gameSpeed))
                               , collision = Just (Collision { blocking = False
                                                             , effectOnSelf = die
                                                             , effectOnTarget =
                                                                    (\actor ->
                                                                          (let
                                                                               newCharacterAttributes = case actor.characterAttributes of
                                                                                   Nothing -> Nothing
                                                                                   Just x -> Just { x | health = Basics.clamp 0 (x.health + 100) 100 }
                                                                           in
                                                                               { actor | characterAttributes = newCharacterAttributes })) })
                               , affectsCamera = Nothing
                               , onDeath = Nothing
                               , onDestination = Nothing
                               , onActorKilled = Nothing
                               , fireCommand = Nothing }
templateRocketCrateActor : Float -> Int -> Vec3 -> Actor
templateRocketCrateActor gameSpeed i p  =
    let
        name = toString i
    in
                               { key = "rocketCrate-" ++ name
                               , actorType = Object
                               , actorSubType = Just Collectable
                               , characterAttributes = Just { health = 100
                                                            , rockets = 0
                                                            , score = 0
                                                            , rateOfFire = 600
                                                            , timeSinceLastFire = 0 }
                               , index= 6
                               , size = 0.1
                               , texture = "rocketCrate"
                               , randomVal = 0
                               , animation =      { name = "idle"
                                                  , current = 0
                                                  , end = 100
                                                  }
                               , position = p
                               , renderPosition = p
                               , spriteCentering = vec3 0 0 0
                               , rotation = Math.Matrix4.identity
                               , worldTransformationMatrix = Math.Matrix4.identity
                               , speed = 4
                               , movesTo = Nothing
                               , moves = False
                               , timeToLive = Just (round (100000/gameSpeed))
                               , collision = Just (Collision { blocking = False
                                                             , effectOnSelf = die
                                                             , effectOnTarget =
                                                                    (\actor ->
                                                                          (let
                                                                               newCharacterAttributes = case actor.characterAttributes of
                                                                                   Nothing -> Nothing
                                                                                   Just x -> Just { x | rockets = Basics.clamp 0 (x.rockets + 1) 3 }
                                                                           in
                                                                               { actor | characterAttributes = newCharacterAttributes })) })
                               , affectsCamera = Nothing
                               , onDeath = Nothing
                               , onDestination = Nothing
                               , onActorKilled = Nothing
                               , fireCommand = Nothing }

templateGroundActor : Actor
templateGroundActor =          { key = "ground"
                               , actorType = Object
                               , actorSubType = Nothing
                               , characterAttributes = Nothing
                               , index= 10
                               , size = 10
                               , texture = "ground"
                               , randomVal = 0
                               , animation =      { name = "idle"
                                                  , current = 0
                                                  , end = 100
                                                  }
                               , position = vec3 0 0 -5
                               , renderPosition = vec3 0 0 -5
                               , spriteCentering = vec3 0 0 0
                               , rotation = Math.Matrix4.identity
                               , worldTransformationMatrix = Math.Matrix4.identity
                               , speed = 0
                               , movesTo = Nothing
                               , moves = False
                               , timeToLive = Nothing
                               , collision = Nothing
                               , affectsCamera = Nothing
                               , onDeath = Nothing
                               , onDestination = Nothing
                               , onActorKilled = Nothing
                               , fireCommand = Nothing }

actorManager : ActorManager
actorManager = Dict.fromList [ ("Player1", templatePlayerActor "Player1")
                             , ("Enemy1", templateEnemyActor 10 "Enemy1" (vec3 2 2 -4.99))
                             , ("Enemy2", templateEnemyActor 10 "Enemy2" (vec3 3 -2 -4.99))
                             , ("Enemy3", templateEnemyActor 10 "Enemy3" (vec3 -3 0 -4.99))
                             , ("Enemy4", templateEnemyActor 10 "Enemy4" (vec3 -3 3 -4.99))
                             , ("ground", templateGroundActor)
                             ]

getAnimationDuration : String -> Int
getAnimationDuration s =
    let
        animation = Dict.get s animationDict
    in case animation of
        Nothing -> 0
        Just x -> x.duration

animateSprite : Bool -> Float -> Maybe Vec3 -> String -> SpriteAnimation -> SpriteAnimation
animateSprite shooting dtf moves key anim =
    let
        new = if shooting
                then
                    "fire"
                else
                    case moves of
                        Nothing -> "idle"
                        Just m -> "move"

        dt = floor dtf
        a = anim
    in
        if anim.name == new || (anim.name == "fire" && anim.end < (anim.current + dt))
            then
                { anim | current = (anim.current + dt) %  anim.end }
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
            , ( "assault", "texture/acharacter.png")
            , ( "grenadeCrate", "texture/grenadeCrate.png")
            , ( "healthCrate", "texture/healthCrate.png")
            , ( "slug", "texture/bullet.png")
            , ( "rocket", "texture/rocket.png")
            , ( "healthCrate", "texture/healthCrate.png")
            , ( "rocketCrate", "texture/grenadeCrate.png")
            , ( "explosion", "texture/explosion.png")
            , ( "death", "texture/death.png")
            ]
        )

framedTextures : Dict.Dict String (Int,Bool)
framedTextures = Dict.fromList [ ( "assault", (4, False) )
                               , ( "ground", (10, True) )
                               , ("slug", (1,False))
                               , ("healthCrate", (1,False))
                               , ("rocketCrate",(1,False))
                               , ("rocket",(1,False))
                               , ("explosion",(16,False))
                               , ("death",(16,False)) ]

animationDict : AnimationDictionary
animationDict = Dict.fromList [ ( "assault-idle", { duration = 100, frames = [ ( 1, 100 ) ] } )
                              , ( "assault-move", { duration = 200, frames = [ ( 1, 100 ) , ( 0, 100 ) ] } )
                              , ( "assault-fire", { duration = 300, frames = [ ( 1, 100 ) , ( 3, 50  ) , ( 2, 100 ) , (3, 50 ) ] } )
                              , ( "ground-idle",  { duration = 100, frames = [ ( 0, 100 ) ] } )
                              , ( "slug-idle",  { duration = 100, frames = [ ( 0, 100 ) ] } )
                              , ( "healthCrate-idle",  { duration = 100, frames = [ ( 0, 100 ) ] } )
                              , ( "rocketCrate-idle",  { duration = 100, frames = [ ( 0, 100 ) ] } )
                              , ( "rocket-idle",  { duration = 100, frames = [ ( 0, 100 ) ] } )
                              , ( "rocket-move",  { duration = 100, frames = [ ( 0, 100 ) ] } )
                              , ( "explosion-idle", {duration = 800, frames =  [ ( 0, 50), ( 1, 50), ( 2, 50), ( 3, 50)
                                                                               , ( 4, 50), ( 5, 50), ( 6, 50), ( 7, 50)
                                                                               , ( 8, 50), ( 9, 50), ( 10, 50), ( 11, 50)
                                                                               , ( 12, 50), ( 13, 50), ( 14, 50), ( 15, 50) ] } )
                             , ( "death-idle", {duration = 400, frames =     [ ( 0, 25), ( 1, 25), ( 2, 25), ( 3, 25)
                                                                             , ( 4, 25), ( 5, 25), ( 6, 25), ( 7, 25)
                                                                             , ( 8, 25), ( 9, 25), ( 10, 25), ( 11, 25)
                                                                             , ( 12, 25), ( 13, 25), ( 14, 25), ( 15, 25) ] } )
                              , ( "slug-move",  { duration = 100, frames = [ ( 0, 100 ) ] } )   ]

windowSize : Task Error Window.Size -> Cmd Msg
windowSize t =
    Task.perform WindowSizeError WindowSizeSuccess t

init : ( Model, Cmd Msg )
init =
    ( { textures = Dict.empty
      , wsize = { width = 800, height = 800 }
      , mousePosition = { x = 0, y = 0 }
      , keys = Keys False False False False False False False
      , lookAt = Math.Matrix4.identity
      , status = MainMenu
      , actorManager = actorManager
      , last_i = 0
      , counter = 0
      , gameSpeed = 7
      , gameDifficulty = False
      , playerName = ""
      , score = 0
      , scoreList = []
      , waiting = 0
      , startCounter = 4999
      }
    , Cmd.batch
        [ Window.size |> windowSize
        , fetchTextures |> Task.perform TexturesError TexturesLoaded
        , (succeed "intro") |> Task.perform SoundError PlayMusic
        ]
    )


mouseClicks : Position -> Msg
mouseClicks position = MouseClicks position

keyPressed : Keyboard.KeyCode -> Msg
keyPressed keyCode =
    case keyCode of
        27 -> ExitButton
        13 -> EnterButton
        _ -> (Basics.identity,Nothing) |> KeyChange

keyChange : Bool -> Keyboard.KeyCode -> Msg
keyChange on keyCode =
    ((case keyCode of

        65 ->
            \k -> { k | left = on }

        68 ->
            \k -> { k | right = on }

        87 ->
            \k -> { k | up = on }

        83 ->
            \k -> { k | down = on }

        109 ->
            \k -> {k | minus = on }

        107 ->
            \k -> {k | plus = on }

        16 ->
            \k -> {k | shift = on }
        _ ->
            Basics.identity
    ),
    (case keyCode of
        109 -> if on then Just False else Nothing
        107 -> if on then Just True else Nothing
        _ -> Nothing
    ))
        |> KeyChange


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


angleBetween : Vec3 -> Vec3 -> Float
angleBetween d v = atan2((getX d) * (getY v) - (getX v) * (getY d)) ((getX d) * (getX v) + (getY d) * (getY v))

makeRotateBetween : Vec3 -> Vec3 -> Mat4
makeRotateBetween v1 v2 =
    let
        d = normalize v1
        v = normalize v2
        angle = angleBetween d v
    in
        makeRotate angle (vec3 0 0 1)

calculateDirection : Position -> Size -> Mat4
calculateDirection pos wsize =
    let
        center = vec3 (toFloat (wsize.width) / 2.0) (toFloat (wsize.height) / 2.0) 0
        target = vec3 (toFloat (pos.x)) (toFloat (pos.y)) 0
        d = normalize (Math.Vector3.sub target center)
        v = vec3 0 1 0
    in
        makeRotateBetween d v

makeWorldTranslation : Float -> Vec3 -> Float -> { x : Int, y : Int } -> Mat4 -> Mat4
makeWorldTranslation gameSpeed pos dt {x,y} lookAt =
    let
        d = vec3 (toFloat x) (toFloat y) 0
        dir = if length d > 1 then normalize d else d
    in
        makeTranslate (add pos (Math.Vector3.scale ((gameSpeed* dt)/10000) (transform lookAt dir)))

port scoreBoard : (Scores -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
    [ AnimationFrame.diffs Animate
    , Keyboard.downs (keyChange True)
    , Keyboard.ups (keyChange False)
    , Keyboard.downs keyPressed
    , Mouse.clicks(mouseClicks)
    , Window.resizes WindowSizeSuccess
    , Mouse.moves (\{ x, y } -> UpdateMouse { x = x, y = y })
    , scoreBoard FetchSucceed
    ]
        |> Sub.batch


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MESHES

getFrame : Int -> List ( Int, Int ) -> Int
getFrame i list =
    case list of
        x::xs -> if i < snd x then fst x else getFrame (i - snd x) xs
        []    -> 0

animatedSprite : Actor -> Drawable { pos : Vec3, coord : Vec3 }
animatedSprite actor =
    let
        cols =
            let
                framedTexture = Dict.get actor.texture framedTextures
            in
                case framedTexture of
                    Nothing -> 1.0
                    Just x -> if snd x then 1.0/(toFloat (fst x)) else toFloat (fst x)

        anim = actor.animation

        frame =
            let
                frames = Dict.get (actor.texture ++ "-" ++ anim.name) animationDict
            in
                case frames of
                    Nothing -> 0.0
                    Just x ->
                        let
                            fs = x.frames
                        in toFloat (getFrame anim.current fs)

        start = frame / cols
        end = start + (1.0 / cols)
        rows = if cols < 1 then end else 1

        spriteSize = actor.size
        topLeft =
            { pos = vec3 -spriteSize spriteSize 0, coord = vec3 start rows 0 }

        topRight =
            { pos = vec3 spriteSize spriteSize 0, coord = vec3 end rows 0 }

        bottomLeft =
            { pos = vec3 -spriteSize -spriteSize 0, coord = vec3 start 0 0 }

        bottomRight =
            { pos = vec3 spriteSize -spriteSize 0, coord = vec3 end 0 0 }

        result  =
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]
    in
        Triangle result



-- VIEW


camera : Mat4 -> Mat4
camera worldTransformation =
    mul
        (makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0))
        worldTransformation



perspective : Size -> Mat4
perspective wsize = perspectiveMatrix ((toFloat wsize.width) / (toFloat wsize.height))

perspectiveMatrix : Float -> Mat4
perspectiveMatrix ratio =
    makePerspective 45 ratio 0.01 100


addActorToScene : Actor -> Model -> List Renderable
addActorToScene actor model =
  case Dict.get actor.texture model.textures of
    Nothing -> []
    Just texture ->
      [ render vertexShader
        fragmentShader
        ( animatedSprite actor)
        { tex = texture
        , perspective =
            List.foldr mul
                Math.Matrix4.identity
                [

                  perspective model.wsize
                , if actor.actorType == Player then makeTranslate actor.renderPosition else makeTranslate actor.renderPosition
                , actor.rotation
                , makeTranslate actor.spriteCentering
                ]
        }
    ]
addActorsToScene : Model -> List Renderable
addActorsToScene model =
    let
        am = model.actorManager
        list = Dict.toList am
        sortFunc x =
            let actor = snd x
            in actor.index
        sortedList = List.sortBy sortFunc list
        aMap x = addActorToScene (snd x) model
        mappedList = List.map aMap sortedList
    in
        List.foldl (++) [] (mappedList)

mainMenu : Html Msg
mainMenu =
    div [] [ div [id "menuOverlay"] []
            , div [id "menuBg"] []
            , div [id "play", class "first", onClick (ChangeStatus EnterName), onMouseEnter (PlaySound "beep")] []
            , div [id "options", class "second", onClick (ChangeStatus Options), onMouseEnter (PlaySound "beep")] []
            , div [id "credits", class "third", onClick (ChangeStatus Credits), onMouseEnter (PlaySound "beep")] []
            ]
optionsMenu : Html Msg
optionsMenu =
    div [] [ div [id "menuOverlay"] []
            , div [id "menuBg"] []
            , div [id "difficulty", class "first", onClick (ChangeStatus Difficulty), onMouseEnter (PlaySound "beep")] []
            , div [id "speed", class "second", onClick (ChangeStatus Speed), onMouseEnter (PlaySound "beep")] []
            , div [id "back", class "third", onClick (ChangeStatus MainMenu), onMouseEnter (PlaySound "beep")] []
            ]

speedMenu : Html Msg
speedMenu =
    div [] [ div [id "menuOverlay"] []
            , div [id "menuBg"] []
            , div [id "fast", class "first", onClick (SetGameSpeed 10), onMouseEnter (PlaySound "beep")] []
            , div [id "normal", class "second", onClick (SetGameSpeed 7), onMouseEnter (PlaySound "beep")] []
            , div [id "back", class "third", onClick (ChangeStatus Options), onMouseEnter (PlaySound "beep")] []
            ]

difficultyMenu : Html Msg
difficultyMenu =
    div [] [ div [id "menuOverlay"] []
            , div [id "menuBg"] []
            , div [id "hard", class "first", onClick (SetGameDifficulty True), onMouseEnter (PlaySound "beep")] []
            , div [id "normal", class "second", onClick (SetGameDifficulty False), onMouseEnter (PlaySound "beep")] []
            , div [id "back", class "third", onClick (ChangeStatus Options), onMouseEnter (PlaySound "beep")] []
            ]

credits : Html Msg
credits =
    div [] [ div [id "menuOverlay"] []
            , div [id "creditlist", onClick (ChangeStatus MainMenu)]
                [ div [id "creditsheader"] [ text "CREDITS"]
                , div [] [ text "programming - Ilya Bolotin"]
                , div [] [ text "ui design - Ilya Bolotin"]
                , div [] [ text "textures - Tatermand"]
                , div [] [ text "music - "
                         , a [ href "https://soundcloud.com/alexandr-zhelanov" ] [ text "Alexandr Zhelanov" ]]
                , div [] [ text "sound effects - "
                         , a [ href "http://productioncrate.com" ] [ text "ProductionCrate" ] ]

                ]
            ]


highscore : Model -> Html Msg
highscore model =
    let
        divMap s = div [] [ text (fst s ++ " - " ++ ( toString (snd s) )) ]
    in
        div [] [ div [id "menuOverlay"] []
                , div [id "creditlist", onClick (ChangeStatus MainMenu)]
                    ([ (div [id "creditsheader"] [ text "HIGH SCORES"]) ] ++ if List.isEmpty model.scoreList
                                                                                then [div [] [ text "Can't connect to the scoreboard server!" ]]
                                                                                else (List.map divMap model.scoreList))
                ]


game : Model -> Html Msg
game model =
    let
      player = getCharacterAttributes model playerActor
    in
             div []   ([ ( addActorsToScene model ) |> WebGL.toHtmlWith [ BlendFunc ( SrcAlphaSaturate , DstAlpha), Enable Blend ] [ width model.wsize.width, height model.wsize.height  ]
                      , img [src "texture/healthIcon.png", id "healthicon"] []
                      , div [id "health"] [text (toString player.health)]
                      , div [id "score"] [text (toString player.score)]
                      , div [id "blood"] []
                      , img [src (if player.rockets > 0 then "texture/iconGrenadeActive.png" else "texture/iconGrenade.png"), class "rocket", id "rocket1"] []
                      , img [src (if player.rockets > 1 then "texture/iconGrenadeActive.png" else "texture/iconGrenade.png"), class "rocket", id "rocket2"] []
                      , img [src (if player.rockets > 2 then "texture/iconGrenadeActive.png" else "texture/iconGrenade.png"), class "rocket", id "rocket3"] []
                      ] ++ (if model.startCounter > 0
                                then
                                    let c = Basics.floor(model.startCounter / 1000)
                                    in
                                        if c == 0
                                            then [div [id "startCounter"] [ text "GO!" ]]
                                            else [div [id "startCounter"] [ text (Basics.toString c) ]]
                                else []))


view : Model -> Html Msg
view model =
      let
            player = getCharacterAttributes model playerActor
      in
        case model.status of
            GameOver ->
                 div [] [div [id "menuOverlay", onClick FetchScore] [], div [id "gameover", onClick FetchScore] [text "GAMEOVER. No continues left."], div [id "gameoverDesc"] [text "Press Esc or click anywhere to return to main menu."]]
            MainMenu ->
                mainMenu
            Credits ->
                credits
            Options ->
                optionsMenu
            Speed ->
                speedMenu
            Difficulty ->
                difficultyMenu
            Help ->
                div [] [ div [id "menuOverlay"] [], div [id "help", onClick EnterButton ] [] ]

            EnterName ->
                div [] [ div [id "menuOverlay"] []
                        , input [placeholder "enter your name", id "nameForm", onInput SetName ] []
                        , button [id "nameButton", onClick EnterButton, onMouseEnter (PlaySound "beep")] []]
            Won ->
                div [] [ div [id "menuOverlay"] []
                        , div [id "menuOverlay", onClick FetchScore] [], div [id "gameover", onClick FetchScore] [text "You've won!"], div [id "gameoverDesc"] [text "Press Esc or click anywhere to return to main menu."]]
            Highscore ->
                highscore model
            WaitingForHighscore ->
                div [] [ div [id "menuOverlay"] []
                        , div [id "gameoverDesc"] [text "Retrieving High Scores"]]
            Game ->
                game model


-- SHADERS

vertexShader : Shader { pos : Vec3, coord : Vec3 } { u | perspective : Mat4 } { vcoord : Vec2 }
vertexShader =
    [glsl|

attribute vec3 pos;
attribute vec3 coord;
uniform mat4 perspective;
varying vec2 vcoord;

void main () {
  gl_Position = perspective * vec4(pos, 1.0);
  vcoord = coord.xy;
}

|]


fragmentShader : Shader {} { u | tex : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|

precision mediump float;
uniform sampler2D tex;
varying vec2 vcoord;

void main () {

    gl_FragColor  =  texture2D(tex, vcoord);

}

|]
