port module Actor exposing (..)

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
import Math2d exposing (..)
import Textures exposing (..)

intList : Int -> Random.Generator (List Int)
intList i =
    Random.list i (Random.int -100 100)


setActorVectorPair : Actor -> Float -> Float -> Float -> ( Actor, Vec3 )
setActorVectorPair actor x y z =
    ( actor, vec3 x y z )


randomVectorForActor : Actor -> Random.Generator ( Actor, Vec3 )
randomVectorForActor actor =
    Random.map3 (setActorVectorPair actor) (Random.float -1 1) (Random.float -1 1) (Random.float -1 1)


randomEnemyPositions : Int -> Random.Generator (List ( Float, Float ))
randomEnemyPositions i =
    Random.list i (Random.pair (Random.float -1.66 1.66) (Random.float -1.66 1.66))


getPlayerActor : Model -> Actor
getPlayerActor model =
    case Dict.get playerActor model.actorManager of
        Nothing ->
            templatePlayerActor playerActor

        Just x ->
            x


getCharacterAttributes : Model -> String -> CharacterAttributes
getCharacterAttributes model actorName =
    let
        player =
            getPlayerActor model
    in
        case player.characterAttributes of
            Nothing ->
                { health = 100
                , rockets = 0
                , score = 0
                , rateOfFire = 300
                , timeSinceLastFire = 0
                }

            Just x ->
                x



-- very primitive AI for enemies. Moves around the player


ai : Actor -> Model -> ( Vec3, Cmd Msg )
ai actor model =
    case Dict.get playerActor model.actorManager of
        Nothing ->
            ( vec3 0 0 0, Cmd.none )

        Just x ->
            case actor.movesTo of
                Nothing ->
                    ( x.position, Task.perform SoundError GetRandomDirectionVector (succeed actor) )

                Just mov ->
                    if
                        Math.Vector3.length (Math.Vector3.sub x.position mov)
                            < 0.5
                            || Math.Vector3.length (Math.Vector3.sub actor.position mov)
                            < 0.5
                            || actor.lastChange
                            > 5000
                    then
                        ( mov, Task.perform SoundError GetRandomDirectionVector (succeed actor) )
                    else
                        ( mov, Cmd.none )


updateActorManagerDict : String -> Actor -> ActorManager -> ActorManager
updateActorManagerDict key target am =
    let
        newAm =
            Dict.remove key am
    in
        Dict.insert key target newAm


affectActorPairs : String -> List String -> ActorManager -> ActorManager
affectActorPairs key targetKeys am =
    case targetKeys of
        tkey :: list ->
            case Dict.get key am of
                Nothing ->
                    am

                Just actor ->
                    case Dict.get tkey am of
                        Nothing ->
                            am

                        Just target ->
                            let
                                affectedPair =
                                    case actor.collision of
                                        Nothing ->
                                            ( actor, target )

                                        Just x ->
                                            case target.collision of
                                                Nothing ->
                                                    ( actor, target )

                                                Just y ->
                                                    affectActorPair ( x, y ) ( actor, target )

                                newAm =
                                    updateActorManagerDict key (fst affectedPair) am
                            in
                                affectActorPairs key list (updateActorManagerDict tkey (snd affectedPair) newAm)

        [] ->
            am


affectActorPair : ( Collision, Collision ) -> ( Actor, Actor ) -> ( Actor, Actor )
affectActorPair ( Collision c1, Collision c2 ) ( source, target ) =
    let
        collide v1 r1 v2 r2 =
            (Math.Vector3.getX v2 - Math.Vector3.getX v1) ^ 2 + (Math.Vector3.getY v2 - Math.Vector3.getY v1) ^ 2 <= ((r1 + r2) / 3) ^ 2

        effect =
            collide source.position source.size target.position target.size

        a1 =
            if effect then
                c2.effectOnTarget (c1.effectOnSelf source)
            else
                source

        a2 =
            if effect then
                c1.effectOnTarget (c2.effectOnSelf target)
            else
                target
    in
        ( a1, a2 )

setRandoms : List Int -> List String -> ActorManager -> ActorManager
setRandoms xs enemies am =
    case xs of
        i :: l ->
            case enemies of
                enemy :: elist ->
                    case Dict.get enemy am of
                        Nothing ->
                            setRandoms xs elist am

                        Just x ->
                            if x.actorType == NPC then
                                let
                                    ca =
                                        case x.characterAttributes of
                                            Just y ->
                                                Just { y | rateOfFire = (i * 2 + y.rateOfFire) }

                                            Nothing ->
                                                Nothing

                                    newAm =
                                        updateActorManagerDict (log ("Setting Random " ++ (toString i) ++ " for") x.key)
                                            { x
                                                | randomVal = clamp 0 (Basics.round (((toFloat i) + 100.0) / 2.0)) 100
                                                , characterAttributes = ca
                                            }
                                            am
                                in
                                    setRandoms l elist newAm
                            else
                                setRandoms xs elist am

                [] ->
                    am

        [] ->
            am

bounceFromList : Actor -> List ( String, Actor ) -> Vec3
bounceFromList a am =
    case am of
        x :: xs ->
            let
                target =
                    snd x

                acceptableDistance =
                    2 * (target.size + a.size)

                blockingDistance =
                    (target.size + a.size)

                diff =
                    acceptableDistance - blockingDistance

                currentDistance =
                    vlength (vsub target.position a.position)

                currentDiff =
                    currentDistance - blockingDistance

                bounceVector =
                    vnormalize (vsub a.position target.position)

                c =
                    if currentDistance > acceptableDistance then
                        0
                    else if currentDistance < blockingDistance then
                        1
                    else
                        currentDiff / diff
            in
                Math.Vector3.add bounceVector (bounceFromList a xs)

        [] ->
            vec3 0 0 0


bounceFromBounds : Actor -> Vec3
bounceFromBounds a =
    let
        x =
            Math.Vector3.getX a.position

        y =
            Math.Vector3.getY a.position

        acceptableDistance =
            0.7

        blockingDistance =
            0.35

        --diff = acceptableDistance - blockingDistance
        currentX =
            if x > 0 then
                10 - x
            else
                10 + x

        currentY =
            if x > 0 then
                10 - y
            else
                10 + y

        norm x =
            if x > acceptableDistance then
                log a.key 0
            else if x < blockingDistance then
                log a.key 1
            else
                log a.key ((acceptableDistance - x) / blockingDistance)

        bounceVectorX =
            if x > 0 then
                vscale (norm currentX) (vec3 -1 0 0)
            else
                vscale (norm currentX) (vec3 1 0 0)

        bounceVectorY =
            if y > 0 then
                vscale (norm currentY) (vec3 0 -1 0)
            else
                vscale (norm currentY) (vec3 0 1 0)
    in
        log ("bounce." ++ a.key) (vadd bounceVectorX bounceVectorY)


bounce : Bool -> Actor -> Vec3 -> ActorManager -> Actor
bounce blocked a newPosition am =
    let
        movement =
            vsub newPosition a.position

        movementLength =
            vlength movement

        movementVector =
            if movementLength < 0.0001 then
                vec3 0 0 0
            else
                (vnormalize movement)

        bounceVector =
            if blocked then
                vadd (bounceFromBounds a) (bounceFromList a (Dict.toList am))
            else
                bounceFromBounds a

        resultingVector =
            if movementLength < 0.0001 then
                vec3 0 0 0
            else
                vscale movementLength (vnormalize (vadd movementVector bounceVector))
    in
        { a | position = log ("bounce2." ++ a.key) (vadd a.position resultingVector) }


waveEnemiesNumber : Int -> Int
waveEnemiesNumber n =
    10 + 5 * n


sector : Vec3 -> ( Int, Int )
sector v =
    ( (Basics.round ((Math.Vector3.getX v) / 3.34)), (Basics.round ((Math.Vector3.getY v) / 3.34)) )


positionInSector : ( Int, Int ) -> ( Float, Float ) -> Vec3
positionInSector ( x, y ) ( vx, vy ) =
    vec3 ((toFloat x) * 3.33 + vx) ((toFloat x) * 3.33 + vy) -4.99


generateEnemyInSector : Model -> ( Int, Int ) -> ( Float, Float ) -> Int -> ActorManager -> ActorManager
generateEnemyInSector model sector position rate am =
    let
        name =
            log "Generating" ("Enemy" ++ (toString model.enemyCounter))

        enemyPosition =
            log "Generating at position" (positionInSector sector position)
    in
        Dict.insert name (templateEnemyActor model.gameSpeed name enemyPosition rate) am


waveGenerator : Model -> Int -> List ( Float, Float ) -> List Int -> Model
waveGenerator model sec list rates =
    let
        am =
            model.actorManager

        player =
            getPlayerActor model

        playerSector =
            sector player.position

        currentSector =
            ( ((sec % 7) - 3), ((sec // 7) - 3) )

        nextSector =
            (sec + 1) % 48
    in
        if playerSector == currentSector then
            waveGenerator model nextSector list rates
        else
            case list of
                x :: xs ->
                    case rates of
                        r :: rs ->
                            let
                                newAm =
                                    generateEnemyInSector model currentSector x r model.actorManager
                            in
                                waveGenerator
                                    { model
                                        | actorManager = newAm
                                        , enemyCounter = model.enemyCounter + 1
                                    }
                                    nextSector
                                    xs
                                    rs

                        [] ->
                            let
                                newAm =
                                    generateEnemyInSector model currentSector x 50 model.actorManager
                            in
                                waveGenerator
                                    { model
                                        | actorManager = newAm
                                        , enemyCounter = model.enemyCounter + 1
                                    }
                                    nextSector
                                    xs
                                    rates

                [] ->
                    { model | actorManager = am }


checkCollisions : String -> Actor -> Vec3 -> ActorManager -> ActorManager
checkCollisions actorKey actor position am =
    let
        collide v1 r1 v2 r2 =
            (Math.Vector3.getX v2 - Math.Vector3.getX v1) ^ 2 + (Math.Vector3.getY v2 - Math.Vector3.getY v1) ^ 2 <= ((r1 + r2) * 2) ^ 2

        -- two circles collision
        collisionFilter key a =
            -- check if path is blocked
            case a.collision of
                Nothing ->
                    False

                Just x ->
                    let
                        (Collision y) =
                            x
                    in
                        y.blocking
                            && collide actor.position actor.size a.position a.size

        -- does it collide with first Actor?
        dict =
            Dict.remove actorKey am

        -- removing Actor from list (don't need to check collisions on self)
        dicts =
            Dict.partition collisionFilter dict

        -- partitioning Dict into two Dicts: fst - blocking Actors, snd - overlapping Actor
        blocked =
            case actor.collision of
                Nothing ->
                    False

                Just x ->
                    let
                        (Collision c) =
                            x
                    in
                        (Dict.size (fst dicts)) /= 0 && c.blocking

        -- is the path blocked?
        nactor =
            bounce (blocked && actor.actorType /= Player) actor position (fst dicts)
    in
        affectActorPairs actorKey (Dict.keys (snd dicts)) (updateActorManagerDict actorKey nactor (am))


updateActorManagerList : Float -> Model -> ActorManager -> List ( String, Actor ) -> ActorManager
updateActorManagerList dt model am amList =
    case amList of
        a :: list ->
            let
                act =
                    snd a

                key =
                    fst a

                player =
                    case (Dict.get playerActor am) of
                        Nothing ->
                            act

                        Just x ->
                            x

                getCA p =
                    case p.characterAttributes of
                        Nothing ->
                            { health = 100
                            , rockets = 0
                            , score = 0
                            , rateOfFire = 300
                            , timeSinceLastFire = 0
                            }

                        Just x ->
                            x
            in
                case act.actorType of
                    Player ->
                        let
                            actor =
                                case (Dict.get key am) of
                                    Nothing ->
                                        act

                                    Just x ->
                                        x

                            dirs =
                                directions model.keys

                            moving =
                                if dirs.x /= 0 || dirs.y /= 0 then
                                    Just (normalize (vec3 (toFloat dirs.x) (toFloat dirs.y) 0))
                                else
                                    Nothing

                            pos =
                                vec3 (cl -10 (Math.Vector3.getX actor.position) 10) (cl -10 (Math.Vector3.getY actor.position) 10) (Math.Vector3.getZ actor.position)

                            worldTransformationMatrix =
                                makeWorldTranslation model.gameSpeed pos dt dirs model.lookAt

                            newPosition =
                                transform worldTransformationMatrix (vec3 0 0 0)

                            newAm =
                                checkCollisions key actor newPosition am

                            checkedActor =
                                case (Dict.get key newAm) of
                                    Nothing ->
                                        actor

                                    Just x ->
                                        x

                            checkedCharacterAttributes =
                                getCA checkedActor

                            newCharacterAttributes =
                                { checkedCharacterAttributes | timeSinceLastFire = checkedCharacterAttributes.timeSinceLastFire + (floor dt) }

                            newActor =
                                { actor
                                    | animation = animateSprite False (dt * (model.gameSpeed / 10)) moving checkedActor.texture checkedActor.animation
                                    , timeToLive =
                                        if newCharacterAttributes.health <= 0 then
                                            Just 0
                                        else
                                            checkedActor.timeToLive
                                    , characterAttributes = Just newCharacterAttributes
                                    , rotation = model.lookAt
                                    , position = log "POS" newPosition
                                    , worldTransformationMatrix = worldTransformationMatrix
                                }
                        in
                            updateActorManagerList dt model (updateActorManagerDict key newActor newAm) list

                    NPC ->
                        let
                            actor =
                                (case (Dict.get key am) of
                                    Nothing ->
                                        act

                                    Just x ->
                                        x
                                )

                            dirs =
                                { x = 0, y = 1 }

                            lookAt =
                                makeRotateBetween (vec3 0 -1 0) (Math.Vector3.sub player.position actor.position)

                            ( v, cmd ) =
                                ai actor model

                            translationVector =
                                case actor.movesTo of
                                    Nothing ->
                                        Math.Vector3.scale ((actor.speed * model.gameSpeed * dt) / 10000) (Math.Vector3.normalize (Math.Vector3.sub v actor.position))

                                    Just x ->
                                        Math.Vector3.scale ((actor.speed * model.gameSpeed * dt) / 10000) (Math.Vector3.normalize (Math.Vector3.sub x actor.position))

                            translation =
                                makeTranslate translationVector

                            tPosition =
                                transform translation actor.position

                            worldTranslation =
                                makeTranslate <| vec3 -(Math.Vector3.getX player.position) -(Math.Vector3.getY player.position) 0

                            difflen =
                                length (Math.Vector3.sub tPosition actor.position)

                            ( movesTo, newPosition ) =
                                case actor.movesTo of
                                    Nothing ->
                                        ( Just v, actor.position )

                                    Just x ->
                                        if difflen > (length (Math.Vector3.sub x tPosition)) then
                                            ( Just v, x )
                                        else
                                            ( Just x, tPosition )

                            renderPosition =
                                transform worldTranslation tPosition

                            distance =
                                length (Math.Vector3.sub newPosition player.position)

                            newAm =
                                checkCollisions key actor newPosition am

                            checkedActor =
                                case (Dict.get key newAm) of
                                    Nothing ->
                                        { actor | movesTo = movesTo }

                                    Just x ->
                                        { x | renderPosition = renderPosition }

                            characterAttributes =
                                case checkedActor.characterAttributes of
                                    Nothing ->
                                        Nothing

                                    Just x ->
                                        Just { x | timeSinceLastFire = x.timeSinceLastFire + (floor dt) }

                            fireCommand =
                                case characterAttributes of
                                    Nothing ->
                                        Nothing

                                    Just x ->
                                        if distance < 5 && x.rateOfFire <= x.timeSinceLastFire then
                                            Just
                                                (Cmd.batch
                                                    [ cmd
                                                    , (succeed ( checkedActor.position, checkedActor.rotation, checkedActor )) |> Task.perform SoundError Fire
                                                    , (succeed "shot") |> Task.perform SoundError PlaySound
                                                    ]
                                                )
                                        else
                                            Nothing

                            newCharacterAttributes =
                                case characterAttributes of
                                    Nothing ->
                                        Nothing

                                    Just x ->
                                        case fireCommand of
                                            Nothing ->
                                                characterAttributes

                                            Just t ->
                                                Just { x | timeSinceLastFire = 0 }

                            isShooting =
                                case fireCommand of
                                    Nothing ->
                                        False

                                    _ ->
                                        True

                            newActor =
                                { checkedActor
                                    | animation = animateSprite isShooting (dt * (model.gameSpeed / 10)) movesTo checkedActor.texture checkedActor.animation
                                    , rotation = lookAt
                                    , movesTo = movesTo
                                    , characterAttributes = newCharacterAttributes
                                    , lastChange = checkedActor.lastChange + dt
                                }

                            ( tActor, fireCommand2 ) =
                                case newCharacterAttributes of
                                    Nothing ->
                                        ( newActor, Nothing )

                                    Just x ->
                                        if x.health <= 0 then
                                            case checkedActor.onDeath of
                                                Nothing ->
                                                    ( newActor, Nothing )

                                                Just (OnAction f) ->
                                                    f.func newActor
                                        else
                                            ( newActor, Nothing )

                            cmds =
                                case fireCommand of
                                    Nothing ->
                                        case fireCommand2 of
                                            Nothing ->
                                                Nothing

                                            _ ->
                                                fireCommand2

                                    Just x ->
                                        case fireCommand2 of
                                            Nothing ->
                                                fireCommand

                                            Just k ->
                                                Just (Cmd.batch [ k, x ])

                            outActor =
                                { tActor
                                    | timeToLive =
                                        case tActor.timeToLive of
                                            Nothing ->
                                                Nothing

                                            Just x ->
                                                Just (Basics.max 0 (x - (Basics.round dt)))
                                    , fireCommand = cmds
                                }
                        in
                            updateActorManagerList dt model (updateActorManagerDict key outActor newAm) list

                    Object ->
                        let
                            actor =
                                (case (Dict.get key am) of
                                    Nothing ->
                                        act

                                    Just x ->
                                        x
                                )

                            dirs =
                                { x = 0, y = 1 }

                            ( destination, translationVector ) =
                                case actor.movesTo of
                                    Nothing ->
                                        if actor.moves then
                                            ( False, Math.Vector3.scale ((actor.speed * model.gameSpeed * dt) / 10000) (Math.Vector3.normalize (transform actor.rotation (vec3 0 -1 0))) )
                                        else
                                            ( False, vec3 0 0 0 )

                                    Just x ->
                                        let
                                            direction =
                                                Math.Vector3.normalize (Math.Vector3.sub x actor.position)

                                            dest =
                                                Math.Vector3.length (Math.Vector3.sub x actor.position) <= ((actor.speed * model.gameSpeed * dt) / 10000)
                                        in
                                            ( dest, Math.Vector3.scale ((actor.speed * model.gameSpeed * dt) / 10000) direction )

                            translation =
                                makeTranslate translationVector

                            tPosition =
                                transform translation actor.position

                            worldTranslation =
                                makeTranslate <| vec3 -(Math.Vector3.getX player.position) -(Math.Vector3.getY player.position) 0

                            renderPosition =
                                transform worldTranslation tPosition

                            ( tActor, fireCommand ) =
                                if destination then
                                    case actor.onDestination of
                                        Nothing ->
                                            ( actor, Nothing )

                                        Just (OnAction f) ->
                                            f.func actor
                                else
                                    ( actor, Nothing )

                            newActor =
                                { tActor
                                    | renderPosition = renderPosition
                                    , animation = animateSprite False (dt * (model.gameSpeed / 10)) Nothing tActor.texture tActor.animation
                                    , position = tPosition
                                    , timeToLive =
                                        case tActor.timeToLive of
                                            Nothing ->
                                                Nothing

                                            Just x ->
                                                Just (Basics.max 0 (x - (Basics.round dt)))
                                    , fireCommand = fireCommand
                                }
                        in
                            updateActorManagerList dt model (updateActorManagerDict key newActor am) list

        [] ->
            am


updateActorManager : Float -> Model -> ActorManager
updateActorManager dt model =
    let
        amlist =
            Dict.toList model.actorManager
    in
        updateActorManagerList dt model model.actorManager amlist


fireCommandsAndkillActors : ActorManager -> ( ActorManager, Cmd Msg )
fireCommandsAndkillActors am =
    let
        getOnActorKilled ( key, actor ) =
            (case actor.fireCommand of
                Nothing ->
                    []

                Just x ->
                    [ x ]
            )
                ++ (case actor.timeToLive of
                        Nothing ->
                            []

                        Just x ->
                            if x == 0 then
                                case actor.onActorKilled of
                                    Nothing ->
                                        []

                                    Just (OnActorKilled x) ->
                                        [ x.onActorKilled actor ]
                            else
                                []
                   )

        filterKilled key actor =
            case actor.timeToLive of
                Nothing ->
                    True

                Just x ->
                    x /= 0

        cmd =
            Cmd.batch (List.concat (List.map getOnActorKilled (Dict.toList am)))

        newam =
            Dict.filter filterKilled am
    in
        ( newam, cmd )


playerActor : String
playerActor =
    "Player1"

animatedSprite : Float -> Drawable { pos : Vec3, coord : Vec3 }
animatedSprite s =
    let
        spriteSize = s
        topLeft =
            { pos = vec3 -spriteSize spriteSize 0, coord = vec3 0 1 0 }

        topRight =
            { pos = vec3 spriteSize spriteSize 0, coord = vec3 1 1 0 }

        bottomLeft =
            { pos = vec3 -spriteSize -spriteSize 0, coord = vec3 0 0 0 }

        bottomRight =
            { pos = vec3 spriteSize -spriteSize 0, coord = vec3 1 0 0 }

        result =
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]
    in
        Triangle result

templatePlayerActor : String -> Actor
templatePlayerActor name =
    { key = name
    , actorType = Player
    , actorSubType = Nothing
    , characterAttributes =
        Just
            { health = 100
            , rockets = 3
            , score = 0
            , rateOfFire = 10000
            , timeSinceLastFire = 0
            }
        --, children = Children []
    , index = 7
    , size = 0.35
    , speed = 0.2
    , randomVal = 0
    , texture = "assault"
    , actorSprite = animatedSprite 0.35
    , animation =
        { name = "idle"
        , current = 0
        , end = 100
        }
    , position = vec3 0 0 -4.99
    , renderPosition = vec3 0 0 -4.99
    , spriteCentering = vec3 -0.02 -0.17 0
    , rotation = Math.Matrix4.identity
    , worldTransformationMatrix = Math.Matrix4.identity
    , movesTo = Nothing
    , lastChange = 0
    , moves = True
    , timeToLive = Nothing
    , collision =
        Just
            (Collision
                { blocking = True
                , effectOnTarget = Basics.identity
                , effectOnSelf = Basics.identity
                }
            )
    , affectsCamera = Nothing
    , onDeath = Nothing
    , onDestination = Nothing
    , onActorKilled = Just (OnActorKilled { onActorKilled = gameOver })
    , fireCommand = Nothing
    }


gameOver : Actor -> Cmd Msg
gameOver actor =
    succeed GameOver |> Task.perform StatusError ChangeStatus


addScoreAndDropCrate : Actor -> Cmd Msg
addScoreAndDropCrate actor =
    Cmd.batch
        [ Task.perform SoundError AddScore (succeed 100)
        , Task.perform SoundError GenerateDropCrate (succeed ( actor.randomVal, actor.position ))
        ]


explode : Actor -> ( Actor, Maybe (Cmd Msg) )
explode act =
    ( { act
        | timeToLive = Just 800
        , texture = "explosion"
        , actorSprite = animatedSprite 1.5
        , animation =
            { name = "idle"
            , current = 0
            , end = 800
            }
        , moves = False
        , movesTo = Nothing
        , size = 1.5
      }
    , Just (Cmd.batch [ Task.perform SoundError Explosion (succeed act.position), (succeed "explosion") |> Task.perform SoundError PlaySound ])
    )


death : Actor -> ( Actor, Maybe (Cmd Msg) )
death act =
    ( { act
        | timeToLive = Just 400
        , actorType = Object
        , texture = "death"
        , actorSprite = animatedSprite 0.2
        , animation =
            { name = "idle"
            , current = 0
            , end = 400
            }
        , moves = False
        , movesTo = Nothing
        , size = 0.2
      }
    , Nothing
    )


templateEnemyActor : Float -> String -> Vec3 -> Int -> Actor
templateEnemyActor gameSpeed name p rates =
    { key = name
    , actorType = NPC
    , actorSubType = Nothing
    , characterAttributes =
        Just
            { health = 100
            , rockets = 0
            , score = 0
            , rateOfFire = round ((10000 + toFloat rates) / gameSpeed)
            , timeSinceLastFire = 0
            }
    , index = 8
    , size = 0.25
    , speed = 0.8
    , randomVal = rates
    , texture = "assault"
    , actorSprite = animatedSprite 0.25
    , animation =
        { name = "idle"
        , current = 0
        , end = 100
        }
    , position = p
    , renderPosition = vec3 2 2 -4.99
    , spriteCentering = vec3 0 -0.2 0.15
    , rotation = Math.Matrix4.identity
    , worldTransformationMatrix = Math.Matrix4.identity
    , movesTo = Nothing
    , lastChange = 0
    , moves = False
    , timeToLive = Nothing
    , collision =
        Just
            (Collision
                { blocking = True
                , effectOnTarget = Basics.identity
                , effectOnSelf = Basics.identity
                }
            )
    , affectsCamera = Nothing
    , onDeath = Just (OnAction { func = death })
    , onDestination = Nothing
    , onActorKilled = Just (OnActorKilled { onActorKilled = addScoreAndDropCrate })
    , fireCommand = Nothing
    }


killBulletCollectable : Actor -> Actor
killBulletCollectable actor =
    case actor.actorSubType of
        Nothing ->
            actor

        Just x ->
            if x == Collectable || x == Bullet then
                { actor | timeToLive = (Just 0) }
            else
                actor


bulletHit : Int -> Actor -> Actor
bulletHit damage actor =
    let
        newCharacterAttributes =
            case actor.characterAttributes of
                Nothing ->
                    Nothing

                Just x ->
                    Just { x | health = Basics.clamp 0 (x.health - damage) 100 }
    in
        { actor | characterAttributes = newCharacterAttributes }


die : Actor -> Actor
die actor =
    { actor | timeToLive = Just 0 }


templateSlugActor : Float -> Int -> Vec3 -> Mat4 -> Int -> Actor
templateSlugActor gameSpeed i p r d =
    let
        name =
            toString i
    in
        { key = "slug-" ++ name
        , actorType = Object
        , actorSubType = Just Bullet
        , characterAttributes =
            Just
                { health = 100
                , rockets = 0
                , score = 0
                , rateOfFire = 600
                , timeSinceLastFire = 0
                }
            --, children = Children []
        , index = 9
        , size = 0.2
        , texture = "slug"
        , actorSprite = animatedSprite 0.2
        , randomVal = 0
        , animation =
            { name = "idle"
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
        , lastChange = 0
        , moves = True
        , timeToLive = Just (round (10000 / gameSpeed))
        , collision =
            Just
                (Collision
                    { blocking = False
                    , effectOnTarget = bulletHit d
                    , effectOnSelf = die
                    }
                )
        , affectsCamera = Nothing
        , onDeath = Nothing
        , onDestination = Nothing
        , onActorKilled = Nothing
        , fireCommand = Nothing
        }


templateRocketActor : Float -> Int -> Vec3 -> Vec3 -> Mat4 -> Actor
templateRocketActor gameSpeed i p t r =
    let
        name =
            toString i
    in
        { key = "slug-" ++ name
        , actorType = Object
        , actorSubType = Just Bullet
        , characterAttributes =
            Just
                { health = 100
                , rockets = 0
                , score = 0
                , rateOfFire = 600
                , timeSinceLastFire = 0
                }
            --, children = Children []
        , index = 9
        , size = 0.3
        , texture = "rocket"
        , actorSprite = animatedSprite 0.3
        , randomVal = 0
        , animation =
            { name = "idle"
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
        , lastChange = 0
        , moves = False
        , timeToLive = Just (round (30000 / gameSpeed))
        , collision = Nothing
        , affectsCamera = Nothing
        , onDeath = Nothing
        , onDestination = Just (OnAction { func = explode })
        , onActorKilled = Nothing
        , fireCommand = Nothing
        }


templateHealthActor : Float -> Int -> Vec3 -> Actor
templateHealthActor gameSpeed i p =
    let
        name =
            toString i
    in
        { key = "health-" ++ name
        , actorType = Object
        , actorSubType = Just Collectable
        , characterAttributes =
            Just
                { health = 100
                , rockets = 0
                , score = 0
                , rateOfFire = 600
                , timeSinceLastFire = 0
                }
        , index = 6
        , size = 0.1
        , texture = "healthCrate"
        , actorSprite = animatedSprite 0.1
        , randomVal = 0
        , animation =
            { name = "idle"
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
        , lastChange = 0
        , moves = False
        , timeToLive = Just (round (100000 / gameSpeed))
        , collision =
            Just
                (Collision
                    { blocking = False
                    , effectOnSelf = die
                    , effectOnTarget =
                        (\actor ->
                            (let
                                newCharacterAttributes =
                                    case actor.characterAttributes of
                                        Nothing ->
                                            Nothing

                                        Just x ->
                                            Just { x | health = Basics.clamp 0 (x.health + 100) 100 }
                             in
                                { actor | characterAttributes = newCharacterAttributes }
                            )
                        )
                    }
                )
        , affectsCamera = Nothing
        , onDeath = Nothing
        , onDestination = Nothing
        , onActorKilled = Nothing
        , fireCommand = Nothing
        }


templateRocketCrateActor : Float -> Int -> Vec3 -> Actor
templateRocketCrateActor gameSpeed i p =
    let
        name =
            toString i
    in
        { key = "rocketCrate-" ++ name
        , actorType = Object
        , actorSubType = Just Collectable
        , characterAttributes =
            Just
                { health = 100
                , rockets = 0
                , score = 0
                , rateOfFire = 600
                , timeSinceLastFire = 0
                }
        , index = 6
        , size = 0.1
        , texture = "rocketCrate"
        , actorSprite = animatedSprite 0.1
        , randomVal = 0
        , animation =
            { name = "idle"
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
        , lastChange = 0
        , moves = False
        , timeToLive = Just (round (100000 / gameSpeed))
        , collision =
            Just
                (Collision
                    { blocking = False
                    , effectOnSelf = die
                    , effectOnTarget =
                        (\actor ->
                            (let
                                newCharacterAttributes =
                                    case actor.characterAttributes of
                                        Nothing ->
                                            Nothing

                                        Just x ->
                                            Just { x | rockets = Basics.clamp 0 (x.rockets + 1) 3 }
                             in
                                { actor | characterAttributes = newCharacterAttributes }
                            )
                        )
                    }
                )
        , affectsCamera = Nothing
        , onDeath = Nothing
        , onDestination = Nothing
        , onActorKilled = Nothing
        , fireCommand = Nothing
        }


templateGroundActor : Actor
templateGroundActor =
    { key = "ground"
    , actorType = Object
    , actorSubType = Nothing
    , characterAttributes = Nothing
    , index = 10
    , size = 20
    , texture = "ground"
    , actorSprite = animatedSprite 20
    , randomVal = 0
    , animation =
        { name = "idle"
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
    , lastChange = 0
    , moves = False
    , timeToLive = Nothing
    , collision = Nothing
    , affectsCamera = Nothing
    , onDeath = Nothing
    , onDestination = Nothing
    , onActorKilled = Nothing
    , fireCommand = Nothing
    }
