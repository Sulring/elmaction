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
import Types exposing (..)
import Actor exposing (..)
import Textures exposing (..)
import Math2d exposing (..)

port music : String -> Cmd msg


port sound : String -> Cmd msg


port setBloodOpacity : Int -> Cmd msg


port getScoreBoard : ( String, Int ) -> Cmd msg


mainLoop : Model -> Float -> ( Model, Cmd Msg )
mainLoop model dt =
    if model.startCounter > 0 then
        let
            numerize f =
                Basics.round (f / 1000)

            changed =
                (numerize model.startCounter) /= (numerize (model.startCounter - dt))
        in
            if changed then
                update (PlaySound "beep2") { model | startCounter = model.startCounter - dt }
            else
                ( { model | startCounter = model.startCounter - dt }, Cmd.none )
    else
        let
            dirs =
                directions model.keys

            characterAttributes =
                getCharacterAttributes model playerActor

            actorManager =
                updateActorManager dt model

            actorsAlive =
                fireCommandsAndkillActors actorManager

            last_i =
                if counter > 5000 then
                    1
                else
                    model.last_i

            counter =
                if counter > 5000 then
                    0
                else
                    (model.counter + 1)
        in
            ( { model
                | actorManager = fst actorsAlive
                , counter = counter
                , last_i = last_i
              }
            , Cmd.batch [ (snd actorsAlive), setBloodOpacity characterAttributes.health ]
            )


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
            ( { model
                | wsize =
                    { width = s.width - 3
                    , height = s.height - 3
                    }
              }
            , Cmd.none
            )

        TexturesLoaded textures ->
            ( { model | textures = Dict.fromList textures }, Cmd.none )

        Animate dt ->
            if model.status == Game then
                let
                    filterEnemies key actor =
                        actor.actorType == NPC

                    enemiesAlive =
                        Dict.size (Dict.filter filterEnemies model.actorManager)
                in
                    if enemiesAlive > 0 then
                        mainLoop model dt
                    else
                        update GetRandomFireRate { model | waveCounter = model.waveCounter + 1 }
            else if model.status == WaitingForHighscore then
                if List.isEmpty model.scoreList && model.waiting < 5000 then
                    ( { model | waiting = model.waiting + dt }, Cmd.none )
                else
                    update (ChangeStatus Highscore) { model | waiting = 0 }
            else
                ( model, Cmd.none )

        UpdateMouse pos ->
            ( { model | mousePosition = pos, lookAt = (calculateDirection pos model.wsize) }, Cmd.none )

        KeyChange ( keyfunc, plus ) ->
            ( { model | keys = keyfunc model.keys }, Cmd.none )

        MouseClicks pos ->
            if model.status == Game then
                let
                    player =
                        getPlayerActor model

                    attr =
                        getCharacterAttributes model playerActor

                    renderPosition =
                        (Math.Vector3.add player.renderPosition (vec3 0 1 0))

                    rotation =
                        player.rotation

                    animation =
                        animateSprite True 1 Nothing player.texture player.animation

                    newPlayer =
                        { player | animation = animation }

                    am =
                        updateActorManagerDict playerActor newPlayer model.actorManager
                in
                    if model.keys.shift && attr.rockets > 0 then
                        ( { model | actorManager = am }
                        , (Cmd.batch
                            [ (succeed ( player.position, rotation, player )) |> Task.perform SoundError Fire
                            , (succeed "rocket") |> Task.perform SoundError PlaySound
                            ]
                          )
                        )
                    else if Basics.not model.keys.shift then
                        ( { model | actorManager = am }
                        , (Cmd.batch
                            [ (succeed ( player.position, rotation, player )) |> Task.perform SoundError Fire
                            , (succeed "shot") |> Task.perform SoundError PlaySound
                            ]
                          )
                        )
                    else
                        ( model, Cmd.none )
            else
                ( model, Cmd.none )

        PlayMusic s ->
            ( model, music s )

        FetchScore ->
            ( { model | status = WaitingForHighscore }, getScoreBoard ( model.playerName, model.score ) )

        SetName name ->
            ( { model | playerName = String.trim name }, Cmd.none )

        FetchSucceed list ->
            update (ChangeStatus WaitingForHighscore) { model | scoreList = list }

        AddScore i ->
            let
                player =
                    getPlayerActor model

                ca =
                    getCharacterAttributes model playerActor

                newModel =
                    { model
                        | actorManager =
                            updateActorManagerDict playerActor
                                { player
                                    | characterAttributes =
                                        (Just { ca | score = ca.score + i })
                                }
                                model.actorManager
                        , score = ca.score + i
                    }
            in
                ( newModel, Cmd.none )

        GetRandomDirectionVector actor ->
            ( model, Random.generate RandomDirectionVector (randomVectorForActor actor) )

        RandomDirectionVector ( actor, dir ) ->
            let
                player =
                    getPlayerActor model

                newActor =
                    { actor
                        | lastChange = 0
                        , movesTo = Just (Math.Vector3.add player.position (Math.Vector3.scale 3 (Math.Vector3.normalize (vec3 (Math.Vector3.getX dir) (Math.Vector3.getY dir) 0))))
                    }
            in
                ( { model | actorManager = updateActorManagerDict actor.key newActor model.actorManager }, Cmd.none )

        GetRandomFireRate ->
            ( model, Random.generate RandomFireRate (intList (waveEnemiesNumber model.waveCounter)) )

        RandomFireRate list ->
            update (GetRandomEnemyPositions list) model

        GetRandomEnemyPositions list ->
            ( model, Random.generate (RandomEnemyPositions list) (randomEnemyPositions (waveEnemiesNumber model.waveCounter)) )

        RandomEnemyPositions list xs ->
            let
                newModel =
                    waveGenerator model 0 xs list
            in
                ( { newModel | startCounter = 4999 }, Cmd.none )

        SoundError err ->
            ( model, Cmd.none )

        PlaySound s ->
            ( model, sound s )

        Fire ( position, direction, actor ) ->
            let
                player =
                    getPlayerActor model

                worldTranslation =
                    makeTranslate <| vec3 -(Math.Vector3.getX player.position) -(Math.Vector3.getY player.position) 0

                ndsx =
                    (toFloat model.mousePosition.x) / (toFloat model.wsize.width)

                ndsy =
                    (toFloat model.mousePosition.y) / (toFloat model.wsize.height)

                dy =
                    (ndsy * 2 - 1)

                dx =
                    (ndsx * 2 - 1)

                my =
                    dy * (-2)

                mx =
                    dx * 2 * ((toFloat model.wsize.width) / (toFloat model.wsize.height))

                vm =
                    vec3 mx my 0

                mousePosition =
                    vec3 dx dy -4.99

                target =
                    Math.Vector3.add player.position vm

                --target = rayCast model
                size =
                    actor.size

                slugint =
                    model.last_i + 1

                slugname =
                    "slug-" ++ (toString (model.last_i + 1))

                newPosition =
                    Math.Vector3.add position (transform direction (vec3 -(size * 0.057) -0.3 0))

                am =
                    case actor.characterAttributes of
                        Nothing ->
                            model.actorManager

                        Just x ->
                            if model.keys.shift == True && actor.actorType == Player && x.rockets > 0 then
                                let
                                    tam =
                                        updateActorManagerDict actor.key { actor | characterAttributes = Just { x | rockets = x.rockets - 1 } } model.actorManager
                                in
                                    Dict.insert slugname (templateRocketActor model.gameSpeed (slugint) newPosition target direction) tam
                            else
                                let
                                    damage =
                                        if actor.actorType == Player then
                                            20
                                        else
                                            5
                                in
                                    Dict.insert slugname (templateSlugActor model.gameSpeed (slugint) newPosition direction damage) model.actorManager
            in
                ( { model
                    | last_i = model.last_i + 1
                    , actorManager = am
                  }
                , Cmd.none
                )

        Explosion position ->
            let
                collide v1 r1 v2 r2 =
                    (Math.Vector3.getX v2 - Math.Vector3.getX v1) ^ 2 + (Math.Vector3.getY v2 - Math.Vector3.getY v1) ^ 2 <= ((r1 / 2 + r2)) ^ 2

                checkActor s actor =
                    if actor.actorType == NPC && collide actor.position actor.size position 1 then
                        { actor | timeToLive = Just 200 }
                    else
                        actor

                newActorManager =
                    Dict.map checkActor model.actorManager
            in
                ( { model | actorManager = newActorManager }, Cmd.none )

        GenerateDropCrate ( rand, position ) ->
            let
                slugint =
                    model.last_i + 1

                am =
                    if rand < 50 then
                        let
                            slugname =
                                "health-" ++ (toString (model.last_i + 1))
                        in
                            Dict.insert slugname (templateHealthActor model.gameSpeed (slugint) position) model.actorManager
                    else
                        let
                            slugname =
                                "rocketCrate-" ++ (toString (model.last_i + 1))
                        in
                            Dict.insert slugname (templateRocketCrateActor model.gameSpeed (slugint) position) model.actorManager
            in
                ( { model
                    | last_i = model.last_i + 1
                    , actorManager = am
                  }
                , Cmd.none
                )

        ChangeStatus s ->
            case s of
                MainMenu ->
                    let
                        newModel =
                            fst init
                    in
                        update (PlaySound "beep2")
                            { newModel
                                | textures = model.textures
                                , wsize = model.wsize
                            }

                EnterName ->
                    ( { model | status = s }, Task.perform SoundError PlaySound (succeed "beep2") )

                Game ->
                    ( { model | status = s }, Cmd.batch [ Task.perform SoundError PlaySound (succeed "beep2"), Task.perform SoundError PlayMusic (succeed "actofwar") ] )

                Won ->
                    ( { model | status = s }, Cmd.batch [ Task.perform SoundError PlaySound (succeed "beep2"), Task.perform SoundError PlayMusic (succeed "intro") ] )

                GameOver ->
                    ( { model | status = s }, Cmd.batch [ Task.perform SoundError PlaySound (succeed "beep2"), Task.perform SoundError PlayMusic (succeed "intro") ] )

                _ ->
                    ( { model | status = s }, Task.perform SoundError PlaySound (succeed "beep2") )

        SetGameSpeed i ->
            ( { model | gameSpeed = i }, Task.perform SoundError ChangeStatus (succeed Types.Options) )

        SetGameDifficulty i ->
            ( { model | gameDifficulty = i }, Task.perform SoundError ChangeStatus (succeed Types.Options) )

        EnterButton ->
            if model.status == EnterName then
                ( model
                , if String.isEmpty model.playerName then
                    Cmd.none
                  else
                    Task.perform SoundError ChangeStatus (succeed Help)
                )
            else
                ( model
                , if String.isEmpty model.playerName then
                    Cmd.none
                  else
                    Task.perform SoundError ChangeStatus (succeed Game)
                )

        ExitButton ->
            case model.status of
                Won ->
                    update FetchScore model

                GameOver ->
                    update FetchScore model

                Highscore ->
                    update (ChangeStatus MainMenu) model

                Speed ->
                    ( model, Task.perform SoundError ChangeStatus (succeed Types.Options) )

                Difficulty ->
                    ( model, Task.perform SoundError ChangeStatus (succeed Types.Options) )

                _ ->
                    ( model, Task.perform SoundError ChangeStatus (succeed MainMenu) )












actorManager : ActorManager
actorManager =
    Dict.fromList
        [ ( "Player1", templatePlayerActor "Player1" )
        , ( "ground", templateGroundActor )
        ]

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
      , waveCounter = 0
      , enemyCounter = 0
      }
    , Cmd.batch
        [ Window.size |> windowSize
        , fetchTextures |> Task.perform TexturesError TexturesLoaded
        , (succeed "intro") |> Task.perform SoundError PlayMusic
        ]
    )


mouseClicks : Position -> Msg
mouseClicks position =
    MouseClicks position


keyPressed : Keyboard.KeyCode -> Msg
keyPressed keyCode =
    case keyCode of
        27 ->
            ExitButton

        13 ->
            EnterButton

        _ ->
            ( Basics.identity, Nothing ) |> KeyChange


keyChange : Bool -> Keyboard.KeyCode -> Msg
keyChange on keyCode =
    ( (case keyCode of
        65 ->
            \k -> { k | left = on }

        68 ->
            \k -> { k | right = on }

        87 ->
            \k -> { k | up = on }

        83 ->
            \k -> { k | down = on }

        109 ->
            \k -> { k | minus = on }

        107 ->
            \k -> { k | plus = on }

        16 ->
            \k -> { k | shift = on }

        _ ->
            Basics.identity
      )
    , (case keyCode of
        109 ->
            if on then
                Just False
            else
                Nothing

        107 ->
            if on then
                Just True
            else
                Nothing

        _ ->
            Nothing
      )
    )
        |> KeyChange







port scoreBoard : (Scores -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ AnimationFrame.diffs Animate
    , Keyboard.downs (keyChange True)
    , Keyboard.ups (keyChange False)
    , Keyboard.downs keyPressed
    , Mouse.clicks (mouseClicks)
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
        x :: xs ->
            if i < snd x then
                fst x
            else
                getFrame (i - snd x) xs

        [] ->
            0

calculatePositions : Actor -> { start : Float, end : Float, rows: Float }
calculatePositions actor =
    let
        cols =
            let
                framedTexture =
                    Dict.get actor.texture framedTextures
            in
                case framedTexture of
                    Nothing ->
                        1.0

                    Just x ->
                        if snd x then
                            1.0 / (toFloat (fst x))
                        else
                            toFloat (fst x)

        anim =
            actor.animation

        frame =
            let
                frames =
                    Dict.get (actor.texture ++ "-" ++ anim.name) animationDict
            in
                case frames of
                    Nothing ->
                        0.0

                    Just x ->
                        let
                            fs =
                                x.frames
                        in
                            toFloat (getFrame anim.current fs)

        start =
            frame / cols

        end = cols
    --        start + (1.0 / cols)

        rows =
            if cols < 1 then
                cols
            else
                1


    in
        { start = start, end = end, rows = rows }




{-

animatedSprite : Actor -> Drawable { pos : Vec3, coord : Vec3 }
animatedSprite actor =
    let
        cols =
            let
                framedTexture =
                    Dict.get actor.texture framedTextures
            in
                case framedTexture of
                    Nothing ->
                        1.0

                    Just x ->
                        if snd x then
                            1.0 / (toFloat (fst x))
                        else
                            toFloat (fst x)

        anim =
            actor.animation

        frame =
            let
                frames =
                    Dict.get (actor.texture ++ "-" ++ anim.name) animationDict
            in
                case frames of
                    Nothing ->
                        0.0

                    Just x ->
                        let
                            fs =
                                x.frames
                        in
                            toFloat (getFrame anim.current fs)

        start =
            frame / cols

        end =
            start + (1.0 / cols)

        rows =
            if cols < 1 then
                end
            else
                1

        spriteSize =
            actor.size

        topLeft =
            { pos = vec3 -spriteSize spriteSize 0, coord = vec3 start rows 0 }

        topRight =
            { pos = vec3 spriteSize spriteSize 0, coord = vec3 end rows 0 }

        bottomLeft =
            { pos = vec3 -spriteSize -spriteSize 0, coord = vec3 start 0 0 }

        bottomRight =
            { pos = vec3 spriteSize -spriteSize 0, coord = vec3 end 0 0 }

        result =
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]
    in
        Triangle result

-}

-- VIEW


camera : Mat4 -> Mat4
camera worldTransformation =
    mul (makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0))
        worldTransformation


perspective : Size -> Mat4
perspective wsize =
    perspectiveMatrix ((toFloat wsize.width) / (toFloat wsize.height))


perspectiveMatrix : Float -> Mat4
perspectiveMatrix ratio =
    makePerspective 45 ratio 0.01 100


addActorToScene : Actor -> Model -> List Renderable
addActorToScene actor model =
    let
        tPos = calculatePositions actor
    in
        case Dict.get actor.texture model.textures of
            Nothing ->
                []

            Just texture ->
                [ render vertexShader
                    fragmentShader
                    actor.actorSprite
                    { tex = texture
                    , start = tPos.start
                    , end = tPos.end
                    , rows = tPos.rows
                    , perspective =
                        List.foldr mul
                            Math.Matrix4.identity
                            [ perspective model.wsize
                            , if actor.actorType == Player then
                                makeTranslate actor.renderPosition
                              else
                                makeTranslate actor.renderPosition
                            , actor.rotation
                            , makeTranslate actor.spriteCentering
                            ]
                    }
                ]


addActorsToScene : Model -> List Renderable
addActorsToScene model =
    let
        am =
            model.actorManager

        list =
            Dict.toList am

        sortFunc x =
            let
                actor =
                    snd x
            in
                actor.index

        sortedList =
            List.sortBy sortFunc list

        aMap x =
            addActorToScene (snd x) model

        mappedList =
            List.map aMap sortedList
    in
        List.foldl (++) [] (mappedList)


mainMenu : Html Msg
mainMenu =
    div []
        [ div [ id "menuOverlay" ] []
        , div [ id "menuBg" ] []
        , div [ id "play", class "first", onClick (ChangeStatus EnterName), onMouseEnter (PlaySound "beep") ] []
        , div [ id "options", class "second", onClick (ChangeStatus Types.Options), onMouseEnter (PlaySound "beep") ] []
        , div [ id "credits", class "third", onClick (ChangeStatus Credits), onMouseEnter (PlaySound "beep") ] []
        ]


optionsMenu : Html Msg
optionsMenu =
    div []
        [ div [ id "menuOverlay" ] []
        , div [ id "menuBg" ] []
        , div [ id "difficulty", class "first", onClick (ChangeStatus Difficulty), onMouseEnter (PlaySound "beep") ] []
        , div [ id "speed", class "second", onClick (ChangeStatus Speed), onMouseEnter (PlaySound "beep") ] []
        , div [ id "back", class "third", onClick (ChangeStatus MainMenu), onMouseEnter (PlaySound "beep") ] []
        ]


speedMenu : Html Msg
speedMenu =
    div []
        [ div [ id "menuOverlay" ] []
        , div [ id "menuBg" ] []
        , div [ id "fast", class "first", onClick (SetGameSpeed 10), onMouseEnter (PlaySound "beep") ] []
        , div [ id "normal", class "second", onClick (SetGameSpeed 7), onMouseEnter (PlaySound "beep") ] []
        , div [ id "back", class "third", onClick (ChangeStatus Types.Options), onMouseEnter (PlaySound "beep") ] []
        ]


difficultyMenu : Html Msg
difficultyMenu =
    div []
        [ div [ id "menuOverlay" ] []
        , div [ id "menuBg" ] []
        , div [ id "hard", class "first", onClick (SetGameDifficulty True), onMouseEnter (PlaySound "beep") ] []
        , div [ id "normal", class "second", onClick (SetGameDifficulty False), onMouseEnter (PlaySound "beep") ] []
        , div [ id "back", class "third", onClick (ChangeStatus Types.Options), onMouseEnter (PlaySound "beep") ] []
        ]


credits : Html Msg
credits =
    div []
        [ div [ id "menuOverlay" ] []
        , div [ id "creditlist", onClick (ChangeStatus MainMenu) ]
            [ div [ id "creditsheader" ] [ text "CREDITS" ]
            , div [] [ text "programming - Ilya Bolotin" ]
            , div [] [ text "ui design - Ilya Bolotin" ]
            , div [] [ text "textures - Tatermand" ]
            , div []
                [ text "music - "
                , a [ href "https://soundcloud.com/alexandr-zhelanov" ] [ text "Alexandr Zhelanov" ]
                ]
            , div []
                [ text "sound effects - "
                , a [ href "http://productioncrate.com" ] [ text "ProductionCrate" ]
                ]
            ]
        ]


highscore : Model -> Html Msg
highscore model =
    let
        divMap s =
            div [] [ text (fst s ++ " - " ++ (toString (snd s))) ]
    in
        div []
            [ div [ id "menuOverlay" ] []
            , div [ id "creditlist", onClick (ChangeStatus MainMenu) ]
                ([ (div [ id "creditsheader" ] [ text "HIGH SCORES" ]) ]
                    ++ if List.isEmpty model.scoreList then
                        [ div [] [ text "Can't connect to the scoreboard server!" ] ]
                       else
                        (List.map divMap model.scoreList)
                )
            ]


game : Model -> Html Msg
game model =
    let
        player =
            getCharacterAttributes model playerActor
    in
        div []
            ([ (addActorsToScene model) |> WebGL.toHtmlWith [ BlendFunc ( SrcAlphaSaturate, DstAlpha ), Enable Blend ] [ width model.wsize.width, height model.wsize.height ]
             , img [ src "texture/healthIcon.png", id "healthicon" ] []
             , div [ id "health" ] [ text (toString player.health) ]
             , div [ id "score" ] [ text (toString player.score) ]
             , div [ id "blood" ] []
             , img
                [ src
                    (if player.rockets > 0 then
                        "texture/iconGrenadeActive.png"
                     else
                        "texture/iconGrenade.png"
                    )
                , class "rocket"
                , id "rocket1"
                ]
                []
             , img
                [ src
                    (if player.rockets > 1 then
                        "texture/iconGrenadeActive.png"
                     else
                        "texture/iconGrenade.png"
                    )
                , class "rocket"
                , id "rocket2"
                ]
                []
             , img
                [ src
                    (if player.rockets > 2 then
                        "texture/iconGrenadeActive.png"
                     else
                        "texture/iconGrenade.png"
                    )
                , class "rocket"
                , id "rocket3"
                ]
                []
             ]
                ++ (if model.startCounter > 0 then
                        let
                            c =
                                Basics.floor (model.startCounter / 1000)
                        in
                            if c == 0 then
                                [ div [ id "startCounter" ] [ text ("WAVE " ++ (toString model.waveCounter)) ] ]
                            else
                                [ div [ id "startCounter" ] [ text (Basics.toString c) ] ]
                    else
                        []
                   )
            )


view : Model -> Html Msg
view model =
    let
        player =
            getCharacterAttributes model playerActor
    in
        case model.status of
            GameOver ->
                div [] [ div [ id "menuOverlay", onClick FetchScore ] [], div [ id "gameover", onClick FetchScore ] [ text "GAMEOVER. No continues left." ], div [ id "gameoverDesc" ] [ text "Press Esc or click anywhere to return to main menu." ] ]

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
                div [] [ div [ id "menuOverlay" ] [], div [ id "help", onClick EnterButton ] [] ]

            EnterName ->
                div []
                    [ div [ id "menuOverlay" ] []
                    , input [ placeholder "enter your name", id "nameForm", onInput SetName ] []
                    , button [ id "nameButton", onClick EnterButton, onMouseEnter (PlaySound "beep") ] []
                    ]

            Won ->
                div []
                    [ div [ id "menuOverlay" ] []
                    , div [ id "menuOverlay", onClick FetchScore ] []
                    , div [ id "gameover", onClick FetchScore ] [ text "You've won!" ]
                    , div [ id "gameoverDesc" ] [ text "Press Esc or click anywhere to return to main menu." ]
                    ]

            Highscore ->
                highscore model

            WaitingForHighscore ->
                div []
                    [ div [ id "menuOverlay" ] []
                    , div [ id "gameoverDesc" ] [ text "Retrieving High Scores" ]
                    ]

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


fragmentShader : Shader { } { u | tex : Texture, start : Float, end : Float, rows: Float } { vcoord : Vec2 }
fragmentShader =
    [glsl|

precision mediump float;
uniform sampler2D tex;
varying vec2 vcoord;
uniform float start;
uniform float end;
uniform float rows;

void main () {
    gl_FragColor  =  texture2D(tex, vec2((vcoord.x)/end, vcoord.y/rows) + vec2(start,0));

}

|]
