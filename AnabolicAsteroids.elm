-- Anabolic Asteroids
{- In the year 29601, a supply ship carrying 99999 megatons of steroids collided into a comet in a horrific accident and exploded.
   The resulting debris infused itself with the ship's cargo to create a toxic asteroid field of glass, metal, and pure muscle.
   Now it's up to you to stop the debris from falling back to earth and turning everyone into toned and ripped monsters -}

-- Coded by Michael Yuan
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Mouse
import Text
import Time exposing (..)
import Window
import Signal exposing (..)
import Random
import Random exposing (Seed)
import String

-- Colors
colorHUD : Color
colorHUD = (rgb 174 238 238)
colorHUD2 : Color
colorHUD2 = (rgb 216 96 125)
colorHUD3 : Color
colorHUD3 = (rgb 50 255 87)

colorSpaceship : Color
colorSpaceship = (rgb 185 215 217)
colorShield : Color
colorShield = (rgb 105 210 231)
colorBullets : Color
colorBullets = (rgb 255 255 255)
colorJetstream : Color
colorJetstream = (rgb 212 30 69)

colorRock1 : Color
colorRock1 = (rgb 236 186 9)
colorRock2 : Color
colorRock2 = (rgb 208 96 50)
colorRock3 : Color
colorRock3 = (rgb 164 74 37)
colorRock4 : Color
colorRock4 = (rgb 94 48 33)

colorAsteroid1 : Color
colorAsteroid1 = (rgb 236 208 120)
colorAsteroid2 : Color
colorAsteroid2 = (rgb 217 91 67)
colorAsteroid3 : Color
colorAsteroid3 = (rgb 192 41 66)
colorAsteroid4 : Color
colorAsteroid4 = (rgb 84 36 55)

colorShield1 : Color
colorShield1 = (rgb 207 240 158)
colorShield2 : Color
colorShield2 = (rgb 168 219 168)
colorShield3 : Color
colorShield3 = (rgb 121 189 154)
colorShield4 : Color
colorShield4 = (rgb 59 134 134)

-- Settings
maxBullets : Int 
maxBullets = 10

ctrlTimer : Float
ctrlTimer = 100

-- MODEL
type State = Intro | Menu | Play | Pause | Win | Lose

type alias MovingObject = 
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , rotation : Float
  , vr : Float
  , radius : Float
  , health : Int
  , color : Color}

type alias Message = 
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , rotation : Float
  , vr : Float
  , radius : Float
  , health : Int
  , message : String
  , textOptions : Text.Text -> Text.Text
  , objOptions : Form -> Form}

type alias Game = 
  { state : State
  , player : MovingObject
  , stageName : String
  , playerScore : Int
  , asteroids : List MovingObject
  , bullets : List MovingObject
  , particles : List MovingObject
  , messages : List Message
  , duration : Float
  , timeLimit : Float
  , ctrlTime : Time
  , asteroidCount : Int}

type alias Input = 
  { space : Bool
  , ctrl : Bool
  , mouse : (Int, Int)
  , click : Bool
  , window : (Int, Int)
  , time : Time}

startGame : Game
startGame =
  { state = Menu
  , player = MovingObject 0 0 0 0 0 0 15 20 colorSpaceship
  , stageName = "Pro 0"
  , playerScore = 0
  , asteroids = []
  , bullets = []
  , particles = []
  , messages = [Message 0 0 0 0 0 -1 25 -100 ("SPACEBAR to change directions, MOUSE to shoot, CTRL to pause \n \n Don't let the asteroids touch you.") (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
  , duration = 0
  , timeLimit = 0
  , ctrlTime = 0
  , asteroidCount = 0
  }

pro1 : Game
pro1 =
  { state = Intro
  , player = MovingObject 0 0 0 0 0 0 15 20 colorSpaceship
  , stageName = "Pro 1"
  , playerScore = 0
  , asteroids = []
  , bullets = []
  , particles = []
  , messages = [Message 0 0 0 0 0 -1 25 -100 ("Prologue:") (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
  , duration = 0
  , timeLimit = 0
  , ctrlTime = 0
  , asteroidCount = 0
  }

pro2 : Game
pro2 =
  { state = Intro
  , player = MovingObject 0 0 0 0 0 0 15 20 colorSpaceship
  , stageName = "Pro 2"
  , playerScore = 0
  , asteroids = []
  , bullets = []
  , particles = []
  , messages = [Message 0 0 0 0 0 -1 25 -100 ("In the year 29601...") (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
  , duration = 0
  , timeLimit = 0
  , ctrlTime = 0
  , asteroidCount = 0
  }

pro3 : Game
pro3 =
  { state = Intro
  , player = MovingObject 0 0 0 0 0 0 15 20 colorSpaceship
  , stageName = "Pro 3"
  , playerScore = 0
  , asteroids = []
  , bullets = []
  , particles = []
  , messages = [Message 0 0 0 0 0 -1 25 -200 ("A supply ship carrying 99999 megatons of steroids collided into a comet in a horrific accident and exploded.") (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
  , duration = 0
  , timeLimit = 0
  , ctrlTime = 0
  , asteroidCount = 0
  }

pro4 : Game
pro4 =
  { state = Intro
  , player = MovingObject 0 0 0 0 0 0 15 20 colorSpaceship
  , stageName = "Pro 4"
  , playerScore = 0
  , asteroids = []
  , bullets = []
  , particles = []
  , messages = [Message 0 0 0 0 0 -1 25 -200 ("The resulting debris infused itself with the ship's cargo to create a toxic asteroid field of glass, metal, and pure muscle.") (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
  , duration = 0
  , timeLimit = 0
  , ctrlTime = 0
  , asteroidCount = 0
  }

pro5 : Game
pro5 =
  { state = Intro
  , player = MovingObject 0 0 0 0 0 0 15 20 colorSpaceship
  , stageName = "Pro 5"
  , playerScore = 0
  , asteroids = []
  , bullets = []
  , particles = []
  , messages = [Message 0 0 0 0 0 -1 25 -200 ("Now it's up to you to stop the debris from falling back to earth and turning everyone into toned and ripped monsters.") (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
  , duration = 0
  , timeLimit = 0
  , ctrlTime = 0
  , asteroidCount = 0
  }

level1 : Game
level1 =
  { state = Intro
  , player = MovingObject 0 0 0 0 0 0 15 20 colorSpaceship
  , stageName = "Level 1"
  , playerScore = 0
  , asteroids = []
  , bullets = []
  , particles = []
  , messages = [Message 0 0 0 0 0 -1 25 -200 ("Level 1") (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
  , duration = 0
  , timeLimit = 1800
  , ctrlTime = 0
  , asteroidCount = 3
  }

level2 : Game
level2 =
  { state = Intro
  , player = MovingObject 0 0 0 0 0 0 15 20 colorSpaceship
  , stageName = "Level 2"
  , playerScore = 0
  , asteroids = []
  , bullets = []
  , particles = []
  , messages = [Message 0 0 0 0 0 -1 25 -100 ("Level 2") (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
  , duration = 0
  , timeLimit = 3600
  , ctrlTime = 0
  , asteroidCount = 5
  }


level3 : Game
level3 =
  { state = Intro
  , player = MovingObject 0 0 0 0 0 0 15 20 colorSpaceship
  , stageName = "Level 3"
  , playerScore = 0
  , asteroids = []
  , bullets = []
  , particles = []
  , messages = [Message 0 0 0 0 0 -1 25 -100 ("Level 3: The Final Level") (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
  , duration = 0
  , timeLimit = 5400
  , ctrlTime = 0
  , asteroidCount = 10
  }

randomInt : Seed -> Int -> Int
randomInt seed max =
  seed |> (Random.generate <| Random.int 0 max) |> fst 


-- UPDATE

update : Input -> Game -> Game
update ({space, ctrl, mouse, click, window, time} as input) ({state, player, stageName, playerScore, asteroids, bullets, particles, messages, duration, timeLimit, ctrlTime, asteroidCount} as game) = 
  let
    onAsteroidPlayerCollision : MovingObject -> MovingObject -> List MovingObject
    onAsteroidPlayerCollision player asteroid = 
      []

    onAsteroidBulletCollision : MovingObject -> MovingObject -> List MovingObject
    onAsteroidBulletCollision bullet asteroid = 
      let
        asteroidColor = toRgb asteroid.color
        newColor = if asteroidColor.red == 94 then colorRock3 else if asteroidColor.red == 164 then colorRock2 else colorRock1
        spawn : Seed -> List MovingObject -> List MovingObject
        spawn seed list = spawnObj 1.0 (asteroid.x, asteroid.y) (asteroid.radius * 2, asteroid.radius * 2) 0.0 seed {asteroid | color <- newColor, radius <- asteroid.radius / 2, health <- asteroid.health // 4} list
        spawn1 = spawn randomSeed []
        (x, randomerSeed) = Random.generate (Random.int 0 1) randomSeed
        spawn2 = spawn randomerSeed spawn1 
        (x2, randomestSeed) = Random.generate (Random.int 0 1) randomerSeed
        spawn3 = spawn randomestSeed spawn2
      in
        List.take 2 spawn3

    onBulletAsteroidCollision : MovingObject -> MovingObject -> List MovingObject
    onBulletAsteroidCollision asteroid bullet = 
      []

    onPlayerAsteroidCollision : List MovingObject -> MovingObject -> MovingObject
    onPlayerAsteroidCollision listAsteroids playerObj = 
      let
        callback : MovingObject -> MovingObject -> List MovingObject
        callback a b = [b]
        aftermath = getCollision callback playerObj listAsteroids
        factor =
          case aftermath of
            Just newObjs -> List.length newObjs
            Nothing -> 0
      in
        {playerObj | health <- playerObj.health - factor, radius <- playerObj.radius + (15 * toFloat factor)}


    onAsteroidAsteroidCollision : MovingObject -> MovingObject -> List MovingObject
    onAsteroidAsteroidCollision asteroid asteroid2 = 
      if (asteroid.x /= asteroid2.x && asteroid.y /= asteroid2.y && asteroid.rotation /= asteroid2.rotation) then
        let
          (m1, t1) = toPolar (asteroid.vx, asteroid.vy)
          (m2, t2) = toPolar (asteroid2.vx, asteroid2.vy)
          (difference, theta) = toPolar ((asteroid.x - asteroid2.x), (asteroid.y - asteroid2.y))
          (dx, dy) = fromPolar (asteroid.radius + asteroid2.radius + 1, theta)
          angleOfReflection = theta + (turns 0.25)
          oppositeAngleOfReflecion = (angleOfReflection + (turns 0.5))
          newAngle1 = angleOfReflection - (t1 - oppositeAngleOfReflecion)
          newAngle2 = angleOfReflection - (t2 - oppositeAngleOfReflecion)
          calcNewV : Float -> Float -> Float -> Float -> Float
          calcNewV mass1 mass2 vel1 vel2 = ((vel1 * (mass1 - mass2)) + (2 * mass2 * vel2)) / (mass1 + mass2)
          vx2 = calcNewV asteroid2.radius asteroid.radius asteroid2.vx asteroid.vx
          vy2 = calcNewV asteroid2.radius asteroid.radius asteroid2.vy asteroid.vy
          calcV : Float -> Float -> Float -> Float -> Float -> Float -> Float
          calcV ma1 ma2 ve1 ve2 an1 an2 =
            (sin (an1 - an2)) / (ma2 * (((sin an1) * (asteroid.vx + asteroid2.vx)) - ((cos an1) * (asteroid.vy + asteroid2.vy))))
          newV2 = calcV asteroid.radius asteroid2.radius m1 m2 (-oppositeAngleOfReflecion + t1) (-oppositeAngleOfReflecion + t2)
          (calcedX, calcedY) = fromPolar (newV2, newAngle2)

          -- Wikipedia to the rescue
          getV : Float -> Float -> Float -> Float -> Float -> Float -> Float -> (Float, Float)
          getV m1 m2 v1 v2 a1 a2 contactAngle = 
            let
              topPart = ((v1 * (cos (a1 - contactAngle)) * (m1 - m2)) + (2 * m2 * v2 * (cos (a2 - contactAngle))))
              vx = ((topPart * (cos contactAngle)) / (m1 + m2)) + (v1 * (sin (a1 - contactAngle)) * (cos (contactAngle + (turns 0.25))))
              vy = ((topPart * (sin contactAngle)) / (m1 + m2)) + (v1 * (sin (a1 - contactAngle)) * (sin (contactAngle + (turns 0.25))))
            in
              (vx, vy)

          (mX, mY) = getV asteroid2.radius asteroid.radius m2 m1 t2 t1 (angleOfReflection + turns 0.25)
        in
          [{asteroid2 | x <- asteroid.x - dx, y <- asteroid.y - dy, vx <- mX, vy <- mY}]
          else
            [asteroid2]

    randomSeed = Random.initialSeed(round (1000 * (Time.inMilliseconds time)))
    (windowW, windowH) = window
    floatWindow = (toFloat windowW, toFloat windowH)
    realCtrl = ctrl && (Time.inMilliseconds time) - (Time.inMilliseconds ctrlTime) > ctrlTimer / 1000
    newState = if state == Menu then if realCtrl then Intro else Menu
      else if state == Intro then (if (List.length messages) > 0 then Intro else Play)
      else if state == Play then (if player.health < 1 then Lose else if duration > timeLimit && List.length asteroids == 0 then Win else if realCtrl then Pause else Play)
      else if state == Pause then if realCtrl then Play else Pause
      else if (state == Win || state == Lose) && realCtrl then Menu
      else state
    newPlayer = 
      if state == Play || state == Intro then player |> onPlayerAsteroidCollision asteroids |> movePlayer input
      else if state == Menu then player
      else player
    newScore = playerScore + (100 * List.length asteroidbulletCollisions)
    newAsteroids = 
      if state == Play then
        let
          collidedAsteroids = asteroids |> listCollision onAsteroidAsteroidCollision asteroids |> listCollision onAsteroidBulletCollision [player] |> listCollision onAsteroidBulletCollision bullets
          movedAsteroids = moveObjects floatWindow (collidedAsteroids)
        in
          if List.length asteroids < asteroidCount && duration < timeLimit then 
            let
              spawnedAsteroids = spawnObj 0.05 (0,0) floatWindow 128 randomSeed (MovingObject 100 100 0 0 0 0 128 128 colorRock4) movedAsteroids
            in
              spawnedAsteroids
            else
                movedAsteroids
                else asteroids                       
    newBullets =
      if state == Intro || state == Play then 
        let
          movedBullets = bullets |> listCollision onBulletAsteroidCollision asteroids |> moveObjects floatWindow
        in
          if click && (List.length bullets < maxBullets) then
            movedBullets |> shootObj (player.x  + (player.radius * cos player.rotation), player.y  + (player.radius * sin  player.rotation)) player.rotation 15 -50 5 white
            else movedBullets
      else bullets

    identityCallback : MovingObject -> MovingObject -> List MovingObject
    identityCallback a b =  if (a.x /= b.x && a.y /= b.y && a.rotation /= b.rotation) then [b] else [b]
    asteroidbulletCollisions = getListCollision identityCallback bullets asteroids
    newParticles =
      let
        --asteroidasteroidCollisions = getListCollision identityCallback asteroids asteroids
        asteroidplayerCollisions = getListCollision identityCallback [player] asteroids
        movedParticles = particles
          |> explodeList 1 25 1 -15 50 randomSeed {-[yellow, orange, red, red, red, black]-} [colorAsteroid1, colorAsteroid2, colorAsteroid3, colorAsteroid4]  asteroidbulletCollisions
          --|> explodeList 1 50 2 -20 10 randomSeed [colorRock1, colorRock2, colorRock3, colorRock4]  asteroidasteroidCollisions
          |> explodeList 1 25 25 -10 50 randomSeed [Color.complement player.color, colorShield1, colorShield2, colorShield3, colorShield4] asteroidplayerCollisions
          |> moveObjects floatWindow
      in
        if state == Play || state == Intro then
          let
            (boosterWiggle, boosterSeed) = Random.generate (Random.float (turns -0.02) (turns 0.02)) randomSeed
          in
            if space then
              movedParticles |> shootObj (player.x - ((player.radius / 2 + 10) * cos player.rotation), player.y - ((player.radius / 2 + 10) * sin player.rotation)) (player.rotation + (turns 0.5) + boosterWiggle) 7 -10 1 colorJetstream
            else movedParticles
        else if state == Lose then explodeObject 0.5 player (player.radius / 2) 25 -10 50 randomSeed {-[yellow, orange, red, red, red, black]-} [colorAsteroid1, colorAsteroid2, colorAsteroid3, colorAsteroid4] movedParticles
        else particles
    newMessages = moveObjects floatWindow messages
    newDuration = if state == Play then duration + 1 else duration
    newTimeLimit = timeLimit
    newCtrlTime = if ctrl then time else ctrlTime
    newCount = asteroidCount
  in
    if (state == Lose) && realCtrl then {startGame | ctrlTime <- newCtrlTime}
    else if state == Play && duration > timeLimit && List.length asteroids == 0 && player.health > 0 then
      let
        nextStage = if stageName == "Pro 0" then pro1
          else if stageName == "Pro 1" then pro2
          else if stageName == "Pro 2" then pro3
          else if stageName == "Pro 3" then pro4
          else if stageName == "Pro 4" then pro5
          else if stageName == "Pro 5" then level1
          else if stageName == "Level 1" then level2
          else level3
        nextPlayer = nextStage.player
      in
        {nextStage | ctrlTime <- newCtrlTime, playerScore <- newScore, player <- {nextPlayer | x <- player.x, y <- player.y, vx <- player.vx, vy <- player.vy, rotation <-player.rotation}, particles <- particles, bullets <- bullets }
    else {game | state <- newState, player <- newPlayer, playerScore <- newScore, asteroids <- newAsteroids, bullets <- newBullets, particles <- newParticles, messages <- newMessages, duration <- newDuration, timeLimit <- newTimeLimit, ctrlTime <- newCtrlTime, asteroidCount <- newCount}

movePlayer : Input -> MovingObject -> MovingObject
movePlayer input player = 
    let 
      (mouseX, mouseY) = input.mouse
      (windowW, windowH) = input.window
      (centerX, centerY) = (toFloat windowW / 2, toFloat windowH / 2)
      (dx, dy) = (toFloat mouseX - (centerX + player.x), (centerY - player.y) - toFloat mouseY)
      speed = sqrt( player.vx * player.vx + player.vy * player.vy)
      newX = wrapAny ( toFloat windowW / -2, toFloat windowW / 2) (player.radius) (player.x + player.vx)
      newY = wrapAny ( toFloat windowH / -2, toFloat windowH / 2) (player.radius) (player.y + player.vy)
      newVX = if input.space then player.vx * 0.9 else if speed < 7 then 7 * cos player.rotation else player.vx
      newVY = if input.space then player.vy * 0.9 else if speed < 7 then 7 * sin player.rotation else player.vy
      newRotation = (atan2 dy dx)
      newRadius = player.radius
      newHealth = player.health
      newColor = player.color
    in
      {player | x <- newX, y <- newY, vx <- newVX, vy <- newVY, rotation <- newRotation, radius <- newRadius, health <- newHealth, color <- newColor}

moveObject : (number, number) -> number -> {a | x:Float, y:Float, vx:Float, vy:Float, rotation:Float, vr:Float} -> {a | x:Float, y:Float, vx:Float, vy:Float, rotation:Float, vr:Float} 
moveObject (w, h) margin obj = 
    {obj | x <- wrapAny (-w/2, w/2) margin (obj.x + obj.vx), y <- wrapAny (-h/2,h/2) margin (obj.y + obj.vy), rotation <- wrapAny ((turns 0), (turns 1)) 0 (obj.rotation + obj.vr)}


type alias Spinable = {r:Float, vr:Float}
moveObjects : (Float, Float) -> List {a | x:Float, y:Float, vx:Float, vy:Float, health:Int, radius:Float, rotation:Float, vr:Float}  -> List {a | x:Float, y:Float, vx:Float, vy:Float, health:Int, radius:Float, rotation:Float, vr:Float} 
moveObjects (w, h) objs =
  let
    head = List.head objs
    maybeTail = List.tail objs
    tail = 
      case maybeTail of
        Just t -> t
        Nothing -> []
  in
    case head of
      Just obj ->
        if obj.health < 0 && obj.health < round obj.vr then
          let
            moved = (moveObject (w, h) obj.radius obj)
          in
            {moved | health <- obj.health + (round -obj.vr)} :: (moveObjects (w, h) tail) else
              if obj.health > 0 then (moveObject (w, h) obj.radius obj) :: (moveObjects (w, h) tail) else moveObjects (w, h) tail
      Nothing -> []

shootObj : (number, number) -> number -> Float -> Int -> Float -> Color -> List MovingObject -> List MovingObject
shootObj (x, y) direction velocity health radius color previous =
    let
      newVX = velocity * cos direction
      newVY = velocity * sin direction
      newRotation = direction
      newRadius = radius
      newHealth = health
      newColor = color
    in
      {x = x, y = y, vx = newVX, vy = newVY, rotation = newRotation, vr = -1, radius = newRadius, health = newHealth, color = newColor} :: previous

spawnObj : Float -> (Float, Float) -> (Float, Float) -> Float -> Seed -> MovingObject -> List MovingObject -> List MovingObject
spawnObj probability (offsetx, offsety) (w, h) margin seed obj previous =
  let
    (spawn, spawnSeed) = Random.generate (Random.float 0 1) seed
  in
    if spawn < probability then
      let
        left = -w/2 - margin
        right = w/2 + margin
        top = h/2 + margin
        bottom = -h/2 - margin
        (vertical, vSeed) = Random.generate (Random.int 1 2) spawnSeed
        (velx, vxSeed) = Random.generate (Random.float 1 10) vSeed
        (velxSign, vxsSeed) = Random.generate (Random.int 1 2) vxSeed
        (vely, vySeed) = Random.generate (Random.float 1 10) vxsSeed
        (velySign, vysSeed) = Random.generate (Random.int 1 2) vySeed
        (vrot, vrotSeed) = Random.generate (Random.float (turns -0.01) (turns 0.01)) vysSeed
        (side, sideSeed) = Random.generate (Random.int 1 2) vrotSeed
        (posx, xSeed) = if vertical == 1 then Random.generate (Random.float 1 (w + 2 * margin)) spawnSeed else if side == 1 then (0, seed) else (w + 2 * margin, sideSeed)
        (posy, ySeed) = if vertical == 2 then Random.generate (Random.float 1 (h + 2 * margin)) spawnSeed else if side == 1 then (0, seed) else (h + 2 * margin, xSeed)
        callback : MovingObject -> MovingObject -> List MovingObject
        callback a b = []
        movedObj = {obj | x <- posx + left + offsetx, y <- posy + bottom + offsety, vx <- (if velxSign == 1 then velx else -velx), vy <- (if velySign == 1 then vely else -vely), vr <- vrot}
        spawnCollision = getCollision callback movedObj previous
      in
        case spawnCollision of
          Just a -> previous
          Nothing -> movedObj::previous
      else
        previous

checkCollision : MovingObject -> MovingObject -> Bool
checkCollision obj1 obj2 = 
  let
    equal = (obj1.x == obj2.x && obj1.y == obj2.y) || (obj1.x + obj1.vx == obj2.x + obj2.vx && obj1.y + obj1.vy == obj2.y + obj2.vy)
    dx = obj1.x - obj2.x
    dy = obj1.y - obj2.y
    distance = sqrt (dx * dx + dy * dy)
  in
    (distance < obj1.radius + obj2.radius) && not equal

getCollision : (MovingObject -> MovingObject -> List MovingObject) -> MovingObject -> List MovingObject -> Maybe (List MovingObject)
getCollision callback one twos = 
  let
    maybeHead = List.head twos
    tail = List.drop 1 twos
  in
    case maybeHead of
      Just head -> if (checkCollision head one) then Just (callback head one) else getCollision callback one tail
      Nothing -> Nothing

listCollision : (MovingObject -> MovingObject -> List MovingObject) -> List MovingObject -> List MovingObject -> List MovingObject
listCollision callback twos ones = 
  let
    maybeHead = List.head ones
    tail = List.drop 1 ones
  in
    case maybeHead of
      Just head ->
        let
          collidedOne = getCollision callback head twos
        in
          case collidedOne of
            Just newObjs -> List.append newObjs (listCollision callback twos tail)
            Nothing -> head :: (listCollision callback twos tail)
      Nothing -> []

getListCollision : (MovingObject -> MovingObject -> List MovingObject) -> List MovingObject -> List MovingObject -> List MovingObject
getListCollision callback twos ones = 
  let
    maybeHead = List.head ones
    tail = List.drop 1 ones
  in
    case maybeHead of
      Just head ->
        let
          collidedOne = getCollision callback head twos
        in
          case collidedOne of
            Just newObjs -> List.append newObjs (getListCollision callback twos tail)
            Nothing -> (getListCollision callback twos tail)
      Nothing -> []

explodeObject : Float -> MovingObject -> Float -> Float -> Float -> Int -> Seed -> List Color -> List MovingObject -> List MovingObject
explodeObject probability obj particleSize crazyness decay limit seed colors previous = 
  let
    (chance, chanceSeed) = Random.generate (Random.float 0 1) seed
    (posX, posY) = (obj.x, obj.y)
    size = obj.radius
  in
    if List.length previous < limit && chance < probability then
      let
        (scale, sSeed) = Random.generate (Random.float 1 2) chanceSeed
        (vMultiplier, vSeed) = Random.generate (Random.float 1 crazyness) sSeed
        (r, rSeed) = Random.generate (Random.float 1 size) vSeed
        (theta, tSeed) = Random.generate (Random.float 1 (turns 1)) rSeed
        (offX, offY) = fromPolar (r, theta)
        (velX, velY) = fromPolar (vMultiplier, theta)

        colorRatio = ((toFloat (List.length previous)) / (toFloat limit))
        maybeColor : Maybe Color
        maybeColor = List.head colors
        color = case maybeColor of
          Just c -> c
          Nothing -> green
        colorTail = if (List.length colors) > 1 then List.drop 1 colors else colors
      in
        explodeObject probability obj particleSize crazyness decay limit tSeed (colorTail) ({x = posX + offX, y = posY + offY, vx = velX, vy = velY, rotation = theta, vr = particleSize / (decay), radius = 1, health = round -(particleSize + scale), color = color } :: previous)
    else
        previous

explodeList : Float -> Float -> Float -> Float ->Int -> Seed -> List Color -> List MovingObject -> List MovingObject -> List MovingObject
explodeList probability particleSize crazyness decay limit seed colors list previous = 
  let
    maybeHead = List.head list
    tail = List.drop 1 list
  in
    case maybeHead of
      Just head ->
        (explodeObject probability head particleSize crazyness decay limit seed colors (explodeList probability particleSize crazyness decay limit seed colors tail previous))
      Nothing -> previous

wrapScreen : (Float, Float) -> (Float) -> {a | x:Float, y:Float} -> {a | x:Float, y:Float}
wrapScreen (w, h) margin obj = wrapHorizontal w margin (wrapVertical h margin obj)

wrapHorizontal : (Float) -> (Float) -> {a | x:Float} -> {a | x:Float}
wrapHorizontal width margin obj =
  let
    upper = width / 2 + margin
    lower = -upper
  in
    if obj.x > upper then {obj | x <- 2 * lower + obj.x} else
      if obj.x < lower then {obj | x <- 2 * upper + obj.x} else
        obj

wrapVertical : (Float) -> (Float) -> {a | y:Float} -> {a | y:Float}
wrapVertical width margin obj =
  let
    upper = width / 2 + margin
    lower = -upper
  in
    if obj.y > upper then {obj | y <- 2 * lower + obj.y} else
      if obj.y < lower then {obj | y <- 2 * upper + obj.y} else
        obj

wrapAny : (number, number) -> number -> number -> number
wrapAny (low, high) margin any =
  if any > high + margin then 2 * (low - margin) + any else
      if any < low - margin then 2 * (high + margin) + any else
        any

-- VIEW
view : (Int, Int) -> Game -> Element
view (w', h') game = 
  let
    (w,h) = (toFloat w', toFloat h')
    (x, y) = (w / 2, h / 2)
    scaleWithWindow = (scale (scaleToFit (w, h) (1920, 1080)))
  in
    layers
      [collage w' h' --Sky
          [rect w h
            |> filled black
            |> move (0, 0)
          ]
      , collage w' h' (if game.state == Play || game.state == Intro || game.state == Pause || game.state == Lose then (List.map scaleWithWindow (List.concat [ (List.map renderBullet game.bullets), (renderPlayer game.player) :: (List.map renderAsteroid game.asteroids), (List.map renderParticle game.particles)])) else [])
      , collage w' h' (List.map scaleWithWindow (List.map renderText (
        let
          menu = [Message 0 0 0 0 0 0 400 10000 "ANABOLIC ASTEROIDS" (Text.typeface ["impact", "impact"] >> Text.bold >> Text.color colorHUD) identity, Message 0 0 0 0 0 0 50 1 "Coded by Michael Yuan         Press CTRL to start the game" (Text.typeface ["impact", "arial"] >> Text.bold >> Text.color colorHUD2) identity]
          health = [Message -335 (-h / 2 + 25) 0 0 0 0 25 10000 ("Health:" ++ (String.padLeft 2 '0' (toString game.player.health))) (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) identity]
          ammo = [Message 335 (-h / 2 + 25) 0 0 0 0 25 10000 ("Ammo:" ++ (String.padLeft 2 '0' (toString (maxBullets - List.length game.bullets)))) (Text.typeface ["monospace", "impact"] >> Text.color colorHUD) (identity)]
          score = [Message 0 (h / 2 - 50) 0 0 0 0 50 10000 ("Score: " ++ (String.padLeft 5 '0' (toString (game.playerScore)))) (Text.typeface ["impact", "impact"] >> Text.color colorHUD) (identity)]
          totalFrames = max 0 (game.timeLimit - game.duration)
          totalSec = totalFrames / 30
          totalMin = totalSec / 60
          minRemaining = floor (totalMin)
          secRemaining = floor (totalSec - (toFloat minRemaining * 60))
          framesRemaining = floor (totalFrames - (toFloat secRemaining * 1000))
          timerColor = if max 0 totalFrames == 0 then colorHUD3 else colorHUD
          timer = [Message 0 (-h / 2 + 30) 0 0 0 0 50 10000 ((String.padLeft 2 '0' (toString ( minRemaining ))) ++ ":" ++ (String.padLeft 2 '0' (toString (secRemaining )))) (Text.typeface ["monospace", "impact"] >> Text.bold >> Text.color timerColor) identity]

          lose = [Message 0 0 0 0 0 0 400 10000 ("WHOOPS!\nYOU DIED!") (Text.typeface ["impact", "impact"] >> Text.color colorHUD2) (identity)
                 ,Message 0 0 0 0 0 0 50 10000 ("Your muscles got too big and tore your body in half in a violent and bloody explosion\nThe remaining asteroids then fell to earth and exploded everyone there too :(") (Text.typeface ["impact", "impact"] >> Text.color colorHUD) (identity)]

          win = [Message 0 0 0 0 0 0 400 10000 ("HOORAY!!!\nYOU DID IT!") (Text.typeface ["impact", "impact"] >> Text.color colorHUD3) (identity)
                 ,Message 0 0 0 0 0 0 50 10000 ("You saved all of mankind with your heroic acts today and earned " ++ (String.padLeft 5 '0' (toString (game.playerScore))) ++" total points!\n Go eat an entire tub of ice cream or something! You deserve it!") (Text.typeface ["impact", "impact"] >> Text.color colorHUD) (identity)]

          pause = [Message -0 0 0 0 0 0 400 10000 ("PAUSED") (Text.typeface ["impact", "impact"] >> Text.color colorHUD) identity]
          append = if String.startsWith "Pro" game.stageName then []
            else if game.state == Play then List.concat [health, timer, ammo, score]
            else if game.state == Pause then List.concat [health, timer, ammo, score, pause]
            else if game.state == Menu then menu
            else if game.state == Win then List.concat [health, timer, ammo, score, win]
            else if game.state == Lose then List.concat [health, timer, ammo, score, lose]
            else []
        in
          if game.state == Menu then menu else List.append append game.messages
        ))) -- Messages
      ]

renderText : Message -> Form
renderText text = 
  toForm (Text.fromString text.message
    |> Text.height text.radius
    |> text.textOptions
    |> centered)
    |> move (text.x, text.y)
    |> text.objOptions

renderPlayer : MovingObject -> Form
renderPlayer player = 
  let
    shieldColor = colorShield
    shieldStyle = dashed (if player.health > 1 then shieldColor else if player.health == 1 then red else (rgba 0 0 0 0))
  in
    [ circle 20 |> outlined shieldStyle, polygon [(-7.5, 0), (-10, -10), (15, 0), (-10, 10)] |> filled (if player.health > 0 then player.color else colorAsteroid4) ]
      |> group
      |> move (player.x, player.y)
      |> rotate player.rotation
      |> scale (player.radius / 20)

renderAsteroid : MovingObject -> Form
renderAsteroid asteroid = 
  ngon 8 (asteroid.radius * 1.25)
    |> filled asteroid.color
    |> move (asteroid.x, asteroid.y)
    |> rotate asteroid.rotation

renderBullet : MovingObject -> Form
renderBullet bullet = 
  square bullet.radius
    |> filled bullet.color
    |> move (bullet.x, bullet.y)
    |> rotate bullet.rotation

renderParticle : MovingObject -> Form
renderParticle particle =
  circle particle.radius
    |> filled particle.color
    |> move (particle.x, particle.y)
    |> rotate particle.rotation
    |> scale (toFloat (abs particle.health))

scaleToFit : (Float, Float) -> (Float, Float) -> Float
scaleToFit (w, h) (prefW, prefH) =
  let
    wScale = w / prefW
    hScale = h / prefH
  in
    min wScale hScale


-- MAIN
main : Signal Element
main = Signal.map2 view Window.dimensions (Signal.foldp update startGame input)

-- SIGNALS
delta : Signal Float
delta = Signal.map inSeconds (fps 30)

input : Signal Input
input =
  Signal.sampleOn delta <|
    (Input <~
      Keyboard.space ~
      Keyboard.ctrl ~
      Mouse.position ~
      Mouse.isDown ~
      Window.dimensions ~
      timeSoFar)

timeSoFar : Signal Time
timeSoFar =
    Signal.foldp (+) 0 delta