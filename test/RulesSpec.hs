-- | Kiểm thử các luật chính của game (hàm thuần).
module RulesSpec (spec) where

import Test.Hspec

import Engine.Input            (tryFire)
import Engine.ParallelPhysics
import Engine.Step
import Model.Config
import Model.Hp (mkHp, hpValue)
import Model.Types
import Model.World

tankAt :: Float -> Float -> Tank
tankAt x y = mkTank (Vec2 x y) 0 tankMaxHp

bulletAt :: Float -> Float -> PlayerId -> Bullet
bulletAt x y o = Bullet (Vec2 x y) (Vec2 bulletSpeed 0) o True

brickAt :: Float -> Float -> Wall
brickAt x y = Wall (Vec2 x y) BreakableBrick brickMaxHp

steelAt :: Float -> Float -> Wall
steelAt x y = Wall (Vec2 x y) SolidSteel steelHp

barrelAt :: Float -> Float -> Wall
barrelAt x y = Wall (Vec2 x y) ExplosiveBarrel barrelHp

far1, far2 :: Tank
far1 = tankAt (-350) 0
far2 = tankAt 350 0

-- | Đang chơi, hai xe ở xa nhau, không có tường.
playing :: GameState
playing = (initGameState []) { screen = PlayScreen, tank1 = far1, tank2 = far2 }

spec :: Spec
spec = describe "Rules" $ do

  describe "processBullet" $ do
    it "dan bay thang thi di chuyen va con song" $ do
      let (b, h) = processBullet [] far1 far2 (bulletAt 0 0 P1)
      vx (bulletPos b) `shouldBe` bulletSpeed
      bulletAlive b `shouldBe` True
      h `shouldBe` NoHit
    it "trung tuong => dan chet, HitWall chi so" $ do
      let (b, h) = processBullet [steelAt 200 200, brickAt 40 0] far1 far2 (bulletAt 20 0 P1)
      bulletAlive b `shouldBe` False
      h `shouldBe` HitWall (WallIx 1)
    it "trung xe doi phuong => HitTank" $ do
      let (b, h) = processBullet [] far1 far2 (bulletAt 330 0 P1)
      bulletAlive b `shouldBe` False
      h `shouldBe` HitTank P2
    it "dan cua chinh chu di xuyen qua xe cua minh" $ do
      let (b, h) = processBullet [] far1 far2 (bulletAt 330 0 P2)
      bulletAlive b `shouldBe` True
      h `shouldBe` NoHit
    it "ra ngoai bien => dan chet, khong sat thuong" $ do
      let (b, h) = processBullet [] far1 far2 (bulletAt 499 0 P1)
      bulletAlive b `shouldBe` False
      h `shouldBe` NoHit

  describe "damageWallsParallel" $ do
    it "gach vo sau 1 phat" $
      damageWallsParallel [HitWall (WallIx 0)] [brickAt 0 0] `shouldBe` []
    it "thep khong vo du trung nhieu dan" $
      damageWallsParallel (replicate 10 (HitWall (WallIx 0))) [steelAt 0 0] `shouldBe` [steelAt 0 0]
    it "thung no bien mat sau 1 phat" $
      damageWallsParallel [HitWall (WallIx 0)] [barrelAt 0 0] `shouldBe` []
    it "chi khoi bi trung bi pha" $
      damageWallsParallel [HitWall (WallIx 1)] [brickAt 0 0, brickAt 40 0] `shouldBe` [brickAt 0 0]
    it "nhieu dan cung trung mot khoi van chi pha khoi do" $
      damageWallsParallel [HitWall (WallIx 1), HitWall (WallIx 1), HitWall (WallIx 1)] [brickAt 0 0, brickAt 40 0]
        `shouldBe` [brickAt 0 0]

  describe "checkGameOver" $ do
    it "dang choi khi ca hai con mau" $
      screen (checkGameOver playing) `shouldBe` PlayScreen
    it "P1 thang khi P2 het mau" $
      screen (checkGameOver playing { tank2 = (tank2 playing) { tankHp = mkHp 0 } })
        `shouldBe` GameOverScreen (Winner P1)
    it "P2 thang khi P1 het mau" $
      screen (checkGameOver playing { tank1 = (tank1 playing) { tankHp = mkHp 0 } })
        `shouldBe` GameOverScreen (Winner P2)
    it "hoa khi ca hai het mau" $
      screen (checkGameOver playing { tank1 = (tank1 playing) { tankHp = mkHp 0 }
                                    , tank2 = (tank2 playing) { tankHp = mkHp 0 } })
        `shouldBe` GameOverScreen Draw

  describe "driveTank (xoay va tien/lui kieu xe tang)" $ do
    let drive ws keys t = driveTank ws P1Forward P1Back P1Left P1Right keys t
    it "di thang khi khong bi chan" $
      vx (tankPos (drive [] [P1Forward] (tankAt 0 0))) `shouldBe` tankSpeed
    it "lui khi giu phim lui" $
      vx (tankPos (drive [] [P1Back] (tankAt 0 0))) `shouldBe` negate tankSpeed
    it "xoay tai cho: giu phim trai/phai chi doi huong, khong doi vi tri" $ do
      let l = drive [] [P1Left] (tankAt 10 10)
          r = drive [] [P1Right] (tankAt 10 10)
      tankAngle l `shouldBe` tankTurnSpeed
      tankAngle r `shouldBe` negate tankTurnSpeed
      tankPos l `shouldBe` Vec2 10 10
      tankPos r `shouldBe` Vec2 10 10
    it "vua xoay vua tien: di theo huong moi" $ do
      let t = drive [] [P1Forward, P1Left] (mkTank (Vec2 0 0) 90 tankMaxHp)
      tankAngle t `shouldBe` 90 + tankTurnSpeed
      vy (tankPos t) `shouldSatisfy` (> 0)
    it "bi tuong chan" $
      tankPos (drive [steelAt 26 0] [P1Forward] (tankAt 0 0)) `shouldBe` Vec2 0 0
    it "bi bien chan" $
      tankPos (drive [] [P1Forward] (tankAt 479 0)) `shouldBe` Vec2 479 0
    it "cham tuong cheo thi dung lai, khong truot doc tuong" $ do
      let t0 = mkTank (Vec2 0 0) 45 tankMaxHp
          t1 = drive [steelAt 26 0] [P1Forward] t0

      tankPos t1 `shouldBe` tankPos t0
    it "khong giu phim thi dung yen" $
      tankPos (drive [] [] (tankAt 10 10)) `shouldBe` Vec2 10 10
    it "driveTankAt: he so toc do nhan vao quang duong moi frame" $
      vx (tankPos (driveTankAt mudFactor [] P1Forward P1Back P1Left P1Right [P1Forward] (tankAt 0 0)))
        `shouldBe` mudFactor * tankSpeed

  describe "tank collision" $ do

    it "hai xe khong duoc chong len nhau khi doi dau" $ do
      let gs = playing
            { tank1 = tankAt (-tankRadius) 0
            , tank2 = (tankAt tankRadius 0)
                { tankAngle = 180 }
            , activeKeys = [P1Forward, P2Forward]
            }

          gs' = stepGame gs

      tankPos (tank1 gs') `shouldBe` tankPos (tank1 gs)
      tankPos (tank2 gs') `shouldBe` tankPos (tank2 gs)

    it "xe dung yen khong bi day khi xe kia tien toi" $ do
      let gs = playing
            { tank1 = tankAt (-tankRadius) 0
            , tank2 = tankAt tankRadius 0
            , activeKeys = [P1Forward]
            }

          gs' = stepGame gs

      tankPos (tank1 gs') `shouldBe` tankPos (tank1 gs)
      tankPos (tank2 gs') `shouldBe` tankPos (tank2 gs)

    it "hai xe van co the lui ra xa nhau" $ do
      let gs = playing
            { tank1 = tankAt (-tankRadius) 0
            , tank2 = (tankAt tankRadius 0)
                { tankAngle = 180 }
            , activeKeys = [P1Back, P2Back]
            }

          gs' = stepGame gs

      vx (tankPos (tank1 gs')) `shouldSatisfy` (< (-tankRadius))
      vx (tankPos (tank2 gs')) `shouldSatisfy` (> tankRadius)

    it "xe van xoay duoc khi dang cham xe khac" $ do
      let gs = playing
            { tank1 = tankAt (-tankRadius) 0
            , tank2 = tankAt tankRadius 0
            , activeKeys = [P1Forward, P1Left]
            }

          gs' = stepGame gs

      tankAngle (tank1 gs') `shouldBe` tankTurnSpeed


  describe "stepGame" $ do
    it "dan trung xe tru dung bulletDamage" $ do
      let gs' = stepGame playing { bullets = [bulletAt 330 0 P1] }
      hpValue (tankHp (tank2 gs')) `shouldBe` tankMaxHp - bulletDamage
      length (bullets gs') `shouldBe` 0
      hitsOnP2 (stats gs') `shouldBe` 1
    it "ngoai PlayScreen thi trang thai khong doi" $
      stepGame (initGameState []) `shouldBe` initGameState []
    it "dang tam dung (PauseScreen) thi dan va xe dung yen" $ do
      let paused = playing { screen = PauseScreen, bullets = [bulletAt 0 0 P1], activeKeys = [P1Forward] }
      stepGame paused `shouldBe` paused

  describe "tryFire (cooldown, nong doi)" $ do
    it "lan ban dau tao HAI vien dan, lan ngay sau bi cooldown chan" $ do
      let once  = tryFire P1 playing
          twice = tryFire P1 once
      length (bullets once) `shouldBe` 2
      length (bullets twice) `shouldBe` 2
      cooldown1 once `shouldBe` fireCooldown
    it "hai vien dan bay song song, lech hai ben truc nong" $ do
      let ys = [vy (bulletPos b) | b <- bullets (tryFire P1 playing)]
      ys `shouldBe` [barrelOffset, negate barrelOffset]
    it "het cooldown thi ban lai duoc" $ do
      let gs' = iterate stepGame (tryFire P1 playing) !! fireCooldown
      cooldown1 gs' `shouldBe` 0
    it "mot lan ban chi tinh mot phat vao thong ke" $
      shotsP1 (stats (tryFire P1 playing)) `shouldBe` 1

  describe "MatchStats (Monoid)" $
    it "mempty la phan tu don vi va (<>) cong don" $ do
      let a = shotStats P1 <> hitStats (HitTank P2)
      (a <> mempty) `shouldBe` a
      (mempty <> a) `shouldBe` a
      shotsP1 (a <> a) `shouldBe` 2
