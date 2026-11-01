-- | Kiểm thử các cơ chế mới: bẫy, lò xo, bùn, cổng dịch chuyển, thùng nổ (nổ dây chuyền)
-- và sinh sân ngẫu nhiên bằng bộ sinh số thuần túy.
module MechanicsSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Collision (tankBlockedAt)
import Engine.Explosion (blastDamageTo, detonate)
import Engine.Floors    (applyFloors, onMud)
import Engine.Random    (nextFloat, nextFloats)
import Engine.Step      (stepGame)
import Levels.Arena     (buildArena, randomCell)
import Levels.Maps      (mapLayout)
import Model.Config
import Model.Hp (hpValue)
import Model.Types
import Model.World      (initGameState, spawnP1, spawnP2, wallMaxHp)
import Parser.MapParser (cellPos)

tankAt :: Float -> Float -> Tank
tankAt x y = mkTank (Vec2 x y) 0 tankMaxHp

-- | Đang chơi, hai xe ở xa nhau, không có tường và sàn.
playing :: GameState
playing = (initGameState []) { screen = PlayScreen, tank1 = tankAt (-350) 0, tank2 = tankAt 350 0 }

genVec :: Gen Vec2
genVec = Vec2 <$> choose (-470, 470) <*> choose (-260, 260)

genWall :: Gen Wall
genWall = do
  gx <- choose (-15, 15 :: Int)
  gy <- choose (-8, 8 :: Int)
  t  <- elements [SolidSteel, BreakableBrick, ExplosiveBarrel]
  pure (Wall (Vec2 (fromIntegral gx * 30) (fromIntegral gy * 30)) t (wallMaxHp t))

spec :: Spec
spec = describe "Co che moi" $ do

  describe "Bay dien tu (Trap)" $ do
    let spot = cellPos 10 5
        trap = Floor spot Trap
    it "buoc vao bay: tru mau va bi choang" $ do
      let t = applyFloors [] [trap] (mkTank spot 0 tankMaxHp)
      hpValue (tankHp t) `shouldBe` tankMaxHp - trapDamage
      stunFrames (tankStatus t) `shouldBe` trapStunFrames
    it "dung yen tren bay khong bi tru mau lien tuc" $ do
      let t1 = applyFloors [] [trap] (mkTank spot 0 tankMaxHp)
          t2 = applyFloors [] [trap] t1
      hpValue (tankHp t2) `shouldBe` tankMaxHp - trapDamage
    it "roi bay roi quay lai thi bi tru tiep" $ do
      let t1   = applyFloors [] [trap] (mkTank spot 0 tankMaxHp)
          away = applyFloors [] [trap] (t1 { tankPos = cellPos 12 5 })
          back = applyFloors [] [trap] (away { tankPos = spot })
      hpValue (tankHp back) `shouldBe` tankMaxHp - 2 * trapDamage
    it "bay khong lam mau xuong duoi 0" $
      hpValue (tankHp (applyFloors [] [trap] (mkTank spot 0 3))) `shouldBe` 0
    it "xe dang choang khong di chuyen du dang giu phim" $ do
      let stunned = (tankAt 0 0) { tankStatus = noStatus { stunFrames = 5 } }
          gs'     = stepGame playing { tank1 = stunned, activeKeys = [P1Forward] }
      tankPos (tank1 gs') `shouldBe` Vec2 0 0
      stunFrames (tankStatus (tank1 gs')) `shouldBe` 4

  describe "Lo xo (Spring)" $ do
    let spot = cellPos 10 5
        gs0  = playing { floors = [Floor spot Spring], tank1 = mkTank spot 0 tankMaxHp }
    it "buoc vao lo xo thi bi ban di" $
      launchFrames (tankStatus (tank1 (stepGame gs0))) `shouldBe` springFrames
    it "bay thang theo huong nong, quang duong = springFrames * springSpeed" $ do
      let t = tank1 (iterate stepGame gs0 !! (springFrames + 1))
      vx (tankPos t) `shouldBe` vx spot + fromIntegral springFrames * springSpeed
      vy (tankPos t) `shouldBe` vy spot
      launchFrames (tankStatus t) `shouldBe` 0
    it "dung vat can thi dung bay (khong xuyen tuong)" $ do
      let wall = Wall (cellPos 11 5) SolidSteel steelHp
          t    = tank1 (iterate stepGame gs0 { walls = [wall] } !! 3)
      launchFrames (tankStatus t) `shouldBe` 0
      tankPos t `shouldBe` spot

  describe "Bun lay (Mud)" $ do
    let p  = cellPos 8 8
        fs = [Floor p Mud]
    it "onMud dung o tren o bun, sai o ngoai" $ do
      onMud fs p `shouldBe` True
      onMud fs (Vec2 (vx p + 10) (vy p + 10)) `shouldBe` True
      onMud fs (cellPos 9 8) `shouldBe` False
    it "tren bun xe di cham con 30% toc do" $ do
      let gs0 = playing { floors = fs, tank1 = mkTank p 0 tankMaxHp, activeKeys = [P1Forward] }
          dx  = vx (tankPos (tank1 (stepGame gs0))) - vx p
      abs (dx - tankSpeed * mudFactor) `shouldSatisfy` (< 0.001)
    it "ngoai bun xe di toc do binh thuong" $ do
      let gs0 = playing { tank1 = mkTank p 0 tankMaxHp, activeKeys = [P1Forward] }
          dx  = vx (tankPos (tank1 (stepGame gs0))) - vx p
      abs (dx - tankSpeed) `shouldSatisfy` (< 0.001)

  describe "Cong dich chuyen (Teleporter)" $ do
    let a  = cellPos 3 3
        b  = cellPos 28 14
        fs = [Floor a (Teleporter b), Floor b (Teleporter a)]
    it "dung tren cong thi nhay sang cong kia, lui ra phia truoc" $ do
      let t = applyFloors [] fs (mkTank a 0 tankMaxHp)
      tankPos t `shouldBe` Vec2 (vx b + teleportExitDist) (vy b)
      teleCooldown (tankStatus t) `shouldBe` teleportCooldownFrames
    it "co hoi chieu nen khong nhay lai ngay" $ do
      let t1 = applyFloors [] fs (mkTank a 0 tankMaxHp)
          t2 = applyFloors [] fs (t1 { tankPos = a })
      tankPos t2 `shouldBe` a
      teleCooldown (tankStatus t2) `shouldBe` teleportCooldownFrames - 1
    it "cho dap bi chan thi dap dung tam cong" $ do
      let wall = Wall (Vec2 (vx b + teleportExitDist) (vy b)) SolidSteel steelHp
      tankPos (applyFloors [wall] fs (mkTank a 0 tankMaxHp)) `shouldBe` b
    it "xe o xa cong thi khong bi dich chuyen" $
      tankPos (applyFloors [] fs (tankAt 0 0)) `shouldBe` Vec2 0 0

  describe "Thung no (Explosion)" $ do
    let c         = cellPos 10 5
        brickNear = Wall (cellPos 12 7) BreakableBrick brickMaxHp   -- lech 2 o: trong vung 5x5
        brickFar  = Wall (cellPos 13 5) BreakableBrick brickMaxHp   -- lech 3 o: ngoai vung
        steelNear = Wall (cellPos 11 5) SolidSteel steelHp
    it "pha gach trong vung 5x5, giu thep va gach ngoai vung" $
      detonate [c] [brickNear, brickFar, steelNear] `shouldBe` ([brickFar, steelNear], [c])
    it "khong co thung no thi khong co gi xay ra" $
      detonate [] [brickNear, steelNear] `shouldBe` ([brickNear, steelNear], [])
    it "no day chuyen: thung trong vung se no theo, thung o xa thi khong" $ do
      let b2 = Wall (cellPos 12 5) ExplosiveBarrel barrelHp
          b3 = Wall (cellPos 20 5) ExplosiveBarrel barrelHp
      detonate [c] [b2, b3] `shouldBe` ([b3], [c, wallPos b2])
    it "sat thuong len xe theo khoang cach va cong don theo so vu no" $ do
      blastDamageTo [c] (tankAt (vx c + 30) (vy c)) `shouldBe` blastDamage
      blastDamageTo [c] (tankAt (vx c + 100) (vy c)) `shouldBe` 0
      blastDamageTo [c, c] (tankAt (vx c + 30) (vy c)) `shouldBe` 2 * blastDamage
    it "dan ban trung thung no: thung bien mat, xe gan do mat mau, hieu ung no xuat hien roi tan" $ do
      let barrel = Wall (Vec2 0 0) ExplosiveBarrel barrelHp
          gs0    = playing { walls   = [barrel]
                           , bullets = [Bullet (Vec2 (-20) 0) (Vec2 bulletSpeed 0) P1 True]
                           , tank2   = tankAt 50 0 }
          gs1    = stepGame gs0
      walls gs1 `shouldBe` []
      hpValue (tankHp (tank2 gs1)) `shouldBe` tankMaxHp - blastDamage
      hpValue (tankHp (tank1 gs1)) `shouldBe` tankMaxHp
      length (explosions gs1) `shouldBe` 1
      explosions (iterate stepGame gs1 !! 20) `shouldBe` []
    it "tinh chat: thep khong bao gio bi pha, ket qua la tap con, giu du cac tam ban dau" $
      property $ withMaxSuccess 200 $
        forAll (resize 40 (listOf genWall)) $ \ws ->
        forAll (listOf genVec) $ \cs ->
          let (ws', cs') = detonate cs ws
              steel xs   = length (filter ((== SolidSteel) . wallType) xs)
          in steel ws' == steel ws && all (`elem` ws) ws' && take (length cs) cs' == cs

  describe "Sinh san ngau nhien (thuan tuy)" $ do
    it "randomCell: 12% thep, 10% gach, con lai trong" $ do
      randomCell 0.05 `shouldBe` Just SolidSteel
      randomCell 0.15 `shouldBe` Just BreakableBrick
      randomCell 0.50 `shouldBe` Nothing
    it "nextFloat luon nam trong [0,1)" $
      property $ \w -> let (x, _) = nextFloat (Seed w) in x >= 0 && x < 1
    it "nextFloats n tra ve dung n so, va so dau bang nextFloat" $ do
      let (xs, _) = nextFloats 5 (Seed 7)
      length xs `shouldBe` 5
      take 1 xs `shouldBe` [fst (nextFloat (Seed 7))]
    it "cung hat giong cung ra mot san, khac hat giong ra san khac" $ do
      let lay = mapLayout Map1
      buildArena (Seed 42) lay `shouldBe` buildArena (Seed 42) lay
      let (w1, _, _) = buildArena (Seed 1) lay
          (w2, _, _) = buildArena (Seed 2) lay
      w1 `shouldSatisfy` (/= w2)
    it "giu nguyen phan co dinh, vat can them chi nam o cac o '?', hat giong tien len" $ do
      let lay            = mapLayout Map2
          fixed          = layoutWalls lay
          (ws, fs, seed') = buildArena (Seed 99) lay
      take (length fixed) ws `shouldBe` fixed
      fs `shouldBe` layoutFloors lay
      all ((`elem` layoutRandom lay) . wallPos) (drop (length fixed) ws) `shouldBe` True
      seed' `shouldSatisfy` (/= Seed 99)
    it "hai diem xuat phat luon thoang tren moi san ngau nhien (3 map x 30 hat giong)" $
      and [ not (tankBlockedAt ws sp)
          | n <- [Map1, Map2, Map3]
          , s <- [1 .. 30]
          , let (ws, _, _) = buildArena (Seed s) (mapLayout n)
          , sp <- [spawnP1, spawnP2] ]
        `shouldBe` True
