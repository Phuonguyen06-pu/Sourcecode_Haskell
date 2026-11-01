{-# LANGUAGE ScopedTypeVariables #-}
-- Kiểm thử tính chất: kết quả SONG SONG bằng kết quả TUẦN TỰ
module Parallel.PropertySpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Math             (distSq)
import Engine.ParallelPhysics
import Model.Config
import Model.Hp (hpValue)
import Model.Types
import Model.World             (initGameState, wallMaxHp)

genVec :: Gen Vec2
genVec = Vec2 <$> choose (-470, 470) <*> choose (-260, 260)

genBullet :: Gen Bullet
genBullet = do
  p <- genVec
  v <- Vec2 <$> choose (-9, 9) <*> choose (-9, 9)
  o <- elements [P1, P2]
  pure (Bullet p v o True)

genWall :: Gen Wall
genWall = do
  gx <- choose (-15, 15 :: Int)
  gy <- choose (-8, 8 :: Int)
  t  <- elements [SolidSteel, BreakableBrick, ExplosiveBarrel]
  pure (Wall (Vec2 (fromIntegral gx * 30) (fromIntegral gy * 30)) t (wallMaxHp t))

genTank :: Gen Tank
genTank = (\p -> mkTank p 0 tankMaxHp) <$> genVec

genScene :: Gen ([Wall], Tank, Tank, [Bullet])
genScene = (,,,) <$> listOf genWall <*> genTank <*> genTank <*> resize 60 (listOf genBullet)

genHits :: Gen ([Hit], [Wall])
genHits = do
  ws <- resize 40 (listOf genWall)
  let n = length ws
  hs <- listOf (oneof ([pure NoHit, HitTank <$> elements [P1, P2]]
                       ++ [HitWall . WallIx <$> choose (0, n - 1) | n > 0]))
  pure (hs, ws)

-- | Bản tuần tự tham chiếu của damageWallsParallel.
damageSeq :: [Hit] -> [Wall] -> [Wall]
damageSeq hits ws = filter ((> 0) . wallHp) (zipWith dmg [0 ..] ws)
  where
    n :: Int -> Int
    n i = length (filter (== HitWall (WallIx i)) hits)
    dmg :: Int -> Wall -> Wall
    dmg i w = case wallType w of
      SolidSteel      -> w
      BreakableBrick  -> w { wallHp = wallHp w - brickDamage * n i }
      ExplosiveBarrel -> w { wallHp = wallHp w - barrelHp * n i }

spec :: Spec
spec = describe "Property-based testing" $ do

  it "distSq doi xung (100 bo du lieu)" $
    property $ withMaxSuccess 100 $ \(x1 :: Int) (y1 :: Int) (x2 :: Int) (y2 :: Int) ->
      let a = Vec2 (fromIntegral x1) (fromIntegral y1)
          b = Vec2 (fromIntegral x2) (fromIntegral y2)
      in distSq a b == distSq b a

  it "xu ly dan SONG SONG == TUAN TU" $
    property $ withMaxSuccess 200 $ forAll genScene $ \(ws, t1, t2, bs) ->
      updateBulletsParallel ws t1 t2 bs == map (processBullet ws t1 t2) bs

  it "pha huy tuong SONG SONG == TUAN TU" $
    property $ withMaxSuccess 200 $ forAll genHits $ \(hs, ws) ->
      damageWallsParallel hs ws == damageSeq hs ws

  it "vat ly SONG SONG == TUAN TU voi dan va tuong thuc te" $
    property $ withMaxSuccess 200 $
      forAll genScene $ \(ws, t1, t2, bs) ->
        let gs = (initGameState ws)
              { tank1 = t1
              , tank2 = t2
              , bullets = bs
              }

            sequential = updatePhysicsSequential gs
            parallel   = updatePhysicsParallel gs

        in parallel == sequential

  it "vat ly song song va tuan tu giong nhau khi dan cham tuong" $ do
    let ws =
          [ Wall (Vec2 60 0) BreakableBrick brickMaxHp
          , Wall (Vec2 120 0) SolidSteel steelHp
          ]

        bs =
          [ Bullet (Vec2 45 0) (Vec2 bulletSpeed 0) P1 True
          , Bullet (Vec2 105 0) (Vec2 bulletSpeed 0) P2 True
          ]

        gs = (initGameState ws)
          { bullets = bs
          }

    updatePhysicsParallel gs
      `shouldBe` updatePhysicsSequential gs

  it "so dan khong bao gio tang sau mot buoc vat ly" $
    property $ withMaxSuccess 100 $ forAll genScene $ \(ws, t1, t2, bs) ->
      let gs  = (initGameState ws) { tank1 = t1, tank2 = t2, bullets = bs }
          gs' = updatePhysicsParallel gs
      in length (bullets gs') <= length bs

  it "mau xe khong bao gio am" $
    property $ withMaxSuccess 100 $ forAll genScene $ \(ws, t1, t2, bs) ->
      let gs' = updatePhysicsParallel ((initGameState ws) { tank1 = t1, tank2 = t2, bullets = bs })
      in hpValue (tankHp (tank1 gs')) >= 0
          && hpValue (tankHp (tank2 gs')) >= 0
