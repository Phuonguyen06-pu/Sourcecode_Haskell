-- Benchmark vật lý song song, chạy không cần cửa sổ Gloss
-- Tạo tải cố định (nhiều đạn x nhiều tường) để so sánh -N1 với -N4
-- Mỗi vòng dùng bộ đạn hơi khác nhau để GHC không dùng chung kết quả giữa các vòng
module Bench.Benchmark (runBenchmark) where

import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq    (force)
import Control.Exception  (evaluate)
import Control.Monad (forM_, when)
import Text.Printf (printf)
import GHC.Clock          (getMonotonicTime)
import Data.List (sort, nub)

import Engine.ParallelPhysics
  ( updatePhysicsSequential
  , updatePhysicsParallel
  , updatePhysicsWithChunks
  )
import Model.Config
import Model.Types
import Model.World (initGameState)

nBullets :: Int
nBullets = 2000

-- 32 x 14 = 448 khối gạch (lưới ô 30px)
benchWalls :: [Wall]
benchWalls =
  [ Wall (Vec2 x y) BreakableBrick brickMaxHp
  | x <- [-465, -435 .. 465]
  , y <- [-195, -165 .. 195]
  ]

-- Đạn bay ngang ở vùng y từ 230 tới 269 (trên hàng gạch cao nhất và vẫn trong sân),
-- không trúng tường nên mỗi viên phải quét hết danh sách tường
benchBullets :: Float -> [Bullet]
benchBullets shift =
  [ Bullet
      (Vec2 (fromIntegral ((k * 7) `mod` 900) - 450 + shift) (230 + fromIntegral (k `mod` 40)))
      (Vec2 bulletSpeed 0)
      (if even k then P1 else P2)
      True
  | k <- [0 .. nBullets - 1]
  ]

-- Đo thời gian xử lý vật lý trong nhiều frame
timeRun :: (GameState -> GameState) -> GameState -> Int -> IO Double
timeRun physics base frames = do
  t0 <- getMonotonicTime

  forM_ [1 .. frames] $ \i -> do
    let gs = base
          { bullets = benchBullets (fromIntegral i * 0.001)
          }

    _ <- evaluate (force (physics gs))
    pure ()

  t1 <- getMonotonicTime
  pure (t1 - t0)



median :: [Double] -> Double
median xs =
  let ys = sort xs
      n = length ys
  in if odd n
       then ys !! (n `div` 2)
       else (ys !! (n `div` 2 - 1) + ys !! (n `div` 2)) / 2

-- Benchmark các cặp chunk qua nhiều lượt
benchmarkChunks :: GameState -> Int -> IO ()
benchmarkChunks base frames = do
  let chunkPairs = nub
        [ (bulletChunkSize, wallChunkSize)
        , (1, 1)
        , (1, 4)
        , (1, 16)
        , (8, 4)
        , (8, 16)
        ]

      orders =
        [ chunkPairs
        , reverse chunkPairs
        , drop 2 chunkPairs ++ take 2 chunkPairs
        ]

  putStrLn "\n========== CHUNK PAIR BENCHMARK =========="
  putStrLn "Bullet | Wall | Median (s)"

  forM_ chunkPairs $ \(b, w) -> do
    _ <- timeRun (updatePhysicsWithChunks b w) base 2
    pure ()

  results <- mapM
    (\order -> mapM
      (\(b, w) -> do
        elapsed <- timeRun (updatePhysicsWithChunks b w) base frames
        pure ((b, w), elapsed))
      order)
    orders

  forM_ chunkPairs $ \(b, w) -> do
    let times =
          [ t
          | roundResults <- results
          , ((b', w'), t) <- roundResults
          , b == b'
          , w == w'
          ]

    printf "%6d | %4d | %.4f\n" b w (median times)

  putStrLn "=========================================="

-- so sánh tuần tự với cấu hình chunk đang dùng
benchmarkConfiguredChunks :: GameState -> Int -> IO ()
benchmarkConfiguredChunks base frames = do
  let sequential = updatePhysicsSequential
      parallelConfigured =
        updatePhysicsWithChunks bulletChunkSize wallChunkSize

  printf "\n========== SEQUENTIAL VS CHUNK (%d,%d) ==========\n"
    bulletChunkSize wallChunkSize

  _ <- timeRun sequential base 2
  _ <- timeRun parallelConfigured base 2

  -- Chạy xen kẽ để giảm ảnh hưởng của lần chạy đầu tiên (có thể bị cache)
  results <- mapM
    (\i ->
      if odd i
        then do
          s <- timeRun sequential base frames
          p <- timeRun parallelConfigured base frames
          pure (s, p)
        else do
          p <- timeRun parallelConfigured base frames
          s <- timeRun sequential base frames
          pure (s, p))
    [1 .. 6 :: Int]

  putStrLn "Run | Sequential (s) | Parallel (s)"

  forM_ (zip [1 :: Int ..] results) $ \(i, (s, p)) ->
    printf "%3d | %14.4f | %.4f\n" i s p

  let sequentialMedian = median (map fst results)
      parallelMedian   = median (map snd results)

  putStrLn "\n========== MEDIAN RESULTS =========="

  printf "Sequential : %.4f s\n" sequentialMedian
  printf "Parallel   : %.4f s\n" parallelMedian

  if parallelMedian > 0
    then printf "Speedup    : %.2fx\n"
           (sequentialMedian / parallelMedian)
    else putStrLn "Speedup    : N/A"

  putStrLn "===================================="

runBenchmark :: Int -> Bool -> IO ()
runBenchmark frames benchChunks = do
  caps <- getNumCapabilities

  putStrLn "========== PHYSICS BENCHMARK =========="

  printf "Bullets      : %d\n" nBullets
  printf "Walls        : %d\n" (length benchWalls)
  printf "Frames       : %d\n" frames
  printf "Capabilities : %d\n" caps

  let base = initGameState benchWalls

  _ <- timeRun updatePhysicsSequential base 2
  _ <- timeRun updatePhysicsParallel base 2

  putStrLn "\nRunning sequential..."
  sequentialTime <- timeRun updatePhysicsSequential base frames

  putStrLn "Running parallel..."
  parallelTime <- timeRun updatePhysicsParallel base frames

  putStrLn "\n========== RESULTS =========="

  printf "Sequential : %.4f s\n" sequentialTime
  printf "Parallel   : %.4f s\n" parallelTime

  if parallelTime > 0
    then printf "Speedup    : %.2fx\n"
           (sequentialTime / parallelTime)
    else putStrLn "Speedup    : N/A"

  putStrLn "================================"

  when benchChunks $ do
    benchmarkChunks base frames
    benchmarkConfiguredChunks base frames