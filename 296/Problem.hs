vecLength :: (Double, Double) -> Double
vecLength (x, y) = sqrt (x * x + y * y)

-- get valid side lengths according to problem
triangleSides :: Int -> [(Int, Int, Int)]
triangleSides maxPerimeter =
    [ (bc, ac, ab)
    | bc <- [1 .. maxPerimeter `div` 3]
    , ac <- [1 .. maxPerimeter `div` 2]
    , bc <= ac
    , ab <- [1 .. maxPerimeter]
    , ac <= ab
    , bc + ac > ab
    ]

-- BC AC AB -> pA pB pC
sidesToPoints :: (Int, Int, Int) -> ((Double, Double), (Double, Double), (Double, Double))
sidesToPoints (bc, ac, ab) = ((-ac_x, ac_y), (fromIntegral ab - ac_x, ac_y), (0.0, 0.0))
  where
    ac_x = (((fromIntegral ac * fromIntegral ac - fromIntegral bc * fromIntegral bc) / fromIntegral ab) + fromIntegral ab) / 2.0
    ac_y = sqrt (fromIntegral ac * fromIntegral ac - ac_x * ac_x)

-- angle bisector of vA and vB
k :: ((Double, Double), (Double, Double)) -> (Double, Double)
k ((a_x, a_y), (b_x, b_y)) = (b_len * a_x + a_len * b_x, b_len * a_y + a_len * b_y)
  where
    a_len = vecLength (a_x, a_y)
    b_len = vecLength (b_x, b_y)

main = do
    print $ triangleSides 5
    print $ map sidesToPoints $ triangleSides 5
    print $ map ((\((a_x, a_y), (b_x, b_y), (c_x, c_y)) -> k ((a_x, a_y), (b_x, b_y))) . sidesToPoints) (triangleSides 5)
