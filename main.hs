import Control.Monad (forM)
import GF

type EC = (Order, GF, GF)
type Point = (EC, GF, GF)

add :: Point -> Point -> Point
add p ((11, GF 11 1, GF 11 6), GF 11 0, GF 11 0) = p
add ((11, GF 11 1, GF 11 6), GF 11 0, GF 11 0) p = p
add p0 p1
    | p0 == p1 = double p0
    | x1 == x0 = (ec, GF p 0, GF p 0)
    | otherwise = (ec, x2, y2)
    where   (ec, x0, y0)    = p0
            (_, x1, y1)     = p1
            (p, a, b)       = ec
            slope           = (y1 .- y0) ./ (x1 .- x0)
            x2              = slope .* slope .- (x0 .+ x1)
            y2              = (x0 .- x2) .* slope .- y0

double :: Point -> Point
double ((p, a, b), x, y)
    | y == GF p 0 = ((p, a, b), GF p 0, GF p 0)
    | otherwise = ((p, a, b), x0, y0)
    where   slope   = (x .* x .* (GF p 3) .+ a) ./ (y .+ y)
            x0      = slope .* slope .- x .- x
            y0      = (x .- x0) .* slope .- y

times :: Point -> Integer -> Point
times point 1 = point
times point number
    | even number = double (point `times` (number `div` 2))
    | odd number  = (point `times` (pred number)) `add` point

validate :: Point -> Bool
validate ((p, a, b), x, y) = y .* y == x .* x .* x .+ x .* a .+ b

main = do
    [p, a, b, x, y, n] <- forM [1..6] (const (fmap read getLine)) :: IO [Integer]
    case validate ((p, GF p a, GF p b), GF p x, GF p y) of
        True    -> case ((p, GF p a, GF p b), GF p x, GF p y) `times` n of 
                        ((_, _, _), GF _ 0, GF _ 0) -> putStrLn "infinity"
                        ((_, _, _), x, y) -> do
                            putStrLn $ show x 
                            putStrLn $ show y
        False   -> putStrLn "wrong"

