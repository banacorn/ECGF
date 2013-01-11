import Control.Monad (forM)
import GF

type EC = (Order, GF, GF)
type Point = (EC, GF, GF)

add :: Point -> Point -> Point
add ((p, a, b), x0, y0) (ec, x1, y1) = (ec, x2, y2)
    where   slope   = (y1 .- y0) ./ (x1 .- x0)
            x2      = slope .* slope .- (x0 .+ x1)
            y2      = (x0 .- x2) .* slope .- y0

double :: Point -> Point
double ((p, a, b), x, y) = ((p, a, b), x0, y0)
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
        True    -> putStrLn . show $ ((p, GF p a, GF p b), GF p x, GF p y) `times` n
        False   -> putStrLn "wrong"