--import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = Int
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--getters for the cities and the distance from the tuples
getCity1 :: (City, City, Distance) -> City
getCity1 (x, _, _) =  x
getCity2 :: (City, City, Distance) -> City
getCity2 (_, x,_) =  x
getDistance :: (City, City, Distance) -> Distance
getDistance(_, _, x) =  x

--conditional to add cities to list, avoiding duplicates
addIfNotInCityList :: (City, City, Distance) -> [City] -> [City]
addIfNotInCityList (a, b, _) mlist
    | a `elem` mlist && b `elem` mlist = mlist
    | b `elem` mlist = a : mlist
    | a `elem` mlist = b : mlist
    | otherwise = a : b : mlist
cities :: RoadMap -> [City]
cities roadmap = foldr(\tup acc -> addIfNotInCityList tup acc) [] roadmap -- modifiy this line to implement the solution, for each exercise not solved, leave the function definition like this
 
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap c1 c2 = any(\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) roadmap


distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap c1 c2 | areAdjacent roadmap c1 c2 = 
                        let result =  [d | (x, y, d) <- roadmap, (x == c1 && y == c2) || (x == c2 && y == c1)] 
                        in if null result then Nothing else Just (head result)
                        | otherwise = Nothing

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadmap c1 = [(y, d) | (x, y, d) <- roadmap, x == c1] ++ [(x,d) | (x,y,d) <- roadmap, y == c1]

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [(7,6,1),(8,2,2),(6,5,2),(0,1,4),(2,5,4),(8,6,6),(2,3,7),(7,8,7),(0,7,8),(1,2,8),(3,4,9),(5,4,10),(1,7,11),(3,5,14)]

gTest2 :: RoadMap
gTest2 = [(0,1,10),(0,2,15),(0,3,20),(1,2,35),(1,3,25),(2,3,30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [(0,1,4),(2,3,2)]
