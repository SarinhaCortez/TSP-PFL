import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

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
cities roadmap = foldr(\tup acc -> addIfNotInCityList tup acc) [] roadmap 
 
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
pathDistance _ [] = Just 0 
pathDistance _ [_] = Just 0 
pathDistance roadmap (c1:c2:rest) =
    case distance roadmap c1 c2 of
        Nothing -> Nothing 
        Just d  -> case pathDistance roadmap (c2:rest) of
                      Nothing -> Nothing 
                      Just restDist -> Just (d + restDist)

-- aux function for rome - counts occurrences of a city in a roadmap
countOccurrences :: RoadMap -> City -> Int
countOccurrences [] _ = 0
countOccurrences ((city1, city2, _):rest) city = 
  (if city == city1 || city == city2 then 1 else 0) + countOccurrences rest city

rome :: RoadMap -> [City]
rome roadmap = let cities_list = cities roadmap
                   city_count = [(city, countOccurrences roadmap city) | city <- cities_list]
                   max_count = maximum $ map snd city_count
               in [city | (city, count) <- city_count, count == max_count]

dfsVisit :: City -> RoadMap -> [City] -> [City]
dfsVisit city roads visited
    | city `elem` visited = visited  
    | otherwise =
        let newVisited = city : visited 
            neighbors = map fst (adjacent roads city)  
        in foldr (\neighbor acc -> dfsVisit neighbor roads acc) newVisited neighbors 

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = True  
isStronglyConnected roads =
    let start = if null (cities roads) then error "no cities" else head (cities roads)
        visited = dfsVisit start roads [] 
    in length visited == length (cities roads)

shortestPath :: RoadMap -> City -> City -> Path
shortestPath rmap start end = 
    let unvisited = cities rmap
        initDists = [(c, if c == start then 0 else maxBound) | c <- unvisited]
        initPath = [(c, Nothing) | c <- unvisited]
        (finalDists, finalPath) = dijkstra end rmap unvisited initDists initPath
        result = reconstruct finalPath start end
        Just end_dist = lookup end finalDists
    in if (start `elem` unvisited) && (end `elem` unvisited) then (if end_dist == maxBound then [] else result) else error "One or more cities is invalid in this roadmap."

dijkstra :: City -> RoadMap -> [City] -> [(City, Distance)] -> [(City, Maybe City)] -> ([(City, Distance)], [(City, Maybe City)])
dijkstra end rmap [] dists path = (dists, path)
dijkstra end rmap unvisited dists path =
    let current = Data.List.minimumBy (\c1 c2 -> compare (lookup c1 dists) (lookup c2 dists)) unvisited
        Just currentDist = lookup current dists
    in if (currentDist == maxBound)
       then (dists, path)
       else 
           let neighbours = adjacent rmap current
               (newDists, newPath) = foldl (relax current) (dists, path) neighbours
           in dijkstra end rmap (Data.List.delete current unvisited) newDists newPath

relax :: City -> ([(City, Distance)], [(City, Maybe City)]) -> (City, Distance) -> ([(City, Distance)], [(City, Maybe City)])
relax current (dists, path) (neighbour, edgeDist) =
    let 
        Just currentDist = lookup current dists      
        Just neighbourDist = lookup neighbour dists  
        newDist = currentDist + edgeDist
    in 
        if newDist < neighbourDist
            then (updateDist neighbour newDist dists, updatePath neighbour current path)
            else (dists, path)

updateDist :: City -> Distance -> [(City, Distance)] -> [(City, Distance)]
updateDist city newDist dists = 
    map (\(c, d) -> if c == city then (c, newDist) else (c, d)) dists

updatePath :: City -> City -> [(City, Maybe City)] -> [(City, Maybe City)]
updatePath city path pathList =
    map (\(c, p) -> if c == city then (c, Just path) else (c, p)) pathList

reconstruct :: [(City, Maybe City)] -> City -> City -> Path
reconstruct path start end = reverse (buildResult path end)
    where
        buildResult path current
            | current == start = [start]
            | otherwise = case lookup current path of
                            Just (Just path') -> current : buildResult path path'
                            _ -> [current] 

travelSales :: RoadMap -> Path
travelSales = undefined
-- usar bitmask

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [(7,6,1),(8,2,2),(6,5,2),(0,1,4),(2,5,4),(8,6,6),(2,3,7),(7,8,7),(0,7,8),(1,2,8),(3,4,9),(5,4,10),(1,7,11),(3,5,14)]

gTest2 :: RoadMap
gTest2 = [(0,1,10),(0,2,15),(0,3,20),(1,2,35),(1,3,25),(2,3,30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [(0,1,4),(2,3,2)]
