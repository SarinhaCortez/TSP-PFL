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
 
-- | Checks if two cities are adjacent in the given roadmap. Two cities are considered adjacent if there is a direct road connecting them.
-- Parameters:
--   roadmap - The roadmap containing all the cities and their connections.
--   c1      - The first city to check for adjacency.
--   c2      - The second city to check for adjacency.
-- Returns:
--   True if the cities are adjacent; otherwise, False.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap c1 c2 = 
    any (\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) roadmap

-- | Retrieves the distance between two cities in the given roadmap.
-- If the cities are adjacent, the function will return the distance as a `Just` value;otherwise, it returns `Nothing`.
-- Parameters:
--   roadmap - The roadmap containing all the cities and their connections.
--   c1      - The first city for which the distance is being calculated.
--   c2      - The second city for which the distance is being calculated.
-- Returns:
--   A `Maybe Distance` that contains the distance between the two cities if they are adjacent;
--   otherwise, returns Nothing.
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap c1 c2 
    | areAdjacent roadmap c1 c2 = 
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

-- | Auxiliary function that visits a city in a depth-first manner and returns a list of visited cities.
-- The function continues visiting neighboring cities until all reachable cities are visited.
-- 
-- Parameters:
--   city    - The current city to visit.
--   roads   - The roadmap containing all cities and their connections.
--   visited - A list of already visited cities.
--
-- Returns:
--   A list of cities that have been visited during the DFS traversal.
dfsVisit :: City -> RoadMap -> [City] -> [City]
dfsVisit city roads visited
    | city `elem` visited = visited  
    | otherwise =
        let newVisited = city : visited 
            neighbors = map fst (adjacent roads city)  
        in foldr (\neighbor acc -> dfsVisit neighbor roads acc) newVisited neighbors 

-- | Checks if the given roadmap is strongly connected.
-- A roadmap is strongly connected if there is a path from any city to every other city.
-- 
-- Parameters:
--   roads - The roadmap containing all cities and their connections.
--
-- Returns:
--   True if the roadmap is strongly connected; otherwise, False.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = True  
isStronglyConnected roads =
    let start = if null (cities roads) then error "no cities" else head (cities roads)
        visited = dfsVisit start roads [] 
    in length visited == length (cities roads)

-- | Finds all the shortest paths between two cities in a roadmap using Dijkstra's algorithm.
-- It initializes distances and paths, then computes the shortest paths from the start city to the end city.
-- 
-- Parameters:
--   rmap  - The roadmap containing all cities and their connections.
--   start - The starting city for the path.
--   end   - The destination city for the path.
--
-- Returns:
--   A list of paths representing the shortest paths from start to end.

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rmap start end =
    let unvisited = cities rmap
        initDists = [(c, if c == start then 0 else maxBound) | c <- unvisited]
        initPaths = [(c, if c == start then [[start]] else []) | c <- unvisited]
        (finalDists, finalPaths) = dijkstra end rmap unvisited initDists initPaths
    in case lookup end finalPaths of
        Just paths -> paths
        Nothing -> []

-- | Performs Dijkstra's algorithm to find all shortest paths to the end city.
-- It recursively visits unvisited cities and relaxes the edges to update distances and paths.
-- 
-- Parameters:
--   end      - The destination city for the path.
--   rmap     - The roadmap containing all cities and their connections.
--   unvisited - A list of cities that have not been visited yet.
--   dists    - A list of tuples containing cities and their current distances from the start city.
--   paths    - A list of tuples containing cities and their current shortest paths.
--
-- Returns:
--   A tuple containing updated distances and paths.
dijkstra :: City -> RoadMap -> [City] -> [(City, Distance)] -> [(City, [Path])] -> ([(City, Distance)], [(City, [Path])])
dijkstra end rmap [] dists paths = (dists, paths)
dijkstra end rmap unvisited dists paths =
    let current = Data.List.minimumBy (\c1 c2 -> compare (lookup c1 dists) (lookup c2 dists)) unvisited
        Just currentDist = lookup current dists
    in if currentDist == maxBound
       then (dists, paths)
       else 
           let neighbours = adjacent rmap current
               (newDists, newPaths) = foldl (relax current) (dists, paths) neighbours
           in dijkstra end rmap (Data.List.delete current unvisited) newDists newPaths

-- | Relaxes the edge between the current city and its neighboring city,
-- updating the distances and paths if a shorter path is found.
-- 
-- Parameters:
--   current  - The current city being relaxed.
--   (dists, paths) - A tuple containing the current distances and paths.
--   (neighbour, edgeDist) - A tuple containing the neighboring city and the distance to it.
--
-- Returns:
--   Updated distances and paths after relaxing the edge.
relax :: City -> ([(City, Distance)], [(City, [Path])]) -> (City, Distance) -> ([(City, Distance)], [(City, [Path])])
relax current (dists, paths) (neighbour, edgeDist) =
    let Just currentDist = lookup current dists
        Just neighbourDist = lookup neighbour dists
        Just currentPaths = lookup current paths
        Just neighbourPaths = lookup neighbour paths
        newDist = currentDist + edgeDist
        newPaths = map (++ [neighbour]) currentPaths
    in case compare newDist neighbourDist of 
        LT -> (updateDist neighbour newDist dists, 
               updatePaths neighbour newPaths paths)
        EQ -> (dists,
               updatePaths neighbour (neighbourPaths ++ newPaths) paths)
        GT -> (dists, paths)

-- | Updates the distance of a specified city in the distance list.
-- 
-- Parameters:
--   city    - The city whose distance is to be updated.
--   newDist - The new distance to set for the city.
--   dists   - The current list of city distances.
--
-- Returns:
--   A new list of distances with the updated distance for the specified city.
updateDist :: City -> Distance -> [(City, Distance)] -> [(City, Distance)]
updateDist city newDist dists =
    map (\(c, d) -> if c == city then (c, newDist) else (c, d)) dists

-- | Updates the list of paths for a specified city with new paths.
-- 
-- Parameters:
--   city     - The city whose paths are to be updated.
--   newPaths - A list of new paths to add for the city.
--   pathsList - The current list of city paths.
--
-- Returns:
--   A new list of paths with the updated paths for the specified city.
updatePaths :: City -> [Path] -> [(City, [Path])] -> [(City, [Path])]
updatePaths city newPaths pathsList =
    map (\(c, p) -> if c == city then (c, newPaths) else (c, p)) pathsList


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
