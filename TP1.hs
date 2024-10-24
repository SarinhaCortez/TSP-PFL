import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import qualified Debug.Trace

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = Int
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- roadmap mais eficaz para TSP
type AdjList = [(City, [(City, Distance)])]

-- ajuda para Dynamic Programming
type DPEntry = ((City, Int), (Distance, Path))

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
    in length visited == length (Data.List.nub (cities roads))  

shortestPath :: RoadMap -> City -> City -> Path
shortestPath rmap start end = 
    let allCities = cities rmap
        initialDistances = [(c, if c == start then 0 else maxBound) | c <- allCities]
        initialPrevious = [(c, Nothing) | c <- allCities]
        unvisited = allCities
        (finalDists, finalPrev) = dijkstra rmap unvisited initialDistances initialPrevious
    in reconstructPath finalPrev start end

dijkstra :: RoadMap -> [City] -> [(City, Distance)] -> [(City, Maybe City)] -> ([(City, Distance)], [(City, Maybe City)])
dijkstra rmap [] dists prev = (dists, prev)
dijkstra rmap unvisited dists prev =
    let current = Data.List.minimumBy (\c1 c2 -> compare (lookupDist c1 dists) (lookupDist c2 dists)) unvisited
        currentDist = lookupDist current dists
        neighbours = adjacent rmap current
        (newDists, newPrev) = foldl (updateNeighbour current) (dists, prev) neighbours
    in dijkstra rmap (Data.List.delete current unvisited) newDists newPrev

updateNeighbour :: City -> ([(City, Distance)], [(City, Maybe City)]) -> (City, Distance) -> ([(City, Distance)], [(City, Maybe City)])
updateNeighbour current (dists, prev) (neighbour, edgeDist) =
    let currentDist = lookupDist current dists
        neighbourDist = lookupDist neighbour dists
        newDist = currentDist + edgeDist
    in if newDist < neighbourDist
       then (updateDist neighbour newDist dists, updatePrev neighbour current prev)
       else (dists, prev)

lookupDist :: City -> [(City, Distance)] -> Distance
lookupDist city dists = 
    case lookup city dists of
        Just d -> d
        Nothing -> maxBound

updateDist :: City -> Distance -> [(City, Distance)] -> [(City, Distance)]
updateDist city newDist dists = 
    map (\(c, d) -> if c == city then (c, newDist) else (c, d)) dists

updatePrev :: City -> City -> [(City, Maybe City)] -> [(City, Maybe City)]
updatePrev city prev prevList =
    map (\(c, p) -> if c == city then (c, Just prev) else (c, p)) prevList

reconstructPath :: [(City, Maybe City)] -> City -> City -> Path
reconstructPath prev start end = reverse $ buildPath prev end
    where
        buildPath prev current
            | current == start = [start]
            | otherwise = case lookup current prev of
                            Just (Just prev') -> current : buildPath prev prev'
                            _ -> [current] 


-- TSP USING BITMASKS

-- ver se connected (não necessariamente strongly) para poder fazer TSP
isConnected :: RoadMap -> Bool
isConnected [] = True  
isConnected roadmap =
    let start = if null (cities roadmap) then error "no cities" else head (cities roadmap)
        visited = dfsVisit start roadmap []  
    in length visited == length (cities roadmap)

-- convert roadmap to adjList for added efficiency
roadmapToAdjList :: RoadMap -> AdjList
roadmapToAdjList roadmap =
    foldr (\(c1, c2, dist) adjList -> addEdgeToAdjList c1 c2 dist (addEdgeToAdjList c2 c1 dist adjList)) [] roadmap

addEdgeToAdjList :: City -> City -> Distance -> AdjList -> AdjList
addEdgeToAdjList c1 c2 dist [] = [(c1, [(c2, dist)])]  
addEdgeToAdjList c1 c2 dist ((city, neighbors):rest)
    | city == c1 = (city, (c2, dist):neighbors) : rest  
    | otherwise = (city, neighbors) : addEdgeToAdjList c1 c2 dist rest  

travelSales :: RoadMap -> Path
-- se vazio ret vazio
travelSales [] = []
-- se só uma edge ret duas cidades
travelSales [(x,y,d)] = [x,y]
--else avaliar
travelSales roadmap 
    | not (isConnected roadmap) = []
    | otherwise = 
        let adjList = roadmapToAdjList roadmap
            nCities = length adjList
            startCity = fst (head adjList)
            initialMask = Data.Bits.setBit 0 startCity
            maxMask = (1 `Data.Bits.shiftL` nCities) - 1
            (_, path) = solve startCity initialMask adjList nCities maxMask []
        in if null path then [] else path ++ [startCity] 

solve :: City -> Int -> AdjList -> Int -> Int -> [DPEntry] -> (Distance, Path)
solve curr mask adjList nCities maxMask dp =
    if mask == maxMask 
    then
        let startDist = getDistanceTSP curr 0 adjList
        in if startDist == maxBound 
           then (maxBound, [])
           else (startDist, [curr])
    else
        let possibilities = [(next, dist) | (next, dist) <- getNeighbors curr adjList,
                            not (Data.Bits.testBit mask next)]
            results = map (\(next, dist) -> 
                let (restDist, restPath) = solve next (Data.Bits.setBit mask next) adjList nCities maxMask dp
                in if restDist == maxBound 
                   then (maxBound, [])
                   else (dist + restDist, curr : restPath)) possibilities
        in if null results 
           then (maxBound, [])
           else minimum results

getNeighbors :: City -> AdjList -> [(City, Distance)]
getNeighbors city adjList =
    case [neighbors | (c, neighbors) <- adjList, c == city] of
        (neighbors:_) -> neighbors
        [] -> []

getDistanceTSP :: City -> City -> AdjList -> Distance
getDistanceTSP c1 c2 adjList =
    case [d | (c, neighbors) <- adjList, c == c1, 
             (nextCity, d) <- neighbors, nextCity == c2] of
        (d:_) -> d
        [] -> maxBound

-- END OF TSP USING BITMASKS


tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function


-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [(7,6,1),(8,2,2),(6,5,2),(0,1,4),(2,5,4),(8,6,6),(2,3,7),(7,8,7),(0,7,8),(1,2,8),(3,4,9),(5,4,10),(1,7,11),(3,5,14)]

gTest2 :: RoadMap
gTest2 = [(0,1,10),(0,2,15),(0,3,20),(1,2,35),(1,3,25),(2,3,30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [(0,1,4),(2,3,2)]
