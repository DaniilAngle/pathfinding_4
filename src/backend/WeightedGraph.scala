package backend

import scala.collection.mutable

class WeightedVertex(var target: GridLocation, var weight: Double) {}

class WeightedGraph(map: List[List[MapTile]]) {
  var vertices: List[GridLocation] = List.empty
  var edges: mutable.Map[GridLocation, List[WeightedVertex]] = mutable.Map.empty

  def setupVerticesAndEdges(): Unit = {
    for (rowIdxY <- map.indices) {
      for (columnIdxX <- map.head.indices) {
        addVertex(new GridLocation(columnIdxX, rowIdxY))
      }
    }
    vertices = vertices.reverse

    for (vertex <- vertices) {
      val listOfAdjacent: List[WeightedVertex] = findPossibilities(vertex)
      for (adjV <- listOfAdjacent) {
        addEdge(vertex, adjV.target, adjV.weight)
      }
    }
  }

  def addVertex(value: GridLocation): GridLocation = {
    vertices = value :: vertices
    edges += (value -> List())
    value
  }

  def addEdge(v1: GridLocation, v2: GridLocation, weight: Double): Unit = {
    edges(v1) = new WeightedVertex(v2, weight) :: edges(v1)
  }

  def findPossibilities(src: GridLocation): List[WeightedVertex] = {
    val idxOfLastX: Int = map.head.indexOf(map.head.last)
    val idxOfLastY: Int = map.indexOf(map.last)
    var moveList: List[WeightedVertex] = List.empty
    val idxOfSource: Int = vertices.indexOf(src)

    var leftMove: Boolean = false
    // left (x)
    if (src.x != 0 && map(src.y)(src.x - 1).passable) {
      moveList ::= new WeightedVertex(vertices(idxOfSource - 1), 1)
      leftMove = true
    }
    // right (x)
    var rightMove: Boolean = false
    if (src.x != idxOfLastX && map(src.y)(src.x + 1).passable) {
      moveList ::= new WeightedVertex(vertices(idxOfSource + 1), 1)
      rightMove = true
    }
    // up (y)
    var upMove: Boolean = false
    if (src.y != 0 && map(src.y - 1)(src.x).passable) {
      moveList ::= new WeightedVertex(vertices(idxOfSource - idxOfLastX - 1), 1)
      upMove = true
    }
    // down (y)
    var downMove: Boolean = false
    if (src.y != idxOfLastY && map(src.y + 1)(src.x).passable) {
      moveList ::= new WeightedVertex(vertices(idxOfSource + idxOfLastX + 1), 1)
      downMove = true
    }
    // up and left
    if (upMove && leftMove && map(src.y - 1)(src.x - 1).passable) {
      moveList ::= new WeightedVertex(vertices(idxOfSource - idxOfLastX - 2), Math.sqrt(2))
    }
    // up and right
    if (upMove && rightMove && map(src.y - 1)(src.x + 1).passable) {
      moveList ::= new WeightedVertex(vertices(idxOfSource - idxOfLastX), Math.sqrt(2))
    }
    // down and left
    if (downMove && leftMove && map(src.y + 1)(src.x - 1).passable) {
      moveList ::= new WeightedVertex(vertices(idxOfSource + idxOfLastX), Math.sqrt(2))
    }
    // down and right
    if (downMove && rightMove && map(src.y + 1)(src.x + 1).passable) {
      moveList ::= new WeightedVertex(vertices(idxOfSource + idxOfLastX + 2), Math.sqrt(2))
    }
    moveList
  }

  def dijkstra(position: GridLocation, dst: GridLocation): List[GridLocation] = {
    val src: GridLocation = findInVertices(position)
    val target: GridLocation = findInVertices(dst)
    var toVisit: mutable.Set[GridLocation] = mutable.Set.empty
    var dist: mutable.Map[GridLocation, Double] = mutable.Map.empty
    var prev: mutable.Map[GridLocation, GridLocation] = mutable.Map.empty
    var path: List[GridLocation] = List.empty

    for (v <- vertices) {
      dist += (v -> Double.PositiveInfinity)
      toVisit += v
    }

    dist(src) = 0

    var found: Boolean = false

    while (!found) {
      var minPath: Double = Double.MaxValue
      var u: GridLocation = target
      for (v <- toVisit) {
        val w: Double = dist(v)
        if (minPath > w) {
          minPath = w
          u = v
        }
      }
      if (u == target) {
        found = true
      }
      toVisit.remove(u)
      for (e <- edges(u)) {
        val v = e.target
        if (toVisit.contains(v)) {
          val alt: Double = dist(u) + e.weight
          if (alt < dist(v)) {
            dist(v) = alt
            prev(v) = u
          }
        }
      }
    }

    var vp: GridLocation = target
    path ::= target
    while (!vp.equals(src)) {
      vp = prev(vp)
      path ::= vp
    }
    path
  }

  def findInVertices(vertex: GridLocation): GridLocation = {
    var foundVertex: GridLocation = null
    vertices.foreach(v => {
      if (v.equals(vertex)) {
        foundVertex = v
      }
    })
    foundVertex
  }
}
