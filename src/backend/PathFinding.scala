package backend

import backend.physics.PhysicsVector

object PathFinding {

  def findPath(start: GridLocation, end: GridLocation, map: List[List[MapTile]]): List[GridLocation] = {
    println(end)
    if (map(end.y)(end.x).passable) {
      val findPath: WeightedGraph = new WeightedGraph(map)
      findPath.setupVerticesAndEdges()
      findPath.dijkstra(start, end)
    } else {
      List(start)
    }
  }


  def getVelocity(path: List[GridLocation], currentLocation: PhysicsVector): PhysicsVector = {
    val x: Int = Math.floor(currentLocation.x).toInt
    val y: Int = Math.floor(currentLocation.y).toInt
    val locationOnGrid: GridLocation = new GridLocation(x, y)
    if (locationOnGrid.equals(path.last)) {
      val centerOfThis: PhysicsVector = new PhysicsVector(locationOnGrid.x + 0.5, locationOnGrid.y + 0.5)
      if (centerOfThis.distance2d(currentLocation) <= 0.1) {
        new PhysicsVector(0.0, 0.0)
      } else {
        val direction: PhysicsVector = new PhysicsVector(centerOfThis.x - currentLocation.x, centerOfThis.y - currentLocation.y)
        direction.normal2d()
        direction.x *= 5
        direction.y *= 5
        direction
      }
    } else {
      val nextTile: GridLocation = path(path.indexOf(locationOnGrid) + 1)
      val centerOfNext: PhysicsVector = new PhysicsVector(nextTile.x + 0.5, nextTile.y + 0.5)
      val direction: PhysicsVector = new PhysicsVector(centerOfNext.x - currentLocation.x, centerOfNext.y - currentLocation.y)
      direction.normal2d()
      direction.x *= 5
      direction.y *= 5
      direction
    }
  }
}
