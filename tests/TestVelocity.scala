import backend.{GridLocation, PathFinding}
import backend.physics.PhysicsVector
import org.scalatest.funsuite.AnyFunSuite

class TestVelocity extends AnyFunSuite{

  def equalDoubles(x1: Double, x2: Double): Boolean = {
    if (Math.abs(x1 - x2) <= 0.01) {
      true
    } else {
      false
    }
  }

  test("Velocity is correct") {
    val path: List[GridLocation] = List(new GridLocation(1,1), new GridLocation(2,1), new GridLocation(2,2))
    val currentLocation: PhysicsVector = new PhysicsVector(1.5, 1.5)
    val correct: PhysicsVector = new PhysicsVector(2.5 - 1.5, 1.5 - 1.5)
    correct.normal2d()
    correct.x *= 5
    correct.y *= 5
    assert(PathFinding.getVelocity(path, currentLocation).toString == correct.toString)
  }

  test("Returns correct vector when it is the almost the middle of last tile") {
    val path: List[GridLocation] = List(new GridLocation(1,1), new GridLocation(2,1), new GridLocation(2,2))
    val currentLocation: PhysicsVector = new PhysicsVector(2.49, 2.49)
    assert(PathFinding.getVelocity(path, currentLocation).toString == new PhysicsVector(0, 0).toString)
  }

  test("Returns correct vector when it is near the middle of last tile") {
    val path: List[GridLocation] = List(new GridLocation(1,1), new GridLocation(2,1), new GridLocation(2,2))
    val currentLocation: PhysicsVector = new PhysicsVector(2.4, 2.4)
    val correct: PhysicsVector = new PhysicsVector(0.1, 0.1)
    val result: PhysicsVector = PathFinding.getVelocity(path, currentLocation)
    assert(equalDoubles(correct.x, result.x) && equalDoubles(correct.y, result.y))
  }
}
