package com.evolutiongaming.bootcamp.homework._2

object _2 {
  sealed trait Shape extends Located with Bounded with Movable {
    def area: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable {
    type Self <: Movable
    def move(dx: Double, dy: Double): Self
  }

  sealed trait Movable3D {
    type Self <: Movable3D
    def move(dx: Double, dy: Double, dz: Double): Self
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override type Self = Point
    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)

    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def area: Double = 0
  }

  final case class Circle(center: Point, radius: Double) extends Shape {
    override type Self = Circle
    override def move(dx: Double, dy: Double): Circle = Circle(center.move(dx, dy), radius)

    override def x: Double = center.x
    override def y: Double = center.y

    override def minX: Double = center.x - radius
    override def maxX: Double = center.x + radius
    override def minY: Double = center.y - radius
    override def maxY: Double = center.y + radius
    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Triangle(first: Point, second: Point, third: Point) extends Shape {
    override type Self = Triangle
    override def move(dx: Double, dy: Double): Triangle = Triangle(first.move(dx, dy), second.move(dx, dy), third.move(dx, dy))

    override def minX: Double = List(first.x, second.x, third.x).min
    override def maxX: Double = List(first.x, second.x, third.x).max
    override def minY: Double = List(first.y, second.y, third.y).min
    override def maxY: Double = List(first.y, second.y, third.y).max

    override def x: Double = (first.x + second.x + third.x) / 3
    override def y: Double = (first.y + second.y + third.y) / 3
    override def area: Double =
      Math.abs(
        (first.x * (second.y - third.y) +
            second.x * (third.y - first.y) +
            third.x * (first.y - second.y)) / 2
      )
  }

  final case class Rectangle(leftBottom: Point, rightTop: Point) extends Shape {
    override type Self = Rectangle
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(leftBottom.move(dx, dy), rightTop.move(dx, dy))

    override def minX: Double = leftBottom.x
    override def maxX: Double = rightTop.x
    override def minY: Double = leftBottom.y
    override def maxY: Double = rightTop.y

    override def x: Double = (rightTop.x - leftBottom.x) / 2
    override def y: Double = (rightTop.y - leftBottom.y) / 2

    override def area: Double = (rightTop.x - leftBottom.x) * (rightTop.y - leftBottom.y)
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override type Self = Point3D
    override def surfaceArea: Double = 0

    override def volume: Double = 0

    override def minZ: Double = z
    override def maxZ: Double = z
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Cube(leftBottom: Point3D, length: Double) extends Shape3D {
    override type Self = Cube
    override def surfaceArea: Double = 6 * Math.pow(length, 2)

    override def volume: Double = Math.pow(length, 3)

    override def z: Double = leftBottom.z + length / 2
    override def x: Double = leftBottom.x + length / 2
    override def y: Double = leftBottom.y + length / 2

    override def minZ: Double = leftBottom.z
    override def maxZ: Double = minZ + length
    override def minX: Double = leftBottom.x
    override def maxX: Double = minX + length
    override def minY: Double = leftBottom.y
    override def maxY: Double = minY + length

    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(leftBottom.move(dx, dy, dz), length)
  }

  final case class Sphere(center: Point3D, radius: Double) extends Shape3D {
    override type Self = Sphere
    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)

    override def volume: Double = 4 / 3 * Math.PI * Math.pow(radius, 3)

    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z

    override def minZ: Double = center.z - radius
    override def maxZ: Double = center.z + radius
    override def minX: Double = center.x - radius
    override def maxX: Double = center.x + radius
    override def minY: Double = center.y - radius
    override def maxY: Double = center.y + radius

    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(center.move(dx, dy, dz), radius)
  }
}
