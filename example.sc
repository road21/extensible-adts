import adts.*
import ADT.~

type circle = "radius" ~ Double
type square = "length" ~ Double
type rectangle = "width" ~ Double & "height" ~ Double
type triangle = "_1" ~ Double & "_2" ~ Double & "_3" ~ Double
type shape = ("typ" ~ "circle" & circle) | ("typ" ~ "square" & square) |
  ("typ" ~ "rectangle" & rectangle) | ("typ" ~ "triangle" & triangle)

def area(shape: ADT[shape]): Double =
  shape.typ: "circle" | "square" | "rectangle" | "triangle" // type of common fields derives via union type
  // shape.radius is not compiling
  shape match
    case Like[circle](c)    => c.radius * c.radius * math.Pi
    case Like[square](s)    => s.length * s.length
    case Like[rectangle](r) => r.width * r.height
    case Like[triangle](t) =>
      val p = (t._1 + t._2 + t._3) / 2
      math.sqrt((p - t._1) * (p - t._2) * (p - t._3))
    // exhaustiveness can be checked by macro, we need to check that (circle | square | rectangle | triangle) is upper type of shape

println(area("typ" ~ "circle" & "radius" ~ 1.0))
// area("radius" ~ 1.0) // is not compiling