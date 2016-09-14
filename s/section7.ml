(* CS51 Section 7 - Object-Oriented Programming *)

open Core.Std

(******************************************************************************)
(*          PART 0 : Flatland                                                 *)
(******************************************************************************)
(*
 * Consider the following motivating problem:
 * Suppose we are in a world called Flatland where there are many shapes.
 * In particular, we want:
 *    * all shapes to have some notion of area, so that when
 *      we meet any new shape, we can easily calculate the shape's area.
 *    * to support many different types of shapes.
 *
 * How can we solve this problem?
 *
 * We can solve this problem in two ways:
 * 1) Using algebraic data types
 * 2) Using object-oriented programming
 *
 * We will discuss reasons why the method (1) is not ideal in this case. *)

(* We first define a point, which we will need later *)
type point = float * float   (* x and y coordinate *)

(* Method 1 - Algebraic data types
 * We first define what a shape is: *)

type shape_adt =
  | Square of point * float        (* lower-left corner and side length *)
  | Rect of point * float * float  (* lower-left corner and width and height *)
  | Circle of point * float        (* center and radius *)


(* Now we define the area function that takes a shape and computes its area *)
let area_adt (s: shape_adt) : float =
  match s with
    | Square(_,s) -> s *. s
    | Rect(_,w,h) -> w *. h
    | Circle(_,r) -> 3.14159 *. r *. r

(* Calculate the sum of the area of all shapes in the list *)
let list_area_adt (lst : shape_adt list) : float =
  List.fold_right ~f:(fun x r -> r +. area_adt x) ~init:0. lst

let l1 = [Square((0.,0.),4.); Rect((2.,3.),1.,2.); Circle((5.,5.),2.)] ;;
let _ = list_area_adt l1 ;;



(* Why is this not ideal?
 *
 * Suppose now you are a traveler in Flatland and you meet a new shape, called
 * Triangle. What must change in your code to support this new shape?
 *
 * The type definition of shape needs to change to include Triangle:
 *
 * type shape_adt =
 *   | ...
 *   | Triangle of point * point * point
 *
 * and the area function (or rather, any function that used a match statement on
 * a shape) will need to change to include the Triangle case:
 *
 * let area (s: shape_adt) : float =
 *   match s with
 *     | ...
 *     | Triangle ... -> ...
 *
 * Thus, every time we wish to extend our world of shapes, we break A LOT of
 * code. In real production code, we often don't have access to certain pieces
 * of code (e.g. the type definitions, the area function), so changing code
 * like this to add new functionality might not even be possible.
 *
 * It would be a comparably bad idea to attempt to declare all possible shapes
 * up front.  We would have to define dinosaur shapes because it is a possible
 * shape, even if we never plan on using it.

 * Using algebraic data types gives us a **closed** definition for all possible
 * types in this world; this means that we must know all possible variants at
 * the site of type definition.   *
 *
 * How can we fix this issue? Use Object-Oriented Programming! *)



(******************************************************************************)
(*          PART 1 : Interfaces, Classes, Objects                             *)
(******************************************************************************)


(* We create a **class type (interface)**. Interfaces define a new type and
 * define **methods** for us to interact with this new type.
 *
 * Once we have defined this class type, then we can always create new shapes
 * by defining **classes* that implement this shape interface. This gives us an
 * open world of shapes! We have thrown in a few more methods to keep things
 * fun. *)
class type shape =
object
  (* return the area on this shape *)
  method area : float

  (* return the lower-left corner and the upper-right corner of the
   * bounding box on this shape *)
  method bounding_box : point * point

  (* We translate this shape by the (x,y) in point *)
  method translate : point -> unit

  (* We dilate this shape *)
  method scale : float -> unit
end

(* We want to create a type hierarchy that looks like this:

                        +------------+
                        |            |
                        |  shape (I) |
                        |            |
                        +--^---^---^-+
                           |   |   |
              implements   |   |   | implements
            +--------------+   |   +-------------+
            |                  |                 |
            |       implements |                 |
            |                  |                 |
     +------+------+     +-----+------+   +------+------+
     |             |     |            |   |             |
     |  square (C) |     | rect (C)   |   | circle (C)  |
     |             |     |            |   |             |
     +-------------+     +------------+   +-------------+

 * An 'I' denotes a class type (interface) and a C denotes a concrete
 * implementation. Concrete classes implement an interface class type (denoted
 * by an arrow with implements *)


(* We create a **class** rect that implements the class type (interface) shape.
 *
 * A class is a specification for how to build objects (think of a class like a
 * blueprint, and an object of this class as a specific building built using
 * that blueprint). Classes include:
 *   * definitions of **instance variables** and **methods**
 *        each object (instance) of a class has its own copy of instance
 *        variables
 *   * information on how to construct & initialize objects
 *   * scope information about what to hold private
 *
 * Here, the arguments to rect represent **constructor** arguments--values
 * we need to initialize our object.
 *
 * The colon represents that the rect class has type shape (meaning
 * that rect implements the shape interface).
 *)
class rect (p: point) (w: float) (h: float) : shape =
(* the 'this' in parenthesis is the name of the object that has been
 * instantiated with the class.  For example, in defining the functionality of
 * area for a square, we may access the bounding box of the square by calling
 * this#bounding_box. Notice, however, that we aren't ever using
 * "this" in this class. So, this will compile with a warning. We can
 * instead just say "object", without the (this), to avoid the
 * unnecessary variable.
 *)
object (this)

  (* These are the **instance variables** of objects from this class.
   * Since we have rect : shape, and because pos, width, and height
   * do not show up in the class type interface shape, then pos, width, and
   * height will be **private** to objects in this class (i.e. these variables
   * can only be accessed from inside this class *)
  val mutable pos = p
  val mutable width  = w
  val mutable height = h

  method area : float = width *. height

  method bounding_box : point * point =
    let (x,y) = pos in
    (pos,(x +. width, y +. height))

  (* we destructively update pos to reflect the lower-left corner *)
  method translate ((tx,ty) : point) : unit =
    let (x,y) = pos in
    (* '<-' is update notation for mutable instance variables. *)
    pos <- (x +. tx, y +. ty)

  (* we scale a rectangle from the lower-left corner *)
  method scale (k: float) : unit =
    width <- width *. k;
    height <- height *. k
end

(* Question 1.1: Implement the square class. *)
class square (p: point) (s: float) : shape =
object












end

(* Question 1.2: Implement the circle class. *)
class circle (c: point) (r: float) : shape =
object










end



(******************************************************************************)
(*          PART 2 : Representation, Inheritance                              *)
(******************************************************************************)

(* First, we define the **representation** of a class to be the way the class
 * is implemented. Notice that square and rect have very similar
 * representations--they both take a point to represent the lower-left corner,
 * and they both take side length(s). A square would essentially be a rectangle
 * if we maintain the invariant that width = height.
 *
 * Thus, we can reimplement square to **inherit** from rectangle and thus use
 * rectangle's representation. *)


(* New Type Hierarchy:

                        +------------+
                        |            |
                        |  shape (I) |
                        |            |
                        +--^---^---^-+
                           |   |   |
              implements   |   |   | implements
            +--------------+   |   +-------------+
            |                  |                 |
            |       implements |                 |
            |                  |                 |
     +------+------+     +-----+------+   +------+------+
     |             |     |            |   |             |
     |  square (C) |     | rect (C)   |   | circle (C)  |
     |             |     |            |   |             |
     +-------------+     +-----^------+   +-------------+
                               |
                               | inherits
                               |
                      +--------+-------+
                      |                |
                      | square_rect (C)|
                      |                |
                      +----------------+

*)

class square_rect (p: point) (s: float) : shape =
object

  (* We inherit from the rect class, and we alias the inherited functionality
   * through the name 'super'
   *
   * By inheriting the representation of rect, we can now call super class's
   * methods by doing super#method_name. In fact, if we don't explicitly
   * define a required method, the default method will be the super class's
   * method. *)
  inherit rect p s s as super

  (* Now we can easily write the methods in terms of super.
   *
   * Note that the four lines below are all optional; if we left them out,
   * the default will be to use the super class's methods.
   *
   * It is actually good practice to leave out optional method definitions that
   * simply delegate to the super class, but we are showing them so you can see
   * what the default behavior looks like.
   *
   * We can also define methods differently from their default super class
   * behavior. For example:
   *
   * method area = 42. +. super#area
   *
   * Here, we have **overrided** area's default super implementation to be
   * something else. Whenever we inherit from a class, we are allowed
   * to override any of the methods. *)
  method! area = super#area
  method! bounding_box = super#bounding_box
  method! translate = super#translate
  (* With scale commented out, we aren't "overriding" it. By default,
   * since square doesn't define its own scale method, its superclass's
   * scale method would be called anyway. So we could also comment out
   * the three methods above, and the functionality would be the same
   *)
  (*
  method! scale = super#scale
  *)
end

(* Question 2.1:
 * Create a new square class by inheriting from the square_rect class.
 * Your new square class, when scaled, should scale such that the middle of the
 * square stays in the same place.
 * *For example:
 *
 * square bottom-left = (4,4), side = 3
 *    ---> scale 2 --> square bottom-left = (2.5,2.5), side = 6
 *
 * You should override the scale method to reflect this new property.
 * Hint: First scale like you normally would; then translate appropriately.
 *)
class square_center_scale (p: point) (s: float) : shape =
object












end

(* Extra note:
 * Sometimes we may want to partially write a class, but leave some methods
 * undefined, but force any classes that inherit this class to define
 * said methods. We use the "virtual" keyword to do this. *)



(******************************************************************************)
(*          PART 3 : Subtyping Polymorphism                                   *)
(******************************************************************************)

(* As we wander more around Flatland, we discover that there are more four-sided
 * shapes than we originally thought. We knew about Square and Rectangle, but
 * now we also have Rhombi, Trapezoids, and other four-sided creatures that
 * collectively call themselves Quadrilateral.
 *
 * Since Square and Rect both like to identify themselves as Quadrilaterals,
 * which also identify themselves as Shapes, we need to make Quadrilateral
 * a **subtype** of Shape.
 *
 * Below, we have defined a new class type (interface) called quad. Notice that
 * quad has all of the methods in shape's signature, but adds an additional
 * method:
 *
 *   method sides : float * float * float * float
 *
 * Since quad can do everything that a shape can do (and thus, wherever we
 * expect a shape, we can safely pass a quad), then we consider quad a
 * **subtype** of shape.
 *)
class type quad =
object
  inherit shape

  (* return the length of the four sides *)
  method sides : float * float * float * float
end

(* New Type Hierarchy:


                        +------------+
                        |            |
                        |  Shape (I) |
                        |            |
                        +------^-----+
                               |
                               |  subtypes
                               |
                        +------+-----+
                        |            |
                        | quad  (I)  |
                        |            |
                        +-^-----^--^-+
                          |     |  |
             implements   |     |  |  implements
           +--------------+     |  +------------------------+
           |                    |                           |
           |                    | implements                |
   +-------+--------+      +----+-----------+      +--------+----------+
   |                |      |                |      |                   |
   | square_quad (C)|      |  rect_quad (C) |      | my_quad (C)       |
   |                |      |                |      |                   |
   +----------------+      +----------------+      +-------------------+

*)

(* Question 3.1: Write a rectangle class to implement a quad. *)
class rect_quad (p: point) (w: float) (h: float) : quad =
object


















end

(* Question 3.2: Write a square class to implement a quad *)
class square_quad (p: point) (s: float) : quad =
object (this)










end

(* Question 3.3: Write your own quadrilateral class! Your quadrilateral
 * need not be geometrically possible... *)
class my_quad (p: point) : quad =
object













end

(* So quad is a subtype of shape. So what? Since square_quad and rect_quad
 * are both quads, then they are both shapes as well. Thus, any function
 * expecting a shape can also be passed a quad as well, after an explicit
 * upcast, using the :> operator. *)
let box (s: shape) : point * point =
  s#bounding_box

let (sq : quad) = new square_quad (3.,4.) 5. ;;

(* let b = box sq
 *
 * The above won't work! We need to cast sq from quad to shape
 *)

let b = box (sq :> shape) (* Works! *)

let (_: shape list) =
  [new circle (3.,4.) 5.; (new square_quad (1.,1.) 3. :> shape)]

(* NOTE: We can always upcast a subtype to its super type, but we can
 * NEVER downcast! *)


(******************************************************************************)
(*          PART 4 : Dynamic Dispatch                                         *)
(******************************************************************************)

(* Let's come back to our original problem with shapes: We wanted to be able
 * to define an area function on all shapes, even though we may not know all
 * possible shapes originally.
 *
 * By coding in an object-oriented fashion, this gave us the ability to create
 * an open world of shape types--we can always add a new shape by implementing
 * a new class, and we never have to change old code!
 *
 *
 * To add a new shape, we define a new class that implements shape (and in the
 * process, define the new shape's area method), and we are done! Let's take
 * a look at the new area function *)

(* New area function, using shape's area method *)
let area (s: shape) : float = s#area

(* Find the area of all the shapes in a list. *)
let area_list (lst: shape list) : float =
  List.fold_right ~f:(fun x r -> r +. area x) ~init:0. lst

let (l: shape list) =
  [new circle (3.,4.) 5.; (new square_quad (1.,1.) 3. :> shape)]

let a = area_list l ;;

(* We note that we don't need a match statement like we did using algebraic
 * data types! Instead, we simply call s's area method.
 *
 * This is an example of **dynamic dispatch**: we decide which code to run
 * at RUNTIME (because it is not possible to determine it at COMPILETIME).
 *
 * In the area example, s#area is determined at runtime by the actual class
 * of s (s can be circle, square, rect, etc.). We cannot always determine the
 * class at compile time. For example, if we are passed a list of shapes
 * from the user at runtime, we cannot know which class's area method to call
 * until we are provided this information at runtime.
 *
 * In the area_adt function using algebraic data types, we DO NOT use
 * dynamic dispatch:
 *
 * let area_adt (s: shape_adt) : float =
 *   match s with
 *     | Square(_,s) -> s *. s
 *     | Rect(_,w,h) -> w *. h
 *     | Circle(_,r) -> 3.14159 *. r *. r
 *
 * area_adt always runs the same piece at code at runtime (the match statement),
 * although the exact branch of the match statement that we execute may not be
 * known.
 *
 * It is this property of dynamic dispatch that allows us to create an open
 * world for shapes!
 *)



(******************************************************************************)
(*          PART 5 : Vocabulary Review                                        *)
(******************************************************************************)

(*
Fill out these definitions below:

1. interface (class type)




2. class




3. object




4. representation




5. inheritance




6. subtyping polymorphism




7. parametric polymorphism




8. dynamic dispatch




9. open type vs. closed type




*)
