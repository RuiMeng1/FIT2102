/**
 * Lambda calculus
 *
 * Refer to the README and web page for instructions.
 */

/** Exercise 1 */
// Used for:
    // 	1. Extracting data from encapsulated types
    // 	2. Higher order functions can be passed to restore default behaviour
    // 	3. Testing higher order functions more broadly
    //     4. For composition with other combinators
// lambda equivalent:
//    \x.x is the 

/** Exercise 2 */
// 1) a
// 2) b
// 3) b

/** Exercise 3 */
// 1) (/x.x)y => (/x[x:=y].x) => x[x:=y] = y  => BETA normal form
// 2) (/x.xx) => already in simplest form => Divergent
// 3) (/z.zz)(/y.yy) => zz[z:=/y.yy] => (/y.yy)(/y.yy) => alpha equivalent to start => divergent
// 4) xx[x:= y] => yy => BETA normal form

/** Exercise 4 */
// 1) (/y.zy)a => zy[y:=a] => za
// 2) (/x.x)(/x.x) => x[x:=/x.x] => /x.x
// 3) (/x.xy)(/x.xx) => xy[x:=/x.xx] => (/x.xx)y => xx[x:=y] => yy
// 4) (/z.z)(/a.aa)(/z.zb) => (/z.z)aa[a:=/z.zb] => (/z.z)(/z.zb)(/z.zb) =>(/z.z)zb[z:=/z.zb]
//         => (/z.z)(/z.zb)b => (/z.z)zb[z:=b] => (/z.z)bb => bb

/** Exercise 5 */
// 1) /x.zx => z
// 2) /x.xz cannot perform ETA
// 3) (/x.bx)(/y.ay) => (/x.bx)a => ba

/** Exercise 6 */
// 1) Combinator
// 2) not a Combinator
// 3) Combinator
// 4) Combinator

/** Exercise 7 */
// Y = /f.(/x.f(xx))(/x.f(xx))
// Yg = (/f.(/x.f(xx))(/x.f(xx)))g => (/x.g(xx))(/x.g(xx)) => g(xx)[x:= /x.g(xx)] = g((/x.g(xx))(/x.g(xx))) 
//          => g(Yg)

/** Exercise 8 */
// 1) NOT FALSE => (/x. IF x FALSE TRUE) FALSE => IF FALSE FALSE TRUE => FALSE FALSE TRUE
// => (/xy.y) FALSE TRUE => TRUE
// 2) OR TRUE FALSE => (\xy. IF x TRUE y) TRUE FALSE => IF TRUE TRUE FALSE => (/xy.x) TRUE FALSE => TRUE
// 3) AND TRUE TRUE => (\xy. IF x y FALSE) TRUE TRUE => IF TRUE TRUE FALSE => TRUE