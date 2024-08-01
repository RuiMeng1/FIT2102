import { show,IMPLEMENT_THIS, IMPLEMENT_THIS_TYPE } from './show'

type BinaryFunction<T,U,V> = (x:T, y:U) => V
type CurriedFunction<T,U,V> = (x:T) => (y:U) => V

const curry : <T,U,V>(f:BinaryFunction<T,U,V>) => CurriedFunction<T,U,V> 
            = f=>x=>y=>f(x,y)
const power = curry(Math.pow)
const a = [1,2,3,4,5,6,7,8]
const twoToTheN = a.map(power(2))

show(twoToTheN)('twoToTheN')
// [2, 4, 8, 16, 32, 64, 128, 256 ]

// ======= EXERCISE 1 =============================
// implement flip and then use it with power to compute cubes, below
const flip : <T,U,V>(f:CurriedFunction<T,U,V>) => CurriedFunction<U,T,V>
           = IMPLEMENT_THIS

const cubes: number[] = IMPLEMENT_THIS /* do something with flip and power and the number 3 here */
show(cubes)('cubes')
// [1, 8, 27, 64, 125, 216, 343, 512]

export { curry, flip, twoToTheN, cubes }