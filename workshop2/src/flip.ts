import { show,IMPLEMENT_THIS, IMPLEMENT_THIS_TYPE } from './show' // WTF are we importing????

type BinaryFunction<T,U,V> = (x:T, y:U) => V
type CurriedFunction<T,U,V> = (x:T) => (y:U) => V

/*
            Explanation:

            power = x => y => Math.pow(x,y);
            power(2) = y => Math.pow(2,y);

            Therefore
            a.map(power(2)) --> a.map(y=>Math.pow(2,y))
            where y is the items in array

            [1,2,3,4,5,6,7,8].map(power(2)) = [2, 4, 8, 16, 32, 64, 128, 256 ]
*/
const curry : <T,U,V>(f:BinaryFunction<T,U,V>) => CurriedFunction<T,U,V> 
            = f=>x=>y=>f(x,y)
const power = curry(Math.pow)
const a = [1,2,3,4,5,6,7,8]
const twoToTheN = a.map(power(2))

show(twoToTheN)('twoToTheN')

// ======= EXERCISE 1 =============================
// implement flip and then use it with power to compute cubes, below

/*
        Reminder: power = x => y => Math.pow(x,y);

        flip takes a function : f = x => y => V
        and returns it with the arguments flipped : f = y => x => V
*/

const flip : <T,U,V>(f:CurriedFunction<T,U,V>) => CurriedFunction<U,T,V>
            = f => x => y => f(y)(x);

const flippedPower = flip(power); // flippedPower = y => x => Math.pow(y,x);

const cubes: number[] = a.map(flippedPower(3)) // [1, 8, 27, 64, 125, 216, 343, 512]
show(cubes)('cubes') 

export { curry, flip, twoToTheN, cubes }