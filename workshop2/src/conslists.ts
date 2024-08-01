import { IMPLEMENT_THIS, IMPLEMENT_THIS_TYPE } from './show'

/**
 * A ConsList is either a function created by cons, or empty (null)
 */
type ConsList<T> = Cons<T>|null; 

/** 
 * The return type of the cons function, is itself a function 
 * which can be given a selector function to pull out either the head or rest
 */
type Cons<T> = (selector: Selector<T>) => T|ConsList<T>;

/**
 * a selector will return either the head or rest
 */
type Selector<T> = (head:T, rest:ConsList<T>)=> T|ConsList<T>;

/**
 * cons "constructs" a list node, if no second argument is specified it is the last node in the list
 */
function cons<T>(head:T, rest: ConsList<T>): Cons<T> {
    return (selector: Selector<T>) => selector(head, rest);
}

/**
 * head selector, returns the first element in the list
 * @param list is a Cons (note, not an empty ConsList)
 */
function head<T>(list:Cons<T>):T {
    return <T>list((head, rest) => head);
}

/**
 * rest selector, everything but the head
 * @param list is a Cons (note, not an empty ConsList)
 */
function rest<T>(list:Cons<T>):ConsList<T> {
    return <ConsList<T>>list((head, rest) => rest);
}

function fromArray<T>(a:T[]): ConsList<T> {
    return a.length ? cons(a[0],fromArray(a.slice(1))) : null
}

function toArray<T>(l:ConsList<T>):T[] {
    return l ? [head(l),...toArray(rest(l))]:[]
}

// example of how to recurse over the list:
function forEach<T>(f: (_:T)=>void, list:ConsList<T>): void {
    if (list) {
        f(head(list));
        forEach(f,rest(list));
    }
}

/** ======= EXERCISE 2 =============================
 * return the number of elements in a ConsList
 * @param list 
 */
const len : <T>(list:ConsList<T>) => number 
          = IMPLEMENT_THIS

/** ======= EXERCISE 3 =============================
 * Check if two lists are deeply equal
 * @param list1 First list
 * @param list2 Second list
 */
const deepEqual : <T>(a:ConsList<T>) => (b:ConsList<T>) => boolean
    = IMPLEMENT_THIS

/** ======= EXERCISE 4 =============================
 * map a function over a list
 * @param f the function to map
 * @param l the list
 */
const map : <T,U>(f:(_:T)=>U) => (l:ConsList<T>) => ConsList<U>
    = IMPLEMENT_THIS

/** ======= EXERCISE 5 =============================
 * Put the concatenated contents of two lists into a new list
 * @param list1 First list
 * @param list2 Second list
 */
const concat : <T>(l1:ConsList<T>)=>(l2:ConsList<T>) => ConsList<T> 
    = IMPLEMENT_THIS

/** ======= EXERCISE 6 =============================
 * join a list of lists into a flat list
 * @param list1 First list
 * @param list2 Second list
 */
const join : <T>(l:ConsList<ConsList<T>>) => ConsList<T> 
    = IMPLEMENT_THIS

/** ======= EXERCISE 7 =============================
 * Apply a function that returns a list to elements of the list l, 
 * and return the results concatenated into a new list.
 * @param l a list of T
 * @param f a function that takes a T and returns a list of U
 * @returns a list of U
 */
const concatMap 
  : <T,U>(f: (_:T)=>ConsList<U>) => (l:ConsList<T>) => ConsList<U>
  = IMPLEMENT_THIS

export { cons, ConsList, len, forEach, concatMap, head, rest, fromArray, deepEqual, map, concat, join, toArray }