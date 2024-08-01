import { show, IMPLEMENT_THIS, IMPLEMENT_THIS_TYPE } from './show'
import { cons, concat, toArray, fromArray, map, forEach, concatMap, len, ConsList, head, rest } from './conslists'

const firstPart = cons('T',cons('h',cons('e',cons(' ',cons('a',cons('n',cons('s',cons('w',cons('e',cons('r',cons(' ',cons('t',cons('o',cons(' ',cons('t',cons('h',cons('e',cons(' ',cons('q',cons('u',cons('e',cons('s', null))))))))))))))))))))))
const secondPart = cons('t',cons('i',cons('o',cons('n',cons(' ',cons('i',cons('s',cons(' ',cons('s',cons('e',cons('v',cons('e',cons('n',cons('t',cons('y',cons(' ',cons('n',cons('i',cons('n',cons('e', null))))))))))))))))))))

const twoPartsTogether = concat(firstPart)(secondPart);

show(toArray(firstPart))('firstPart')
show(toArray(secondPart))('secondPart')
show(toArray(twoPartsTogether))('twoPartsTogether')

const concatSolution = len(twoPartsTogether);

show("The answer is " + concatSolution)("listlength")

// the following are the column and row ids for a spreadsheet
// the cells can be accessed by looking up their ids 
// which are the column and row ids together: 'A1', 'B3', etc.
const columns = fromArray(['A','B','C','D','E'])
const rows = fromArray([1,2,3,4])

/**
 * the list of indices of all cells in the given row
 */
function indices(rowId: number) : ConsList<string> {
    return map((s:string)=>s+String(rowId))(columns)
}

// generate all cell indices in the table
const concatMapSolution = concatMap(indices)(rows)

// place a message in every cell of the table
forEach(show("Hello"), concatMapSolution);

type Suit = string;
type Rank = number;
type Card = Readonly<{
    suit:Suit;
    rank:Rank;
}>

const card : (suit:Suit) => (rank:Rank) => Card
           = suit => rank => ({suit,rank})

const suits : ConsList<Suit> = fromArray(['♠','♣','♢','♡'])
const ranks : ConsList<Rank> = fromArray([1,2,3,4,5,6,7,8,9,10,11,12])

const deck = 
    concatMap(
        (suit:Suit)=>
            map((
                rank:Rank)=>card(suit)(rank)
            )(ranks)
    )(suits)

show(toArray(deck).map(({suit,rank})=>suit+rank).join(','))('carddeck')

/** 
 * Applicative Functor
 * going a bit further... maybe come back and look at this after 
 * we have learned about Functors, Applicative and Monad in Haskell
 */
const apply : <T,U>(fl: ConsList<(_:T)=>U>) => (l:ConsList<T>) => ConsList<U>
            = fl => l => fl ? concat(map(head(fl))(l))(apply(rest(fl))(l)) : null;

// now we can use map to create a list of partially applied card constructors for each rank
const suitCards = map(card)(suits)

// now we apply the partial card constructors to ranks
const deck2 = apply(suitCards)(ranks)

/** Monadic binding */
type MathOperation = (input:number) => ConsList<number>
const mathOperationThatSucceeds : MathOperation
   = x => cons(x,null)
const mathOperationThatCausesADivisionByZeroError : MathOperation
   = x => null

// now we can chain them:
concatMap(mathOperationThatCausesADivisionByZeroError)
  (concatMap(mathOperationThatSucceeds)(cons(1,null)))
