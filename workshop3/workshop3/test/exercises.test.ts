import { describe, expect, it } from 'vitest';
import { toArray, reduce, from, zip, lastValueFrom, range } from 'rxjs';
import { deck$, deckSize, shoe$, count$, randomInsert } from '../src/exercises';
import { testShoeCount } from './practiceDeck';

describe('Exercise 1', () => {
  const count = reduce(n=>n+1,0)
  it("Deck has 52 cards", async function() {
    const r = await lastValueFrom(deck$.pipe(count))
    expect(r).toBe(deckSize)
  });

  it("randomInsert works as expected", async function() {
    const a = await lastValueFrom(range(1000).pipe(toArray()))
    const r = a.reduce(randomInsert<number>,[])
    expect(r).to.not.deep.equal(a)
    expect(r.slice().sort((a,b)=>a-b)).to.deep.equal(a)
  })
  
  it("Shoe has deckCount*52 cards", async function() {
    const deckCount = 6;
    const r = await lastValueFrom(shoe$(deckCount).pipe(count))
    expect(r).toBe(deckCount*deckSize)
  });

  it("Count works as expected", async function() {
    const r
     = await lastValueFrom(
         zip(count$(Math.floor(testShoeCount.length/52)
                   ,from(testShoeCount.map(c=>c.card)))
            , from(testShoeCount.map(c=>c.trueCount)))
         .pipe(toArray()))
    
    r.forEach(([r,e])=>expect(r.trueCount).toBe(e))
  }) 
})
