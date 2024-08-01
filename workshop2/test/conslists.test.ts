import { describe, expect, it } from 'vitest'
import { cons, head, ConsList, len, forEach, fromArray, deepEqual, map, concat, concatMap, join, toArray } from '../src/conslists'

describe("from/to Array", function() {
  it("fromArray", function() {
    const l = fromArray([1,2,3,4,5]);
    expect(l).not.toBeNull()
    expect(head(l!)).toEqual(1)
    let sum = 0;
    forEach(x=>sum+=x,l);
    expect(sum).toEqual(15)
  })
  it("toArray", function() {
    const a = toArray(fromArray([1,2,3,4,5]));
    const sum = a.reduce((x,y)=>x+y);
    expect(sum).toEqual(15)
  })
})
describe("Exercise 2: len", function () {
  it("Generated code exists", function () {
    try {
      expect(len).is.a('function')
    } catch (e) {
      expect(false, "Generated code doesn't seem to exist!  Did you run tsc?").to.be.true;
    }
  });
  it("empty list", () => expect(len(null)).to.equal(0))
  it("non-empty list", () => expect(len(fromArray([1, 2, 3]))).to.equal(3));
})
describe("Exercise 3: deepEqual", function () {
  it("Generated code exists", function () {
    try {
      expect(deepEqual).is.a('function')
    } catch (e) {
      expect(false, "Generated code doesn't seem to exist!  Did you run tsc?").to.be.true;
    }
  });
  const l = fromArray([1, 2, 3])
  const l2 = fromArray([1, 2, 3, 4])
  const l3 = fromArray([1, 4, 3])
  it("empty lists", () => expect(deepEqual(null)(null)).to.be.true)
  it("first list empty", () => expect(deepEqual(<ConsList<number>>null)(l)).to.be.false)
  it("second list empty", () => expect(deepEqual(l)(null)).to.be.false)
  it("equal lists", () => expect(deepEqual(l)(l)).to.be.true)
  it("unequal length lists", () => expect(deepEqual(l)(l2)).to.be.false)
})
describe("Exercise 4: map", function () {
  it("Generated code exists", function () {
    try {
      expect(map).is.a('function')
    } catch (e) {
      expect(false, "Generated code doesn't seem to exist!  Did you run tsc?").to.be.true;
    }
  });
  const l = fromArray([1, 2, 3])
  const l2 = fromArray([2, 3, 4])
  const f = x => x + 1
  it("empty list", () => expect(map(f)(null)).to.equal(null))
  it("non-empty list", () => expect(deepEqual(map(f)(l))(l2)).to.be.true);
})
describe("Exercise 5: concat", function () {
  it("Generated code exists", function () {
    try {
      expect(concat).is.a('function')
    } catch (e) {
      expect(false, "Generated code doesn't seem to exist!  Did you run tsc?").to.be.true;
    }
  });
  it("solution correct", function () {
    try {
      const firstPart = cons('T', cons('h', cons('e', cons(' ', cons('a', cons('n', cons('s', cons('w', cons('e', cons('r', cons(' ', cons('t', cons('o', cons(' ', cons('t', cons('h', cons('e', cons(' ', cons('q', cons('u', cons('e', cons('s', null))))))))))))))))))))))
      const secondPart = cons('t', cons('i', cons('o', cons('n', cons(' ', cons('i', cons('s', cons(' ', cons('s', cons('e', cons('v', cons('e', cons('n', cons('t', cons('y', cons(' ', cons('n', cons('i', cons('n', cons('e', null))))))))))))))))))))

      const twoPartsTogether = concat(firstPart)(secondPart);
      const concatSolution = len(twoPartsTogether);
      expect(hash(concatSolution)).to.equal(53284013)
    } catch (e) {
      expect(false, "Incorrect Answer!").to.be.true;
    }
  });
})

/* Simple hash function from https://gist.github.com/iperelivskiy/4110988
   Not meant to be particularly secure, just easier for you to solve the problem than reverse the hash
   */
function hash(v) {
  let a = 1, c = 0, h, o;
  let s = '' + Math.round(v);
  if (s) {
    a = 0;
    for (h = s.length - 1; h >= 0; h--) {
      o = s.charCodeAt(h);
      a = (a << 6 & 268435455) + o + (o << 14);
      c = a & 266338304;
      a = c !== 0 ? a ^ c >> 21 : a;
    }
  }
  return a;
}

describe("Exercise 6: join", function () {
  it("Generated code exists", function () {
    try {
      expect(join).is.a('function')
    } catch (e) {
      expect(false, "Generated code doesn't seem to exist!  Did you run tsc?").to.be.true;
    }
  });
  const l1 = fromArray([1, 2, 3])
  const l2 = fromArray([4, 5])
  const l3 = fromArray([6, 7])
  const nestedlist = fromArray([l1, l2, l3])
  const flattenedlist = fromArray([1, 2, 3, 4, 5, 6, 7])
  it("join of an empty list is an empty list",
    () => expect(join(null)).to.equal(null))
  it("join of a nested list is a flattened list",
    () => expect(deepEqual(join(nestedlist))(flattenedlist)).to.be.true);
})

describe("Exercise 7: what are the indices of a 5x4 spreadsheet?", function () {// the following are the column and row ids for a spreadsheet
  // the cells can be accessed by looking up their ids 
  // which are the column and row ids together: 'A1', 'B3', etc.
  const columns = fromArray(['A', 'B', 'C', 'D', 'E'])
  const rows = fromArray([1, 2, 3, 4])

  /**
   * the list of indices of all cells in the given row
   */
  function indices(rowId: number): ConsList<string> {
    return map((s: string) => s + String(rowId))(columns)
  }
  it("There should be 20 indices", function () {
    expect(len(concatMap(indices)(rows))).to.equal(20)
  });

  it("The indices are A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3,A4,B4,C4,D4,E4", function () {
    // generate all cell indices in the table
    const concatMapSolution = concatMap(indices)(rows)
    expect(String(toArray(concatMapSolution))).to.equal('A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3,A4,B4,C4,D4,E4')
  });
})