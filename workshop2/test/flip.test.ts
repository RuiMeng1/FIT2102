import { describe, expect, it } from 'vitest'
import { curry,flip,twoToTheN,cubes } from '../src/flip'

describe('Exercise 1', () => {
  it("Generated code exists", function() {
    try {
      expect(curry).is.a('function')
    } catch(e) {
      expect(false, "Generated code doesn't seem to exist!  Did you run tsc?").to.be.true;
    }
  });
  it("Given code works as expected", ()=>{
    expect(twoToTheN).to.deep.equal([2, 4, 8, 16, 32, 64, 128, 256 ])
  })
  it("flip works as expected", function() {
    const f = x=>y=>x/y;
    expect(flip(f)(2)(6)).to.equal(3)
  });
  it("map numbers in an array to an array of their cubes", function() {
    const expectedCubes = [1, 8, 27, 64, 125, 216, 343, 512]
    expect(cubes).to.deep.equal(expectedCubes)
  });
})
