import { IMPLEMENT_THIS, show } from './show'

/*****************************************************************
 * Exercise 5 (Supplementary)
 *   ___  _   _    __    __    __    ____  _  _  ___  ____
 *  / __)( )_( )  /__\  (  )  (  )  ( ___)( \( )/ __)( ___)
 * ( (__  ) _ (  /(__)\  )(__  )(__  )__)  )  (( (_-. )__)
 *  \___)(_) (_)(__)(__)(____)(____)(____)(_)\_)\___/(____)
 *
 */

type jsonTypes =
  | Array<jsonTypes>
  | { [key: string]: jsonTypes }
  | string
  | boolean
  | number
  | null;

/**
 * For this, we have a series of "base cases" which are the primitive or simple
 * json value types - string, boolean, number, null - which can just be
 * encoded as strings.
 *
 * For more complex types like Array and Object, we will recursively call
 * this function to convert their elements into the required format, add
 * indentation, and wrap with brackets.
 */
const prettifyJson = (json: jsonTypes): string => 
  IMPLEMENT_THIS

const json = {
  unit: "FIT2102",
  year: 2021,
  semester: "S2",
  active: true,
  assessments: {
    week1: null,
    week2: "Tutorial 1 Exercise",
    week3: "Tutorial 2 Exercise",
  },
  languages: ["Javascript", "Typescript", "Haskell", "Minizinc"],
};

show(prettifyJson(json))("pretty_json_output");

export { prettifyJson }
