/**
 * Please have a look at the README file before attempting these exercises.
 *
 * You must make all the tests pass, indicated by a green ✓, and
 * follow all the instructions given in the code file for each exercise.
 *
 * The code must compile with no Typescript warnings or errors.
 *
 * * Must use Observables and Observable operators. Using `addEventListener`
 * * or similar will result in 0 marks unless explicitly required.
 *
 * Marks are only awarded for correct understanding of the question
 * and demonstration of concepts.
 *
 * Completing the tasks with correctly compiling code does not guarantee
 * full marks.
 *
 * Make sure you understand the question and your solution.
 *
 * Ask a tutor if in doubt!
 *
 * **There are hints throughout these exercises encoded in base64.**
 * You can use online tools such as https://www.base64decode.org/
 * to decode them.
 *
 * **Reminders**
 *
 * You must **not** use for-loops, while-loops, or similar imperative
 * techniques in these exercises unless explicitly approved, required,
 * or provided.
 *
 * * All code outside of the `subscribe` callback must be pure and
 * * values immutable.
 *
 * This means declaring variables with `const`, using `Readonly` and
 * `... as const` to ensure immutable types, and avoiding using mutating
 * methods such as `Array.prototype.push()`.
 *
 * * Code inside the `subscribe` may have side effects (e.g. updating DOM), but
 * * **must not** mutate values.
 */

import "./style.css";

import {
  concat,
  fromEvent,
  interval,
  merge,
  timer,
  zip,
  partition,
  Observable,
  of,
} from "rxjs";
import { concatMap, filter, map, scan, takeUntil, delay, timeInterval } from "rxjs/operators";

// Stub value to indicate an implementation
const IMPLEMENT_THIS: any = undefined;
type IMPLEMENT_THIS = any;

type RectProps = Readonly<{
  x: number;
  y: number;
  width: number;
  height: number;
  fill: string;
}>;

const SVG_WIDTH = 600;
const SVG_HEIGHT = 600;

const startProps = {
  x: 100,
  y: 70,
  width: 120,
  height: 80,
  fill: "#95B3D7",
} as RectProps;

const initialiseRect = (props: RectProps, id: string) => {
  // get the svg canvas element
  const svg = document.getElementById(id)!;
  svg.setAttribute("height", String(SVG_HEIGHT));
  svg.setAttribute("width", String(SVG_WIDTH));
  const rect = document.createElementNS(svg.namespaceURI, "rect");
  Object.entries(props).forEach(([key, val]) =>
    rect.setAttribute(key, String(val)),
  );
  svg.appendChild(rect);

  return rect;
};

/*****************************************************************
 * Exercise 1
 *
 * Display the mouse cursor position.
 *
 * Iff the x value is > 400, attach the "highlight" class.
 *
 * Parts of the implementation have been filled in for you.
 *
 * see: https://tgdwyer.github.io/functionalreactiveprogramming/#a-user-interface-example
 */

/**
 * An example of traditional event driven programming style - this is what we are
 * replacing with observable.
 * The following adds a listener for the mouse event handler,
 * sets p and adds or removes a highlight depending on x position
 */
function mousePosEvents() {
  const pos = document.getElementById("pos_event")!;

  document.addEventListener("mousemove", ({ clientX, clientY }) => {
    const p = clientX + ", " + clientY;
    pos.textContent = p;
    if (clientX > 400) {
      pos.classList.add("highlight");
    } else {
      pos.classList.remove("highlight");
    }
  });
}

/**
 * Reimplement mousePosEvents using observables.
 */
function mousePosObservable() {
  // The ! tells Typescript this is a non-null value
  const elem = document.getElementById("pos_obs")!;

  /** Write your code after here */

  // https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
  const source$ = fromEvent<MouseEvent>(document, "mousemove");
  source$
    .pipe(
      map(
        e=>({
          x: e.clientX,
          y: e.clientY
        })
      )
    )// This must be pure
    .subscribe(
      ({x,y}) =>
      {
        elem.textContent = `${x} , ${y}`;
        (x>400) ? elem.classList.add("highlight"): elem.classList.remove("highlight");
      }
    ); // Side effects should be contained here
}

/*****************************************************************
 * Exercise 2
 *
 * Demonstrates the interval method.
 *
 * Animate a rectangle that moves in the x direction and stops.
 *
 * We want to choose an interval so the rectangle animates smoothly
 * and terminates after 1 second (1000 milliseconds).
 *
 * Do NOT use Element.getAttribute as that is using the DOM to store
 * state. Impure accumulation of state will result in 0 marks.
 *
 * Parts of the implementation have been filled in for you.
 *
 * /Hint/: Have a look through the operators we covered in the
 *  readings.
 *
 * see: https://tgdwyer.github.io/functionalreactiveprogramming/#observable-cheatsheet
 */

function animatedRect() {
  const rect = initialiseRect(startProps, "animatedRect");

  /** Write your code after here */

  const source$ = interval(1);

  const move$ = source$
    .pipe(
      takeUntil(timer(1000)),
      scan((currX: number) => currX += 5, 0),
    )
    .subscribe((newX: number) => rect.setAttribute("x", String(newX)));
}

/*****************************************************************
 * Exercise 3
 *
 * Keeping track of multi-value state.
 *
 * Animate the rectangle so that it moves diagonally down-right
 * for 1.41s.
 *
 * Do NOT use Element.getAttribute as that is using the DOM to store
 * state. Impure accumulation of state will result in 0 marks.
 *
 * /Hint/: How did we keep track of the x value in Exercise 2?
 *
 * /Hint 2/: What can we use to store more than 1 value?
 *
 * see: https://tgdwyer.github.io/functionalreactiveprogramming/#observable-streams
 */
function animatedRect2() {
  const rect = initialiseRect(startProps, "animatedRect2");

  /** Write your code after here */

  const moveDownRight$ = interval(10)
    .pipe(
      // Stop taking values after some amount of time
      takeUntil(timer(1410)),

      // Update position of rectangle
      scan(({x,y}) => ({
        x: x+2,
        y: y+2
      }), ({x:0, y:0})),
    )
    .subscribe(({ x, y }: {x: number, y:number}) => {
      rect.setAttribute("x", String(x));
      rect.setAttribute("y", String(y));
    });
}

/*****************************************************************
 * Exercise 4
 *
 * Create and control a rectangle using the keyboard!
 *
 *
 * /Challenge/: Try to make the rectangle move smoothly! This may
 *  require some research and changing the way we implement movement.
 */
function keyboardControl() {
  const rect = initialiseRect(startProps, "moveableRect");

  /** Write your code after here */

  const key$ = fromEvent<KeyboardEvent>(document, "keydown");

  /**
   * Create an observable for a particular keypress.
   *
   * Reference for KeyBoard events https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
   *
   * @param keyCode
   * @param IMPLEMENT_THIS Add as many properties as you want to
   * identify the keypress and the associated change in state.
   * @returns Observable stream that indicates changes in state for
   *  the particular keypress
   */
  const fromKey = (keyCode: string, {x, y}: {x:number, y:number}) =>
    key$.pipe(
      filter(({ code }) => code === keyCode),
      map(() => ({x: x ,y: y})))
    ;

  /**
   * /Hint/: An object like { axis: 'x' | 'y', amount: int } can be used to represent a particular keypress.
   * E.g. Pressing KeyA might produce { axis: 'x', amount: -10 }
   */

  /** Decrease x */
  const left$ = fromKey("KeyA", {x: -10, y: 0});
  /** Decrease y */
  const up$ = fromKey("KeyW", {x: 0, y: -10});
  /** Increase x */
  const right$ = fromKey("KeyD", {x: 10, y: 0});
  /** Increase y */
  const down$ = fromKey("KeyS", {x: 0, y: 10});
  /**
   * /Hint/: What operator can we use to merge observables?
   *         Have a look through the operators we covered in the
   *         readings.
   *
   * /Hint 2/: This should make use of the scan function
   */

  merge(left$, down$, up$, right$)
    .pipe(
      scan(({x,y}, {x: dx, y: dy}) => ({
        x: x+dx,
        y: y+dy
      }))
  )
    .subscribe(({ x, y }: {x:number, y:number}) => {
      rect.setAttribute("x", String(x));
      rect.setAttribute("y", String(y));
    });
}

/*****************************************************************
 * Exercise 5
 *
 * Add some code which looks at the data.csv file
 * This code shall console.log a string (second column)
 * after a specified delay (first column)
 *
 */
function printWithDelay() {
  // This fetches the csv from your computer and converts it to a string
  fetch("http://localhost:5173/src/data.csv")
    .then((response) => response.text())
    .then((text) => process(text));

  const process = (text: string) => {
    const lines = text.split("\n");
    lines.forEach((line) =>
      { const [delay, word] = line.split(",");
        const timer$ = timer(Number(delay));
        timer$.subscribe(() => console.log(word));
      }
    );
  };
}
/**
 * Do Not Modify
 */

document.addEventListener("DOMContentLoaded", function (event) {
  mousePosEvents();
  mousePosObservable();

  animatedRect();
  animatedRect2();

  keyboardControl();
  printWithDelay();
});
