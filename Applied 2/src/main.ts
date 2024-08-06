/**
 * IMPORTANT: The README file contains important information on
 * getting started.
 * Please read before attempting these exercises.
 *
 * You must make all the tests pass, indicated by a green ✓, and
 * follow all the instructions given in the code file for each exercise.
 *
 * The code must compile with no Typescript warnings or errors.
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
 * All code must also be pure and values immutable.
 *
 * This means declaring variables with `const`, using `Readonly` and
 * `... as const` to ensure immutable types, and avoiding using mutating
 * methods such as `Array.prototype.push()`.
 */

// Stub value to indicate an implementation
const IMPLEMENT_THIS: any = undefined;
type IMPLEMENT_THIS = any;

const outputText = (targetDiv: string, text: string) =>
  (document.getElementById(targetDiv)!.innerHTML =
    "<pre>Output:\n\n" + (text || "Not Implemented") + "</pre>");

/*****************************************************************
 * Exercise 1
 *
 * Create an object called anObject with the property "x" set to 5.
 *
 * Create another object object called anotherObject that
 *  has the properties of anObject, and
 *  has the property "y" set to 10,
 * using the spread operator.
 *
 * Make sure to NOT mutate anything, including objects.
 *
 * see: https://tgdwyer.github.io/typescript1/#using-the-compiler-to-ensure-immutability
 */

const anObject = {
  x: 5
}
const anotherObject = {
  ...anObject,
  y: 10
}
/*****************************************************************
 * Exercise 2
 *
 * Part 1:
 * Write the types for the following code, which was introduced in https://tgdwyer.github.io/html/
 * - hint: look at the type of the object being passed in to animate in the call below.
 * - note: it's not just adding the type definitions, you'll also need to add code to handle nulls and convert types as necessary.

 * Part 2:
 * Add a second Blue rectangle, and animate it to move in the opposite direction for 10s.
 * - hint: start by declaratively defining the blue rectangle in the index.html file, similarly to the existing red one.
 */

// Define an animation function
function animate(
  rect: HTMLElement,

  startX: number,
  finalX: number,
  duration: number,
) {
  const startTime = performance.now();
  function nextFrame() {
    // Calculate elapsed time
    const currentTime = performance.now(),
      elapsedTime = currentTime - startTime;

    // Check if animation duration has elapsed
    if (elapsedTime >= duration) {
      // Set the final position of the rectangle.
      // We can use `setAttribute` to modify the HTML Element. In this case, we are changing the x attribute.
      rect.setAttribute("x", String(finalX));
      return; // Stop the animation
    }

    // Calculate position based on elapsed time
    const x = startX + ((finalX - startX) * elapsedTime) / duration;

    // Set the intermediate position of the rectangle.
    rect.setAttribute("x", String(x));

    // Call the nextFrame function again after a delay of 1000/60 milliseconds
    setTimeout(nextFrame, 1000 / 60); // 60 FPS
  }
  nextFrame();
}

const rectangle = document.getElementById("redRectangle")!;
const duration = 5000; // 5 seconds in milliseconds
animate(rectangle, 0, 370, duration);
const blueRectangle = document.getElementById("blueRectangle")!;
const durationBlueRec = 10000; // 10 seconds
animate(blueRectangle,370,0,duration);
/*****************************************************************
 * Exercise 3a
 *
 * Define the type of the "todo" object using TodoItem and Todo.
 *
 * The types have been initialised for you, identify the correct
 *  correct fields and types for those fields. Use the "todo"
 *  object as reference!
 *
 * Remember to use Readonly and ReadonlyArray!
 *
 * see: https://tgdwyer.github.io/typescript1/#type-annotations-cheat-sheet
 */

/**
 * List of todo items with some additional metadata
 *
 * /Hint/: Have a look at the object provided below, what is it's type?
 *
 * /Hint 2/: The `content` field is an *array* of something, what could it be?
 */
type Todo = Readonly<{
  timestamp: number,
  from: string,
  verySecretVerySecureCodeThatDefinitelyDoesNotContainAnythingUseful: string,
  content: TodoItem[]
}>;

/**
 * Item in a todo list
 */
type TodoItem = Readonly<{
  userId: number,
  id: number,
  title: string,
  completed: boolean
}>;

const todo: Todo = {
  timestamp: 65535,
  from: "https://jsonplaceholder.typicode.com/todos",
  // Base64 encoding of something ... I wonder what ((⇀‸↼))
  verySecretVerySecureCodeThatDefinitelyDoesNotContainAnythingUseful:
    "bmV2ZXIgZ29ubmEgZ2l2ZSB5b3UgdXAKbmV2ZXIgZ29ubmEgbGV0IHlvdSBkb3duCm5ldmVyIGdvbm5hIHJ1biBhcm91bmQKYW5kIGRlc2VydCB5b3UKCm5ldmVyIGdvbm5hIG1ha2UgeW91IGNyeQpuZXZlciBnb25uYSBzYXkgZ29vZGJ5ZQpuZXZlciBnb25uYSB0ZWxsIGEgbGllCmFuZCBodXJ0IHlvdQ==",
  content: [
    {
      userId: 1,
      id: 1,
      title: "delectus aut autem",
      completed: false,
    },
    {
      userId: 1,
      id: 2,
      title: "quis ut nam facilis et officia qui",
      completed: false,
    },
    {
      userId: 1,
      id: 3,
      title: "fugiat veniam minus",
      completed: false,
    },
  ],
};

/*****************************************************************
 * Exercise 3b
 *
 * Implement the following functions to pretty print this mock
 *  response of todo data.
 *
 * Note that the field
 * `verySecretVerySecureCodeThatDefinitelyDoesNotContainAnythingUseful`
 * is not included in the pretty printing.
 *
 * ! Must use higher order functions !
 * ! Do NOT hardcode the number of items !
 *
 * Expected format:
 *
 * timestamp: 65535
 * from: https://jsonplaceholder.typicode.com/todos
 * content:
 * userId: 1 | id: 1 | title: delectus aut autem | completed: false
 * userId: 1 | id: 2 | title: quis ut nam facilis et officia qui | completed: false
 * userId: 1 | id: 3 | title: fugiat veniam minus | completed: false
 *
 * see: https://tgdwyer.github.io/javascript1/#array-cheatsheet
 */

/**
 *
 * /Hint/: Useful functions T2JqZWN0LmVudHJpZXMKQXJyYXkucHJvdG90eXBlLmpvaW4=
 *
 * @param item Todo item
 * @returns Prettified todo item
 */
const prettifyTodoItem = (item: TodoItem): string => 
  `userId: ${item.userId} | id: ${item.id} | title: ${item.title} | completed: ${item.completed}`;


/**
 * 
 * @param content Collection of todo items
 * @returns Prettified content
 */
const prettifyContent = (content: ReadonlyArray<TodoItem>): string =>
  // content.reduce((str, item) => str + prettifyTodoItem(item) + '\n', '');
  content.map(prettifyTodoItem).join("\n")

/**
 * 
 * @param todo Todo object
 * @returns prettified representation of todo data
 *
 * /Hint/: String templates can make this look a lot nicer
 *  `some string ${variable_expression} some other string`
 *
 * /Hint 2/: Object destructuring can be used in the arguments
 *  e.g. ({ someKey }) => someKey
 */
const prettifyTodo = (todo: Todo): string =>`timestamp: ${todo.timestamp} \nfrom: ${todo.from}\ncontent:\n` + prettifyContent(todo.content);

outputText("pretty_object_output", prettifyTodo(todo).toString());

/*****************************************************************
 * Exercise 4a
 *
 * We will now look at defining recursive types with generics. This is
 * very useful in creating our own custom data types and data structures.
 *
 * see: https://tgdwyer.github.io/typescript1/#generic-types
 *
 * The binary tree is a very basic recursive data structure which stores
 * a data value, and two children.
 *
 * Define the BinaryTree type and binaryTree constructor.
 */

type BinaryTree<T> = Readonly<{
  data: T;
  left?: BinaryTree<T>;
  right?: BinaryTree<T>;
}>;

/**
 * Creates a binary tree node
 *
 * /Hint/: Remember to declare the generic type!
 *
 * /Hint 2/: The ? is used to indicate an optional parameter
 *  e.g. (x: int, y?: int) => {}
 *
 * /Hint 3/: You may need to cast the object
 *
 * @param data Data to store at node
 * @param left Left child
 * @param right Right child
 * @returns Binary tree node
 */
const binaryTree = <T> (
  data: T,
  left?: BinaryTree<T>,
  right?: BinaryTree<T>,
): BinaryTree<T> => ({
  data,
  left,
  right
});

const binaryTreeExample = binaryTree(
  1,
  binaryTree(2, binaryTree(3)),
  binaryTree(4, binaryTree(5), binaryTree(6)),
);

/*****************************************************************
 * Exercise 4b
 *
 * Implement the following functions to pretty print a binary tree.
 *
 * What this means is that the value of each node is printed, and
 * the children are indented by a certain amount. Note that the tree
 * can be of any depth and may have missing children.
 *
 * /Hint/: Recursive data structures are very conducive to recursion!
 *
 * Expected Format:
 *
 * 1
 * |- 2
 * |- |- 3
 * |- 4
 * |- |- 5
 * |- |- 6
 *
 *
 * /Note/: You can add parameters to functions if you wish, as long
 *  as it doesn't affect the current usage (i.e. tests). This can be
 *  done by adding an optional/default parameter.
 *  E.g.
 *    function prettifyBinaryTree<T>(node: BinaryTree<T>): string
 *    function prettifyBinaryTree<T>(node: BinaryTree<T>, anotherParameter = ""): string
 */

/**
 * Pretty print a binary tree
 *
 * @param node Root node of binary tree
 * @returns Prettified binary tree
 */
function prettifyBinaryTree<T>(node: BinaryTree<T>, depth = 0): string {
  const current = "|- ".repeat(depth) + String(node.data);

  const left = node.left? prettifyBinaryTree(node.left, depth + 1): "";
  const right = node.right? prettifyBinaryTree(node.right, depth + 1): "";

  return `${current}\n${left}${right}`;
}

const prettyBinaryTree = prettifyBinaryTree(binaryTreeExample);
outputText("pretty_btree_output", prettyBinaryTree);

/*****************************************************************
 * Exercise 5a
 *
 * Implement prettyPrintNaryTree, which takes a NaryTree as input
 * and returns a list of the type expected by your nest function
 */

type NaryTree<T> = Readonly<{
  data: T;
  children: NaryTree<T>[];
}>;

const naryTree = <T>(data: T, children: NaryTree<T>[] = []) =>
  ({data, children});

const naryTreeExample = naryTree(1, [
  naryTree(2),
  naryTree(3, [naryTree(41), naryTree(42), naryTree(43)]),
  naryTree(5, [naryTree(6)]),
]);

/*****************************************************************
 * Exercise 5b
 *
 * Implement the following functions to pretty print an nary tree.
 *
 * /Hint/: What's the difference between 3b and 4b?
 *
 * Expected format:
 *
 * 1
 * |- 2
 * |- 3
 * |- |- 41
 * |- |- 42
 * |- |- 43
 * |- 5
 * |- |- 6
 *
 *
 * /Note/: You can add parameters to functions if you wish, as long
 *  as it doesn't affect the current usage (i.e. tests). This can be
 *  done by adding an optional/default parameter.
 *  E.g.
 *    function prettifyNaryTree<T>(node: NaryTree<T>): string
 *    function prettifyNaryTree<T>(node: NaryTree<T>, anotherParameter = ""): string
 */

/**
 * Prettify an Nary tree
 *
 * @param node Root of Nary tree
 * @returns Prettified Nary tree
 */
function prettifyNaryTree<T>(node: NaryTree<T>, depth = 0): string {
  const current = "|- ".repeat(depth) + String(node.data);

  const children = node.children.map((item) => item ? prettifyNaryTree(item, depth + 1): "");

  return [current, ...children].filter(Boolean).join("\n");
}

const prettyNaryTree = prettifyNaryTree(naryTreeExample);
outputText("pretty_ntree_output", prettyNaryTree);

/*****************************************************************
 * Exercise 6 (Supplementary)
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
 * encoded as strings and converted to the List line format.
 *
 * For more complex types like Array and Object, we will recursively call
 * this function to convert their elements into the required format, add
 * indentation, and wrap with brackets.
 */
const prettifyJson = (json: jsonTypes, depth=1): string => { //depth keeps track of how many spaces for formatting
  const keyIndent = '  '.repeat(depth) //for formatting
  const bracketIndent = '  '.repeat(depth-1) //for formatting
  const out = json == null || ['string', 'boolean', 'number'].includes(typeof(json)) ? String(json) // covers all simple bases cases
  : Array.isArray(json) ? `[${json.reduce((acc, el) => acc + ',\n' + keyIndent + prettifyJson(el, depth+1))}\n${bracketIndent}]`  //for array case
  : typeof(json) == 'object' ? `{\n${Object.entries(json).map(([key, val]) => `${keyIndent}${key}: ${prettifyJson(val, depth+1)}`).join(',\n')}\n${bracketIndent}}` //for objects
  : `${json} is not a jsonType.`
  return out;
}

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

outputText("pretty_json_output", prettifyJson(json));
