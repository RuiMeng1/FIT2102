import { ajax } from "rxjs/ajax";
import { fromEvent, merge, Observable } from "rxjs";
import { map, switchMap } from "rxjs/operators";

function main() {
  // Get DOM elements with explicit typing
  const getButton: HTMLElement = document.getElementById("getButton")!;
  const postButton: HTMLElement = document.getElementById("postButton")!;
  const output: HTMLElement = document.getElementById("output")!;

  type StringDictionary = { [key: string]: string };

  const displayResponse = (response: StringDictionary): void => {
    if (output) {
      output.innerHTML = `<pre>${JSON.stringify(response, null, 2)}</pre>`;
    }
  };

  const btnStreamCreator =
    (button: HTMLElement) =>
    (url: string) =>
    (method: string): Observable<StringDictionary> => {
      return fromEvent(button, "click").pipe(
        switchMap(() =>
          ajax<{ message: string }>({
            url: url,
            method: method,
            body: "",
          }),
        ),
        map((res) => res.response),
      );
    };

  const getButton$ = btnStreamCreator(getButton)("/api")("GET");
  const postButton$ = btnStreamCreator(postButton)("/submit-form")("POST");

  merge(getButton$, postButton$).subscribe({
    next: (response) => displayResponse(response),
    error: (err) => console.error("Error:", err),
  });
}

document.addEventListener("DOMContentLoaded", function (event) {
  try {
    main();
  } catch (e) {
    console.log(e);
  }
});
