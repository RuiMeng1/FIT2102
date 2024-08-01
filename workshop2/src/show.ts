
// display a msg on a given element of the html document if it's available
export function show<T extends Object>(msg:T) {
    return function (id:string){ 
        try {
            document.getElementById(id)!.innerText = String(msg) 
        } catch (e) { 
            if (e instanceof ReferenceError) {
                console.log("Show unavailable")
            } else { console.log(e) }
        }
    }
}

// Stub value to indicate an implementation
export type IMPLEMENT_THIS_TYPE = any;
export const IMPLEMENT_THIS: IMPLEMENT_THIS_TYPE = undefined;