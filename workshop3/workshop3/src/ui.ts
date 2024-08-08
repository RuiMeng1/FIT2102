import { fromEvent, interval, zip, Observable } from 'rxjs';
import { tap, scan, map, reduce, mergeMap, zipWith  } from 'rxjs/operators';
import { Card, count$, shoe$, deckSize, Count, IMPLEMENT_THIS, IMPLEMENT_THIS_TYPE } from './exercises'

const button = document.getElementById('hit-me-button')!,
      cardDisplay = document.getElementById('card-display')!,
      countDisplay = document.getElementById('count-display')!,
      shoeSize = 5;

/**
 * Exercise 5: for the given stream of Card,
 * triggered by a button press,
 * emit one card every 500 milliseconds.
 * /Hint/ 
 * there's more than one way to do it, but the following might be useful:
 *  fromEvent - create a stream of events, e.g. fromEvent(button, 'click')
 *  mergeMap - operator to merge a stream of streams into a flat stream
 *  interval - emit a count at specified intervals (in milliseconds)
 *  zipWith - an operator to merge the stream of a with another stream of b, returning pairs [a,b]
 */
const deal : (card$: Observable<Card>) => Observable<Card>
  = IMPLEMENT_THIS

/*
 * Count the cards as they're dealt!
 * Display the deal and the count info
 */
count$(shoeSize,deal(shoe$(shoeSize)))
  .subscribe((count:Count) => {
    const cardColour = '♦♥'.includes(count.card.suit) ? "red" : "black";
    cardDisplay.innerHTML
      = `Card dealt:<br><span id="cardspan" style="color:${cardColour}">${count.card.suit}${count.card.rank}</span>`

    if (count.cardsDealt===shoeSize * deckSize) 
      return; // count is not sensible (NaN) when all cards are dealt

    const countColour = count.trueCount < 0 ? "#93c5df" 
                      : count.trueCount > 0 ? "#f4a581" 
                      : "antiquewhite";
    countDisplay.innerHTML = `True count = <span style="color:${countColour}">${count.trueCount}</span>`
    addBar(count, countColour);
})

/**
 * Adds a bar to the SVG chart for the given Count
 * 
 * Exercise: what are the side effects of this function?
 * Write them here...
 * 
 * @param count 
 * @param colour colour to use
 */
function addBar(count: Count, colour: string) {
  const chart = getSVG(count.cardsDealt === 1)
  , chartBounds = chart.getBoundingClientRect()
  , bar = document.createElementNS(chart.namespaceURI, "rect")
  , height = 1 + Math.abs(count.trueCount) * 2
  , width = chartBounds.width / (shoeSize * deckSize)
  , x = count.cardsDealt * width
  , yMid = chartBounds.height / 2
  , y = count.trueCount < 0 ? yMid
      : count.trueCount > 0 ? yMid - height 
      : yMid - 0.5;
  Object.entries({ 
    x: String(x),
    y: String(y),
    width: String(width),
    height: String(height),
    fill: colour}).forEach(kv=>bar.setAttribute(...kv))
  chart.appendChild(bar);
}

/**
 * get SVG.
 * 
 * Exercise: what are the side effects of this function?
 * Write them here...
 * 
 * @param refresh replace old SVG
 * @returns existing or new SVG depending on refresh
 */
function getSVG(refresh: boolean): SVGSVGElement {
  const existingSVG = document.getElementById('chart'),
    body = document.getElementsByTagName("body")[0];
  if (existingSVG) {
    if(refresh) {
      body.removeChild(existingSVG)
    } else {
      return <unknown>existingSVG as SVGSVGElement
    }
  }
  const svg = document.createElementNS('http://www.w3.org/2000/svg','svg')
  svg.setAttribute('id','chart')
  body.appendChild(svg)
  return svg;
}