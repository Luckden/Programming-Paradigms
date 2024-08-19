/**
 * Inside this file you will use the classes and functions from rx.js
 * to add visuals to the svg element in index.html, animate them, and make them interactive.
 *
 * Study and complete the tasks in observable exercises first to get ideas.
 *
 * Course Notes showing Asteroids in FRP: https://tgdwyer.github.io/asteroids/
 *
 * You will be marked on your functional programming style
 * as well as the functionality that you implement.
 *
 * Document your code!
 */

import "./style.css";
import { Tetromino, Coordinates, Color, State, Key, Event, Block, Direction } from "./types";
import {fromEvent, interval} from "rxjs";
import { map, filter, scan, mergeWith} from "rxjs/operators";


/** 
 * Constants 
 * */

const Viewport = {
  CANVAS_WIDTH: 200,
  CANVAS_HEIGHT: 400,
  PREVIEW_WIDTH: 160,
  PREVIEW_HEIGHT: 80,
} as const;

const Constants = {
  TICK_RATE_MS: 500,
  GRID_WIDTH: 10,
  GRID_HEIGHT: 20,
} as const;

const BlockConst = {
  WIDTH: (Viewport.CANVAS_WIDTH / Constants.GRID_WIDTH),
  HEIGHT: (Viewport.CANVAS_HEIGHT / Constants.GRID_HEIGHT)
} as const;

const colors:ReadonlyArray<Color> = ["green", "red", "blue", "yellow", "purple", "orange"]


/** User input */
const observeKey = <T>(eventName:Event, k:Key, result:()=>T)=>
    fromEvent<KeyboardEvent>(document,eventName)
      .pipe(
        filter(({code})=>code === k),
        filter(({repeat})=>!repeat),
        map(result))

const
  moveLeft = observeKey('keydown','KeyA',()=>new MoveLeft()), 
  moveRight = observeKey('keydown','KeyD',()=>new MoveRight()),
  moveDown = observeKey('keydown','KeyS',()=>new MoveDown()),
  restart = observeKey('keydown','KeyR',()=>new Restart())




/**
 * Creates initial state
 */

const currentTetromino = createTetromino(1),
nextTetromino = createTetromino(0);

const initialState: State = {
  time: 0,
  gameEnd: false,
  score: 0,
  highScore: 0,
  floorTetromino: {id: "floor", blocks: []},
  exit: [],
  currentTetromino: currentTetromino,
  nextTetromino: nextTetromino,
  transform: false,
  level: 0,
} as const;



/**
 * Create a new tetromino with blocks of same color but a unique id
 * @returns Tetromino
 */

function createTetromino(n:number): Tetromino {
  const tetrominosPositions:ReadonlyArray<ReadonlyArray<Coordinates>> = [
    // Square Tetromino
    [
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: BlockConst.HEIGHT },
      { x: Viewport.CANVAS_WIDTH / 2, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2, y: BlockConst.HEIGHT },
    ],
  
    // L-Shaped Tetromino
    [
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: BlockConst.HEIGHT },
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: 2 * BlockConst.HEIGHT },
      { x: Viewport.CANVAS_WIDTH / 2, y: 2 * BlockConst.HEIGHT },
    ],
  
    // T-Shaped Tetromino
    [
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2 + BlockConst.WIDTH, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2, y: BlockConst.HEIGHT },
    ],
  
    // Z-Shaped Tetromino
    [
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2, y: BlockConst.HEIGHT },
      { x: Viewport.CANVAS_WIDTH / 2 + BlockConst.WIDTH, y: BlockConst.HEIGHT },
    ],
  
    // S-Shaped Tetromino
    [
      { x: Viewport.CANVAS_WIDTH / 2 + BlockConst.WIDTH, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2, y: BlockConst.HEIGHT },
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: BlockConst.HEIGHT },
    ],
  
    // I-Shaped Tetromino
    [
      { x: Viewport.CANVAS_WIDTH / 2 - 2 * BlockConst.WIDTH, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2 + BlockConst.WIDTH, y: 0 },
    ],
    
    // J-Shaped Tetromino
    [
      { x: Viewport.CANVAS_WIDTH / 2, y: 0 },
      { x: Viewport.CANVAS_WIDTH / 2, y: BlockConst.HEIGHT },
      { x: Viewport.CANVAS_WIDTH / 2, y: 2 * BlockConst.HEIGHT },
      { x: Viewport.CANVAS_WIDTH / 2 - BlockConst.WIDTH, y: 2 * BlockConst.HEIGHT },
    ]
    
    // Add positions for other Tetrominos here...
  ];
  const number = (1103515245 * n + 12345) % 0x80000000 //avoid using Math.random
  const color = colors[Math.floor(number)  % colors.length]; // random color, but same for all blocks
  const pos = tetrominosPositions[Math.floor(number) % tetrominosPositions.length];
  const tetrominoID = number.toString()
  return {
    id: tetrominoID,
    blocks: pos.map((p, index) => ({id: tetrominoID + '-' + index.toString(), pos: p, color: color })),
  };
}
/**
 * Checks if a block can enter the next position.
 * @param s State
 * @param b Block to check
 * @returns boolean true if block can enter, false otherwise
 */
const canEnterBlock = (s:State, b: Block):boolean => {
  const blocks = s.floorTetromino.blocks
  return (0<= b.pos.y) && (0 <= b.pos.x) && (b.pos.y < Viewport.CANVAS_HEIGHT) && (b.pos.x < Viewport.CANVAS_WIDTH) 
&& blocks.reduce((acc, cur) => acc && (cur.id === b.id || (cur.pos.x !== b.pos.x || cur.pos.y !== b.pos.y) ), true)
}

/**
 * Checks if a tetromino can enter the next position.
 * @param s State
 * @param t The tetromino to check
 * @returns boolean true if tetromino can enter, false otherwise
 */
const canEnterTetromino = (s:State, t:Tetromino):boolean => 
  t.blocks.reduce((acc, cur) => acc && canEnterBlock(s, cur), true)


/**
 * Returns a block with the next position.
 * @param block the block to move
 * @param direction the direction to move
 * @returns Block with the next position.
 */
const nextBlockPos = (block:Block, direction:Direction):Block => {
  const newPos:Coordinates = direction === "left" 
  ? {x:block.pos.x - BlockConst.WIDTH, y: block.pos.y}
  : direction === "right"
  ? {x:block.pos.x + BlockConst.WIDTH, y: block.pos.y}
  : {x:block.pos.x, y: block.pos.y + BlockConst.HEIGHT };
  
  return {...block, pos: newPos};
}
/**
 * Returns a tetromino with the next position.
 * @param t the tetromino
 * @param direction direction that tetromino is moving
 * @returns tetromino with the next position
 */
const nextTetrominoPos = (t:Tetromino, direction:Direction):Tetromino => {
  return {...t, blocks: t.blocks.map( (block) => (nextBlockPos(block, direction)))}
}

  /**
   * Sub - function for shiftDownAll
   * Row elimination.
   * Uses array functions to go through every row and remove blocks.
   * @param s State
   * @returns State
   */
  function removeFullRows(s:State):State{
    const countForRow = (row:number):number => s.floorTetromino.blocks.reduce((acc, cur) => acc + (cur.pos.y === row? 1:0), 0)
    const countPerRow = Array(Viewport.CANVAS_HEIGHT).fill(0)
    .map((_, rowNum) => countForRow(rowNum))
      
    const remainingRowsIndex = countPerRow.map((val, i) => val !== Constants.GRID_WIDTH? i:null).filter((index) => index !== null) as Array<number>
    const remainingBlocks = s.floorTetromino.blocks.filter(block =>
      remainingRowsIndex.includes(block.pos.y)
    )
    const removeRowsIndex = countPerRow.map((val, i) => val === Constants.GRID_WIDTH? i:null).filter((index) => index !== null) as Array<number>
    const removeBlocks = s.floorTetromino.blocks.filter(block =>
      removeRowsIndex.includes(block.pos.y)
    )

    const newFloorTetromino = {...s.floorTetromino, blocks: remainingBlocks}  // create a new floorTetromino with the remaining rows

    return {...s, exit:[...s.exit, ...removeBlocks], 
      floorTetromino: newFloorTetromino, 
      score: s.score + removeBlocks.length} // remove full rows from view, update score and increases level

  }

  /**
   * Sub - function for tick used to shift every block down by one space.
   * Also determines if the game has ended.
   * @param s 
   * @returns 
   */
  function shiftDownAll (s:State){
    if (!(canEnterTetromino(s, nextTetrominoPos(s.currentTetromino, "down"))) && !canEnterTetromino(s, s.nextTetromino)){ // if game end; can't move down and can't spawn 
      s =  {...s, gameEnd: true, exit: [...s.exit, ...s.currentTetromino.blocks, ...s.nextTetromino.blocks, ...s.floorTetromino.blocks]}
    }
    else if (!(canEnterTetromino(s, nextTetrominoPos(s.currentTetromino, "down")))){ // if current tetromino cannot move down, pull from preview
      s = ({ ...s,
        currentTetromino: s.nextTetromino,
        nextTetromino: createTetromino(s.time),
        floorTetromino: {...s.floorTetromino, blocks: [...s.floorTetromino.blocks, ...s.currentTetromino.blocks]},  // basically forms a larger floor tetromino
        transform: true,
      })
    }
    else{ // move down
      s = ({ ...s,
        currentTetromino: nextTetrominoPos(s.currentTetromino, "down"),
        transform: false
      })
      s = removeFullRows(s)
    }

    s = <State>{...s, tetromino:canEnterTetromino(s, nextTetrominoPos(s.floorTetromino, "down"))? nextTetrominoPos(s.floorTetromino, "down"): s.floorTetromino}
    return s
  }


/**
 * Actions defined for current tetromino movement.
 * Each action is applied at every tick
 * @see nextTetrominoPos used to obtain the next position of the tetromino
 * @see canEnterTetromino used to check the validity of the next position
 */


interface Action {
  apply(s: State): State;
}

class MoveDown implements Action {
  apply(s: State): State {
    const newPosTetromino = nextTetrominoPos(s.currentTetromino, "down")
    return canEnterTetromino(s, newPosTetromino)?
    {...s, currentTetromino: newPosTetromino}
    : s
  }
}
class Restart implements Action { // restart game
  apply(s: State): State { // new tetrominos, keeps, high score
    return s.gameEnd? {...initialState, transform: s.transform, currentTetromino: createTetromino(s.time+1), nextTetromino:createTetromino(s.time) , highScore: s.highScore, gameEnd: false, exit: [...s.exit, ...s.currentTetromino.blocks, ...s.floorTetromino.blocks, ...s.nextTetromino.blocks]}: s
  } // only applies when gameEnd
}

class MoveLeft implements Action {
  apply(s: State): State {
    const newPosTetromino = nextTetrominoPos(s.currentTetromino, "left")
    return canEnterTetromino(s, newPosTetromino)? 
    {...s,  currentTetromino: newPosTetromino}
    :s
    }
  }

class MoveRight implements Action{
  apply(s: State): State {
    const newPosTetromino = nextTetrominoPos(s.currentTetromino, "right")
    return canEnterTetromino(s, newPosTetromino)?
    {...s,  currentTetromino: newPosTetromino}
    :s
    }
  }

/**
 * Action to be applied on each tick.
 */
class Tick implements Action { 
  constructor(public readonly elapsed:number) {} 
  apply(s:State):State {
    s = {...s, time:s.time + this.elapsed};
    s = shiftDownAll(s)  // function to help split code into smaller functions, look above
    if (s.score > s.highScore){
      s = {...s, highScore: s.score} // update high score
    }
    s = {...s, level: Math.floor(s.score/10)} // level increases per row cleared
    s = moveDownPerLevel(s, s.level) // will increase drop speed for every level increased
    return s
  }
}
/**
 * Recursive function used to achieve greater drop speed. Triggered by Tick.
 * @param s 
 * @param level 
 * @returns 
 */
function moveDownPerLevel(s: State, level: number): State {
  if (level === 0) {
    return s;
  }
  const newState = new MoveDown().apply(s); // basically applies move down (acts as a key press), for level times.
  return moveDownPerLevel(newState, level - 1);
}

/** Rendering (side effects) */

/**
 * Displays a SVG element on the canvas. Brings to foreground.
 * @param elem SVG element to display
 */
const show = (elem: SVGGraphicsElement) => {
  elem.setAttribute("visibility", "visible");
  elem.parentNode!.appendChild(elem);
};

/**
 * Hides a SVG element on the canvas.
 * @param elem SVG element to hide
 */
const hide = (elem: SVGGraphicsElement) =>
  elem.setAttribute("visibility", "hidden");

/**
 * Creates an SVG element with the given properties.
 *
 * See https://developer.mozilla.org/en-US/docs/Web/SVG/Element for valid
 * element names and properties.
 *
 * @param namespace Namespace of the SVG element
 * @param name SVGElement name
 * @param props Properties to set on the SVG element
 * @returns SVG element
 */
const createSvgElement = (
  namespace: string | null,
  name: string,
  props: Record<string, string> = {}
) => {
  const elem = document.createElementNS(namespace, name) as SVGElement;
  Object.entries(props).forEach(([k, v]) => elem.setAttribute(k, v));
  return elem;
};

/**
 * This is the function called on page load. Your main game loop
 * should be called here.
 */
export function main() {
  // Canvas elements
  const svg = document.querySelector("#svgCanvas") as SVGGraphicsElement &
    HTMLElement;
  const preview = document.querySelector("#svgPreview") as SVGGraphicsElement &
    HTMLElement;
  const gameover = document.querySelector("#gameOver") as SVGGraphicsElement &
    HTMLElement;
  const container = document.querySelector("#main") as HTMLElement;

  svg.setAttribute("height", `${Viewport.CANVAS_HEIGHT}`);
  svg.setAttribute("width", `${Viewport.CANVAS_WIDTH}`);
  preview.setAttribute("height", `${Viewport.PREVIEW_HEIGHT}`);
  preview.setAttribute("width", `${Viewport.PREVIEW_WIDTH}`);

  // Text fields
  const levelText = document.querySelector("#levelText") as HTMLElement;
  const scoreText = document.querySelector("#scoreText") as HTMLElement;
  const highScoreText = document.querySelector("#highScoreText") as HTMLElement;


/** 
 * Game loop
 * Observable which will run the game at a constant tick rate.
 */
const reduceState = (s: State, action: Tick|Action) => action.apply(s);
interval(Constants.TICK_RATE_MS).pipe(
  map(elapsed=>new Tick(elapsed)),
  mergeWith(moveLeft, moveRight, moveDown, restart),
  scan(reduceState, initialState)).subscribe(updateView)


/**
 * Updates the view based on the current state.
 * This includes adding and removing blocks from the canvas.
 * @param s 
 */
  function updateView(s: State) {
    const createBlockView = (block:Block, namespace: string | null, isPreview: boolean) => {
        const elm = createSvgElement(namespace, "rect", { // create block
          height: `${BlockConst.HEIGHT}`,
          width: `${BlockConst.WIDTH}`,
          x: `${block.pos.x}`,
          y: `${block.pos.y}`,
          id: `${block.id}`,
          
          style: `fill: ${block.color}`,
        });
        isPreview
        ?preview.appendChild(elm)
        :svg.appendChild(elm)
        return elm
      }

    if (s.gameEnd){
      show(gameover)
      preview.querySelectorAll("rect").forEach((block) => preview.removeChild(block))
    }
    else{
      hide(gameover)
      s.exit.forEach( block =>{ // clears the board
        const elm = document.getElementById(block.id)
        if (elm)
        svg.removeChild(elm)|| preview.removeChild(elm)
      }
      )
      s = {...s, exit: []}
    }
    if (s.transform){ // this means the preview needs to be moved to the canvas, hence spawning next tetromino.
      s.currentTetromino.blocks.forEach(block => {preview.removeChild(document.getElementById(block.id)!)})
      s = {...s, transform: false}
    }
      levelText.innerHTML = s.level.toString() // updates display
      scoreText.innerHTML = s.score.toString()
      highScoreText.innerText = s.highScore.toString()
      // update block position for every block
      s.floorTetromino.blocks.forEach(block => {
      const v = document.getElementById(block.id) || createBlockView(block, svg.namespaceURI, false);
        v.setAttribute("x",String(block.pos.x))
        v.setAttribute("y",String(block.pos.y))
      })

      s.currentTetromino.blocks.forEach(block => {
        const v = document.getElementById(block.id) || createBlockView(block, svg.namespaceURI, false);
        v.setAttribute("x",String(block.pos.x))
        v.setAttribute("y",String(block.pos.y))
      })
      s.nextTetromino.blocks.forEach(block => {
        const v = document.getElementById(block.id) || createBlockView(block, preview.namespaceURI, true);
        v.setAttribute("x",String(block.pos.x))
        v.setAttribute("y",String(block.pos.y))
      })

      // remove blocks that are no longer needed, such as previous previews and eliminated blocks
  }
    
}


// The following simply runs your main function on window load.  Make sure to leave it in place.
if (typeof window !== "undefined") {
  window.onload = () => {
    main();
  };
}