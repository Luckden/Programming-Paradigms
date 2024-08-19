
import "./style.css";
export type { Tetromino, Coordinates, Color, State, Key, Event, Block, Direction };


type Tetromino = Readonly<{
    id:string,
    blocks: ReadonlyArray<Block>,
  }>
type Coordinates = {x:number, y:number}
type Color = "green"| "red" | "blue" | "yellow" | "purple" | "orange"

type Block = Readonly<{
    pos: Coordinates,
    color: Color,
    id: string
}>

type Direction = "left" | "right" | "down"; // used to determine the direction of that tetromino is headinng
  
type State = Readonly<{
time: number;
gameEnd: boolean;
score: number;
highScore: number;
floorTetromino: Tetromino,
exit: ReadonlyArray<Block>,
currentTetromino: Tetromino,
nextTetromino: Tetromino,
transform: boolean,
level: number,
}>;

type Key = "KeyS" | "KeyA" | "KeyD" | "KeyR";

type Event = "keydown" | "keyup" | "keypress";