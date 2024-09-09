// heavily modified, but originally based on: https://github.com/ChenYuHo/handwriting.js
// license: MIT
class HandwritingCanvas {
  private canvas: HTMLCanvasElement;
  private cxt: CanvasRenderingContext2D;
  private strokeStyle: string;
  private lineWidth: number;
  private drawing: boolean;
  private handwritingX: number[];
  private handwritingY: number[];
  private step: string[];
  private redo_step: string[];
  private redo_trace: number[][][];
  private allowUndo: boolean;
  private allowRedo: boolean;
  private options: { [key: string]: any };
  private trace: number[][][];

  constructor(cvs: HTMLCanvasElement, theme: "dark" | "light") {
    this.canvas = cvs;
    this.cxt = cvs.getContext("2d")!;
    this.strokeStyle = theme === "dark" ? "#fff" : "#1f1f1f";
    this.cxt.lineCap = "round";
    this.cxt.lineJoin = "round";
    this.lineWidth = 3;
    this.drawing = false;
    this.handwritingX = [];
    this.handwritingY = [];
    this.trace = [];
    this.step = [];
    this.redo_step = [];
    this.redo_trace = [];
    this.allowUndo = false;
    this.allowRedo = false;
    this.options = {};
    this.bindEvents();
  }

  private bindEvents() {
    this.canvas.addEventListener("mousedown", this.onPointerDown.bind(this));
    this.canvas.addEventListener("mousemove", this.onPointerMove.bind(this));
    this.canvas.addEventListener("mouseup", this.onPointerUp.bind(this));
    this.canvas.addEventListener("touchstart", this.onTouchStart.bind(this));
    this.canvas.addEventListener("touchmove", this.onTouchMove.bind(this));
    this.canvas.addEventListener("touchend", this.onTouchEnd.bind(this));
  }

  public getTrace() {
    return this.trace;
  }

  public setUndoRedo(undo: boolean, redo: boolean) {
    this.allowUndo = undo;
    this.allowRedo = undo && redo;
    if (!this.allowUndo) {
      this.clearRedoHistory();
    }
  }

  public setLineWidth(lineWidth: number) {
    this.lineWidth = lineWidth;
  }

  public setOptions(options: { [key: string]: any }) {
    this.options = options;
  }

  public erase() {
    this.cxt.clearRect(0, 0, this.canvas.width, this.canvas.height);
    this.clearAllHistory();
  }

  private clearAllHistory() {
    this.step = [];
    this.redo_step = [];
    this.redo_trace = [];
    this.trace = [];
  }

  private clearRedoHistory() {
    this.redo_step = [];
    this.redo_trace = [];
  }

  private onPointerDown(e: MouseEvent) {
    this.startDrawing(e.clientX, e.clientY);
  }

  private onPointerMove(e: MouseEvent) {
    if (this.drawing) {
      this.continueDrawing(e.clientX, e.clientY);
    }
  }

  private onPointerUp() {
    this.stopDrawing();
  }

  private onTouchStart(e: TouchEvent) {
    const touch = e.touches[0];
    this.startDrawing(touch.pageX, touch.pageY);
  }

  private onTouchMove(e: TouchEvent) {
    if (this.drawing) {
      const touch = e.touches[0];
      this.continueDrawing(touch.pageX, touch.pageY);
    }
  }

  private onTouchEnd() {
    this.stopDrawing();
  }

  private startDrawing(x: number, y: number) {
    this.cxt.strokeStyle = this.strokeStyle;
    this.cxt.lineWidth = this.lineWidth;
    this.handwritingX = [];
    this.handwritingY = [];
    this.drawing = true;
    const { offsetX, offsetY } = this.getRelativePosition(x, y);
    this.cxt.beginPath();
    this.cxt.moveTo(offsetX, offsetY);
    this.handwritingX.push(offsetX);
    this.handwritingY.push(offsetY);
  }

  private continueDrawing(x: number, y: number) {
    const { offsetX, offsetY } = this.getRelativePosition(x, y);
    this.handwritingX.push(offsetX);
    this.handwritingY.push(offsetY);
    this.cxt.lineTo(offsetX, offsetY);
    this.cxt.stroke();
  }

  private stopDrawing() {
    if (!this.drawing) return;
    this.trace.push([this.handwritingX, this.handwritingY, []]);
    this.drawing = false;
    if (this.allowUndo) this.step.push(this.canvas.toDataURL());
  }

  private getRelativePosition(
    x: number,
    y: number
  ): { offsetX: number; offsetY: number } {
    const rect = this.canvas.getBoundingClientRect();
    return {
      offsetX: x - rect.left,
      offsetY: y - rect.top,
    };
  }

  public async recognize(
    trace: number[][][],
    options: any,
    callback: (result: string[], err: string) => void
  ) {
    const payload = {
      options: "enable_pre_space",
      requests: [
        {
          writing_guide: {
            writing_area_width: options.width || this.canvas.width,
            writing_area_height: options.height || this.canvas.height,
          },
          ink: trace,
          language: options.language || "zh_TW",
        },
      ],
    };
    try {
      const response = await fetch(
        "https://www.google.com/inputtools/request?ime=handwriting&app=mobilesearch&cs=1&oe=UTF-8",
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        }
      );
      if (!response.ok) {
        throw new Error(`Error ${response.status}: ${response.statusText}`);
      }

      const jsonResponse = await response.json();
      if (jsonResponse.length === 1) {
        callback([], jsonResponse[0]);
      } else {
        let results = jsonResponse[1][0][1];
        if (options.numOfWords) {
          results = results.filter(
            (result: string) => result.length === options.numOfWords
          );
        }
        if (options.numOfReturn) {
          results = results.slice(0, options.numOfReturn);
        }
        callback(results, "");
      }
    } catch (err) {
      callback([], `Recognition error: ${err}`);
    }
  }
}

export default {
  Canvas: HandwritingCanvas,
};
