open System
open System.Windows.Forms
open System.Timers
open System.Drawing

let WindowWidth=720
let WindowHeigth=640
let FieldWidth=10
let FieldHeight=24
let BlockSize=25
let FPS=60

type Point = {x:int ; y:int}
type MinoType = | IMino=0 | OMIno=1 | TMino=2 | JMino=3 | LMino=4 | SMino=5 | ZMino=6
type Mino = {minoType:MinoType ; pos:Point ; rot:int}
type MinoAction = | MoveBottom | MoveRigth | MoveLeft | Rotation | None
type Tetris = {mino:Mino; field:Point List}

let getStanderdMinoPoints (minotype:MinoType) = 
    match minotype with
    | MinoType.IMino -> [{x=0;y=0}; { x=0;y=1}; {x=0;y= -1}; {x=0;y= -2}]
    | MinoType.OMIno -> [{x=0;y=0}; { x=0;y=1}; {x=1;y=1}; {x=1;y=0}]
    | MinoType.TMino -> [{x=0;y=0}; { x= -1;y=0}; {x=1;y=0}; {x=0;y=1}]
    | MinoType.JMino -> [{x=0;y=0}; { x=0;y=1}; {x=0;y= -1}; {x= -1;y= -1}]
    | MinoType.LMino -> [{x=0;y=0}; { x=0;y=1}; {x=0;y= -1}; {x=1;y= -1}]
    | MinoType.SMino -> [{x=0;y=0}; { x= -1;y=0}; {x=0;y=1}; {x=1;y=1}]
    | MinoType.ZMino -> [{x=0;y=0}; { x=1;y=0}; {x=0;y=1}; {x= -1;y=1}]
    | _ -> []

//rotの回数分、(0,0)を中心にpointをπ/2回転
let rec rotationPoint (rot:int) point = 
    let rotation point = {x = -point.y ; y = point.x} 
    match rot%4 with
    | 0 -> point
    | a -> rotationPoint (a-1) (rotation point)

let movePoint (move:Point) point = {x = move.x + point.x ; y = move.y + point.y}

let getMinoPoints (mino:Mino) = 
    List.map (rotationPoint mino.rot) (getStanderdMinoPoints mino.minoType) |> List.map (movePoint mino.pos)

//あるpointがフィールドの内側であり、また、すでに使用されていないかチェック
let isValidPoint (field:Point List) point =
    if point.x < 0 || FieldWidth-1 < point.x then false
    elif point.y < 0 || FieldHeight-1 < point.y then false
    elif List.exists (fun p->p.x=point.x&&p.y=point.y) field then false
    else true

let isValidMino (field:Point List) (mino:Mino) = 
    List.map (isValidPoint field) (getMinoPoints mino) |> List.contains false |> not

let getActionedMino (mino:Mino) (action:MinoAction) = 
    match action with
    | MoveBottom -> {minoType = mino.minoType; pos = {x=mino.pos.x; y=mino.pos.y-1}; rot = mino.rot}
    | MoveLeft -> {minoType = mino.minoType; pos = {x=mino.pos.x-1; y=mino.pos.y}; rot = mino.rot}
    | MoveRigth -> {minoType = mino.minoType; pos = {x=mino.pos.x+1; y=mino.pos.y}; rot = mino.rot}
    | Rotation -> {minoType = mino.minoType; pos = {x=mino.pos.x; y=mino.pos.y}; rot = mino.rot+1}
    | None -> mino
   
//Actionした後のミノが有効なら更新、有効でないならそのまま
let updateMino (tetris:Tetris) (action:MinoAction) =
    let actionedMino = getActionedMino tetris.mino action
    if isValidMino tetris.field actionedMino then actionedMino
    else tetris.mino

let RandomDevice = new System.Random ()
let getNewMino () = { 
    minoType = enum<MinoType>(RandomDevice.Next(7)); 
    pos = {x = FieldWidth/2; y = FieldHeight-2}; 
    rot = 0 }

let drawBlock (graphics:Graphics) (solidBrush:SolidBrush) (p:Point) =
    let x = WindowWidth / 2 - FieldWidth / 2 * BlockSize + p.x * BlockSize
    let y = WindowHeigth / 2+ (FieldHeight / 2 - 1) * BlockSize - p.y * BlockSize
    graphics.FillRectangle(solidBrush, x, y, BlockSize,BlockSize)

//指定された行が消せるなら処理、消せないならそのまま
let checkLine (field:Point List) (line:int) = 
    let num = List.filter (fun p->p.y=line) field |> List.length
    if num >= FieldWidth then 
        let foldFunc point acc =
            if point.y > line then {x = point.x; y = point.y-1}::acc
            elif point.y < line then point::acc
            else acc
        List.foldBack foldFunc field []
    else field

type TetrisForm = class 
    inherit Form

    val mFieldColor : SolidBrush
    val mMinoColor : SolidBrush
    val mLinePen : Pen
    val mutable mFrameCnt : int
    val mTimer : Timer
    val mutable mTetris : Tetris

    //コンストラクタ
    new () as this = 
        {inherit Form(); 
        mFieldColor = new SolidBrush(Color.FromArgb(255,50,50,50)); 
        mMinoColor = new SolidBrush(Color.FromArgb(255,100,100,100));
        mLinePen = new Pen(Color.Black);
        mFrameCnt = 0; 
        mTimer = new Timer(); 
        mTetris = {mino = getNewMino(); field = []} } then
            this.SetStyle(ControlStyles.AllPaintingInWmPaint,true)
            this.Text <- "Tetris"
            this.MinimumSize <- this.Size
            this.ClientSize <- new Size(WindowWidth,WindowHeigth)
            this.DoubleBuffered <- true

    override this.OnPaint (e:PaintEventArgs) =
        let g = e.Graphics
        g.Clear(Color.White)
        //フィールドの描写
        List.map (drawBlock g this.mFieldColor) (this.mTetris.field) |> ignore
        List.map (drawBlock g this.mMinoColor) (getMinoPoints this.mTetris.mino) |> ignore
        //枠の描写
        let x = WindowWidth / 2 - FieldWidth / 2 * BlockSize
        let y = WindowHeigth / 2 - FieldHeight / 2 * BlockSize
        g.DrawRectangle(this.mLinePen, x, y, FieldWidth*BlockSize, FieldHeight*BlockSize)
    
    override this.OnLoad (e:EventArgs) =
        this.mTimer.Elapsed.AddHandler(new ElapsedEventHandler(this.update))
        //updateが一秒あたりに呼び出される回数
        this.mTimer.Interval <- 1.0/(float FPS)
        this.mTimer.Start()
    
    member this.update (sender:Object) (e:ElapsedEventArgs) =
        if this.mFrameCnt >= FPS then
            //１秒ごとの処理
            this.mFrameCnt <- 0
            let movedMino = updateMino this.mTetris MoveBottom

            //ミノの下がフィールドの端またはブロックであり、下に移動できない場合
            if movedMino.pos.y = this.mTetris.mino.pos.y then
                //ミノが置かれた行のリスト、すなわち消せるかどうかを確認する必要のある行が示されているリスト
                let checkLineNums = List.map (fun p->p.y) (getMinoPoints this.mTetris.mino) |> List.distinct
                //一行ずつ消せるかチェック
                let nextField = List.fold checkLine ((getMinoPoints this.mTetris.mino) @ this.mTetris.field) checkLineNums
                let newMino = getNewMino()

                //新しいミノが置けない場合ゲームオーバー、リセット
                if isValidMino nextField newMino then
                    this.mTetris <- {mino=newMino; field=nextField}
                else
                    this.mTetris <- {mino=newMino; field=[]}
            else
                this.mTetris <- {mino=movedMino; field=this.mTetris.field}
        
        this.mFrameCnt <- this.mFrameCnt+1
        //再描写
        this.Invalidate()
       

    override this.OnKeyDown (e:KeyEventArgs) =
        match e.KeyCode with
        | Keys.Space -> this.mTetris <- {mino=updateMino this.mTetris Rotation; field=this.mTetris.field}
        | Keys.Left -> this.mTetris <- {mino=updateMino this.mTetris MoveLeft; field=this.mTetris.field}
        | Keys.Right -> this.mTetris <- {mino=updateMino this.mTetris MoveRigth; field=this.mTetris.field}
        | Keys.Down -> this.mTetris <- {mino=updateMino this.mTetris MoveBottom; field=this.mTetris.field}
        | _ -> ()
end

let tetris = new TetrisForm()
Application.Run(tetris)