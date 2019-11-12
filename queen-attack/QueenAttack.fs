module QueenAttack

let moveUpperLeft (queen: int * int): int * int = 
    let (qX, qY) = queen
    (qX - 1, qY - 1)

let moveUpperRight (queen: int * int): int * int = 
    let (qX, qY) = queen
    (qX + 1, qY - 1)

let moveLowerLeft (queen: int * int): int * int = 
    let (qX, qY) = queen
    (qX - 1, qY + 1)

let moveLowerRight (queen: int * int): int * int = 
    let (qX, qY) = queen
    (qX + 1, qY + 1)

let notOutsideUpperLeft (queen: int * int): bool =
    let (qX, qY) = queen
    qX > -1 && qY > -1

let notOutsideUpperRight (queen: int * int): bool =
    let (qX, qY) = queen
    qX < 8 && qY > -1

let notOutsideLowerLeft (queen: int * int): bool =
    let (qX, qY) = queen
    qX > -1 && qY < 8

let notOutsideLowerRight (queen: int * int): bool =
    let (qX, qY) = queen
    qX < 8 && qY < 8

let canAttackDiagonal (queen1: int * int) (queen2: int * int) moveFunction notOutsideFunction : bool = 
    let mutable (q1X, q1Y) = moveFunction queen1
    let mutable found = false
    while ((notOutsideFunction(q1X, q1Y))&&not(found)) do
        if (q1X, q1Y) = queen2 then
            found <- true    
        let (tmpX, tmpY) = moveFunction(q1X, q1Y)
        q1X <- tmpX
        q1Y <- tmpY
    found

let canAttackRow (queen1: int * int) (queen2: int * int) : bool =
    let (q1X, _) = queen1
    let (q2X, _) = queen2
    q1X = q2X

let canAttackCol (queen1: int * int) (queen2: int * int) : bool =
    let (_, q1Y) = queen1
    let (_, q2Y) = queen2
    q1Y = q2Y

let create (position: int * int) : bool = 
    let (x, y) = position
    (x > -1 && x < 8) && (y > -1 && y < 8)

let canAttack (queen1: int * int) (queen2: int * int) : bool =
    if queen1 = queen2 then
        false
    else
        let mutable isAttack = canAttackRow queen1 queen2
        isAttack <- isAttack || canAttackCol queen1 queen2
        isAttack <- isAttack || canAttackDiagonal queen1 queen2 moveUpperLeft notOutsideUpperLeft
        isAttack <- isAttack || canAttackDiagonal queen1 queen2 moveUpperRight notOutsideUpperRight
        isAttack <- isAttack || canAttackDiagonal queen1 queen2 moveLowerLeft notOutsideLowerLeft
        isAttack || canAttackDiagonal queen1 queen2 moveLowerRight notOutsideLowerRight