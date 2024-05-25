// Lab 1
// Solving simple computational problems in functional style 

// Task 1

open System

let num_points = 10.
let EPS = 1e-14

let isLessThanOrEqualToEpsilon x = float x <= EPS

let rec factorial x =
    if x < 1 then
        1
    else
        x * factorial (x - 1)

let rec iterateWhile f isLessThanOrEqualToEpsilon (tuple: int * float) sum step iterations =
    if f tuple |> isLessThanOrEqualToEpsilon then
        (sum, iterations)
    else
        iterateWhile f isLessThanOrEqualToEpsilon ((step + (fst tuple)), (f tuple)) (sum + (f tuple)) step (iterations + 1)

let dumbTaylorSeries x =
    let nthMember (n, _) = (Math.Pow(2. * x, float n)) / float (factorial n)
    iterateWhile nthMember isLessThanOrEqualToEpsilon (1, 0) 0. 1 1

let smartTaylorSeries x =
    let nthMember (n, prev) = 
        if n = 0 then
            1.
        else
            prev * ((2. * x) / (float (n)))
    iterateWhile nthMember isLessThanOrEqualToEpsilon (0, 0) 0. 1 1

let taylorTable builtinFunc dumbTaylorFunc smartTaylorFunc (points: float) (a: float) (b: float) =
    printfn "--------------------------------------------------------------------------------------"
    printfn "|   x   |  Builtin  | Taylor series, smart | # terms | Taylor series, dumb | # terms |"
    printfn "--------------------------------------------------------------------------------------"
    for i = 0 to int points do
        let x = (b - a) / points * (float i) + a
        let resSmartTaylor = smartTaylorFunc x
        let resDumbTaylor = dumbTaylorFunc x 
        printfn "| %5.3f |  %-9.6f|       %-14.6f |    %-5d|       %-13.6f |    %-5d|" x (builtinFunc (2. * x)) (fst resSmartTaylor) (snd resSmartTaylor) (fst resDumbTaylor + 1.) (snd resDumbTaylor)
    printfn "--------------------------------------------------------------------------------------"

// Task 2

// Iterations

let absoluteValue (x: float) =
    if x < 0 then
        -x
    else
        x

let rec iterationFunc F x cond next =
    if cond x then
        x
    else
        iterationFunc F (F x) cond next

let iterations F x =
    let cond x = absoluteValue (F x - x) <= EPS
    let next x = F x
    iterationFunc F x cond next

// Newthon

let derivative f x =
    (f (x + EPS) - f x) / EPS

let Newthon F x =
    let cond x = absoluteValue (F x - x) <= EPS
    let next x = x - F x / derivative F x
    iterationFunc F x cond next

// Dichotomy

let rec Dichotomy f (a: float) (b: float) =
    if b - a <= EPS then
        (a + b) / 2.
    else
        let c = (a + b) / 2.
        if f b * f c < 0. then
            Dichotomy f c b
        else
            Dichotomy f a c

let f1 (x: float) = (0.1 * (x ** 2)) - x * Math.Log(x) 
let F1 (x: float) = Math.Exp((0.1 * (x ** 2)) / x)

let f2 (x: float) = Math.Tan(x) - ((Math.Tan(x) ** 3.) / 3.) + ((Math.Tan(x) ** 5) / 5.) - (1. / 3.)
let F2 (x: float) = Math.Atan(((Math.Tan(x) ** 3) / 3.) + ((Math.Tan(x) ** 5) / 5.) - (1. / 3.))

let f3 (x: float) = Math.Acos(x) - Math.Sqrt(1. - 0.3 * x**3)
let F3 (x: float) = Math.Cos(Math.Sqrt(1. - 0.3 * x**3))

let equationTable f1 F1 f2 F2 f3 F3 a1 b1 a2 b2 a3 b3 = 
    printfn "----------------------------------------"
    printfn "| # eq |  Iters  | Dichotomy | Newthon |"
    printfn "----------------------------------------"
    printfn "|   1  | %7.4f |   %-8.4f| %7.4f |" (iterations F1 a1) (Dichotomy f1 a1 b1) (Newthon F1 a1)
    printfn "|   2  | %7.4f |   %-8.4f| %7.4f |" (iterations F2 a2) (Dichotomy f2 a2 b2) (Newthon F2 a2)
    printfn "|   3  | %7.4f |   %-8.4f| %7.4f |" (iterations F3 a3) (Dichotomy f3 a3 b3) (Newthon F3 a3)
    printfn "----------------------------------------"

taylorTable Math.Exp dumbTaylorSeries smartTaylorSeries num_points 0.1 0.6
printfn ""
equationTable f1 F1 f2 F2 f3 F3 1 2 0 0.8 0 1
