//функция для вычисления встроенного значения
let builtinFunction x = 
    (1.0 + x * x) / 2.0 * atan x - x / 2.0

// dumb метод Тейлора
let dumbTaylor x eps =
    let rec calculate n sum =
        let term = 
            let sign = if n % 2 = 0 then -1.0 else 1.0
            sign * (x ** (2.0 * float n + 1.0)) / (4.0 * (float n) ** 2.0 - 1.0)
        if abs term < eps then (sum + term, n + 1)
        else calculate (n + 1) (sum + term)
    calculate 1 0.0

// умный способ тейлора
let smartTaylor x eps =
    let rec calculate n prevTerm sum count =
        let nextTerm = 
            -prevTerm * x * x * (4.0 * (float n - 1.0) ** 2.0 - 1.0) 
            / (4.0 * (float n) ** 2.0 - 1.0)
        if abs nextTerm < eps then (sum + nextTerm, count + 1)
        else calculate (n + 1) nextTerm (sum + nextTerm) (count + 1)
    
    let firstTerm = x ** 3.0 / 3.0
    if abs firstTerm < eps then (firstTerm, 1)
    else calculate 2 firstTerm firstTerm 1

//задаю точки интервала
let a = 0.1
let b = 0.6
let step = (b - a) / 9.0
let points = [ for i in 0..9 -> a + float i * step ]
printfn "| %5s | %10s | %10s | %6s | %10s | %6s |" "x" "Builtin" "Smart" "#" "Dumb" "#"
printfn "|-------|-----------|-----------|--------|-----------|--------|"
for x in points do
    let builtin = builtinFunction x
    let (smartVal, smartCount) = smartTaylor x 1e-6
    let (dumbVal, dumbCount) = dumbTaylor x 1e-6
    printfn "| %5.3f | %9.6f | %9.6f | %6d | %9.6f | %6d |" x builtin smartVal smartCount dumbVal dumbCount