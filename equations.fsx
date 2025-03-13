// метод дихотомии
let dichotomy f a b eps =
    let rec iter a b =
        let c = (a + b) / 2.0
        if b - a < eps then c
        else
            if f a * f c <= 0.0 then iter a c
            else iter c b
    if f a * f b > 0.0 then failwith "Корень не найден"
    else iter a b

// метод простых итераций
let iterations phi x0 eps =
    let rec iterate x =
        let nextX = phi x
        if abs (nextX - x) < eps then nextX
        else iterate nextX
    iterate x0

// метод Ньютона
let newton f f' x0 eps =
    let rec iterate x =
        let nextX = x - f x / f' x
        if abs (nextX - x) < eps then nextX
        else iterate nextX
    iterate x0


//              уравнение 18:     x + sqrt(x + sbrt(x - 2.5)) = 0
let f18 x = x + sqrt(abs(x + (x - 2.5) ** (1.0/3.0)))

//уравнение 19: x - 1.0 / (3.0 + sin(3.6 * x)) = 0
let f19 x = x - 1.0 / (3.0 + sin(3.6 * x))
let phi19 x = 1.0 / (3.0 + sin(3.6 * x))

// уравнение 20: 0.1*x^2 - x*log(x) = 0
let f20 x = 0.1 * x**2.0 - x * log(x)
let f20' x = 0.2 * x - (log(x) + 1.0)

// решение и вывод результатов
printfn "| %-15s | %-10s | %-10s | %-10s |" "Уравнение" "Дихотомия" "Итерации" "Ньютон"
printfn "|-----------------|------------|------------|------------|"

// для  18
try
    let root18_dich = dichotomy f18 2.5 3.0 1e-6
    let root18_iter = iterations phi19 0.5 1e-6
    let root18_newt = newton f20 f20' 1.5 1e-6
    printfn "| %-15s | %10.6f | %10.6f | %10.6f |" "18" root18_dich root18_iter root18_newt
with _ -> ()

// для 19
let root19_dich = dichotomy f19 0.0 0.85 1e-6
let root19_iter = iterations phi19 0.5 1e-6
let root19_newt = newton f19 (fun x -> 1.0 + 3.6 * cos(3.6 * x) / (3.0 + sin(3.6 * x)) ** 2.0) 0.5 1e-6
printfn "| %-15s | %10.6f | %10.6f | %10.6f |" "19" root19_dich root19_iter root19_newt

// для 20
let root20_dich = dichotomy f20 1.0 2.0 1e-6
let root20_iter = iterations (fun x -> sqrt(10.0 * x * log(x))) 1.5 1e-6
let root20_newt = newton f20 f20' 1.5 1e-6
printfn "| %-15s | %10.6f | %10.6f | %10.6f |" "20" root20_dich root20_iter root20_newt