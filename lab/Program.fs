// For more information see https://aka.ms/fsharp-console-apps
type Coach = {
    Co: string
    Player: bool
}
type Stats = {
    Wins: int
    Losses: int
}
type Team = {
    T: string
    Coach: Coach
    Stats: Stats
}


let co1 = { Co = "Joe Mazzulla"; Player = false }
let co2 = { Co = "Billy Donovan"; Player = true }
let co3 = { Co = "Jason Kidd"; Player = true }
let co4 = { Co = "Erik Spoelstra"; Player = false }
let co5 = { Co = "Mike Budenholzer"; Player = false }

let s1 = { Wins = 64; Losses = 18 }
let s2 = { Wins = 39; Losses = 43 }
let s3 = { Wins = 50; Losses = 32 }
let s4 = { Wins = 46; Losses = 36 }
let s5 = { Wins = 57; Losses = 25 }

let t1 = { T = "Boston Celtics"; Coach = co1; Stats = s1 }
let t2 = { T = "Chicago Bulls"; Coach = co2; Stats = s2 }
let t3 = { T = "Dallas Mavericks"; Coach = co3; Stats = s3 }
let t4 = { T = "Miami Heat"; Coach = co4; Stats = s4 }
let t5 = { T = "Oklahoma City Thunder"; Coach = co5; Stats = s5 }

let teams = [t1; t2; t3; t4; t5]
teams |> List.iter (fun t -> printfn "Team: %s, Coach: %s, Wins: %d, Losses: %d" t.T t.Coach.Co t.Stats.Wins t.Stats.Losses)


let isSuccessful t =
    t.Stats.Wins > t.Stats.Losses
let successfulTeams = teams |> List.filter isSuccessful
successfulTeams |> List.iter (fun t ->
    printfn "Successful Team: %s, Coach: %s, Wins: %d, Losses: %d" 
        t.T 
        t.Coach.Co 
        t.Stats.Wins 
        t.Stats.Losses
)


let calculateSuccessPercentage t =
    let totalGames = float (t.Stats.Wins + t.Stats.Losses)
    let successPercentage = (float t.Stats.Wins / totalGames) * 100.0
    (t.T, successPercentage)
let successPercentages = teams |> List.map calculateSuccessPercentage
successPercentages |> List.iter (fun (teamName, percentage) ->
    printfn "Team: %s, Success Percentage: %.2f%%" teamName percentage
)





type Cuisine =
    | Korean
    | Turkish

type Rest = {
    Name: string
    Cuisine: Cuisine
    Cost: float
}

type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float

let calculateBudget activity =
    match activity with
    | BoardGame -> 0.0, 0
    | Chill -> 0.0, 0
    | Movie movieType ->
        match movieType with
        | Regular -> 12.0, 0
        | IMAX -> 17.0, 0
        | DBOX -> 20.0, 0
        | RegularWithSnacks | IMAXWithSnacks | DBOXWithSnacks ->
            let basePrice =
                match movieType with
                | RegularWithSnacks -> 12.0
                | IMAXWithSnacks -> 17.0
                | DBOXWithSnacks -> 20.0
                | _ -> 0.0
            basePrice + 5.0, 0
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0, 0
        | Turkish -> 65.0, 0
    | LongDrive (kilometers, fuelCostPerKm) ->
        let cost = float kilometers * fuelCostPerKm
        cost, kilometers

let exampleActivities = [
    BoardGame
    Chill
    Movie Regular
    Movie IMAX
    Movie DBOX
    Movie RegularWithSnacks
    Restaurant Korean
    Restaurant Turkish
    LongDrive (100, 1.25)
]

exampleActivities
|> List.iter (fun activity ->
    match calculateBudget activity with
    | cost, kilometers when kilometers > 0 ->
        printfn "Activity: LongDrive (%d km), Cost: %.2f CAD" kilometers cost
    | cost, _ ->
        printfn "Activity: %A, Cost: %.2f CAD" activity cost)
