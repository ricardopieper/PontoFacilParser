// Learn more about F# at http://fsharp.org

open System

type Record = {
    date: String;
    time: String;
    customer: String;
}

type Checkpoint = {
    date: String;
    timeIn: String;
    timeOut: String;
    customer: String;
}

let parseLine (line: String) : Record =
    let components = line.Split(' ')
    let hourComponents = components.[1].Split(';')
    let date = components.[0]
    let hour = hourComponents.[0]
    if hourComponents.Length > 3 then
        {date = date; time = hour; customer = hourComponents.[3]}
    else     
        {date = date; time = hour; customer = "" }
        
//https://stackoverflow.com/a/7518857
let batchesOf n =
    Seq.mapi (fun i v -> i / n, v) >>
    Seq.groupBy fst >>
    Seq.map snd >>
    Seq.map (Seq.map snd)


let createCheckpoint (batch: Record List) : Checkpoint =
    match batch with
    | recordIn::recordOut::_ -> { date=recordIn.date; timeIn=recordIn.time; timeOut=recordOut.time; customer=recordOut.customer }
    | _ -> failwithf "Verifique se teus registros estao corretos, tem que ter um numero PAR de registros"

let joinRecords (records: Record List) : Checkpoint List = 
    records 
    |> batchesOf 2 
    |> Seq.map (List.ofSeq)
    |> Seq.map createCheckpoint
    |> List.ofSeq

let formatCheckpoints (checkpoints: Checkpoint List) : String = 
    let strings = 
        checkpoints
        |> List.map (fun c -> c.date+"\t"+c.timeIn+"\t"+c.timeOut+ "\t"+c.customer)
        |> Array.ofList

    String.Join("\n", strings)


[<EntryPoint>]
let main argv =
    
    let lines = System.IO.File.ReadAllLines("source")

    let formatted = 
        lines 
        |> Array.toList
        |> List.map parseLine
        |> joinRecords
        |> formatCheckpoints

    System.Console.Write(formatted + "\n")

    0 // return an integer exit code
