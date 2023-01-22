(*
    Rules:
    * bouncer can be either guarding or having a break
    * bouncer can report new people only when guarding
    * when scheduling a break, at least two bouncers needs to be guarding
    * no more than 100 people partying
    * at least three bouncers
*)

module BouncersShift =
    module OO =
        type private Bouncer(name: string) =
            let mutable isGuarding = false
            let mutable peopleCount = 0
            member this.IsGuarding with get() = isGuarding
            member this.PeopleCounted
                with get() = peopleCount
            member this.Name 
                with get() = name
            member this.HaveABreak() =
                isGuarding <- false
            member this.Guard() =
                isGuarding <- true
            member this.ReportNewPeople(newPeopleCount: int) =
                if this.IsGuarding
                    then peopleCount <- peopleCount + newPeopleCount
                    else failwith $"{this.Name} bouncer is not guarding at the moment"


        let private ensureAtLeastThreeBouncers (bouncerNames: string list) =
            if bouncerNames |> List.length < 3
                then failwith "At least three bouncers are required!"

        let private atLeastTwoGuarding (bouncers: Bouncer list): bool =
            let guardingBouncersCount =
                bouncers 
                |> List.filter (fun bouncer -> bouncer.IsGuarding)
                |> List.length

            guardingBouncersCount > 2

        let private reportedLessThan100PeopleWith (newPeopleComing: int) (bouncers: Bouncer list): bool =
            let currentlyPartyingPeople =
                bouncers
                |> List.map(fun bouncer -> bouncer.PeopleCounted) |> List.sum
            
            currentlyPartyingPeople + newPeopleComing <= 100

        type BouncersShift(bouncerNames: string list) =
            do ensureAtLeastThreeBouncers bouncerNames
            let mutable bouncers = bouncerNames |> List.map (fun name -> name, Bouncer name) |> dict
            member private this.Bouncers with get() = bouncers.Values |> Seq.cast |> List.ofSeq

            member this.ScheduleBreakFor(bouncerName: string) =
                let bouncer = bouncers[bouncerName]
                if atLeastTwoGuarding this.Bouncers
                    then bouncer.HaveABreak()
                    else failwith "Can't schedule break because less than two bouncers are guarding"

            member this.StartGuarding(bouncerName: string) =
                let bouncer = bouncers[bouncerName]
                bouncer.Guard()

            member this.ReportNewPeopleComing(bouncerName: string, newPeopleComing: int) =
                let bouncer = bouncers[bouncerName]
                if this.Bouncers |> reportedLessThan100PeopleWith newPeopleComing
                    then bouncer.ReportNewPeople(newPeopleComing)
                    else failwith "Can't allow more people enter the club"

open BouncersShift.OO

let shift = new BouncersShift(["mike";"john";"derek"])

shift.StartGuarding("mike")
shift.StartGuarding("derek")
shift.StartGuarding("john")
shift.ReportNewPeopleComing("mike", 20)
shift.ScheduleBreakFor("mike")
try shift.ReportNewPeopleComing("mike", 20) with ex -> printfn $"{ex}"
shift.ReportNewPeopleComing("derek", 40)
shift.ReportNewPeopleComing("derek", 40)
try shift.ReportNewPeopleComing("derek", 40) with ex -> printfn $"{ex}"
try shift.ScheduleBreakFor("derek") with ex -> printfn $"{ex}"
shift.StartGuarding("mike")
shift.ScheduleBreakFor("john")