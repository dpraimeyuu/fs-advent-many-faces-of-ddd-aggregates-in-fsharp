(*
    Rules:
    * bouncer can be either guarding or having a break
    * bouncer can report new people only when guarding
    * when scheduling a break, at least two bouncers needs to be guarding
    * no more than 100 people partying
    * at least three bouncers
*)
        
module BouncersShift =
    module FP =            
        [<RequireQualifiedAccess>]
        module BouncersShift =
            [<RequireQualifiedAccess>]
            module internal Bouncers =
                type Bouncer = Guarding of GuardingBouncer | HavingABreak of BouncerHavingABreak
                and GuardingBouncer = private GuardingBouncer of peopleCount: int * name: string
                and BouncerHavingABreak = private BouncerHavingABreak of peopleCount: int * name: string

                type private StartGuarding = BouncerHavingABreak -> GuardingBouncer
                type private HaveABreak = GuardingBouncer -> BouncerHavingABreak
                type private ReportNewPeople = int -> GuardingBouncer -> GuardingBouncer
        
                let startGuarding: StartGuarding =
                    fun (BouncerHavingABreak (count, name)) ->
                        GuardingBouncer (count, name)

                let haveABreak: HaveABreak = 
                    fun (GuardingBouncer (count, name)) ->
                        BouncerHavingABreak (count, name)
                        
                let reportNewPeople: ReportNewPeople =
                    fun newPeople (GuardingBouncer (count, name)) ->
                        GuardingBouncer (count + newPeople, name)
                
                let countPeople (bouncer: Bouncer) = 
                    match bouncer with
                    | Guarding (GuardingBouncer (count, _)) -> count
                    | HavingABreak (BouncerHavingABreak (count, _)) -> count
                
                let startShift (name: string) = (BouncerHavingABreak (0, name))
            
            type BouncersShift = private BouncersShift of (Map<string, Bouncers.Bouncer>)

            type BouncersShiftProblem = 
            | CountingRefused of name: string * reason: string
            | PartyingPeopleLimitReached
            | SchedulingABreakRejected of reason: string

            type StartGuarding = 
                string -> BouncersShift -> Result<BouncersShift, BouncersShiftProblem * BouncersShift>
            type ScheduleBreakFor = 
                string -> BouncersShift -> Result<BouncersShift, BouncersShiftProblem * BouncersShift>
            type ReportNewPeopleComing = 
                string -> int -> BouncersShift -> Result<BouncersShift, BouncersShiftProblem * BouncersShift>

            [<RequireQualifiedAccess>]
            module internal Map =
                let update key value map =
                    map |> Map.change key (fun _ -> Some value)

            let startGuarding: StartGuarding =
                fun (name: string) (BouncersShift bouncers) ->
                    let bouncer = bouncers |> Map.find name
                    match bouncer with
                    | Bouncers.HavingABreak bouncerHavingABreak -> 
                        let guardingBouncer =
                            bouncerHavingABreak 
                                |> Bouncers.startGuarding
                                |> Bouncers.Guarding
                        Ok (BouncersShift (bouncers |> Map.update name guardingBouncer))
                    | _ -> Ok (BouncersShift bouncers)

            let private reportedLessThan100PeopleWith (newPeopleComing: int) (bouncers: Map<string, Bouncers.Bouncer>) =
                let currentlyPartyingPeople = 
                        bouncers.Values
                        |> (Seq.cast >> List.ofSeq)
                        |> List.sumBy Bouncers.countPeople
                
                currentlyPartyingPeople + newPeopleComing <= 100

            
            let private atLeastTwoGuarding (bouncers: Map<string, Bouncers.Bouncer>) =
                let guardingBouncersCount =
                    bouncers.Values
                    |> (Seq.cast >> List.ofSeq)
                    |> List.sumBy (fun bouncer ->
                        match bouncer with
                        | Bouncers.Guarding _ -> 1
                        | _ -> 0
                    )
                guardingBouncersCount > 2

            
            let private ensureAtLeastThreeBouncers (bouncerNames: string list) =
                if bouncerNames |> List.length < 3
                    then failwith "At least three bouncers are required!"

            let reportNewPeopleComing: ReportNewPeopleComing =
                fun name newPeopleComing (BouncersShift (bouncers)) ->
                    if bouncers |> reportedLessThan100PeopleWith newPeopleComing
                        then
                            let bouncer = bouncers |> Map.find name
                            match bouncer with
                            | Bouncers.Guarding guardingBouncer -> 
                                let guardingBouncer =
                                    guardingBouncer 
                                        |> Bouncers.reportNewPeople newPeopleComing
                                        |> Bouncers.Guarding
                                Ok (BouncersShift (bouncers |> Map.update name guardingBouncer))
                            | Bouncers.HavingABreak _ -> Error(CountingRefused (name, "having a break"), BouncersShift bouncers)
                        else Error(PartyingPeopleLimitReached, BouncersShift bouncers)

            let scheduleBreakFor: ScheduleBreakFor =
                fun name (BouncersShift (bouncers)) ->
                    if atLeastTwoGuarding bouncers
                        then
                            let bouncer = bouncers |> Map.find name
                            match bouncer with
                            | Bouncers.Guarding guardingBouncer -> 
                                let bouncerHavingABreak =
                                    guardingBouncer
                                        |> Bouncers.haveABreak
                                        |> Bouncers.HavingABreak
                                Ok (BouncersShift (bouncers |> Map.update name bouncerHavingABreak))
                            | _ -> Ok (BouncersShift bouncers)
                        else Error(SchedulingABreakRejected "at least two bounces need to be guarding", BouncersShift bouncers)// Error (SchedulingABreakRejected "at least two bounces need to be guarding")

            let start (bouncerNames: string list) =
                ensureAtLeastThreeBouncers bouncerNames
                let bouncers = 
                    bouncerNames 
                    |> List.map (fun name -> (name, Bouncers.HavingABreak (Bouncers.startShift name)))
                    |> Map.ofList

                BouncersShift (bouncers)


open BouncersShift.FP
let handleProblems (handleProblem: BouncersShift.BouncersShiftProblem -> unit) (bouncersShift: Result<BouncersShift.BouncersShift, (BouncersShift.BouncersShiftProblem * BouncersShift.BouncersShift)>) =
    match bouncersShift with
    | Ok bouncersShift -> bouncersShift
    | Error (error, bouncersShift) -> 
        handleProblem error
        bouncersShift

let ignoreProblems = handleProblems ignore

let shift = BouncersShift.start ["mike";"john";"derek"]


shift
|> BouncersShift.startGuarding "mike"
|> (ignoreProblems >> BouncersShift.startGuarding "derek")
|> (ignoreProblems >> BouncersShift.startGuarding "john")
|> (ignoreProblems >> BouncersShift.reportNewPeopleComing "mike" 20)
|> (ignoreProblems >> BouncersShift.scheduleBreakFor "mike")
|> (ignoreProblems >> BouncersShift.reportNewPeopleComing "mike" 20)
|> (ignoreProblems >> BouncersShift.reportNewPeopleComing "derek" 40)
|> (ignoreProblems >> BouncersShift.reportNewPeopleComing "derek" 40)
|> (ignoreProblems >> BouncersShift.reportNewPeopleComing "derek" 40)
|> (ignoreProblems >> BouncersShift.scheduleBreakFor "derek")
|> (ignoreProblems >> BouncersShift.startGuarding "mike")
|> (ignoreProblems >> BouncersShift.scheduleBreakFor "derek")
