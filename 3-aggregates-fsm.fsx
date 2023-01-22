 

module StateMachines =
    type FSM<'Input, 'Output> = FSM of 'Output
    type Evolve<'Input, 'Output, 'FSM> = 'Input -> 'FSM -> 'Output * 'FSM
    type Aggregate<'Command, 'Event, 'State> = FSM<'Command, 'Event list * 'State>

[<RequireQualifiedAccess>]
module internal Map =
    let update key value map =
        map |> Map.change key (fun _ -> Some value)

[<RequireQualifiedAccess>]
module BouncersShift =
    open StateMachines
    
    [<RequireQualifiedAccess>]
    module internal Bouncers =
        open StateMachines

        type BouncerCommand = 
            private
            | StartGuarding
            | HaveABreak
            | ReportNewPeopleComing of count: int
        type BouncerDetails = private { name: string; count: int }
        type BouncerState = 
            | Guarding of bouncer: BouncerDetails
            | HavingABreak of bouncer: BouncerDetails
        type BouncerEvent = 
            | StartedGuarding
            | StartedABreak
            | CountingRefused of reason: string 
            | ReportedCount of count: int
        type Bouncer = Bouncer of Aggregate<BouncerCommand, BouncerEvent, BouncerState>
        type private EvolveBouncer = Evolve<BouncerCommand, BouncerEvent list * BouncerState, Bouncer>

        let private with' = List.append
        let private NoEvents = List.empty
        let private evolve: EvolveBouncer =
            fun cmd bouncer ->
                let (Bouncer(FSM (events, bouncer'))) = bouncer

                let output =
                    match cmd, bouncer' with
                    | StartGuarding, Guarding _ -> NoEvents, bouncer'
                    | HaveABreak, Guarding bouncer'' -> [StartedABreak], HavingABreak bouncer''
                    | StartGuarding, HavingABreak bouncer'' -> [StartedGuarding], Guarding bouncer''
                    | HaveABreak, HavingABreak _ -> NoEvents, bouncer'
                    | ReportNewPeopleComing count, Guarding bouncer'' -> 
                        [ReportedCount count], Guarding { bouncer'' with count = bouncer''.count + count }
                    | ReportNewPeopleComing _, HavingABreak _ -> [CountingRefused "I am having a break"], bouncer'

                let (newEvents, newBouncerState) = output

                output, (Bouncer(FSM(with' events <| newEvents, newBouncerState)))

        let startShift name = Bouncer(FSM(NoEvents, HavingABreak { name = name; count = 0}))
        let haveABreak = evolve HaveABreak
        let startGuarding = evolve StartGuarding
        let reportNewPeopleComing count = evolve (ReportNewPeopleComing count)
        let countPeople ((Bouncer (FSM((_, bouncer')))): Bouncer) =
            match bouncer' with
            | Guarding guardingBouncerDetails -> guardingBouncerDetails.count
            | HavingABreak havingABreakBouncerDetails -> havingABreakBouncerDetails.count

    type BouncersShiftCommand = 
        private StartGuarding of name: string | ScheduleBreakFor of name: string | ReportNewPeopleComing of name: string * count: int
    type BouncersShiftDetails = private {
        bouncers: Map<string, Bouncers.Bouncer>
    }
    type BouncersShiftEvent = 
        private 
        | StartedGuarding of name: string
        | ScheduledBreakFor of name: string
        | CountReported of name: string * count: int
        | CountingRefused of name: string * reason: string
        | PartyingPeopleLimitReached
        | SchedulingABreakRejected of reason: string

    type BouncersShiftState = private NoMorePeopleAllowed of BouncersShiftDetails | MorePeopleAllowed of BouncersShiftDetails
    type BouncersShift = private BouncersShift of Aggregate<BouncersShiftCommand, BouncersShiftEvent, BouncersShiftState>
    type EvolveBouncersShift = Evolve<BouncersShiftCommand, BouncersShiftEvent list * BouncersShiftState, BouncersShift>

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
            |> List.sumBy (fun ((Bouncers.Bouncer(FSM(_, bouncer))): Bouncers.Bouncer) ->
                match bouncer with
                | Bouncers.Guarding _ -> 1
                | _ -> 0
            )
        guardingBouncersCount > 2

    
    let private ensureAtLeastThreeBouncers (bouncerNames: string list) =
        if bouncerNames |> List.length < 3
            then failwith "At least three bouncers are required!"

    let private translateFacts bouncerName (bouncerFacts: Bouncers.BouncerEvent list): BouncersShiftEvent list =
        bouncerFacts
        |> List.map (fun fact ->
            match fact with
            | Bouncers.StartedGuarding -> StartedGuarding bouncerName
            | Bouncers.StartedABreak -> ScheduledBreakFor bouncerName
            | Bouncers.ReportedCount count -> CountReported (bouncerName, count)
            | Bouncers.CountingRefused reason -> CountingRefused (bouncerName, reason)
        )
    let private handle cmd bouncerName shift =
        let bouncer = shift.bouncers[bouncerName]
        let ((bouncerFacts, _), bouncer') = bouncer |> cmd
        let bouncers =
            shift.bouncers |> Map.update bouncerName bouncer'
        (translateFacts bouncerName bouncerFacts), { shift with bouncers = bouncers }  
    
    let private handleBouncersWhenMorePeopleAllowed cmd bouncerName shift =
        handle cmd bouncerName shift
        |> fun (events, shiftState) -> events, MorePeopleAllowed shiftState
    
    let private handleBouncersWhenNoMorePeopleAllowed cmd bouncerName shift =
        handle cmd bouncerName shift
        |> fun (events, shiftState) -> events, NoMorePeopleAllowed shiftState


    let handleShiftWhenNoMorePeopleAllowed cmd shiftDetails' =
        match cmd with
        | StartGuarding bouncerName ->
            handleBouncersWhenNoMorePeopleAllowed (Bouncers.startGuarding) bouncerName shiftDetails'

        | ReportNewPeopleComing _ ->
            [PartyingPeopleLimitReached], NoMorePeopleAllowed shiftDetails'

        | ScheduleBreakFor _ when (not << atLeastTwoGuarding) shiftDetails'.bouncers ->
            [SchedulingABreakRejected "Can't schedule break because less than two bouncers are guarding"], NoMorePeopleAllowed shiftDetails'

        | ScheduleBreakFor bouncerName ->
            handleBouncersWhenNoMorePeopleAllowed (Bouncers.haveABreak) bouncerName shiftDetails'
    
    let handleShiftWhenMorePeopleAllowed cmd shiftDetails' =
        match cmd with
        | StartGuarding bouncerName ->
            handleBouncersWhenMorePeopleAllowed (Bouncers.startGuarding) bouncerName shiftDetails'

        | ReportNewPeopleComing (_, newPeopleComingCount) 
            when shiftDetails'.bouncers |> (not << reportedLessThan100PeopleWith newPeopleComingCount) ->
            [PartyingPeopleLimitReached], NoMorePeopleAllowed shiftDetails'

        | ReportNewPeopleComing (bouncerName, newPeopleComingCount) ->
            handleBouncersWhenMorePeopleAllowed (Bouncers.reportNewPeopleComing newPeopleComingCount) bouncerName shiftDetails'

        | ScheduleBreakFor _
            when (not << atLeastTwoGuarding) shiftDetails'.bouncers ->
            [SchedulingABreakRejected "Can't schedule break because less than two bouncers are guarding"], MorePeopleAllowed shiftDetails'

        | ScheduleBreakFor bouncerName ->
            handleBouncersWhenMorePeopleAllowed (Bouncers.haveABreak) bouncerName shiftDetails'
    let private with' = List.append
    let evolve: EvolveBouncersShift =
        fun cmd bouncersShift ->

            let (BouncersShift(FSM(events, (shiftDetails)))) = bouncersShift
            
            let result =
                match shiftDetails with
                | NoMorePeopleAllowed shiftDetails' ->
                    handleShiftWhenNoMorePeopleAllowed cmd shiftDetails'
                | MorePeopleAllowed shiftDetails' ->
                    handleShiftWhenMorePeopleAllowed cmd shiftDetails'

            let (newEvents', squadState) = result

            result, BouncersShift(FSM(with' events <| newEvents', squadState))
                    
    let startGuarding name = evolve (StartGuarding name)
    let scheduleBreakFor name = evolve (ScheduleBreakFor name)
    let reportNewPeopleComing name count = evolve (ReportNewPeopleComing (name, count))

    let private toBouncers bouncerNames =
        bouncerNames
            |> List.map (fun name -> name, Bouncers.startShift name)
            |> Map.ofList
    let private NoEventsWhenStarting = List.empty
    let start bouncerNames =
        ensureAtLeastThreeBouncers bouncerNames
        let bouncers= bouncerNames |> toBouncers
        
        BouncersShift(FSM (NoEventsWhenStarting, MorePeopleAllowed { bouncers = bouncers }))


let ignoreFacts = snd

let shift = BouncersShift.start ["mike";"john";"derek"]

shift 
|> BouncersShift.startGuarding "mike"
|> (ignoreFacts >> BouncersShift.startGuarding "derek")
|> (ignoreFacts >> BouncersShift.startGuarding "john")
|> (ignoreFacts >> BouncersShift.reportNewPeopleComing "mike" 20)
|> (ignoreFacts >> BouncersShift.scheduleBreakFor "mike")
|> (ignoreFacts >> BouncersShift.reportNewPeopleComing "mike" 20)
|> (ignoreFacts >> BouncersShift.reportNewPeopleComing "derek" 40)
|> (ignoreFacts >> BouncersShift.reportNewPeopleComing "derek" 40)
|> (ignoreFacts >> BouncersShift.reportNewPeopleComing "derek" 40)
|> (ignoreFacts >> BouncersShift.scheduleBreakFor "derek")
|> (ignoreFacts >> BouncersShift.startGuarding "mike")
|> (ignoreFacts >> BouncersShift.scheduleBreakFor "derek")