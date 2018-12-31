namespace TimeOff

open System

// CTRL + K + C = comment
// CTRL + K + U = decomment

// Then our commands
type Command =
    //Demander un congé
    | RequestTimeOff of TimeOffRequest
    //validé une demande de congé
    | ValidateRequest of UserId * Guid
    //Annuler une demande de congé
    | CancelRequest of UserId * Guid 
    //Refuser une demande de congé
    | RefuseRequest of UserId * Guid 
    //Demander l'annulation
    | AskingCancelOfAValidatedRequest of UserId * Guid
    //Refuser demande d'annulation
    | RefuseAskingCancelOfAValidatedRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId,_) -> userId
        | CancelRequest(userId, _) -> userId
        | AskingCancelOfAValidatedRequest(userId, _) -> userId
        | RefuseRequest(userId, _) -> userId
        | RefuseAskingCancelOfAValidatedRequest(userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCancelled of TimeOffRequest 
    | RequestAskingCancelOfAValidated of TimeOffRequest
    | RequestAskingCancelOfAValidatedRefused of TimeOffRequest
    | RequestRefused of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancelled request -> request
        | RequestAskingCancelOfAValidated request -> request
        | RequestAskingCancelOfAValidatedRefused request -> request
        | RequestRefused request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest 
        | AskingCancelOfAValidated of TimeOffRequest
        | AskingCancelOfAValidatedRefused of TimeOffRequest
        | Cancelled
        | Refused
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | Cancelled -> invalidOp "Cancelled request can't change state"
            | Refused -> invalidOp "Refused request can't change state"
            | PendingValidation request -> request
            | AskingCancelOfAValidatedRefused request -> request
            | Validated request -> request
            | AskingCancelOfAValidated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | Cancelled -> false
            | Refused -> false
            | PendingValidation _
            | Validated _
            | AskingCancelOfAValidatedRefused _
            | AskingCancelOfAValidated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestAskingCancelOfAValidated request -> AskingCancelOfAValidated request
        | RequestAskingCancelOfAValidatedRefused request -> AskingCancelOfAValidatedRefused request
        | RequestCancelled request -> Cancelled

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsHalfday (a: HalfDay) (b: HalfDay): bool =
        let aTuple=(a,b)
        match aTuple with
            | (a,b) when a=PM && b=PM -> true
            | (a,b) when a=AM && b=AM -> true
            | _ -> false


    let overlapsWithHalfday (request1: TimeOffRequest, request2: TimeOffRequest): bool =
        true

    //let overlapsWithHalfday request1 request2: bool =
    //    let aTuple=(request1,request2)
    //    if(overlapsHalfday(request1.Start.HalfDay,request2.Start.HalfDay)) then true
    //    else
    //    match aTuple with
    //        | (a,b) when overlapsHalfday(a.Start.HalfDay,b.Start.HalfDay) -> true
    //        | (a,b) when a.Start.Date = b.Start.Date && a.End.Date < b.End.Date -> true
    //        | _ -> false
     
    let overlapsWith request1 request2: bool =
        let aTuple=(request1,request2)
        if(not(request1.UserId.Equals request2.UserId)) then false
        else 
        match aTuple with
            | (a,b) when b.Start.Date > a.Start.Date && b.Start.Date < a.End.Date -> true
            | (a,b) when a.Start.Date > b.Start.Date && a.Start.Date < b.End.Date -> true
            | (a,b) when b.End.Date < a.End.Date && b.End.Date > a.Start.Date -> true
            | (a,b) when a.End.Date < b.End.Date && a.End.Date > b.Start.Date -> true
            | (a,b) when a.Start.Date = b.Start.Date || a.End.Date = b.End.Date || a.Start.Date = b.End.Date || a.End.Date = b.Start.Date -> overlapsWithHalfday(a,b)
            | _ -> false

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        let list = otherRequests |> Seq.toList
                                 |> List.filter (fun arg1 -> overlapsWith request arg1)
        if(not(list.IsEmpty)) then true
        else false
        

    let createRequest today activeUserRequests request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        // This DateTime.Today must go away!
        elif request.Start.Date <= today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | _ ->
            Error "Request cannot be refused"

    let cancelRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelled request]
        | AskingCancelOfAValidated request ->
            Ok [RequestCancelled request]
        | AskingCancelOfAValidatedRefused request ->
            Ok [RequestCancelled request]
        | _ ->
            Error "Request cannot be cancelled"

    let askingCancelValidatedRequest requestState =
        match requestState with
        | Validated request ->
            Ok [RequestAskingCancelOfAValidated request]
        | _ ->
            Error "Request cannot be asking to cancel she must be already validated to do this"

    let decide (today : DateTime) (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            match command with
            | RefuseRequest (_, requestId) ->
                Error "You can cancel but not refuse this request"
            | _ -> Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest today activeUserRequests request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "You must refer to your Manager"
                else
                    let request = defaultArg (userRequests.TryFind requestId) NotCreated
                    if request.Request.Start.Date < today then 
                        Error "It s too late to valide this request"
                    else
                        let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                        validateRequest requestState
            | RefuseRequest (_, requestId) ->
                let request = defaultArg (userRequests.TryFind requestId) NotCreated
                if (user <> Manager && request.Request.Start.Date > today) then
                    Error "You can cancel but not refuse this request"
                else
                    if(user <> Manager) then
                        Error "Unauthorized"
                    else
                        if request.Request.Start.Date < today then
                            Error "It s too late to refuse this request"
                        else
                            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated  
                            refuseRequest requestState
            | CancelRequest (_, requestid) ->
                let request = defaultArg (userRequests.TryFind requestid) NotCreated
                if (user <> Manager) then
                    Error "you can't cancel this request. pleaze refer to your manager"
                else
                    Error "carotte"

                    
