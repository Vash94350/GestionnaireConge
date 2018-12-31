module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let AndDateIs (year,month,day) (events: RequestEvent list,user : User) = events, user, DateTime (year,month,day)
let When (command: Command) (events: RequestEvent list, user: User,today : DateTime) = events, user, today, command
let Then expected message (events: RequestEvent list, user: User,today : DateTime, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide today userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }

    test "Two requests with a same start and/or end Halfday overlap" {
        let request1 = {
            UserId =1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018,11,03); HalfDay = AM }
            End = { Date = DateTime(2018,11,08); HalfDay = PM } }

        let request2 = {
            UserId =1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018,11,08); HalfDay = PM }
            End = { Date = DateTime(2018,11,14); HalfDay = AM } }

        Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
    }

    test "Two requests with start or end date overlaping overlaps" {
        let request1 = {
            UserId =1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018,11,03); HalfDay = AM }
            End = { Date = DateTime(2018,11,08); HalfDay = PM } }

        let request2 = {
            UserId =1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018,11,06); HalfDay = PM }
            End = { Date = DateTime(2018,11,14); HalfDay = AM } }

        Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee 1)
      |> AndDateIs(2018,05,19)
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }

    test "A request in the past cannot be created" {
        let request = {
            UserId =1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018,11,03); HalfDay = AM }
            End = { Date = DateTime(2018,11,03); HalfDay = PM } }

        Given [ ]
        |> ConnectedAs (Employee 1)
        |> AndDateIs(2018,12,19)
        |> When (RequestTimeOff request)
        |> Then (Error "The request starts in the past") "The request should not have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> AndDateIs(2018,05,19)
      |> When (ValidateRequest (1, request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }

    test "A request can t be validated by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee 1)
      |> AndDateIs(2018,05,19)
      |> When (ValidateRequest (1, request.RequestId))
      |> Then (Error "You must refer to your Manager") "The request should have not been validated"
    }

    test "A request can t be validated if start date is in the past" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> AndDateIs(2018,12,29)
      |> When (ValidateRequest (1, request.RequestId))
      |> Then (Error "It s too late to valide this request") "The request should have not been validated"
    }
  ]

[<Tests>]
let refusationTests =
  testList "Refusation tests" [
    test "A request is refused" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> AndDateIs(2018,05,19)
      |> When (RefuseRequest (1, request.RequestId))
      |> Then (Ok [RequestRefused request]) "The request should have been refused"
    }

    test "A request with a start date in the future can t be refused by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee 1)
      |> AndDateIs(2018,05,19)
      |> When (RefuseRequest (1, request.RequestId))
      |> Then (Error "You can cancel but not refuse this request") "The request should not have been refused"
    }

    test "A request with a start date in the past can t be refused by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee 1)
      |> AndDateIs(2019,05,19)
      |> When (RefuseRequest (1, request.RequestId))
      |> Then (Error "Unauthorized") "The request should not have been refused"
    }

    test "A request can t be refused by Manager if start date is in the past" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> AndDateIs(2018,12,29)
      |> When (RefuseRequest (1, request.RequestId))
      |> Then (Error "It s too late to refuse this request") "The request should have not been refused"
    }
]

[<Tests>]
let cancellationTests = 
    testList "Cancellation tests" [
        test "A request is cancelled" {
            let request = {
                UserId = 1;
                RequestId = Guid.NewGuid()
                Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
                End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

            Given [ RequestCreated request ]
            |> ConnectedAs(Employee 1)
            |> AndDateIs(2018,05,19)
            |> When (CancelRequest (1, request.RequestId))
            |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
        }

        test "A validated request is cancelled by employee" {
            let request = {
                UserId = 1;
                RequestId = Guid.NewGuid()
                Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
                End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

            Given [ RequestValidated request]
            |> ConnectedAs(Employee 1)
            |> AndDateIs(2018,05,19)
            |> When (CancelRequest (1, request.RequestId))
            |> Then (Error "This request is validated, you must ask to your manager to cancel it") "The request should not have been cancelled"
        }

        test "A validated request is cancelled by Manager" {
            let request = {
                UserId = 1;
                RequestId = Guid.NewGuid()
                Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
                End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

            Given [ RequestValidated request]
            |> ConnectedAs(Manager)
            |> AndDateIs(2018,05,19)
            |> When (CancelRequest (1, request.RequestId))
            |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
        }

        test "A validated request is cancelled by Manager" {
            let request = {
                UserId = 1;
                RequestId = Guid.NewGuid()
                Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
                End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

            Given [ RequestAskingCancelOfAValidated request]
            |> ConnectedAs(Manager)
            |> AndDateIs(2018,05,19)
            |> When (CancelRequest (1, request.RequestId))
            |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
        }

        test "A validated request is cancelled by Manager" {
            let request = {
                UserId = 1;
                RequestId = Guid.NewGuid()
                Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
                End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

            Given [RequestAskingCancelOfAValidatedRefused request]
            |> ConnectedAs(Manager)
            |> AndDateIs(2018,05,19)
            |> When (CancelRequest (1, request.RequestId))
            |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
        }

        test "A validated request is cancelled by employee" {
            let request = {
                UserId = 1;
                RequestId = Guid.NewGuid()
                Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
                End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

            Given [RequestValidated request]
            |> ConnectedAs(Employee 1)
            |> AndDateIs(2018,05,19)
            |> When (CancelRequest (1, request.RequestId))
            |> Then (Error "You can't cancel this request. Pleaze Refer to your manager") "The request should have been cancelled"
        }

    ]
