module Validation

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Ctx = Ctx

type A = A

module A =

    let define = Define<Ctx, A, string>()
    let resId = define.Id.Simple(fun _ -> "1")
    let resDef = define.Resource("a", resId).CollectionName("as")
    let lookup = define.Operation.Lookup(fun _ -> Some A)
    let post = define.Operation.Post(fun () -> A).AfterCreate(ignore)


[<Tests>]
let tests =
    testList "Validation" [

        testJob "Does not return 406 if Accept is */*" {
            let! response = Request.post Ctx "/as/1" |> Request.setHeader (Accept "*/*") |> getResponse
            test <@ response.statusCode <> 406 @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" <> "406" @>
        }

        testJob "Does not return 406 if Accept is application/vnd.api+json" {
            let! response =
                Request.post Ctx "/as/1"
                |> Request.setHeader (Accept "application/vnd.api+json")
                |> getResponse

            test <@ response.statusCode <> 406 @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" <> "406" @>
        }

        testJob "Returns 406 if Accept is not */* or application/vnd.api+json" {
            let! response =
                Request.post Ctx "/as/1"
                |> Request.setHeader (Accept "application/json")
                |> getResponse

            response |> testStatusCode 406
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "406" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The client must accept the JSON:API media type (application/vnd.api+json)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 406 if Accept is application/vnd.api+json but is modified with parameters" {
            let! response =
                Request.post Ctx "/as/1"
                |> Request.setHeader (Accept "application/vnd.api+json;foo=bar")
                |> getResponse

            response |> testStatusCode 406
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "406" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The JSON:API media type in the Accept header must not be modified with media type parameters"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Does not return 415 if Content-Type is application/vnd.api+json" {
            let! response =
                Request.post Ctx "/as/1"
                |> Request.setHeader (ContentType (ContentType.parse "application/vnd.api+json").Value)
                |> getResponse

            test <@ response.statusCode <> 415 @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" <> "415" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 415 if Content-Type is not application/vnd.api+json" {
            let! response =
                Request.post Ctx "/as/1"
                |> Request.setHeader (ContentType (ContentType.parse "application/json").Value)
                |> getResponse

            response |> testStatusCode 415
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "415" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Request content must be sent with Content-Type set to the JSON:API media type (application/vnd.api+json)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 415 if Content-Type is application/vnd.api+json but is modified with parameters" {
            let! response =
                Request.post Ctx "/as/1"
                |> Request.setHeader (
                    ContentType(ContentType.create ("application", "vnd.api+json", System.Text.Encoding.UTF8))
                )
                |> getResponse

            response |> testStatusCode 415
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "415" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The JSON:API media type in the Content-Type header must not be modified with media type parameters"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 if a query parameter is invalid" {
            let! response = Request.post Ctx "/as/1?invalid" |> getResponse
            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].source.parameter" = "invalid" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "'invalid' is not an allowed query parameter name according to the JSON:API specification"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        ftestJob "Returns 400 if a query parameter family is invalid" {
            let! response = Request.post Ctx "/as/1?invalid[fooBar]" |> getResponse
            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].source.parameter" = "invalid[fooBar]" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "'invalid[fooBar]' is not an allowed query parameter name according to the JSON:API specification"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

    ]
