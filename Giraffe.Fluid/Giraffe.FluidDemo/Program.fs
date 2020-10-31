namespace Giraffe.FluidDemo

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open FSharp.Control.Tasks.V2.ContextInsensitive

[<AutoOpen>]
module Helpers =
    open Giraffe.Fluid
    open Giraffe.Fluid.HttpHandlers
    open Fluid

    type Message = { Text: string }

    let indexHandler (name : string) =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let env = ctx.RequestServices.GetService<IWebHostEnvironment>()
                let path = Path.Combine(env.ContentRootPath, "Views/Index.liquid")
                let! viewText = readFileAsStringAsync path
                let model = { Text = name }
                let templateContext () =
                    configureContext (fun x ->
                        x.MemberAccessStrategy.Register<Message>()
                        x.SetValue("Model", model)
                        |> ignore
                    )
                let mutable template: Fluid.FluidTemplate = null
                if FluidTemplate.TryParse (viewText, &template) then
                    let! result = template.RenderAsync <| templateContext()
                    return! htmlString result next ctx
                else
                    return! (setStatusCode 500 >=> text "Your code is crap") next ctx
            }

    let webApp =
        choose [
            GET >=>
                choose [
                    route "/" >=> indexHandler "Will"
                    routef "/hello/%s" indexHandler
                ]
            setStatusCode 404 >=> text "Not Found" ]

    // ---------------------------------
    // Error handler
    // ---------------------------------

    let errorHandler (ex : Exception) (logger : ILogger) =
        logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
        clearResponse >=> setStatusCode 500 >=> text ex.Message

module Program =
    open Helpers

    let configureCors (builder : CorsPolicyBuilder) =
        builder.WithOrigins("http://localhost:8080")
               .AllowAnyMethod()
               .AllowAnyHeader()
               |> ignore

    let configureApp (app : IApplicationBuilder) =
        let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
        (match env.EnvironmentName with
        | "Development" -> app.UseDeveloperExceptionPage()
        | _ -> app.UseGiraffeErrorHandler(errorHandler))
            .UseHttpsRedirection()
            .UseCors(configureCors)
            .UseStaticFiles()
            .UseGiraffe(webApp)

    let configureServices (services : IServiceCollection) =
        services.AddCors()    |> ignore
        services.AddGiraffe() |> ignore

    let configureLogging (builder : ILoggingBuilder) =
        builder.AddFilter(fun l -> l.Equals LogLevel.Error)
               .AddConsole()
               .AddDebug() |> ignore

    [<EntryPoint>]
    let main args =
        let contentRoot = Directory.GetCurrentDirectory()
        let webRoot     = Path.Combine(contentRoot, "wwwroot")
        Host.CreateDefaultBuilder(args)
            .ConfigureWebHostDefaults(
                fun webHostBuilder ->
                    webHostBuilder
                        .UseContentRoot(contentRoot)
                        .UseWebRoot(webRoot)
                        .Configure(Action<IApplicationBuilder> configureApp)
                        .ConfigureServices(configureServices)
                        .ConfigureLogging(configureLogging)
                        |> ignore)
            .Build()
            .Run()
        0
