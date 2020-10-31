module Giraffe.Fluid

open System.IO
open System.Text
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open Fluid
open Fluid.Values
open Giraffe.Common

type FluidFilterFunc = FluidValue -> FilterArguments -> TemplateContext -> FluidValue
type FluidAsyncFilterFunc = FluidValue -> FilterArguments -> TemplateContext -> ValueTask<FluidValue>
type FluidAccessGetter = System.Func<obj, obj>
type FluidAccessGetterWithContext = System.Func<obj, TemplateContext, obj>
type FluidAccessNamedGetter = System.Func<obj, string, obj>
type FluidAccessNamedGetterWithContext = System.Func<obj, string, TemplateContext, obj>
type FluidTypeMapping = System.Type * System.Func<obj, FluidValue>
type FluidValueConverter = System.Func<obj, obj>
//type FluidTryParse<'T> = delegate of (string * FluidTemplate byref) -> bool

type AccessStrategy =
| AccessPublic of System.Type
| AccessWhitelist of System.Type * string seq
| AccessOverride of FluidAccessNamedGetter
| AccessOverrideWithContext of FluidAccessNamedGetterWithContext
| AccessWhitelistOverride of (string * FluidAccessGetter) seq
| AccessWhitelistOverrideWithContext of (string * FluidAccessGetterWithContext) seq

type AccessRegistration = AccessStrategy seq
type FilterRegistration = (string * FluidFilterFunc) seq
type AsyncFilterRegistration = (string * FluidAsyncFilterFunc) seq
type ContentRegistration = (string * obj) seq

type FluidContextDefinition =
    {
        GlobalMemberAccessRegistration: AccessRegistration
        MemberAccessRegistration: AccessRegistration
        GlobalFilters: FilterRegistration
        GlobalAsyncFilters: AsyncFilterRegistration
        Filters: FilterRegistration
        AsyncFilters: AsyncFilterRegistration
        Content: ContentRegistration
        MemberNameStrategy: MemberNameStrategy
        MaxRecursionOverride: int option
        MaxStepsOverride: int option
        CultureInfoOverride: System.Globalization.CultureInfo option
        TypeMappings: FluidTypeMapping seq
        ValueConverters: FluidValueConverter seq
    }
    with
        static member Empty =
            {
                GlobalMemberAccessRegistration = Seq.empty
                MemberAccessRegistration = Seq.empty
                GlobalFilters = Seq.empty
                GlobalAsyncFilters = Seq.empty
                Filters = Seq.empty
                AsyncFilters = Seq.empty
                Content = Seq.empty
                MemberNameStrategy = MemberNameStrategies.CamelCase
                MaxRecursionOverride = None
                MaxStepsOverride = None
                CultureInfoOverride = None
                TypeMappings = Seq.empty
                ValueConverters = Seq.empty
            }

module FluidContextDefinition =
    let buildTemplateContext (config : FluidContextDefinition) (configureGlobals : bool) =
        do
            if configureGlobals then
                config.GlobalMemberAccessRegistration
                |> Seq.iter (fun strategy ->
                    // TODO: Find a way to outsource this to an inner function.
                    // Already tried with byref<IMemberAccessStrategy>, but isn't supported.
                    match strategy with
                    | AccessPublic type_ ->
                        TemplateContext.GlobalMemberAccessStrategy.Register(type_)
                    | AccessWhitelist (type_, keys) ->
                        TemplateContext.GlobalMemberAccessStrategy.Register(type_, keys |> Array.ofSeq)
                    | AccessOverride getter ->
                        TemplateContext.GlobalMemberAccessStrategy.Register(getter)
                    | AccessOverrideWithContext getter ->
                        TemplateContext.GlobalMemberAccessStrategy.Register(getter)
                    | AccessWhitelistOverride keysAndGetters ->
                        for key, getter in keysAndGetters do
                            TemplateContext.GlobalMemberAccessStrategy.Register(key, getter)
                    | AccessWhitelistOverrideWithContext keysAndGetters ->
                        for key, getter in keysAndGetters do
                            TemplateContext.GlobalMemberAccessStrategy.Register(key, getter)
                )
                config.GlobalFilters
                |> Seq.iter (fun (key, filterFunc) -> TemplateContext.GlobalFilters.AddFilter(key, new FilterDelegate(filterFunc)))
                config.GlobalAsyncFilters
                |> Seq.iter (fun (key, filterFunc) -> TemplateContext.GlobalFilters.AddAsyncFilter(key, new AsyncFilterDelegate(filterFunc)))
                config.TypeMappings
                |> Seq.iter (fun (type_, mapping) -> FluidValue.SetTypeMapping(type_, mapping))
                config.ValueConverters
                |> Seq.iter (fun converter -> FluidValue.ValueConverters.Add (converter))

        let mutable context = new TemplateContext()
        do
            config.MemberAccessRegistration
            |> Seq.iter (fun strategy ->
                // TODO: See above
                match strategy with
                | AccessPublic type_ ->
                    context.MemberAccessStrategy.Register(type_)
                | AccessWhitelist (type_, keys) ->
                    context.MemberAccessStrategy.Register(type_, keys |> Array.ofSeq)
                | AccessOverride getter ->
                    context.MemberAccessStrategy.Register(getter)
                | AccessOverrideWithContext getter ->
                    context.MemberAccessStrategy.Register(getter)
                | AccessWhitelistOverride keysAndGetters ->
                    for key, getter in keysAndGetters do
                        context.MemberAccessStrategy.Register(key, getter)
                | AccessWhitelistOverrideWithContext keysAndGetters ->
                    for key, getter in keysAndGetters do
                        context.MemberAccessStrategy.Register(key, getter)
            )
            config.Filters
            |> Seq.iter (fun (key, filterFunc) -> context.Filters.AddFilter(key, new FilterDelegate(filterFunc)))
            config.AsyncFilters
            |> Seq.iter (fun (key, filterFunc) -> context.Filters.AddAsyncFilter(key, new AsyncFilterDelegate(filterFunc)))
            config.Content
            |> Seq.iter (fun (key, value) -> context.SetValue(key, value) |> ignore)

            match config.MaxRecursionOverride with
            | Some max -> context.MaxRecursion <- max
            | None -> ()
            match config.MaxStepsOverride with
            | Some max -> context.MaxSteps <- max
            | None -> ()
            match config.CultureInfoOverride with
            | Some info -> context.CultureInfo <- info
            | None -> ()

        context

type FluidContextDefinition with
    member this.BuildTemplateContext () = FluidContextDefinition.buildTemplateContext this true

module HttpHandlers =

    let configureContext (f: TemplateContext -> unit) =
        let mutable context = new TemplateContext()
        f context
        context

    type FluidTryParseResolution<'T> = delegate of 'T byref -> bool
    type FluidTryParse<'T> = string -> FluidTryParseResolution<'T>
    /// Renders a model and a template with the Fluid template engine and sets the HTTP response
    /// with the compiled output as well as the Content-Type HTTP header to the given value.
    let fluid<'T when 'T : null and 'T :> IFluidTemplate>
        (tryParse : FluidTryParse<'T>) (templateContext: TemplateContext) (contentType : string) (source : string) (model : obj) : HttpHandler =
        let mutable template: 'T = null
        if (tryParse source).Invoke (&template) then
            let bytes =
                template.Render(templateContext)
                |> Encoding.UTF8.GetBytes

            setHttpHeader "Content-Type" contentType
            >=> setBody bytes

        else id

    /// Reads a Fluid template file from disk and compiles it with the given model and sets
    /// the compiled output as well as the given contentType as the HTTP response.
    let fluidTemplate<'T when 'T : null and 'T :> IFluidTemplate> (tryParse : FluidTryParse<'T>) (templateContext: TemplateContext) (contentType : string) (templatePath : string) (model : obj) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let env = ctx.RequestServices.GetService<IWebHostEnvironment>()
                let path = Path.Combine(env.ContentRootPath, templatePath)
                let! template = readFileAsStringAsync path
                return! fluid tryParse templateContext contentType template model next ctx
            }

    //let defaultFluidTemplate =
    //    let x: string * byref<FluidTemplate> -> bool = FluidTemplate.TryParse
    //    let x2 = fun string -> fun (template: byref<FluidTemplate>) -> true
    //    fluidTemplate x

    /// Reads a fluid template file from disk and compiles it with the given model and sets
    /// the compiled output as the HTTP response with a Content-Type of text/html.
    //let fluidHtmlTemplate (templateContext : TemplateContext) (templatePath : string) (model : obj) : HttpHandler =
    //    fluidTemplate templateContext "text/html" templatePath model
