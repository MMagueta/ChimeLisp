#nowarn "62"

namespace Configuration

module Builder =
    open FsConfig

    open Serilog
    
    type ConfigurationContext =
        { Logger: Serilog.ILogger }

    let buildLogger loggingLevel loggingPath =
        let logger =
            LoggerConfiguration()
                .Enrich.FromLogContext()
                .WriteTo.ColoredConsole(outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionContext}] {Message:l}{NewLine}{Exception}")
                .WriteTo.File(loggingPath, outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionContext}] {Message:l}{NewLine}{Exception}")
        
        match loggingLevel with
        | Variables.Debug -> logger.MinimumLevel.Debug()
        | Variables.Information -> logger.MinimumLevel.Information()
        | Variables.Warning -> logger.MinimumLevel.Warning()
        | Variables.Error -> logger.MinimumLevel.Error()
        | Variables.Fatal -> logger.MinimumLevel.Fatal()
        |> fun l -> l.CreateLogger()

    let loadConfiguration (): Result<ConfigurationContext, string> = 
        EnvConfig.Get<Variables.ConfigurationVariables>()
        |> Result.map (fun config -> { Logger = buildLogger config.LoggingLevel config.LoggingPath } )
        |> Result.mapError 
            (function
                | BadValue (key, got) -> $"Variable '{key}' received '{got}'."
                | NotFound value -> $"Value '{value}' not found."
                | NotSupported value -> $"Value '{value}' not supported.")
        |> Result.mapError (fun v ->
                "Error on parsing environment variables for configuration context. " ^ v)