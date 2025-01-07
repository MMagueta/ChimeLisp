using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using Microsoft.NET.HostModel.AppHost;

public class Auxiliar() {
    public static Type Core()
    {
        string referencePath = @"C:/Program Files/dotnet/packs/Microsoft.AspNetCore.App.Ref/9.0.0/ref/net9.0/";
        var x = Directory.GetFiles(referencePath, "*.dll");
        System.Reflection.PathAssemblyResolver resolver = new(Directory.GetFiles(referencePath, "*.dll"));
        using System.Reflection.MetadataLoadContext context = new(resolver);
        Assembly coreAssembly = context.CoreAssembly!;
        //Type voidType = coreAssembly.GetType(typeof(void).FullName!)!;
        //Type objectType = coreAssembly.GetType(typeof(object).FullName!)!;
        //Type stringType = coreAssembly.GetType(typeof(string).FullName!)!;
        //Type stringArrayType = coreAssembly.GetType(typeof(string[]).FullName!)!;
        Type consoleType = coreAssembly.GetType(typeof(Console).FullName!)!;
        return consoleType;
    }
    public static void HostWritter()
    {
        HostWriter.CreateAppHost(
        @"C:/Program Files/dotnet/packs/Microsoft.NETCore.App.Host.win-x64/9.0.0/runtimes/win-x64/native/apphost.exe",
        "C:/Users/mague/Source/Repos/ChimeLisp/Test.exe",
        "C:/Users/mague/Source/Repos/ChimeLisp/Test.dll");
    }
}