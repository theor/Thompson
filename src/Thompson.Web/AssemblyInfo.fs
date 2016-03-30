namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Thompson.Web")>]
[<assembly: AssemblyProductAttribute("Thompson")>]
[<assembly: AssemblyDescriptionAttribute("Thompson, automatas and regexes")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
