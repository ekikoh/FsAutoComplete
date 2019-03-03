namespace FsAutoComplete

module DotnetProjInfoInspectHelpers =

  let msbuildPropBool (s: string) =
    match s.Trim() with
    | "" -> None
    | Dotnet.ProjInfo.Inspect.MSBuild.ConditionEquals "True" -> Some true
    | _ -> Some false

  let msbuildPropStringList (s: string) =
    match s.Trim() with
    | "" -> []
    | Dotnet.ProjInfo.Inspect.MSBuild.StringList list  -> list
    | _ -> []

module NETFrameworkInfoProvider =

  open System
  open System.IO
  open Dotnet.ProjInfo.Workspace

  let netFWInfo =
    let config = NetFWInfoConfig.Default Environment.msbuildLocator
    let netFwInfo = NetFWInfo.Create(config)
    netFwInfo

  let installedNETVersions () =
    netFWInfo.InstalledNetFws()

  let latestInstalledNETVersion () =
    netFWInfo.LatestVersion()

#if SCRIPT_REFS_FROM_MSBUILD
#else
  let netReferecesAssembliesTFM () =
    Environment.dotNetVersions ()
    |> Array.map Path.GetFileName

  let netReferecesAssembliesTFMLatest () =
    netReferecesAssembliesTFM ()
    |> Array.sortWith (fun x y -> StringComparer.OrdinalIgnoreCase.Compare(x, y))
    |> Array.rev
    |> Array.tryHead
#endif

