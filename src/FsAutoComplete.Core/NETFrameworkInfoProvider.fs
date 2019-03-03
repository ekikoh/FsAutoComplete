namespace FsAutoComplete

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

