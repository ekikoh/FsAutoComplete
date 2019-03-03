module FsAutoComplete.Workspace

open ProjectRecognizer
open System.IO

type DPW_ProjectOptions = Dotnet.ProjInfo.Workspace.ProjectOptions
type DPW_ProjectSdkType = Dotnet.ProjInfo.Workspace.ProjectSdkType
type DPW_ProjectOutputType = Dotnet.ProjInfo.Workspace.ProjectOutputType
type DPW_ExtraProjectInfoData = Dotnet.ProjInfo.Workspace.ExtraProjectInfoData

let getProjectOptions notifyState (loader: Dotnet.ProjInfo.Workspace.Loader, fcsBinder: Dotnet.ProjInfo.Workspace.FCS.FCSBinder) verbose (projectFileName: SourceFilePath) =
    if not (File.Exists projectFileName) then
        Error (GenericError(projectFileName, sprintf "File '%s' does not exist" projectFileName))
    else

        let loadProj projectPath =
            loader.LoadProjects [projectPath]

            match fcsBinder.GetProjectOptions (projectPath) with
            | Some po ->
                Result.Ok (po, List.ofArray po.SourceFiles, Map.empty)
            | None -> 
                Error (GenericError(projectFileName, (sprintf "Project file '%s' parsing failed" projectFileName)))

        match projectFileName with
        | NetCoreProjectJson ->
            ProjectCrackerProjectJson.load projectFileName
        | NetCoreSdk ->
            loadProj projectFileName
        | FSharpNetSdk ->
            Error (GenericError(projectFileName, (sprintf "Project file '%s' using FSharp.NET.Sdk not supported" projectFileName)))
        | Net45 ->
            loadProj projectFileName
        | Unsupported ->
            Error (GenericError(projectFileName, (sprintf "Project file '%s' not supported" projectFileName)))

let private mapExtraOptions (dpwExtraProjectInfo: DPW_ExtraProjectInfoData) : ExtraProjectInfoData =
    let mapProjectSdkType (x: DPW_ProjectSdkType) : ProjectSdkType =
        match x with
        | DPW_ProjectSdkType.Verbose v ->
            ProjectSdkType.Verbose
                { ProjectSdkTypeVerbose.TargetPath = v.TargetPath }
        | DPW_ProjectSdkType.DotnetSdk v ->
            ProjectSdkType.DotnetSdk
                { ProjectSdkTypeDotnetSdk.IsTestProject = v.IsTestProject
                  Configuration = v.Configuration
                  IsPackable = v.IsPackable
                  TargetFramework = v.TargetFramework
                  TargetFrameworkIdentifier = v.TargetFrameworkIdentifier
                  TargetFrameworkVersion = v.TargetFrameworkVersion
                  MSBuildAllProjects = v.MSBuildAllProjects
                  MSBuildToolsVersion = v.MSBuildToolsVersion
                  ProjectAssetsFile = v.ProjectAssetsFile
                  RestoreSuccess = v.RestoreSuccess
                  Configurations = v.Configurations
                  TargetFrameworks = v.TargetFrameworks
                  TargetPath = v.TargetPath
                  RunArguments = v.RunArguments
                  RunCommand = v.RunCommand
                  IsPublishable = v.IsPublishable }

    let mapProjectOutputType (x: DPW_ProjectOutputType) : ProjectOutputType =
        match x with
        | DPW_ProjectOutputType.Library -> ProjectOutputType.Library
        | DPW_ProjectOutputType.Exe -> ProjectOutputType.Exe
        | DPW_ProjectOutputType.Custom o -> ProjectOutputType.Custom o

    let extraInfo =
        { ExtraProjectInfoData.ProjectOutputType = mapProjectOutputType dpwExtraProjectInfo.ProjectOutputType
          ProjectSdkType = mapProjectSdkType dpwExtraProjectInfo.ProjectSdkType }

    extraInfo

let bindExtraOptions (opts: FSharp.Compiler.SourceCodeServices.FSharpProjectOptions, projectFiles, logMap) =
    match opts.ExtraProjectInfo with
    | None ->
        Error (GenericError(opts.ProjectFileName, "expected ExtraProjectInfo after project parsing, was None"))
    | Some x ->
        match x with
        | :? ExtraProjectInfoData as extraInfo ->
            Ok (opts, extraInfo, projectFiles, logMap)
        | :? DPW_ProjectOptions as poDPW ->
            let extraInfo = mapExtraOptions poDPW.ExtraProjectInfo
            let fsacOpts = { opts with ExtraProjectInfo = Some (box(extraInfo)) }
            Ok (fsacOpts, extraInfo, projectFiles, logMap)
        | :? DPW_ExtraProjectInfoData as extraInfoDPW ->
            let extraInfo = mapExtraOptions extraInfoDPW
            let fsacOpts = { opts with ExtraProjectInfo = Some (box(extraInfo)) }
            Ok (fsacOpts, extraInfo, projectFiles, logMap)
        | x ->
            Error (GenericError(opts.ProjectFileName, (sprintf "expected ExtraProjectInfo after project parsing, was %A" x)))

let private parseProject' onLoaded (loader, fcsBinder) verbose projectFileName =
    projectFileName
    |> getProjectOptions onLoaded (loader, fcsBinder) verbose
    |> Result.bind bindExtraOptions

let parseProject (loader, fcsBinder) verbose projectFileName =
    projectFileName
    |> parseProject' ignore (loader, fcsBinder) verbose

let loadInBackground onLoaded (loader, fcsBinder) verbose (projects: Project list) = async {

    for project in projects do
        match project.Response with
        | Some res ->
            onLoaded (WorkspaceProjectState.Loaded (res.Options, res.ExtraInfo, res.Files, res.Log))
        | None ->
            project.FileName
            |> parseProject' onLoaded (loader, fcsBinder) verbose
            |> function
               | Ok (opts, extraInfo, projectFiles, logMap) ->
                   onLoaded (WorkspaceProjectState.Loaded (opts, extraInfo, projectFiles, logMap))
               | Error error ->
                   onLoaded (WorkspaceProjectState.Failed (project.FileName, error))

    }
