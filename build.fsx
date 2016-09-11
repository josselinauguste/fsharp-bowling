// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing.NUnit3

// Directories
let buildDir  = "./bin/"
let deployDir = "./deploy/"


// Filesets
let appReferences  =
    !! "src/**/*.csproj"
    ++ "src/**/*.fsproj"
let testReferences  =
    !! "tests/**/*.csproj"
    ++ "tests/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "BuildTests" (fun _ ->
  MSBuildDebug buildDir "Build" testReferences
    |> Log "TestBuild-Output: "
)

Target "Test" (fun _ ->
  !! (buildDir + "/*.Tests.dll")
    |> NUnit3 (fun p ->
      p)
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

// Build order
"Clean"
  ==> "Build"
  ==> "BuildTests"
  ==> "Test"
  ==> "Deploy"

// start build
RunTargetOrDefault "Build"