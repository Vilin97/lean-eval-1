import LeanEvalGenerator.Core.Generate

namespace EvalTools

open Lean LeanEvalGenerator.Core

set_option autoImplicit false

/-- Lean Pool's libraries are available to proofs, but must never enter a
trusted statement's import closure. Follow repository-local helpers as well. -/
def checkProblemSolutionImports (root : System.FilePath) (moduleName : String) : IO Unit := do
  for imported in (← problemWorkspaceImports root moduleName) do
    let name := parseModuleName imported
    if #[`LeanPool, `Challenge, `Solution].any (·.isPrefixOf name) then
      throw <| IO.userError
        s!"Problem module '{moduleName}' depends on solution-only module '{imported}'. \
           Lean Pool may be imported by Submission files, not problem statements or their helpers."

/-- Add the pinned solution library before the statement's dependencies, keeping
Mathlib last so its shared transitive pins win during `lake update`. -/
def solutionWorkspaceRequires (deps : RootDependencies) (imports : Array String) :
    Except String (Array DependencySpec) := do
  let pool := deps.extras.filter (·.name == "lean-pool")
  unless pool.size == 1 do
    throw "Expected exactly one pinned lean-pool root dependency for solution workspaces"
  let pool ← pool[0]!.toSpec
  let statementDeps ← workspaceRequires deps imports
  return #[pool] ++ statementDeps.filter (·.name != pool.name)

/-- Apply lean-eval's solution dependency policy to the generic generator output.
Only the Lake configuration and solver instructions change; statement imports and
the comparator's trusted environment remain those emitted by the generator. -/
def withSolutionDependencies (root : System.FilePath) (entry : EvalProblemMetadata)
    (deps : RootDependencies) (files : Array (String × String)) :
    IO (Array (String × String)) := do
  checkProblemSolutionImports root entry.moduleName
  let requires ← IO.ofExcept <|
    solutionWorkspaceRequires deps (← problemWorkspaceImports root entry.moduleName)
  let hasChallengeDeps := files.any (·.1 == "ChallengeDeps.lean")
  return files.map fun (path, content) =>
    if path == "lakefile.toml" then
      (path, lakefileToml entry.id requires hasChallengeDeps)
    else if path == "README.md" then
      (path, content ++ "\nLean Pool is available to solutions at the pinned revision in `lakefile.toml`.\n" ++
        "Import individual `LeanPool.*` modules in `Submission.lean` or `Submission/` helpers.\n" ++
        "Problem statements and trusted helpers must not depend on Lean Pool.\n")
    else (path, content)

/-- Shared renderer for generation and scoring's pristine-workspace comparison. -/
def renderSolutionWorkspace (root : System.FilePath) (entry : EvalProblemMetadata)
    (extracteds : Array ExtractedTheorem) (toolchain : String)
    (deps : RootDependencies) (workspaceTest : String) : IO (Array (String × String)) := do
  let files ← renderWorkspace root entry extracteds toolchain deps workspaceTest
  withSolutionDependencies root entry deps files

end EvalTools
