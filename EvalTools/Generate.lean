import LeanEvalGenerator.Core.Generate
import EvalTools.SolutionDependencies

namespace EvalTools

open Lean LeanEvalGenerator.Core

set_option autoImplicit false

/-- Repository orchestration around the generic renderer, with lean-eval's
solution-only dependency policy applied identically in write and check modes. -/
def generateSolutionWorkspaces (root : System.FilePath)
    (selectedProblemId : Option String) (check : Bool) : IO Unit := do
  let problems ← loadManifest root
  let selectedProblems ←
    match selectedProblemId with
    | some id =>
        let filtered := problems.filter (·.id == id)
        if filtered.isEmpty then
          throw <| IO.userError s!"Unknown problem id '{id}'"
        pure filtered
    | none =>
        validateManifestAgainstInventory root problems
        let selectedIds : Std.HashSet String := problems.foldl (fun acc p => acc.insert p.id) {}
        let mismatches ← syncUnknownProblemDirs root selectedIds check
        if !mismatches.isEmpty then
          throw <| IO.userError <| "\n".intercalate mismatches.toList
        pure problems
  for entry in selectedProblems do
    checkProblemSolutionImports root entry.moduleName
  validateHoleShape root selectedProblems
  let toolchain ← IO.FS.readFile (root / "lean-toolchain")
  let deps ← loadRootDependencies root
  buildExtractor root selectedProblems
  let workspaceTest ← loadWorkspaceTestTemplate root
  let mut mismatches : Array String := #[]
  for entry in selectedProblems do
    let extracteds ← entry.holes.mapM (extractOne root entry)
    let files ← renderSolutionWorkspace root entry extracteds toolchain deps workspaceTest
    let problemDir := root / "generated" / entry.id
    if check then
      mismatches := mismatches ++ (← checkWorkspace problemDir s!"generated/{entry.id}" files)
    else
      writeWorkspace problemDir files
  if selectedProblemId.isNone then
    mismatches := mismatches ++
      (← writeOrCheckIndex root (problems.map generatedIndexEntry) check)
  if !mismatches.isEmpty then
    throw <| IO.userError <| "\n".intercalate mismatches.toList
  if check then
    IO.println "Generated workspaces are up to date."
  else
    IO.println s!"Generated {selectedProblems.size} problem workspace(s)."

end EvalTools
