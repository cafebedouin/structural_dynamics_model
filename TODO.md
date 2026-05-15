  1. Engineering Cleanup (Surgical Updates)
   * Kill the Legacy Chi Path: Complete the legacy-power-modifier-migration TODOs. Ensure every classification path uses
     the drl_core canonical sigmoid pipeline.
   * Python Toolset Consolidation: Group the scripts in python/ into logical subdirectories (e.g., python/audits/,
     python/sensitivity/, python/utils/) and perhaps create a single entry-point CLI for the most common analysis tasks.

  2. Completing the "Open Work" (Research Frontiers)
   * Engine Extensions: Implement the Scope-design validator and MaxEnt parameterization for arbitrary sites mentioned
     in when_apparatus_sharpens_taxonomy.md. These aren't just features; they are the "repairs" surfaced by your latest
     audits.
   * Arakelov Fragility on 10-Slice Contexts: The orientation marks this as an "Open" infrastructure task. Computing
     this would close a significant gap in the sheaf audit findings.
