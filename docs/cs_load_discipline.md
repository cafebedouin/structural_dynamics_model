# CS Testset Load Discipline

**OQ-25 resolution.** This note documents the invariant that keeps the canonical
testset directory clean, the grouping-key decision behind the load guard, and the
regeneration protocol any future multi-run workflow must follow.

## The Invariant

**DP-001 (per-reading ε-invariance):** each ConstraintAtom (reading name) must
resolve to exactly one base extractiveness ε in the loaded DB.

The invariant is enforced at load time by a `config_violation/1` clause in
`prolog/config_validation.pl`, which fires inside `validate_config_postcorpus/0`
— the final step of `corpus_loader:load_all_testsets/0`. A violation halts the
Prolog session with exit code 1 before any CS-layer predicate can run.

## Why ConstraintAtom, Not KernelAtom

The guard groups by ConstraintAtom (reading name), not by KernelAtom
(`cs_kernel_id/2`'s second argument). The reason is OQ-26 (resolved): ε is
reading-relative, not kernel-intrinsic. A kernel with three readings legitimately
carries three distinct ε values (one per reading). Grouping by KernelAtom would
false-positive on correct data.

The chimera failure mode is narrower: the *same* ConstraintAtom loaded from two
generation runs with different authored ε. That produces two
`constraint_metric(C, extractiveness, E)` facts for the same C — non-determinism
in the engine's χ = ε × f(d) × σ(S) path. ConstraintAtom grouping catches
exactly this case without touching legitimate multi-reading kernels.

## What the Guard Checks

```prolog
setof(C, UID^cs_story_uid(C, UID), AllCs),
member(C, AllCs),
findall(E, constraint_metric(C, extractiveness, E), Es),
sort(Es, Unique),
Unique = [_, _ | _].   % 2+ distinct ε values → violation
```

`sort/2` deduplicates exact float values. Authored ε are decimal literals in
testset files (no arithmetic), so exact comparison is correct.

## Why This Matters: The §5.11 Sentinel

The kernel divergence count (§5.11 of `deferential_realism_paper_v7.md`: 79
reading-pair divergences across 34 kernels) is the one number in the trifurcation
profile that depends directly on ε via `classify_at_time/4` → sigmoid pipeline.
A chimera load that produces conflicting ε for any reading silently shifts this
count without any other signal. The guard seals the path before classification runs.

## Regeneration Protocol

1. **One coherent generation run per canonical testset load.** The `corpus_path`
   in `config.pl` must point to a directory populated from a single run (or from
   manually triaged additions that are narrative-inspected and ε-stable).

2. **Never assign cs_kernel_id to a non-CS-run story.** A non-CS story handed
   a `cs_kernel_id` fact becomes invisible to the ε guard (the guard keys on
   `cs_story_uid`, which only CS-run stories carry) but visible to
   `cs_kernel_divergence/4`. This is a provenance problem, not an ε-conflict — the
   doc discipline is the only check. Keep `cs_kernel_id` facts exclusive to files
   that also carry `cs_story_uid`.

3. **Never reuse a ConstraintAtom across runs without triaging.** If a new run
   generates a reading with the same atom name as an existing reading, triage
   explicitly: archive the stale one (see `testsets_archive_20260525/`) or give
   the new one a distinct atom name. Silent merge is the chimera failure mode.

4. **Archive first, replace second.** The pattern established in May 2026:
   stale duplicates go to a dated archive dir before the canonical dir is updated.
   The archive is never on the live load path (corpus_path resolves to `testsets`
   only).

## Relationship to Other Checks

| Check | Location | What it enforces |
|---|---|---|
| DP-001 authoring rule | Testset file headers | Each story has one stable ε (authoring discipline) |
| ε range validation | `data_validation.pl` | Each constraint's ε is within [0,1] |
| ε mirror consistency | `data_validation.pl` | `base_extractiveness` and `constraint_metric(extractiveness)` agree within 0.01 |
| UID uniqueness | `config_validation.pl` (above guard) | No two stories share a UUID value (UID-side, not chimera-side) |
| **ε coherence (this guard)** | `config_validation.pl` (OQ-25) | **No reading has two conflicting ε in the DB (chimera-side)** |
