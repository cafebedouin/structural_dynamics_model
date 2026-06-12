# OQ-105 alignment rule landed — ruling (a) implemented as prompt rule + fail-closed compiler gate

**Date:** 2026-06-12. **Substrate:** post-cohort-zero-swap live corpus (5 `_c0` stories,
HEAD `7ca48e0b`). **Ruling:** operator ratified fix-fork option (a) — grid alignment at
generation, no read-side interpolation machinery — on 2026-06-12, with two amendments
(densification trade named; time-bound reopen). This audit lands the implementation and
re-derives the live exposure on the post-swap corpus.

## Substrate change since the ruling was drafted

The OQ-109 Phase C cohort-zero swap (`7ca48e0b`, 2026-06-12) retired the entire 62-story
pre-cohort-zero corpus to `prolog/archives/datasets/kernel_v2_test2/` — including **all 11
OQ-105 host constraints**. Live exposure re-derived on the 5-story cohort-zero corpus:
**0 misaligned rows** — every `_c0` story authors one shared time grid across all its
temporal metrics (grid extraction pasted in session log; W2 below is the gate-level
witness). The three series-less `_c0` stories all carry the compiler-stamped
`suppression_profile(static)` marker (OQ-46 sanctioned path).

Consequence for the ratified time-bound ("(b) reopens if the 11 hosts are not regenerated
within Phase C or by a named date"): **discharged by events** — the 11 hosts are no longer
live, so the exposure the deadline bounded is zero. The successor clock is *the alignment
rule must be enforced before further cohort generation* (cohort one regenerates the rest of
the ~60 seeds), which this unit lands.

## What landed

1. **Prompt rule** (`prompts/constraint_story_generation_prompt_json.md`, Temporal
   Measurements section): "One time grid per story (alignment rule, 2026-06-12)" — the
   union grid is a first-class authoring requirement: the model asserts each tracked
   metric's value at each shared point; if it cannot honestly assert a value, it uses a
   sparser shared grid or drops the series (the OQ-46 scalar rule unchanged). Explicitly
   framed as commitment, not backfill — this resolves the densification framing the
   ratification required.
2. **Fail-closed compiler gate** (`python/generate_constraint_pl.py:_grid_alignment_errors`,
   wired into `validate_json` on BOTH the jsonschema and fallback paths): ≥2 temporally
   tracked metrics with differing time-point sets → validation error naming the union grid
   and the missing points. Every generation driver imports `validate_json`
   (cohort_zero_regen, story_generator_base, generate_kernel_corpus, generate_grid_batch,
   regenerate_stories, recover_historical_seeds, validate_constraint_story), so the gate
   covers all paths including cohort one. One-metric and zero-metric stories are trivially
   aligned; the gate cannot fire on absence (no Pattern-5 exposure: it compares two
   *authored* sets, never passes on an empty table — `len(grids) < 2` returns no error,
   which is the OQ-46-sanctioned case, not a vacuous pass).

## Witnesses (`witness_gate.py` / `witness_gate.out`)

- **W1 (positive, synthetic):** live `institutional_trust_erosion_c0.json` with
  `suppression_requirement@T=10` removed → exactly 1 gate error, present in full
  `validate_json` output. PASS.
- **W2 (negative, live):** all 5 live cohort-zero JSONs → 0 gate errors each; full
  `validate_json` on two of them returns CLEAN (return-path regression check). PASS.
- **W3 (positive, real data — the gate's extension equals the known defect set):** all 60
  archived pre-cohort-zero JSONs scanned → **exactly the 11 OQ-105 hosts flagged, no
  others**. The gate would have caught every host and produces zero false positives on the
  retired corpus. PASS.

## The densification trade (recorded so it cannot resurface as a discovered defect)

Option (a) means the model authors values at time-points it didn't organically choose —
generation-side densification, unlabeled, indistinguishable from organically chosen points.
This is the cost (b)'s `interpolated` provenance tag would have labeled. The defense, named
at ratification: model-authored-at-generation carries the same epistemics as every other
authored point in the story, whereas the OQ-105 defect was *code* injecting endpoints post
hoc — (a) does not reproduce the anti-causal failure. The prompt rule's "sparser shared
grid" escape keeps the assertion honest: alignment is achieved by committing or thinning,
never by inventing.

## Reopen conditions (operative)

(b) — labeled interpolation at read, extending the `measurement_basis/2` spine with an
`interpolated` bucket (the extension point stays alive regardless via OQ-107's `witnessed`
bucket) — reopens if:
1. a story with misaligned grids reaches the live corpus despite the gate (gate defect), or
2. the densification cost turns real: cohort-one generation systematically degrades under
   the rule (e.g. grids thinned below drift-detection usefulness, or evidence the model
   fabricates rather than thins), or
3. a Backed-blind consumer of raw `classify_at_time`/`constraint_history` timelines becomes
   load-bearing over a corpus that still contains misaligned rows (archive overlays qualify).

## Scope note (per ratification)

The 2026-06-11 row sweep's "19/23 substitution-robust" rows are robust *relative to linear
interpolation* — the sweep enumerated (b)'s payoff under (b)'s own semantics, not ground
truth about what suppression was at those times.

## Artifacts

| file | role |
|---|---|
| `witness_gate.py` / `witness_gate.out` | W1/W2/W3 witnesses |
| `../2026-06-11_oq105_row_sweep/` | the evidence package the ruling consumed |
