# Phase 0 Recon — kernel/reading orbit discovery (OQ-150)

Read-only recon for the OQ-150 discovery program (plan:
`~/.claude/plans/review-oq-53-from-issues-md-dynamic-planet.md`). No statistic is trusted
until its positive control is witnessed (CLAUDE.md governing stance). Substrate =
the twin pipeline outputs already on disk; no engine re-run.

## Substrate and a provenance caveat (recorded, not introduced here)

- `outputs/pipeline_output.haiku.json` — n=960, `pipeline_run_at` 2026-06-13T23:57:44Z,
  `code_commit_short` **f3ec052**, schema_version 2.
- `outputs/pipeline_output.flash.json` — n=960, `pipeline_run_at` 2026-06-13T20:36:33Z,
  `code_commit_short` **8126231**, schema_version 2.

**The two twins were classified at different code commits** (f3ec052 vs 8126231). These are the
same artifacts the 2026-06-13 twin_comparison and 2026-06-18 within-kernel audits used; the
difference is a pre-existing property of the substrate, not something this audit introduced. It
is a confound to name in any cross-model claim: a fraction of cross-model disagreement could be
code drift rather than model draw. (Both pass the extraction control below, which bounds the
drift's effect on `claimed_type`.)

## Positive controls (witnessed BEFORE any orbit number)

| control | expected | observed | verdict |
|---|---|---|---|
| common ids (same `kernel__reading` slots both twins) | 960 | **960** (0 haiku-only, 0 flash-only) | PASS |
| `cs_kernel_id` cross-model agreement (same kernels both sides) | 1.000 | **1.000** (331 kernels, identical sets) | PASS |
| extraction: `claimed_type` cross-model agreement | ~0.721 | **0.7208** | PASS |
| multi-reading kernels per twin | ~328 | **328 / 328** | PASS |

The extraction control reproduces the independently-computed 0.721 of
`2026-06-13_twin_comparison`, confirming the JSON fields are read correctly (guards against the
schema-stranded `reading_cells` failure that made the 2026-06-18 census vacuous).

## Orbit-key input population on BOTH twins

Per-reading fields in the twin outputs (population = fraction non-null over 960):

| orbit-key input | field | haiku | flash | usable? |
|---|---|---|---|---|
| observer orbit (proxy) | `signature` | 1.000 | 1.000 | YES (proxy for gauge_orbit dr_type-set; see note) |
| commitment-apparatus orbit [axis 1] | `cs_pattern` | 1.000 | 1.000 | YES |
| terminal-projection orbit (committer) [axis 2] | `cs_drift_terminal` | 0.995 | 1.000 | YES |
| seat-signature / role-vector orbit | `perspectives` (4 seats) | 1.000 | 1.000 | YES |
| observer orbit (full gauge set) | `classifications` | **0.000** | **0.000** | NO — empty on both twins |
| axiom-grounding-profile orbit | `cs_axiom_grounding` | — | — | NOT serialized in JSON (Prolog-only) |
| obstruction-class (kernel orbit) | `cs_kernel_obstruction/4` | — | — | NOT serialized (Prolog-only; reads `cs_reading_relation`) |
| (sparse, model-divergent) | `cs_axiom_foreclosed` | 0.142 | 0.019 | sparse + asymmetric |
| (sparse, model-divergent) | `cs_verdicts` | 0.428 | 0.682 | sparse + asymmetric |

**Decision (scope of the JSON-computable pass):** 4 of the 6 reading-orbit keys and 1 of the 2
kernel-orbit keys are computable deterministically from the twin JSON at 100%/~100% population —
including BOTH operator-named axes (apparatus = axis 1, terminal = axis 2). The
`classifications` field is empty on both twins, so the observer-orbit "full gauge dr_type-set
across contexts" is unavailable; `signature` (100%) is the usable observer-orbit proxy, and
`perspectives` gives the 4-seat dr_type vector. `cs_axiom_grounding` and `cs_kernel_obstruction`
are not serialized; obtaining them requires a read-only `swipl` probe over each twin corpus
(safe — a probe, not a pipeline). Those two keys are **deferred to a Phase 1b Prolog probe**,
gated on whether the JSON-computable pass leaves the menu interesting enough to warrant the
extra cost.

## Statistic-family check (Wilson validity) — RESOLVED

The membership-agreement statistic this audit uses is **"fraction of the 960 common ids whose
key value is identical across the two twins"** — a per-id Bernoulli proportion. Wilson-95% is
the correct interval for a binomial proportion, so the Wilson-lo-vs-permutation-band95
comparison inherited from the 2026-06-18 method is **correctly typed** (no Jaccard/Rand
set-overlap index is used, which would have mis-typed it). Confirmed before inheriting the method.

## Report-path witness (settles OQ-53's (a)/(b) close) — SURPRISES THE PLAN

The OQ-53 card names three conflation-locus files. Code-read witness:

| locus | kernel a queryable object? | evidence |
|---|---|---|
| `cs_kernel_registry.pl` | **first-class** | `cs_readings_for_kernel/2`, `cs_kernel_obstruction/4` |
| `json_report.pl` | **first-class** | enumerates `narrative_ontology:cs_kernel_id(_,K)`, calls `cs_readings_for_kernel(K,Rs)`, emits `cs_kernel_comparison` array (C3, l.1681-1690) + `cs_kernel_divergence_count` (B3, l.1625-1646) |
| `logical_fingerprint.pl` | **prefix-opaque** | 0 kernel mentions; the fingerprint is per-reading by design |

The plan expected the report path to be prefix-opaque (⇒ (b) close). The witness shows the
report path is **first-class** in its kernel-comparison block; only the *fingerprint* path is
prefix-opaque. Per the plan's own rule ("if the witness surprises and the report path does
enumerate kernels, upgrade to (a); the claim follows the witness"), the eventual OQ-53
same-kernel close is **the split actually witnessed**: first-class in registry + report,
prefix-opaque only in `logical_fingerprint` (where a per-reading fingerprint kernel-blind is
arguably correct — a fingerprint is *of a reading*). **Owed before the close is ruled:** a
firing-witness that `cs_kernel_comparison` is non-empty on the twin corpus (code-path existence
≠ fires; Build Discipline Pattern 5). Deferred to execution.

## Carry-forward

- Phase 1 runs the cross-twin membership-agreement test on the JSON-computable keys (below),
  pre-registered in `PRE_REGISTRATION.md`.
- Two keys (`cs_axiom_grounding`, `cs_kernel_obstruction`) deferred to a Prolog probe (Phase 1b),
  cost-gated on the Phase 1 menu.
- The differing-commit confound and the `cs_kernel_comparison` firing-witness are open owes.
