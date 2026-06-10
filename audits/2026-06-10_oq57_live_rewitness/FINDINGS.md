# OQ-57 re-witness — live + archive corpora (2026-06-10)

**Trigger.** OQ-57 was marked resolved 2026-06-04 (qualifier fix at `drift_events.pl:236`,
`narrative_ontology:` → `domain_priors:requires_active_enforcement/1`). All three behavioral
witnesses in that entry were measured on the **pre-reset** corpus; the corpus was RESET
2026-06-05, so those witnesses describe constraints that no longer exist. The code fix is in
source (durable), but the clause it fixes (`internalized_piton`) only *throws when reached*
(`extractiveness<0.10 AND theater_ratio>0.70`) — a Pattern-5 risk: "no throw" can mean
"never reached," not "exercised clean." This re-witnesses the resolution.

## Verdict: RESOLVED — confirmed, with a corpus-independent witness added.

The fix is durable, load-bearing, and fires clean on every constraint that reaches it. The 2026-06-04
behavioral witness is **reproduced** (not merely re-asserted) on the archive where it was measured.

## Evidence

| corpus | N (`corpus_constraint`) | reaches piton guard | `drift_event/3` on those | full-scan |
|---|---|---|---|---|
| live `testsets` (post-reset) | 39 | 0 (unexercised) | — | no throw, 4 drift events |
| `archives/datasets/kernel_v1` | 1,106 | **2** | both **CLEAN** | no throw, 4,473 events |
| `archives/datasets/original_v6` | 3,380 | 0 (unexercised) | — | no throw, 13,731 events |
| synthetic positive control | 1 | 1 (constructed) | **CLEAN** | — |

**kernel_v1 reaching set reproduces the original witness exactly:**
- `kodashim_obligation__memorial_archival` (eps=0.08, theater=0.85) → `evidence(extraction,0.08,theater,0.85)`
  — byte-identical to the value recorded in the 2026-06-04 ISSUES.md entry.
- `statutory_debt_ceiling__constitutional_nullity_reading` (eps=0.00, theater=0.95) → `evidence(extraction,0.0,theater,0.95)`.

**Synthetic positive control** (corpus-independent): a constructed constraint with eps=0.05,
theater=0.85 and no enforcement fact drives the fixed clause; it returns
`evidence(extraction,0.05,theater,0.85)` with no throw. This proves the clause fires when reached,
independent of whether any given corpus reaches it — the witness the original entry lacked.

**Diagnostic positive control** (proves the probe distinguishes throw from clean):
- `domain_priors:requires_active_enforcement/1` (FIXED qualifier) → `succeeded`.
- `narrative_ontology:requires_active_enforcement/1` (PRE-FIX qualifier) →
  `threw(error(existence_error(procedure,narrative_ontology:requires_active_enforcement/1),...))`.
  The wrong qualifier still throws, so the qualifier change is load-bearing and the probe is not
  vacuously clean.

**Suite:** `run_dynamic_suite` on the live corpus → Passed 39, Failed 0, Errors 0.

## Note carried forward

On the live (39-constraint) corpus the `internalized_piton` clause is currently **unreached** —
correct-but-dormant. As the rebuild grows, a low-extraction/high-theater constraint will reach it
again; the fix holds (kernel_v1 + synthetic prove it). No action needed; recorded so a future
"no drift throw on live corpus" read is not mistaken for "exercised."

## Artifacts (this dir)
- `oq57_rewitness_probe.pl` / `rewitness_probe.out` — live corpus reachability + symptom + diagnostic control
- `oq57_synthetic_positive_control.pl` / `synthetic_positive_control.out` — corpus-independent fire-when-reached
- `oq57_archive_rewitness.pl` — parametric archive probe (OQ57_CORPUS env)
- `archive_kernel_v1.out` / `archive_original_v6.out` — archive runs
- `run_dynamic_suite_summary.out` — suite clean-completion
