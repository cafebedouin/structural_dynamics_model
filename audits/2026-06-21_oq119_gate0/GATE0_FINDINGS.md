# OQ-119 Gate 0 — three-axis substrate re-witness (NO SPEND)

**Date:** 2026-06-21. **Corpus:** `testsets_haiku` (960 stories, load witnessed:
`[corpus] Loaded 960 testsets successfully.`, `corpus_constraint/1 = 960` — NOT the silent-44
overlay trap). **Verdict: GATE 0 CLEARS DECISIVELY. Proceed to Phase 1/2; do NOT take the
fall-through.**

All counts below are witnessed against engine queries (artifacts in this directory), not greps
over files. The plan's binding fear — that the multi-reading (b) and temporal (c) populations
could be disjoint, leaving an empty joint cell — is falsified.

## What the staleness finding got right, and the one correction

OQ-119's blocker text (filed 2026-06-13) was pinned to the 5-kernel OQ-117 probe corpus and is
**stale**. On the `testsets_haiku` twin:
- **observer axis** — live (was already conceded live).
- **axiom/committer axis** — the OQ-119 text says it "no-opped (`cs_kernel_id` absent)." **Now
  327/328 multi-reading kernels carry a typed obstruction status** (220 real_closure, 107
  licensed_plurality, 1 untyped). Discharged.
- **temporal axis** — OQ-119 cites OQ-93 (grid) and OQ-33 (fabricated suppression) as live
  blockers. **Both are resolved (2026-06-11).** The `Backed` provenance bit now distinguishes
  authored from defaulted snapshots, and it is reachable in both states (control below).

**Correction to the plan's own framing:** the plan's axis (c) is "authored `coercion_grid` OR a
≥2-timepoint series." `coercion_grid` is **not an engine predicate** — it is a prose convention
in `$comment`/description fields (8 files mention it, all in prose; 0 authored facts). The real
authored temporal substrate is `narrative_ontology:measurement/5`. Measuring (c) by the prose
token would have undercounted the temporal axis ~100×. The temporal axis is read off
`measurement/5` series + the `classify_at_time/5` `Backed` bit, as the engine does.

## G0.0 — roster set query (the early-exit gate)

`probe_oq119_gate0_roster.pl` → `g00_roster_output.txt`:

| population | count |
|---|---|
| distinct `cs_kernel_id` | 331 |
| (b) multi-reading kernels (≥2 readings) | 328 |
| (b) ∧ axiom non-vacuous (real_closure / licensed_plurality) | 327 |
| **JOINT CELL: (b) ∧ axiom-live ∧ ≥2 Backed-temporal readings** | **325** |
| looser cell (≥1 Backed-temporal reading) | 327 |

**Kill condition was "< ~3 kernels clearing all three." We have 325.** The (b)∧(c) overlap the
plan flagged as the binding unknown is near-total: virtually every multi-reading kernel also
carries Backed temporal series on ≥2 of its readings.

## G0.1 — observer axis (`g01_g02_g03_controls_output.txt`)

4-seat χ spreads are non-degenerate on every sampled roster story (the four values are not all
equal), e.g.:
- `acceptable_risk_energy__expected_value_dominant`: powerless 0.354 / moderate 0.752 /
  institutional 0.242 / analytical 0.932.
- `westphalia_sovereignty__absolute_non_intervention`: 0.406 / 0.863 / 0.014 / 1.069.

**No-fabrication justification (two-sided-or-justify):** `extractiveness_for_agent` returns
`null`/fail on a seat it cannot compute (json_report.pl:934–941) — no 0.5 floor on the observer
path. Witnessed: a thin scalar-only story still yields four *distinct* seats from its authored
scalar ε, so the spread is a real engine computation, not a defaulted constant.

**Stakeholder caveat (raised in-session — honest limitation, not a gate failure):**
`stakeholder_seats:power_witness_map/2` (OQ-108: authored named stakeholders per power atom)
shows many roster stories author **0 stakeholders at the observer power atoms** (e.g.
`acceptable_risk` and `westphalia` are all-zero; `woman_category` partial: powerless 2 / organized
3 / institutional 3). Per OQ-108 semantics a 0 means that seat's χ is **inference-only** —
computed from the constraint-level ε projected across power atoms via directionality/context, not
from a per-power authored stakeholder. The observer spread is therefore non-vacuous **as an engine
computation** (it varies with context, which is what OQ-119's join reads), but its per-seat
*authoring* is sparse. This bounds what the fed-vs-withheld observer diff can attribute: a moved
seat reflects a moved ε/directionality, not a re-authored stakeholder. Recorded for Phase 2.

## G0.2 — axiom/committer axis

Readings genuinely diverge, not merely co-stamped: `cs_kernel_divergence` fires for
`acceptable_risk_energy` (readings classify differently at national scope:
`__expected_value_dominant` vs `__catastrophic_tail_dominant`). **No-fabrication justification:**
`cs_kernel_obstruction_status` is FAIL-CLOSED on absence — a multi-reading kernel with no typed
edge is `untyped`, never silently `glued` (cs_kernel_registry.pl:113). Agreement cannot be
fabricated by absence.

## G0.3 — temporal axis (THE positive control)

`g01_g02_g03_controls_output.txt` + `g03_backed_reachable_control.txt`. The probe **separates
known-rich from known-thin** — the byte-identical-read guard:

| story | base_extr. series | Backed timepoints | `reading_temporal_nonvacuous` |
|---|---|---|---|
| `acceptable_risk_energy__expected_value_dominant` (rich) | 6 | 6 | **pass** |
| `digital_money…__consumer_holdings_reading` (thin, scalar-only) | 0 | 0 | **fail** |
| `monopoly_rulebook__social_scaffold_reading` (thin) | 0 | 0 | **fail** |

**Backed-bit reachability positive control** (guards against a vacuously-always-true probe —
Pattern 5): `classify_at_time/5` returns
- THIN `digital_money…@t0` → `snap(0.72, false, 0.5, 0.41, none)` — **Backed=false**, the ε=0.5
  default flagged.
- RICH `acceptable_risk…@t0` → `snap(0.72, true, 0.52, 0.68, 0.28)` — **Backed=true**, ε authored.

So "no series-present-but-Backed=false story found in the twin" is a genuine property of the haiku
authoring discipline (suppression co-authored with ε on every series), not a probe that cannot
report false.

## Phase 1 — comparator validated (`phase1_comparator_controls.txt`)

`python/audits/oq119_join_diff.py` over the four exported join records
(`join_records.json`, emitted by `prolog/export_oq119_join_records.pl` — all three axes
populated, incl. `verdict_joined`/`sig_grade`/alert set):
- **negative control (self-diff)**: scalar = 0 on every axis, every record. PASS.
- **positive control (cross-kernel)**: westphalia vs woman_category → scalar 0.629, moved fields
  named (4 observer seats + 3 temporal metrics). PASS.
- **within-kernel sensitivity** (the fed-vs-withheld scale, Phase 2's noise-floor concern): two
  readings of `acceptable_risk_energy` → scalar 0.274, **resolvable (>0)**, driven by the
  institutional-seat divergence + temporal rates.
- **synthetic micro-perturbation**: distance tracks a known Δχ linearly to ~5e-4 — the
  comparator's numerical floor is ~0.

**The binding floor is the substrate redraw floor** (withheld-vs-withheld generation variance,
OQ-26), which is NOT established here because it needs spend. Phase 2's `PREDICTION.md` defines the
discrimination threshold relative to that measured floor and mandates ≥3 withheld redraws per story
to establish it.

## Disposition

- Gate 0 open → Phases 1 (comparator) and 2 (frozen prediction) complete; both witnessed.
- The Ω_E question (does feeding move the join?) **stays OPEN** — it is answerable only by the
  pre-registered spend, which **STOPS at the operator's spend-go** (`PREDICTION.md`).
- OQ-119 updated: blocker re-scoped from the stale "blocked on three-axis-instrumented corpus" to
  "substrate witnessed open; blocked on spend-go." Left **open**, not resolved.

## Artifacts

- `g00_roster_output.txt` — roster + joint-cell counts + status distribution.
- `g01_g02_g03_controls_output.txt` — observer / axiom / temporal two-sided controls.
- `g03_backed_reachable_control.txt` — Backed-bit reachability positive control.
- `join_records.json` — exported three-axis join records (withheld-arm shape).
- `phase1_comparator_controls.txt` — comparator self/cross/within-kernel/micro controls.
- `PREDICTION.md` — FROZEN pre-registration + spend spec (DO NOT RUN).
- probes: `prolog/probe_oq119_gate0_roster.pl`, `probe_oq119_gate0_controls.pl`,
  `probe_oq119_backed_reachable.pl`, `export_oq119_join_records.pl`;
  comparator: `python/audits/oq119_join_diff.py`.
