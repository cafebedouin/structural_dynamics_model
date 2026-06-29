# OQ-22 Hub-1/Hub-2 starvation — findings (evidence-first); VERDICT B

**Audit dir:** `audits/2026-06-28_oq22_hub_starvation/`
**Execution date:** 2026-06-28
**Engine commit (testsets manifest):** `bbbf2c6` (`outputs/pipeline_output.json` `pipeline_run_at`
2026-06-27T21:04:40Z, n=109)
**Probe:** `python/audits/oq22_starvation_census.py` (census) + `oq22_analyze.py` (analysis) +
`oq22_controls.py` (Phase-3 positive controls) + `oq22_grid.py` (Phase-2 grid). Read-only w.r.t.
engine + corpus.
**Status:** ALL PHASES COMPLETE. **Verdict B** — starvation occurs under the default sigmoid; the
starved subset (a) is grid-witnessed Hub-2-sourced (see Phase 2). Resolution landed in ISSUES.md
OQ-22 (resolved) + follow-up OQ-192 (provenance field).
**Regenerating evidence:** the lean verdict-encoding TSVs (`base_`/`obs_`/`census_`/`grid_`),
`summary.json`, the hash files, and `injected_starved_01.pl` are committed. The bulky per-χ-sweep raw
(`bands_*.tsv`, `gateown_*.tsv`, ~8.8M) is regenerable and NOT committed — rerun
`python3 python/audits/oq22_starvation_census.py` (census + bands + gateown) and `oq22_grid.py` (grid).

---

## Substrate (Phase 0, read against code, not docs)

Cascade = `drl_core.pl:classify_from_metrics/6` (`:327–422`); χ gates only. Call path
`metric_based_type_indexed/3` (`:479–483`): of the four non-`C` inputs, **only `Chi`
(`extractiveness_for_agent/3`, the sigmoid Hub 1) and `Context` vary across observers**; `BaseEps`
(`base_extractiveness/2`) and `Supp` (`get_raw_suppression/2`) are observer-INVARIANT. So the
χ→type map branches on observer only through `Context` — i.e. through Hub 2
(`effective_immutability_for_context/2`, `constraint_indexing.pl:195–234`) and the cross-stalk
`snare_immutability_check/1`.

Hub-2 IS observer-indexed (the 4 `standard_context` positions co-vary time_horizon/exit_options,
`drl_core.pl:744–762`): powerless=biographical/trapped→**mountain**, moderate=biographical/mobile→
rope, institutional=generational/arbitrage→rope, analytical=civilizational/analytical→**mountain∧
rope** (first solution mountain). So subset (a) is NOT empty by construction.

**Doc-vs-code discrepancy (fix at writeup):** the two-hub comment (`drl_core.pl:205`) says "Mountain
gate: requires BOTH low χ (Hub 1) AND immutability = mountain (Hub 2)". The `mountain` clause
(`:330–336`) ignores `_Chi` entirely — it gates on suppression, ε, `emerges_naturally`, immutability
only. Mountain is a **pure Hub-2 type** (no χ gate). This is exactly why the clearest subset-(a)
cases flip mountain↔scaffold/rope on immutability with χ irrelevant.

**Gate inequality convention (discovered by boundary tests, not recalled):** realized band cutpoints
fall AT the config values {0.0, 0.35, 0.45, 0.66, 0.90} (0 boundaries off-config, all four legs). But
which TYPE owns each value is constraint-specific — see kill condition.

**Knife-edge seam (genuine gate geometry):** χ=0.35 is owned by `rope` only if rope's non-χ gates
pass; otherwise it is a **measure-zero `unknown` point** — `naturalized` (χ<0.35 strict) and
`tangled_rope` (χ>0.35 strict, OQ-37) both exclude 0.35. 216/2492/1912/2928 such single-grid `unknown`
points across the legs; all type `unknown`. Reported as geometry, excluded from the band-width floor.

---

## CRITICAL kill condition — ANSWERED: per-constraint map is mandatory

Plan §"the type-band is not the config-gate partition" kill condition: *prove the cascade maps the
four thresholds to four non-overlapping types.* It does NOT. Gate ownership (type the cascade returns
with χ placed exactly on each gate value), testsets / all legs concur:

| χ | types owning it (count, testsets) |
|---|---|
| 0.45 | tangled_rope 228, scaffold 138, mountain 6, unknown 32 |
| 0.66 | snare 220, tangled_rope 28, mountain 6, unknown 150 |

A single config threshold maps to 4–5 different types depending on the constraint's non-χ gates →
the config partition is **inadmissible** as the starvation partition. `starved` is therefore derived
from the realized per-(constraint,observer) χ→type map (swept here), exactly as the plan required.

---

## OUTPUT 1 — analytic boundary (gate geometry, scoped to swept configs)

Min NON-degenerate single-type band width over swept (constraint, observer) maps = **0.099**
(haiku/flash/kernel_v1; testsets 0.208) — the `piton` band [0.351, 0.45]. This is the floor of
Hub-1's per-step dynamic range *for the configurations actually swept* — NOT a floor over the full
reachable non-χ input space (that is combinatorial and not derived). The plan's flagged naive
"min-adjacent-gap 0.10" is ≈ this best case, but realized bands run far wider per constraint (a
constraint whose intermediate clauses don't fire has a band spanning multiple config gaps, e.g. a
single `scaffold` band of width 0.45+), which is why the screen is per-constraint.

---

## χ-path BRIDGE (Phase-1 prerequisite) — HOLDS

Re-probe χ on `testsets` vs `pipeline_output.json` `perspective_chi`: 404/404 defined observer cells
equal (worst |Δ| = 4.99e-7 = JSON 6-place rounding), 0 differ, 32 both-undefined (the 8 fail-closed
constraints × 4 obs), 0 final-type mismatches. The re-probe χ path is faithful to the cached pipeline;
the other three legs' recomputed χ is therefore trustworthy on the same code path.

---

## OUTPUT 2 — empirical census (band SCREEN; Phase 2 is the arbiter)

| corpus | N | starved | (a) type-varies (Hub-2-sourced?) | (b) fixed @ Hub-2-gated | contrast (non-starved & type-varies) |
|---|---|---|---|---|---|
| testsets | 109 | 26 | 5 | 0 | 75 |
| testsets_haiku | 960 | 148 | 23 | 7 | 812 |
| testsets_flash | 960 | 233 | 100 | 7 | 727 |
| kernel_v1 | 1106 | 145 | 49 | 0 | 961 |

(8 testsets constraints have no full 4-observer map — fail-closed ε/supp — excluded, not counted
starved.) Per-constraint detail: `census_<corpus>.tsv`; machine summary: `summary.json`.

**Reading:** starvation is present under the DEFAULT sigmoid on every leg → **Verdict B is indicated,
not Verdict A.** The screen DISCRIMINATES: the large `contrast` column (non-starved constraints whose
type varies — normal two-hub, Hub-1 has range) is ~7–20× the starved count, so the screen is not
flagging everything. Clearest (a) shape: `actinide_replenishment_mechanism_flat_control` (testsets)
χ-span 0.0424, mtypes [mountain, scaffold, scaffold, mountain] — `mountain` ignores χ, so the
variation is purely immutability/Hub-2. subset (b)=0 in testsets/kernel_v1, 7 in each twin.

---

## PHASE 3 — positive controls (the probe must find what it reports absent)

**Global** (compressed transform → widespread starvation), testsets:
default **26** → sigmoid-only [f∈[0.48,0.50]] **46** → sigmoid+σ-flat [σ≡1.0] **101/109** (near-total).
χ ceiling collapsed 1.3014→0.4735 (overlay took effect, capped by U=0.5·σ=1.0). Counts strictly
increase with compression depth → the probe responds to compression and flags widespread starvation.
**Finding:** sigmoid-only leaves a residual σ-driven cross-observer span — **χ = ε·f(d)·σ(scope) has
TWO Hub-1 span sources**, f(d) (power/displacement) and σ(scope) (observers span local 0.8/national
1.0/global 1.2); compressing the sigmoid alone does not starve them. **Finding:** the originally-
witnessed extreme regime (χ ceiling 0.15) is now **VALIDATOR-FORBIDDEN** (`config_schema.pl:74`
`sigmoid_upper∈[0.5,3.0]` + ordering `L<midpoint<U`) — the engine's own config gate already forecloses
the most degenerate starvation; it was reachable in the May-2026 T2 run before/around that guard.

**Single** (one constructed starved member in a healthy bed): an ε-pinned (0.02) copy of a real
testset injected into 10 real non-starved testsets → χ∈[−0.0008, 0.0274] (span 0.0282), flagged
starved=True and individually resolved; healthy bed 0/10 starved. The probe resolves a single planted
member, not just a collapsed population.

Both controls PASS → the default-transform census above is licensed.

---

## What Phase 2 must still do (held at manual-approve gate)

The (a)/(b) members above are SCREEN candidates. Phase 2 builds the (observer × immutability) grid via
`probe_harness:with_overlay`, overriding `effective_immutability_for_context/2` OUTPUT across ALL
observers, and:
- **(a)** pin immutability constant while observer varies — if the cross-observer type variation
  VANISHES, it was Hub-2's (witnesses (a)); if it PERSISTS, it was χ-sourced (Hub-1 via d or σ) →
  route to "Hub-1-sourced", NOT a false halt.
- **(b)** vary immutability at a fixed observer — if type TRACKS immutability, it is immutability-driven.
- **negative control** — a cell invariant across the immutability axis is Hub-1/χ-sourced.

Only members carrying a Phase-2 grid witness may be enumerated in the ISSUES.md resolution.

---

## PHASE 2 — (observer × immutability) grid (COMPLETE; `oq22_grid.py`, `grid_<corpus>.tsv`)

**Observer-indexing witnessed (operator finding 2), not inferred:** the immutability INPUT to the
cascade is observer-indexed by construction — the 4 `standard_context` positions co-vary
(time_horizon, exit_options): powerless=(biographical,trapped)→mountain, moderate=(biographical,
mobile)→rope, institutional=(generational,arbitrage)→rope, analytical=(civilizational,analytical)→
mountain∧rope (`effective_immutability/3`, `constraint_indexing.pl:195–234`; observer table
`drl_core.pl:744–762`). So a constant-immutability pin must override across ALL observers (the grid
does, by constructing each observer's context with pinned (T,E)).

**Mechanism (no retract):** `effective_immutability/3` is a STATIC procedure — `with_overlay` cannot
pin it (`permission_error(modify,static_procedure)`). Inside `classify_from_metrics/6` the passed
Context controls ONLY the immutability checks (χ is passed in; `snare_immutability_check` reads
`standard_context` independently). So the grid computes each observer's NATURAL χ once, then calls
`classify_from_metrics` with a context keeping the observer's power+scope (natural χ) but pinning
(T,E) to force immutability: mountain←(immediate,trapped), rope←(immediate,mobile). The NATURAL cell
(observer's real T,E) reproduces the Phase-1 metric type — **diagonal CONSISTENT on all four legs**
(grid self-consistency).

**Grid self-control (operator finding 3) — PASSES.** The a-priori (a) members are the
mountain↔scaffold flippers (`mountain` ignores χ → the flip CANNOT be Hub-1). All three testsets
flippers collapse under the pin: nat=`mountain|scaffold|scaffold|mountain`, pinMountain=
`mountain|mountain|mountain|mountain`, pinRope=`scaffold|scaffold|scaffold|scaffold` — variation
vanishes under EITHER constant pin → Hub-2-sourced. The grid collapses the population whose
Hub-2-sourcing is most certain, so it is trusted on the marginals.

| corpus | screen (a) | **grid-confirmed (a)** | reclassified→Hub-1 | grid-(b) | neg-control | persists→Hub-1 | diagonal |
|---|---|---|---|---|---|---|---|
| testsets | 5 | **5** | 0 | 61 | 96 | 75 | CONSISTENT |
| testsets_haiku | 23 | **23** | 0 | 666 | 937 | 812 | CONSISTENT |
| testsets_flash | 100 | **100** | 0 | 798 | 860 | 727 | CONSISTENT |
| kernel_v1 | 49 | **49** | 0 | 811 | 1057 | 961 | CONSISTENT |

- **(a)** every screen subset-(a) member is grid-witnessed Hub-2-sourced (cross-observer variation
  vanishes under the all-observer immutability pin); **zero** reclassified to Hub-1.
- **(b)** immutability-driven cells exist broadly (type tracks immutability at a fixed observer):
  61/666/798/811.
- **negative control** (cell invariant across the immutability axis = Hub-1/χ-sourced): 96/937/860/1057.
- **persists-under-pin → Hub-1-sourced (NOT a halt):** 75/812/727/961 — these EXACTLY match the
  Phase-1 `contrast` counts, cross-validating screen and grid; the false-halt guard routed persistence
  to Hub-1, never halt.

**Verdict: B.** Starvation is present under the default sigmoid on all four legs at the stamped
hashes (corpus_hash files in this dir), and every starved subset-(a) member is a Hub-2 decision
reported as a two-hub decision — the OQ-22 phenomenon, witnessed.

**Scope (operator ruling, Q2 option 1):** counts are per-generation, at the stamped hashes; NOT
claimed as expected prevalence across re-generations. **B kill condition (state, don't spend):** a
re-generation in which a flagged member STOPS starving would falsify the member, not the existence
claim (B is discharged by ≥1 witnessed instance; four legs witnessed).

---

## HELD for operator (write-pass, per Q1 option 2 — NOT auto-applied)

1. **mountain-ignores-χ: the direction is ALREADY RULED by the canonical spec — `docs/logic.md:644`.**
   The behavior is witnessed (mountain clause `drl_core.pl:330–336` has no χ gate). The operator
   flagged that calling it a "doc discrepancy" pre-encodes "code correct, doc stale", and whether
   mountain-ignores-χ is *intended* is the operator's ruling, not a measurement. **Resolution found
   during this pass:** `docs/logic.md:644` (the canonical spec) states verbatim "**Mountains don't
   check χ** … They use raw ε and Supp directly … If a constraint's burden varies dramatically by
   power, it's not a Mountain—it's constructed." So code AND canonical spec agree: mountain is a pure
   Hub-2 type by design. The ONLY artifact that disagrees is the inline two-hub comment
   `drl_core.pl:205` ("Mountain gate: requires BOTH low χ (Hub 1) AND immutability"), which forked
   from the canonical spec (Build-Discipline Pattern 2). This is NOT an engine-bug branch; it is a
   stale inline comment. **Still NOT auto-edited** (operator reserved the direction); recommended
   correction in the write-turn: align `:205` to logic.md (mountain = pure Hub-2, no χ gate). Bearing
   on (a): grid-(a) members whose nat-vector contains mountain — testsets 3/5, haiku 14/23, flash
   43/100, kernel_v1 41/49 — are "working as designed" (Hub-2 by spec), not a bug surfacing as Hub-2
   substitution.
2. **ISSUES.md OQ-22 resolution (hash-scoped Verdict B)** + **mint the follow-up provenance-field OQ**
   (a per-context "deciding hub" field; authored Priority/Deps) — held for the write-turn.
3. **logic.md analytic-boundary note (operator finding 4):** the 0.099 floor is *sigmoid-driven*
   Hub-1 disengagement; σ(scope) is a second, compression-immune span source, so the note must NOT
   claim a "Hub-1 disengagement" floor full stop. The census (on realized χ) already handles σ
   correctly; only the floor prose can slip.
