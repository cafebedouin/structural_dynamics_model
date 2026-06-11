# External-review triage — XPrize / rejuvenation run (2026-06-10)

Second instance of the ongoing external-review practice (see the vote-market audit dir for the
convention). A model reviewed the three constraint reports from the David Sinclair / XPrize
Healthspan reprogramming essay run; six critiques returned. Each verified against the reports,
the engine code, and the source article (`agent/analysis/originals/rejuvenation_drugs.md`)
before any disposition — external output is hypothesis, not evidence.

- Raw feedback: `raw_feedback.md` (verbatim).
- Report evidence: `reports/` (copies of the three 2026-06-10 reports the witnesses cite;
  `outputs/` is gitignored, so these copies are the stable substrate).
- Run-outputs commit: **`96113b05`** (XPrize three testsets/json + essay + source on main).
- Gate witness: shared with the vote-market audit dir
  (`../2026-06-10_external_review_vote_market/gate_witness.txt`) — one 48-constraint corpus,
  suite GREEN exit 0.

## The load-bearing outcome: cut the auto-synthesis step

Operator ruling (2026-06-10): **remove orchestrator step 6 (the Sonnet essay draft); replace it
with a deterministic, non-generative tensions ledger** (filed as OQ-101). Rationale, in force
order:

1. **The essay form collapses plurality — that is its function.** The reports preserve plurality
   (per-position types, index mismatches, signature+grade, omegas, drift, contamination, each
   caveated); an essay advances one thesis. The auto-essay literally announced the collapse:
   *"converges on a single structural conclusion"* (`sinclair_xprize_reprogramming_2026.md:256`).
   The project exists to hold perspectives open without collapsing — the synthesis step is
   structurally opposed to the goal.
2. **Form, not implementation.** The operator previously used `uke_think`; it over-stated the
   same way. Invariant under synthesizer swap ⇒ the *form* is the defect; prompt guidance cannot
   fix a form whose job is to collapse. (This retires the "synthesis-fidelity prompt-guidance"
   candidate.)
3. **The draft is throwaway and harmful as a starting point.** Final synthesis is redone live
   with a model; starting from a pre-collapsed, over-confident draft anchors toward collapse.
4. **Run sizes (3–11 constraints, avg 5–6) justify a replacement, not bare reports** — reading
   N × ~360-line reports cold each run is expensive; the ledger earns its keep and absorbs
   OQ-100(d)'s distillation need.

### Live-synthesis checklist (replaces the synthesis-fidelity OQ; apply when synthesizing from the ledger)

Cutting step 6 **relocates** the over-statement risk into the live conversation (human in the
loop), it does not eliminate it. When synthesizing:

- **Carry every caveat the engine attached** — strength, confidence, edge-type, provenance,
  coverage. A `confidence: low` or a `0.30` edge stays low/small in the prose.
- **Do not borrow a corpus-topology neighbor as if the case sourced it.** A `shared_beneficiary`
  edge is the corpus's story until the article supplies the link.
- **Do not inflate a sub-salience signal** (e.g. a 0.30 contamination edge, Δpurity −0.05) into
  a narrative section.
- **Do not render an authored trajectory as a measured event.** A two-point ε guess is a guess,
  not a time series.
- **Hold the plurality.** Per-position disagreement is the product; resist "converges on a single
  conclusion."

## Triage table (claim → verified witness → disposition)

| # | Critique | Verified witness | Disposition |
|---|----------|------------------|-------------|
| 1 | "Precision laundering" — hand-declared metrics reported to 3 decimals over a 0-authored grid; engine performs the `false_ci_rope` move it detects | `[PROVENANCE] grid 32 = authored 0` (reprogramming:23), `Kappa: DATA_INSUFFICIENT` (:316), severity `0.531` (competition:149) | **Already covered** — OQ-98 spine + OQ-93/OQ-92. One-sentence framing note into OQ-98; no new OQ. |
| 2 | Drift cascade = authored guesses rendered as critical events; scariest numbers airiest | `evidence(extraction_delta,0,6,0.35,0.58)` tagged `[critical]` (competition:64-65); terminal pred `confidence: low` (:165); +66%, no series | **OQ-102** (drift-series provenance + the "critical"-outranks-"low" inversion). |
| 3 | Contamination edge imported from corpus topology, not the story | `digital_colonialism_data_extraction \| shared_beneficiary \| 0.30`, Δpurity −0.0554 (reprogramming:136); **not authored** in the testset (grep: no `affects_constraint`); essay inflates to a biobank narrative (`:176-180`) absent from source | **OQ-103** (narrowed: edge provenance + salience floor). Proximate inflator is the synthesis step → OQ-101 + checklist; engine sub-component is the missing provenance/salience bit. |
| 4 | "Snare, right verdict wrong wire" — coercion is right-to-try desperation in subjects, not prize-pressure on teams | engine snare computed from authored `prize_pressure`/`no_exit`; essay itself surfaces "right to try" (`:143`) | **Decline as engine OQ** — authoring/synthesis fidelity. Engine angle (who-bears vs who-benefits) cross-refs **OQ-94**. |
| 5 | Gladyshev-as-mountain-anchor oversells; source hands you the rope read | source quote "trying to improve the combinations makes sense" (`rejuvenation_drugs.md:78`); essay concedes rope at `:25` | **Decline** — essay-synthesis framing; **moot** once step 6 is cut. |

Shared root of 1/3/4/5: the synthesis step strips the caveats the engine attached and re-asserts
thin/low-confidence/authored signals as confident prose. Cutting step 6 + the checklist is the
lever; OQ-102 and OQ-103 carry the residual engine-side work.

## Protect list (what the reviewer flagged as working — do not break)

- The reports' own honesty about provenance (the `[PROVENANCE]`/`grid diet`/`Kappa:
  DATA_INSUFFICIENT` lines) — the engine *does* say plainly that it didn't author the grid.
- The per-position plurality (index mismatches, orbit signatures, H¹ bands) — the structure the
  essay then flattened.
- The engine's own caveats (`confidence: low`, edge strength `0.30`) — present and correct; it
  is the synthesis step that discards them.
