# OQ-152 — per-seat naturalization-collapse cross-section: answered with a negative

**Execution date:** 2026-07-24. **Corpus:** live `testsets/` (N=199), plus twins
`testsets_haiku`/`testsets_flash` (N=960 each) for the 634-reconciliation. **Code state:** `main`
at session HEAD (dirty). All numbers here are pasted from the probe scripts in this directory,
run in-engine (`swipl -g "[stack], corpus_loader:load_all_testsets, ..."`).

## Question

OQ-152 asked for a commentary diagnostic: for each seat, the suppression level at which its
`naturalized` reading flips to a visible `snare`, with the **ordering** (beneficiary last) as the
verdict. Deliverable: a per-seat suppression-collapse *curve*.

## Verdict: disposed, answered-reframed (negative) — no instrument is buildable that reads corpus structure

Two independent grounds, both witnessed.

### 1. Suppression is a constraint-level GATE, not a seat dial (`verify.pl`, `pattern5.pl`)

- Seat χ is `constraint_indexing:extractiveness_for_agent_d/4` = `ε · f(d_eff) · σ(scope)` — **no
  suppression term**. Suppression enters only `classify_from_metrics/6` via
  `drl_core:get_raw_suppression(C, Supp)`, **keyed on the constraint C**, identical across every
  seat.
- Suppression-invariance sweep (fixed naturalized-profile ε=0.90, χ=0.20): `naturalized` at supp ∈
  {0.0 … 0.95} — **invariant**. The `naturalized` clause ignores `Supp` (`drl_core.pl:422`).
- Gate witness (fixed ε=0.90, χ=0.70): `unknown` for supp ≤ 0.59, `snare` for supp ≥ 0.60 — a
  **step at the snare floor**, not a continuous dial. So no per-seat suppression ordering exists;
  a per-seat suppression-collapse curve is not substrate-computable.
- Coverage (not the gate, but recorded): 181/199 authored numeric, 18 `unknown`; 82 ≥ snare floor.
  The real domain where a "collapse threshold" is even *defined* (supp≥0.60 ∧ naturalized seat ∧
  ε≥0.46) = **68**, not 199.

### 2. Seat-χ ordering is fixed by `role→d` config; the corpus cannot author a departure (`probeD2.pl`, `probeE.pl`, `verify.pl`)

- `role→d`: agenda_setter 0.12 < beneficiary 0.25 ≪ observer 0.72 < payer 0.85 < excluded 0.90.
- Live displacement is **zero in both profiles**: `cognitive_displacement=0.0` (uniform) and every
  `positional_displacement=0.0`. So `D_eff = d`, and the authored **`agent_power` atom is inert
  for seat χ** (positive control: identical χ=0.442 at power=powerless vs institutional, fixed
  D/scope). The only per-seat channels are `exit_modulation ∈ [−0.03,+0.05]` and
  `scope_modifier ∈ [0.8,1.2]`.
- `sigmoid_f` is affine-scaled to **[−0.20, 1.50]** (`f(D)=−0.20+1.70·logistic(D)`), zero-crossing
  at D≈0.164. Engine values: f(0.30)=0.1935, f(0.69)=1.0881, f(0.09)=−0.0662.
- **Bridgeability bound**: most-favorable high-d seat mult f(0.69)·0.8 = 0.870 vs most-unfavorable
  beneficiary f(0.30)·1.2 = 0.232 — **3.75× apart, disjoint**. exit(±0.05)/scope([0.8,1.2]) cannot
  bridge the sigmoid gap across d∈[0.30,0.69]. Empirical confirmation: **0/158** within-constraint
  crossings (no payer/excluded/observer seat ever below its constraint's beneficiary). Per-constraint
  min-χ seat is always agenda_setter (128) or beneficiary (30), never a high-d role.
- **Sign structure has no missing term** (`verify.pl` decomposition, matched to 6 decimals):
  agenda_setter χ<0 because f(0.09)=−0.066<0; e.g. `ε=0.85 · f(0.09)=−0.066 · σ=1.2 = −0.0675 =
  chi_for_stakeholder`. ε range corpus-wide [0.03, 0.95], all ≤1.

### The substrate finding (what the leg actually produced)

The `naturalized` (cover-story) reading concentrates on the **beneficiary seat by band membership,
not by lowest-χ**. Modal case (65/128 agenda-min constraints): agenda_setter reads **rope** (χ<0,
below the cover story), beneficiary reads **naturalized**. This *refines* SEAT_SWEEP's "beneficiary
is the cover-story seat" to *band membership*.

The 12/158 stratum where agenda_setter *out-naturalizes* the beneficiary is **not** seat-relative
(`subfloor2.pl`): both seats sit far below the 0.35 χ-gate; in **12/12** the agenda_setter's own
metric type is `naturalized` and `agenda d (0.12/0.14) < beneficiary d (0.22–0.29)` with
`agenda χ < beneficiary χ` — role→d ordering inside the naturalized band, **0 genuinely
seat-relative**. Which sub-floor seat reads `naturalized`, and their order, is decided by ε + role→d
+ cascade routing (the rope clause's immutability/emergence conjunct failing lets a χ<0 seat fall
through to `naturalized`), **not by seat structure**.

### Revival class (what a future reviver needs)

The foreclosure holds **under the baseline calibration** and is revivable **only by a config-level
change, never a corpus authoring pass**: (i) a positional-δ calibration (`positional_displacement`
nonzero — would also make `agent_power` live for seat χ), (ii) the `role→d` map itself, (iii) the
`exit_modulation`/`scope_modifier` ranges. All three are config, not per-story authored fields.

## Reconciliations

- **634 → 176**: corpus size. Twins beneficiary-naturalized readings: haiku 644/960, flash 566/960
  (SEAT_SWEEP `8126231` reported 634; since drifted). testsets 176/199.
- **Rate 0.88 (testsets, 176/199) vs 0.67 (haiku, 644/960)**: an ε/authoring-distribution
  difference across generation legs — flagged, not resolved.
- **317 was mislabeled** in an interim probe: it counted (constraint × beneficiary-seat) pairs, not
  constraints. Corrected: 158 distinct constraints with a beneficiary seat + ≥2 agent seats.

## Downstream

- **OQ-153**: husk condition (3) must key on `naturalized`/FNL **band membership at any seat**, with
  the seat recorded as an attribute, not as the test — because seat identity carries no information
  here (witnessed 12/12 config). Recording "the beneficiary seat naturalizes" as if observed would
  inherit the mislabel.
- **design_gaps.md**: new line — `agent_power` inert for seat χ under δ=0 (its own gap, reach past
  OQ-152).
