# Findings — kernel/reading orbit discovery (OQ-150)

Adjudication of `phase1_results.json` + the Phase-2 orthogonality probe against
`PRE_REGISTRATION.md`. Substrate: `outputs/pipeline_output.{haiku,flash}.json` (n=960 each,
classified at f3ec052 / 8126231 — differing-commit confound, see RECON). Deterministic Python,
no engine re-run; permutation null N=1000, seed 20260620.

## Positive controls (all pass — reported before the verdicts)

- `claimed_type` cross-model agreement = **0.7208** (≈ 0.721 baseline) — JSON read correctly.
- `cs_kernel_id` set identical across twins = **True** (331 kernels).
- **K1 reproduces the prior audit exactly:** kernel structure-signature membership agreement =
  **0.1341**, matching the 2026-06-18 M3 full-signature 0.134. The kernel-orbit machinery is
  wired correctly (independent cross-check on a different script).

## Elected filter vs under-operationalized gate (the method gap, named correctly)

The pre-registered rule (`Wilson-95 lo > permute band95`, non-degenerate) marks **all eight keys
"pass"** → the band95-keyed empty-menu kill **did not fire**. But that gate tests *"agreement
beats random label assignment,"* a different and weaker thing than the admissibility filter the
plan's **Context already elected**: *"draw-robustness is an Ω_E property I am electing to impose
as an admissibility filter on the seat"* — i.e. **reproducibility**, named up front. The
pre-registration then operationalized that election as `lo>band95` (beats-chance), under-serving
it. So applying reproducibility is **not a retroactive standard-switch**; it **honors the
pre-committed election** the gate under-operationalized. The band95 passes are real but answer a
question the plan didn't ask. The substantive readout is therefore the **absolute
membership-reproducibility ranking, judged against the extraction baseline (~0.721 — the
reproducibility of the substrate itself, the natural floor)**:

| key | agreement | Wilson-lo | band95 | beats-chance | reproducibility |
|---|---|---|---|---|---|
| **kernel-obstruction-class** (Phase 1b) | **0.734** | 0.684 | 0.589 | yes | **at baseline** |
| **R1** observer_signature | **0.722** | 0.693 | 0.476 | yes | **at baseline** |
| R4 terminal_observer | 0.566 | 0.534 | 0.348 | yes | below baseline (highest fragile) |
| R2 apparatus `cs_pattern` [axis 1] | 0.487 | 0.455 | 0.344 | yes | below baseline |
| R3 terminal_committer [axis 2] | 0.300 | 0.271 | 0.226 | yes | below baseline |
| axiom-grounding-profile (Phase 1b) | 0.272 | 0.245 | 0.107 | yes | below baseline |
| R5 seat_role_vector | 0.245 | 0.219 | 0.109 | yes | below baseline |
| K1 kernel structure-signature | 0.134 | 0.102 | 0.027 | yes | below baseline (very fragile) |

**This ranking IS the substantive Ω_E finding**, and it confirms the plan's pre-stated
expectations almost exactly:

1. **R1 (observer signature) is the only clearly draw-robust key** — its 0.722 agreement equals
   the `claimed_type` extraction layer (0.721). The observer axis reproduces across the redraw.
2. **The committer side is more fragile than the observer side** at every comparison: terminal
   committer R3 (0.300) ≪ terminal observer R4 (0.566); apparatus R2 (0.487) is mid-low. This
   is OQ-149 ("committer axis is the most model-divergent layer") reproduced on a fresh object
   (per-reading orbit membership, previously unmeasured).
3. **R2 (apparatus, operator axis 1) was predicted "most likely to fail"** the declarability
   bar — borne out: it clears the chance floor but ~half of readings change apparatus-orbit on
   redraw, so it is **not declarable as a stable vocabulary**. The substance: *"the commitment
   system you most want to group by is above-chance-structured but membership-model-relative"* —
   a real corpus finding, not a method failure. (Caveat held: OQ-149's 0.392 is
   `cs_reading_relation`, R2 here is `cs_pattern` — same family, now directly measured.)
4. **R5 (seat role-vector, the OQ-56 reading-stance unit) is membership-fragile (0.245).** A
   reading-stance taxonomy keyed directly on the 4-seat vector would be model-relative — the
   same conclusion the 2026-06-18 audit reached for kernel depth-vectors, now confirmed for the
   cross-kernel reading unit OQ-56 actually needs.
5. **Coarsening the committer signal RECOVERS draw-stability (the Phase 1b surprise).** The
   committer axis is not uniformly fragile: its fine signatures are model-relative
   (axiom-grounding-profile 0.272, apparatus 0.487), but its **coarse 4-way obstruction verdict
   reproduces at baseline (0.734)** — `real_closure` (66%) vs `licensed_plurality` (~32%) is a
   draw-stable kernel property even though the fine reading-relations that compute it are the
   most model-divergent layer (OQ-149). This is the key Decision-2 (run-the-probe) bought; the
   JSON pass would have missed it and the "same-family ⇒ fragile" inference mis-predicted it.
   **Granularity, not axis, is what governs declarability here.**

## Phase 2 — orthogonality (the mandated false-unification distinction-check)

Apparatus orbit (R2, `cs_pattern`, committer axis 1) vs observer orbit (R1, `signature`), haiku:

- Mutual information = **0.082 bits**; normalized MI (over min entropy) = **0.063**.
- Uncertainty coefficients: U(observer | apparatus) = 0.063, U(apparatus | observer) = 0.044.
- Cross-tab: every apparatus class maps to the same dominant observer signature
  (`constructed_high_extraction`, purity 0.35–1.00) — the observer axis carries essentially no
  apparatus information.

**Ruling (witnessed, per Theorem 7 / CLAUDE.md false-unification):** the commitment-apparatus
orbit is **gradient-orthogonal** to the observer orbit — a **genuine second axis**, NOT
redundant. Keep them separate; do not fold the committer axis into the observer one. This is the
in-file witness (not analogy) that the architecture's two-axis separation (v7 Axiom 7 / Theorem
7) is reflected in the corpus orbits.

## "Do kernels share readings?" (Phase 2 item 3)

Not by name (0 collisions — RRECON). The semantic-proximity readout (cross-kernel `reading_diff`
on same-orbit pairs) **must be restricted to draw-robust orbits** or the shared-invariant cells
become redraw-dependent. Given the reproducibility ranking, the only orbit robust enough to
condition on is **R1 (observer signature)** — so a defensible shared-invariant readout is
available only on the observer axis until/unless the operator rules a lower declarability cut
(below). Deferred to Phase 3, gated on that ruling.

## The two-tier menu (operator ruling 2026-06-20: cut at R1, present two-tier)

The cut sits at the extraction baseline (operator Decision 1). The menu is presented in two
tiers, labelled by the **measured property** so the labels do not smuggle the seat's answer:

- **Tier 1 — membership-reproducible at extraction baseline (~0.72):**
  - **R1 observer_signature** (0.722) — observer axis.
  - **kernel-obstruction-class** (0.734) — coarse committer-derived kernel verdict.
- **Tier 2 — above-chance but membership-fragile (below baseline; numbers attached):**
  - R4 terminal_observer (0.566, the highest fragile), R2 apparatus (0.487),
    R3 terminal_committer (0.300), axiom-grounding-profile (0.272), R5 seat_role_vector (0.245),
    K1 kernel structure-signature (0.134).

The band95-keyed empty-menu kill **did not fire** (Tier 1 cleared on reproducibility, not just
chance) — so we are correctly in the **present-and-escalate** branch. The fragile keys are the
**third state** the empty/non-empty binary never named; the Tier-2 list is its home.

## Two Ω_P rulings remain reserved (escalated, NOT bundled into the evidence read)

The engine settles robust-vs-fragile (above). It does **not** settle these two — both the
operator's seat (Seat Theorem Cor 2b; the plan reserves them three times):

1. **OQ-56 vocabulary selection** — which orbits "matter" as the stance vocabulary. Tier 1 is
   the evidence-settled reproducible set; whether to *also* admit Tier-2 keys (knowingly
   model-relative) is the operator's pick, informed by the 0.13–0.57 numbers, not foreclosed by
   them.
2. **OQ-53 committer-transpose disposition** — a model-relative axis is **not disqualified** from
   a model-relative transpose. Whether to build a knowingly-model-relative committer transpose
   anyway is the operator's call. The audit does **not** pre-close it as "foreclosed."

The audit presents the ranked two-tier menu and escalates these two picks. It does not pick the
vocabulary or rule the transpose.

## Phase 1b — DONE (operator Decision 2: run the probe)

Read-only `swipl` probe over each twin (`phase1b_probe.pl`, load witnessed = 960 both twins via
`asserta` overlay; non-vacuity control `cs_axiom_grounding` facts = 2037/1903). Results
(`phase1b_results.json`): axiom-grounding-profile **0.272** (fragile, as predicted);
kernel-obstruction-class **0.734** (reproducible — the surprise the operator anticipated). Both
folded into the ranking and menu above.

## Still owed

- **`cs_kernel_comparison` firing-witness** on the twins — **DONE 2026-06-20.** Ran
  `write_kernel_comparison_entry(user_output, ai_risk_prioritization, false)` on the haiku twin
  (consult(stack) + load_all_testsets + consult(json_report)). Emitted a first-class kernel
  object: `{"kernel_id":"ai_risk_prioritization","reading_count":2,"readings":[{"reading_id":
  "ai_risk_prioritization__existential_risk_reading",...},{"reading_id":
  "ai_risk_prioritization__near_term_harms_reading",...}]}`. The report path ENUMERATES a
  kernel's reading-set (not a string prefix) ⇒ **OQ-53 same-kernel close is (a)-for-json_report**,
  earned by witnessed query output, reversing the prior prefix-opaque expectation.
- **Orthogonality positive control** — **DONE 2026-06-20.** apparatus is non-degenerate (8
  classes, H=1.869 bits, largest cell 0.403); the same MI machinery returns high on
  known-dependent pairs (observer vs claimed_type 0.515; R5-seat-vec vs R4-mode 1.000) and low
  (0.063) on apparatus vs observer — so the orthogonality is real, not an imbalance artifact.
- **Differing-commit confound** bound but not eliminated (extraction control caps its effect on
  `claimed_type`).
- **Phase 3 operator + wiring** — build the Tier-1 reproducible keys (R1 reading-orbit +
  obstruction-class kernel-orbit) as a reusable operator. Gated on the OQ-56 vocabulary pick
  (whether Tier-2 keys are also materialized).
- **Replication on a fresh twin pair** — the thin-margin R4 (0.566) and any Tier-2 key the
  operator elects to admit warrant the registered replication the 2026-06-18 audit also owed.
