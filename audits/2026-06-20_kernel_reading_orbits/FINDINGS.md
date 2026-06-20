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

## The pre-registered verdict — and why it under-discriminates (FLAGGED)

The pre-registered rule (`Wilson-95 lo > permute band95`, non-degenerate) marks **all 5
reading-orbit keys AND K1 DRAW_ROBUST** → non-empty menu. **But this rule only tests "agreement
beats random label assignment," not "membership is reproducible enough to declare as a
vocabulary."** It passes the very 0.134 kernel-membership the plan's own prose (and the
2026-06-18 FINDINGS) calls *draw-sensitive / model-relative*. Per audit discipline a
wrongly-specified pre-registered criterion is **halt-and-escalate, not inline-amended** — so the
floor verdict stands as registered, and the substantive readout is the **absolute
membership-reproducibility ranking**:

| key | agreement | Wilson-lo | band95 | floor verdict | reproducibility |
|---|---|---|---|---|---|
| **R1** observer_signature | **0.722** | 0.693 | 0.476 | ROBUST | **high — declarable** |
| R4 terminal_observer | 0.566 | 0.534 | 0.348 | ROBUST | borderline |
| R2 apparatus `cs_pattern` [axis 1] | 0.487 | 0.455 | 0.344 | ROBUST | fragile (~½ flip) |
| R3 terminal_committer [axis 2] | 0.300 | 0.271 | 0.226 | ROBUST | fragile |
| R5 seat_role_vector | 0.245 | 0.219 | 0.109 | ROBUST | fragile |
| K1 kernel structure-signature | 0.134 | 0.102 | 0.027 | ROBUST | very fragile |

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

## The ruling this surfaces (operator's Ω_P seat — escalated, not self-resolved)

The menu is non-empty under the registered chance-floor, but **"non-empty" at the chance floor ≠
"declarable as OQ-56's stance vocabulary."** Where the **declarability cut** sits — above-chance
(all 6 keys) vs membership-reproducible (R1 only; R4 borderline) — is the operator's Ω_P
selection seat (Seat Theorem Cor 2b; the plan: *"the selection of which orbits matter is the
operator's Ω_P ruling — escalated, not self-resolved"*). The audit presents the ranked candidate
menu; it does not pick the cut. See the escalation recorded in ISSUES.md OQ-150/OQ-56.

## Deferred / owed

- **Phase 1b (Prolog probe, cost-gated):** `cs_axiom_grounding` profile orbit and
  `cs_kernel_obstruction/4` class — not serialized in the twin JSON. Given the JSON pass already
  shows only the observer axis is declarable, these two committer-side keys are *expected*
  fragile (same family as R2/R3); run only if the operator wants the apparatus/obstruction axes
  measured directly before ruling.
- **`cs_kernel_comparison` firing-witness** on the twins (RECON owe) — for the OQ-53 (a)/(b) close.
- **Differing-commit confound** bound but not eliminated (extraction control caps its effect on
  `claimed_type`).
