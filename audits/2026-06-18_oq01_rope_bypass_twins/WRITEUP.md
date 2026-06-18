# OQ-01 grounding on the twin corpora — the A3 collapse does not reproduce

**Date:** 2026-06-18
**OQ:** OQ-01 (rope-gate `Chi =< 0` bypass: intentional modeling or artifact?)
**Question put to the run:** does the A3 presheaf collapse (the evidence motivating
OQ-01's "latent artifact" worry) reproduce on the post-reset live-regime corpora, so
that a *positive safe-envelope* boundary note can be written for `logic.md`?

## Method

`ground_rope_bypass.py` re-runs the range-sweep variants (A1 sigmoid baseline, A2, A3,
B3) — the same `alt_sigmoid_f/3` power-function clauses in
`prolog/tests/test_battery_variants.pl` the original May-2026 sweep used — but overlays
`corpus_path` onto the two live-regime **twin** corpora (`testsets_haiku`,
`testsets_flash`, 960 stories each) instead of the chimera-era `archives/prolog_v5`
the original sweep ran on. Presheaf set = constraints with `h0 == 0` from
`product_site_export:run_product_export_to/1`; Jaccard computed vs the per-corpus A1
sigmoid baseline (identical reference frame to the original, which reported A2/A3 vs the
sigmoid baseline).

Variant geometry (d0=0.50, k=6.0 throughout):
- A2: L=−0.20, U=1.00, sign-flip, span 1.20
- **A3: L=−0.20, U=0.65, sign-flip, span 0.85 — compressed ceiling, bypass-exercising**
- B3: L=0.02, U=0.87, no sign-flip, span 0.85 — matched span, no flip

A3 has L=−0.20 with sign-flip, so f(d) < 0 at low d ⇒ χ ≤ 0 ⇒ the rope-gate bypass
fires. The variant exercises exactly the clause OQ-01 is about.

## Positive controls

- Overlay-took-effect: `[corpus] Loading 960 testset files...` on each twin (not the
  default 80-file `testsets`) — confirms the `retract→asserta` overlay loaded the twin,
  not silently the default (the witnessed silent-fallback trap, CLAUDE.md Corpus Loading).
- Export schema: A1 export carries `h0` (distribution on haiku: 871×h0=0, 89×h0=1) — the
  presheaf set is a real partition, not a `.get('h0', 0)` default over a missing field.

## Result

Jaccard vs A1 sigmoid baseline:

| Variant | geometry | prolog_v5 (original) | testsets_haiku | testsets_flash |
|---|---|---|---|---|
| A2 | span 1.20, flip | 0.864 | 0.919 (N=814) | 0.965 (N=808) |
| **A3** | **span 0.85, flip, compressed** | **0.319 (1,417 spurious)** | **0.904 (N=797)** | **0.897 (N=751)** |
| B3 | span 0.85, no flip | 0.780 | 0.894 (N=792) | 0.820 (N=687) |

Baseline presheaf counts: haiku N=871, flash N=833. Raw per-variant set deltas
(`s→p` = new presheaves, `p→s` = lost) in `evidence/summary.json` (committed witness). The
per-variant presheaf exports (~9 MB each) are not committed — regenerate deterministically
with `ground_rope_bypass.py` (`evidence/.gitignore` lists them).

## Finding (contradicts OQ-01's evidence)

**The A3 collapse is corpus-specific to the chimera-era `prolog_v5`; it does not reproduce
on either live-regime twin.** On both twins the compressed-ceiling + sign-flip variant
(A3) drifts only modestly from baseline (Jaccard ≈ 0.90; at most 84 presheaves move),
nowhere near the 0.319 / 1,417-spurious collapse the original sweep reported. Two further
points cut against the original causal story:

1. On `testsets_flash`, the no-flip B3 (0.820) drifts **more** than the sign-flip A3
   (0.897) — the opposite of "sign-flip is the destabilizer." On haiku the three variants
   are within 0.025 of each other.
2. The "bypass × sign-flip × compressed-ceiling interaction" that the original sweep
   isolated as the cause of the collapse is not visible as an interaction here: A3 is not
   anomalous relative to A2 or B3 on either twin.

The collapse was therefore a property of the **prolog_v5 ε/d distribution** (ID-reuse
chimera corpus) interacting with the compressed variant — not an intrinsic behavior of the
`Chi =< 0 → true` clause. The de-leaked, kernel-based generation of the live corpus does
not produce the dense band of high-ε constraints whose χ crosses zero under compression.

## Consequence for closing OQ-01

The grounding **removes the motivation for a guard** (OQ-01 resolution path b). The worry
that the bypass is a latent artifact that misbehaves under compressed ceilings is not
supported by live data — the misbehavior is absent in the current regime. Combined with the
standing facts that (i) the bypass's intent is documented in `docs/logic.md` §rope
(negative-chi epsilon bypass, v6.0) and `drl_core.pl:381`, predating the sweep, and (ii)
the net-beneficiary theory is coherent, the bypass is **intentional, defensible modeling
content**.

The boundary note cannot state a positive safe-envelope grounded in the collapse, because
the collapse does not occur on the live corpora. What it can — and should — record instead
is the *corpus-dependence* itself: the dramatic A3 instability seen on `prolog_v5` is not a
property of the clause and does not appear post-reset.

### Under-claim / scope

Two live twins (post-reset rebuild regime, 960 each) is a genuine cross-corpus check but
not exhaustive. The honest claim is: *the A3 collapse does not reproduce on the two
live-regime twins.* It does **not** establish that no corpus could ever enter the collapse
region — only that the live regime, and the de-leaked generation that produces it, does
not. That residual ("which ε/d distributions re-enter the starvation/collapse band") is
the OQ-22 question (Hub-1 starvation under degenerate transforms), which stays open.
