# OQ-81 A/B results — 2026-06-10

Design and decision rules: `AB_PLAN.md` (pre-registered before the run). Execution:
`run_ab.py`, 9/9 calls succeeded; raw responses, parsed stories, contexts, and
`analysis_output.txt` in `ab_runs/`. Worktree `oq81-investigation`.

## Verdict against the pre-registered rules

**Primary metric: INERT. Secondary: a clean R-only theater_ratio shift. The outcome falls
BETWEEN the pinned categories** — it does not meet pinned-DISTORTION (which required the
authored claimed_type to move) and does not meet pinned-INERT (which required secondary
shifts ≤ within-arm spread). Reported exactly as that, not graded up or down.

**Reframe (operator review, 2026-06-10 — supersedes "no verdict import" as the closure
language):** the two findings are one phenomenon at two thresholds, not a relief plus a
separate concern. **Verdict import occurred in the gradable channel and was absorbed before
the categorical one**: theater_ratio was pulled toward tangled_rope's genuine-coordination
profile (reasoned in prose) while claimed_type held — presumably because the explicit snare
hypothesis in `axis_source_desc` anchors the categorical field hard. The R-arm prose
explicitly reasoning about theater doubles as the positive control this design would
otherwise lack: it proves the injected verdict was read and attended to, so the 9/9 snare
result is a real null, not a dead probe. At n=3, one axis, with a maximally clean hypothesis
anchor, the categorical channel is established as STICKIER, not safe. The general finding —
**categorical-stable, continuous-distorted under context injection** — is recorded beyond
this decision (KNOWN_STATE 2026-06-10; memory).

### Metric 1/2 — claimed_type (decision-bearing): no CATEGORICAL import (see reframe above)

All 9 reps, all arms: **snare** (the axis hypothesis), 3/3 within-arm agreement everywhere.
The injected upstream verdict (tangled_rope) — deliberately chosen to differ from the
hypothesis — was imported by zero R reps. A reading's claimed_type does not flip the
supplementary axis's authored type at n=3.

### Metric 3 — scalars: theater_ratio moves in arm R only, with zero range overlap

| field | N (no ctx) | R (reading verdict) | K (kernel substrate) |
|---|---|---|---|
| extractiveness | 0.593 [0.58..0.62] | 0.593 [0.58..0.62] | 0.620 [0.62..0.62] |
| suppression | 0.640 [0.62..0.68] | 0.640 [0.62..0.68] | 0.580 [0.58..0.58] |
| **theater_ratio** | **0.690 [0.68..0.71]** | **0.513 [0.48..0.58]** | **0.680 [0.68..0.68]** |

The three-line verdict block (R's only delta over N) moved authored theater_ratio down by
~0.18 mean with **no range overlap** against either other arm; within-arm spread (N 0.03,
R 0.10, K 0.00) is well under the between-arm gap. The shift is authored reasoning, not
noise: N1 writes theater as dominant-performative at 0.68; R1 explicitly hedges —
"Theater ratio (0.48): Moderate … But theater is not dom[inant]" — consistent with the
injected tangled_rope verdict pulling authoring toward tangled_rope's profile (genuine
coordination function, not pure theater). Direction-of-pull is interpretation, not
established mechanism. theater_ratio is a classification input (theater/piton gates), so a
0.18 authored shift is not cosmetic. extractiveness did not move N→R; K's small constant
offsets (extr +0.03, supp −0.06) are 1-value-vs-range and not decision-bearing at n=3.

### Metric 4 — frame leakage: INVALID (confounded; metric failed its own positive control)

Husk vocabulary and the `husk_reading` token appear in **all arms including N**. Diagnosis:
`axis_source_desc` appends the manifest's full `commitment_system_recognition` JSON — both
readings, verdict-free — to EVERY supplementary-axis prompt (witnessed in
`ab_runs/contexts.json`: source_desc contains `husk_reading`, does NOT contain
`tangled_rope`). `tangled_rope` counts in N prose come from the system-prompt type
vocabulary. No term exists that is present only via the injected block, so the metric
cannot discriminate; no leakage conclusion is drawn.

### Run health

9/9 parse OK, end_turn, ~7k output tokens each. One schema violation: R3
`perspectives[5]` used `analytical` in three enum slots — n=1, not attributable to arm.

## Discovered fact (load-bearing for the operator decision)

**Every supplementary-axis prompt in a kernel manifest already carries the kernel substrate**
— `axis_source_desc` injects the full CSR (kernel_id + both readings' commitments) into all
arms. This means: (1) option (b)'s "inject kernel substrate instead" is ALREADY structurally
in effect; (2) arm K's explicit kernel block was redundant, which explains K ≈ N; (3) the
live decision collapses to a single bit: **keep or suppress the three-line reading-verdict
injection.** This run's evidence: the verdict line does not flip the type but systematically
moves an authored classification input (theater_ratio, ~0.18, no overlap) toward the
injected verdict's profile.

## Recommendation (evidence-supported; ruling stays the operator's)

Suppress reading-typed upstream injection (options (a) and (b) are now equivalent, since
kernel substrate is already present via CSR). The verdict line adds no needed context — the
author already sees both readings — and demonstrably perturbs an authored metric in the
direction of one contested reading's verdict, which is precisely the "privileges one seat"
failure the OQ named.

## Caveats (carried, not buried)

- n=3 per arm, ONE axis, ONE kernel, ONE injected verdict value, temperature 0.2. The
  theater shift is a witnessed instance, not a corpus-level effect size.
- Transport was the direct messages API, not the batch API (same model/system/params).
- "Pulled toward the injected type's profile" is an interpretation of one scalar's direction;
  a different axis/verdict pair could move different fields or none.
