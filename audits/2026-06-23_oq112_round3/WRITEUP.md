# OQ-112 Round 3 — Round 0 (re-witness on 92 + reader recon)

**Date:** 2026-06-23. **Substrate:** `main` @ `1d473730`, live corpus 92 testsets.
**Probes (read-only):** `probe_round0.pl`, `probe_round0_control.pl`, `probe_round0_pc3.pl`;
raw outputs `round0_out.txt`, `round0_control_out.txt`. **Phase:** evidence-gathering only;
no engine writes.

## Headline

Item 4 (the metric-fallback-`0.0` idiom) is **LATENT on 92** — confirmed against a positive
control. The reader recon then found that **two of the three pre-registered Round-3 commits target
mechanisms the evidence contradicts**: Commit-2's "silent findall-drop / item-2 blind spot" is
actually a LOUD throw that item-2's completion gate already catches, and Commit-3's "un-wrapped
external caller crashes" has no caller at all. Commit-1 (idiom cleanup) is the one valid,
witnessed-valuable write. **Escalated to operator (scope re-ruling).**

## Round 0 step 1 — re-pin 92

```
A: corpus_constraint=92 ; distinct constraint_claim subjects=86
A: claim-subjects not in corpus (engine demos) = 0: []
```

92 corpus members; 86 carry `constraint_claim` (the maxent driver enumeration at
`maxent_classifier.pl:662/724/886`); 0 non-corpus demo subjects (post-2026-06-04 regime, manifest
clean).

## Round 0 step 2 — item-4 firing re-witness on 92 (per non-sentinel source)

```
B: claim+corpus lacking base_extractiveness      = 0: []
B: claim+corpus lacking extractiveness_for_agent = 0: []
B: claim+corpus lacking theater(theater_ratio)   = 0: []
```

**All 86 claim-bearing constraints carry all three metrics.** The 6 absent-suppression / absent-
metric constraints found in Round 2 (`actinide_..._contradictions`, `digital_money_...`, etc.)
lack `constraint_claim`, so they never enter the maxent driver. **Item 4 does not fire live on 92**
for any source — same latency mechanism as the Round-2 suppression case.

### Positive control (PC1) — the probe detects a firing

An empty B is a clean-grep until shown to detect. Retracting a real classifiable constraint's
theater (overlay, `probe_round0_control.pl`):

```
control constraint C0 = actinide_replenishment_mechanism_flat_control (baseline dr_type = mountain)
baseline get_constraint_metrics: eps=0.03 supp=0.02 theater=0.08
PC1: theater retracted -> get_constraint_metrics: eps=0.03 supp=0.02 theater=0.0
PC1 verdict: theater else-branch FIRED (fabricated 0.0 — item-4 firing reproduced)
```

The else-branch fires and a fabricated `0.0` flows. The LATENT verdict is real, not a probe that
never looked.

## Round 0 step 3 — arith/diag reader recon

**The maxent-local accessors are not used outside `maxent_classifier.pl`** (empty cross-file grep
for `get_constraint_metrics` / `metric_value_indexed`). So **Commit-1's blast radius is contained
to `maxent_classifier.pl`** — the hybrid fixes the *local accessor*, leaving the shared sources
(`base_extractiveness` ~30 consumers, `extractiveness_for_agent`, `constraint_metric`) untouched.

**The cross-term (the finding the recon was meant to surface):** `get_constraint_metrics` feeds
BOTH `continuous_log_likelihood` (arith, `:246`) AND `maxent_threshold_proximity` (`:531`, the
boundary path). Disposition: sentinel at the accessor; the boundary path's throw-handling is
Commit-3's concern (but see below — no live caller).

### Why theater is the unique residual (PC2/PC3)

```
PC2: dr_type(theater-absent C0) -> mountain      (theater-absence does NOT block dr_type -> ENTERS findall)
PC3: dr_type(eps-absent     C0) -> unknown        (eps-absence yields the `unknown` TYPE -> excluded from per-maxent-type profiles)
```

`is_X/3` calls `base_extractiveness` and `extractiveness_for_agent` first, so eps/chi-absence
drives `dr_type` to the `unknown` type → the constraint is excluded from every per-maxent-type
profile findall (`dr_type(C,Ctx,mountain)` fails). **Only theater-absence leaves the constraint
classified** (`mountain`), so it is the one source whose absence reaches the profile arith while
the constraint still enters the findall.

## Round 0 step 3a — Commit-2 mechanism (the build-order determinant) — CONTRADICTS PLAN

The plan frames Commit-2 as a **silent findall-drop** that **item-2's gate structurally cannot
catch**. Both halves are false on the evidence.

**(a) It is a LOUD throw, not a silent drop.** `sum_list` is OUTSIDE the findall
(`compute_type_profile_indexed`, findall `:838–843`, `sum_list` `:846`). Direct witness:

```
MECH: sum_list([0.1, unknown, 0.2]) -> error(type_error(evaluable, unknown/0), context(system:(is)/2,_))
```

Post-Commit-1, a theater-absent classifiable constraint contributes `unknown` to the theater
findall (PC2: it enters) → `sum_list` throws → the throw propagates out of
`compute_type_profile_indexed`. The constraint is *included as `unknown`*, not silently dropped;
there is no "profile over a subset that reads complete."

**(b) Item-2's gate already catches it.** `maxent_indexed_run` runs
`maxent_compute_profiles_indexed` (`:897`, where the throw happens) **strictly before**
`assertz(maxent_indexed_run_info(...))` (`:905`). A precompute throw therefore leaves `run_info`
**absent**, and `diagnostic_summary.pl:648/651` floors `verdict_join` on
`\+ maxent_indexed_run_info(Ctx,_,_)`. In the pipeline, `json_report.pl:81/86` wraps the run in
`catch(...,_,fail)` (the throw is absorbed there) — **but the resulting `run_info`-absence still
trips item-2's completion gate.** This is exactly the case item-2 was built for (its own comment at
`:900–903` cites the mid-loop throw).

**The only genuinely-silent path** is a profile of exactly one classifiable constraint that lacks
theater: `Values=[unknown]` fails the `[_,_|_]` ≥2-sample guard → falls to `default_profile`, the
`unknown` discarded. That is the *designed* sparse-type fallback (benign), not corruption.

**Conclusion:** under the ruled Commit-1 (return `unknown` → route to item-2's gate), there is no
silent wrong-population profile. Commit-2 as specified closes a blind spot item-2 does not have.

## Round 0 step 3b — Commit-3 premise — CONTRADICTS PLAN

```
all callers of maxent_boundary_analysis  : (none)
callers of maxent_threshold_proximity    : maxent_report.pl:211, maxent_diagnostic.pl:395  — BOTH catch-wrapped
```

`maxent_boundary_analysis` has **zero callers** in the entire codebase. The threshold arith's only
two live callers are already `catch(...,_,fail)`. Post-Commit-1 they catch the `abs(unknown-Thresh)`
throw and fail gracefully — **no un-wrapped external crash exists.** Commit-3's "called by external
diagnostics → crashes the caller" has no live instance.

## Round 0 step 3b — the 4 "sound" sites

Not re-confirmed individually yet — deferred pending the scope re-ruling, since the disposition of
the diag sites depends on whether Commit-1 ships alone or with reshaped residuals.

## Net recon verdict (what the evidence supports building)

| Commit | Plan premise | Evidence | Disposition |
|---|---|---|---|
| **1 — idiom cleanup** | fabricated `0.0` → arith; dead `;Supp=0.0` | confirmed (PC1 fires; Round-2 W3 dead) | **BUILD** — blast radius = `maxent_classifier.pl` only |
| **2 — findall silent-drop** | item-2 blind spot | **FALSE** — loud throw, caught by item-2's run_info gate | **escalate**: drop, or reshape as graceful-degradation (quality, not defect) |
| **3 — boundary external-crash** | un-wrapped caller crashes | **FALSE** — 0 callers; live threshold callers catch-wrapped | **escalate**: drop, or log latent hardening in `design_gaps.md` |

**Commit-1 is the trigger for the (now-shown-handled) downstream hazards.** Its end-to-end witness
(constructed theater-absent claim constraint → item-2 gate fires / verdict floored) doubles as the
disproof of Commit-2's necessity.
