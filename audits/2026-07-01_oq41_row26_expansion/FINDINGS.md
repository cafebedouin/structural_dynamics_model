# OQ-41 row-26 expansion — five-site branch-shape resolution + disposition

**Date:** 2026-07-01. **HEAD:** `27afde7a` (branch `oq197-three-valued-gap-operability`).
**Corpus:** live `testsets/` + twins + `archives/datasets/kernel_v1/` (per engine-change rule).
Raw grep witness: `step0_grep_HEAD.txt`.

---

## Step 0 — grep of all five sites at HEAD (the witness that decides the instrument)

**Line drift vs the 2026-06-24 cites: NONE on these five.** All cited lines are exact at HEAD:
`covering_analysis:490` (sib `:497`), `gap_diagnostic:120` (sib `:127`), `omega1_audit:102`
(sib `:115`), `drl_fpn:206`, `drl_fpn:197`. (The census's `:486`→`:490` drift was an earlier
window; it did not recur.)

### Confirmed branch shapes and trigger classes

| Site | Literal branch | Guard | Trigger class | Reachability |
|---|---|---|---|---|
| `covering_analysis:490` `BaseEps=0.5` | `constraint_metric(C,ExtMetricName,BaseEps) -> true ; BaseEps=0.5` | metric-presence | **metric-absence guard** | legacy phase-2 report (`classify_at_interpolated/4`) |
| `covering_analysis:497` `Supp=0` (sib) | `constraint_metric(C,SuppMetricName,Supp) -> true ; Supp=0` | metric-presence | **metric-absence guard** | same |
| `gap_diagnostic:120` `BaseEps=0.5` | `constraint_metric(C,ExtName,BaseEps) -> true ; BaseEps=0.5` | metric-presence | **metric-absence guard** | dormant `run_gap_diagnostic` (`cache_gap_profile/4`) |
| `gap_diagnostic:127` `Supp=0` (sib) | `constraint_metric(C,SuppName,Supp) -> true ; Supp=0` | metric-presence | **metric-absence guard** | same |
| `omega1_audit:102` `BaseEps=0.5` | `constraint_metric(C,ExtName,BaseEps0) -> BaseEps=BaseEps0 ; BaseEps=0.5` | metric-presence | **metric-absence guard** | fully dormant (`run_omega1_audit` 0 callers) |
| `omega1_audit:115` `Theater=0.0` (sib) | `constraint_metric(C,TheaterName,Theater0) -> Theater=Theater0 ; Theater=0.0` | metric-presence | **metric-absence guard** | same |
| `drl_fpn:206` `Immunity=0.5` | `fpn_type_cache(C,Context,Type) -> type_immunity(Type,Immunity) ; Immunity=0.5` | type-cache-presence | **compute-failed fallback** | **LIVE** (`fpn_enabled=1`) |
| `drl_fpn:197` `NewEP=IP` | `IP<0.0 -> NewEP=IP` (IP=`-1.0` when `fpn_intrinsic` absent) | — | **SENTINEL PASS-THROUGH** | **LIVE** |

### Two substrate corrections to the plan's step-3 taxonomy (both resolve toward branches the plan pre-authored)

1. **`drl_fpn:197` is a sentinel pass-through, NOT a `0.5`/`0.0` fabricated default.** The branch is
   `IP < 0.0 -> NewEP = IP`, where `IP` was set to the negative sentinel `-1.0` upstream when
   `fpn_intrinsic(C,IP)` is absent, and the code comments it `% Sentinel: no purity data`. It
   propagates a negative sentinel, it does not fabricate a plausible mid-value. **Per the plan
   (lines 58–63, 130–131) this is carved OUT of row-26** with a scope note ("not a
   fabricated-default, out of OQ-41 scope"), NOT assigned a row-26 verdict. The contested-mechanism
   read resolves toward the carve-out. Do **not** cite ":197 already-ruled" as precedent for `:206`.

2. **`covering_analysis:490` is a metric-absence guard, NOT "interpolation off-grid."** The plan's
   step-3 (lines 87–89) classes `:490` as `interpolation off-grid` — "keyed on `D`/`Sigma`, not a
   metric-presence guard ... there is no bogus-atom guard to run." The grep contradicts this: the
   `0.5` branch is gated on `constraint_metric(C, ExtMetricName, _)` presence; `D`/`Sigma` only
   parametrise `Chi is BaseEps * FD * Sigma` **after** the default resolves — they set the
   predicate's *reachability envelope*, not the `0.5` branch's *trigger*. So `:490` (and its `:497`
   `Supp` sib) take the **bogus-atom pre-test** of step-3(a), exactly like the other three
   metric-absence sites. This is the "the control must match the branch" refinement the review
   convergence predicted; the grep is the signal that settled it.

**Net:** the row-26 taxonomy for the expansion is **four metric-absence-guard sites**
(`covering_analysis`, `gap_diagnostic`, `omega1_audit`, each with a `0.5`/`0`/`0.0` sibling —
all one class) + **one compute-failed fallback** (`drl_fpn:206`) + **one carved-out sentinel**
(`drl_fpn:197`). The plan's "interpolation off-grid" class turns out to have **zero** members among
the `0.5`-default branches.

---

## Per-site verdicts (five verdicts; DEAD distinct from DORMANT)

Config metric names: `extractiveness_metric_name=extractiveness`, `suppression_metric_name=suppression_requirement`,
`theater_metric_name=theater_ratio` (`config.pl:23–25`). Probes: `probe_metric_absence.pl`, `probe_fpn206.pl`.

### Reachability (static, decisive for the DEAD/DORMANT/LIVE axis)
- `classify_at_interpolated/4` (covering:490): **only** caller is `gap_diagnostic.pl` — itself dormant.
- `cache_gap_profile/4` (gap:120) & `compute_one_profile/2` (omega1:102): **zero** callers anywhere.
- No Python pipeline and no Prolog `stack`/phase-2 path invokes `gap_diagnostic`/`omega1_audit`.
- `fpn_compute_ep/3` (fpn:197/206): **LIVE** via `fpn_run` ← `json_report.pl:91` (`fpn_enabled=1`, `config.pl:485`).

### metric-absence guards (all reject-guard; pred-success on a metric-absent atom is a firing witness)
| Site | Verdict | Witness |
|---|---|---|
| `covering_analysis:490` (+`:497` Supp sib) | **DORMANT/LOCKED** | reject-guard (guard rejects bogus); must-fire control fires — `classify_at_interpolated(<metric-absent>,2.0,1.0)` SUCCEEDS ⇒ 0.5 default fired (returns `unknown`, so even the fabricated 0.5 manufactures no spurious type here); live pipeline count 0 (reachable only via dormant `gap_diagnostic`) |
| `gap_diagnostic:120` (+`:127` Supp sib) | **DORMANT/LOCKED** | reject-guard; `cache_gap_profile/4` has 0 callers |
| `omega1_audit:102` (+`:115` Theater sib) | **DORMANT/LOCKED** | reject-guard; `compute_one_profile/2` / `run_omega1_audit` has 0 callers |

Would-fire counts (genuine scalar absence — `constraint_metric` has no grid, so no rows-24/25 off-grid confound):
testsets/ **9 / 119** lack each scalar metric; kernel_v1 breadth **0 / 1106** (the 9 is a testsets/ sparsity artifact).
Disposition: covered by the **OQ-44 once-for-class** dormant ruling (same as rows 24–25), not fixed per-site.

### compute-failed fallback
| Site | Verdict | Witness |
|---|---|---|
| `drl_fpn:206` `Immunity=0.5` | **NEUTRAL-by-corpus (cosmetic-if-fired)** | firing-marker patch: **0 natural fires** over testsets/ (119, 4 iters, converged) AND kernel_v1 (1106, 17 iters, converged); positive control (forced type-cache miss on a neighboured IP≥0 constraint) **FIRES** both runs ⇒ 0 is measured-empty, not didn't-look. dr_type is total on both corpora (never *fails* in precompute, only returns `unknown`). Sink: `fpn_ep` feeds only diagnostic/report/abductive-evidence (`json_report` diag probes, `abductive_triggers`, `fpn_report`) — **never `dr_type`** ⇒ non-classifying even if it fired. |

### carved out (contested mechanism resolved against substrate)
| Site | Verdict | Witness |
|---|---|---|
| `drl_fpn:197` `NewEP=IP` | **CARVED OUT of row-26** — not a fabricated default | branch is `IP < 0.0 -> NewEP = IP`, IP set to `-1.0` sentinel upstream when `fpn_intrinsic` absent, commented `% Sentinel: no purity data`. Propagates a negative sentinel; does not fabricate a mid-value. Out of OQ-41 scope; **no row-26 verdict assigned** (assigning one is the category error the contest existed to prevent). |

## Net disposition for OQ-41 row 26
No site is LIVE-CLASSIFYING ⇒ **no fail-close lands**; all measurement is behavior-preserving. Three
DORMANT/LOCKED (OQ-44 once-for-class), one NEUTRAL-by-corpus+cosmetic (fpn:206), one carved-out sentinel
(fpn:197). Row 26 closeable.
