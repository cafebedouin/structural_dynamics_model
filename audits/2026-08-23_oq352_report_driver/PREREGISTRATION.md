# PREREGISTRATION — OQ-352 per-leg REPORT driver (`report_corpus`)

**Frozen:** 2026-08-23, BEFORE any witness-run artifact exists.
**OQ:** OQ-352 (gates OQ-353, OQ-354).
**Code state at freeze:** `3573c5797` (commits 1-4 landed; gate GREEN, selftest 49/49).
**Purpose:** fix, in advance, the three things that otherwise get written after the
fact because they have no slot — the cross-leg difference criterion, `original_v6`'s
expected refusal profile, and the v6 `giant_comp` ceiling.

This document is md5-stamped at commit and the stamp is logged in `audit_log.md`
before the first counted row (the OQ-301 precedent). If this file changes, the
stamp stops naming what is on disk and every result below loses its
pre-registration.

---

## 0. What is being measured, and what it is NOT

The driver is an apparatus, not a result. The only claims this audit may make are:

1. `report_corpus` runs the eleven built report stages against an overlay leg and
   refuses on each of the sixteen 4a conditions, two-sided by reason code.
2. The full artifact set exists for at least two live legs and one archive
   overlay, with manifest sidecars `assert_corpus_current` accepts.
3. A pre-named subset of statistics differs across `testsets_sonnet2` /
   `testsets_sonnet3`, and a pre-named subset is expected saturated.

**Explicitly NOT claimed.** The sonnet2/sonnet3 contrast is a **k=2 point
estimate with no distribution behind it**. Two draws give a difference, not a
floor with a confidence statement, and nothing here licenses one. `original_v6`
is **not a third draw** of that population: it serves exactly two claims — the
archive overlay path executes, and the size arm exists — and the pair floor rests
on sonnet2/sonnet3 alone.

---

## 1. Cross-leg difference criterion (Verification #4)

Declared BEFORE the run. The failure this prevents is "all artifacts must
differ", which misfires on anything legitimately saturated or stable by design.

### 1a. REQUIRED TO DIFFER across sonnet2 vs sonnet3

Named now, from the `diagnostic` block keys present on both legs' existing
classify outputs plus the report-stage artifacts the driver produces:

| # | statistic | source | why it must differ |
|---|---|---|---|
| R1 | `corpus_wasserstein_fracture` (median) | classify | measured DRAW-DOMINATED on this exact pair (1.124 vs 0.602, ratio 1.87) — `audits/2026-08-23_leg_diagnostic_table/` |
| R2 | `arakelov_threshold` | classify | same finding, same pair |
| R3 | `type_distribution` (per-type counts) | classify | per-story classifications differ across redraws |
| R4 | `purity_n_scored` / `purity_n_no_data` | classify | coordination_type authoring varies by draw |
| R5 | `drift_event_counts` | classify | per-story drift differs across redraws |
| R6 | orbit-class histogram | `orbit_data.json` | per-story orbits derive from per-story types (R3) |
| R7 | giant-component size / member set | `giant_component_analysis.raw.json` | edges are story-derived |

**Verdict rule:** ≥5 of R1–R7 differ ⇒ the driver is measuring corpus content and
not echoing a shared substrate. **<5 differing is a RED and a HALT**, not a
finding to interpret: it would mean the overlay is not taking effect, which is the
2026-06-13 `assertz`-shadowing failure mode wearing a success shape.

### 1b. EXPECTED SATURATED — identical is a FINDING, never a gate failure

| # | statistic | why saturation is expected |
|---|---|---|
| S1 | `network_stability` | `cascading` on 19/19 legs via an absolute `NumSevere >= 3` (OQ-355) |
| S2 | `boltzmann_summary` | a deterministic coarsening of `coupling_summary` — same `CouplingScore =< Threshold` test (OQ-355) |
| S3 | `contextuality.by_type` | mountain 1.0 / scaffold 0.0 on every leg (OQ-355) |
| S4 | `network_cascade_count_threshold` | a config constant, not a measurement |

Anything in 1a that comes out **identical**, and anything outside S1–S4 that comes
out identical, is **recorded as a candidate construction-bound / saturated
statistic and handed to OQ-353** — not scored as a failure here.

### 1c. sonnet2 vs original_v6

A separate and **weaker** check: these differ in size (1003 vs 3380), regime
(post-reset vs chimera-era) and edge semantics simultaneously, so a difference
attributes to nothing in particular. Recorded, never used as evidence for 1a.

---

## 2. `original_v6` expected refusal profile

Written down per arm rather than discovered at run time. v6's profile differs
from a live leg's **by construction**:

| condition | expected on v6 | why |
|---|---|---|
| single-model fingerprint | **NOT APPLIED** (`expected_model=None`) | mixed-model chimera-era corpus |
| prompt hash | **`PROMPT_HASH_ABSENT` RECORDED** | `story_provenance` on 0/3380 files (P3c, re-witnessed this session) |
| `MISSING_CLASSIFY_OUTPUT` | **APPLIES UNMODIFIED** | v6 gets a fresh `classify_corpus` at HEAD as part of this OQ |
| every other 4a code | **applies unmodified** | no exemptions |

**v6 COMPLETES; it does not refuse.** That is the whole reason the 4a/4b split
exists. An empty prompt-hash set must never read as agreement (Pattern 5), so
`ABSENT` is a distinct recorded token, not a flavour of `UNIFORM`.

**The `CLASSIFY_EXEMPT` route — a code letting v6 ship a partial artifact set —
was considered and REJECTED.** Recorded here so nobody weakens
`MISSING_CLASSIFY_OUTPUT` later to make v6 pass. OQ-353 needs v6's fresh classify
run regardless: the on-disk `pipeline_output_original_v6.json` (`3b169bb`, schema
2) is explicitly non-comparable.

---

## 3. The v6 `giant_comp` ceiling — DERIVED FROM A TIMED PROBE

**Rule, fixed before the probe ran:** ceiling = measured wall time × 3, floored at
the existing 900 s. The ×3 is not a new constant — it is `_CLASSIFY_HEADROOM`
(`run_pipeline.py`), reused so the two drivers do not carry different unstated
headrooms. `soft_timeout` = ceiling ÷ 2, mirroring `_classify_timeout_for`.

**A probe that does not complete cleanly is a HALT, never a fallback to the 900 s
floor.** With no wall time the formula has no input, and defaulting to the floor
would let a *failed measurement* present as a *configured ceiling* — the same
absence-satisfies-the-gate shape as Pattern 5.

### 3a. PROBE RESULT: HALT (executed 2026-08-23)

The probe **did not complete**. Evidence:
`audits/2026-08-23_oq352_report_driver/giant_comp_probe_v6.json`.

```
leg        archives/datasets/original_v6   n_files 3380
wall_s     230.3
result     PrologError (rc=2)
error      >=/2: Arithmetic: `unknown/0' is not a function
md_bytes   null      raw_bytes null      transit_emitted []
```

**No ceiling is derived, and none is assumed.** The 900 s floor is a lower bound
on a computed value, not a stand-in for one.

The `~6 min at n=3380` figure in `run_pipeline.py`'s code comment is now known to
describe a run that **never completed**: the throw arrives at 230 s, so that
comment has never been a measurement of a successful v6 giant_comp. Recorded as a
correction, not carried forward.

### 3b. Consequence for the v6 arm, declared in advance

`giant_comp` is **expected to fail on `original_v6`** at this code state. Under
the driver's own gate that produces `ARTIFACT_ABSENT` for
`giant_component_analysis.md`, so **the v6 arm will not produce a complete
artifact set until the throw is fixed.**

This is pre-registered as the expected outcome rather than discovered:

- The v6 arm is run **with `giant_comp` excluded** (`--stages` naming the other
  ten), so the archive-overlay claim is still witnessed for those ten.
- The `giant_comp`-on-v6 failure is written up as a **finding with its own OQ**,
  not silently absorbed and not repaired in flight. Repairing an engine
  arithmetic guard is above the fix-simple-errors threshold (engine behaviour
  change) and is the operator's call.
- **The full-v6 claim is therefore DECLARED INCOMPLETE**, with the missing stage
  named. It is not downgraded to "v6 passed the stages we ran".

---

## 4. Stop rule (pre-committed, restated here so it is inside the frozen document)

If anything in the witness run wants to touch `prolog/validation_suite.pl`, or to
write into shared `outputs/` outside the enumerated transit paths,
`outputs/legs/<leg>/`, and the declared append to `outputs/prolog_children.log`
— **halt, do not adjust.** That is the signal the scope boundary was drawn wrong,
and it becomes an Omega on OQ-353, not an in-flight scope expansion of a build OQ.

Guard state is not an exception: lock, journal and backups live in
`.report_corpus/`, outside `outputs/` entirely.
