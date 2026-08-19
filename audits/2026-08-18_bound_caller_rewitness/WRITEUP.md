# OQ-303(a) — the `latent-B` premise re-witnessed: two instruments with disjoint blind spots, one live bound caller the regex could not see, and a residue class that turned out not to be shared

**Executed:** 2026-08-18 (= directory date)
**OQ:** OQ-303 (arm (a) — class-B head conversions)
**Verdict:** The pre-registered verdict, verbatim — *"the bound-caller premise rests on an
instrument pair with disjoint blind spots and one shared residue class; the partition is the
evidence, the residue is the finding, and the count of clean-converting rows is neither."*
**ESCALATED under `PREREGISTRATION.md` §2's escalation clause, and CORRECTED under its §6
falsifier:** the disjointness held and was witnessed in *both* directions on real engine
code, but the shared residue class **does not exist as named** — `library(prolog_codewalk)`
executes `A=B` while walking, so the very shape the plan called shared residue is a third
codewalk-only capability; and the disjointness is no longer the largest finding, because one
`latent-B` row (`signature_grade/2`) has a **live bound caller on the `verdict_join` headline
path** that the regex arm scored zero for at census time.
**Substrate:** no pipeline run — this audit reads code, not corpus. Static reference for the
session: `outputs/pipeline_output.json` manifest `pipeline_run_at` 2026-08-18T20:34:53Z,
`n_constraints` 279, `code_commit` 6523046, `code_dirty` true. Engine load chain: `[stack]`
plus every registry-named file except one declared exclusion (§4). swipl 10.0.2.
**Fired:** live — a `latent-B` row's class label is wrong (`signature_grade/2` has a live
bound caller reaching `diagnostic_summary:join_alerts/2`, so the gate row
`codewalk_caller_check --check` is RED on it); and the unit's own pre-registered residue
claim was falsified by its own selftest control before any live row was read.
**Evidence map:**

| artifact | what it is | which claim it witnesses |
|---|---|---|
| `PREREGISTRATION.md` | frozen before the first run; md5 `900d4a42…` recorded in `audit_log.md` above the first result line | the verdict shape, the four dispositions, the zero rule, the §6 falsifiers |
| `audit_log.md` | chronological log: OPEN stamp, prior-art grep, baseline gate, registry read, R1–R10 with pasted output | every numeric claim below |
| `partition.py` | regenerates the partition; runs BOTH arms, joins them, asserts the row count against the registry, carries the three adjudications | the partition is reproducible and cannot fork from the registry |
| `partition.md` | the receiver's worklist — 58 rows, disposition + both columns + witness owed | §3 |
| `partition.tsv` | same rows, machine-readable, with `machine_disposition` vs adjudicated | §3, §5 |
| `regex_sweep_raw.txt` | full output of `caller_sweep.py` on this run | the regex column |
| `codewalk_evaluate_true.json` / `codewalk_evaluate_false.json` | the codewalk column, both `evaluate` settings | §2 (the residue correction), §6 (measured-empty stratum) |
| `../../python/codewalk_caller_check.py`, `../../prolog/codewalk_caller.pl` | the new arm (not in this directory — it is apparatus, not evidence) | the instrument itself; discrimination record in both headers |

---

## 1. What was actually under test

OQ-303 arm (a) proposes converting ~60 predicates whose registry class is `latent-B`. That
label means: *shape present, **no live bound caller found** in the 2026-08-17 caller sweep.*
The finder was `audits/2026-08-17_bound_dispatch_hardening/caller_sweep.py` — a single-line
regex whose blind spots are readable in its own source (`:44`, `:72-73`): one physical line
at a time, no nested-term arguments (conceded in its docstring), bare lowercase atoms only,
name/arity matching with no module resolution.

So the premise is not *none exist*. It is *none found by one instrument with undeclared blind
spots*. The unit built a second arm — `library(prolog_codewalk)` over the loaded program,
never before used in this repo (prior-art grep: 0 hits) — and reported the join.

**The worklist size came from the registry, not from prose.** `N_latentB = 58` (registry
total 73). OQ-303 says "~60"; the partition's row count is asserted equal to 58 in
`partition.py`, so "all N classified" means a checked 58.

## 2. The correction the unit made to its own premise

The plan's headline named a *shared residue class*: a selector bound by unification before
the call (`T = rope, ..., p(C, T)`), invisible to both arms. **It is not shared.** The
selftest control for that shape came back BOUND, and the cause was witnessed rather than
inferred (`audit_log.md` R3): SWI does not compile the unification away — `clause/2` still
shows `_G=alpha, q(a,_G)` — but `library(prolog_codewalk)` **executes** `A=B` as it walks
(`prolog_codewalk.pl:663-664`, `unify_with_occurs_check/2`, default on), propagating the
binding into later goals of the same body.

Consequences, all of them load-bearing:

1. The named shape is a **third codewalk-only capability**, not residue. The instrument pair
   is *more* asymmetric than the plan supposed, not less.
2. The genuine shared residue is **narrower**: selectors bound by *runtime computation* —
   `member/2`, a helper predicate's output, arithmetic, `findall`. `evaluate/2` handles
   unification only. Both shapes are now selftest controls, asserted in both directions.
3. The stratum became **measurable instead of declared**: `run_codewalk_caller/2` takes the
   flag, and evaluate(true) minus evaluate(false) is exactly the unification-bound set.
   Measured on the live engine: **0 sites** (§6).

`PREREGISTRATION.md` §6 pre-registered exactly this falsifier — *"The residue class is not
shared if either arm sees a dataflow-bound selector … the 'one shared residue class' clause
is false and must be struck."* It is struck.

## 3. The partition

58 rows, every one classified, row count asserted against the registry:

| disposition | n | meaning |
|---|---|---|
| `converts-clean` | 55 | zero bound callers under both arms — template application, no six-leg run |
| `live-output-path` | 1 | `signature_detection.pl signature_grade/2` — see §5 |
| `regex-only` | 2 | `json_report.pl` `boltzmann_label/2`, `live_index_label/3` — codewalk returned no verdict (§4); **not a pre-registered disposition**, see §7 |
| unification-bound residue | 0 sites | measured, with a firing control (§6) |

Full worklist with both columns and per-row witness obligations: `partition.md`.

## 4. The load chain is where this arm's blind spot actually lives

`[stack]` alone loads **11 of the registry's 26 files not at all**, leaving 17 of the 58
`latent-B` rows unscoreable — and a row the arm cannot see reads exactly like a row with no
callers. Four of those eleven carry **no `:- module/2` header at all** (`json_report`,
`fpn_report`, `maxent_report`, `orbit_report`), so a file→module lookup can never resolve
them; resolution was moved to `source_file/2`, and the chain extended with `ensure_loaded/1`.

That reduced the unresolved set from 20 to 3 — and turned up a second property of `evaluate`:

**`json_report.pl` does not terminate under `evaluate(true)`** (>90 s, against 0.5–0.7 s for
every other registry file) and walks in 0.6 s under `evaluate(false)`. The non-termination is
in the `A=B` propagation — the same feature that produced §2. It is a **declared, printed load
exclusion** in `codewalk_caller_check.py`, costing exactly the two `regex-only` rows plus
`write_json_number/2` (input-key).

## 5. The three disagreements — three different mechanisms, both directions witnessed

This is the evidence that the arms are **not nested**. Reads and git witnesses in
`audit_log.md` R9.

| row | rx | cw | mechanism |
|---|---|---|---|
| `composition_rule/3` | 5 | 0 | **regex false positives** — all five are prose inside block comments (`dirac_classification.pl` spans 224–239, 429–455); `is_comment()` skips only `%`/`*` lines. Reproduces `RECON.md:95` mechanically. → `converts-clean` |
| `claimed_natural/2` | 3 | 0 | **regex true positives the codewalk arm cannot see** — real bound goals inside a Prolog goal string embedded in `python/audits/oq49_override_remeasure.py:73,75,93` (committed 2026-06-14, so present at the sweep). The regex arm's genuine unique capability. Adjudicated class B with note at `RECON.md:96`. → `converts-clean`, conversion must update that probe |
| `signature_grade/2` | 0 | 1 | **codewalk true positive the regex missed, NEW** — see below |

**`signature_grade/2` is the finding with consequences.**
`signature_detection.pl:1951` is `signature_severity(C, moderate) :- signature_grade(C, correction).`
The call is the clause's last goal on its own line, so `caller_sweep.py`'s `is_clause_head()`
reads the terminating `.` as a fact head and skips it. `caller_sweep_output.txt:251` records
`0 bound call site(s)`, and `git show 9a5d8526:prolog/signature_detection.pl` line 1901 shows
the call was already there — a **true false negative at sweep time**, not a later addition.

It is on a live output path: `signature_grade/2` → `signature_severity/2` →
`diagnostic_summary:join_alerts/2:749` → the OQ-98 `verdict_join` headline. So this row's
conversion owes the **full six-leg `classify_corpus` clean-vs-edited pair**, not the template
treatment its `latent-B` label licensed.

`python/codewalk_caller_check.py --check` is **RED** on this row, and that is its correct
state: the registry asserts no live bound caller and one exists. It goes green when the row is
converted or reclassified.

## 6. What the controls license, and what they do not

- **Two-sided, same process, enforced not documented** (`check_controls()`): fires on
  `dr_type/3` (`sites=67 bound=19`), declines on `constraint_signature/2` (`sites=18
  bound=0`). The decline half carries the information: the arm looked and found 18 call
  sites, then reported zero bound among them. A declines-control with `sites=0` fails the run.
- **The measured-empty unification stratum is a real zero, not a didn't-look.** evaluate(true)
  and evaluate(false) return byte-identical counts on the live engine (214 sites, 35 bound
  both ways), and the selftest's `fx_unify` control shows the measurement discriminates on
  planted code (bound=1 vs bound=0 across the same pair of runs).
- **What no control here licenses:** the *runtime-bound* residue is unmeasured, by
  construction. Neither arm can see `member(T, Ts), p(C, T)` or a selector arriving from a
  helper predicate. `partition.md` reports that as an unmeasured class, and the fixture
  `fx_runtime_sink/2` asserts it stays free under both flags so a future change that starts
  resolving it turns the selftest red rather than silently rewriting the count.

## 7. Prereg gap, recorded rather than papered over

`PREREGISTRATION.md` §3 fixed four dispositions and required every row to be classified.
`converts-clean` is defined as zero bound sites under **both** instruments — which presumes
both instruments always return a verdict. Two rows have no codewalk verdict at all (§4), so
they fall through every pre-registered disposition. They are reported as **`regex-only`**,
labelled in `partition.py` and in `partition.md` as NOT PRE-REGISTERED. The prereg was not
amended.

## 8. Residue

- **Hard stop.** Unit A ends here. No class-B conversion was performed; no `prolog/*.pl`
  semantics changed; no gate row was added (gate stays at 24 rows, GREEN except the new
  checker, which is not wired into it).
- **Re-price Unit B against this partition**, per the plan. The relevant inputs: 55 rows
  convert on the template; 1 row (`signature_grade/2`) needs the six-leg pair and a class
  reassignment; 2 rows are regex-evidence-only and inherit the `json_report` walk exclusion.
- **`codewalk_caller_check --check` is RED and stays RED** until `signature_grade/2` is
  converted or its registry class corrected. It is deliberately **not** in `scripts/gate.sh`
  — wiring a knowingly-red row into the gate would be a standing red light, which is a
  different decision and the operator's.
- Written to substrate: one line on OQ-303 pointing here (§ ISSUES.md), one ADOPTED row in
  `docs/technical/swipl_load_path_and_probe_gotchas.md` §16.
