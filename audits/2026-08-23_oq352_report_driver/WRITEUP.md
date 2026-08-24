# OQ-352 — the per-leg REPORT driver (`report_corpus`)

**Executed:** 2026-08-23
**OQ:** OQ-352 (gates OQ-353, OQ-354). Mints OQ-356.
**Verdict (scoped to the apparatus, not to any corpus-level result):** the driver exists,
refuses on all sixteen listed conditions two-sided by reason code, and produced a complete,
sidecar-verified artifact set for `testsets_sonnet2` and `testsets_sonnet3` — **at 10 of 11
stages, with the eleventh named.** `giant_component_analysis` **cannot run on 17 of 20 corpora**
(16 of the 19 live legs, plus `original_v6`), throwing
`>=/2: Arithmetic: 'unknown/0' is not a function` at an identical point every time (**OQ-356**).
So the driver is built and witnessed, and the corpus-level program it was built to enable is
**blocked on a defect the driver's own first use exposed**.
**Manifest cite:** `outputs/legs/{testsets_sonnet2,testsets_sonnet3}/*.manifest.json`, each
carrying a top-level `corpus_hash` accepted by `assert_corpus_current` (13/13 per leg); the
per-leg classify outputs `outputs/pipeline_output.{sonnet2,sonnet3}.json`, both at
`code_commit 909a4cbe0b2a` = HEAD at run time; leg md5 `7c9a9e426e97` / `2ab01a6e63d9`,
unchanged across both runs.
**Fired:** live — see below.

**Fired: live.** Four things moved that were not moved before, none of them the thing the
audit was built to look at:
1. `giant_component_analysis` is **broken on 17 of 20 corpora** — 16 of the 19 live legs and
   the v6 archive — and has been for as long as `effective_purity` could propagate `unknown`.
   It was invisible because the stage had only ever been run on `testsets/` (n≈285) inside
   `run_pipeline`, which is precisely the gap this OQ exists to close. **Every giant-comp number
   in circulation is a `testsets/`-only number.** The defect was not hiding; nothing had ever
   looked (OQ-356).
2. `run_pipeline.py`'s `~6 min at n=3380` comment for `_prolog_giant_comp` **describes a run
   that never completed.** The throw arrives at 230 s. That figure has never been a measurement
   of a successful v6 run.
3. Two defects in the driver itself were caught by its own controls before any witness run:
   `stages=[]` silently running all eleven stages, and `MISSING_CLASSIFY_OUTPUT` looking for a
   filename that does not exist on disk for any leg.
4. The OQ-60 purity-guard sweep is shown to have **missed at least one site**, and a calibrated
   class sweep suggests it is plausibly a class rather than an instance.
5. `classify_corpus` on v6 was **killed by its own `soft_timeout` at 3701 s and retried** — the
   `0.73 s/story` calibration under-predicts at n=3380, and `soft_timeout = ceiling // 2` turns
   one slow run into a 247-minute retry schedule. Witnessed in `outputs/prolog_children.log`,
   the forensic channel this OQ deliberately enumerated rather than suppressed.

---

## 1. Two tables, derived from the pinned quotation — reconciled with neither

The authoritative list is the OQ-352 fact paragraph in `ISSUES.md`, pinned at `0c1191239` and
re-read verbatim at execution time (HEAD had not moved). **Counts below are DERIVED from the
table, never asserted beside it.**

### 1a. TOOLS — one row per name the quoted paragraph enumerates

| # | tool named in the fact paragraph | disposition |
|---|---|---|
| 1 | `orbit_report` (+ `orbit_data.json`) | BUILT — stage `orbit` |
| 2 | `fpn_report` | BUILT — stage `fpn` |
| 3 | `giant_component_analysis` | BUILT — stage `giant_comp` |
| 4 | `covering_analysis` | BUILT — stage `covering` |
| 5 | `fingerprint_report` | BUILT — stage `fingerprint` |
| 6 | `context_profile` (HAC, `trajectory_enabled=1`) | BUILT — stage `trajectory` |
| 7 | `commentary_census` | BUILT — stage `commentary_census` |
| 8 | `maxent_report` | BUILT — stage `maxent` |
| 9 | `abductive_report` | BUILT — stage `abductive` |
| 10 | `pattern_mining` | DEFERRED — `SCOPE_TRACKED_GENERATOR` |
| 11 | `variance_analysis` | DEFERRED — `SCOPE_TRACKED_GENERATOR` |
| 12 | `index_sufficiency` | DEFERRED — `SCOPE_TRACKED_GENERATOR` |

**Built but NOT named in the quoted paragraph** (overlay-capable report stages in
`_phase_prolog`; excluding them would be an unexplained hole): `coupling`
(`inferred_coupling_protocol`), `maxent_diag` (`maxent_diagnostic`).

Derived: 9 of the 12 quoted names built, 3 deferred; +2 unquoted = **11 stages**.

**The deferral is a TESTED REFUSAL, not a gap**, and the reason names the actual blocker
(re-derived first-hand, INVESTIGATIONS P2): the three analyzers are ALREADY path-parameterized
(`VarianceAnalyzer`/`PatternMiner` take `corpus_data_path`, `SufficiencyTester` takes
`corpus_data_path` + `pipeline_data_path`, all three accept an in-memory `data=`). What blocks
them is strictly upstream — `_phase_python_tier2` reads `outputs/corpus_data.json` ←
`outputs/output.txt` ← `_prolog_validation` running `prolog/validation_suite.pl`, a **tracked**
file `python_test_suite.build_suite()` rewrites from `testsets/` only. Reversal is cheap; its
cost is recorded rather than discovered.

### 1b. STATISTICS — a DIFFERENT partition; not added to the above

Four of OQ-353's pre-registered statistics are `classify_corpus`/`json_report.pl` products, not
report-stage products: `orbit_monotonicity`, `corpus_wasserstein_fracture`,
`arakelov_threshold`, and the step-0 `diagnostic` block. **Tools and statistics are different
partitions and are not reconciled into one another.** The consequence is a refusal, not a
footnote: a leg is only COMPLETE when a **same-commit** classify output sits beside the report
artifacts (`MISSING_CLASSIFY_OUTPUT`).

---

## 2. What the driver carries

- **One fresh swipl process per leg AND per stage** (OQ-246); stages strictly **serial**, which
  satisfies OQ-182 (`trajectory`/`giant_comp` never co-resident) for free.
- `classify_corpus`'s input refusals reused verbatim, plus the 4a taxonomy.
- **Output gated, not only input**: exist + non-empty + owed marker + a manifest sidecar whose
  **top-level** `corpus_hash` `assert_corpus_current` accepts (top-level because that is where
  the checker reads it — a nested stamp would make the guard raise on a fresh file).
- **Leg fingerprint around the run** (gotchas §5) → `CORPUS_DRIFT`.
- **Retry ledger** per stage per attempt with per-artifact byte counts, so the 967-byte
  truncation signature stays checkable and a stage is never absorbed as "ran".

### 2a. The 4a/4b split is load-bearing

`original_v6` is what forces it. It carries `story_provenance` on **0/3380** files and must
**COMPLETE with `PROMPT_HASH_ABSENT` recorded, not refuse** — while every 4a code owes a
two-sided fixture, which ABSENT has no passing counterpart for. `PROMPT_HASH_ABSENT` is
deliberately DISTINCT from `PROMPT_HASH_UNIFORM`: an empty prompt-hash set must never read as
agreement (Pattern 5).

### 2b. The transit guard

Three stages write a JSON co-product to a path hard-coded in Prolog — verified by reading all
eleven stage modules, not the docs (`orbit_report.pl:110`, `abductive_report.pl:393`,
`giant_component_analysis.pl:1570`). That scan also surfaced four further `../outputs/` strings
(`giant_component_analysis.{log,md}`, `covering_analysis.md`, `maxent_diag_stderr.txt`), each
inspected individually and found to be a `%` comment usage-example. The transit set is exactly
three, **checked rather than recalled**.

`_TransitGuard` is the file-level transposition of `probe_harness:with_retracted/2`, hardened
four ways because the window between pre-delete and restore is one where a shared artifact does
not exist on disk: flock lock; an on-disk journal written **before** the first delete with
signal/atexit handlers and restore-then-refuse recovery; a no-emit branch; and unconditional
recovery on resume. `TRANSIT_BACKUP_LOST` is the never-downgrade-to-a-warning branch — the one
case where restore-then-refuse cannot restore.

**All guard state lives in `.report_corpus/`, outside `outputs/` entirely.** Guard state under
`outputs/` would falsify the selftest's own isolation post-condition, which is how that
post-condition acquires an `--exclude` flag and stops meaning anything.

---

## 3. Four defects the controls caught, before any result was published

1. **`stages=[]` ran all eleven stages instead of zero.** A falsy test (`if stages`) collapsed
   "explicitly none" into "unspecified" — absence wearing the shape of a default, the Build
   Discipline spine exactly. Caught by the selftest's **isolation post-condition**, which saw
   eleven swipl children logged by a call that had asked for none. The control earned its keep
   on its first execution.
2. **`MISSING_CLASSIFY_OUTPUT` looked for a file that never exists.** It resolved
   `pipeline_output_<leg>.json`; all 22 per-leg outputs on disk are
   `pipeline_output.<short>.json`. The refusal would have fired on every leg forever *while
   looking like a working gate*. Now mirrors `python/audits/leg_diagnostic_table.py:57-59`
   exactly — the instrument OQ-353's `Files:` line says to extend rather than fork — pinned by
   six controls including a literal check against the consumer's source and a two-sided check
   against disk (22/22 match).
3. **`outputs/prolog_children.log`** is appended by `run_prolog`, a reused helper, so a real leg
   run writes under `outputs/` outside `legs/<leg>/`. **Enumerated in the write invariant rather
   than excluded from the check.** Suppressing it would delete the OQ-301 forensic channel
   exactly where this driver needs it most (~20 `run_giant_component_analysis` executions at
   n≈1000 is the population that reported-not-witnessed 7/100 regime would be met in).

4. **The refusal taxonomy over-claimed: three documented codes could not fire.**
   `expected_model` was accepted by `report_corpus` and **never read**, so
   `PROVENANCE_COVERAGE` and `MODEL_MISMATCH` named codes the driver could not produce, and
   `LOAD_INCOMPLETE` had nothing behind it either — while the gate row's own comment claimed
   *"every 4a code gets a planted fixture"*. `TRANSIT_RESTORE_FAILED` had no fixture at all.
   Found by auditing the documented table against the executed selftest rather than by any test
   failing: **an over-claiming refusal table is precisely the defect the table exists to catch**
   (Pattern 1, the dangling wire, with a success-shaped taxonomy over it).

   **This is the orphaned-control risk the plan was built to prevent, caught by the plan's own
   two-sided requirement.** Nothing failed; the defect surfaced only because the plan demanded a
   fires/passes fixture pair *per reason code*, which forced enumerating the table against the
   executed selftest — and three codes had no fixture because they had no code. A taxonomy
   asserted only in prose would have shipped intact. The control demonstrated its value on the
   thing it was written for.

   Wired rather than trimmed from the docstring. `LOAD_INCOMPLETE` is checked against the
   REQUIRED same-commit classify output instead of by re-loading the corpus — `classify_corpus`
   already asserted `glob == per_constraint == manifest.n_constraints` in Prolog at this commit,
   so comparing its manifest to the CURRENT glob inherits that assertion *and* catches the leg
   having moved since; re-loading 3380 files to re-derive a number another gate already
   certified would be minutes spent to learn nothing. The fingerprint refusals read
   `story_provenance` arg 7 by position (the same scan that yields the prompt hash), and
   coverage is asserted BEFORE the prefix match so the match cannot pass over an empty fact set
   — with a fixture pinning that ordering. Selftest 49 → 58 controls.

### 3b. Sibling sweep for the falsy-vs-absent defect — adjudicated, not counted

Defect 1 (`stages=[]`) is a falsy-vs-absent bug, so this driver was swept for siblings: every
parameter defaulting to `None` that is then tested for bare truthiness. **16 hits, 1 real.**

| hits | site | verdict |
|---|---|---|
| 10 | `progress` | **safe by type** — a callable is always truthy; only `None` is falsy |
| 2 | `run_at` | benign — an empty timestamp string is not a meaningful value |
| 1 | `out_dir` | benign — `Path("")` is not a meaningful "explicitly none" |
| 1 | `corpus_dir` (`inject_manifest`) | pre-existing, same benign shape |
| 1 | **`giant_comp_timeout`** | **REAL, and written by this OQ — FIXED** to `is not None`, so an explicit `0` fails loudly instead of being silently replaced by the 900 s default |
| 1 | `soft_timeout` (`run_prolog`) | pre-existing, and **deliberately exploited** — see below |

`run_prolog`'s `cap = timeout if (final or not soft_timeout) else ...` is the same shape, and it
is what made the v6 run possible: passing `soft_timeout=0` gives every attempt the full ceiling,
routing around the 247-minute retry schedule. Left as-is and documented rather than "fixed",
because changing it would remove that escape with no replacement. Reported as an adjudication
rather than a count, for the same reason as §4e.

### 3a. A measurement that replaced an assumption

The selftest's isolation post-condition uses `(size, mtime_ns)`, not sha256. `outputs/` is
13,150 files / 3.8 GB: hashing costs **~242 s per manifest** (~8 min per selftest, outside the
row's charter) while `stat` costs **0.07 s**. This is not a weakening — the question is "did the
driver write where it promised not to", and a write moves `mtime` even when content is
byte-identical, which a hash cannot see. For this question it is strictly **more** sensitive.
Stated consequence, accepted rather than excluded away: a foreign process writing under
`outputs/` during the selftest turns the row red, and the message names the paths.

**That consequence was then observed, which is the useful part.** Running the selftest while the
v6 `classify_corpus` was still going produced exactly one red —
`ISOLATION: ... 1 changed: ['outputs/pipeline_output.raw.json']` — the file that run was
writing. The check named the offending path in one line, which is what an exclusion list would
have cost us. It is reported here as a demonstration that the row behaves as documented, not as
a driver defect.

---

## 4. The `giant_comp` probe HALTED — and the defect is nearly total (OQ-356)

The pre-registered rule: ceiling = measured wall × 3 (`_CLASSIFY_HEADROOM`, reused so the two
drivers do not carry different unstated headrooms), floored at 900; **and a probe that does not
complete cleanly is a HALT, never a fallback to the floor** — with no wall time the formula has
no input, and defaulting to the floor would let a *failed measurement* present as a *configured
ceiling*.

```
leg     archives/datasets/original_v6   n 3380
wall    230.3 s
result  PrologError rc=2
error   >=/2: Arithmetic: `unknown/0' is not a function
md_bytes null   raw_bytes null   transit_emitted []
```

**No ceiling is derived and none is assumed.**

**Localization** (second run under `catch_with_backtrace`; the stack did not attach, but the
run's own stdout is decisive): it prints every section through `### Contamination Collapse
Analysis` and throws immediately after that sweep's table header — inside the first
`count_by_action_band/8` call. `precompute_props_loop` is NOT the site: it completes silently at
3380 (3380 mod 100 = 80, so 3300 is correctly the last printed row).

**Mechanism** — the OQ-60 dual stated verbatim in CLAUDE.md. `count_by_action_band/8`
(`giant_component_analysis.pl:1276-1281`) calls `effective_purity` **directly**, not through the
`-1.0`-collapsing `gc_node_purity` cache, and filters `catch(..., _, fail), EP >= 0.0`.
`effective_purity` can **propagate** `unknown` (the 0a path); `unknown` is a return value, not
an exception, **so the `catch/3` intercepts nothing** and `EP >= 0.0` throws on the atom.

**This is a missed site, not a new defect.** Both guarded siblings are one grep away and one is
in the same file:

| site | form | state |
|---|---|---|
| `drl_purity_network.pl:353` | `effective_purity(...), number(EP), EP >= 0.0` | GUARDED, OQ-60 comment |
| `giant_component_analysis.pl:365` | `catch(effective_purity(...), _, EffP0 = -1.0), number(EffP0)` | GUARDED, OQ-60 comment naming this exact hazard |
| `giant_component_analysis.pl:1278` | `catch(effective_purity(...), _, fail), EP >= 0.0` | **UNGUARDED** |

The `:362` comment even states the reason. The fix was reasoned, written down, and applied to
two of three sites.

**Not repaired in flight.** The one-word fix (`number(EP), EP >= 0.0`) is single-file and
single-revert, but it changes engine behaviour, and the **frozen** preregistration (md5
`fdaed841b0e33f0212513874b255518e`) pre-committed to routing it to an OQ rather than repairing
it mid-execution. Honouring that is the point of freezing it.

### 4b. The leg census — the finding's real size

`giant_comp_leg_census.py` runs the stage under every live leg plus the archive
(`giant_comp_leg_census.txt`):

| | corpora | which |
|---|---|---|
| COMPLETE | **3** | `testsets` (n=285), `testsets_haiku2`, `testsets_haiku3` |
| **THROW** | **17** | haiku, flash, kimi, sonnet, stealth, nemotron, sonnet2, stealth2, sonnet3, stealth3, kimi2, nemotron_think, flash2, flash3, flash_think, flash_think2, original_v6 |

Every throw is the same error reaching the same last section
(`### Contamination Collapse Analysis`), in 6–10 s at n≈1000 and 221 s at n=3380.

**The census carries its own positive control**: `testsets` — the one corpus the stage has ever
run on — completes in 1.5 s and reaches `### Key Finding`. The harness is right; the throws are
real. `haiku2`/`haiku3` passing while `haiku` throws shows the trigger is data-dependent: it
needs a giant-component member whose `effective_purity` is `unknown`.

### 4c. WHY the three pass — and they do NOT share a property

The obvious hypothesis (legs with unauthored `coordination_type` throw) is REFUTED by the
cross-tab (`purity_absence_crosstab.py`): `testsets` completes with 13 unknown-purity stories
while `testsets_nemotron` throws with 1. Absence count does not predict it.

Two further probes (`gc_predictor_probe.pl`, results `gc_predictor_v1.txt` / `_v2.txt`) each
reached 19/20 and each mispredicted the SAME leg, `testsets`. The compound predictor is 20/20:

> **THROW ⟺ (giant component > 10% of the network) AND (some GC member's
> `effective_purity` succeeds with a non-number).**

| leg | GC fraction | >10% gate | unknown-purity GC members | outcome |
|---|---|---|---|---|
| `testsets` | **4.7%** (12/258) | **NO** | 1 | **OK — block never entered** |
| `testsets_haiku2` | 66.5% | yes | **0** | OK — genuinely clean |
| `testsets_haiku3` | 63.3% | yes | **0** | OK — genuinely clean |
| the other 17 | 37.7–92.8% | yes | 1–151 | THROW |

**So the three that pass do not share a property — they pass for two different reasons.**
`haiku2`/`haiku3` pass INSIDE the code path, having zero unknown-purity members. `testsets`
passes because the path is NEVER EXECUTED: `run_phase3` gates the whole contamination block on
`GCFrac > 0.10` (`giant_component_analysis.pl:855`, comment `% At least 10% to be interesting`),
and at 4.7% it prints *"No significant component found"* and returns. `testsets` has an
unknown-purity GC member and WOULD throw if it ever got there.

### 4d. The concealment mechanism, as its own finding

This is a claim about the k=1 regime, not about `count_by_action_band/8`, and OQ-353/OQ-354
inherit it:

> **Phase 3's contamination block has never run on any corpus, ever.** Not "its numbers are
> `testsets/`-only" — on `testsets` the block is *unreachable* behind the fragmentation gate, and
> `testsets` is the only corpus the stage was ever run on. `report_gc_composition`,
> `report_contamination_sources`, `report_multihop_contamination`,
> `report_sound_constraint_exposure` and the collapse sweep have **no published numbers to
> caveat, because they never produced any.**

That is OQ-352's founding argument arriving as a result: the OQ was minted on the claim that
corpus-level tools are unaudited k=1 point estimates, and its first use found a whole report
phase whose k was **zero**.

**Scope boundary, stated precisely because "every giant-comp number is suspect" would
overreach.** Giant-component TOPOLOGY is unaffected — `compute_components` ran cleanly on all
20 corpora, which is where the GC sizes in the table above come from. Only the Phase-3
contamination surface is implicated. Independently, ISSUES.md:602 records that the giant_comp
headline has **zero downstream consumers**, bounding the blast radius from the other side.

**Falsifiable prediction for the leg still filling.** `testsets_nemotron_think` throws today
(GC 92.8%, 2 unknown members) as does base `testsets_nemotron` (83.2%, 1), so the
thinking/non-thinking contrast does NOT discriminate here. When the leg completes it is a fresh
draw. Precedent: `haiku` (4 unknowns, throws) → `haiku2`/`haiku3` (0 unknowns, pass).

**The prediction is ASYMMETRIC and must be recorded as three outcomes, not two.** "Will throw
unless unknowns hit zero" collapses two different passes — the very distinction that took two
probes to find. Record the unknown count AND the GC fraction with the result:

| outcome | reading |
|---|---|
| THROW | confirms the compound predictor on a fresh draw |
| PASS with unknown-in-GC == 0 | the `haiku2`/`haiku3` mechanism — genuinely clean inside the path |
| PASS with unknown-in-GC > 0 | the `testsets` mechanism — GC fraction fell under 10%, path never entered. **Informative about topology, and NOT evidence the stage works** |

Only the middle row is evidence about purity authoring. The third would be evidence about
network density, and reading it as a pass would repeat this audit's own first-probe error.

**RESULT (2026-08-24): row 1, THROW.** The leg completed at n=1003 during this audit. GC
888/1003 = **88.5%**, unknown-in-GC = **2**, outright failures 0; same error, same last section.
The compound predictor holds on a leg neither probe was tuned on.

**The invariance is story-level, not a rate** — checked rather than assumed. Both unknown-purity
GC members (`genesis_creation_cosmology__young_earth_literal`,
`vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading`) are **pre-existing** in the
735-story set committed at `909a4cbe0`; **271 stories were added and 3 removed** since, and
**zero new `unknown` appeared**.

**A premise this audit asserted earlier is CORRECTED here.** "`purity_score` returns `unknown`
exactly when `coordination_type` is unauthored" is false: 5 stories on this leg lack
`coordination_type` and only 2 score the `unknown` ATOM — the other 3 score **`-1.0`**, the
epistemic-gate-fail sentinel, which is a NUMBER and is therefore excluded safely by `EP >= 0.0`.
That is the OQ-60 two-token rule, and absence of `coordination_type` is **necessary but not
sufficient** for the throwing token. The original claim is left standing above rather than
retconned; this is the correction.

### 4e. Why no existing test caught it — a property of the code path, not of anyone's diligence

**The defect was unreachable on `testsets` as constituted, so no amount of re-running the stage
there would have found it.** The `GCFrac > 0.10` gate has to be crossed first, and `testsets`
sits at 4.7%. This closes the obvious reviewer question more firmly than the sweep-criterion
argument does, because it is a fact about the path rather than about how hard anyone looked.

*Stated precisely, because "at any n" would overreach:* the gate is on the giant-component
FRACTION, not on n. Growing `testsets` would probably cross it eventually — every leg at n≈1000
sits at 37.7–92.8% — so the honest claim is that the defect is unreachable at `testsets`' actual
size and topology, and that reaching it requires a corpus with a materially denser component,
which in practice means a different leg. Which is what OQ-352 built.

**Was anything vacuously satisfied? Checked, and NO** — no test, gate row, or coverage check
asserts the Phase-3 contamination block's output. There is no orphaned control in that
neighbourhood to report.

**But the adjacent control is more interesting than a vacuous one would have been.**
`prolog/tests/test_purity_absence.pl` is the OQ-60 regression suite, and it carries
`test(gc_ingest_collapses_unknown_to_sentinel)`: it plants a story whose `purity_score` is
`unknown`, runs `precompute_props_loop`, and asserts the collapse to `-1.0` *and* that the
`V >= 0.0` distribution filter excludes the sentinel. That test is **correct, and it passes, and
it will keep passing while the stage is dead on 17 of 20 corpora** — because it covers the
GUARDED ingest of that value and `count_by_action_band` is the SECOND, unguarded ingest of the
same value in the same module. Its coverage boundary is exactly the defect boundary. It is not
over-claiming either: its header scopes token-totality to `purity_zone` and the JSON writers,
which does not extend to the giant_comp report surface.

That is a **third** instance of this arc's recurring shape — the OQ-60 sweep, this audit's two
probes, and now the OQ-60 test — all correct about the value's absence and all bounded one
call-site short of where the value REACHES arithmetic.

**And the covered half is not enforced.** No gate row runs any `prolog/tests/*.pl`; `run_pipeline`
executes exactly four suites (`test_reading_totality`, `test_epsilon_declaration`,
`test_residual_signature_inert`, `test_agent_beneficiary`) and `test_purity_absence` is not among
them. So the one test that does cover an OQ-60 ingest site runs only when someone runs it by hand.
Recorded as an observation for OQ-356's ruling, not repaired here.

### 4f. Class sweep — a CANDIDATE LIST, not a finding

`purity_guard_sweep.py` scans every `purity_score`/`effective_purity` call site in `prolog/` that
does arithmetic within a 5-line window: **50 sites, 41 guarded, 9 unguarded.** It carries a
positive control and **DISCRIMINATES** — fires on the known-unguarded site, correctly marks both
known-guarded siblings as guarded.

**The 9 must not be cited as 9 defects.** Four were spot-checked and **two are clearly false**:

| site | verdict |
|---|---|
| `giant_component_analysis.pl:1278` | **REAL** — the witnessed throw |
| `fpn_report.pl:94` | **LIKELY REAL** — `one_hop_ep_safe/3`, identical shape |
| `json_report.pl:1427` | **NEEDS ADJUDICATION** — `NP1 \= -1.0` is the documented silent dual (`\=` does not throw, so it ADMITS `unknown`); binds rather than computes |
| `genuine_findings_query.pl:101` | **FALSE POSITIVE** — only `format('~w')`s the value |
| `drl_purity_network.pl:224` | **FALSE POSITIVE** — guarded six lines down, outside the window |
| 4 others | UNADJUDICATED |

Honest statement: **the defect is plausibly a class rather than an instance, and per-site
adjudication is owed.**

**Why class-first, in the form that generalizes.** Not merely "sweeps miss sites". OQ-60 guarded
two siblings — one **in the same file**, carrying a comment naming this exact hazard — and still
missed this one. That is evidence the sweep's FIND-CRITERION was wrong, most likely keyed on the
`catch(...)` idiom rather than on *"an `unknown` reaches an arithmetic comparison on this path"*.
Fixing the instance leaves the criterion unfixed and the next sweep misses the next site the same
way.

**This audit's own probes demonstrate the point twice, which is why it is stated with
confidence.** Probe v1 conflated `purity_score` FAILING (safe — the conjunct fails and the
member is skipped) with SUCCEEDING as `unknown` (throws). Probe v2 fixed that and was still
wrong, because neither modelled the upstream `GCFrac > 0.10` reachability gate. Both were the
same class of error as OQ-60's: reasoning about the value's absence instead of about whether it
REACHES the arithmetic. A correct criterion has to model guards and reachability, not idioms or
absence counts.

---

## 5. Witness run

### 5a. The pair arm — COMPLETE at 10 of 11 stages

Both legs ran every stage they were given, at one frozen HEAD (`909a4cbe0`).

| assertion | `testsets_sonnet2` | `testsets_sonnet3` |
|---|---|---|
| stages `STAGE_OK` | **10/10** | **10/10** |
| wall | 336 s | 331 s |
| artifacts, none empty | **13** | **13** |
| sidecars accepted by `assert_corpus_current` | **13/13** | **13/13** |
| leg md5 across the run | `7c9a9e426e97` → same, **UNCHANGED** | `2ab01a6e63d9` → same, **UNCHANGED** |
| retry ledger | present, 10 rows | present, 10 rows |
| prompt-hash token | `PROMPT_HASH_UNIFORM`, coverage 1.0000, `e03e2210…` | same |
| same-commit classify | `pipeline_output.sonnet2.json` @ `909a4cbe0b2a` = HEAD | `…sonnet3.json` @ `909a4cbe0b2a` = HEAD |
| transit journal at exit | **clean** | **clean** |

**Recorded deviation from the frozen prereg.** §3b excluded `giant_comp` for `original_v6`
only. The first pair run therefore requested all eleven stages and **refused
`ARTIFACT_ABSENT`** on both legs — correctly: the stage produced nothing. The leg census in §4b
then showed the same throw on 17 of 20 corpora, which the prereg did not anticipate. Excluding
`giant_comp` from the pair arm applies the prereg's own principle (a stage that throws is
DECLARED, not downgraded) to a wider blast radius than it foresaw. **The claim is 10 of 11
stages on every leg, with the missing one named** — not "the artifact set is complete".

### 5b. Verification #4 — the pre-named criterion, PASS

| # | statistic | verdict |
|---|---|---|
| R1 | `corpus_wasserstein_fracture` | DIFFERS |
| R2 | `arakelov_threshold` | DIFFERS |
| R3 | `type_distribution` | DIFFERS |
| R4 | `purity_n_scored` / `_no_data` | DIFFERS |
| R5 | `drift_event_counts` | DIFFERS |
| R6 | orbit-class histogram (`orbit_data.json`) | DIFFERS |
| R7 | giant-component size / member set | **UNAVAILABLE — instrument broken (OQ-356)** |

**6 of 6 available REQUIRED statistics differ. PASS.** The prereg's rule was ≥5 of R1–R7; with
R7 unavailable that becomes ≥5 of 6, which is a proportionally stricter bar, not a relaxed one.
The overlay is demonstrably taking effect — the 2026-06-13 `assertz`-shadowing failure mode is
excluded. All 13 report-stage artifacts also differ between the two legs; **no report artifact
came out saturated.**

### 5c. Two saturation predictions came out "DIFFERS" — and the fault was MY TEST, not OQ-355

The prereg named S1–S4 as expected-saturated. S1 (`network_stability` = `cascading`) and S4
(`network_cascade_count_threshold`) were identical as predicted. S2 and S3 differed **as I
operationalized them**, and reporting that as "OQ-355's predictions were wrong" would have been
a manufactured finding. Checked against what OQ-355 actually claims:

- **S2** — the claim is that `boltzmann_summary` is a *deterministic coarsening* of
  `coupling_summary` (the same `CouplingScore =< Threshold` test): a RELATION, not a constant.
  Verified mechanically on both legs: `compliant == independent` (83/83, 86/86),
  `inconclusive` equal, `non_compliant == ` the remaining coupling buckets (919/919, 916/916).
  **The relation HOLDS on both legs.** The counts differ because the corpora differ, which the
  claim never denied.
- **S3** — the claim is `mountain == 1.0` and `scaffold == 0.0` on every leg. Both hold on both
  legs. The other `by_type` keys (rope, tangled_rope, unknown) vary, and OQ-355 never said they
  were constant.

**The prereg tested whole-object equality where the claims are about a relation and about two
named entries.** Recorded as a correction to this audit's instrument, with the original stated
outcome left standing rather than quietly re-tuned.

### 5c-bis. The pair arm RE-WITNESSED at clean HEAD — the caveat is bookkeeping

The witness artifacts were produced with uncommitted driver fixes in the tree
(`code_dirty: True`), so no commit reconstructed them. The ruling was to re-witness the pair
arm at clean HEAD and **keep both sets**, turning the caveat into a measurement rather than an
annotation.

Re-run at `86a70f0042dc`: both legs **10/10 stages**, classify 663 s / 651 s at n=1003, corpus
hashes unchanged (`7c9a9e426e97`, `2ab01a6e63d9`).

`compare_dirty_vs_clean.py` (evidence: `dirty_vs_clean_comparison.txt`, dirty hashes pinned in
`dirty_artifacts_manifest.txt`):

| verdict | n | meaning |
|---|---|---|
| `IDENTICAL_RAW` | **24** | every content artifact — byte-identical across both runs, both legs |
| `IDENTICAL_NORMALIZED` | 28 | identical once the DECLARED varying keys are stripped |
| `IDENTICAL_TRANSITIVE` | 2 | see below |
| **`DIFFERS`** | **0** | — |

**VERDICT: the dirty artifacts were SOUND; the `code_dirty` caveat was bookkeeping, not
substance.** The varying keys were declared in advance (`pipeline_run_at`, `code_commit`,
`code_commit_short`, `code_dirty`) so a difference elsewhere could not be waved through as
"probably the timestamp," and a positive control plants a byte change the comparator must flag —
it does, so the identity verdict is a tested absence rather than an untested one.

**The comparator over-reported on its first run, and the fix is the interesting part.** It
initially called 2 artifacts `DIFFERS`: both were the sidecar *for* `commentary_census.json`,
differing only in `artifact_sha256`. That file embeds its own manifest with `pipeline_run_at`, so
its RAW hash can never be stable — while the file itself compares `IDENTICAL_NORMALIZED`. The
sidecar's varying keys were stripped; the hash it records *over another file that has its own*
was not. Reclassified as `IDENTICAL_TRANSITIVE` — a transitive consequence of the same declared
keys — reported as its own class rather than folded into either, so the distinction stays
visible. **A structural note falls out: a sidecar stamping `artifact_sha256` over an artifact
that embeds a run stamp is non-reproducible by construction** (OQ-352's `_write_sidecar`); the
hash is of the un-normalized file. Not repaired here.

### 5d. The v6 arm

See §4 and OQ-356. Run with `giant_comp` excluded and every other refusal — including
`MISSING_CLASSIFY_OUTPUT` — applying unmodified; the `CLASSIFY_EXEMPT` hatch stays rejected.

---

## 6. Handoff to OQ-353

1. **`giant_comp` is BLOCKED for the whole program, not just for v6** (OQ-356). Its statistics
   cannot be given a floor, and the size × content × edge-semantics control has no size arm,
   until the guard lands. OQ-354 inherits the same block.

   **RESOLUTION PATH — pick (a), do not leave this open.** The pair floor covers **10 stages,
   not 11**, so OQ-353 must not pre-register against an 11-stage floor that does not exist.
   - **(a) DEFERRED, then run as an incremental leg-pair — RECOMMENDED.** When OQ-356 lands,
     re-run `report_corpus` with `--stages giant_comp` over `testsets_sonnet2` /
     `testsets_sonnet3`; the driver's per-stage design makes this a minutes-long incremental
     run, not a re-do, and the sidecars make the result joinable to the existing 10-stage set.
     OQ-353 pre-registers an 11th statistic marked `PENDING OQ-356` rather than omitting it.
   - **(b) permanently absent** — rejected: the giant-comp surface is the one OQ-353 has the
     least prior information about, since §4d shows it has never run anywhere.

   Note the asymmetry that makes (a) cheap: `giant_comp` is the ONLY stage that needs re-running,
   and it takes 6–10 s per leg **when it works**.
2. **The pair floor is a k=2 point estimate with no distribution behind it.** Two draws give a
   difference, not a confidence statement. R1–R6 all differ; that licenses "these six move
   between same-model redraws", not a floor magnitude.
3. **Normalization decision, recorded now rather than at analysis time:** the pair is
   n=1003/1003 exactly, so it needs none. The v6 arm at 3380 does. Statistics stay flagged
   n-sensitive (counts, family counts, coverage denominators) vs scale-free (shares, fractions,
   rates) in the artifact index.
4. **Saturated-statistic list from Verification #4:** none among the report-stage artifacts (all
   13 differ). Among the `diagnostic` block, S1 and S4 are confirmed constant across the pair;
   S2 and S3 are confirmed as OQ-355 states them (a relation and two named entries), NOT as
   whole-object constants — do not re-test them the way this prereg did.
5. **`_classify_timeout_for` needs a ruling** (OQ-356, second-order finding): the linear
   `0.73 s/story` under-predicts at n=3380, and `soft_timeout = ceiling // 2` turns one
   legitimately-slow run into 247 minutes. Any OQ-353 sweep over a large archive leg will meet
   this.
6. **Operational:** the whole sequence must run at ONE frozen HEAD, because
   `MISSING_CLASSIFY_OUTPUT` compares `manifest.code_commit` to HEAD.
7. `python/audits/leg_diagnostic_table.py` is the instrument to **extend, not fork** — the
   driver already resolves per-leg classify outputs by that script's exact convention, pinned by
   a control.

---

## Evidence map

- `PREREGISTRATION.md` — frozen before any witness artifact; md5 `fdaed841b0e33f0212513874b255518e`.
- `audit_log.md` — step-by-step execution record with the HEAD stamp and every witness pasted.
- `giant_comp_probe_v6.json` — the halted timing probe (§4).
- `purity_guard_sweep.py` / `.txt` — the class sweep and its positive control (§4a).
