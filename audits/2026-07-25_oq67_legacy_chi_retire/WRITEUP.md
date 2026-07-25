# OQ-67 — Retiring the legacy power-modifier χ path by value

**Date:** 2026-07-25 · **Commit:** `a8ec22f0` · **Verdict:** resolved — retired, neither fork of the
posed question was right.

## The question as posed, and why both branches were wrong

OQ-67 asked whether `drl_audit_core` — computing χ = ε × π instead of the canonical sigmoid
χ = ε × f(d) × σ(S) — was **(a)** the last unmigrated caller (finish the migration) or **(b)** a
deliberately separate audit path (replace the TODO with a declared-exemption comment).

Both branches presuppose the path *runs*. It does not:

- `stack.pl:38` loaded the module with an **empty import list** (`use_module(drl_audit_core, [])`),
  so no unqualified call could resolve to it.
- Its only importer was `drl_composition.pl:34`. All five call sites there sat behind
  `constraint_data/2` or `agent_index/2`.
- Both of those terminated in **unconditional fail-stubs** (`agent_index(_,_) :- fail.` /
  `constraint_data(_,_) :- fail.`) that nothing in the live tree ever asserts.

So (a) would have ported dead code onto the canonical path, and (b) would have certified a path
that never executes. The disposal question is therefore *value*, not migration.

## Adjudication by product, not by wiring

Per Build Discipline *Unwired ≠ worthless*, "has no caller" answers **is it used**, not **is it
useful**. Each export was adjudicated against the live product it would yield:

| Export | Verdict | Live product that already yields it |
|---|---|---|
| `structural_signature/3` | duplicate | `omega1_audit:determine_primary_gate/11` — same four χ params, χ-only cascade |
| `ontological_fraud_check(_, fm_alert)` | duplicate **+ defect** | `drl_core:type_1_false_summit`; also `signature_detection.pl` |
| `omega_risk/4` | duplicate **+ mislabel** | `drl_core` (`type_iii`) + `transition_paths` (`type_vi`) |
| `ontological_fraud_check(_, z_alert)` | **unique** | none — no ε∧χ extreme-corner detector exists |

Three duplicates justify deletion. The unique one is a genuine capability loss, so it was preserved
as a **declared absence** — `docs/design/design_gaps.md` GAP-29 — before the deletion landed, and
carries its own doc-diff witness rather than riding the deletion commit.

Two defects in the deleted code are recorded in ISSUES OQ-67 so they do not survive as folklore:
`fm_alert` bound `suppression_score` to a variable *named* `Epsilon` where `logic.md:749` Rule FM
specifies ε (and dropped Rule FM's `∃I(¬■C[I])` leg); `omega_risk`'s `type_vi` for "naturalization
of extraction" is Type **I** per `logic.md:3293` (`logic.md:3370` Type VI is Tangled Rope
Mishandling). Neither propagated, because the path never fired.

## Why the witness set is shaped the way it is

The load-bearing witness is **the stub removal**, not the reachability probe.

While the fail-stubs existed, `is_snare/1` et al. were *defined-and-failing*. Any caller — including
one built by `call/N`, `=..`, or meta-predicate dispatch, which a `forall` probe **structurally
cannot see** — would fail silently. Deleting the stubs makes those predicates **undefined**, so such
a call now throws `existence_error`. That converts the failure mode from silent to loud, which is
what makes a post-deletion exit-0 pipeline run a *positive* result rather than a null diff.

That argument rests on a property ("it would throw") which is itself a claim, so it was witnessed
rather than assumed — KILL #2 below.

Breadth was bought on the cheap instrument: the probe is a `forall` over facts (seconds per leg), so
it ran on all six corpora; `run_pipeline.py` ran once, on `testsets/`.

## Witnesses

| # | Witness | Result |
|---|---|---|
| **KILL #1** | Three pre-write positive controls, one per predicate (the χ cascade routes them apart): `is_snare`+`is_rope` at base 0.90, `is_mountain` at base 0.40, `detect_perspectival_risk` via two `agent_index` facts | **all three fire** — probe is two-sided per predicate; both fact-table channels closed |
| 1 | All-fail × 3 predicates × 6 corpora, per-process controls re-run in each leg | denominators **199/960/960/1005/1001/1106**, matching the loader-glob sizes exactly; all unreachable |
| 2 | `git diff docs/design/design_gaps.md` | GAP-29 present (+59 lines), on the GAP-20 template |
| **KILL #2** | Post-deletion, six goals under `catch/3` | **all six throw `existence_error`** — silent → loud conversion confirmed |
| 3 | `run_pipeline.py` pair | exit 0 both; mtime advanced 02:17:58 → 02:20:47; corpus md5 legs re-checked identical; `per_constraint` **byte-identical** at n=199 (sha256 in `step6_diff_witness.txt`) |
| 4 | `check_stack.pl` vs a pristine HEAD extract (`git archive`, no worktree) | **byte-identical** — no wrong-qualifier regression |
| 5 | `python/load_warning_gate.py` | 3 warnings, 3 allowlisted, **0 unexpected** (never `grep -v Warning`) |
| 6 | `./scripts/gate.sh` | **GREEN** (9/9) |

Note on witness 3: the mtime check is not ceremony — `run_pipeline.py` aborts on its gates *before*
writing `pipeline_output.json`, so a diff taken after an aborted run compares the baseline against
itself and reads byte-identical. Exit 0 **and** an advanced mtime were both confirmed.

## Scope explicitly not covered

- **`omega1_audit.pl`** is itself uncalled and retains the surviving χ-only bander product. This
  change resolved a Pattern-2 fork (two χ-only banders, the weaker retired); it did **not**
  adjudicate `omega1_audit`, which stands open to its own value question.
- **`classify_corpus()` output pairs on the four non-`testsets` legs and `kernel_v1`.** Bought
  instead by running the reachability probe against every leg (probe breadth full, run breadth
  one). A call site reachable *only* under another leg's data *and only* via dynamic dispatch lies
  outside this witness set; the per-leg static scan and per-leg probe stand in for it.

## Collateral corrections (fix-simple-errors)

Three comments named `transition_paths.pl` as a legacy-path member. That was **already false at
HEAD** — it computes `derive_directionality_at → sigmoid_f → scope_modifier` and contains zero
`power_modifier` references. Corrected in `config.pl`, `python/sweeps/bifurcation_sweep.py`, and
`docs/lawvere_glossary.md` (the last was not in the original plan; it cited `power_modifier/2` at a
line number that would have dangled after deletion).

`power_modifier/2` lost its sole reader and was deleted. The six `power_modifier_*` params were
**retained** in `config.pl:57-62` (specs `config_schema.pl:43-48`) as reader-free calibration
anchors for `canonical_d_*` — they serialize into `pipeline_output.json`'s config dump, so deleting
them would have made this an output-changing commit and muddied the run pair doing the
behavior-preservation work.

## Files

- `step0_controls.pl` / `step1_reachability.pl` / `step5_loud_failure.pl` — the three probes
- `step1_results.txt` — per-leg denominators, controls, and all-fail results
- `step3_md5_before.txt` / `step6_md5_after.txt` — corpus fingerprints bracketing the run pair
- `step3_baseline_run.log` / `step6_after_run.log` — both pipeline runs
- `step6_diff_witness.txt` — `per_constraint` sha256 before/after (the 2.8 MB baseline JSON is
  deliberately not committed: it is byte-identical to `outputs/pipeline_output.json`)
