# SWI-Prolog load-path and in-session probe gotchas

Scope (per the `docs/technical/` charter): things that caused real bugs or real diagnostic
confusion, with the witnessed instance for each. All instances 2026-06-03/04 (agency-gate,
OQ-57, OQ-63 sessions); full provenance in KNOWN_STATE.md entries of those dates.

---

## 1. Module-qualified calls to undefined predicates are LOAD-PATH-DEPENDENT

**The bug class:** `some_module:pred(X)` where `some_module` never defines `pred` does not have
one behavior. It has (at least) two, selected by which files happen to be loaded:

- **Strict path** (e.g. `swipl -g "[stack], [validation_suite], ..."`): the predicate is
  undefined in the named module → `existence_error(procedure, some_module:pred/1)` thrown at
  call time.
- **Inherited path**: SWI modules inherit from `user`. If ANY **non-module file** in the load
  chain did `use_module(home_module)` — its imports land in `user` — then
  `some_module:pred(X)` silently resolves through user-inheritance to the imported predicate.
  `predicate_property/2` reports it as `imported_from(home_module)`.

**Why this matters here:** every pipeline report entry point is a non-module file —
`json_report.pl`, `orbit_report.pl`, `fpn_report.pl`, `maxent_report.pl`,
`fingerprint_report.pl`, `context_profile_report.pl`, `abductive_report.pl` (census 2026-06-04;
`grep -L "^:- module" prolog/*.pl`). So the pipeline phases run with a richly-populated `user`
module, and **REPL behavior is NOT pipeline behavior** for wrong-qualifier bugs.

**Witnessed instance (OQ-57, resolved 2026-06-04):** `metric_drift_events.pl:230` called
`narrative_ontology:requires_active_enforcement/1` (predicate lives in `domain_priors`). The
suite path threw and aborted the whole drift scan; the pipeline path silently resolved to
`drl_core`'s bridge via `json_report.pl`'s user-imports and produced CORRECT drift events for
months. The contradiction (pipeline JSON had events the throwing clause "couldn't" produce) was
the tell.

**Diagnostics:**
- Where does a qualified call actually resolve?
  `forall(predicate_property(m:p(_), P), writeln(P))` — look for `imported_from/1` and `file/1`.
- Which module in a load chain creates the resolution? Bisect the `-l` chain:
  load prefixes incrementally, probe `predicate_property(m:p(_), defined)` after each.
- **Rule: test a module-resolution hypothesis on the CONSUMER'S exact load path**, not in a
  convenient REPL. The two paths witnessed opposite behaviors for the same line of code.
- **Smoothed (2026-06-04):** `prolog/check_stack.pl` runs library(check) over the full stack
  (`cd prolog && swipl -l check_stack.pl -g "run_check_stack, halt" -t "halt(1)"`), surfacing
  undefined-predicate references of exactly this class as a command instead of forensics.
  Compare against the recorded baseline (KNOWN_STATE.md 2026-06-04) — new findings are
  regressions.
- **Coverage is bounded to the LOADED IMAGE — a clean run is not "no wrong-qualifier rot
  anywhere" (silent over-trust trap).** library(check) only inspects modules present in the
  image, so any module `run_pipeline.py` loads in a SEPARATE process *outside* `[stack]` escapes
  the check entirely. Witnessed 2026-06-25: `context_profile_mining.pl` called the removed
  `dirac_classification:standard_context/1` for an unknown duration — the production path is
  disabled (`trajectory_enabled=0`) and the module isn't in `[stack]`, so neither the pipeline nor
  check_stack ever exercised it (fix `fc9b4688`). **Now covered:** check_stack loads the
  trajectory-mining chain before `check/0` (commit `a82d7ed0`; positive-controlled — reintroducing
  the bug makes it fire; baseline unchanged). **Still uncovered (honest boundary):** the other
  standalone report scripts (`abductive_report`, `orbit_report`, `fingerprint_report`,
  `isomorphism_report`, `maxent_report`, `global_delta_report`, `fpn_report`,
  `quantum_verification_report`, …) — several consult into `user`, so co-loading them into one
  image cross-contaminates with redefinition false-positives that never occur in production (one
  process each). A faithful per-chain check needs a fresh process per chain (shell loop). So:
  before trusting a clean check_stack for a side-chain script, confirm that script is actually in
  the loaded image — `current_predicate(M:P)` for one of its predicates.

---

## 2. `setup_call_cleanup/3` defers Cleanup while the Goal holds choicepoints

Cleanup runs when the goal completes **deterministically** (or fails/throws). A goal that
succeeds with choicepoints (a `findall` composed with member checks, an unwrapped engine query)
has NOT completed — so code sequenced *after* `setup_call_cleanup/3` can run **before** the
cleanup, while the setup's state mutation is still live.

**Witnessed instance:** `prolog/tests/test_agent_beneficiary.pl` — the :287 inertness test
swapped a predicate definition in setup and restored it in cleanup; the post-restore dispatch
control ran while the swap was still active (restore deferred behind goal choicepoints) and the
test failed mysteriously. **Fix: wrap the goal in `once/1`.** The test file carries the comment.

---

## 3. Test-local redefinition of static predicates (the swap/restore pattern)

SWI allows `abolish/1` on static predicates (default `iso` flag false); after
`abolish + assertz` the predicate is dynamic for the rest of the process. Pattern used by
`test_agent_beneficiary.pl` for raw-vs-filtered comparison:

- Swap: `abolish(m:p/1), assertz((m:p(X) :- <fully-qualified body>))`.
- Restore: same, re-asserting the ORIGINAL body. Fully qualify every body goal — the clause
  asserted from a test module gets a context wrapper (`plunit_xxx:(...)`), harmless iff goals
  are module-qualified.
- **Three dispatch controls or the probe is blind:** (pre) original behavior witnessed;
  (mid) the redefinition is visible at the consumer's call site — proves the second run is not
  a byte-identical re-read; (post) restore witnessed. See §2 for why the post control can lie
  without `once/1`.

---

## 4. Sequential in-session retract/re-assert probes need PER-ITEM restore verification

Measuring "engine with fact F removed" by `retract → query → assertz` is fine for one
constraint. Measuring a LIST of constraints in sequence is where it goes wrong: a failed
restore on item 1 silently corrupts the measurements of items 2..N, and every individual query
still returns a plausible value (the session-scope version of §2's trap).

**Rules (witnessed in the OQ-63 d/χ probe, 2026-06-04, restore PASS ×11):**
- `findall` the facts FIRST, then retract — never retract while enumerating.
- Verify restore **immediately after each item** (re-query the raw observables and compare to
  the pre-retract snapshot) — not once at the end of the run.
- Filtering is VALUE-level, not host-level: for a host with mixed facts, retract only the
  facts under test. The OQ-63 probe's first control "failure" was the probe retracting ALL
  beneficiaries of a partial host — a probe-design error the control caught, not an engine
  finding.
- Overrides pin values upstream of everything: `directionality_override/3` preempts both the
  structural and canonical d paths, so an override-bearing control host shows "no movement"
  that means OVERRIDE, not "filter has no effect". Enumerate overrides for control hosts too
  (`constitutional_text_authority...:360` was found by anomaly, not by plan).
- **Smoothed (2026-06-04):** `probe_harness:with_retracted/2` / `with_overlay/3` encapsulate
  §§2–4 (snapshot-first, setup_call_cleanup + once, VERIFIED restore that throws
  `probe_restore_failed` on mismatch) plus the §7 cache clears. Fact overlays only; rule
  clauses matching a template are left untouched with a warning. Tested:
  `prolog/tests/test_probe_harness.pl`. Prefer it over hand-rolled retract/assert probes.

---

## 5. Diffing pipeline_output.json: determinism, ripple, and the manifest

- **The pipeline is deterministic**: a same-code re-run produces a byte-identical
  `per_constraint` (witnessed 2026-06-03: 0/1107 rows differ). Use this as the attribution
  probe: if old-vs-new differs but new-vs-rerun doesn't, the drift is a real effect of your
  change, not noise.
- **The complement: a behavior-PRESERVING change never reads byte-identical whole-file — the manifest
  defeats it.** `inject_manifest` stamps `pipeline_run_at` (and flips `code_dirty` on any uncommitted
  edit) every run, so a whole-file SHA/diff across two runs ALWAYS differs even when nothing behavioral
  changed (witnessed 2026-06-25, OQ-18: clean and edited runs differed *only* in `pipeline_run_at`).
  `outputs/` is gitignored, so you cannot diff against a committed baseline, and a baseline from a
  PRIOR run is not a valid comparand (its timestamp/commit differ for reasons unrelated to your
  change). To prove an uncommitted change is behavior-preserving, use **same-session clean-vs-edited**:
  `git show HEAD:path > path` (or move the edit aside) → run → capture; restore the edit → run →
  capture; diff the two with `pipeline_run_at` normalized, or diff `per_constraint` only (the manifest
  does not touch it). This is the symmetric twin of the next bullet's false-PASS: there a real change
  reads identical (stale file); here a behavior-preserving change reads different (live manifest).
- **The diff is only valid if the run REWROTE the file — the aborted-gate stale-output false pass.**
  `run_pipeline.py` runs its gates (load-warning gate, ISSUES status-grammar gate) and aborts
  *non-zero BEFORE* the json write. On an abort, `outputs/pipeline_output.json` keeps its PRIOR
  contents — so a before/after diff compares the baseline against itself and reads **byte-identical,
  a false "behavior-preserving" pass**. Witnessed 2026-06-24: a `*/` inside a `/* … */` Prolog block
  comment (closes the comment early → a clause syntax error swipl tolerates on load but the
  load-warning gate flags) aborted the pipeline; the json was stale and the diff read "identical"
  while the change was actually broken. **Before trusting a byte-identical pipeline diff: assert
  exit code 0 AND that the output mtime advanced** (`stat -c %Y outputs/pipeline_output.json`
  before/after). This is the file-boundary instance of the success-shaped-absorption pattern — a
  write that did not happen is indistinguishable from a write that produced identical output unless
  you check the run actually completed.
- **Corpus-fitted fields ripple corpus-wide on ANY single reclassification**: changing one
  constraint's type moved `raw_maxent_probs` / `arakelov_height` / `wasserstein_*` on all 1106
  rows (max |Δ| ~0.036, zero top-type flips). Diff at TWO levels and report both: the
  classification level (`perspectives`/`signature`/`claimed_type` — where "exactly N rows"
  expectations live) and the full-record level (where the ripple shows). A 1106-row
  full-record diff is NOT automatically a stop signal; an unexpected classification row is.
- **Single-writer (changed 2026-06-04):** `run_json_report` writes
  `pipeline_output.raw.json`; `run_pipeline.py`'s manifest step is the SOLE writer of the
  canonical `pipeline_output.json` (raw + manifest). A direct swipl re-export rewrites the
  raw file only — it can no longer clobber the canonical artifact's provenance (witnessed:
  re-export after a pipeline run left the canonical md5 unchanged). Historical note: before
  this change, both wrote the same filename and a direct re-export silently destroyed the
  manifest.
- `per_constraint` has 1107 entries vs manifest `n_constraints` 1106: `constraint_instances.pl`
  (loaded by `stack.pl:13`) contributes a demo constraint outside `testsets/`. **The surfacing
  demo is `catholic_church_1200`, not `carbon_tax_2026`** (witnessed 2026-06-04: per_constraint
  ids ∖ testset ids = {catholic_church_1200}, exactly one element — OQ-70 Probe 0,
  `audits/2026-06-04_fnl_bait_confound/fnl_probe0_file_constraint_map.json`). Both demos exist
  in the file; catholic_church_1200 enters the export via its authored
  `constraint_classification/3` CLAUSES — it has **zero** `constraint_metric/3` facts even
  after corpus load (witnessed: `findall` = `[]`). carbon_tax_2026 defines only `drl_core:*`
  facts and does not surface. Consequences: (a) exclude `catholic_church_1200` from any
  corpus statistic computed over per_constraint (it is also the one `claimed_type: None`
  row); (b) which demo your probe sees depends on the enumeration predicate — a
  `constraint_metric/3` sweep never sees it, a `constraint_classification/3` sweep does.
  **Closed for runs from 2026-06-04 on:** the export now enumerates
  `corpus_loader:corpus_constraint/1` (the membership registry asserted per loaded testset),
  so per_constraint == manifest n_constraints (witnessed: 1106 == 1106; removal diff was
  exactly the demo row with ZERO classification- or full-record-level collateral). Outputs
  from EARLIER runs still carry the extra row — check `manifest.pipeline_run_at`.

---

## 6. Validated in-session signature sweep (reproduces the pipeline exactly)

A one-shot probe that re-derives every constraint's signature in-session, witnessed
byte-equal to the pipeline's per_constraint signatures (0/1106 mismatches, 2026-06-04,
OQ-70 Probe 1):

```prolog
% from prolog/:
:- [stack], corpus_loader:load_all_testsets.
setof(C, M^V^(narrative_ontology:constraint_metric(C, M, V), atom(C)), Cs0),
exclude(==(catholic_church_1200), Cs0, Cs),          % defensive; see §5
forall(member(C, Cs),
       ( signature_detection:constraint_signature(C, Sig) -> ... ; Sig = none )).
```

Rules that make it faithful:
- **`Sig` must be UNBOUND** — `constraint_signature(C, false_natural_law)` with the atom
  bound bypasses the lock cuts and lies about clause order (Pattern 3 /
  build_discipline.md). One solution via `->` matches what the pipeline records.
- **Enumerate ids via `corpus_loader:corpus_constraint/1`** (the authoritative membership
  registry, available since 2026-06-04 — same enumeration the export now uses). The older
  `constraint_metric/3` setof recipe still works (testset ids are 1:1 with files; `atom(C)`
  guard drops list-wrapped ids) but is a derived proxy, not the registry.
- Before trusting any downstream analysis, paste the consistency check: in-session signature
  counts vs the pipeline JSON. If they differ, your load path differs from the pipeline's —
  stop there (§1).

---

## 7. Boltzmann memo caches make in-session overlay/retract probes read STALE — clear them, then prove the recompute ran

`boltzmann_compliance` memoizes per-constraint results in two dynamic predicates:
`cached_classification/3` (12 grid cells per constraint) and `cached_coupling/2` — and five
other modules carry memo caches of their own (covering_analysis, grothendieck_cohomology,
drl_fpn, context_profile_mining, arakelov_height). Any in-session retract/assertz overlay that
should change a memoized result **silently reads the pre-overlay cache** unless cleared.

**Smoothed (2026-06-04):** clear them ALL with one call —

```prolog
cache_registry:clear_all_caches.   % multifile clear_hook/0 per caching module
```

(`probe_harness:with_overlay/3` does this automatically, before the goal and after restore.
maxent_* state is deliberately NOT cleared — it is fitted model state, re-established only by
its own fit runner; see cache_registry.pl header.)

The failure is unreadable at the read site: a stale-cache "no change" is byte-identical to a
real null result. **Therefore a corpus-wide counterfactual needs BOTH controls, pasted
before the headline diff is read:**
- **Sensitivity (the null-branch guard):** one pre-selected constraint whose result is KNOWN
  to flip under the overlay, with the predicted destination **named in advance** (derive it
  from the clause chain). "Flipped to something" does not pass; "did not flip" means the
  recompute is stale and the corpus diff is untrusted.
- **Specificity:** a population the overlay must NOT touch, shown unchanged.

Also report the counterfactual as a **destination histogram** (where did each changed row
land), never as an aggregate delta — if the removed mechanism has a sibling clause that
catches the released population, the aggregate reads null and inverts the conclusion.
Witnessed end-to-end: OQ-70 Probe 2 (`audits/2026-06-04_fnl_bait_confound/`), where 809/827
ex-FNL rows migrated to the next clause (FCR) and the FNL+FCR aggregate moved only 1046→1042.

---

## 8. Evidence-term slots give clause-ordered attribution for free

Detector predicates that return structured evidence record WHICH internal source fired,
because the source predicates are cut-ordered: `false_natural_law(C, fnl_evidence(Claim,
…))` puts the FIRST matching `claimed_natural/2` source in the `Claim` slot
(`explicit_mountain_claim` | `indexed_mountain_classification` |
`natural_law_signature_match`); `false_ci_rope(C, fcr_evidence(AppearanceType, …))` does the
same for `appears_as_rope/2`. Sweeping the slot over the corpus is a full attribution probe
with zero instrumentation — no tracing, no predicate swaps. Positive control: a constraint
known to carry the rarest source (e.g. an explicit `constraint_claim(C, mountain)`) must
report that source when queried directly. Witnessed: OQ-70 Probe 1 (827/827 FNL =
`indexed_mountain_classification`; control returned `explicit_mountain_claim`).

---

## 9. Agent-harness cwd persists across tool calls — `cd prolog` poisons later repo-relative paths

For models working in this repo: the Bash tool's working directory persists between calls.
After a `cd prolog && swipl …` call, a later `python3` analysis call using repo-relative
paths (`outputs/pipeline_output.json`) fails with FileNotFoundError — or worse, a relative
glob silently resolves against `prolog/` (the corpus-loading trap in reverse). Witnessed
twice in one session (2026-06-04). Rule: in mixed swipl/python workflows, either prefix
every analysis call with `cd /abs/repo/root` or use absolute paths; never assume the cwd of
the previous call.

**Partially smoothed (2026-06-04):** corpus LOADING is now cwd-independent
(`corpus_loader:resolve_corpus_dir/2` anchors relative `corpus_path` to `prolog/`, and a
0-file glob throws `corpus_empty` instead of silently proceeding). Output WRITES are still
cwd-relative (`../outputs/...` in exporters and probe scripts) — keep `cd prolog/` for
anything that writes; full write-path anchoring is a recorded follow-up (ISSUES.md OQ-69).

---

## 10. Changing a term's (or predicate's) ARITY fails consumers SILENTLY — the fail-don't-error trap

Clause-head unification is the franchise hazard of this whole codebase: a consumer matching
`agree(_)` does **not** match `agree(_, _)` — it **fails**, it does not raise. So adding an argument
to a returned compound term (or to a predicate) makes every *unenumerated* pattern-match consumer
silently yield nothing: a `findall` returns `[]`, an `aggregate_all(count, member(_-agree(_), …))`
returns `0`, a guard quietly skips — and nothing errors, nothing warns. It reads exactly like
"there are no agreements," which is the Pattern-5/Pattern-6 spine (absence presents as presence) at
the term level. This is distinct from §1 (wrong *module qualifier*, also load-path-dependent); here
the qualifier is right and the *shape* is wrong.

**Witnessed (OQ-51 build-extension, 2026-06-25):** enriching the kernel-comparison verdict tokens
`agree(Type) → agree(Type, NUnk)` and `diverge(TypeMap) → diverge(TypeMap, NUnk)` would have left
`json_report.pl:2024`'s `member(_-agree(_), Profile)` matching nothing → `robust_context_count`
silently 0 on every kernel, with a green test suite and a clean pipeline (the exact signature an
arity-fail-match produces).

**Rule — same desync discipline as a renamed JSON key, but invisible because it fails-closed not
loud:** before changing an arity, `grep -rn 'theterm(\|otherterm('` across `prolog/` and update
every pattern-match consumer in the SAME commit; paste the grep as the witness. Watch for
**same-named terms in different modules** (`reading_diff.pl` and `axiom_diff.pl` each have their own
`agree(_, _)` — unrelated to the kernel verdict) — confirm each match reads the structure you are
changing, not a homonym. The wellformed/positive-control test that pins the term shape
(`tests/test_cs_kernel_registry.pl` `compare_profile_verdicts_wellformed`) must move to the new
arity in lockstep, or it's the first thing that goes red — which here is the *good* case, the loud
one.

## 11. Querying a CLASSIFICATION predicate with the key UNBOUND returns a false empty set

`drl_core:dr_type(C, Ctx, _)`, and everything that wraps it (`cs_pattern_detection:cs_verdict(C, V)`,
`drl_composition:classify_at_time/4`, the `transition_path`/`drift_event` clauses that gate on a
type), **classify a bound `C` — they do not GENERATE constraints.** So a probe that leaves `C`
unbound,

```prolog
findall(C, cs_pattern_detection:cs_verdict(C, scaffold_suppression_escalating), Cs).   % WRONG
```

backtracks into `dr_type(C, Ctx, scaffold)` with `C` a fresh variable, finds no solution, and returns
`[]` — which reads exactly like "this verdict never fires / is dormant." This is the Pattern-5 spine
(absence presents as presence) at the query level: the empty result is a fact about your *binding*,
not about the corpus. Always bind the key from the authoritative membership first:

```prolog
findall(C, ( corpus_loader:corpus_constraint(C),
             cs_pattern_detection:cs_verdict(C, scaffold_suppression_escalating) ), Cs).   % RIGHT
```

This is the silent failure mode behind CLAUDE.md's "enumerate `corpus_constraint/1` in probes" rule —
stated here as the concrete trap. **Positive control for any "verdict X never fires / is dormant"
claim:** count some *other* verdict on the identical query shape; if that is also `0`, your query is
dead, not the verdict. **Witnessed (OQ-18 probe, 2026-06-25):** `cs_verdict(C, scaffold_suppression_escalating)`
unbound returned `0` (read as dormant); bound from `corpus_constraint/1` returned `14` on `testsets`
(52 on `testsets_haiku`, 43 on `testsets_flash`) — a false-dormancy conclusion that a one-line
positive-control count reversed.
