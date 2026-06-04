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
`fingerprint_report.pl`, `trajectory_report.pl`, `abductive_report.pl` (census 2026-06-04;
`grep -L "^:- module" prolog/*.pl`). So the pipeline phases run with a richly-populated `user`
module, and **REPL behavior is NOT pipeline behavior** for wrong-qualifier bugs.

**Witnessed instance (OQ-57, resolved 2026-06-04):** `drift_events.pl:230` called
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

---

## 5. Diffing pipeline_output.json: determinism, ripple, and the manifest

- **The pipeline is deterministic**: a same-code re-run produces a byte-identical
  `per_constraint` (witnessed 2026-06-03: 0/1107 rows differ). Use this as the attribution
  probe: if old-vs-new differs but new-vs-rerun doesn't, the drift is a real effect of your
  change, not noise.
- **Corpus-fitted fields ripple corpus-wide on ANY single reclassification**: changing one
  constraint's type moved `raw_maxent_probs` / `arakelov_height` / `wasserstein_*` on all 1106
  rows (max |Δ| ~0.036, zero top-type flips). Diff at TWO levels and report both: the
  classification level (`perspectives`/`signature`/`claimed_type` — where "exactly N rows"
  expectations live) and the full-record level (where the ripple shows). A 1106-row
  full-record diff is NOT automatically a stop signal; an unexpected classification row is.
- **The manifest is injected by `run_pipeline.py`, not by `run_json_report`**: re-running the
  swipl export goal directly rewrites `pipeline_output.json` WITHOUT a `manifest` key. If you
  re-export for a diff, preserve the manifest-bearing artifact and restore it (or accept the
  provenance loss knowingly).
- `per_constraint` has 1107 entries vs manifest `n_constraints` 1106: `constraint_instances.pl`
  contributes a demo constraint (`carbon_tax_2026`) outside `testsets/`.
