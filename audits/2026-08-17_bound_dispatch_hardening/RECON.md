# RECON — bound-dispatch fail-loud hardening (Phase 1, read-only deciding pass)

No engine files were edited in this phase. Every explorer claim consumed here was
re-witnessed against the substrate before being cited (re-witness column per item below).

**Census HEAD dating (operator amendment, 2026-08-17):** the definition-site census and
every membership list below were produced at HEAD `9a5d8526` (OPEN stamp `6f42b67a` + two
Phase-0 audit commits; no engine file differs between them). OQ-301 (giant_comp re-entry)
is ACTIVE at this HEAD — if that re-entry later touches dispatch predicates, the class-B
list ages; re-run `prolog/dispatch_head_check.pl` rather than trusting this snapshot.

## 1. The census instrument (Phase-1.1, same artifact Phase 3 wires as a gate row)

`prolog/dispatch_head_check.pl` — reads source via `read_term` (never loads modules),
honours in-file `op/3`, counts per-term syntax errors (`DHC_READERR`, fail-visible),
fails loud on a 0-file glob (Pattern 5). Criterion: predicate with **>= 2 clauses whose
head carries an atom in the LAST argument position, plus >= 1 cut**. Declared
assumptions/limits, each recorded rather than silent:
- **output = last argument** by engine convention. Checked against the hand table: all
  17 hand-table rows have last-position outputs and all 17 fired (see §2 for why that
  agreement is weak evidence). A predicate with a non-last output escapes.
- atoms only (`atom/1`); numeric-constant heads would need their own adjudication.
- scope `prolog/*.pl` non-recursive (124 engine files; testsets/tests out of scope).
- input-key vs output at the flagged position is NOT statically decidable; the caller
  sweep (§3) + per-item adjudication layer that on.

**Discrimination record (3 of 4 runs; 4th = post-fix decline, lands with Phase 3):**
- FIRES on `constraint_signature/2` (signature_detection.pl, 6 atom clauses/7 total) ✓
- FIRES on pre-fix `classify_from_metrics/6` (drl_core.pl, 9 atom clauses/10 total) ✓
- DECLINES on `dr_type/3` (0 DHC_HIT lines for it — the fresh-variable-head +
  unify-after-cut idiom has only ONE atom-headed clause, the terminal fact) ✓
Raw run: `census_checker_run1_HEAD_9a5d8526.txt` (90 hits, 124 files, 0 read errors;
exit 0). Note: 0 read errors is itself uncontrolled in this run — the Phase-3 gate
wrapper adds a syntax-error fixture that must produce DHC_READERR.

### 1b. CENSUS CORRECTION (2026-08-17, same session — marked on close, not silently rewritten)

The Phase-3 wrapper's selftest (zero-cuts fixture) caught a walker bug in run 1:
`sub_term_cut(!)` as a clause head UNIFIES with any unbound variable, so every
non-ground clause counted as cut-bearing. Fixed with an `==` check
(prolog/dispatch_head_check.pl, `body_has_cut/1`); census re-run at HEAD `e16f9c0f`:
**90 → 73 members** (17 cut-free predicates dropped — out of criterion; corrected run
archived as `census_checker_run2_cutfix_HEAD_e16f9c0f.txt`, drop list is the diff of
the two files). Both targets, all hand-table members, and all §4 adjudicated findings
SURVIVE the correction (none of the 17 dropped rows appears in §§2–4). The run-1
`cut_clauses=` counts are unreliable; membership below refers to run 2.

## 2. Checker vs hand table (Phase-1.2)

**Independence caveat (operator, stated before the diff was read):** the checker's shape
criterion was written from the same exploration that produced the hand table, so
AGREEMENT is weaker evidence than it looks; the disagreements are the informative part,
in both directions.
- **Hand-table rows the checker declines: 0.** All ~17 hand-table members fired
  (classify_by_signature/3, composition_rule/3, drift_severity/3, purity_zone/2,
  ep_band/2, fpn_band/2, action_band/2, ep_base_severity/2, chi_subband/2,
  boltzmann_label/2, live_index_label/3, sigma_label/2,
  resolve_with_perspectival_check/4, predict_rope_snare/4, predict_rope_tangled/4,
  predict_snare_tangled/4, predict_three_type/4). So no evidence the criterion is
  narrower than the concept — at the altitude "on this population"; shared ancestry
  means this is consistent with both instruments sharing a blind spot.
- **Checker-only hits: ~70** — the genuine finds. Two carried live bound callers the
  hand table missed entirely (§4: `epistemic_access_check/2`, `cs_verdict/2`).
- **Hand-table corrections found while adjudicating:**
  - "bound goals in `run_pipeline.py:973,:977`" is WRONG twice over: the sites are now
    `:1091,:1095` (drift), and they are **unbound** (`..., Cx, At`) with `memberchk`
    post-filter — the honest form. Removed from the class-A caller set.
  - `constraint_instances.pl:170,:193` are bound calls to `is_tangled_rope`/`is_snare`
    (multi-line, caller-sweep-invisible — found by hand), not direct
    `classify_from_metrics` callers. They bind the alias's own type atom.

## 3. Caller sweep (A/B split input)

`caller_sweep.py` (this dir) — for each of the 90 census hits, single-line regex for call
sites with the last argument a literal atom, over prolog/python/agent/scripts (599
files). Output: `caller_sweep_output.txt`. Positive control: found the six is_X
delegation sites in drl_core.pl. Declared limit: single-line calls only — multi-line
bound calls escape (witnessed: `constraint_instances.pl:170,:193`; found by direct read).
Result: **74/90 predicates have 0 bound-last-atom call sites** (class-B latent
candidates); 16 have >0, each adjudicated by reading (§4).

## 4. Adjudication of the 16 predicates with bound call sites

| predicate | adjudication | evidence |
|---|---|---|
| `classify_from_metrics/6` | **CLASS A — the live exposure.** 12 live bound sites: 6 internal is_X delegations (drl_core.pl:135–170), 6 condition-position sites genuine_findings_query.pl:80–85. (13th sweep hit is a text anchor in check_logic_symbolic_drift.py:78, not a call.) | §5 probe: 311 live disagreement rows |
| `constraint_signature/2` | Class A SHAPE, **0 live bound callers** — all 7 sweep hits are bound_selector_check.py's own sentinel-bounded fixtures. Explorer claim "all live sites repaired" independently confirmed. | caller_sweep_output.txt |
| `epistemic_access_check/2` (boltzmann_compliance.pl:466) | **NEW CLASS A FIND (checker-only).** Own header WARNS bound-`false` always succeeds (2026-06-03, structural_purity precedent) — yet `boltzmann_compliance.pl:577` calls exactly that: `boltzmann_invariant_mountain/2` clause 1 fires for EVERY constraint, so it is unconditionally `inconclusive(insufficient_data)` and its real 4-test body (`:579`) is unreachable. Blast radius: diagnostic surfaces only — consumers are `boltzmann_shadow_audit` (`:139`, fires `Invariant=inconclusive(...)` always) and `drl_boltzmann_analysis:boltzmann_invariant_check/2` (`:176`, itself consumer-less). NOT in the dr_type classification path. Fix is one line (`once(epistemic_access_check(C, S)), S == false`) but OUTPUT-CHANGING on diagnostic surfaces — out of pilot scope, proposed as immediate follow-on with its own witness pair (WRITEUP). | sed reads pasted in session; boltzmann_compliance.pl:466–498, :577, :139 |
| `cs_verdict/2` (cs_pattern_detection.pl) | **NEW latent-in-practice FIND (checker-only).** Live bound goal in `agent/validate_naturalization_gap.py:287` (manual forward-validation script, no automated consumers) + 5 bound fixtures in test_cs_pattern_detection.pl (the OQ-266 red suite, already non-citable). Over-permissive if verdict bodies overlap. Proposed repair: `once(cs_verdict(C, V)), V == false_natural_law_constraint` — not applied (running the script spends generation; repair without run would be an unwitnessed behavior change). | validate_naturalization_gap.py:280–295 |
| `subsystem_available/1`, `ds_subsystem_available/1`, `test/1`, `write_json_number/2` | **input-key / input-value** — the last argument is supplied by the caller by contract (availability key, test name, value to serialize). Bound calls are the intended usage. (`test/1` sweep hits also include generator string templates and one format-string false positive `test(s)` at signature_detection.pl:761.) | caller_sweep_output.txt + reads |
| `classify_deltas/2` (grothendieck_cohomology.pl:543) | bound calls at :714/:716 sit in an if-then-else that REPLICATES clause order (ascending tried first, matching clause 1) → benign today, fragile under reorder. Also called unbound at :528. Adjudicated benign-with-note. | grothendieck_cohomology.pl:700–723 |
| `composition_rule/3` | all 5 sweep hits are PROSE (block-comment lines in dirac_classification.pl) → 0 live bound callers → class B. | caller_sweep_output.txt |
| `claimed_natural/2` | 3 hits all in `python/audits/oq49_override_remeasure.py` — point-in-time audit records (same class bound_selector_check excludes) → class B with note. | caller_sweep_output.txt |
| `is_mountain/3` … `is_piton/3` (6) | wrappers OF the class-A member. External bound-to-own-atom callers (psych_bridge.pl:32–57, constraint_instances.pl:170,:193) are contract-consistent at the wrapper level; the exposure is the INTERNAL bound delegation. psych aliases (`is_substrate` etc.) have **zero callers** repo-wide. | grep pasted in session |

**Class B (latent, no live bound caller): the 74 zero-caller census rows** in
`caller_sweep_output.txt`, plus `composition_rule/3` and `claimed_natural/2` per above.
Conversion is out of pilot scope (writeup proposes; mechanical per this pilot's template).

## 5. Dynamic disagreement probe (Phase-1.3, read-only, corpus-loaded)

`disagreement_probe.pl` (this dir): every `corpus_constraint/1` story × canonical
context; each live alias `is_X(C,Ctx,R)` vs the engine's own first solution
(`once(classify_from_metrics(...))` over identical metrics). Load chain `[stack]` +
`load_all_testsets` — classify_from_metrics is pre-signature/metric-only (§7 body read
confirms no MaxEnt read anywhere in its cascade). Same-path positive control (planted
alias solution routed through the real row emitter): fired, `DP_POSCTL ... OK`.

**Result: NON-EMPTY — the bound callers manufacture classifications on the live corpus.**

| leg | rows | cells probed |
|---|---|---|
| testsets (live) | **311** | 279 × 4 |
| testsets_haiku | 1,093 | 960 × 4 |
| testsets_flash | 1,356 | 960 × 4 |
| testsets_kimi | 1,172 | 1,005 × 4 |
| testsets_sonnet | 762 | 1,001 × 4 |

Live-leg structure (raw lists in `disagreement_probe_<leg>.txt`): 148 `bound=rope
engine=scaffold`, 140 `bound=tangled_rope engine=snare`, 8 `bound=scaffold
engine=mountain`, 8 `bound=rope engine=mountain`, 7 `bound=piton engine=rope` — exactly
the priority-cascade bypass: the constraint satisfies BOTH bodies and the bound call
skips the earlier clause the engine would commit to. Pre-stated asymmetry holds: these
rows are defect witnesses; agreements witness only corpus-contingent exclusivity.

## 6. Negated / condition-position embedding table (Phase-1.4)

| call site | embedding | what a truth-value flip does |
|---|---|---|
| drl_core.pl:135–170 (6, internal delegation) | positive goal + `!` | is_X stops succeeding on manufactured types → per_constraint diffs where §5 rows live (the intended fix) |
| psych_bridge.pl:32–57 (6 aliases) | positive goal inside `with_psych_metric` | psych aliases have zero callers — no downstream flip |
| constraint_instances.pl:170,:193 | positive goal + `!` in demo `constraint_classification/3` clauses (carbon_tax_2026) | demo-constraint classification may flip; NOT in corpus enumeration (`corpus_constraint/1`), so no pipeline surface |
| genuine_findings_query.pl:80–85 (6) | **condition of `( C -> A ; B )`** | printed MATCHES_* diagnostics flip from manufactured true to honest false; manual script, no automated consumer |
| maxent_diagnostic.pl:461 | `\+ (constraint_signature(C, Sig), is_override_sig(Sig))` | **already unbound + post-filter — honest form, unaffected** |
| bound `\+ is_X` / `\+ classify_from_metrics` sites | — | **none found** (grep over prolog/) |

## 7. Clause-body read (Phase-1.5) — what bound callers will now execute

`classify_from_metrics/6`, all 10 bodies read (drl_core.pl:364–459 + helpers
`coordination_dead`, `effective_theater_ratio`, `scaffold_temporality_check`,
`snare_immutability_check`, `natural_law_without_beneficiary`, `base_extractiveness`,
`get_raw_suppression`):
- **(a) throw-capable:** comparisons over BaseEps/Chi/Supp/TR. Supp is guarded by the
  variable-headed clause-1 `\+ number(Supp) -> fail` (runs for bound callers too —
  variable head). BaseEps/Chi are authored/computed numerics; TR is an authored metric
  or the nb_getval-threaded temporal value. **No NEW throw class:** every body already
  executes for every unbound `dr_type` call, so any metric that would throw
  post-transformation already throws today in the cascade path.
- **(b) side effects: NONE** (no assert/retract/output in any body or helper;
  `nb_getval` is a read).
- **(c) cost:** param + fact lookups and comparisons; the real cost question is the loss
  of first-clause indexing on the bound callers — priced by Phase 4's six paired timings.
**⇒ NOT disqualifying.**

`constraint_signature/2`, 6 lock bodies read: routes into `false_natural_law/2`,
`false_ci_rope/2`, `false_summit_mountain/2`, `coupling_invariant_rope/2` — which reach
**memoization asserts** in boltzmann_compliance (`assertz(cached_coupling)` :168,
`assertz(cached_classification)` :237, with retractall cleanup :38–39). Scoped verdict:
**NOT disqualifying** — (i) zero live bound callers exist, so no live call's execution
set changes; (ii) the asserts are idempotent memo-cache fills, semantically transparent
to any future bound caller. Recorded rather than silently waved through: the plan's
disqualification targets writes a live bound caller currently SKIPS; there are none.

## 8. isomorphism_engine.pl:44 dependency (carried to Phase 3)

`cluster_by_signature`'s contract is a bound selector with NO callers today; invisible
to both checker designs. Its safety is CONTINGENT on the `constraint_signature/2` head
transformation landing (Phase 3 step 2 + 3b's site comment); if that step does not land,
this site stays armed and gets its own record instead.

## Decision (end of read-only pass)

Proceed to Phase 2 prereg + Phase 3 pilot on both predicates. The §5 lists mean the
prereg's **non-zero-diff** arm is the expected outcome for `classify_from_metrics/6`
IF any manufactured cell reaches per_constraint output — whether it does depends on
which surfaces consume is_X vs dr_type; the six-leg diff answers that. The
`constraint_signature/2` half remains zero-diff-by-construction (no live bound callers).
