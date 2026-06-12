# OQ-97 close-out — Pattern-6 success-shaped-absorption census (bounded grep, class-based triage)

**Date:** 2026-06-11. **Substrate:** branch `oq97-pattern6-census` from main `1bfd0b72`.
**Method:** recon → proposal (plan `lively-bubbling-cray`) → execution → writeup; class-based
triage per the plan (per-site adjudication of ~600 raw lines was ruled infeasible at proposal).
**Fix policy: file, don't fix** — no engine `.pl` modified; confirmed candidates filed as OQ-112.

## 1. Census denominator (witnessed at execution)

The census covers all top-level `prolog/*.pl` — **106 files** at execution
(`ls prolog/*.pl | wc -l` → 106), a superset of the 47 `stack.pl` load directives
(over-coverage cannot miss sites; per-class members may be not-stack-live).

Subdirectory `.pl` (`belief_battery/`, `probsets/`, `tests/`, `archives/`, `testsets*/`) are
outside the census **as reachable by static load directives**: the module-name-position grep

```
grep -nE "(use_module|ensure_loaded|consult)\(\s*'?[a-zA-Z_]+/" prolog/*.pl   → empty (exit 1)
grep -cE "use_module\(" prolog/stack.pl                                        → 47 (positive control fires)
```

**Accepted limitation (transitive-load residual).** This denominator argument proves no
top-level file loads a subdir module **by static load directive only**. It does not cover a
runtime `consult/1` with a computed path, nor a non-`stack` entry point. Almost certainly
vacuous here, but the claim is scoped to its witness: *static load directives only*, not
"subdirs are outside the census" flatly.

## 2. Shape greps, raw lists, and positive controls (all pasted)

Raw output saved verbatim in this directory. Recon estimates (146/~184/~217) were hypotheses;
the numbers below are what the saved lists contain.

| Shape | Pattern | Raw lines | Files | Raw list |
|---|---|---|---|---|
| A | `;\s*Var (=\|is) <number>` numeric default in else-branch (comment-tolerant) | **160** | 38 | `shapeA_numeric_defaults.txt` |
| B | `findall/setof/bagof/aggregate_all` + aggregate within 3 lines; `aggregate_all(count\|sum\|max…)` direct; setof-with-default | **227** | — | `shapeB_findall_aggregate.txt` |
| C | `;\s*Var = <atom\|compound>` constant default (excl. `true/false/unknown/none`), comment-tolerant | **210** | — | `shapeC_atom_constants.txt` |

**Shape A controls (both fire):**

```
drl_composition.pl:238:    ;   BaseX = 0.5, EpsBacked = false   % :201 fabrication — now FLAGGED, not silent
drl_fpn.pl:206:            ;   Immunity = 0.5         % Default immunity if no type cached
```

**Shape B controls (both fire; vocabulary includes `aggregate_all` and setof-with-default):**

```
narrative_ontology.pl:479:[DIRECT-AGG]:aggregate_all(count, detect_omega(_, _), Count).
report_generator.pl:452:[SETOF-DEFAULT?]:(setof((CM, Err, Sev), drl_core:dr_mismatch(CM, Err, Sev), Errors) -> ... ; format('  - None detected.~n')),
```

**Shape C controls (all three fire — and the controls did their job).** Two grep iterations
were rejected by the pinned controls before the final pattern: a bare-atom version missed the
compound-term default `pass(no_extraction_data)`, and a no-comment-tolerance version missed
defaults followed by a trailing `%` comment. The final pattern (compound-tolerant,
comment-tolerant) fires on all three; Shape A was regenerated with the same comment tolerance
(149→160 lines):

```
boltzmann_compliance.pl:573:    ;   T3 = pass(no_extraction_data)  % Mountains often have ε ≈ 0
drift_events.pl:92:    ;   Trend = stable
intent_engine.pl:80:    ; Pattern = stable).
```

**OQ-41 row-26 cluster located by content** (line drift confirmed): `purity_scoring.pl:57→58`,
`drl_boltzmann_analysis.pl:135/:154` unchanged, `drl_fpn.pl:197→194` (`IP = -1.0`).

**Accepted limitation (Shape-B proximity window).** The 3-line findall→aggregate coupling is a
deliberate bound: it misses the split idiom — a findall whose list is summed in a *different
predicate* or beyond the window. Both pinned B controls are within-window, so passing controls
cannot surface this under-coverage. The census is bounded by design; split idioms are out of
scope — which is, fittingly, the Pattern-6 discipline applied to the census itself: the bound
is declared, not silent.

**Method caveat (sampled class adjudication).** Class verdicts below rest on read witnesses of
1–3 representative members per class (§4 reads pasted/cited), generalized by idiom identity.
A member whose context diverges from its class exemplar may be mis-sorted; member-level re-sort
is OQ-112's job for the candidate classes.

## 3. Strike list (already adjudicated; no re-sort)

| Site | Why struck | Citation |
|---|---|---|
| `purity_scoring.pl:58` (factorization_subscore), `drl_boltzmann_analysis.pl:135` (coupling_factor), `:154` (excess_extraction_factor) | Measured NEUTRAL — 999.9 tripwire, default branch unreachable, 194 baseline rows (corpus-relative, not a design property) | `outputs/tripwire_row26_results.json` (main tree; outputs/ gitignored) |
| `drl_composition.pl:238` | Carries `EpsBacked` provenance bit — flagged, not silent | code comment; OQ-110 lineage |
| `coercion_projection.pl:85` | Comment line documenting the FIXED system_gradient `[] → 0.0` instance (grep artifact) | code comment; OQ-93 |
| `drl_core.pl` — zero Shape-A hits | OQ-44 commit C removed the `get_raw_suppression` `Value = 0` default; **the census itself witnesses the fix** (the old default matched Shape A; drl_core is in the censused set; the grep that fires 160× elsewhere returns nothing in it) | `audits/2026-06-11_oq44_policy_close/` |
| `report_generator.pl:481/:500/:507` | 0.0 defaults print `MISSING (using default 0.0)` — pass carries witness; CONFORMING (not matched by Shape A because the witness changes the shape) | OQ-44 disposition 2 |
| Verdict-banner site | Now a witnessed join (`diagnostic_summary:verdict_join/3`), serialized with raw inputs; absent from all three lists | OQ-98 close; OQ-97 update line 2026-06-11 |

Note: `drl_fpn.pl:206` (`Immunity = 0.5`) and `drl_boltzmann_analysis.pl:302`
(`Reformability = 0.5`) are **not** in the tripwire JSON — they stay in triage (class A6), not
struck.

## 4. Class-based triage

Halt check 1: 19 classes total across the three shapes — well under the ~40-class abort line.
Halt check 2: **no CONFIRMED candidate is live on the classification path** (`dr_type` /
`classify_from_metrics`): drl_core is Shape-A-clean (§3); the signature layer's
`tangled_rope` else-branches (`signature_detection.pl:818/:905`) were read and are fired-signature
override **dispatch** with config ablation switches and the OQ-37 honest-unknown guard — not
absence-defaults. No mid-audit escalation required.

### Shape A classes

**A1 — Guarded rate, denominator co-printed at read site → SOUND.**
Read witness `invertibility_analysis.pl:597–599`: `(Total > 0 -> Rate is ... ; Rate = 0.0)` then
`format('... (~w/~w roundtrips)', [..., Successes, Total])` — coverage carried to the read site.
Members: `invertibility_analysis.pl:597,605,678,685,699,874`;
`maxent_diagnostic.pl:544,545,552,553,560,561,580,581`; `grothendieck_cohomology.pl:258`;
`giant_component_analysis.pl:409`; `boltzmann_compliance.pl:412`; `json_report.pl:783`.

**A2 — Statistic-on-empty → 0.0 (mean/median/slope/fraction/entropy) → CONFIRMED candidate
(report-grade).** The system_gradient precedent class: empty input emits a value byte-identical
to measured-flat. Members: `maxent_report.pl:94,101,288–293`;
`maxent_classifier.pl:248,299,405,665,733,789,878,883`; `trajectory_report.pl:147,157`;
`trajectory_mining.pl:138,186,372,383,398,401,404,417,471,643,779`;
`grothendieck_cohomology.pl:347,382`; `drl_purity_network.pl:332,376`;
`audit3_maxent_compare.pl:164`; `gap_diagnostic.pl:693`; `json_report.pl:443,655`;
`drl_composition.pl:360` (degenerate-denominator slope → 0.0 = measured-flat);
`quantum_verification_report.pl:119,137,393,399`; `inferred_coupling_protocol.pl:170`;
`drift_events.pl:335`; `arakelov_height.pl:77`; `measurement_layer.pl:121`;
`drl_boltzmann_analysis.pl:308`. Some members co-print N (A1-like) — member-level sort in OQ-112.

**A3 — Metric-fallback 0.0 on absent authored metric → CONFIRMED candidate (diagnostic-grade).**
The exact idiom OQ-44 commit C fixed in drl_core, surviving in diagnostic/report consumers.
Read witness `constraint_indexing.pl:892–898` (`get_true_metric`: absent
`base_extractiveness`/`constraint_metric` → 0.0), feeding `observer_accessible` →
`classify_from_restricted` — consumed only by diagnostic_summary's probe layer
(consumer grep: defining file only; positive control `extractiveness_for_agent` fires across
10 files). Members: `constraint_indexing.pl:860,892,895,898`;
`maxent_classifier.pl:254,255,257,578,760,761,763,768,773`;
`invertibility_analysis.pl:111,113,115,539,540`; `omega1_audit.pl:115`;
`genuine_findings_query.pl:69`; `constraint_bridge.pl:30,45,50`.
**Interaction note (new, census-surfaced):** post-OQ-44, `drl_core:get_raw_suppression` succeeds
with the `unknown` **sentinel** — so `maxent_classifier.pl:255/:761`'s `; Supp = 0.0` branches
are now dead and the atom `unknown` flows toward Gaussian-LL arithmetic. Whether a guard
catches it is unverified here → filed in OQ-112.

**A4 — `BaseEps = 0.5` / `Supp = 0` copied pair → CONFIRMED candidate.** Silent copies of the
fabrication that OQ-110 flagged at `drl_composition.pl:238` only. Members:
`boltzmann_compliance.pl:251,257`; `covering_analysis.pl:490,497`; `gap_diagnostic.pl:120,127`;
`omega1_audit.pl:102` (+ companion sentinel `:107` is A5).

**A5 — Out-of-band sentinel defaults → SOUND (provenance-carrying).** Sentinel is
distinguishable at the read site; witnessed filter `giant_component_analysis.pl:509`
(`gc_node_purity(C, IP, _), IP >= 0.0`); `purity_scoring.pl:51` `purity_score(_, -1.0)` same
family. Members: `drl_fpn.pl:109,194`; `fpn_report.pl:91,96,100`;
`giant_component_analysis.pl:353,357,1131`; `trajectory_mining.pl:191`;
`quantum_verification_report.pl:510`; `omega1_audit.pl:107`; `abductive_triggers.pl:894`.

**A6 — Absence-certifies-cleanliness optimistic defaults → CONFIRMED candidate.** Same
semantics as OQ-43's `BeneficiaryCount == 0`: missing data passes the clean/healthy gate.
Read witness `purity_scoring.pl:80/:88` (`CC = 1.0  % No coupling = clean`,
`EX = 1.0  % No extraction data = clean`). Members: `purity_scoring.pl:71,80,88`;
`drl_boltzmann_analysis.pl:302`; `drl_fpn.pl:206`; `covering_analysis.pl:137` (`Rate = 1.0`);
`signature_detection.pl:1090` (`ExcessEps = 0.0`). (Three same-family predicates measured
NEUTRAL by tripwire are struck in §3; these five were not measured.)

**A7 — Zero-contamination on untyped/uncoupled neighbor → candidate (low; needs design
ruling).** "Untyped node contributes no contamination" may be intended semantics, but the
aggregate cannot distinguish it from didn't-look. Members: `drl_fpn.pl:240,245,262,267,285`;
`drl_purity_network.pl:277,282,300,304`; `network_dynamics.pl:108,112,142`;
`boltzmann_compliance.pl:195`; `drl_boltzmann_analysis.pl:220`; `signature_detection.pl:1483`.

**A8 — Count-of-empty after in-scope population step → SOUND/NOISE.** Read witness
`grothendieck_cohomology.pl:218–243`: `maplist(force_obstruction, Constraints)` precedes the
`cached_obstruction` aggregation — didn't-look excluded by construction; count of empty = 0 is
measured. Members: `grothendieck_cohomology.pl:165,243`; `covering_analysis.pl:746`;
`gap_diagnostic.pl:672`; `giant_component_analysis.pl:464,658,694,1044,1274`;
`trajectory_report.pl:254`; `product_site_export.pl:87`; `abductive_report.pl:146`;
`abductive_triggers.pl:471`.

**A9 — Probability of type absent from enumerated distribution = 0.0 → SOUND.** A MaxEnt
distribution enumerates its support; a missing key genuinely means P=0 (measured). Members:
`diagnostic_summary.pl:725,726`; `maxent_report.pl:290,293`; `maxent_classifier.pl:437`;
`maxent_diagnostic.pl:239`.

**A10 — catch(error) → 0.0 → CONFIRMED candidate.** An exception collapses to measured-zero
mass (the errored-unknown trap). Members: `json_report.pl:415,416,417,418` (wasserstein
incomparable mass; the catch arm AND the failure arm both emit 0.0).

**A11 — NOISE (arithmetic/dispatch/config, not defaults-on-empty).** Members:
`isomorphism_engine.pl:37` (authored 0/1 metric); `constraint_indexing.pl:312,337` (sigmoid
piecewise); `arakelov_height.pl:153` (config-default threshold); `data_validation.pl:431`
(condition, not binding); `utils.pl:218`; `diagnostic_summary.pl` n/a;
`grothendieck_cohomology.pl:101–107 region n/a`; `gap_diagnostic.pl:693` listed A2;
`giant_component_analysis.pl:1131` listed A5.

### Shape B classes

**B1 — Collector over directly-defined predicate → SOUND by construction.** `findall` over a
defined rule IS the look; empty result = measured-empty. The large majority of the 227 rows
(all `member/2`-driven re-collections, `narrative_ontology:` fact scans, `config_violation/1`,
omega1/gap region scans, report row re-shaping).

**B2 — Collector over asserted cache populated in another stage → candidate (low).** Empty
cache = never-built collapses with measured-none unless the read site carries coverage; the
OQ-93 findall-over-partial-levels precedent lives here. Sound exemplar witnessed (grothendieck
forces in-predicate, A8). Members to re-check in OQ-112 only if their report path can run
without the build step: `covering_analysis.pl:124,368` (`cached_grid_sig`);
`trajectory_mining.pl:498,861,871,887,889,978,1002` (`trajectory_cached`/`family_assignment`);
`maxent_diagnostic.pl:129` (`maxent_dist`); `invertibility_analysis.pl` `inv_*` rows;
`giant_component_analysis.pl` `gc_*` rows.

**B3 — `aggregate_all(count, …)` as gate/report input over defined rules → SOUND/NOISE.**
Count source is computed logic, not a possibly-unauthored table (`config_validation.pl:36,52,280`;
`data_validation.pl:310,313`; `validation_suite.pl:122,123`; `reading_diff_census.pl:72,73,97`;
`data_repair.pl:335–352`; `diagnostic_summary.pl:709–712` — the last two are the
provenance-bucket counters, themselves the Pattern-6 fix shape).

**B4 — setof-with-default (`SETOF-DEFAULT?` rows) → SOUND with declared bound.** `setof` fails
on empty → explicit absence token printed (`None detected.`); generators are live computed
predicates (measured). Members: `report_generator.pl:71,452,454`; `coercion_projection.pl:73`
(the OQ-93 fix itself — returns `open(no_gradient_data)`); `constraint_bridge.pl:70`.

### Shape C classes

**C1 — `null` on absent (json_report export layer) → SOUND.** `null` is the
provenance-carrying absent token — the prescribed fix shape. Members: `json_report.pl:206,215,
234,243,252,262,271,281,355,364,895,922–928,984,1019,1030,1079,1085,1432`.

**C2 — `na`/`missing`/`error`/`no_type` tokens → SOUND (provenance-carrying).** Members:
`maxent_diagnostic.pl:144,145,146,148,155,266–274,339,348–355,632`;
`fingerprint_report.pl:120,121,122`; `audit3_maxent_compare.pl:113,127`;
`omega1_audit.pl:120,393,913,918,919,920`; `inferred_coupling_protocol.pl:502`.

**C3 — `open`/`inconclusive`/`undersampled`/`insufficient` honest-absence tokens → SOUND.**
Members: `coercion_projection.pl:129,131`; `pattern_analysis.pl:38`;
`trajectory_mining.pl:176,183,213,218,566,842`; `boltzmann_compliance.pl:102`;
`diagnostic_summary.pl` `inconclusive`/`unavailable` rows (`:165,183,224,239,250,276,295–312,
340,359,361`); `stakeholder_seats.pl:139,151,155`; `signature_detection.pl:1490`;
`reading_diff.pl:196,258`; `axiom_diff.pl:83,132,173`; `cs_kernel_registry.pl:130`;
`sheaf_analysis.pl:102,111`; `audit3` per C2.

**C4 — Success/benign-shaped atom on absence → CONFIRMED candidate (three sub-families).**

- **C4a — `; Signal = agrees` on absent probe input** (`diagnostic_summary`). Read witness
  `probe_abductive` (`diagnostic_summary.pl:190–199`): no `abd_triggers` fact (loader never ran,
  or catch swallowed) → `Signal = agrees` — vacuous agreement indistinguishable from
  checked-and-consistent, feeding the OQ-98 verdict join as absence-of-alert. The honest tokens
  (`inconclusive`, `unavailable`) already exist in the same file's probe vocabulary (10 uses).
  13 `; Signal = agrees` sites total; some are legitimate agrees-on-no-conflict — member-level
  sort (data-absence vs conflict-absence else-branch) is OQ-112's first job. Members:
  `diagnostic_summary.pl:163,196,198,210,212,237,272,274,297,300,326,328,357`.
- **C4b — blind = stable trend family.** Insufficient temporal data reads as measured-stable
  (the sweep-primitive precedent: a coverage field was added to perturb.py for exactly this
  confusion). Members: `drift_events.pl:92,437` (`Trend = stable`, `Acceleration = constant`);
  `intent_engine.pl:80`; `pattern_analysis.pl:37`; `logical_fingerprint.pl:338`.
- **C4c — `pass(no_*_data)` / `no_scaffold_needed` on absence.** The pass token carries a
  reason argument (provenance present) but read sites matching `pass(_)` collapse it; scaffold
  assessment defaults to the benign verdict when type lookup fails. Members:
  `boltzmann_compliance.pl:573`; `signature_detection.pl:1166,1176`;
  `drl_boltzmann_analysis.pl:229–238,659`; `drl_counterfactual.pl:189,191`;
  `stakeholder_seats.pl:117`.

**C5 — NOISE (dispatch/binning on computed values).** Members: `abductive_report.pl:444`;
`constraint_bridge.pl:75,95`; `boltzmann_compliance.pl:100,557–590,609` (fail tokens are
alarm-shaped, honest); `drl_boltzmann_analysis.pl:188,609–633`; `covering_analysis.pl:219,699`;
`gap_diagnostic.pl:173,333,629`; `global_delta_report.pl:75–79,106,251`; `json_report.pl:705`;
`intent_engine.pl:85,89` (confidence downgrade = fail-toward-caution);
`maxent_classifier.pl:240,241,781,782`; `reading_diff.pl:151,154,217`; `axiom_diff.pl:121,123,141`;
`data_validation.pl:167,265`; `diagnostic_summary.pl:155–161,174–180,208,220,232–248,308,310,
642,647` (alarm-shaped or OQ-98 vocabulary); `drl_counterfactual.pl:44,79`;
`network_dynamics.pl:218` (alarm-shaped); `signature_detection.pl:271,514–641 (confidence
downgrades),818,905 (override dispatch, read §4 head); `report_generator.pl:387`;
`purity_scoring.pl:67`; `drift_events.pl:491`; `omega1_audit.pl:455,532`.

## 5. Findings table (confirmed Pattern-6 candidate classes → OQ-112)

Prioritized by success-shapedness per the OQ-44 common-law ruling:

| # | Class | Grade | Why it leads |
|---|---|---|---|
| 1 | C4a agrees-on-absence probe signals | verdict-input (feeds OQ-98 join as absence-of-alert) | green-over-absence one level below the fixed banner |
| 2 | A6 absence-certifies-cleanliness | diagnostic | absence passes the clean gate (OQ-43 semantics) |
| 3 | A4 BaseEps=0.5/Supp=0 copies | diagnostic | known fabrication, flagged at 1 of 5 sites only |
| 4 | A3 metric-fallback 0.0 (+ `unknown`-sentinel interaction) | diagnostic | OQ-44-fixed idiom surviving downstream; possible dead branches + atom-in-arithmetic |
| 5 | C4b blind=stable | report | measured-stable token on no-data |
| 6 | A2 statistic-on-empty → 0.0 | report | system_gradient twin, breadth |
| 7 | A10 catch-error → 0.0 | report | errored-unknown collapse |
| 8 | C4c pass(no_data)/no_scaffold_needed; A7 zero-contamination; B2 cache collectors | low | provenance partially carried / design-arguable / build-step-adjacent |

None is on the `dr_type` classification path (halt condition checked, §4 head).

## 6. Verification summary

- Positive controls: pasted §1–§2 (denominator pair; 2×A, 2×B, 3×C; consumer-grep control §4 A3).
- Denominators are run outputs (160/227/210 vs recon's 146/~184/~217).
- No engine `.pl` modified — `git diff --stat` witnessed at commit (ISSUES.md, KNOWN_STATE.md,
  audits/ only).
- Checkers green pre-commit and post-merge (pasted in commit/merge messages).

## 7. Accepted limitations (named, not plan changes)

1. **Transitive-load residual** (§1): denominator claim scoped to static load directives;
   runtime computed-path consult and non-stack entry points unexamined.
2. **Shape-B proximity window** (§2): 3-line coupling window is a declared bound; split
   findall→aggregate idioms are out of scope and the within-window controls cannot detect that
   under-coverage.
3. **Sampled class adjudication** (§2): class verdicts generalize 1–3 read witnesses per idiom;
   member-level divergence is possible and OQ-112 owns the re-sort for candidate classes.
