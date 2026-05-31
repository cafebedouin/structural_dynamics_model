# Known State — Session Changelog

This is the dated session log split out of `CLAUDE.md` (2026-05-31) to cut the
auto-loaded instruction file's per-session token cost (~3,050 tokens / 45% of CLAUDE.md
were this section). **It is NOT auto-loaded** — read it on demand.

**Read this file before touching:** `signature_detection.pl`, `product_site_export.pl`,
`enhanced_report.py`, `python/sweeps/perturb.py`, `python/demotion_pass.py`,
`config_validation.pl`, `drl_composition.pl`, or the `corpus_loader` glob. Recent changes
and mitigations to those files are recorded below.

**Standing warnings lifted into auto-loaded `CLAUDE.md` sections** (the tripwire lives there;
full provenance stays here):
- Green cut `product_site_export.pl:75–77` → `CLAUDE.md` Architecture Invariants.
- Run-tagged subdir glob isolation → `CLAUDE.md` Corpus Loading.
- Corpus is 223 not 3,337 / cite the manifest → `CLAUDE.md` Critical Distinctions.

Entries are roughly chronological. New session findings go here (see `CLAUDE.md`
End-of-Session Documentation Review), not in CLAUDE.md.

---

<!-- BODY: verbatim from CLAUDE.md Known State section as of 2026-05-31 -->
- **Corpus is 223 constraints (not 3,337).** The reduction reflects a deliberate rebuild:
  exploratory committer-axis generation runs reused constraint IDs across runs (the
  "chimera" documented in OQ-25 and v7 §5.11 "corpus provenance" note). Cleanup triaged
  collisions, archived stale duplicates, and reduced testsets/ to a single coherent run
  (kernel_run_03: 109 CS readings + ~114 observer-axis constraints). §5.11 trifurcation
  figures are verified single-run coherent. The 3,337 figure predates the rebuild.
- **Run-tagged subdirs (`prolog/testsets/<run_tag>/`) are isolated** — `corpus_loader.pl`
  uses a non-recursive glob (`testsets/*.pl`), so subdir stories are NOT loaded by default.
  This is **load-time** safety, not generation-time dedup. If `corpus_path` is ever changed
  to include a run-tagged subdir, or runs are flattened together, duplicate loading becomes
  live. The shield is the glob; removing it reopens the question.
- Last audit (2026-02-28): passing tests / param sweep — live items migrated to ISSUES.md (OQ-11 – OQ-13); historical record in AUDIT.md
- Config params: see `prolog/config.pl` for current count (`grep -c "^param(" prolog/config.pl`)
- All numeric params inert at ±25%; all 17 directionality constants inert at ±25%
- Corpus is actively growing; param count and testset numbers will drift — cite the manifest
- **2026-05-28: green cut applied to `product_site_export.pl:75–77`** — added `!` after
  `write_one_entry` in `write_entries` clause 3 to enable LCO and fix OOM under
  compressed-ceiling sigmoid variants. Zero-diff verified (3,380 constraints, before/after
  outputs in `outputs/cut_proof_*.json`). Underlying choice-point question is OQ-02 in
  `ISSUES.md`.
- **2026-05-28: python/ phase-1 reorganization** — 8 tests → `python/tests/`, 12 sweeps
  → `python/sweeps/`, 19 audits → `python/audits/`. Frozen CLI commands
  (`run_pipeline.py`, `enhanced_report.py`, `config_sensitivity_sweep.py`,
  `directionality_sensitivity_sweep.py`) and all load-bearing pipeline modules stay in
  `python/` root. ~30 exploratory scripts stay (phase 2 pending). sys.path fixes applied
  to all 39 moved files. Verification script: `python3 python/verify_reorg.py`.
- **2026-05-28: v6 of observers_not_humans paper — §2.3 correction** — Sign-flip is
  load-bearing only in tangled_rope constraint family, not corpus-wide. Empirical
  concentration: Jaccard +0.21 in tangled_rope vs +0.014 in snare+rope (14.6× difference).
  H0 (sign-flip is load-bearing) conditionally confirmed; condition is that rope-gate
  bypass behavior is treated as given (OQ-01 in `ISSUES.md`). Corrected universality-class
  claim from corpus-wide to regime-specific. Unified §2.3 and §3.3 as one mechanism
  (institutional sign-flip at d < d_zero) viewed at two resolutions. Jaccard range
  corrected to 0.697–0.833 from published v5 range 0.685–0.828 (full-corpus rerun,
  3,380 constraints, testsets_3000). See `docs/observers_not_humans_v6.md` and witness
  files `outputs/alt_power_transform_results.json`, `outputs/range_sweep_results.json`.
  OQ-05 and OQ-09 resolved.
- **2026-05-28: OQ-25 resolved — ε coherence load guard** — `config_validation.pl`
  now includes a `config_violation/1` clause that fires inside `validate_config_postcorpus`
  (called at end of `corpus_loader:load_all_testsets`). Rejects any load where the same
  ConstraintAtom carries two distinct `constraint_metric(C, extractiveness, E)` values —
  the chimera failure mode. Grouping key is ConstraintAtom (not KernelAtom; OQ-26
  rationale). §5.11 divergence count confirmed unchanged (79 pairs / 34 kernels).
  See `docs/cs_load_discipline.md` (regeneration protocol) and
  `docs/technical/config_validation_wiring.md` (implementation notes).
- **2026-05-29: kernel-linkage join wired** — `agent/generate_kernel_corpus.py` is now
  canonical (6 evidence signals; `commitment_corpus/generate_kernel_corpus.py` and
  `commitment_corpus/uke_scope_v2_json.md` deleted). Fix applied: `story_uid` now minted
  before `_kernel_id` injection in `process_batch_results` (ordering gate); `stamp_kernel_linkage`
  post-batch function added. Migration script `python/migrate_kernel_linkage.py` wrote
  `cs_contradiction_of` facts into 32 `*_contradictions.pl` files (idempotent, all SKIP on
  second run). 22 orphaned readings listed in bucket B (hand-confirm worklist); 72
  candidate standalones in bucket C (eyeball only). Validation suite: clean after all edits.
  `cross_reading_diff.py` on `end_of_life_decision_authority`: 3 readings, no warnings.
- **2026-05-29: build-discipline patterns documented** — two recurring defects named in
  `docs/technical/build_discipline.md`: produced-but-not-consumed and silent-fork.
  See build_discipline.md for diagnostics and the corpus-you-want naming rule.
- **2026-05-30: Pattern 3 added to build_discipline.md** — bound-probe bypasses clause-order
  (query-binding-bypasses-cut). Bound `findall(C, constraint_signature(C, natural_law), Cs)`
  over-counts by bypassing lock cuts (`false_natural_law:70`, `false_ci_rope:77`,
  `false_summit_mountain:87`). Live demo: bound form yields `[behavioral_competence_reading]`,
  unbound+post-filter yields `[]` (actual sig: false_summit_mountain). Fix: query unbound,
  post-filter with `== natural_law`. See build_discipline.md Pattern 3.
- **2026-05-29: perturb() primitive implemented** — `python/sweeps/perturb.py` is the
  type-stability sweep primitive: `perturb(param, values) → re-export → fold-survival per
  kernel`. Uses Dialect A1 overlay (retract/asserta on config:param/2) + product_site_export
  re-export. Output schema: {fold_survival, stable, flipped, touched, coverage, per_reading}
  per kernel per param value. coverage=0 means "blind, not stable" (param didn't reach
  kernel's decision path at this value). Verified: determinism (byte-identical double-export
  diff=0), identity (snare_epsilon_floor=0.46: 0 kernels affected), detection (0.50:
  end_of_life_decision_authority fold_survival=0.917, coverage=0.167, 39 flips in
  vulnerability_protection_reading institutional contexts tangled_rope→naturalized).
  product_site_export must be explicitly loaded in overlay ([stack] alone does not load it).
  OQ-29 opened: 19/19 results files have no corpus_hash; bifurcation_results.json confirmed
  stale (7 flipping constraints are testsets_3000/ archive only, absent from live testsets/).
  dval_sweep does not exist in repo (grep exit 1). cross_reading_diff.diff() is the design
  model for the diff shape; the primitive has its own re-export loop. 5 type-stability sweeps
  collapse to perturb(); 9 resistant sweeps stay separate by design (see ISSUES.md OQ-29,
  plan file audit-only-do-not-functional-kay.md §6.1).
- **2026-05-29: stability band wired into enhanced_report.py (Phase 1 + Phase 2)** —
  `python/enhanced_report.py` now runs perturb() at generation time for kernel-linked
  constraints with confirmed governing params, renders a stability band section (E5), and
  writes `stability_band` to the JSON sidecar. Confirmed governing param: `snare_epsilon_floor`
  × `end_of_life_decision_authority` kernel (boundary at +8.7%, 39 flips; floor at +4.3%,
  no coverage). All other kernels render "not yet witnessed." Unlinked constraints render "no
  kernel linkage." Architectural finding: 76/97 kernel-linked readings have `false_natural_law`
  signature (unconditional tangled_rope) — chi_floor params reach the metric decision path
  (coverage>0) but the final type is signature-locked; they are NOT valid governing params.
  17/97 have `false_ci_rope` (conditional); 3/97 `coupling_invariant_rope`; 1/97
  `constructed_low_extraction`. `tangled_rope_chi_floor` is blind or signature-locked on all
  tested kernels. Phase 2 restructure: kernel cross-reading panel moved to top (immediately
  after verdict banner); Wasserstein, cohomology, game-theory, Level-3 distribution and
  structural sections deleted (not stubbed; option a taken — git diff 7af6b945 confirms
  five `-def` removals). File: 2670 lines (was 2836; 2698 was mid-session before deletion).
  OQ-31 resolved. Sidecar validator unchanged
  (extra fields pass silently).

- **2026-05-29: predicate denominator established + full 191-param sweep complete** —
  Bidirectional dataflow trace: 191 engine params (168 config.pl + 23 supplementary) +
  6 authored fields = 197 static-type surface. Three surfaces distinguished (static type,
  PoA, temporal/drift). 6 positional_displacement tagged SHADOWED. OQ-32 fixed (6 sweeps).
  Float ±10% batch (179 params): 21 survivors (pre-batch 2 + new 19). Integer ±1 batch
  (19 errored-untested): 3 more survivors (boltzmann_min_classifications, critical_mass_threshold,
  fcr_override_enabled). Total: 24 survivors. All wired into `_WITNESSED_PARAMS` (18 kernels,
  enhanced_report.py) and `_WITNESSED` (demotion_pass.py). Final demotion_pass:
  6 shadowed + 0 errored-untested + 20 unperturbable + 0 reachable-locked + 24 witnessed +
  141 backlog = 191. Results: `outputs/witness_backlog_results.json` (float),
  `outputs/witness_backlog_integer_results.json` (integer). Fisher probe wired into E5
  (all stability-band paths). Priority sort bug fixed. OQ-30 mitigated (18/38 kernels
  witnessed). `docs/engine_handoff.md` §2(a) updated with denominator and survivor section.

- **2026-05-30: 4 epsilon params characterized; all 141 backlog params now exhausted** —
  `--resume` confirmed all 141 PERTURBABLE_UNPERTURBED params already in results (swept at
  end of prior batch due to priority bug; not skipped). Corrected tiering for the 4 epsilon
  params: (1) `rope_epsilon_ceiling` split-tier: +10% permanently blocked by
  `config_schema.pl:482–487` `classification_rope_snare` invariant (`rope_epsilon_ceiling >=
  snare_epsilon_floor` → export_failed); −10% reachable-stable (23 kernels, fs=1.0, 0 flips).
  (2) `tangled_rope_epsilon_floor` perturbable-but-unperturbed EARNED: 25–26 kernels reached
  across full ±10% band, fs=1.0 on all — genuine stability finding. (3) `fpn_epsilon` and
  `piton_epsilon_floor` unreached-at-tested-range: coverage=0 or near-0 at ±10%; flip
  potential unknown; wider range required. Bucket split within 141: 2 unreached-at-tested-range
  (fpn_epsilon, piton_epsilon_floor); 139 remainder (includes rope_epsilon_ceiling one-sided
  and tangled_rope full-band). Top-level 191 count unchanged. OQ-30 updated.

- **2026-05-30: Surface 2 + Surface 3 perturbation primitive scoped (proof-of-life)** —
  Observable identified and proven per surface. Scripts: `python/sweeps/proof_of_life_surface2.py`,
  `python/sweeps/proof_of_life_surface3.py`.
  
  **Surface 2** (`excess_extraction/2`, `boltzmann_compliance.pl`): MOVED. Observable =
  `boltzmann_compliance:excess_extraction(C, ExcessEps)`. Overlay = `config:param/2`
  retract/assertz on `boltzmann_floor_identity_coordination` (0.08→0.60) for
  `civic_eugenic_reading`. Baseline: 0.60, perturbed: 0.08, diff: −0.52. Floor path
  confirmed as coordination_type (not override, not default) — overlay valid, not shadowed.
  Cache confirmed 0 before and after clear. Full primitive observable:
  `excess_extraction(C, ExcessEps)` per constraint per param value. Coverage analog:
  if `boltzmann_floor_for/2` takes the override path, perturbing the floor param is
  shadowed (coverage=0) — same blind-green trap as Surface 1.

  **Surface 3** (`constraint_history/3`, `drl_composition.pl`): NOT MOVED — with diagnostic.
  Observable = `constraint_history(C, Ctx, Timeline)` → `[state(T, Type), ...]`. Overlay =
  `narrative_ontology:measurement/5` retract/assertz (dynamic, confirmed). Constraint
  `civic_eugenic_reading` baseline at T=4: `unknown` (not tangled_rope). Perturbed
  base_extractiveness T=4 (0.68→0.95): Chi=1.30 > snare_chi_floor=0.66 and ε=0.95 >
  snare_epsilon_floor=0.46 — both snare thresholds crossed — yet type remains `unknown`.
  Binding variable: theater_ratio=0.55 at T=4 vs 0.42/0.48 at T=0/T=2; Supp=0.5 fallback
  at all time points. The piton gate (reading theater_ratio via nb_setval) appears to block
  at theater=0.55 without completing, leaving a gap where neither piton nor tangled_rope
  fires. Not-moved is a valid scoping output: observable confirmed, overlay confirmed,
  wrong metric targeted for this time point. Full primitive: use T=0 or T=2 as perturbation
  anchor (baseline tangled_rope) OR include theater_ratio as perturbable metric.

  **Reconciliation of prior-session claim**: "boltzmann_floor_override dead-ends at
  line 453" was correct at Surface-1 granularity (product_site_export never calls
  excess_extraction or boltzmann_floor_for — the control break holds). At Surface-2
  granularity it was imprecise: boltzmann_floor_for/2's output IS consumed by
  excess_extraction/2 and 14+ callers in drift_events.pl, drl_boltzmann_analysis.pl, etc.
  Both claims are true at their respective surface levels.

- **2026-05-30: 6 authored fields graduated from trace-asserted to grep-witnessed +
  perturb-confirmed** — All 6 live on Surface 1 (product_site_export → dr_type/3). Path
  split: extractiveness/suppression/theater_ratio/d_value reach classify_from_metrics/6
  via argument slots (BaseEps, Supp, TR lookup on C arg, Chi); accessibility_collapse/
  resistance reach dr_type/3 via integrate_signature_with_modal/3 (signature override
  layer, called AFTER classify_from_metrics in dr_type/3) — NOT through
  classify_from_metrics/6 arg slots. 197 denominator confirmed. Per-field type flips
  pasted in docs/engine_handoff_4.md witness-tier ledger. Key corpus fact: only 2
  constraints currently get natural_law signature with Sig unbound (as the engine calls
  it): explanatory_closure_mechanism, state_role_time_collapse. Liveness testing for
  AC/resistance requires testsets from this narrow set; most naturally-emerging
  constraints in the corpus get false_natural_law, false_ci_rope, or
  false_summit_mountain (which fire first). See
  docs/technical/signature_detection_wiring.md for query gotchas.

- **2026-05-30: Authoring-closure + fabricated-default census (OQ-33 updated)** —
  Full audit run; all 7 OPEN graduation steps executed. Key corrections to prior claims:
  (1) D1a (drl_composition.pl:179, Supp=0.5): LOAD-BEARING-WRONG confirmed. Tripwire
  yields 279/647 temporal rows changed: 219 tangled_rope→snare + 60 unknown→snare, 0→unknown.
  The plan's instance-reported "443 unknown flips" was WRONG — direction is reversed.
  snare_suppression_floor=0.60 blocks Supp=0.5 from snare; 50.4% of non-unknown temporal
  classifications are systematically mis-classified too low (tangled_rope instead of snare).
  (2) D2 (drl_core.pl:96, Supp=0): DORMANT, not LOAD-BEARING-WRONG. The 32 testsets
  missing suppression_requirement are _contradictions.pl stubs, excluded by
  all_corpus_constraints/1 (requires extractiveness metric). Tripwire: 0 changes on 191
  classified constraints. (3) D20/D21 (boltzmann_compliance.pl:245/251): DORMANT for
  same reason as D2. (4) D1b (drl_composition.pl:180, BaseX=0.5): LATENT-TRAP confirmed —
  fallback unreachable via constraint_history (all measurement time points have BaseX data).
  (5) requires_active_enforcement IS on main classification path (drl_core.pl:371/277/286) —
  A\P gap CLOSED. Scripts: python/sweeps/tripwire_fabricated_defaults.py.
  Results: outputs/tripwire_fabricated_defaults_results.json.
  Audit: outputs/audit_authoring_closure_fabricated_defaults.md. OQ-33 updated.

- **2026-05-31: NL circularity audit — cosmetic relabel, not manufacturing** —
  T.1 (testsets_3000, 3380 constraints): the 404 natural_law-signature constraints
  are 100% bucket A (metric-real mountains). eps range 0.00–0.22, supp range 0.00–0.04,
  all pass both mountain metric gates (eps≤0.25, supp≤0.05) with emerges_naturally.
  Bucket B = 0/404 — the NL→mountain signature override manufactures zero mountains.
  The AC=0.92 authoring stamp is cosmetic: removing the NL override changes the mountain
  count by zero (engine witness: NL=404 before and after strip).
  T.2: prompt `accessibility_collapse ≥ 0.85` threshold stable from first commit
  (`51033e8a 2026-02-21`) through entire testsets_3000 generation window. 84.3% of AC
  values are exactly 0.92 (one stable prompt regime, not drift).
  Generator strip artifacts: `fix/stripped_schema.json` and `fix/stripped_prompt.md`
  remove AC.minimum=0.85 and resistance.maximum=0.15 from the mountain allOf branch
  and matching prompt instructions; keep `extractiveness.maximum=0.25`,
  `suppression.maximum=0.05`, `emerges_naturally` intact. `ab_test/stripped_*` files
  over-strip (also remove ε and supp constraints) — do not reuse.
  Engine-insensitivity witnessed; generation-side stamp removal requires a live
  generation run with DR_GEN_PROMPT/DR_SCHEMA pointing to `fix/` artifacts.

- **2026-05-31: Empty-table pattern scoped (affects_constraint / intent_power_change)** —
  Both tables are empty across all of testsets_3000 (no interval data in the new corpus
  format). 10 distinct engine consumers identified via grep on prolog/*.pl. Two are
  SILENT-SAT; eight are SKIP-safe. SILENT-SAT consumers:
  (1) `signature_detection:count_power_beneficiaries/2` — returns Count=0 for every
  constraint, so `BeneficiaryCount==0` in `natural_law_signature` is vacuously
  satisfied corpus-wide. Bailed out: NL gate is cosmetically redundant (T.1 result).
  (2) `data_verification:verify_interval_completeness` — `forall(intent_beneficiary_class,
  intent_power_change)` vacuously succeeds; test-harness-only, not classification pipeline.
  No live classification bugs from empty tables. All eight SKIP-safe consumers either
  fail-and-backtrack or return empty findall lists with correct downstream behavior.
  Key architectural distinction: `natural_law_signature` checks BC via
  `count_power_beneficiaries` (reads `affects_constraint`/`intent_power_change`,
  EMPTY); `false_summit_mountain` checks beneficiaries via `constraint_beneficiary/2`
  (static authored facts, POPULATED for the 15 FSM targets). These are DIFFERENT
  predicates — FSM firings are real and unaffected by the empty interval tables.

- **2026-05-31: Build discipline Pattern 3 in live audit** —
  Calling `constraint_signature(C, natural_law)` with Sig BOUND bypasses the priority
  cascade (FNL/FCR/FSM clause heads fail to unify → bodies never run → cuts never fire).
  Bound form found 432 "NL" constraints; unbound form found 404 (the correct engine
  count). The 28-gap constraints get FNL or FCR in the real cascade but pass the NL
  body when queried directly. Always call `constraint_signature(C, Sig)` with Sig
  UNBOUND and post-filter for `Sig == natural_law`. Documented in
  docs/technical/signature_detection_wiring.md query gotchas.

- **2026-05-31: NL beneficiary gate is satisfy-on-absence, not belt-and-suspenders (OQ-43)** —
  Gap check (testsets_3000): of the 404 `natural_law`-signature constraints, **0/404** carry a
  `constraint_beneficiary/2` fact (corpus holds 6739, none on the 404) and **0/404** carry an
  `intent_power_change` beneficiary. `intent_power_change` is empty corpus-wide (0 facts), so
  `natural_law_signature`'s `BeneficiaryCount == 0` gate (`signature_detection.pl:295`) passes by
  absence for every constraint — dormant-over-empty-table, not a discriminating check. FSM coverage
  of the NL population is **0/404 by cascade construction** (FSM at `:87` requires a beneficiary fact
  and catches every beneficiary-bearing mountain before the NL clause at `:97`; the NL residue is the
  beneficiary-blind set). The `:84–86` source comment claiming FSM makes the NL gate "belt-and-
  suspenders" was **corrected** — it was false for the 404. The 404 NL certifications mean "no
  beneficiary **authored**," not "no beneficiary **exists**"; activating the gate is a content
  re-audit of the 404, not engine maintenance. Same satisfy-on-absence class as OQ-41 (G6 0.5
  defaults) and OQ-36/OQ-37 (empty `intent_*`) — policy decision (fail-closed vs keep-vacuous-pass)
  should be made once across the class. See ISSUES.md OQ-43.
