# Wiring-Gap Census — prompt ↔ schema ↔ engine disagreements

**Read-only audit. Characterization only — no gap is resolved here.** Cruft-vs-wire
adjudication is a separate session; the one allowed flag is "low-stakes, clear direction."

## Manifest

- Date: 2026-05-31
- git HEAD: `220739b8` (working tree dirty: 19 files)
- Live corpus: `prolog/testsets/*.pl` = **226 files** (what the engine loads)
- Archive corpus: `prolog/testsets_3000/*.pl` = **3380 files**
- Canonical schema: `python/constraint_story_schema.json`; compiler: `python/generate_constraint_pl.py`
- Every count below is re-derived by grep (raw witnesses in the appendix). Phase-1 Explore
  reports were treated as hypothesis; one load-bearing error was caught and corrected (see §6).

## Gap types

G1 prompt/schema demands a field the engine never consumes (or consumes only inertly);
G2 engine consumes a field/table the author never fills **and** the corpus is empty in **both**
corpora; G3 engine computes something nothing downstream consumes; G4 prompt states a rule no
engine predicate enforces/reads; G5 two engine paths read different representations of the same
metric (scalar vs temporal) such that they can disagree; **G6 a path imputes a fixed value for
absent data** (fail-closed-vs-impute correctness question — distinct from G5, the highest-stakes
category because the engine then computes on a number nobody authored).

---

## Census table

| # | gap | type | layers referencing | actual consumer(s) — both axes | authoritative layer | wire-cost | strip-cost | cross-axis note |
|---|-----|------|--------------------|-------------------------------|---------------------|-----------|-----------|-----------------|
| 1 | `mandatrophy_resolved` authored boolean | G1 | prompt, schema (`allOf[7]`, required `=true` at ε>0.70) | **none** — compiler emits no fact for it; engine uses a *separate* hardcoded `is_mandatrophy_resolved/1` (2 names: `gale_shapley`, `planetary_boundaries`) read at `narrative_ontology.pl:329` | undeterminable (engine has a parallel hardcoded path; authored value is orphaned at the compiler seam) | med (emit + wire to detect_omega) | low (schema gate only) | observer-only concept; not on committer axis |
| 2 | `accessibility_collapse` scalar | G1 (consumed-but-classification-inert) | prompt, schema (mountain ≥0.85), compiler emits | sole classification reader `signature_detection.pl:136` via `get_metric_average` → the NL signature, which T.1 proved **cosmetic** (removing NL override changes mountain count by 0) | engine-design leans CRUFT *for classification*; field still documents NL-profile intent | low | low | observer-only; a *separate* compound temporal form is row 12 |
| 3 | `resistance` scalar | G1 (consumed-but-classification-inert) | prompt, schema (mountain ≤0.15), compiler emits | sole reader `signature_detection.pl:138` (same cosmetic NL path) | same as row 2 | low | low | observer-only |
| 4 | `reference_frame` → `cs_reference_frame/2` | G1 + G3 | schema (`cs_structure`), compiler emits (`generate_constraint_pl.py:500`) | **none** — appears only at `narrative_ontology.pl:77,97` (declaration); zero read sites | undeterminable (committer drift design may have intended a t0 anchor it never wired) | med | low | committer-axis field, dead on both |
| 5 | `uke_scope.{epsilon_bin,hypothesis,downstream_of}` | G1 (provenance, by-design) | schema | **none** — not compiler-emitted, not engine-read; pure manifest/seed provenance | prompt-intent = provenance only; not a harmful gap | n/a | trivial | neither axis |
| 6 | `commentary.*` block (7 fields) | G1 (documentation, by-design) | prompt, schema | emitted as `.pl` **comment text** (`generate_constraint_pl.py:396+`); `perspectival_gap` emits a `test(perspectival_gap)` plunit test (`:245`). Never read as classification facts | prompt-intent = human-readable narrative; by design | n/a | trivial | neither axis |
| 7 | `intent_*` family — **7 predicates** (`intent_power_change`, `intent_beneficiary_class`, `intent_viable_alternative`, `intent_alternative_rejected`, `intent_suppression_level`, `intent_resistance_level`, `intent_norm_strength`) | G2 | engine only (no prompt/schema field authors them) | read by `intent_engine.pl` + `signature_detection.pl`; **all 0 facts in BOTH corpora**. `intent_engine` is loaded (`stack.pl:43`, empty import) and called only by `report_generator`/`test_harness` — not by classification or committer drift. SILENT-SAT (witnessed): in `count_power_beneficiaries`, conjunct 1 `affects_constraint(I,C)` is **live** (e.g. `retributive_reading` binds 2 sources), but conjunct 2 `intent_power_change(I,_,_)` has 0 facts → join collapses → Count=0 → NL gate `BeneficiaryCount==0` vacuously true corpus-wide. **Reached-but-empty-join, not unreached:** the populated `affects_constraint` supplies join keys, so populating `intent_power_change` *alone* would activate the gate | undeterminable (intent subsystem fully built, never populated) | high (author + schema + prompt for a whole subsystem) | med (delete intent_engine + readers) | dead on **both** pipelines; surfaces only in reporting |
| 8 | `constraint_metric(_, inevitability, _)` | G2 | engine only | read `constraint_bridge.pl:22` (cut + fallback); **0/0 both corpora**; not compiler-emitted | undeterminable | low | low | observer bridge |
| 9 | `constraint_metric(_, internalization_depth, _)` | G2 | engine only | read `psych_bridge.pl:19`; **0/0 both corpora**; not emitted | undeterminable | low | low | observer/report |
| 10 | `constraint_metric(_, resistance_to_change, _)` | G2 | engine only | read `data_validation.pl:300,309`, `json_report.pl:237`, `utils.pl:346`, + `get_metric_average`; **0/0 both corpora**; not emitted | undeterminable | low | low | validation/report |
| 11 | `constraint_metric(_, accumulation_speed, _)` | G2 | engine only | read `utils.pl:211` via `safe_get_metric(..., 0.0, false)`; **0/0 both corpora**; not emitted | undeterminable | low | low | has explicit 0.0 default (not silent) |
| 12 | compound measurement metrics `accessibility_collapse(Level)`, `stakes_inflation(Level)`, `suppression(Level)` | G2 + G5 | engine only | read `coercion_projection.pl:15` (+ `data_repair`/`data_verification` vector form); compiler emits `measurement/5` **only** for `theater_ratio`/`base_extractiveness`/`suppression_requirement` — never the compound forms | undeterminable (stress-test surface) | med | low | distinct representation from row 2's scalar |
| 13 | `predict_transformation/3` | G3 | engine only | **none** — 3 clauses at `drl_composition.pl:256/267/278`, exported `:20`, **0 callers** in prolog, tests, or python | engine-design = dead code | n/a | trivial | neither axis |
| 14 | scaffold rule "suppression must decline over time" | G4 | prompt (`:30`) | **no enforcer** — no `decline/trajectory/monotonic` check in `drl_core`/`structural_signatures`/`signature_detection`; `classify_from_metrics(...,scaffold)` (`drl_core.pl:342`) reads scalar `Chi`/`has_coordination_function`; `scaffold_temporality_check` (`:274`) uses `has_sunset_clause` or absence-of-enforcement | prompt-intent expresses a temporal rule the engine reduces to a static flag | high (needs trajectory read) | low (drop prompt line) | committer drift *does* read trajectories but not for scaffold gating |
| 15 | rule "final measurement value must match `base_properties.extractiveness`" | G4 | prompt (`:295`) | **no enforcer** (grep of `data_validation`/`data_verification` empty) | prompt-intent; unenforced | low | trivial | neither |
| 16 | piton rule "primary function atrophied" | G4 (by-design) | prompt (`:31`) | narrative-only; no metric encodes it (engine uses `theater_ratio≥0.70`) | prompt-intent = authoring guidance | n/a | trivial | neither |
| 17 | rule "theater_ratio rising >0.5 = Goodhart drift" | G4 (partial) | prompt (`:280`) | **partially** enforced — `drift_events.pl` reads `theater_ratio` temporally (committer), but no observer-axis gate | mixed | n/a | n/a | committer-only enforcement |
| 18 | perspective-minimum rules (≥2; powerless+institutional) | G4 (by-design) | prompt (`:199`), schema (`minItems:2`) | enforced by schema + linter, **not** engine | upstream-enforced | n/a | n/a | neither |
| 19 | extractiveness — scalar vs temporal | G5 | engine | `constraint_metric` (observer `drl_core`) vs `measurement/5` (committer `drl_composition`) — can disagree | both live; representation choice | n/a | n/a | **cross-axis**: observer=scalar, committer=temporal |
| 20 | `base_extractiveness` — scalar vs temporal | G5 | engine | scalar `constraint_metric` (rare) vs `measurement/5` (`drift_events`) | both | n/a | n/a | committer temporal |
| 21 | `suppression_requirement` — scalar vs temporal | G5 | engine | scalar (`drl_core`) vs `measurement/5` (`drl_composition`) | both | n/a | n/a | cross-axis |
| 22 | `compute_temporal_stability` reads scalar as time-series | G5 | engine | `signature_detection.pl` folds multiple `constraint_metric(C,M,_)` as a pseudo-trajectory instead of `measurement/5` | engine-design (likely a representation bug) | low | low | NL path (cosmetic) |
| 23 | `drl_composition.pl:179` `Supp=0.5` on absent `measurement(suppression_requirement)` | **G6** | engine | **LIVE, LOAD-BEARING-WRONG** (D1a): tripwire flips 279/647 temporal rows (219 tangled_rope→snare + 60 unknown→snare); `snare_suppression_floor=0.60` blocks Supp=0.5 from snare, so 50.4% of non-unknown temporal classifications mis-classify low | engine-design = fail-open imputation; **strong candidate fail-closed** | low (guard) | low | committer temporal path |
| 24 | `drl_composition.pl:180` `BaseX=0.5` on absent `measurement(base_extractiveness)` | **G6** (latent) | engine | latent — all current measurement points carry BaseX, so fallback unreached | same as 23 | low | low | committer temporal |
| 25 | `constraint_bridge.pl:42` extractiveness→`0.5` if absent | **G6** (latent) | engine | latent — extractiveness is a required authored field | same | low | low | observer |
| 26 | analysis-path `0.5` defaults — `boltzmann_compliance.pl:245`, `covering_analysis.pl:486`, `gap_diagnostic.pl:120`, `omega1_audit.pl:102` (`BaseEps=0.5`); `purity_scoring.pl:57,70`; `drl_boltzmann_analysis.pl:135,154,302`; `drl_fpn.pl:197`; `signature_detection.pl:160` | **G6** | engine | impute 0.5 on missing data across boltzmann/purity/fpn/NL analysis paths | engine-design; mix of by-design "neutral" and silent traps | low each | varies | analysis/report paths |
| 27 | `domain_priors_expanded.pl:64,70,77,94` `default_*=0.5` "neutral fallback" | **G6** (by-design) | engine | explicit prior fallbacks | engine-design = intended priors | n/a | n/a | prior layer |

---

## Totals by type

| type | count of rows | notes |
|------|---------------|-------|
| G1 | 6 (rows 1–6) | 2 are "consumed-but-classification-inert" (cosmetic NL path); 2 are by-design (provenance/commentary) |
| G2 | 6 (rows 7–12) — but row 7 is a **7-predicate cluster** | only genuine G2 once empty-in-both confirmed |
| G3 | 2 confirmed (rows 13, 4) + **217 grep-upper-bound candidates** (see §5) | needs clause-level triage |
| G4 | 5 (rows 14–18) — 2 unenforced, 3 partial/by-design | |
| G5 | 4 (rows 19–22) | |
| G6 | 5 row-groups (rows 23–27) | 1 LIVE-LOAD-BEARING-WRONG, 2 latent, 1 multi-site analysis, 1 by-design |

## Cross-axis-live subset (appear dead/inert on observer axis, feed committer axis)

These are the dangerous ones — do not strip on observer-axis evidence alone:

- **`boltzmann_floor_override`** — sole consumer `boltzmann_compliance.pl:453` → `boltzmann_floor_for` → `excess_extraction` → 14+ callers in `drift_events`/`drl_boltzmann_analysis` (committer drift). Authored 27 (live) / 573 (archive). Inert on Surface-1 export, **live on committer axis**. (Not a strip candidate; cross-axis asymmetry only.)
- **Row 23 `Supp=0.5` (G6)** — lives on the committer temporal path (`drl_composition`); invisible to observer-axis classification but flips 279/647 committer temporal rows.
- **Rows 19–21 (G5 scalar/temporal)** — observer reads scalar, committer reads temporal; the same metric can carry different values per axis.
- **Row 17 (Goodhart rule)** — enforced only on committer drift, absent on observer.

## Corpus-coverage-divergence subset (NOT wiring gaps)

**EMPTY.** No predicate's emptiness flips between `testsets/` and `testsets_3000/`. Every G2
candidate (intent_* family, the four read-only metrics) is empty in **both**; every populated
predicate (`constraint_beneficiary` 421/6712, `constraint_victim` 441/7879, `affects_constraint`
520/9305, `measurement` ~1307/21994, `coordination_type` 193/3309, `emerges_naturally` 12/847,
`has_sunset_clause` 1/17, `boltzmann_floor_override` 27/573) is populated in both.

**Documentation correction (not a gap):** CLAUDE.md (2026-05-31 note) states `affects_constraint`
is "empty across all of testsets_3000." This is **false** — 9305 emitted facts in the archive.
The note conflated `affects_constraint/2` (a populated network edge) with the genuinely-empty
`intent_*` tables. The empty-table finding holds only for `intent_power_change`/
`intent_beneficiary_class` (and the wider intent_* family). → see ISSUES stub.

## Low-stakes, clear-direction subset (human fast-path)

Both directions cheap **and** authoritative-layer evidence strong:

- **Row 13 `predict_transformation/3`** — confirmed dead (0 callers anywhere); strip is trivial,
  wiring it would need a new caller no one asked for. Direction: engine-design (dead code).
- **Row 5 `uke_scope.*`** — provenance only, never reaches engine; leave as-is (no action) or
  document as seed metadata. Direction: prompt-intent = provenance.
- **Row 6 commentary block** — by-design text/test emission; no action. Direction: by-design.
- **Row 4 `cs_reference_frame/2`** — emitted-but-never-read; strip the emission or wire a reader.
  Cheap both ways; the committer-drift intent question is the only judgment needed.

The high-judgment rows are: **1** (parallel hardcoded mandatrophy path), **7** (whole intent_*
subsystem — keep-and-populate vs delete), **14** (scaffold temporal rule), and **23** (the
load-bearing `Supp=0.5` fail-open — the single highest-stakes correctness decision in the census).

---

## §5 — G3 exhaustive export-vs-caller sweep (methodology + honest limit)

Script: `/tmp/g3_sweep.py`, `/tmp/g3_refine.py` (raw output in appendix). Of **528** exported
predicates across all `prolog/*.pl`:

- 422 have no parenthesised caller in any *other* prolog module, decomposing into:
  - **65** arity-`/0` (CLI/REPL entry points — 13 confirmed referenced in shell/python; the rest
    invoked externally by design),
  - **114** referenced as a bare atom elsewhere in prolog (meta-call / driver-list / reexport —
    e.g. the `detect_*` drift family is goal-listed, not paren-called),
  - **26** referenced only in `tests/`/`python/`/shell,
  - **217** "genuine" candidates with no caller, no bare ref, no external ref.

**The 217 is an upper bound, not an orphan list.** It still conflates two classes a regex cannot
separate without clause-head-vs-body parsing: (a) genuinely dead code, and (b) **over-exported but
called internally** by the module's own `/0` driver (my check excludes the defining file, so an
internally-used predicate looks orphaned). Spot-checks confirm both classes are present. Converting
217 → a genuine dead-code list is a scoped clause-level audit, routed as its own OQ. Two are
individually confirmed dead and listed above: `predict_transformation/3` (row 13),
`cs_reference_frame/2` (row 4). **Presenting 217 as orphans would be the false-orphan failure the
`mandatrophy_resolved` canary (§7) exists to prevent.**

## §6 — Completeness reconciliation (forced count — must hit zero)

Phase A enumerated the static-type surface and engine-read predicates. Accounting:

- **SCHEMA properties (~65):** the 14 base_properties, 8 header, 7 perspective, 7 omega, 4
  measurement, 2 interval, 7 commentary, 2 boltzmann, 2 network, 2 directionality, 3 uke_scope,
  7 cs_structure fields. Disposition: **aligned** (consumed + emitted: extractiveness, suppression,
  theater_ratio, claimed_type, emerges_naturally [rope gate `drl_core:358`], has_sunset_clause
  [`drl_core:275`+drift], beneficiaries/victims, coordination_type, boltzmann_floor_override,
  d_value/power_atom, perspectives, measurements, interval, omegas [report/maxent], cs_structure
  {kernel_codification, authority_grounding, interpretation_layer_present, reading_relations,
  axioms, drift_state} [committer], header IDs) **or gap-row** (rows 1–6, 12 cover
  mandatrophy_resolved, accessibility_collapse, resistance, reference_frame, uke_scope×3,
  commentary×7) **or divergence** (none).
- **Engine-read predicates not in schema:** intent_* ×7 (row 7), inevitability/
  internalization_depth/resistance_to_change/accumulation_speed (rows 8–11), compound measurement
  metrics (row 12), `is_mandatrophy_resolved/1` (engine hardcoded, row 1).
- **Rules:** scaffold-decline, final-measurement-match, piton-atrophy, Goodhart, perspective-min,
  ε-invariance (rows 14–18; ε-invariance partially via `config_validation`/OQ-25).
- **Representation pairs & defaults:** rows 19–27.

**Unaccounted = 0** at the granularity of (schema property + distinct engine-read predicate + named
prompt rule). The one residual *open* item is the 217-candidate G3 set (§5), which is accounted-for
as a routed work item, not an enumeration hole.

## Appendix — raw witnesses

All counts produced by the grep commands recorded in the session transcript; key ones:

- Fact counts both corpora (filtered for facts, module-qualified): `constraint_beneficiary`
  421/6712, `constraint_victim` 441/7879, `affects_constraint` 520/9305, `measurement` 1307/21994,
  `coordination_type` 193/3309, `boltzmann_floor_override` 27/573, `emerges_naturally` 12/847,
  `has_sunset_clause` 1/17; `intent_power_change` 0/0, `intent_beneficiary_class` 0/0,
  `intent_viable_alternative` 0/0, `intent_alternative_rejected` 0/0, `intent_suppression_level`
  0/0, `intent_resistance_level` 0/0, `intent_norm_strength` 0/0; `inevitability`/
  `internalization_depth`/`resistance_to_change`/`accumulation_speed` all 0/0.
- compiler `constraint_metric` emissions: extractiveness, suppression_requirement, theater_ratio,
  accessibility_collapse, resistance (`generate_constraint_pl.py:436–447`).
- engine `constraint_metric` literal reads: extractiveness, inevitability, internalization_depth,
  resistance_to_change, suppression_requirement, theater_ratio; parameterized (via
  `get_metric_average`/`safe_get_metric`): accessibility_collapse, accumulation_speed, resistance,
  resistance_to_change, suppression_requirement.
- G6 sites: full `0.5` default sweep (appendix grep) — see rows 23–27.
- G3 sweep: 528 exports → 422 zero-caller → {65 /0, 114 meta-bare, 26 ext-only, 217 candidate}.
