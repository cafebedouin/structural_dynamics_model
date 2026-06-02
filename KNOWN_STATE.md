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

## 2026-06-02 — Reading-axis structural obstruction built + cs_reading_relation name-form repair

**Built (OQ-54, "establish").** `cs_kernel_obstruction/4` + `cs_kernel_obstruction_status/2` +
`cs_kernel_obstruction_report/0` in `cs_kernel_registry.pl` — the committer-axis analog of the observer
H¹ (`grothendieck_cohomology.pl`), over the **reading** cover. Counts foreclosing reading-pairs
(`real_closure`) vs coexisting (`licensed_plurality`) vs none (`untyped` — **fail-closed on absence**,
NOT silently glued). **Observer-blind by construction** (reads only `cs_reading_relation`, never
χ/`live_index`/`classify_at_time`), so Theorem-7 gradient-orthogonality holds and `live_index` / the
none-compliant cross-tab are NOT prerequisites. Distinct axis from OQ-51's observer H¹/W1.

**Data defect found + repaired (the load-bearing part).** The independent must-flag oracle (NOT via the
probe) exposed that `cs_reading_relation` targets are authored in inconsistent name forms — **short
targets** (`ishmael_covenant_reading`) that don't match the registered **full** reading name
(`abrahamic_covenant__ishmael_covenant_reading`). Exact-match consumers — this probe **and pre-existing
`cs_corpus_analysis.pl:131-149` and `json_report.pl:1432`** — silently under-counted closure/plurality.
Partition (hard counts): forecloses exact 117 / fixable 16 / dangling 13; coexists 528/48/59; influences
212/22/27. **86 name-form edges normalized in-place** across **47 testset files** (short → `<kernel>__<short>`;
script asserted every edit matched, 0 zero-match). **Predicted-delta positive control passed exactly:**
`real_closure` 84→**94**, the 10 named movers (`abrahamic_covenant`, `magna_carta_1215`,
`rogers_commission_findings`, `second_amendment_boundary`, …) all in; dangling untouched (13/59/27).
**Decision discipline (do not relax):** option 2 (probe-only robust) and option 3 (read-time resolver)
were **rejected** — three consumers must not drift to three counts, and a resolver is self-blinding (it
would hide whether the generator fix worked; exact-match on cleaned data is self-witnessing).

**Flow fix (generator), two layers.** (1) `generate_constraint_pl.py:482` canonicalizes the emitted
target to `<kernel_id>__<short>` (hard transform, not a prompt request) — fixes the name-form class.
(2) `generate_kernel_corpus.py:validate_reading_relation_integrity/3` (called after
`stamp_kernel_linkage`) is the **hard-fail referential-integrity** check: a target must resolve to a
declared reading whose canonical file `<kernel>__<short>.pl` exists on disk — **no plausible-form escape**
(a well-formed but absent `<kernel>__<name>` fails like a typo). Unresolved edges route to a
**quarantine bucket** (`cs_reading_relation_quarantine.json`), reported loudly, **never auto-written /
rewritten / pre-classified**. Positive control passed (synthetic fixtures: canonical + short-of-existing
attach; absent-full + typo quarantine).

**Stock quarantine view:** `cs_kernel_registry:cs_reading_relation_unresolved/4` enumerates the existing
unresolved edges (currently **99**: forecloses 13 / coexists 59 / influences 27) — the review queue for
the OQ-58 narrative-read pass. The obstruction stays fail-closed on these (no invented gluing status).

**OQ-58 policy (ruled 2026-06-02):** exactly two automated outcomes — *canonical → attach*,
*everything else → quarantine*. **No** auto-rewrite tier (writes authored structure on an uncalibrated
confidence) and **no** plausible-form tier (well-formed ≠ intended; that's a syntactic tell, not
evidence). The missing-vs-typo-vs-noise sort is the **review's** output, made on the source narrative —
the only place that evidence lives — not a mechanical rule.

**Tripwire:** `cs_reading_relation` targets are now canonical full names in the live corpus, and consumers
stay **exact-match** (self-witnessing). Do NOT add a read-time short→full resolver — it re-hides the
defect. New dirty edges should fail loudly (under-count), then be repaired at source. Pipeline regenerated
green (manifest after repair, 49.6s, all steps ok).

---

## 2026-06-02 — Coupling liveness profile wired into per-constraint JSON (seat structure, not just verdict)

**What & why.** The Boltzmann coupling test computed scope-violations and power-violations
separately inside `count_coupling_violations/4` and then summed them, discarding *which* observer
index moves the verdict (Build-Discipline Pattern 1: produced-but-not-consumed). Now surfaced.

**Changes (all behavior-preserving for the coupling score):**
- `boltzmann_compliance.pl`: refactored `count_coupling_violations/4` to delegate to a new
  `coupling_violation_components/5` (SOLE source of the violation logic — score path and the new
  liveness predicate both route through it, so they cannot drift). Added + exported
  `coupling_liveness/3` (rebuilds the Power×Scope grid, returns ScopeViolations, PowerViolations).
- `json_report.pl`: `write_coupling_object/2` now emits `scope_violations`, `power_violations`,
  `live_index` (`none|scope|power|both|inconclusive`) in the per-constraint `coupling` object.
  No-epistemic-access constraints emit nulls + `inconclusive` (absence reported as absence, not
  defaulted to a `(0,0)` "seat-free" reading).
- Consumers (so it is not produced-but-not-consumed): `query.py` (row dict + `--detail` display),
  `enhanced_report.py` (batch Identity block).

**Witness.** Full pipeline regenerated (manifest `ae10e7e`, 50.3s, all steps ok). Positive control:
engine-emitted `(scope_violations, power_violations)` matched an independent oracle
(`/tmp/decomp_out.csv`, same filter) for **772/772**, 0 mismatches; `live_index` agrees with the
(SV,PV) signs everywhere; 1 constraint correctly `inconclusive`. Corpus `live_index` distribution:
both 591, none 87, power 87, scope 7, inconclusive 1 — `none` (87) tracks the ~90
Boltzmann-compliant/invariant population.

**Score behavior-preserving (witnessed).** Direct `cross_index_coupling` under the old code (parent
`51612b0d`, pre-refactor) vs the new code on the same 772-constraint corpus is **byte-identical for
773/773** (`/tmp/old_scores.csv` vs `/tmp/new_scores_direct.csv`, 0 mismatches) — the refactor sums the
same components it now also exposes. The score path is unchanged; only the per-constraint JSON gained
the liveness fields.

**Framing note (corrected this session).** Boltzmann invariance is a *partial test for Mountain-ness*,
not a pathology flag: an index-invariant verdict is seat-free/contentless (Seat Theorem §4), so
`live_index=none` is Mountain-consistent and non-`none` = the verdict is seated on the observer index.
The reading-axis analog is NOT built — see GAP-04/05/06 and OQ-53..56.

---

## 2026-06-02 — Toy corpus finished 769/770; generator repair + 3 robustness fixes

**Result.** The kernel-aware toy corpus is complete at **769/770** (ladder `beta_processed.txt`;
~772 `.pl` in `testsets/` incl. 3 user-added). Composition: ~566 kernel readings (200 kernels
decomposed → 570 reading seeds) + ~200 plain (sampled from `beta_seeds.json`). One lone holdout:
`market_as_natural_default__genuine_natural_reading` — claims `mountain` but the model keeps
authoring `suppression 0.08 > 0.05`; this is a **false-mountain** (claimed-natural + real
suppression), correctly rejected by the schema's conditional mountain gate, not a pipeline bug.
Leave it or hand-author.

**Four fixes landed in `agent/generate_kernel_corpus.py` + new `python/story_repair.py`** (all
witnessed — plain went 0/200 → 199/200, residual 8 → 1):
1. **`overwrite=True` in the no-scope path (`run_no_scope`).** The 0/200 plain failure cause:
   `json/` still holds the **pre-rebuild ~4067-file corpus** (it was never archived alongside
   `testsets/`), so plain seeds reuse archive ids whose stale `json/<id>.json` exists and hit the
   `out_json.exists()` SKIP. The ladder (`beta_processed.txt`), not json-existence, is the rebuild's
   idempotence source. **Tripwire: don't "fix" this back to skip-on-exists, and note `json/` is
   stale — archiving it (like `testsets_3000`) is an open cleanup (GAP-style).**
2. **`poll_batch` transient-error retry** (≤30) for 503/overloaded/rate-limit/timeout — a single
   503 was crashing long batch runs mid-poll.
3. **Plain seed summaries capped to ≤500** (`prolog/toy_plain_seeds_capped.json`). Phase-0.5
   uncapped summaries (median 2585) made the model over-produce invented fields; the proven
   generation regime is ≤500 (median 338). The full `beta_seeds.json` stays uncapped (for the probe).
4. **`python/story_repair.py` — canonical deterministic repair**, wired into
   `process_batch_results` (strip + `repair_story` + re-validate before fail). Repairs required-
   defaults, **non-ASCII id transliteration across all 12 schema id-pattern fields** (incl.
   `cs_structure.axioms[].atom`, `network.affects_constraints`, `reference_frame`), null→0,
   unconditional [0,1] clamps. It does **NOT** touch conditional `allOf/then` bounds (claimed_type
   vs metric) — those are semantic; clamping would fabricate. `recover_historical_seeds.fix_story`
   now delegates to `repair_story` (fork removed; the one dropped nicety: commentary-key merge).

**Prompt hardening.** `prompts/constraint_story_generation_prompt_json.md` gained a
"TYPE↔METRIC CONSISTENCY IS A HARD GATE" block (piton ⇒ theater_ratio ≥ 0.70; mountain ⇒
suppression ≤ 0.05 / extractiveness ≤ 0.25). It nudged `tsunami_stone…` (piton) over the line on
retry; the mountain holdout resists because it is genuinely a false-mountain.

**Open / follow-ups:** (1) `json/` pre-rebuild corpus archive cleanup; (2) the 1 false-mountain
residual; (3) the static-ε-below-series-max authoring finding (70/499, prior entry) is still not
an OQ.

## 2026-06-02 — `sheaf_status` now persisted (W1×sheaf join built); orbit provenance is a sidecar

**If you are editing `json_report.pl`, `run_pipeline.py`'s `_manifest_step`, or anything that reads
`orbit_data.json` — read this.** Two additive changes landed plus a new join tool. The pipeline ran
clean afterward (exit 0, all steps `ok`).

**1. `json_report.pl` now emits `sheaf_status` per constraint (closes a produced-but-not-consumed
gap).** `sheaf_analysis:sheaf_status/2` (`sheaf_analysis.pl:54-63`: `manifest_presheaf` if H1>0; else
`fragile_presheaf` if Arakelov height > corpus p75 threshold; else `genuine_sheaf`) was computed but
never written to disk — only `h1_band` was. Added a `sheaf_status` emit beside the `h1_band` block
(after `json_report.pl:387`) and `:- use_module(sheaf_analysis, []).` (after `:33`; called
module-qualified). Additive only — the `sheaf_status/2`, cohomology, and W1 predicates are untouched.
Live result at n=563: `manifest_presheaf` 98 / `fragile_presheaf` 100 / `genuine_sheaf` 366; emit-sanity
holds (manifest count == h1_band>0 count == 98).

**2. `run_pipeline.py:_manifest_step` writes the `orbit_data.manifest.json` sidecar.** Same
`build_manifest(run_at)` dict as `pipeline_output.json`, so `orbit_data.json` is provably the same
run. **Tripwire (silent-corruption risk):** `orbit_data.json` provenance lives in the **sidecar, NOT
in-file**. Do **not** `inject_manifest` a `"manifest"` key into `orbit_data.json` — it is a pure
`id→orbit` dict that **7 consumers iterate** with bare `.items()` (`game_theory_nash.py:158`,
`game_theory_mixed_strategy.py:89`, `sheaf_audit.py:310`, `container_typology_analysis.py:259`,
`meta_reporter.py:100`, `extract_corpus_data.py:250`, `normalize_orbit_ids.py:43`); an inline
`"manifest"` key would be silently read as a fake constraint by all of them. This is recorded as a
design gap (`design_gaps.md` GAP-03). See OQ-51/OQ-52 in `ISSUES.md` for the findings.

**3. Bare-context vacuity extends to `sheaf_status` / Arakelov, not just W1.** A smoke test that ran
`sheaf_status/2` after `[stack] + load_all_testsets` but **without** `maxent_multi_run` returned
`fragile_presheaf=0` (vs 100 in the full pipeline). Arakelov height reads
`maxent_distribution_raw/3`, populated only by the MaxEnt run, so heights degenerate and the p75
fragile/genuine split collapses in a bare context — the same vacuous-path trap as W1
(`test_harness.pl:76`), one layer over. **Compute `sheaf_status` only on the maxent-first pipeline
path; a bare `[stack]` recompute is vacuous (reads as "no fragile presheaves").**

**4. New tool `python/w1_sheaf_join.py` (read + join, no Prolog recompute).** Reads
`pipeline_output.json` (W1=`wasserstein_total_fracture`, `h1_band`, `sheaf_status`) + `orbit_data.json`
(shift vector), guarded same-run by the sidecar; merges on id, sorts descending by W1, writes
`outputs/w1_sheaf_join.{json,md}` (full 564-row ranked table + 2×2 concordance + per-id off-diagonal
rows + the four positive controls). Run at n=563 (commit b5ccee0): W1 sum 33.47, nonzero 112, max
1.904589 (`privilege_architecture_coordination`). **W1-max field-identity CONFIRMED**
(`wasserstein_total_fracture` = sum of the 3 canonical edges, proven on the argmax); the recon's ~4.7
does **not** reproduce and appears nowhere in the repo as a W1 value — likely the longer tail of the
larger archived `testsets_3000` (3,380), **not** staleness or field-misidentification (testsets_3000
max unverified). 2×2 concordance: 58 off-diagonal (36 with H1=0∧W1>0, 22 with H1>0∧W1≈0) — see OQ-51.

## 2026-06-02 — Dirac Axis-1 (`derived_from/3`) removed → design gap; `gauge_fixed/3` straggler fixed

**If you are editing `dirac_classification.pl` or looking for primary/secondary constraint
tracking — read this.** Two changes landed together; neither is output-changing for the live
pipeline (the affected predicates had no consumers).

**1. `derived_from/3` + `constraint_generation_order/2` removed (Dirac Axis-1, primary/secondary).**
Declared `:- multifile/:- dynamic` so testsets *could* assert derivation chains, read only by
`constraint_generation_order/2`, which was called only by `full_dirac_report/3`. **Zero producers
corpus-wide** — no testset, no generator, no engine code ever asserted a fact (witnessed:
`grep -rln derived_from` over `testsets/`, `testsets_sotu/`, `testsets_3000/` all empty). So
`constraint_generation_order/2` returned `primary` for every constraint via the `\+ derived_from`
cut — absence presenting as a presence (Build Discipline Pattern 5). The module's own header had
already sorted this axis into "merely relabels." Removed: the two export entries, the §4 block
(comment + declarations + both clauses; §5 renumbered → §4), and the `generation_order(Order)`
field of `full_dirac_report/3` (now 7 fields; nothing external destructured it). The capability it
reached for — systematic derivation-chain tracking, with a typed `Reason` slot the live
`affects_constraint/2` edge cannot carry — is now recorded as **GAP-01 in
`docs/design/design_gaps.md`** (new design-doc, a ledger of declared absences; pointer added to
CLAUDE.md "Design intent"). Re-opening is a framework-direction decision, not a code fix; do NOT
re-add an unfed `derived_from/3`.

**2. `gauge_fixed/3:208` straggler fixed.** It still called the removed `standard_context/1` (deleted
in the v2.0 SITE CONTEXTS migration, which moved `gauge_orbit/2` and
`preserved_under_context_shift/2` to `constraint_indexing:site_contexts/1` but missed this one).
Latent because `full_dirac_report/3` — its only path to `gauge_fixed/3` with a real `context(...)`
tuple — has no callers, so the `Unknown procedure` throw never surfaced. Now delegates via
`constraint_indexing:site_contexts(Contexts), member(AltCtx, Contexts)` like its siblings.
Witnessed end-to-end: `gauge_fixed(abrahamic_covenant__isaac_covenant_reading, <analytical ctx>, true)`
and `full_dirac_report/3` returns a complete 7-field `dirac_report(...)` (was: `Unknown procedure:
standard_context/1`).

**Standing note:** `full_dirac_report/3` itself is still a dangling wire (no consumers). It works
now, but if it stays unconsumed it is a candidate for the same removal treatment as Axis-1.

---

## 2026-06-02 — False-summit forensic detector repaired (was vacuous) + two report bugs + stale comment

**If you are editing `drl_core.pl` false-mountain detection, `report_generator.pl`'s forensic
audit, or `drl_composition.pl`'s `classify_at_time` temporal comment — read this first.** Four
fixes landed together this session; three are output-changing. Open follow-ups are **OQ-50**.

**1. `drl_core.pl:548` `dr_claim_mismatch(_,_,type_1_false_summit,_)` was a vacuous gate that had
never functioned.** The body was `is_mountain(C, Context, fail)`. `is_mountain/3` has a second
clause `is_mountain(_,_,fail).` (`drl_core.pl:123`) that is an **unconditional catch-all** — it
unifies with any `(C, Context)` because the third arg `fail` matches; clause 1's head (third arg
`mountain`) never unifies with `fail`, so the metric test never runs. Positive control:
`is_mountain(C, boundCtx, fail)` SUCCEEDS while `is_mountain(C, boundCtx, R)` gives `R=mountain` —
i.e. the constraint **is** a mountain yet the `fail` call also succeeds. The trailing `!` then
committed to the **first** mountain-claimer, with `Context` left **unbound** (reports printed
`Context: _NNNN`). Net: the detector returned one arbitrary mountain-claimer — and that one
(`honor_satisfaction_mechanism__contraction_reading`) is a **genuine** mountain (`dr_type=mountain`
at all 4 contexts). It detected nothing and accused the floor.

**Fix: negate `dr_type/3` (post-signature), enumerate `standard_context`, drop the cut.** Now:
```prolog
dr_claim_mismatch(C, Context, type_1_false_summit, severe) :-
    narrative_ontology:constraint_claim(C, mountain),
    standard_context(Context),
    dr_type(C, Context, ActualType),
    ActualType \= mountain.
```
**Why `dr_type`, not `is_mountain` (evidence-settled, not preference).** `is_mountain`
(classify_from_metrics, **pre-signature**) returns non-mountain at the moderate+institutional power
contexts for **all 8** mountain-claimers — a χ=ε·f(d)·σ(S) power-scaling artifact (mid-power shifts
off the mountain band); the signature layer then restores genuine mountains in `dr_type`. So
negating `is_mountain` flags every claimer including the 4 genuine mountains; negating `dr_type`
flags only constraints whose authoritative classification actually departs from the claim. `dr_type`
does **not** call `dr_mismatch`, so no recursion. **Do not "simplify" this back to `is_mountain`,
and do not re-add the cut** (the cut stops the per-context enumeration that locates the break).

Live-corpus result after fix: **4** false summits across **14** (constraint, context) instances —
`papal_temporal_authority_mountain` (moderate+institutional; mountain at powerless/analytical),
`press_reformation_causality__technological_inevitability`, `statutory_debt_ceiling__constitutional_nullity_reading`,
`total_war_winnability_post1945__structural_contraction_reading` (all 4 contexts, never mountain).
The 4 genuine mountains (`honor_…`, `state_killing_…__abolition`, `tsunami_stone_…`, `zero_as_number_…`)
are correctly **excluded**.

**2. `report_generator.pl:445` queried `type_1_false_mountain` — an atom no clause produces.** The
producer emits `type_1_false_summit` (above). The `setof` therefore always failed → the forensic
audit always printed *"All mountains are structurally validated"* whenever any mountain was claimed
(Pattern-5 absence-pass: a dead query reads identically to a clean result). Positive control:
pre-fix old-atom solution count = 0; `type_1_false_summit` solution count = 14. Fixed the atom.
**This means the audit was doubly-dormant: wrong atom queried, and the detector under it vacuous.**

**3. `report_generator.pl:447` miscounted.** Header said "Detected N constraint(s)" using
`length(FalseMountains, N)` where `FalseMountains` is a list of (C, Context) **pairs** — 14 pairs
across 4 constraints read as "14 constraints." Now reports distinct constraints + instance count:
*"Detected 4 constraint(s) … across 14 observer-context instance(s)."* (Vocabulary note: the
report register is **context / observer / perspective**, not "seat" — "seat" is `design_discipline.md`
internal design language and must not appear in product output. There are only 4 observer contexts;
the 14 is constraint×context instances.)

**4. `drl_composition.pl:174` stale comment.** The OQ-41 fail-close comment cited "650/656 rows had
no temporal suppression series" — pre-rebuild (n=656 era) provenance. Engine-measured on the live
corpus: **471/562** carry a temporal `suppression_requirement` series (the temporal path), **91/562**
are scalar-only (hit the STOPGAP fallback), **0/562** reach `unknown` (every constraint authors at
least a scalar). The stopgap scalar clause is **still load-bearing for the 91** — do not delete it
until coverage is complete. Comment updated; code unchanged.

**Untouched, recorded as OQ-50 (do not assume these work):**
- `forensic_explain_false_mountain` (`report_generator.pl:459+`) re-derives its verdict from raw
  suppression/extractiveness heuristics **independent of `dr_type`** — it printed "AMBIGUOUS" for
  `papal` even though the detector correctly flagged it (`dr_type=scaffold≠mountain`). The
  explanation can disagree with the (now-correct) detection.
- Sibling clauses `type_3_snare_as_rope` (`drl_core.pl:555`) and `type_5_piton_as_snare` (`:562`)
  share the **bound-Context requirement** (clause 1 of `is_snare`/`is_piton` computes Chi from
  Context). They are **not** vacuous (they ask for the positive type atom → clause 1's real test),
  but would silently no-op if ever called with `Context` unbound. Currently only reached with bound
  context. Same latent-trap class as the `type_1` bug.

The vacuous catch-all gate is a **new Pattern-5 sibling** (absence-of-a-real-test satisfies via
clause-head unification, not empty-table — see `build_discipline.md` Pattern 5 / OQ-44).

---

## 2026-06-02 — Removed superseded observer-axis husk (saturation_floor) — commit ef92a61d

**If you are looking for the `--- HUSK SIGNATURE ---` report section or `saturation_floor` /
`born_saturated` / `husk_metrics` and cannot find them: they were deleted, deliberately.**
Commit `ef92a61d` removed the observer-axis husk machinery. Do not re-add it as "missing."

**Two husks existed; only the observer one was removed. The CS one is live and untouched.**
- **Removed (observer axis, cruft):** `husk_series/3`, `ep_native_series/3`, `husk_exists/3`,
  `husk_point/5` in `drl_composition.pl`; the `husk_report.pl` standalone; `outputs/husk_data.json`
  + `outputs/husk_report.md`; `_load_eps_series` + the `saturation_floor`/`born_saturated`/
  `husk_metrics` block in `enrich_pipeline_json.py`; `build_husk_signature` + the HUSK SIGNATURE
  section in `enhanced_report.py`; the `_prolog_husk` pipeline step in `run_pipeline.py`; both
  `husk_metrics` schema rows in `shared/schemas.py`.
- **Kept (committer axis, live):** `cs_terminal_attractor(..., husk)` in `cs_drift_engine.pl` and
  its 9 consumers (`cs_pattern_detection`, `cs_axiom_engine`, `cs_drift_mismatch`, ...). This is
  the framework's real husk — design-endorsed (`design_discipline.md:344`), reads authored
  `cs_drift_state` gap vectors, NOT the ε series. **The "husk 57" §5.11 count (`ISSUES.md:803`)
  and the `husk_reading` corpus story used as a `:738` positive control are this husk, not the
  removed one — leave them.**

**Why removed (cruft, not a wiring gap).** Provenance: the observer husk landed 2026-05-25 06:43
(`e56bc18c`, "Second round") and was superseded ~4h later by the categorical CS husk attractor
(`624e3b66`, 10:41); the first draft was never deleted. It had **zero engine consumers** — a closed
produce→report loop terminating in a display string the report itself disclaimed as "ε authoring,
not an observed property." Wiring it to the CS engine would be a cross-axis reduction
(`two_axis_architecture_v7.md:124`), i.e. construction, not closing an existing wire.

**Blast radius (witnessed):** one behavioral change — generated reports no longer emit the
`--- HUSK SIGNATURE ---` block. Engine `[stack]` loads clean; the four Python files compile + import
clean; zero dangling references. No classification / χ / drift / purity / CS verdict read any
removed symbol.

**Two loose ends, both still OPEN (not closed by this commit):**
1. The 71 `[ENRICH] WARN ... saturation_floor suppressed` warnings are gone (the gated field is
   gone). No OQ ever tracked them; nothing to retract in `ISSUES.md`.
2. The **real** finding underneath is untouched: static `constraint_data:base_extractiveness/2`
   (the ε χ consumes) understates the depicted ε-series peak for **70/499 (14%) of with-series
   readings, one-sided (0 overshoots), ~2× higher rate among kernel readings (14.4% vs 7.1%)**.
   That is a χ-input question on the observer axis, independent of the deleted report field. It is
   **not** yet an OQ — open one if it graduates from "authoring-convention note" to a classification
   concern.

## 2026-06-01 — Corpus rebuild pipeline built + validated on N=1 (decompose → no-scope gen)

**New CLI on `agent/generate_kernel_corpus.py` (default behavior CHANGED).** The script now
has three modes:
- **default = no-scope generation** (no flag): `python3 -m agent.generate_kernel_corpus [N]`
  reads a seed pool (`--seeds`, default `prolog/beta_seeds.json`), takes the **next N
  unprocessed** seeds per `prolog/beta_processed.txt`, generates full stories **flat** into
  `prolog/testsets/` + `json/`, with collision-proof naming (`base` else `base__<uuid8>`,
  checked vs corpus ∪ ladder) and **3× retry** → `outputs/no_scope_runs/failures.json`.
  Seeds carrying `kernel_id`+`reading_id` generate as kernel readings (stamp `cs_kernel_id`);
  others as plain. Repeated calls **advance** the ladder (no treadmill).
- **`--decompose KERNELS_JSON [N]`**: batch-SCOPE (Sonnet) the next N kernels into reading
  **seeds** (constraint-story seeds, NOT stories), namespaced `constraint_id=<kernel_id>__<reading_id>`,
  appended to `prolog/kernel_readings_pool.json`; idempotent via `outputs/decompose/decomposed.txt`.
- **`--scope --run-tag TAG`**: the legacy serial-SCOPE+generate, run-tagged (unchanged).

**Rebuild input assembly.** `python/merge_kernels.py` merges `prolog/kernels/*.json`
(per-model kernel proposals) + `prolog/kernel_seeds.json`, dedups (id OR normalized title) →
`prolog/kernels_merged.json` (**K=200**), and samples K plain seeds from `beta_seeds.json` →
`prolog/toy_plain_seeds.json` (200). `prolog/beta_seeds.json` is the full 3,380 re-harvest
(Phase 0.5).

**Probe finding (why kernels come from authored files, not the archive).**
`python/partition_probe.py` (+ `outputs/partition_probe/validity_analysis.md`): the
prolog_v5 archive is **observer-axis** — a tightened committer-kernel rubric finds **0
kernels / 99** there while detecting **74%** of authored kernels (positive control). So
committer-kernels are sourced from `kernels_merged.json`, the archive supplies plain seeds.

**Validated end-to-end (N=1).** Decomposed `homoousios_christology` → 3 readings → generated
3 `.pl` stories; engine loads, `cs_kernel_coverage(homoousios_christology, 3)`,
`cs_kernel_divergence` fires (semi_arian vs pro_nicene at analytical contexts). The 3
`homoousios_christology__*_reading.pl` in `testsets/` are the live PoC output (ladder records
them). Next forward move: scale incrementally — `--decompose prolog/kernels_merged.json 10`
then generate from `kernel_readings_pool.json`, and `--seeds prolog/toy_plain_seeds.json N`
for plain. Tripwire: a generation quirk (model emits an extra `'description'` property) fails
some seeds on first try but the 3× retry usually recovers; persistent ones land in
`failures.json`.

**Scale run (2026-06-01): decompose-all + generate-100, two engine-level fixes.**
- Decomposed all **200** kernels → **570** reading seeds in `kernel_readings_pool.json`
  (Sonnet batch, $7.21, $0.036/kernel). Generated **96/100** readings (4 skipped, below).
- **FIX — duplicate `story_uid` (engine-rejecting).** The generator minted a UUID only via
  `setdefault`, but Haiku copies the example's placeholder UUID (`550e8400-…`) into every
  story, so 10 stories shared one uid and CS validation halted the corpus
  (`duplicate story_uid`). Fixed at `generate_kernel_corpus.py:520` to **always overwrite**
  `header.story_uid` with a fresh `uuid4` (story_uid is a per-generation surrogate, never
  authored by the content model). Existing files repaired in place (re-mint + replace).
  **Tripwire: do NOT revert to `setdefault` for story_uid** — it readmits duplicates.
- **FOLLOW-UP — reading ids >64 chars are skipped (fail-loud).** `run_no_scope` skips seeds
  whose `constraint_id` exceeds the batch `custom_id` 64-char limit (4 of the first 100, e.g.
  `basic_law_interpretive_authority__parliamentary_sovereignty_reading`). They are logged,
  not generated. To recover them, shorten the `<kernel_id>__<reading_id>` namespacing (e.g.
  hash or abbreviate) in `run_decompose` before re-decomposing those kernels.
- Post-fix corpus loads clean: 102 testsets, 99 cs_story_uid, 33 kernels, swipl exit 0.

## 2026-06-01 — Corpus rebuild Phase 0: old corpora archived, `testsets/` emptied

**What changed.** Start of the kernel-aware corpus rebuild (plan:
`~/.claude/plans/i-rough-sketch-of-steady-squid.md`). Two `git mv`s and a retarget:
- `prolog/testsets/` (229 `.pl` + 11 run-tagged subdirs) → `prolog/archives/prolog_v6/`.
- `prolog/testsets_3000/` (3380 `.pl`) → `prolog/archives/prolog_v5/`.
- Fresh **empty** `prolog/testsets/` (only `.gitkeep`) is now the active corpus — the
  rebuild output destination. **The live engine corpus is empty until Phase 3 generates.**
- The 4 executable overlays that hardcoded `corpus_path='testsets_3000'`
  (`python/sweeps/range_sweep.py`, `python/tests/diff_cut_proof.py`,
  `python/tests/test_battery.py` ×2, `python/tests/alt_power_transform_test_3k.py`) were
  retargeted to `'archives/prolog_v5'`. Positive control: that overlay now loads
  **3380** from the new path (`[corpus] Loading 3380 testset files...`, exit 0).

**Archives are testable.** `prolog/archives/prolog_v5` holds the **3,380**-story pre-rebuild
corpus; `prolog/archives/prolog_v6` holds the prior 229-story live corpus. To test either,
overlay `corpus_path` to `archives/prolog_v5` (or `_v6`) before `load_all_testsets` — the
glob `Dir/*.pl` resolves relative to swipl's cwd (`prolog/`).

**Tripwire — `testsets_3000/` no longer exists; `testsets/` is empty.** A fresh agent that
overlays `corpus_path='testsets_3000'`, or expects the live `testsets/` to hold ~223 stories,
will **silently load 0**. The path is now `archives/prolog_v5`. (CLAUDE.md's "corpus is 223"
distinction is stale during the rebuild — pending end-of-session CLAUDE.md update.) Note: this
is the *archive convention* `prolog/archives/prolog_vN` matching the existing v1/v3/v4, not a
top-level `archives/`.

## 2026-06-01 — `signature_detection.pl`: honest `unknown` now SURFACES (override removed, OQ-37)

**What changed (commit `c90c5482`).** The FNL/FCR overrides no longer launder an honest
`unknown` modal type into tangled_rope. Two guards added:
- `resolve_modal_signature_conflict(unknown, false_natural_law, unknown)` before the
  unconditional FNL clause (`:738`).
- `resolve_with_perspectival_check/4` false_ci_rope branch (`:685`): `ModalType == unknown ->
  AdjustedType = unknown`.
The reversed comment at `:669-671` ("never preserve unknown") was updated to match.

**Tripwire — do NOT reinstate "never preserve unknown."** That behavior was removed by ruling
(correctness pivot: an honest `unknown` is an *absence* of metric classification — band-gap,
authored gap, or swallowed compute-error — and must stay VISIBLE, not be masked). A future agent
reading the old design intent might "restore" the launder; don't. The `unknown` surfacing is
load-bearing for OQ-37 (it's how a band-gap reading becomes observable).

**Witness.** Corpus-wide set delta (default context, full corpus): `unknown → tangled_rope : 8`
became `unknown → unknown : 8`; **all other (metric→final) rows byte-identical** (snare→tangled
90, scaffold→tangled 6, mountain→tangled 3, snare→snare 20, tangled→tangled 59, rope→rope 2,
scaffold→rope 4, mountain→mountain 2). Same-path positive control: catastrophic_tail / husk /
abolition (metric=snare, sig=false_natural_law — the *same* `:738` clause, non-unknown modal type)
**stay tangled_rope** — the guard does not over-fire. Validation suite 0 errors / 0 warnings.
N=8 masked-unknown population = 5 diagnosed (4 taxonomy holes / 1 authored gap, see ISSUES OQ-37)
+ 3 uncharacterized (`constitutional_supremacy_reading`, `hybrid_atrophy_reading`,
`relational_autonomy`).

**Consequence — orbits regenerated.** The change altered 8 dr_types, so
`outputs/product_site_orbits.json` (perturb.py's baseline, gitignored) was regenerated
(`python/sweeps/regenerate_orbits.py`, corpus_hash `0d2ecfce17ae`). perturb.py's staleness guard
checks only the *testsets* hash, **not engine state** — so after any engine edit that changes
classifications, regenerate orbits manually or every stability-band comparison reads a stale
baseline silently.

**Ledger.** `boltzmann_coupling_threshold` added to `enhanced_report.py` `_WITNESSED_PARAMS`
(equal_protection_clause, sovereign_legitimacy) as the Surface-2 lock lever (commit `739979c6`).
Co-lever `coordination_type_offset` is per-constraint (`boltzmann_compliance.pl:388`), NOT a flat
config param — it is **not** perturb-sweepable; documented in-comment, do not add it to
`_WITNESSED_PARAMS` (perturb would raise `param not found`).

---

## 2026-05-31 — Surface-2 primitive built; lock hypothesis witnessed (lever was misnamed)

**New tool.** `python/sweeps/surface2_lock_sweep.py` — the Surface-2 primitive (PoL graduated to
instrument). One swipl process, corpus loaded once, in-memory `retract/assertz` overlay of THREE
Boltzmann levers swept INDEPENDENTLY (never bundled): `boltzmann_floor_*` (observable
`excess_extraction`), `boltzmann_coupling_threshold`, `coordination_type_offset` (both gate
`boltzmann_compliant` via `complexity_adjusted_threshold = base + offset`). Does NOT extend
`perturb.py` (Surface 1). Derives its target in-engine (no inherited list). Results:
`outputs/surface2_lock_sweep_results.json`. Runs in ~2s.

**The handoff/ISSUES lever was wrong — corrected and witnessed.** Handoff 6/7 and ISSUES OQ-30
named `boltzmann_floor_*` as the lock lever (the −0.52 PoL moved `excess_extraction` via the floor).
But the FNL/CI_rope override gates do NOT consume `excess_extraction` (`signature_detection.pl:927-930`
removed that gating); the lock gate is `boltzmann_compliant`, driven by `cross_index_coupling` vs
`boltzmann_coupling_threshold + coordination_type_offset` (`boltzmann_compliance.pl:380-383`).
Perturbing the floor moves excess but leaves `boltzmann_compliant`/signature/`dr_type` unchanged for
the FNL majority.

**Witnessed (perturb-confirmed, full 96-reading sweep).** 96 Boltzmann-gated locked readings
(FNL 76, FCR 17, CI_rope 3); 56 load-bearing (override changes final type), 40 over-included
(final == metric — bare signature-read over-includes by 40). `boltzmann_coupling_threshold` flips
48/56 load-bearing final types (0/40 over-included — clean control). Floor flips only 5/96, all in
the Boltzmann-*compliant* CI_rope/FCR cluster (excess gates `false_ci_rope` via `collect_fcr_failures`,
priority 77 > CI_rope 114). `coordination_type_offset` is a real second lever (48 flips, same set).
Combined Surface 2 witnesses 50/56 load-bearing; 6 residual immovable (5 metric=unknown FNL re-pin to
tangled_rope; 1 has a per-constraint `boltzmann_floor_override` shadowing its floor). Original "floor
flips the locked kernels" hypothesis FALSIFIED as the primary lever; corrected coupling-threshold
hypothesis WITNESSED → Surface 2 is the critical path.

**No engine source was edited** — overlay is runtime-only. Two positive controls passed before the
sweep (PoL floor flip reproduced on `civic_eugenic_reading`; coupling overlay moves `boltzmann_compliant`
on `abolition_reading`). Set-not-count caught the 5 non-uniform floor flips an aggregate "0/N" would
have hidden.

**Witness-tier note (2026-06-01).** The per-reading row dump (48 coupling type-flips) was reported as
"48 rows pasted" but the terminal output **truncated** (only rows ~44–48 + the total survived). The
row-level witness tier is therefore **structure-closed + substrate-regenerable** from
`outputs/surface2_lock_sweep_results.json` (committed at `db66cc53`) — **not pasted-to-reviewer**. No
re-paste needed; regenerate from the per-value `sig`/`final` fields if the rows are wanted.

---

## 2026-05-31 — Commit A: row-23 fail-close in `drl_composition.pl` `classify_at_time` (OQ-41)

**What changed.** `classify_at_time/4` no longer fabricates `Supp=0.5` when the temporal
`suppression_requirement` measurement is absent. New order: temporal measurement → else authored
**scalar** `constraint_metric(C, suppression_requirement, _)` → else `unknown`. Body factored into
helper `classify_at_time_with_supp/5` (module-private; `current_predicate/1` from `user` won't see it
— qualify as `drl_composition:`).

**Why the fix is scalar-fallback, not the literal `unknown` ruling.** Positive control found 650/656
temporal-timeline rows lack the temporal *series* but **all 650 carry an authored scalar** suppression
(genuine-no-data = 0). Returning `unknown` would discard real authored data — the same absence-as-value
sin as `Supp=0.5`. Witnessed impact: **268 rows corrected** vs the old fabricated 0.5 (185
tangled_rope→snare, 58 unknown→snare, 9 scaffold→mountain, 6 rope→mountain, 10 tangled_rope→unknown);
the absence `unknown` floor fires 0×; validation suite 0 errors / 0 warnings.

**STOPGAP — do not harden.** The scalar clause is a labeled temporary bridge. It is retired by
**OQ-46** (generation template must author a temporal suppression series; then delete the scalar clause
and let the temporal path stand alone). **Do not build a scalar/temporal equivalence check on it.**
The regen that retires it is gated behind the SCOPE→seed seam audit (**OQ-47**).

**Row-26 (same OQ-41) measured NEUTRAL** — `outputs/tripwire_row26_results.json`; the guard-falsity
count shortcut was caught vacuous by its positive control (guards succeed for a bogus constraint), so
the 999.9 branch-reachability tripwire is the sound test. Commit B (behavior-preserving: D2 strips, D5
row-14, D7 schema gate, D3 NL-gate fail-close + OQ-45, D6/D8 docs) is **not yet applied** — it rides
behind review of Commit A.

**Downstream-consumer audit of the 268-row shift (produced-but-not-consumed-at-a-seam check).** The
fix changed exactly one producer, `classify_at_time/4`. Traced every consumer of temporal types:
- **`classify_at_time` has ONE live Prolog consumer:** `cs_kernel_registry:cs_kernel_divergence/4`
  (at T=0), surfaced by `json_report.pl:1368` as `cs_kernel_divergence_count` in `pipeline_output.json`.
  **Persisted COUNT fields are invariant under the fix** — `cs_kernel_divergence_count` = 79 (new == old),
  `cs_kernels_with_divergence`, `diverging_pair_count` unchanged (0 pairs added/removed at pair
  granularity), confirmed in both `pipeline_output.json` and `enriched_pipeline.json`. **But the
  per-CONTEXT divergence set is NOT identical: 6542 → 7184 (+642) — the fix surfaces genuine
  divergence the fabricated 0.5 was homogenizing.** (Set-identity check, not just count.) **No
  persisted JSON carries the per-context map** — only counts — so no pipeline artifact is stale from
  this. The per-context shift is consumed by `cross_reading_diff.py` (live diagnostic, regenerates) and
  the **docs/memory `253/468` per-context numbers (`project_cs_kernel_registry.md`), which ARE
  stale-pending-regen.** pipeline_output.json is regenerated by `run_pipeline.py` (case (a); already
  generally stale vs HEAD, predating this commit; Commit A adds no new *persisted* staleness).
- **`constraint_history` / `transformation_detected` / `degradation_chain`** (full-timeline
  classify_at_time, where the 268-row shift lives): **no live consumer** — only internal callers + the
  sweep harnesses. The shift lands in an unconsumed producer.
- **`snapshot_type/3` (transition_paths) was NOT changed and already uses the scalar fallback**
  (`drift_events:safe_metric`, tier 2) — so the fix incidentally **converges** the two temporal paths
  that previously disagreed (classify_at_time=0.5 vs snapshot_type=scalar). Its consumers are unaffected.
- **All other pipeline temporal/drift fields** (`drift_events`, `drift_trajectory`,
  `transition_boundaries`, `cs_drift_terminal`, `cs_axiom_foreclosed`, `cs_drift_unacknowledged`) are
  produced by `drift_events`/`grothendieck_cohomology`/`cs_drift_engine`/`cs_axiom_engine`, **none of
  which call `classify_at_time`** — unaffected, regenerate from engine.
- **Docs/memory CS divergence numbers** (`project_cs_kernel_registry.md` 253/468 per-context): **STALE
  — the per-context divergence map shifted +642 (above); regenerate before citing.** (Correctly so:
  real suppression surfaces real divergence.)
**Verdict: no silent staleness blocks Commit B.** Persisted pipeline counts are measured invariant; the
per-context shift touches only regenerating live diagnostics + the now-flagged doc/memory numbers.

## 2026-05-31 — Commit B LANDED (behavior-preserving batch behind Commit A)

Applied: **B1** NL-gate fail-close (`signature_detection.pl` `count_power_beneficiaries` now reads the
authored `constraint_beneficiary` table, not the empty `intent_power_change` join) — gate discriminates
(conception_reading 0→2), **live NL certs 5→2** (3 false natural-laws with authored beneficiaries
correctly declined; recorded as a finding per the D3 framing, not reverted). **B2** stripped the dead
`inevitability` clause of `constraint_bridge:constraint_status/3` (uncalled predicate; corpus loads
clean, validation 0/0). **B3** removed the unenforced "suppression must decline over time" scaffold
clause from `prompts/constraint_story_generation_prompt_json.md:27`. **B4** stripped the
`accessibility_collapse`/`resistance` thresholds from the mountain `allOf` gate in the canonical
`python/constraint_story_schema.json` (kept `emerges_naturally`/`extractiveness`/`suppression`; JSON
valid; generator still EMITS the two fields as documentation). **B5/B6** docs (D6 defer, D8 correction).

**Deferred / found during B (not done, with reason):**
- **`internalization_depth` strip (row 9) NOT done** — its only reader is `psych_bridge:with_psych_metric`,
  and **`psych_bridge` is never loaded by `stack.pl`** (dead, unloaded module). The read is doubly
  inert; the real action is whole-module removal, which is a dead-module audit (OQ-38 family), not a
  metric-read strip. Do not "strip the read" — remove or revive the module deliberately.
- **`resistance_to_change` strip (row 10) deferred** — reached by live report paths
  (`json_report:237` emits a field, `utils:safe_get_all_metrics/2`, `data_validation`); not a free strip.
- **Schema fork noted, NOT reconciled:** `python/constraint_story_schema.json` (canonical, loaded by
  `generate_constraint_pl.py`) and `agent/data/constraint_story_schema.json` DIFFER. B4 edited only the
  canonical one. The `agent/data/` copy is a pre-existing divergent fork (build-discipline Pattern 2) —
  resolve separately; confirm whether the orchestrator ever loads it via the `_load_context_file` path.

Original prep notes (the framings that guided B):

- **G5 CLOSED (bonus from Commit A): the two temporal classification paths now agree.** Before Commit A,
  `classify_at_time` fabricated `Supp=0.5` while `snapshot_type/3` (transition_paths) already used the
  authored scalar (`drift_events:safe_metric`) — a silent G5 scalar-vs-temporal split on suppression.
  The row-23 scalar-fallback converges them. Logged against OQ-40. (The split reopens if the row-23
  stopgap is removed without the generation template authoring a temporal series — see OQ-46.)
- **D3 framing for B1 (NL-gate fail-close) — a count shift can be the CORRECT outcome.** Success
  criterion is **"the gate stops passing on absence"**, NOT "the mountain/NL count stayed at its old
  value." A change in mountain count or the 404 NL count after fail-closing is a *possible correct
  result* (the gate was certifying by absence), not a regression. Witness the gate *behaviour*
  (passes only when a beneficiary datum was authored), and report any count delta as an expected
  consequence, not a failure. (T.1's "0 mountain change" was the prior observation, not the pass bar.)
- **D2 refinement from the caller-chain check — not all three dead reads are free strips.**
  `inevitability` (`constraint_bridge.pl:22`, in `constraint_status/3`) has live callers only in
  `archives/` → free strip. `internalization_depth` (`psych_bridge.pl:19`, `with_psych_metric/2`) has
  zero callers → free strip. **`resistance_to_change` is NOT a free strip** — read inside
  `safe_get_all_metrics/2` (utils), `json_report.pl:237` (emits a pipeline-JSON field), and
  `data_validation.pl:300/309`, all reached by live report paths; the metric is 0/0 so the read always
  defaults, but removing it changes report output. Handle deliberately (drop the emitted field + its
  consumers in one change, or leave it).

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
  **CORRECTION (D8/OQ-42): `affects_constraint` is NOT empty** — it is a populated network edge
  (520 facts live / **9305 in testsets_3000**). Only `intent_power_change` (and the wider `intent_*`
  family) is genuinely empty (0/0 both corpora). The original claim here conflated the two; the
  empty-table finding holds only for `intent_*`. 10 distinct engine consumers identified via grep on
  prolog/*.pl. Two were SILENT-SAT; eight are SKIP-safe. SILENT-SAT consumers:
  (1) `signature_detection:count_power_beneficiaries/2` — **RESOLVED 2026-05-31 (Commit B1)**: it no
  longer joins the empty `intent_power_change`; it now reads the authored, populated
  `constraint_beneficiary` table, so `BeneficiaryCount==0` in `natural_law_signature` is a checked
  condition, not a vacuous pass. Live NL certifications dropped 5 → 2 (3 false natural-laws with
  authored beneficiaries correctly declined). (Supersedes the prior "cosmetically redundant / bailed
  out" note.)
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

- **2026-05-31: NL-gate fix is a diagnostic-layer decline, NOT classification-changing**
  **(corrects the handoff_6 ~:221 "3-case tail" / "cosmetic must not be cited unqualified" claim)** —
  VERIFY-OR-CORRECT pass re-derivation. The B1 NL-gate fix declined **3 raw `natural_law_signature`
  certifications (raw match 5→2)** — TRUE and a real diagnostic-layer improvement (the gate now
  discriminates on authored beneficiaries; all 3 carry ≥1 `constraint_beneficiary`, the 2 survivors
  carry 0). **But it changed no classification.** Final `dr_type` of all 3 declined
  (`behavioral_competence_reading`, `disparity_as_depth_signal`, `generational_economic_decline`) is
  **`tangled_rope` at BOTH `39630182` (parent-of-`3116ac08`, pre-NL-gate) and HEAD** — identical
  (cascade sig `false_summit_mountain`, claim source `explicit_mountain_claim`, both commits). They
  claim naturality via `explicit_mountain_claim`, and `false_summit_mountain` sits higher in the
  priority cascade than the `natural_law` clause (and reads `constraint_beneficiary` directly), so it
  captured them before and after; the raw 5→2 match was **shadowed** and never reached final
  classification. **Correct the conflation: declined-a-raw-certification ≠ classification-changing.**
  T.1 "cosmetic" is **fully cosmetic at the final-type level** (majority AND the 3-case tail); it is
  non-cosmetic **only** at the raw `natural_law_signature` certification layer (a diagnostic output).
  **Tier-flag:** the prior B1 "*perturb-confirmed*" tag was raw-count evidence (NL 5→2) standing in
  for a final-type claim — a witness one layer below the claim it backed; the final-type claim is now
  perturb-confirmed via the two-commit `drl_core:dr_type/3` (default_context) query over the 3, held.

- **2026-05-31: `demotion_pass.py` is engine-blind — its buckets cannot witness any engine change** —
  VERIFY-OR-CORRECT pass. `python/sweeps/demotion_pass.py`'s six-bucket sort (`6/0/20/0/24/141`) is a
  pure function of (a) a regex `param(...)` count over `config.pl` + `constraint_indexing.pl` (=191)
  and (b) the hand-maintained `_WITNESSED` / `_GENUINELY_UNPERTURBABLE` / `_SHADOWED` dicts inside the
  script. It runs **no `swipl`, no `subprocess`, and calls no classifier** (imports:
  `argparse/json/re/sys/pathlib` + `sweeps.perturb._compute_corpus_hash`). So a "block matches
  `6/0/20/0/24/141`" result is HELD **by construction** and **cannot witness** row-23 / NL-gate or any
  other engine change. The handoff's verify-item-1 ("re-run the demotion sort before trusting the
  block") is **mis-routed through this script.** The block's real validity rests on whether those
  dicts still match live `perturb.py` survival on the **post-fix** engine — **UNVERIFIED / OPEN**
  (graduation step: re-run `perturb.py` on the post-fix engine and diff against the dict contents).
  Route item-1-type verification through `perturb.py`, not `demotion_pass.py`.
