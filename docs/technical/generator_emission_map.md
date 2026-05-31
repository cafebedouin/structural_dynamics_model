# generate_constraint_pl.py — Authored-field → Prolog-fact emission map

> **Provenance.** Verified field-by-field against `python/generate_constraint_pl.py` **and** the
> post-arc engine/schema state at commit **`3116ac08`** (Commit B — the wiring-gap arc's last
> code-changing commit, 2026-05-31; this docs commit adds no code, so the described state holds
> through it). The content reflects post-A/B wiring: the row-23 `classify_at_time` read path, the
> B1 beneficiary gate (`constraint_beneficiary`), and the B4 mountain-schema strip. Re-verify
> against a newer HEAD if any code has landed since. **Line numbers are deliberately omitted** — they rot on the next edit
> and are the least durable content; this table is keyed on the durable field→fact mapping instead.
> This doc is a *derived second copy* whose source of truth is `generate_constraint_pl.py` (a
> Pattern-2 fork candidate — see `build_discipline.md`). **Re-verify before trusting:** the mapping
> is reproducible with `grep -nE 'emit\(f?"[a-z_]+:' python/generate_constraint_pl.py`; if that
> inventory disagrees with the table below, the generator moved and this doc is stale.

What each authored JSON field becomes when `generate_constraint_pl.py` compiles a constraint story
to a `.pl` testset. This is the **prompt/schema ↔ engine seam**: a field is only consumable by the
engine if it is emitted here, under the predicate name the engine reads.

## The seam matters because two layers rename and two layers drop

- **The engine reads renamed predicates.** Authored `suppression` is read as
  `suppression_requirement`; authored `extractiveness` is read both as `base_extractiveness`
  (domain_priors bridge) and as `constraint_metric(_, extractiveness, _)`. Grepping the engine for
  the authored field name finds nothing — grep the *emitted* name.
- **Some authored fields are emitted to nothing.** A schema-required field is not necessarily a
  fact. Known drops are listed below; treat any field absent from this table as "not wired until
  proven otherwise."

## Emission table (base_properties)

| Authored field | Emitted fact(s) | Note |
|---|---|---|
| `extractiveness` | `domain_priors:base_extractiveness(C, V)` **and** `narrative_ontology:constraint_metric(C, extractiveness, V)` | dual emission (unconditional); `drl_core:base_extractiveness/2` bridges to the `constraint_metric` form |
| `suppression` | `domain_priors:suppression_score(C, V)` **and** `narrative_ontology:constraint_metric(C, suppression_requirement, V)` | **renamed**: engine name is `suppression_requirement`. Read as a scalar (`constraint_metric`) and — when authored — as a temporal series (`measurement/5`, see whitelist). `drl_composition:classify_at_time` reads temporal `measurement` → **else this scalar `constraint_metric`** → else `unknown` (row-23 fail-close, OQ-41): the scalar emission here is that fallback source, so every constraint that lacks a temporal series still classifies on real authored data. |
| `theater_ratio` | `domain_priors:theater_ratio(C, V)` **and** `narrative_ontology:constraint_metric(C, theater_ratio, V)` | dual emission (unconditional) |
| `claimed_type` | `narrative_ontology:constraint_claim(C, Type)` | author's claim, not the engine verdict |
| `accessibility_collapse` | `narrative_ontology:constraint_metric(C, accessibility_collapse, V)` | emitted **only if authored** (`is not None`). Sole classification reader is the `natural_law` signature (via `get_metric_average` in `signature_detection`). Cosmetic for the mountain *type* (T.1: removing the NL override = 0 mountain-count change) but **not inert for NL certification** (B1 scoping, `build_discipline.md`). **As of B4 no longer schema-required for mountains** — the mountain `allOf` gate now requires only `emerges_naturally`; AC/resistance thresholds were stripped. The generator still emits when authored, but its inline `% required for mountain constraints` comment is now **stale**. |
| `resistance` | `narrative_ontology:constraint_metric(C, resistance, V)` | emitted only if authored; same NL-profile reader and same post-B4 status as `accessibility_collapse` |
| `requires_active_enforcement` | `domain_priors:requires_active_enforcement(C)` | |
| `has_sunset_clause` | `narrative_ontology:has_sunset_clause(C)` | live: scaffold temporality check in `drl_core` + drift |
| `emerges_naturally` | `domain_priors:emerges_naturally(C)` | live and **load-bearing**: observer rope gate in `drl_core` (distinct from the cosmetic NL signature) |
| `beneficiaries[]` | `narrative_ontology:constraint_beneficiary(C, B)` | per element. Read by **both** `false_summit_mountain` **and — since B1 (OQ-43) —** `count_power_beneficiaries` → `natural_law_signature`. (Pre-B1 the NL gate read the empty `intent_power_change` join; see gotcha and `signature_detection_wiring.md`.) |
| `victims[]` | `narrative_ontology:constraint_victim(C, V)` | per element |
| **`mandatrophy_resolved`** | **— nothing —** | **NOT emitted.** See the two-mandatrophy gotcha below |

## Emission table (other blocks)

| Authored block.field | Emitted fact(s) | Note |
|---|---|---|
| `perspectives[]` | `constraint_indexing:constraint_classification(C, Type, Context...)` | the indexed per-observer readings |
| `omegas[]` | `narrative_ontology:omega_variable(Id, TypeClass, Desc)` | report/maxent/diagnostic consumers, **not** classification |
| `interval` | `narrative_ontology:interval(C, Start, End)` | |
| `measurements[]` | `narrative_ontology:measurement(MId, C, Metric, T, V)` | **whitelist** — only `theater_ratio`, `base_extractiveness`, `suppression_requirement` are emitted (each guarded by its own per-metric measurement list). Any other measurement metric the engine reads (e.g. the compound `accessibility_collapse(Level)` form in `coercion_projection`) is never produced here. |
| `boltzmann.coordination_type` | `narrative_ontology:coordination_type(C, T)` | |
| `boltzmann.boltzmann_floor_override` | `narrative_ontology:boltzmann_floor_override(C, F)` | cross-axis-live: inert on Surface-1 export, feeds `boltzmann_floor_for`→`excess_extraction`→committer drift |
| `network.affects_constraints[]` | `narrative_ontology:affects_constraint(C, Target)` | **populated in both corpora — NOT empty** (OQ-42 corrected a stale "empty across testsets_3000" note); the genuinely-empty table is `intent_*` |
| `directionality_overrides[]` | `constraint_indexing:directionality_override(C, PowerAtom, DValue)` | |
| `cs_structure.*` | `cs_story_uid`, `cs_kernel_codification`, `cs_authority_grounding`, `cs_interpretation_layer_present`, `cs_reading_relation`, `cs_axiom`, `cs_axiom_status`, `cs_axiom_grounding`, `cs_reference_frame`, `cs_drift_state`, `cs_created_at`, `cs_kernel_id` | committer-axis; most consumed by `cs_*` modules. **Exception: `cs_reference_frame/2` has zero readers** — emitted, declared, never read. |

## Fields emitted to nothing (schema-required ≠ wired)

- **`mandatrophy_resolved`** (schema requires `=true` at ε>0.70): no emit. See gotcha.
- **`uke_scope.{epsilon_bin, hypothesis, downstream_of}`**: no emit — pure manifest/seed provenance, never reaches the engine.
- **`commentary.*`**: emitted as `.pl` **comment text** (`narrative_context`), not as facts;
  `perspectival_gap` additionally generates a `test(perspectival_gap)` plunit test.

## Gotchas

**The two `mandatrophy` predicates are unrelated.** Authored `mandatrophy_resolved` is never
compiled. The engine's mandatrophy logic uses a *separate, hardcoded* `is_mandatrophy_resolved/1`
(in `narrative_ontology.pl`, two names: `gale_shapley`, `planetary_boundaries`), read inside
`detect_omega(_, mandatrophy)`. A model wiring the authored field will not find it by following
`is_mandatrophy_resolved/1`, and editing the hardcoded list does nothing to author-supplied values.
(OQ-35; D6 ruling = document-and-defer — the two hardcoded names appear in 0 live testsets.)

**Metric reads are often parameterized — grepping literal atoms undercounts consumers.**
`get_metric_average(C, accessibility_collapse, _)` and `safe_get_metric(C, accumulation_speed, ...)`
read `constraint_metric(C, MetricType, V)` with `MetricType` as a *variable*. A grep for
`constraint_metric(_, accessibility_collapse, _)` misses these reads; also grep the accessor call
sites (`get_metric_average(`, `safe_get_metric(`). This is how the wiring-gap census initially
mis-listed AC/resistance as "emitted but never read."

**Facts are module-qualified; declaration lines are not facts.** Emitted facts carry the
`narrative_ontology:` / `domain_priors:` / `constraint_indexing:` prefix. An anchored grep like
`^measurement(` returns 0 — a false negative. Count facts with the qualified form and filter out
`:- dynamic` / `:- multifile` / `:- module` declarations.

## See also

- `outputs/wiring_gap_census.md` + ISSUES.md **OQ-35–OQ-44** — the prompt↔schema↔engine
  disagreement census this map was extracted from.
- `docs/technical/build_discipline.md` Patterns 4 (fabricated default) & 5 (absence satisfies the
  gate) — what goes wrong *downstream* of a field read but never populated.
- `docs/technical/signature_detection_wiring.md` — as of Commit B1 (OQ-43)
  `count_power_beneficiaries` reads the authored `constraint_beneficiary` table directly (no longer
  the empty `affects_constraint`×`intent_power_change` join); the historical reached-but-empty-join
  (OQ-36) is recorded there as resolved.
