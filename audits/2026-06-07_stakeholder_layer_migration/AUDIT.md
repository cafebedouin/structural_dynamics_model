# Stakeholder-Layer Migration — Pass 1 Audit

**Date:** 2026-06-07 · **Corpus:** live rebuild, 100 testsets (witnessed: `[corpus] Loaded 100 testsets successfully.`, all probe runs) · **Probes:** `probe_harness:with_overlay/3` (verified restore, auto cache-clear) per `docs/technical/swipl_load_path_and_probe_gotchas.md`. All evidence artifacts in this directory.

**Scope note:** every claim below carries its witness (pasted excerpt or artifact file); items that could not be witnessed this pass are marked OPEN with their graduation step. Line numbers re-derived this session against the working tree (branch `kernel-first-router`).

---

## A1 — KEYSTONE: the computed classification path never reads authored perspective facts. **VERIFIED, controlled null. Stop-rule NOT triggered.**

Probe: `a1_probe.pl`. Story: `ai_governance_accountability` (6 authored perspectives, 2 beneficiaries, 3 victims, ε=0.35). Registers captured per run: per-context `dr_type/3` and χ over canonical-4 **and** product-156, story signature (unbound query), H0/H1. Artifacts: `a1_baseline.txt`, `a1_mut_perspective.txt`, `a1_mut_metric.txt` (+ stderr files).

**Mutation (i)** — flip authored P1 `snare → mountain` at (powerless,biographical,trapped,national), via `with_overlay`:

```
--- AUTHORED diff (mid-control: must show snare->mountain):
< AUTHORED_PERSP snare context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national))
> AUTHORED_PERSP mountain context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national))
--- COMPUTED diff (invariance: must be EMPTY):
COMPUTED: byte-identical (162/162 lines)
```

The mid-control proves the substrate flip was live at capture time (gotchas §3); the COMPUTED block (160 type/χ rows + SIG + H0/H1) is byte-identical. Note the flip was *to mountain* and `SIG false_ci_rope` did not move — independently confirms the OQ-70 class rule (no signature reads a single authored perspective).

**Mutation (ii)** — positive control on the same story, ε 0.35→0.75 (overlay on `narrative_ontology:constraint_metric` only; see ε-path witness below). Required to move EVERY register (i) claims invariant (operator-reviewed plan):

| Register | (i) perspective flip | (ii) ε control |
|---|---|---|
| per-context TYPE (160 rows) | byte-identical | 120/160 flipped (9 scaffold→snare, 64 scaffold→tangled_rope, 47 tangled_rope→snare) |
| per-context χ (160 rows) | byte-identical | 160/160 changed |
| signature | unchanged | `false_ci_rope` → `constructed_high_extraction` |
| H¹ | unchanged | `H0 0 H1 3` → `H0 0 H1 5` |

```
--- AUTHORED diff (mid-control):
< AUTHORED_METRIC extractiveness 0.35
> AUTHORED_METRIC extractiveness 0.75
COMPUTED lines changed: 162/162
```

Every register was shown movable by the same probe machinery on the same story; the (i) null is therefore a **controlled null**. Verdict: **the additive-refactor premise holds — the computed path (dr_type → classify_from_metrics + signature integration; χ = ε·f(d)·σ(S); H¹ over the orbit) reads metrics/structure, not authored perspectives.**

Probe-infrastructure notes (witnessed by failures during the run):
- `probe_harness` is NOT loaded by `[stack]` — first run exited 2 with `Unknown procedure: probe_harness:with_overlay/3`. Fixed with explicit `use_module(probe_harness)`.
- `domain_priors:base_extractiveness/2` is **static** — retract throws `permission_error`. The ε overlay must target `narrative_ontology:constraint_metric/3`, which is the table the computed path actually reads: `drl_core:base_extractiveness` → `constraint_data:base_extractiveness` → `narrative_ontology:constraint_metric(C, <extractiveness_metric_name>, V)` (`drl_core.pl:84`, `constraint_data.pl:11–13`, pasted in session). The testsets' `domain_priors:base_extractiveness` facts are a write-side mirror, not the ε read path.

Exclusion (by design, stated): validation tests, `perspectival_gap/2`, and the export's `"classifications"` key DO read authored facts — A1 tests the computed classification path only; those consumers are dispositioned in A3.

---

## A2 — Straitjacket: directionality keys on the power atom, not agent identity. **VERIFIED; collapse exists as claimed.**

Probe: `a2_probe.pl`, output `a2_run.txt`. All five predictions named in advance from the clause chain (`derive_directionality/3` → `power_role_heuristic/4` → `exit_modulation/2`, `constraint_indexing.pl:405–469`); all five hit:

```
A2 baseline:              d=0.5  type=scaffold     chi=0.2275   victims=[3 names]
A2 one_victim_removed:    d=0.5  type=scaffold     chi=0.2275   victims=[2 names]   <- identity changed, d unchanged
A2 other_victim_removed:  d=0.5  type=scaffold     chi=0.2275   victims=[2 names]   <- different identity, d unchanged
A2 all_victims_removed:   d=0.46 type=scaffold     chi=0.1920   victims=[]          <- existence boolean flipped: probe CAN move d
A2 override_powerful_0_9: d=0.9  type=tangled_rope chi=0.4755                       <- the ONLY handle is the atom
A2 post_restore_control:  d=0.5  type=scaffold     chi=0.2275                       <- restore verified
```

Mechanism (code witness, re-derived this session): `context/4` has no agent-identity slot; `beneficiary_victim_directionality` consumes only HasBeneficiaries/HasVictims **existence booleans** + power atom + exit (`constraint_indexing.pl:417–442`); `directionality_override/3` is keyed `(Constraint, PowerAtom, D)` (`:407`, schema `DirectionalityOverride.power_atom`). Two opposed powerful agents therefore collapse to one d/χ/type, and the override moves them **together** (scaffold→tangled_rope above). The motivating problem is correctly stated.

Boundary note for the migration plan: per-(C,Name) d resolves the *derivation* collapse; whether it resolves the *classification* collapse is a Phase-A pilot fact with a declared falsifier (the contention story must flip across framings), not a Pass-1 fact.

---

## A3 — Consumer verdict table. **Every grep hit dispositioned.**

Classes: **(a)** reads authored perspective facts / authored `perspectives[]` — must migrate or be re-pointed; **(b)** reads pipeline-output COMPUTED context tuples — survives untouched; **(c)** compiler/schema/linter/generator — Tier-1 rewrite surface; **(d)** demo/infra/none.

Pivotal wiring fact (witnessed): pipeline rows carry BOTH `"perspectives"` = **computed** per-power `dr_type` at the 4 standard contexts (`json_report.pl:804–819`) and `"classifications"` = **authored** facts dump (`json_report.pl:341–348`). Python disposition keys on which the script reads.

### Prolog

| Consumer | Site(s) | Class | Note |
|---|---|---|---|
| `drl_core.pl` `perspectival_gap/2` | :619–642 | (a) | recompute over seats at migration |
| `drl_core.pl` `cross_context_analysis/2` | :652–658 | OPEN | callee unverified this pass; verify before migration (graduation: read the findall body) |
| `signature_detection.pl` mountain-unanimity | :1184–1185 | (a) | already OQ-70-suspect; retire or recompute |
| `signature_detection.pl` `claimed_natural/2` | :903–907 | (b)-equiv | reads story-level `constraint_claim`, NOT perspectives (witnessed A1) |
| `narrative_ontology.pl` `check_indexical_relativity`, `validate_indexical_completeness`, `detect_omega(_, mandatrophy)` | :365–378, :395 | (a) | validation/omega utilities over authored facts |
| `boltzmann_compliance.pl` | :472 | (a)/OPEN | authored read confirmed by hit; exact role verify at migration |
| `constraint_indexing.pl` `constraint_claim_indexed`, `compare_perspectives`, conflict finders | :169, :688–719 | (a) | query utilities |
| `data_validation.pl` | :132, :172 | (a) | "has ≥1 classification" gates → become stakeholder-presence gates |
| `data_repair.pl` | :154–203 | (a) | v3.4 legacy repair |
| `report_generator.pl` mandatrophy gap reports | :167–176, :240–269 | (a) | powerless/institutional authored seats |
| `test_harness.pl` | :113 | (a) | declared-type extraction |
| `logical_fingerprint.pl` | :480 existence check; `standard_context_for_power` | (a)+(d) | the latter feeds the COMPUTED export path |
| `json_report.pl` | :343 authored dump; :804–819 computed | mixed | computed emitter survives; `"classifications"` key migrates/retires |
| `reading_diff.pl` | :13, :67 | (a) **by design** | "READS AUTHORED CELLS ONLY"; under R4 it is the **four-tuple-arm instrument** for the A/B pair — keep for the control arm; re-point only at Phase C |
| `corpus_loader.pl` :35, `probe_harness.pl` | comments | (d) | none |
| `constraint_instances.pl`, `tangled_rope_examples.pl` | clause/fact defs | (d) | engine demo data (excluded from corpus stats per KNOWN gotcha) |
| `validation_suite.pl` | auto-generated | (c) | regenerated by run_pipeline |

### Python / agent

**(b) — survive untouched** (read computed `perspectives`/orbit keys; spot-witnessed use-sites pasted in session): `game_theory_cover_story`, `query` (:123; but :457 reads `classifications` — display-only, dual-listed), `sweeps/representation_robustness_sweep`, `boolean_independence`, `batch_claim_reconciliation`, `evaluative_convergence`, `theater_coalition_crosstab`, `oracle_gap_analysis`, `type_count_reconciliation`, `rope_dominant_spot_check`, `tangled_rope_sign_flip`, `sweeps/epsilon_sensitivity`, `tangled_decomposition`, `sweeps/game_theory_delta_sensitivity`, `sweeps/game_theory_pi_sensitivity`, `game_theory_mixed_strategy`, `golden_file_check` (:32–45 witnessed), `fcr_ablation`, `enhanced_report` (`_authored_vs_computed` compares claimed_type — which stays — against computed seats), `tangled_gradient`, `chi_variance_decomposition`, `testset_rebuild` (reads computed; WRITES testsets — repair tool, flag at cutover), `husk_signature_read` (prose), `sweeps/proof_of_life_surface3` (comment), `sweeps/demotion_pass` (false hit: `boltzmann_min_classifications` is a param name).

**(a) — must migrate**: `generate_manual` (authored json list), `sotu_mountain_decoupling` (json list + `classifications`), `find_u2_exemplars` (.pl regex), `fix_missing_claims` (.pl regex), `cluster_space_phase5` (.pl regex), `orbit_characterization` (:143 `classifications`), `idea_site_exploration` (:162,210 `classifications`), `sweeps/position_geometry_metric_sensitivity` (authored list), `perspective_analysis` (gen-experiment artifacts).

**(c) — Tier-1 rewrite surface**: `generate_constraint_pl` (validate+emit+tests), `linter` (powerless/institutional-required rules :96–101, Rule 18), `validate_constraint_story`, `story_repair`, `sotu_generate_batch`, `agent/orchestrator`, `agent/perspective_experiment`; plus the migration's own subjects (`agent/generate_kernel_corpus`, `agent/c-orchestrator`, `agent/story_generator_base`, `schemas/constraint_story_schema.json`, the generation prompt + example).

---

## A4 — Mechanical alignment. **85.0% — middle band → PROCEED with residue ledger; R2 is a recompute, not a redefinition.**

Full ledger: `a4_residue_ledger.md`; probe `a4_role_alignment.py`; raw `a4_results.txt`.

- 1230 mentions / 100 stories; **1046 clean (85.0%)** by the pre-registered probe (537 mechanical-by-construction from beneficiaries[]/victims[], 509 by role-keyword); ~89% after keyword-artifact correction (51 regex misses, reported separately).
- Cuts 90/70: operator-declared fit-for-purpose, revisable against the ledger — recorded as such, not as discovered boundaries. The <70 escalation did not fire.
- **Systematic residue** (the ledger's payload): **contender** 77 (6.3%) — the dial-set backgrounds *contention itself* (no role for the organized counter-builder; same shape as A2's collapse). **ritual_operator** ~23 (1.9%) — institutions administering atrophied constraints; cross-confirms R5 (these are the zombie population, surfaced in agent lists). **dual_role** ~16; **non_agent** ~7 (OQ-64 recurrence → gate agent-hood in the stakeholder schema).
- Mechanical-alignment half: `reading_diff.pl:1–28` witnessed — observer-axis cells align mechanically on the closed (P,T,E,S) tuple with the alignment relation an explicit argument. A closed ROLE enum aligns the same way; NAMES are bespoke and are not the alignment key. **Discovery regime.**

---

## A5 — ε pinning. **Post-hoc overlay pin WITNESSED feasible; recommend unpinned generation + overlay-pinned diff + ε-delta axis.**

ε path (witnessed): schema `base_properties.extractiveness` → compiler emits `narrative_ontology:constraint_metric(C, extractiveness, V)` (+ `domain_priors` mirror, not on read path) → all computed consumers read via the `constraint_data` bridge (`drl_core.pl:84`, `constraint_data.pl:11–13`).

A1 mutation (ii) **is** the feasibility witness for post-hoc pinning: the overlay machinery changed ε and every register recomputed. Available designs: **(α) post-hoc overlay pin** (arm-B ε := arm-A ε at analysis time; no authoring leak; witnessed) — recommended; **(β) prompt-pinning** (hands arm B a number it should author independently — de-leak tension; rejected as default); **(γ) unpinned + raw ε-delta as separate diff axis**. Plan: generate unpinned (preserve authoring independence), compute the cross-framing diff twice — raw and ε-pinned-by-overlay — and report ε-delta as its own axis, separating framing-moved-the-mountain from ε-happened-to-move.

---

## A6 — Cross-reading coupling. **Benign ONLY on a new predicate; guard asymmetry found.**

Witnessed: `shared_agent_link/4` reads `constraint_beneficiary`/`constraint_victim` (`drl_purity_network.pl:111–118`). The intra-kernel guard exists at the contamination-network site (`:96–98`: `\+ (cs_kernel_id(C,K), cs_kernel_id(Other,K))`) — **but NOT at the second consumer**: `inferred_coupling_protocol.pl:218–222` consumes `shared_agent_link(C1, C2, _, _)` raw (only `C2 \= C1`). Same-kernel shared names are filtered from contamination but DO enter `run_coupling_protocol`'s edge set.

Live-corpus baseline (probe pasted in session): 504 distinct agent atoms, 25 shared across ≥2 constraints, 38 cross-constraint pairs.

Verdict: the per-agent cross-reading join is benign **iff** stakeholder names ride a NEW predicate (e.g. `narrative_ontology:stakeholder/7`) that no coupling machinery consumes, and the derived `constraint_beneficiary`/`victim` facts are NOT name-stabilized across readings. If stable names were pushed into beneficiary/victim, the guard asymmetry makes the re-coupling **silent** at the inferred-coupling site. Plan constraint recorded. OPEN (graduation: enumerate `run_coupling_protocol` output consumers before Phase A lands the projection — one grep + read).

---

## A7 — R5 apparatus map. **EXTEND the (dangling) mandatrophy apparatus + computed piton; do not add a third surface.**

Witnessed chain:
- Schema `base_properties.mandatrophy_resolved` (boolean) → **0 compiler emissions** (`grep -c "attribute(" generate_constraint_pl.py` = 0; only `commentary.mandatrophy_analysis` prose is emitted, :624–646). The single live-corpus grep hit is commentary prose (`challenge_as_commons_maintenance.pl:193`), not a fact.
- Engine consumers read inputs nothing produces: `has_mandatrophy_declaration/1` reads `attribute(C, lifecycle, mandatrophy)` (`narrative_ontology.pl:117–121`; 0 facts on the live corpus); `is_mandatrophy_resolved/1` = 2 hardcoded archived-corpus facts (`:389–390`, gale_shapley / planetary_boundaries — neither live). Positive control: the same greps find the engine-side definitions, so the corpus absences are real, not probe blindness.
- Computed side is alive: piton dead-coordination via `classify_from_metrics` (`drl_core.pl:300–323`), `detect_omega(_, mandatrophy)` (`narrative_ontology.pl:393–401` — itself an (a)-class authored-perspective reader, see A3).

**Verdict: the existing mandatrophy authoring surface is a Build-Discipline Pattern-1 dangling wire.** R5's genealogy field + mismatch consumer should REWIRE it: the zombie flag (founding-problem dead + world-rearranges) is what `mandatrophy_resolved`/`has_mandatrophy_declaration` wanted to be, with the computed piton/theater path as its cross-check, and the A4 `ritual_operator` residue (~23 mentions) as its witnessed authoring-side population. One canonical thing (Pattern 2): extend, don't fork.

---

## OPEN items (graduation steps)

1. `drl_core.pl` `cross_context_analysis/2` callee (:652–658) — read body before consumer migration.
2. `boltzmann_compliance.pl:472` exact role — read clause before consumer migration.
3. `run_coupling_protocol` output consumers (A6 blast radius) — enumerate before Phase A lands the projection.
4. Whether per-(C,Name) d *resolves* (vs relocates) the classification collapse — Phase-A pilot fact; declared falsifier: the two-powerful-agents contention story must flip across framings.
5. A4's keyword-corrected 89% is judgment-assisted (51 reclassifications + 9 INFERRED) — marked in the ledger; the pre-registered 85.0% is the headline.
