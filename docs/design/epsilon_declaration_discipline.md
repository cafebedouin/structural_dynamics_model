# The ε Declaration Discipline (spec)

**Tracker:** OQ-205 (`ISSUES.md` — the single tracking surface; this document is the spec it
points at, per the `v8_seat_gauge_orientation_design_spec.md` precedent).
**Obligation source:** `docs/deferential_realism_paper_v8.md` §6.4 — v8's handed-forward
artifact. ε is the framework's least-grounded, most load-bearing primitive: authored by
judgment, not computed from beneath. The Coupling Theorem settles the world-anchor question
negatively (a world-anchored ε would be a seat-free seat), so what is owed is **declaration**,
not grounding: (a) provenance carried with the value and surfaced at read sites; (b) stability
as a checked, surfaced fact.
**Consumer (Pattern 1):** the OQ-205 build phase implements this spec; its first empirical
customer is OQ-78 (§8).
**Anchors:** all code citations verified at commit `6c59615e` (2026-07-03). Line numbers are
convenience only; the predicate/param name is the anchor.
**Status:** spec landed 2026-07-03; build NOT started. Controls in §6 are pre-registered, not
run. Rulings R2–R4 (§9) **RATIFIED by the operator 2026-07-03** with two amendments folded
into §3 (three-site equality check) and §5 (two-class stability flag), and a promotion
trigger attached to R4.

## 1. Scope and non-goals

**In scope:** the ε provenance carrier (schema, fail-closed rule, read-site coverage), the
ε-stability protocol (radius, flag semantics, surfacing), pre-registered positive controls for
both, graduation criteria for the build.

**Out of scope:** grounding ε (settled negatively in principle — v8 §6.4); any threshold or
classification change (the recalibration debt is OQ-48, untouched here); constraining
generation for run-reproducible ε (OQ-26 option (b), deferred there); the SCOPE-stage
`epsilon_bin` channel design (OQ-78/OQ-117 territory — §3's anti-leak rule is the only
interaction).

## 2. Disambiguation: three ε principles that must not conflate

The repo already carries two other principles with "ε-invariance" in their vocabulary. They
answer different questions. Nothing in this spec may be titled "ε invariance."

| Principle | Home | Claim | Violated by (example) |
|---|---|---|---|
| ε-invariance across **observables** = constraint identity | DP-001 (`docs/dp001_epsilon_invariance_constraint_identity.md`, `docs/epsilon_invariance_principle.md`) | If two observables yield different ε, that is two constraints (decompose, link via `affects_constraint/2`), never one constraint with a measurement parameter | Adding a `measurement_basis` 5th context axis (the rejected Comitatus-class proposal) |
| ε reading-relativity across **generation runs** | OQ-26 close; Axiom 2 as amended (`docs/deferential_realism_paper_v6.13.1.md` ll.66–91) | ε is invariant across observer positions but NOT across generation runs; ε-dependent statistics are scoped to one coherent generation | Citing an H¹ proportion pooled across two generation regimes as one statistic |
| ε **declaration** (this spec) | OQ-205; v8 §6.4 | Every ε carries who/what authored it, and whether conclusions anchored on it survive small perturbation — both as checked facts at read sites | A report headline built on a `Val = 0.0` fabricated ε with no flag (§3, live instance) |

The three are orthogonal: the first is about constraint *identity*, the second about statistic
*scope*, the third about value *authorship and robustness*. OQ-78's clustering finding
(same-run, cross-topic) is likewise distinct from OQ-26 (same-topic, cross-run) — the OQ-78
body states the distinction; it transfers here unchanged.

## 3. Provenance schema

**Recommendation (R2, committed — operator ruling requested at review).** A new per-constraint
fact in the story-file namespace, loaded with the story so absence is detectable at load:

```prolog
narrative_ontology:epsilon_provenance(Constraint, ValueAsWritten, Author, GenerationRunId, Route).
```

- `ValueAsWritten` — the authored ε literal, redundantly recorded so drift between the
  provenance record and the live `constraint_metric` fact is a checkable inequality (Control P
  gates on equality).
- `Author` — the authoring model atom (e.g. `sonnet_4_5`) or `human` for hand-authored
  literals.
- `GenerationRunId` — the run tag / batch id, or `none` for hand authorship.
- `Route` — how the value was produced: `scope_bin(Bin)` (the SCOPE-stage `epsilon_bin` →
  generation mapping, OQ-78's two-layer mechanism), `direct`, `hand_authored`, `seed_inherited`.
  Every fabrication source gets its own token — a binary authored/not split launders
  fabrication into "authored" (provenance-buckets rule).

Prompt/schema commits and sampling params are NOT duplicated here: where
`narrative_ontology:story_provenance/8` exists (`narrative_ontology.pl:98–107` — C,
PromptCommit, SchemaCommit, Date, SourceEssay, OneShotExample, Model, SamplingParams), they
come from the join on C. `epsilon_provenance/5` must stand alone only for stories with no
`story_provenance/8` (the hand-authored class, e.g. `constraint_instances.pl` carbon_tax_2026,
ε = 0.55, currently provenance-less — the natural Control P planted-case).

**Rejected alternatives (recorded so the fork is not relitigated blind):**
- *Widen `story_provenance/8`* — arity change touches every existing read site and every
  already-committed story file; and ε-specific fields (Route, ValueAsWritten) are not cohort
  metadata.
- *JSON sidecar* — splits the fact from the value it describes (Pattern 2, the silent fork);
  load-time absence detection is lost.

**The dual-authoring fork (found at recon, must be covered).** Every story file authors ε
*twice*: `domain_priors:base_extractiveness(C, V)` and
`narrative_ontology:constraint_metric(C, extractiveness, V)` (e.g.
`testsets/ability_ceiling_reading.pl:101,106`). The live read path uses only the second
(`drl_core.pl:85` → `constraint_data:base_extractiveness/2` → `constraint_metric/3` via
`config:param(extractiveness_metric_name)`), so a divergence would be silent. And
`epsilon_provenance/5`'s `ValueAsWritten` makes a **third** in-file ε site. **The equality
check the build must land covers all three** (operator amendment at R2 ratification,
2026-07-03): `domain_priors:base_extractiveness/2` = `constraint_metric/3` =
`ValueAsWritten`, per story, validation_suite class — otherwise the provenance fact becomes a
third fork surface instead of the anchor it exists to be.

**Fail-closed rule (the spine rule applied to ε).** At every *surfaced* read site (§4:
report/emission/derived-surface layer), a `base_extractiveness` read without a matching
`epsilon_provenance` fact yields `unknown`/loud — never a silent pass, never a fabricated
default. The classification core already fail-closes on missing ε (the `is_X/3` clauses fail
when `base_extractiveness/2` fails); this rule extends the same posture to the surfaces.

**No-backfill ruling (operator, 2026-07-03, at build-plan review).** No `prolog/testsets*`
leg is backfilled with `epsilon_provenance/5` — they are test corpora (the standing
"test bed, not backfill target" ruling). The discipline lands **generator-forward**: new
stories emit the fact from the compiler; the ENTIRE existing corpus is the **declared
loud-null stratum** (criterion 1's sanctioned second arm — surfaced per-story as the
emission's `"none_authored"` token, counted at the gate every run), and corpus-complete
arrives at the future rebuild. No edits to existing story files or `constraint_instances.pl`
literals.

**Named live violations the build must fix (Pattern-5 instances, both verified at `6c59615e`):**
1. `constraint_indexing.pl:902–903` — `get_true_metric(C, extractiveness, Val)` falls back to
   `Val = 0.0` when `constraint_data:base_extractiveness/2` fails: a fabricated
   mountain-shaped ε (0.0 ≤ every floor) feeding `observer_accessible/3`'s restricted views.
2. `boltzmann_compliance.pl:248–252` — `classify_at_context_impl/3` falls back to
   `BaseEps = 0.5` on a missing extractiveness metric: the OQ-89 neutral-default class
   (0.5 > `snare_epsilon_floor` 0.46), fabricated inside a classification path. Sibling
   `Supp = 0` fallback two clauses down, same class.

Both become `unknown`-propagating (or fail) under this spec; each fix inherits the
output-changing commit discipline (own commit, before/after diff).

**Anti-leak constraint (OQ-78's de-leak in reverse).** Provenance reads authorship *out*;
nothing feeds target-ε *into* generation. No build artifact of this spec may disclose
threshold values, target bins, or desired ε to the authoring model. The OQ-78 probe history
(dead `epsilon_bin` channel; scrubbed numeric type-bands, commit `b6c4e113`) is the precedent.

## 4. Read-site enumeration

Sites that must carry or surface the provenance bit (build phase wires them; anchor =
predicate/param name at `6c59615e`):

| Layer | Site | What it does with ε |
|---|---|---|
| Authoring | `testsets*/​*.pl` dual facts (`domain_priors:base_extractiveness/2` + `narrative_ontology:constraint_metric/3`); temporal series `measurement/5` on `base_extractiveness` | The authored values themselves (§3 fork note) |
| Authoring (hand) | `constraint_instances.pl:151` carbon_tax_2026 `ε = 0.55` | Hand-authored literal, no provenance — Control P case |
| Read path | `drl_core.pl:85` → `constraint_data.pl:11–13` (via `extractiveness_metric_name`) | Canonical live read |
| Classification | `drl_core.pl:130–171` six `is_X/3` clauses → `classify_from_metrics/6` | ε gate of the dual-threshold rule |
| Restricted views | `constraint_indexing.pl:871–903` `observer_accessible/3` → `get_true_metric/3` | **`Val = 0.0` fabrication (fix, §3)** |
| Coalition upgrade | `constraint_indexing.pl:~513` `resolve_coalition_power/3` reads ε vs `snare_epsilon_floor` | Threshold-gated side path |
| Derived | `logical_fingerprint.pl:357–371` `extraction_zone/2` | Zone-bins ε over all five ε thresholds |
| Derived | `boltzmann_compliance.pl:248–252` `classify_at_context_impl/3`; `:503–513` `excess_extraction/2` | **`BaseEps = 0.5` fabrication (fix, §3)**; PoA excess |
| Derived | `arakelov_height.pl:~101` `arakelov_height_pair/3` | Height ∝ ε |
| Thresholds | `config.pl` ε params (§5 list); ordering invariants `config_schema.pl:485–516` | The boundary set stability is measured against |
| Emission | `json_report.pl:246–253` per-constraint `base_extractiveness` (null-fallback); `:1088–1107` per-perspective `epsilon` (null-fallback) | Where provenance joins `pipeline_output.json` |
| Reports | `enhanced_report.py` ~:1000/:1089 (cross-metric), ~:3092 (face ε), E-section consumer of `outputs/epsilon_sensitivity_results.json` (~:2117–2167) | Human-facing surfaces that must show the bit |
| Sweeps | `python/sweeps/epsilon_sensitivity.py` (data-side ε, RAW pre-override); `python/sweeps/perturb.py` (config-side) | Existing stability instruments (§5) |

`run_pipeline.py` itself does not touch ε (verified by grep — orchestration only). The
emission null-fallbacks are honest (null, not fabricated) and stay; provenance rides alongside.

## 5. Stability protocol

**Definition.** An ε value is *stable at radius r* iff the signature-resolved classification
(the post-`integrate_signature_with_modal/3` `dr_type`, per the OQ-27 disclosure — never the
raw `classify_from_metrics/6` type) is unchanged under ε ± r, all else fixed. "Conclusions
anchored on an ε survive small perturbation" (v8 §6.4) operationalizes to: every constraint
whose ε is *unstable at r* carries a flag at the §4 surfaced sites, and any cross-axis anchor
(design_discipline §7 Rule ε-stability) must be flag-free.

**The threshold set.** ε gates classification at five params (verified at `6c59615e`):
`piton_epsilon_floor` 0.10, `mountain_extractiveness_max` 0.25, `tangled_rope_epsilon_floor`
0.30, `rope_epsilon_ceiling` 0.45 (numerically shared by `scaffold_extraction_ceil` and
`piton_extraction_ceiling`), `snare_epsilon_floor` 0.46. (The plan's draft set omitted 0.25;
recon corrected it.) Threshold-proximity metric: min distance from ε to this set, vs r.
The flag itself must come from the engine's actual flip behavior (perturb-and-reclassify),
not from the distance screen alone — overlapping type-bands make config-value proximity a
false boundary (the starvation-screen lesson); the distance metric is the cheap pre-filter,
the flip is the fact.

**Census (read-only, 2026-07-03, `6c59615e`; script + raw TSVs in
`audits/2026-07-03_oq205_epsilon_census/`).** All four legs, live read path
(`corpus_constraint/1` enumeration; twins + kernel_v1 via `corpus_path` overlay). Positive
control: a constraint planted **in-memory** at `snare_epsilon_floor + 0.0005` through the same
enumeration + read path — flagged at every candidate radius ≥ 0.0005 (PASS; never written to
any corpus).

| Leg | n (with ε) | within 0.001 of a threshold | within 0.02 | within 0.05 | exactly AT a threshold | distinct values | mode |
|---|---|---|---|---|---|---|---|
| testsets (live) | 110 | 1 (0.9%) | 11 (10.0%) | 29 (26.4%) | 1 (0.25) | 28 | 0.68 × 46 (41.8%) |
| testsets_haiku | 960 | 9 (0.9%) | 105 (10.9%) | 181 (18.9%) | 9 | 42 | 0.68 × 305 (31.8%) |
| testsets_flash | 960 | **218 (22.7%)** | 220 (22.9%) | 436 (45.4%) | **218** (0.45×100, 0.30×69, 0.25×26, 0.10×23) | 30 | 0.65 × 184 (19.2%) |
| kernel_v1 | 1106 | 4 (0.4%) | 104 (9.4%) | 203 (18.4%) | 4 (0.45) | 30 | 0.58 × 382 (34.5%) |

The live leg's 9 no-ε members are the `*_contradictions` axiom meta-files (known,
OQ-136/OQ-202 strata) — measured-empty, not didn't-look.

Two census findings the protocol must absorb:
- **The flash twin authors ε exactly ON thresholds** for 22.7% of its corpus (its .x5/.x0
  authoring grid lands on 0.45/0.30/0.25/0.10, which are grid points). An exactly-at value
  flips under *any* r > 0 (boundary semantics: `rope_epsilon_ceiling` is ≤, so 0.45 + δ exits
  rope for every δ) — these stories are unstable at every radius, by authoring convention.
  **The flag therefore carries two classes** (operator amendment at R3 ratification,
  2026-07-03): `on_threshold_grid` (distance exactly 0 — an authoring-convention fact, flagged
  at every radius by construction) vs `near_threshold` (0 < distance ≤ r — the
  landed-near-a-boundary-by-chance artifact the check exists to surface). Collapsing them
  would let the flash convention swamp the signal: 218 convention flags drowning the handful
  of genuine near-misses. Read sites surface the classes separately. Both classes block a
  cross-axis anchor (an ε exactly at a threshold is maximally unstable — the design_discipline
  §7 rule applies with full force); the split is for the *readout*: `near_threshold` is the
  per-story artifact to inspect, `on_threshold_grid` is additionally a corpus-level authoring
  finding (an OQ-78-class statistic, §8) that must not read as 218 independent fragility
  discoveries.
- **The (0.45, 0.46) open interval is empty on all four legs** (0 values, all legs — the 0.01
  quantization cannot populate it). The plan's binding constraint (i) — r ≤ 0.005 lest
  every ε between the pair be within r of both — is therefore **moot on current corpora**,
  witnessed, and must be re-checked at any regeneration (a kill condition on R3, below).

**Recommendation (R3, committed — operator ruling requested at review): r = 0.02.**
Rationale from the census: below 0.02 (0.001–0.01) the flag set is degenerate — it contains
only the exactly-at-threshold class, which any r catches; at 0.02 the flag additionally
captures the .x8-rail near-miss class (0.48 vs snare floor 0.46; ~10% on live/haiku/kernel_v1),
which is exactly the "hand-authored number landed near a threshold" artifact the
design_discipline §7 rule targets; at 0.05 the flag hits 18–45% of a leg and, per OQ-78's
grid-spacing bound (authored grid is 0.1-spaced ⇒ r ≪ 0.05), the perturbation axis
degenerates into the authoring grid itself. **Kill conditions:** (a) any regeneration
populates (0.45, 0.46) → binding (i) re-activates and r must drop to ≤ 0.005 with the
degeneracy re-argued; (b) the flag rate at 0.02 on a future corpus exceeds ~1/3 of a leg →
r is reading the authoring grid, not threshold proximity — re-derive.

**Relation to existing instruments.** `python/sweeps/perturb.py` perturbs *config-side*
(thresholds move, ε fixed); this protocol is *data-side* (ε moves, thresholds fixed) — the
complementary axis. Whether the build extends perturb.py or lands a sibling is a build-phase
call; the spec states the gap, not the wiring. `python/sweeps/epsilon_sensitivity.py`'s
RAW-pre-override rule is **inherited in one direction and refined in the other**: Fisher
sensitivity is computed pre-override because overrides flatten it — but for *this* protocol
the flag is on the signature-resolved type, so an override-locked story whose raw distribution
flips while its final type holds is *stable in the flag sense* and must additionally carry the
lock bit (`override_locked`) so "stable" is never read as "insensitive" (the OQ-30
signature-locked demotion class). Control S's riskiest shape tests exactly this seam.

## 6. Pre-registered positive controls (specified now, RUN in the build phase)

Running them this session would un-pre-register them (an introduced instrument is itself a
claim). Each is same-path, two-sided, riskiest-shape:

- **Control P (provenance).** A planted provenance-less ε must fail loud through the *real*
  load path (story file consulted by `corpus_loader`, read via
  `constraint_data:base_extractiveness/2`, surfaced at a §4 site) — not through a unit-test
  shortcut. Two-sided: a fully-provenanced ε passes flag-free through the same path. Riskiest
  shapes: (a) the hand-authored literal class (`constraint_instances.pl` carbon_tax_2026);
  (b) the `get_true_metric` `Val = 0.0` fallback path — after the §3 fix, absence must read
  `unknown`, and Control P must confirm the fallback is dead, not rerouted. Plus the equality
  gate: a planted `epsilon_provenance` whose `ValueAsWritten` ≠ the live `constraint_metric`
  value must fail loud (drift detection).
- **Control S (stability).** A planted ε at threshold + δ (δ < r) must flag; two-sided: an ε
  equidistant from all five thresholds (e.g. deep in a band interior) must not flag. Riskiest
  shape: a signature-override-locked story whose raw distribution flips under ε ± r while the
  final type does not — it must still surface (as `override_locked`, per §5), because a bare
  "stable" there is the false-pass this control exists to catch.

Both controls land in the *recurring* gate/suite, not as one-off audit scripts — once OQ-205
closes, the gate is the sole enforcement (ruling/close-honesty rule).

## 7. Enforceability boundary

`docs/design/design_discipline.md` §8 (~l.821): **declaration can be mandated; honoring
cannot.** Whether every ε carries its provenance and stability bits is a checkable fact about
the corpus and the surfaces — gate-wireable, and this spec mandates exactly that. Whether a
consumer *honors* a flag (declines to anchor a conclusion on an unstable ε) is decided at use,
by the seat using it. The discipline secures the gate, not the fall; claiming more would
oversell it.

## 8. First customer: OQ-78

The provenance surface converts the OQ-78 fingerprint from a one-off census into a standing
per-run readout: mode fraction, distinct-value count, last-digit histogram, and (new, from
§5's census) the exactly-at-threshold count — each per `Author`/`Route` stratum, which the
current one-off censuses cannot do. The 2026-07-03 census is the prototype and already
extends OQ-78's evidence: the live-leg mode share has risen to 41.8% (0.68 × 46/110, n=110 vs
34% at n=91), and the rail is *model-specific* — flash authors on .x5/.x0 (mode 0.65, last
digit 5 × 599/960) where live/haiku/kernel_v1 sit on the .x8/.x2 rail. OQ-78's
"re-baseline on cohort zero" need lands here for free once the readout is standing.

## 9. Rulings (RATIFIED, operator, 2026-07-03) + graduation criteria

**Rulings — all three RATIFIED by the operator 2026-07-03, with the amendments noted:**
- **R2 (ratified)** — provenance carrier = new `epsilon_provenance/5` (§3), not widened
  `story_provenance/8`, not a sidecar. **Amendment folded into §3:** the build's equality
  check covers all THREE in-file ε sites (`domain_priors:base_extractiveness/2`,
  `constraint_metric/3`, `ValueAsWritten`) — with value-as-written the provenance fact is
  itself a fork surface unless anchored by the check.
- **R3 (ratified)** — stability radius r = 0.02 (§5), census-informed, two kill conditions
  attached. **Amendment folded into §5:** the flag distinguishes `on_threshold_grid`
  (distance 0, authoring convention — flash's 218/960) from `near_threshold` (0 < d ≤ r,
  the by-chance artifact) as separate classes, or the convention swamps the signal. Radius
  unchanged.
- **R4 (ratified)** — flag disposition: **report-surfaced, commentary-grade** (annotates,
  never overrides classification — the verdict-grade distinction; a stability flag is
  commentary about a value, not a correction of a type), plus a *gate-blocking* check only
  for the fail-closed provenance rule (§3) and the two controls (§6). Commentary-grade is
  also forced by the census: with flash's on-threshold rate, gate-blocking stability would
  hold the gate permanently red on a whole corpus. **Promotion trigger (kill condition,
  operator-attached):** if a commentary-grade stability flag is ever shown to have concealed
  a classification flip that mattered at a downstream read site, the disposition promotes to
  verdict-grade.

**Build ruling (operator, 2026-07-03):** the loud-null arm of criterion 1 is the RULED arm
for the current corpora — no backfill of any `testsets*` leg (see §3); corpus-complete
arrives at the rebuild. Build landed 2026-07-03: `audits/2026-07-03_oq205_build/`.

**Graduation criteria (the build is "done" when):**
1. `epsilon_provenance/5` landed, corpus-complete or loud-null — with the OQ-89 0.5-neutral
   stratum and the ~94/116 legacy under-vectored `json/` class named as known provenance-poor,
   never papered over;
2. every §4 read site carries or surfaces the bit; the two §3 fabrication fallbacks are fixed
   (own output-changing commits);
3. the stability check runs per-corpus; Controls P and S green **through the recurring gate**;
   witnessed in an `audits/` dir;
4. the OQ-78 standing readout (§8) is produced from the surface, per-stratum;
5. v8 §9.5's second falsifiability leg annotated built (the paper edit is one line; the
   annotation cites the audit dir).
