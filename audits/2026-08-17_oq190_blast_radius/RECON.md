# OQ-190 RECON — what data exists, what is answerable

**Executed:** 2026-08-17 · read-only pass · decides nothing.
Prior art grep (`docs/technical/build_discipline.md`): `constraint_captured`, `agent_beneficiary`,
`has_mandatrophy_declaration`, `stakeholder_gain_flow`, `founding_problem_corroboration_class` —
**no hit** for any of the five as a build-discipline mechanism name. `shared_agent_link` — no hit.
So the findings below are not re-discoveries of a documented pattern instance.

---

## 1. Authored field vocabulary (frozen from `python/cohort_stability.py:180–220`)

`FIELDS` is the declared single source of truth (`cohort_sigma_seat_eval.py:32` imports it).
Buckets as the plan freezes them:

- **Cast (9):** `base_properties.{beneficiaries,victims,vindicated_propositions}`,
  `stakeholders.{roster_card,role_multiset,power_multiset,time_horizon_multiset,
  exit_options_multiset,spatial_scope_multiset}`
- **Verdict (2):** `six_questions.{disappearance_verdict,founding_problem_status}`
- **Apparatus-presence (control-eligible):** `boltzmann.presence`, `network.presence`, `interval`
- **Degenerate constants (witness nothing):** `emerges_naturally`, `claimed_type`,
  `has_sunset_clause`, `omegas.count`

Computed fields (`chi`, `dr_type`, `h1_band`, purity, fingerprint) get **no bucket here** — §4 assigns it.

### 1a. RECON FINDING — the OQ-118 stability scores are not what a `status` read gives

Read from `audits/2026-06-12_cohort_zero/stability_table.json` (**not** re-run; `git status --short
audits/2026-06-12_cohort_zero/` is clean at OPEN, witnessed in `audit_log.md` — the file's mtime is
2026-08-17 16:00 but its content is byte-identical to HEAD, so nothing was mutated).
Artifact: `stability_positive_grade.tsv`.

| field | bucket | stable/6 | **positive**-stable/6 | absence-stable/6 |
|---|---|---|---|---|
| `base_properties.beneficiaries` | cast | 0 | **0** | 0 |
| `base_properties.victims` | cast | 4 | **0** | **4** |
| `base_properties.vindicated_propositions` | cast | 1 | **0** | **1** |
| `stakeholders.roster_card` | cast | 2 | **2** | 0 |
| `stakeholders.role_multiset` | cast | 0 | **0** | 0 |
| `stakeholders.power_multiset` | cast | 0 | **0** | 0 |
| `stakeholders.time_horizon_multiset` | cast | 1 | **1** | 0 |
| `stakeholders.exit_options_multiset` | cast | 0 | **0** | 0 |
| `stakeholders.spatial_scope_multiset` | cast | 0 | **0** | 0 |
| `six_questions.disappearance_verdict` | verdict | 4 | **4** | 0 |
| `six_questions.founding_problem_status` | verdict | 3 | **3** | 0 |
| `boltzmann.presence` | apparatus | 6 | **6** | 0 |
| `network.presence` | apparatus | 6 | **6** | 0 |
| `interval` | apparatus | 6 | **6** | 0 |

**Two things this changes, and one it confirms.**

1. **`victims` at "4/6 stable" is Pattern 5 — absence satisfies the gate.** All four stable cells
   are `agreement_kind: absence`: the field was absent in every draw of that story, so the
   comparison had nothing to compare. Positive draw-stability of `victims` across the frozen
   cohort is **0/6**, not 4/6. A reader taking `status` at face value would report `victims` as the
   most stable cast field in the set; it is tied for the least. Same shape, smaller, on
   `vindicated_propositions` (its lone stable is absence).
2. **Cast is not uniformly 0/6.** `roster_card` (2/6) and `time_horizon_multiset` (1/6) carry
   *genuine positive* stability. The plan's "~0/6" holds for 7 of 9 cast fields and for the
   population aggregate (**3 positive-stable cells out of 54**), but it is not literally true
   field-by-field, and §(b)'s "fails by construction at name-identity grade" is therefore a claim
   about the *population*, not a theorem about every row.
3. **Confirmed and load-bearing for Limb 3: the verdict bucket is materially more stable than the
   cast bucket** — 7/12 positive-stable vs 3/54. Whatever the temperature confound does, the
   verdict fields are not in the same stability regime as the cast fields, and a disposition that
   treats "cast/verdict" as one radius would be wrong on the arithmetic.

Apparatus-presence rows are 18/18 positive-stable — the control-eligibility premise in §(d) holds
on measurement, not just on lineage argument.

---

## 2. The rename seam (authored name ≠ emitted predicate)

`docs/technical/generator_emission_map.md` warns it is a derived second copy. **Re-verified against
`python/generate_constraint_pl.py` at HEAD `f80bc3eb`** — the emit inventory
(`/usr/bin/grep -nE 'emit\(f?"[a-z_]+[:(]'`) is 46 `narrative_ontology:`, 5 `domain_priors:`,
1 `omega_variable`, 1 `constraint_indexing:`; the cast/verdict emit sites are lines
204, 244–260, 761, 792–809, 828–839. Emitted-name set for this audit:

`constraint_beneficiary/2`, `constraint_victim/2`, `constraint_vindicates/2`,
`constraint_stakeholder/7`, `stakeholder_secondary_role/3`, `stakeholder_non_agent/2`,
`stakeholder_gain_flow/2`, `fixing_cost_class/2`, `disappearance_verdict/2`,
`founding_problem_status/2`, `founding_problem_corroboration_class/2`.

**Derived views a name-grep misses** (sibling-surface sweep, per false-absence sub-rule (c)) —
these are how the Prolog rule bodies actually consume the cast surface:

| Derived view | Site | Reads |
|---|---|---|
| `has_coordination_function/1` | `narrative_ontology.pl:371–372` | `constraint_beneficiary` (presence) |
| `has_asymmetric_extraction/1` | `narrative_ontology.pl:380–381` | `constraint_victim` (presence) |
| `agent_beneficiary/2` | `narrative_ontology.pl:567–569` | `constraint_beneficiary` minus `non_agent_beneficiary` |
| `has_mandatrophy_declaration/1` | `narrative_ontology.pl:158–170` | `attribute/3` **or** `founding_problem_status`+`disappearance_verdict` |
| **`constraint_captured/1`** | **`narrative_ontology.pl:332–335`** | **`stakeholder_gain_flow` ⋈ `constraint_stakeholder` on RECEIVER NAME** |
| `uncaptured/1`, `piton_candidate/1`, `transient_neglect/1` | `narrative_ontology.pl:344–363` | `stakeholder_gain_flow`, `fixing_cost_class` |

### 2a. RECON FINDING — `constraint_captured/1` is a name-identity join, and it gates classification

`constraint_captured(C) :- stakeholder_gain_flow(C, Receiver), Receiver \== diffuse,
constraint_stakeholder(C, Receiver, _,_,_,_,_), !.`

This joins two cast fields **on the receiver's name**. Its consumers are not diagnostic:

- `drl_core.pl:420` — the scaffold clause of `classify_from_metrics/6` (`\+ constraint_captured(C)`)
- `signature_detection.pl:1220, 1378, 1477` — CI_Rope / pure_coordination gates (OQ-94 rows 1–3, all ruled GATE)
- `maxent_classifier.pl:184, 203–204` — `boolean_spec(scaffold, constraint_captured, forbidden)`

So a **name-identity-grade** cast dependency sits directly on the `dr_type` path. `roster_card`
scores 2/6 positive-stable; the receiver name has to survive *and* match a roster entry that also
has to survive. This is the sharpest single edge found in recon.

### 2b. RECON FINDING — the plan's §(e) closure hop does not exist

§(e) names `has_mandatrophy_declaration/1 → check_indexical_relativity/1` as a verdict-bucket
closure edge. **`check_indexical_relativity/1` is not a predicate anywhere in the repo** — the only
two occurrences are prose comments (`narrative_ontology.pl:155` and `:576`). `has_mandatrophy_declaration/1`
is exported at `:111` and has **no consumer in `prolog/`, `python/`, or `agent/`**. It is a T5b
(inert-unconsumed) row, not a closure hop. The prereg carries the correction rather than the pin.

---

## 3. Corpus prevalence (per leg, disk-verified 2026-08-17 at HEAD `f80bc3eb`)

Counted as *files emitting the fact* over the non-recursive `*.pl` glob per leg — the same
denominator `corpus_loader:corpus_constraint/1` uses (id = file base name). Artifact:
`corpus_prevalence.tsv`.

| emitted fact | testsets/279 | haiku/960 | flash/960 | kimi/1005 | sonnet/1001 |
|---|---|---|---|---|---|
| `constraint_beneficiary` | 252 | 942 | 944 | 974 | 997 |
| `constraint_victim` | 227 | 841 | 877 | 915 | 940 |
| `constraint_vindicates` | 150 | 349 | 146 | 314 | 735 |
| `constraint_stakeholder` | 236 | 494 | 748 | 987 | 1000 |
| `stakeholder_gain_flow` | 131 | 408 | 185 | 583 | 617 |
| `disappearance_verdict` | 237 | 494 | 768 | 1004 | 1001 |
| `founding_problem_status` | 236 | 494 | 768 | 1004 | 1000 |
| **`founding_problem_corroboration_class`** | **0** | **0** | **0** | **0** | **0** |

The blast radius is **not empty by absence** — every cast/verdict field except one is densely
authored on all five legs.

### 3a. RECON FINDING — `founding_problem_corroboration_class/2` is authored nowhere

Emitted by the generator (`:838–839`), declared in `narrative_ontology.pl`, read by
`enhanced_report.py` and `stakeholder_seats.pl` — and **0/4205 across all five live legs**. That is
a T5 (inert-**unexercised**: consumers starved of input), the opposite condition from `constraint_vindicates`
and `has_mandatrophy_declaration` (T5b, inert-**unconsumed**). Both land in the census; neither is `cleared`.

---

## 4. Derivation graph — what the computed layer's bucket turns out to be

Seeded from the channels the plan names, each re-read at HEAD and each carrying its **grade**:

| # | Source (cast/verdict) | → derived | Site | Grade |
|---|---|---|---|---|
| E1 | `constraint_beneficiary` → `agent_beneficiary` → `HasBeneficiaries` → `BaseD` → `d` → **χ** | `constraint_indexing.pl:455–458, 465–468` | **presence** |
| E2 | `constraint_victim` → `HasVictims` → `BaseD` → `d` → **χ** | `constraint_indexing.pl:459–462` | **presence** |
| E3 | `constraint_victim` count ≥ `critical_mass_threshold` → `resolve_coalition_power` | `constraint_indexing.pl:523–528` | **cardinality** |
| E4 | `constraint_beneficiary` → `has_coordination_function` → scaffold clause of `classify_from_metrics/6` | `narrative_ontology.pl:371–372`, `drl_core.pl:411` | **presence** |
| E5 | `constraint_victim` → `has_asymmetric_extraction` → maxent/boltzmann/fingerprint specs | `narrative_ontology.pl:380–381` | **presence** |
| E6 | `stakeholder_gain_flow` ⋈ `constraint_stakeholder` → `constraint_captured` → scaffold clause + 3 signature gates + maxent spec | `narrative_ontology.pl:332–335`, `drl_core.pl:420`, `signature_detection.pl:1220/1378/1477` | **name-identity** |
| E7 | `constraint_beneficiary`/`constraint_victim` counts → `fingerprint_actors` topology (`none`/`concentrated`/`distributed`) | `logical_fingerprint.pl:296–306` | **cardinality** |
| E8 | `agent_beneficiary` → `false_summit_mountain` / `natural_law_signature` beneficiary count | `signature_detection.pl:1693+` | **cardinality** |
| E9 | `stakeholder_gain_flow`+`fixing_cost_class` → `uncaptured`/`piton_candidate` → FCR-branch refinement | `narrative_ontology.pl:344–363` | **name-identity** (via `uncaptured`: presence) |
| E10 | `founding_problem_status`+`disappearance_verdict` → `has_mandatrophy_declaration` → **(no consumer)** | `narrative_ontology.pl:168–170` | terminal — T5b |

`dr_type` is reached at **presence grade** (E1/E2/E4) and **name-identity grade** (E6).
`h1_band` is over the signature-resolved `dr_type` orbit, so it inherits both.
`logical_fingerprint` is reached at **cardinality** grade (E7).

**The [EDGE] prediction in the plan is confirmed, and is stronger than predicted:** the plan
predicted presence/cardinality reach into classification. E6 reaches it at **name-identity** grade —
the grade OQ-118 *did* score, and scored at 2/6 positive for `roster_card`.

Consequences already fixed by this, and carried into the prereg:
- The computed layer **cannot serve as the cast decline control** (§(d) already forbids it; recon
  now shows the forbidding is load-bearing, not precautionary).
- A name-keyed grep cannot reach E1–E9's downstream consumers; the closure is required.

Edge **admission** (can the source *change* the derived value, not merely be read) is executed in
Phase 3 by perturbation, per the prereg's pruner clause; the ten above are *candidates* seeded here.

---

## 5. What is answerable, and what is not

**Answerable in this arc (Ω_E, zero generation spend):** the emitted-name consumer census; the
T2b closure over the graph; corpus prevalence per leg; the documentary sweep over the active
surface; the resolved-ISSUES closure-premise sweep; the verdict-bucket exhaustion set.

**Not answerable in this arc:** presence-grade and cardinality-grade draw stability. OQ-118 scored
name-identity only, so E1–E5, E7, E8 land `SUSPECT-confirmed-grade-unmeasured` by construction —
which is exactly the routed follow-on OQ the plan predicts (re-score the same 17 frozen replicates
at the two coarser grades; a re-read of artifacts already on disk).

**Newly answerable, not in the plan:** E6 is name-identity grade, so its dependents *are* scoreable
against OQ-118 — and `roster_card`'s 2/6 is a positive number rather than the 0/6 the plan assumed
throughout. The prereg records that §(b)'s "fails by construction" is a population claim.
