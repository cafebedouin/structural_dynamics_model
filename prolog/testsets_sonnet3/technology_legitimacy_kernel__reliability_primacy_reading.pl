% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability-Primacy Reading of Technology Legitimacy for Climate Mitigation
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the reliability-primacy reading of the contested
 *   technology-legitimacy kernel: a technology counts as
 *   climate-mitigation-legitimate only if it delivers dispatchable,
 *   baseload-capable generation. This reading is authored as its own
 *   ε-invariant constraint, distinct from the velocity-primacy reading
 *   (deployability within the carbon-budget timeline) and the precautionary
 *   reading (bounded, reversible worst-case risk), which are separate
 *   constraint stories linked via network.affects_constraints. Under this
 *   reading, nuclear and gas peaker capacity enter the beneficiary set
 *   because they already clear the dispatchability bar, while intermittent
 *   wind and solar must absorb storage or firming costs to qualify, and the
 *   resulting cost burden lands on ratepayers and distributed resource owners
 *   who have no seat in the resource-adequacy proceedings that set the
 *   standard.
 *
 * KEY AGENTS:
 *   - incumbent_nuclear_operators: primary beneficiary (institutional/arbitrage) — existing fleet reclassified as gold-standard without new investment
 *   - gas_peaker_fleet_owners: secondary beneficiary (powerful/mobile) — firm capacity credited despite emissions
 *   - grid_reliability_regulators: agenda_setter (institutional/analytical) — writes and enforces the dispatchability gate
 *   - ratepayers_funding_storage_mandates: primary payer (powerless/trapped) — bears cost pass-through with no exit
 *   - wind_and_solar_developers: primary target (moderate/constrained) — must fund storage to clear the legitimacy bar
 *   - velocity_and_precautionary_advocates: excluded — structurally outside the reliability-focused regulatory venue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.62).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability-Primacy Reading of Technology Legitimacy for Climate Mitigation").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, 'f1077083-aef1-49ea-8c25-ab0deaa9e162').
narrative_ontology:cs_kernel_codification('f1077083-aef1-49ea-8c25-ab0deaa9e162', distributed).
narrative_ontology:cs_authority_grounding('f1077083-aef1-49ea-8c25-ab0deaa9e162', distributed).
narrative_ontology:cs_reading_relation('f1077083-aef1-49ea-8c25-ab0deaa9e162', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1077083-aef1-49ea-8c25-ab0deaa9e162', technology_legitimacy_kernel__precautionary_reading, influences).
narrative_ontology:cs_axiom('f1077083-aef1-49ea-8c25-ab0deaa9e162', foundational, grid_stability_requires_synchronous_dispatchable_capacity).
narrative_ontology:cs_axiom_status(grid_stability_requires_synchronous_dispatchable_capacity, holdable).
narrative_ontology:cs_axiom_grounding('f1077083-aef1-49ea-8c25-ab0deaa9e162', grid_stability_requires_synchronous_dispatchable_capacity, empirically_contingent).
narrative_ontology:cs_axiom('f1077083-aef1-49ea-8c25-ab0deaa9e162', secondary, climate_legitimacy_is_conditioned_on_reliability_not_speed_or_reversibility).
narrative_ontology:cs_axiom_status(climate_legitimacy_is_conditioned_on_reliability_not_speed_or_reversibility, holdable).
narrative_ontology:cs_axiom_grounding('f1077083-aef1-49ea-8c25-ab0deaa9e162', climate_legitimacy_is_conditioned_on_reliability_not_speed_or_reversibility, instrumental).
narrative_ontology:cs_reference_frame('f1077083-aef1-49ea-8c25-ab0deaa9e162', grid_operator_resource_adequacy_doctrine).
narrative_ontology:cs_drift_state('f1077083-aef1-49ea-8c25-ab0deaa9e162', post_storage_cost_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f1077083-aef1-49ea-8c25-ab0deaa9e162', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_nuclear_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, gas_peaker_fleet_owners).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_reliability_regulators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, baseload_dependent_heavy_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_funding_storage_mandates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, wind_and_solar_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, distributed_energy_resource_owners).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, low_income_households_facing_capacity_charges).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, grid_stability_requires_synchronous_baseload).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, dispatchability_is_necessary_condition_for_climate_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own long-lived, high-capacity-factor plants that already meet the dispatchability test without new investment. The reliability-primacy standard reclassifies their existing fleet as the gold standard of climate legitimacy overnight, unlocking subsidies, capacity payments, and favorable procurement rules that were previously contested on cost and waste grounds.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_nuclear_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Operate flexible fossil generation that can be dispatched on demand. Under a reliability-primacy standard their firm capacity is treated as a necessary grid-stability service, letting them collect capacity payments and delay retirement even though their marginal emissions undercut the mitigation goal the standard claims to serve.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, gas_peaker_fleet_owners, beneficiary,
    powerful, biographical, mobile, national).

% Write and enforce interconnection standards, capacity market rules, and resource-adequacy tests that operationalize dispatchability as the legitimacy gate. They control which technologies qualify for capacity credit and can require storage, firming contracts, or outright exclusion for technologies that fail the baseload test.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_reliability_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Steel, cement, and chemical manufacturers whose processes require continuous, uninterruptible power. The reliability-primacy standard aligns policy with what they already need, giving them a legitimacy argument for continued access to firm generation and grounds to oppose renewable-heavy grid transitions that could raise curtailment risk.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, baseload_dependent_heavy_industry, beneficiary,
    powerful, biographical, constrained, regional).

% Bear the cost pass-through when regulators require wind and solar projects to pair with expensive storage or firming contracts to qualify as legitimate. Cannot opt out of utility rate structures and see bills rise to fund reliability infrastructure whose necessity is asserted rather than demonstrated against their specific grid's actual variability.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_funding_storage_mandates, payer,
    powerless, biographical, trapped, regional).

% Build technologies with zero marginal emissions but variable output. Under the reliability-primacy gate their projects are excluded from full climate-legitimacy status, capacity markets, and green procurement unless they absorb the cost of storage or firming contracts that erase much of their cost advantage. Their alternative is to build anyway at a competitive disadvantage or exit the jurisdiction.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, wind_and_solar_developers, payer,
    moderate, biographical, constrained, national).

% Rooftop solar and small battery owners whose aggregate contribution to grid stability is real but statistically diffuse and hard to certify against a dispatchability standard built for centralized plants. They receive little or no legitimacy credit and can face interconnection fees justified by the same reliability framework.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, distributed_energy_resource_owners, payer,
    powerless, biographical, trapped, local).

% Cannot relocate, self-generate, or absorb rate volatility. Capacity charges levied to fund firm generation and storage mandates fall disproportionately on this group as a share of income, with no corresponding voice in how the reliability standard is set.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, low_income_households_facing_capacity_charges, payer,
    powerless, immediate, trapped, local).

% Argue that rapid deployability within the carbon budget, or bounded reversible risk, should govern legitimacy instead of dispatchability. They are structurally outside the reliability-primacy regulatory conversation, which is conducted inside grid-operator technical bodies that treat their framing as a separate, lower-priority policy debate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, velocity_and_precautionary_advocates, excluded,
    organized, generational, constrained, global).

% Study how the reliability-primacy standard interacts with decarbonization pace and cost outcomes, without a direct stake in which technology wins the legitimacy label.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__reliability_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__reliability_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Grid operators must maintain frequency and voltage stability in real time; a standard privileging dispatchable, baseload-capable generation solves the genuine engineering problem that variable resources alone cannot guarantee instantaneous supply-demand balance without complementary firm capacity or storage.
% TRANSFER_FUNCTION: Moves capacity payments, favorable procurement status, and regulatory legitimacy toward incumbent nuclear and gas generation, and moves cost burden (storage mandates, firming contracts, capacity charges) onto ratepayers, distributed resource owners, and non-dispatchable renewable developers.
% ABSENT_VOICES: Velocity-primacy and precautionary-reading advocates are excluded from the reliability-focused regulatory venues where legitimacy criteria are actually written; distributed energy resource owners have no aggregated voice in resource-adequacy proceedings dominated by utility-scale technical stakeholders.
% DISAPPEARANCE_RATIONALE: If the dispatchability gate vanished, capacity markets would need to be redesigned around different adequacy metrics, nuclear and gas incumbents would lose a major legitimacy and subsidy lever, storage mandates on renewables would likely relax, and resource planning would shift toward probabilistic reliability models that credit aggregated variable and distributed resources — a substantial reorganization of who qualifies for capacity payments and green procurement.
% FOUNDING_PROBLEM: Grid operators needed an objective technical test to prevent under-provisioned, unstable grids as fossil baseload retired and variable renewables scaled, given real historical instances of frequency excursions and blackouts linked to inadequate firm capacity.
% FOUNDING_PROBLEM_CORROBORATION: Independent grid engineers and reliability councils (e.g., system operators publishing resource-adequacy reports) attest that firm capacity genuinely matters for stability, corroborating the founding problem as partly live. However, storage-technology researchers and renewable-integration studies from outside incumbent generation interests argue the modern grid-stability problem is increasingly solvable through storage, demand response, and transmission — suggesting the reliability-primacy framing, as currently codified favoring synchronous baseload specifically, has drifted from a genuinely open engineering question into an incumbency-preserving legitimacy filter.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a genuine coordination function (grid stability) layered with asymmetric cost allocation: the standard imposes real, measurable costs on non-dispatchable technologies and their ratepayers while conferring uncompensated legitimacy and revenue advantages on incumbents whose firmness happens to coincide with the rule. Suppression (0.62) is high because the standard is enforced through capacity-market rules and interconnection requirements that functionally exclude technologies failing the test, not merely disfavor them. Theater ratio (0.40) captures that some dispatchability requirements track genuine engineering need while a growing share functions as an incumbency-preserving qualification filter, particularly as storage and demand-response alternatives mature but are not credited proportionally. The temporal series shows extraction and enforcement intensity both rising over the interval as capacity markets increasingly formalize dispatchability into binding qualification rules rather than an advisory grid-planning consideration.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and incumbent-generator seats, the standard reads as a straightforward, technically necessary coordination mechanism protecting against blackouts. From the ratepayer and renewable-developer seats, the same rule reads as an enforced transfer that entrenches incumbents under a reliability rationale that outpaces the actual, declining engineering necessity as storage and demand-response technologies mature. The engine's per-seat computation should surface this divergence rather than resolve it toward either seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators, gas peaker owners, and grid regulators sit near the beneficiary end of directionality: the standard's coordination logic maps onto what they already provide or administer, so the constraint subsidizes their position with minimal added cost. Ratepayers, distributed resource owners, and renewable developers sit near the target end: they bear the transfer (storage mandates, capacity charges) without commensurate voice in setting the standard, and their exit options are trapped or constrained by grid dependency and regulatory jurisdiction. Heavy industry is a beneficiary by alignment of interest rather than by administering the rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (grid instability from inadequate firm capacity) was genuinely live when baseload retirements began outpacing dispatchable replacement capacity. Its status is contested rather than dead or clearly live: modern storage and virtual power plant technologies increasingly substitute for synchronous baseload in ways the standard's own codification does not credit, meaning the rule may now function partly as a legitimacy shield for incumbents rather than purely as an engineering safeguard. This is precisely the kind of divergence a mandatrophy analysis should flag rather than assume away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispatchability_engineering_necessity_vs_incumbency_shield,
    'Is the dispatchability requirement, as currently codified around synchronous baseload specifically, still tracking a genuine unresolved grid-engineering necessity, or has it become primarily a legitimacy shield preserving incumbent nuclear and gas capacity against storage-based and aggregated-DER alternatives that now substitute for the same reliability function?',
    'Independent technical audits comparing grid stability outcomes in jurisdictions with high storage/DER penetration and relaxed dispatchability gates against jurisdictions with strict synchronous-baseload requirements, controlling for grid topology and demand profile.',
    'If storage/DER substitutes prove functionally equivalent, the reliability-primacy standard reclassifies from tangled_rope toward snare (coordination story as cover for incumbent rent extraction); if genuine necessity persists, the tangled_rope classification with real coordination function is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispatchability_engineering_necessity_vs_incumbency_shield, empirical, 'Whether dispatchability-as-codified still reflects engineering necessity or has drifted into incumbency protection.').

omega_variable(
    kernel_framing_selection_ambiguity,
    'Is reliability-primacy the objectively correct lens for climate-technology legitimacy, or is the choice among reliability, velocity, and precautionary framings itself a values-laden policy choice with no single defensible answer?',
    'There is no empirical resolution — the three readings encode different risk tolerances and time horizons (grid engineers favor reliability-primacy; climate scientists racing carbon budgets favor velocity-primacy; risk-averse publics and legacy-cost bearers favor precautionary framing). Resolution, if any, would come from an explicit multi-criteria policy synthesis rather than data alone.',
    'If reliability-primacy is treated as the sole legitimacy test rather than one input among three, technologies that would qualify under velocity or precautionary framings (e.g., fast-deployable solar-plus-short-duration-storage, or non-nuclear approaches with bounded failure modes) are excluded from legitimacy status they might otherwise deserve, and the standard''s real function shifts from technical necessity to gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_selection_ambiguity, conceptual, 'Whether reliability-primacy''s selection as THE legitimacy criterion, rather than one of three competing readings, is itself defensible or a contested framing choice.').

omega_variable(
    storage_cost_allocation_fairness,
    'Is it structurally fair to require renewable developers and their ratepayers to fund the full cost of firming/storage to meet a dispatchability standard, when incumbent baseload''s historical construction, decommissioning, and waste-management costs were themselves substantially externalized or subsidized?',
    'Comparative lifecycle cost accounting across generation technologies including historical subsidies, stranded-cost recovery, and waste/decommissioning liabilities, benchmarked against current storage-mandate cost allocations.',
    'If historical baseload costs were comparably externalized, the asymmetric cost burden on renewables under this reading looks less like neutral technical requirement and more like a double standard reinforcing the beneficiary/victim asymmetry already authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_cost_allocation_fairness, preference, 'Whether cost allocation under the dispatchability standard is fair relative to historical treatment of incumbent generation costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__reliability_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of technology_legitimacy_kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: reliability_primacy_reading (this file, tangled_rope — genuine grid-stability coordination with asymmetric cost transfer), velocity_primacy_reading (deployability within carbon budget as the legitimacy test), and precautionary_reading (bounded reversible risk as the legitimacy test). The three readings produce different beneficiary/victim sets from the same underlying policy debate over which technologies count as climate-legitimate, and are linked via affects_constraints rather than merged into one constraint with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
