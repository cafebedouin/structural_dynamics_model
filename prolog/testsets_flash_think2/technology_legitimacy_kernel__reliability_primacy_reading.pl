% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   human_readable: Reliability Primacy Reading of Climate Tech Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint defines a technology as legitimate for climate mitigation
 *   if it provides dispatchable, baseload-capable generation, prioritizing
 *   grid stability. It is one reading of the broader
 *   'technology_legitimacy_kernel' which is contested by other framings like
 *   'velocity primacy' and 'precautionary' approaches. This reading
 *   structurally favors established dispatchable technologies (e.g., nuclear,
 *   fossil with CCS) and imposes significant burdens on intermittent
 *   renewables, shaping policy and investment flows in the energy transition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.65).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.75).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability Primacy Reading of Climate Tech Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '4707ffeb-05dd-4e58-86c6-336179fdce34').
narrative_ontology:cs_kernel_codification('4707ffeb-05dd-4e58-86c6-336179fdce34', implicit).
narrative_ontology:cs_authority_grounding('4707ffeb-05dd-4e58-86c6-336179fdce34', expertise).
narrative_ontology:cs_interpretation_layer_present('4707ffeb-05dd-4e58-86c6-336179fdce34').
narrative_ontology:cs_reading_relation('4707ffeb-05dd-4e58-86c6-336179fdce34', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4707ffeb-05dd-4e58-86c6-336179fdce34', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('4707ffeb-05dd-4e58-86c6-336179fdce34', foundational, grid_stability_is_paramount).
narrative_ontology:cs_axiom_status(grid_stability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('4707ffeb-05dd-4e58-86c6-336179fdce34', grid_stability_is_paramount, deontological).
narrative_ontology:cs_axiom('4707ffeb-05dd-4e58-86c6-336179fdce34', secondary, dispatchability_is_key_to_stability).
narrative_ontology:cs_axiom_status(dispatchability_is_key_to_stability, holdable).
narrative_ontology:cs_axiom_grounding('4707ffeb-05dd-4e58-86c6-336179fdce34', dispatchability_is_key_to_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('4707ffeb-05dd-4e58-86c6-336179fdce34', traditional_grid_stability_paradigm).
narrative_ontology:cs_drift_state('4707ffeb-05dd-4e58-86c6-336179fdce34', contemporary_climate_crisis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4707ffeb-05dd-4e58-86c6-336179fdce34', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, fossil_fuel_with_ccs_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining grid stability and reliability. They advocate for technologies that provide dispatchable, baseload power and influence policy to prioritize these attributes.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from this legitimacy criterion as nuclear power is inherently dispatchable and baseload-capable, making it a preferred technology for climate mitigation under this framing.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry, beneficiary,
    powerful, generational, mobile, global).

% Benefits as fossil fuel plants with carbon capture and storage (CCS) can also provide dispatchable, baseload power, extending the lifespan of existing infrastructure under a climate mitigation mandate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, fossil_fuel_with_ccs_industry, beneficiary,
    powerful, generational, mobile, global).

% Bear the costs of this criterion, as their technologies (solar, wind) are not dispatchable or baseload-capable without significant, costly energy storage solutions, which are often not fully subsidized or incentivized.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers, payer,
    organized, biographical, constrained, global).

% Indirectly bear the costs of prioritizing dispatchable generation, either through higher electricity prices to fund storage for renewables or to support more expensive baseload technologies.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    powerless, immediate, trapped, national).

% Translate this legitimacy criterion into energy policy, funding decisions, and regulatory frameworks, shaping the market for climate mitigation technologies.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Advocate for rapid deployment of climate technologies to meet carbon budget timelines, often prioritizing speed and cost over strict dispatchability. They are excluded from this specific framing of legitimacy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_activists_velocity_primacy, excluded,
    organized, generational, constrained, global).

% Advocate for technologies with bounded and reversible worst-case failure modes and legacy costs. They are excluded from this specific framing of legitimacy, which might accept higher risks for stability.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_activists_precautionary, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure a stable, reliable, and secure electricity grid during the complex energy transition, preventing blackouts and maintaining energy security.
% TRANSFER_FUNCTION: Transfers legitimacy, policy support, and investment priority to dispatchable, baseload-capable generation technologies (e.g., nuclear, fossil with CCS) while imposing additional costs or disqualification on intermittent renewable technologies (e.g., solar, wind) unless paired with costly storage.
% ABSENT_VOICES: Advocates for 'velocity primacy' (prioritizing rapid deployment) and 'precautionary' (prioritizing bounded risks) readings of climate tech legitimacy are structurally excluded from this specific framing. They would argue that an overemphasis on dispatchability hinders the speed of decarbonization or accepts unacceptable risks.
% DISAPPEARANCE_RATIONALE: If this legitimacy criterion vanished overnight, policy and investment would rapidly shift towards other priorities (e.g., lowest cost, fastest deployment, lowest risk), fundamentally altering the energy technology landscape, investment flows, and potentially grid architecture. Technologies currently disfavored would gain prominence.
% FOUNDING_PROBLEM: The historical challenge of maintaining grid stability and energy security while integrating variable renewable energy sources and transitioning away from traditional fossil fuel baseload generation.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators, national security agencies, and some energy economists (outside the direct beneficiaries like the nuclear industry) corroborate the ongoing challenge of grid stability and the need for dispatchable power, citing historical blackouts and energy supply crises.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial because this criterion imposes significant costs on intermittent renewables (requiring expensive storage) and effectively transfers policy support and funding towards dispatchable technologies. Suppression (0.75) is high as it actively constrains the deployment and funding of alternatives that do not meet the dispatchability standard. The theater ratio (0.20) is low because the concern for grid stability is a genuine, functional problem, even if the proposed solution (prioritizing baseload) is contested. The claimed type is 'tangled_rope' because it serves a genuine coordination function (grid stability) but also involves asymmetric extraction from certain technology developers and ratepayers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of grid operators and beneficiaries, this criterion is essential for a stable energy transition. From the perspective of intermittent renewable developers and advocates of other legitimacy framings, it is an extractive barrier that slows decarbonization or locks in higher-risk technologies. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid operators, the nuclear industry, and the fossil fuel with CCS industry are beneficiaries, as their technologies are favored and receive policy support. Intermittent renewable developers and ratepayers are victims, bearing the costs of either adding storage or supporting more expensive baseload options. Policy makers act as agenda setters, translating this criterion into actionable policy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_of_reliability_vs_alternatives,
    'Is the cost imposed on intermittent renewables (for storage or grid integration) truly necessary for grid stability, or is it inflated by favoring incumbent dispatchable technologies and a conservative grid management paradigm?',
    'Independent economic analysis comparing the system-level costs of different grid architectures (e.g., high baseload vs. high renewables with advanced grid management and storage), and pilot projects demonstrating new grid flexibility solutions.',
    'If costs are inflated, the constraint''s effective extractiveness is higher than currently assessed, and its coordination function is partly a cover for rent-seeking by incumbent technologies. If costs are genuinely high, the extractiveness is a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_reliability_vs_alternatives, empirical, 'Whether the costs attributed to ensuring reliability for intermittent sources are objectively necessary or strategically amplified.').

omega_variable(
    definition_of_dispatchable_flexibility,
    'Is the definition of ''dispatchable'' and ''baseload-capable'' too narrow, failing to account for emerging flexible grid solutions, demand-side management, or hybrid renewable-storage systems?',
    'Technological advancements and successful large-scale deployments of non-traditional grid balancing solutions that demonstrate equivalent stability without traditional baseload generation.',
    'If the definition is too narrow, the constraint''s suppression of alternatives is higher than necessary, and its claimed coordination function could be achieved with a more diverse technology mix. This would shift the classification towards a ''snare'' for intermittent renewables.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_dispatchable_flexibility, conceptual, 'Whether the technical criteria for grid stability are evolving faster than the policy definitions.').

omega_variable(
    legitimacy_framing_contest,
    'Is the primacy of reliability a genuine, universally accepted technical necessity for climate mitigation, or a strategic framing by incumbent industries and conservative actors to slow the transition to renewables and maintain existing infrastructure?',
    'Analysis of lobbying efforts, funding flows, and public discourse from various industry and advocacy groups, alongside expert consensus on the technical feasibility and trade-offs of different grid futures.',
    'If primarily a strategic framing, the constraint''s ''tangled_rope'' classification leans more heavily towards extraction, and its coordination function is more theatrical. If a genuine necessity, the coordination aspect is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_framing_contest, preference, 'Whether the emphasis on reliability is a technical imperative or a political preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
