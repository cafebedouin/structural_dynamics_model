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
 *   human_readable: Technology Legitimacy: Reliability Primacy Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the legitimacy of climate mitigation technologies
 *   based on their ability to provide dispatchable, baseload-capable
 *   generation, ensuring grid stability. It is one reading of the broader
 *   'technology_legitimacy_kernel' which is contested across different
 *   priorities. This reading prioritizes the operational stability of the
 *   electricity grid, often favoring established baseload technologies like
 *   nuclear and traditional fossil fuels (with or without CCS) and imposing
 *   significant costs or exclusions on intermittent renewables unless they
 *   are paired with expensive storage solutions. The constraint is claimed as
 *   a 'rope' by its proponents, framing it as essential coordination for grid
 *   reliability, but its operation is substantially extractive, leading to a
 *   'tangled_rope' classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.7).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.75).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Technology Legitimacy: Reliability Primacy Reading").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '954e92b1-4379-497f-a2f3-e67996ce6471').
narrative_ontology:cs_kernel_codification('954e92b1-4379-497f-a2f3-e67996ce6471', formalized).
narrative_ontology:cs_authority_grounding('954e92b1-4379-497f-a2f3-e67996ce6471', expertise).
narrative_ontology:cs_interpretation_layer_present('954e92b1-4379-497f-a2f3-e67996ce6471').
narrative_ontology:cs_reading_relation('954e92b1-4379-497f-a2f3-e67996ce6471', technology_legitimacy_kernel__precautionary_reading, forecloses).
narrative_ontology:cs_reading_relation('954e92b1-4379-497f-a2f3-e67996ce6471', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('954e92b1-4379-497f-a2f3-e67996ce6471', foundational, grid_stability_is_paramount).
narrative_ontology:cs_axiom_status(grid_stability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('954e92b1-4379-497f-a2f3-e67996ce6471', grid_stability_is_paramount, deontological).
narrative_ontology:cs_axiom('954e92b1-4379-497f-a2f3-e67996ce6471', foundational, dispatchability_is_non_negotiable).
narrative_ontology:cs_axiom_status(dispatchability_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('954e92b1-4379-497f-a2f3-e67996ce6471', dispatchability_is_non_negotiable, conventional).
narrative_ontology:cs_reference_frame('954e92b1-4379-497f-a2f3-e67996ce6471', traditional_grid_stability_paradigm).
narrative_ontology:cs_drift_state('954e92b1-4379-497f-a2f3-e67996ce6471', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('954e92b1-4379-497f-a2f3-e67996ce6471', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, traditional_baseload_generators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, industrial_consumers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, climate_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining grid stability and reliability, they prioritize technologies that offer dispatchable, baseload power. They enforce this through grid codes, interconnection standards, and market rules. Their operational mandate aligns directly with this reading.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, agenda_setter,
    institutional, biographical, constrained, national).

% Benefits from this reading as nuclear power inherently provides dispatchable, baseload generation. This criterion legitimizes their technology and directs policy support and investment towards them, despite other concerns.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry, beneficiary,
    organized, generational, mobile, global).

% Includes coal, natural gas, and large hydro operators. They benefit from this reading as it reinforces the value of their existing assets and operational models, even as climate goals push for decarbonization. They advocate for this criterion in policy debates.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, traditional_baseload_generators, beneficiary,
    organized, biographical, mobile, national).

% Develop solar and wind projects. They bear the cost of this constraint by needing to integrate expensive storage solutions or face exclusion from 'legitimate' climate mitigation portfolios. Their projects are often delayed or rejected based on these criteria.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers, payer,
    moderate, immediate, constrained, regional).

% Indirectly bear the costs of prioritizing baseload-capable generation, either through higher electricity prices (if baseload is more expensive than available intermittent options) or through the cost of mandated storage for renewables. They have little direct influence on the criteria.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    powerless, immediate, trapped, local).

% Prioritize rapid decarbonization and often advocate for the fastest, cheapest deployment of low-carbon technologies, including intermittent renewables. This reading's emphasis on baseload can slow down overall decarbonization efforts, which they view as a critical failure. They are often excluded from the technical discussions that set these legitimacy criteria.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_advocates, excluded,
    organized, generational, constrained, global).

% Require highly stable and reliable power for their operations. They benefit from policies that prioritize grid stability and dispatchability, as it ensures their energy supply is consistent and predictable, even if it comes at a higher cost.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, industrial_consumers, beneficiary,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates energy policy and investment around a shared understanding of grid stability, ensuring that new generation capacity contributes to a reliable and dispatchable electricity supply.
% TRANSFER_FUNCTION: Transfers legitimacy, policy support, and investment from intermittent renewable technologies (unless paired with costly storage) to dispatchable, baseload-capable technologies, primarily from ratepayers and climate advocates to traditional energy industries and grid operators.
% ABSENT_VOICES: Advocates for rapid, cost-effective decarbonization and developers of intermittent renewables without storage are often marginalized in policy discussions that prioritize this definition of reliability. They would argue for a broader definition of grid stability that includes demand-side management and flexible loads, or for prioritizing speed of deployment.
% DISAPPEARANCE_RATIONALE: If this criterion vanished, energy policy would immediately shift to prioritize other factors (e.g., speed of deployment, lowest cost, lowest environmental footprint). Investment would flow more freely to intermittent renewables, and grid operators would be forced to innovate new ways to manage stability without relying solely on baseload generation. The energy landscape would rapidly reconfigure.
% FOUNDING_PROBLEM: The historical challenge of maintaining a stable and reliable electricity grid, where supply must constantly match demand, and large-scale outages are economically and socially catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and traditional energy experts universally attest that grid stability remains a live and critical problem, especially with increasing penetration of variable renewable energy. Independent academic studies on grid resilience and blackouts corroborate the ongoing nature of this challenge, though they may contest the specific solutions or criteria for legitimacy.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.7) is high because it imposes significant costs on technologies that do not inherently provide baseload power, forcing them to add expensive storage or be deemed 'illegitimate' for certain policy supports. Suppression (0.75) is also high, as it actively limits the market access and policy acceptance of alternatives. The theater ratio (0.15) is low because the concern for grid stability is genuine, not merely performative. The increasing trend in extractiveness and suppression reflects growing grid integration challenges for intermittent renewables, leading to a hardening of these criteria over time. Resistance is moderate (0.55) from advocates of other technologies and policy priorities.
 *
 * PERSPECTIVAL GAP:
 *   Grid operators and traditional energy industries (agenda-setters and beneficiaries) perceive this as a necessary 'rope' for coordinating a reliable energy system. Intermittent renewable developers and climate advocates (payers and excluded) experience it as a 'snare' or 'tangled_rope' that extracts resources and suppresses innovation by imposing an overly narrow definition of legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid operators, nuclear, and traditional baseload generators are clear beneficiaries (low d) as their technologies are favored. Intermittent renewable developers, ratepayers, and climate advocates are targets (high d) as they bear the costs of compliance or exclusion. Industrial consumers are beneficiaries due to their need for stable power. The constraint actively enforces its criteria, leading to high suppression for non-conforming technologies.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a universally accepted technical requirement for grid stability, or one contested reading of the ''technology_legitimacy_kernel''?',
    'Analysis of policy debates and expert testimony from diverse fields (e.g., grid engineering, climate science, economics) to identify the extent of disagreement on the necessity and scope of this criterion.',
    'If it is a universally accepted technical requirement, its extractiveness might be re-evaluated as a necessary cost of coordination. If it is a contested reading, its classification as a ''tangled_rope'' (or even ''snare'') is reinforced, highlighting the political nature of its enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Distinguishing a technical necessity from a policy choice within a contested kernel.').

omega_variable(
    cost_benefit_of_reliability_criteria,
    'Are the costs imposed by this reliability-primacy criterion (e.g., higher prices, slower decarbonization) justified by the actual, measurable benefits to grid stability and resilience?',
    'Comprehensive economic modeling and empirical studies comparing grid performance and costs in systems with varying degrees of baseload/dispatchability requirements, especially those integrating high levels of intermittent renewables with advanced grid management.',
    'If costs outweigh benefits, the constraint''s extractiveness is confirmed as excessive, strengthening its ''snare'' characteristics. If benefits are clearly superior, the coordination function is reinforced, potentially shifting it closer to a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_of_reliability_criteria, empirical, 'Evaluating the economic and climate trade-offs of prioritizing baseload reliability.').

omega_variable(
    technological_evolution_impact,
    'Could advancements in grid technology (e.g., smart grids, advanced storage, demand-side management) render the ''baseload-capable'' requirement less critical or obsolete for ensuring grid stability?',
    'Ongoing technological forecasting, pilot project evaluations, and expert consensus on the future capabilities of grid management and energy storage solutions.',
    'If future technologies can provide equivalent stability without traditional baseload, the constraint''s ''founding_problem_status'' would shift from ''live'' to ''dead'', indicating mandatrophy and a reclassification towards ''piton'' or ''snare'' if it persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_evolution_impact, empirical, 'Assessing the long-term relevance of baseload requirements in a changing technological landscape.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tech_tr_t6, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(tech_tr_t18, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(tech_tr_t30, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tech_be_t6, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(tech_be_t18, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(tech_be_t30, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tech_su_t6, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(tech_su_t18, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(tech_su_t30, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
