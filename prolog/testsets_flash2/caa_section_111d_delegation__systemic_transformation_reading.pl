% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__systemic_transformation_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: Clean Air Act Section 111(d) Systemic Transformation Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'systemic transformation' reading of Clean
 *   Air Act Section 111(d), where the Environmental Protection Agency (EPA)
 *   is authorized to mandate grid-wide, generation-shifting strategies for
 *   states, including renewable substitution and early coal retirement. This
 *   interpretation allows the EPA to drive significant decarbonization across
 *   the power sector, leading to substantial extraction from fossil fuel
 *   industries and states dependent on them, while benefiting renewable
 *   energy sectors and environmental advocates. The constraint is actively
 *   enforced through federal regulations and state implementation plans,
 *   facing strong resistance and legal challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.7).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.8).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "Clean Air Act Section 111(d) Systemic Transformation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '94b384f2-ed02-460c-950c-8f9e4c3b1a1a').
narrative_ontology:cs_kernel_codification('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', fixed_text).
narrative_ontology:cs_authority_grounding('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', lineage).
narrative_ontology:cs_interpretation_layer_present('94b384f2-ed02-460c-950c-8f9e4c3b1a1a').
narrative_ontology:cs_reading_relation('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', foundational, broad_delegation_for_environmental_protection).
narrative_ontology:cs_axiom_status(broad_delegation_for_environmental_protection, holdable).
narrative_ontology:cs_axiom_grounding('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', broad_delegation_for_environmental_protection, deontological).
narrative_ontology:cs_axiom('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', foundational, grid_wide_solutions_are_best_system).
narrative_ontology:cs_axiom_status(grid_wide_solutions_are_best_system, holdable).
narrative_ontology:cs_axiom_grounding('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', grid_wide_solutions_are_best_system, empirically_contingent).
narrative_ontology:cs_reference_frame('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', active_federal_environmental_governance).
narrative_ontology:cs_drift_state('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', contemporary_judicial_review, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('94b384f2-ed02-460c-950c-8f9e4c3b1a1a', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, states_with_renewable_mandates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_fired_power_plants).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_dependent_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Section 111(d) to authorize broad decarbonization strategies, issuing regulations that mandate states to shift their energy generation mix. Faces legal challenges but asserts its statutory authority to address climate change.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_protection_agency, agenda_setter,
    institutional, generational, constrained, national).

% Forced to retire early or invest in costly carbon capture technologies, facing significant economic losses and stranded assets. Their business model is directly targeted by the regulation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_fired_power_plants, payer,
    powerful, biographical, trapped, regional).

% Required to develop and implement state-specific plans for grid-wide decarbonization, often involving significant economic and political disruption to their existing energy infrastructure and workforce. They resist these mandates through litigation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_dependent_states, payer,
    institutional, generational, constrained, national).

% Benefit from increased demand for renewable energy sources, subsidies, and regulatory certainty that favors their development. Their projects become key components of state compliance plans.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, national).

% Advocate for aggressive climate action and support the EPA's broad interpretation of its authority. They participate in rulemaking and litigation to defend and expand the scope of the regulation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups, beneficiary,
    organized, generational, analytical, global).

% Adjudicates legal challenges to the EPA's interpretation, particularly concerning the 'major questions doctrine' and the scope of delegated authority. Its rulings determine the ultimate enforceability and scope of this reading.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a national strategy to reduce carbon emissions from the power sector by enabling states to implement diverse, grid-wide decarbonization pathways, leveraging renewable energy and retiring high-emitting facilities.
% TRANSFER_FUNCTION: Transfers economic value from fossil fuel industries (especially coal) and fossil-fuel-dependent states to renewable energy developers and states with existing renewable infrastructure, through regulatory compliance costs and investment incentives.
% ABSENT_VOICES: Future generations, who would bear the full costs of climate change if this constraint were weakened, are not directly represented in the current legal and political debates, though environmental groups attempt to voice their interests.
% DISAPPEARANCE_RATIONALE: If this interpretation of Section 111(d) vanished, the EPA's ability to mandate systemic decarbonization would be severely curtailed. States would revert to less aggressive, facility-specific measures, slowing the transition to renewables and increasing carbon emissions, leading to a significant rearrangement of climate policy and energy markets.
% FOUNDING_PROBLEM: The Clean Air Act needed a mechanism to regulate greenhouse gas emissions from existing power plants, which were not explicitly covered by other sections, to address the growing threat of climate change.
% FOUNDING_PROBLEM_CORROBORATION: Scientific consensus on climate change and international climate agreements corroborate the ongoing urgency of the problem. Environmental organizations and a majority of states (through their climate action plans) also attest to the problem's live status. The fossil fuel industry and some states contest the severity and the need for federal intervention.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.7) because this reading enables the EPA to impose significant costs on the coal sector and fossil-fuel-dependent states, forcing them to abandon existing infrastructure. Suppression is also high (0.8) due to the federal mandate overriding state preferences and the legal battles required to challenge EPA authority. The theater ratio is low (0.1) as the EPA's actions are direct and functional, aimed at achieving specific environmental outcomes rather than performative maintenance. Accessibility collapse is moderate (0.6) as states have some flexibility in how they achieve compliance but cannot opt out of the overall decarbonization goal. Resistance is high (0.75) reflecting the intense political and legal opposition from affected industries and states.
 *
 * PERSPECTIVAL GAP:
 *   From the EPA's and environmental groups' perspective, this is a necessary and effective regulatory tool (a Rope or Scaffold) to address a critical environmental problem. From the perspective of coal power plants and fossil-fuel-dependent states, it is a highly extractive Snare, forcing them to bear disproportionate costs and threatening their economic viability. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The EPA, environmental advocacy groups, and renewable energy developers are beneficiaries, as the constraint aligns with their goals and creates market opportunities. Coal-fired power plants, fossil-fuel-dependent states, and coal mining communities are victims, bearing the direct costs of compliance and economic disruption. The Supreme Court acts as an observer, adjudicating the legal boundaries of this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the founding problem (regulating power plant emissions for climate change) is widely considered 'live' by its proponents. However, the contest over its scope (systemic transformation vs. facility-level) is central to its persistence and perceived legitimacy. The classification as a Tangled Rope reflects the genuine coordination function (decarbonization) intertwined with significant asymmetric extraction from specific sectors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    major_questions_doctrine_impact,
    'Will the Supreme Court''s application of the ''major questions doctrine'' ultimately limit the EPA''s authority under this systemic transformation reading?',
    'Future Supreme Court rulings on Section 111(d) or similar broad agency interpretations.',
    'If the Court applies the doctrine broadly, this reading''s authority will be severely curtailed, potentially reclassifying it closer to a Piton or even a Mountain (if the Court declares it beyond statutory authority). If the Court upholds broad delegation, the Tangled Rope classification will be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_impact, conceptual, 'Uncertainty regarding the legal limits of EPA''s delegated authority.').

omega_variable(
    economic_transition_feasibility,
    'Is the mandated systemic transformation economically and technologically feasible for all affected states within the regulatory timelines, or will it lead to unmanageable economic disruption?',
    'Empirical data on state-level energy transition costs, grid stability, and job displacement over time, compared against projected benefits.',
    'If the transition proves unfeasible or excessively disruptive, resistance will intensify, potentially leading to political reversal or legal challenges that weaken the constraint. If feasible, it strengthens the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_transition_feasibility, empirical, 'Feasibility and impact of decarbonization mandates on diverse state economies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(caa__tr_t5, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(caa__be_t5, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(caa__su_t5, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 15, 0.8).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of two primary readings of the Clean Air Act Section 111(d) delegation. This 'systemic_transformation_reading' asserts broad EPA authority for grid-wide decarbonization, while the 'facility_constraint_reading' (caa_section_111d_delegation__facility_constraint_reading) limits EPA's authority to measures implementable at individual facilities. Their differing interpretations lead to distinct beneficiary/victim structures and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
