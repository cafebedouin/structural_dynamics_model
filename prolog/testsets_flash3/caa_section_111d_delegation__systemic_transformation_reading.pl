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
 *   human_readable: CAA Section 111(d) 'Best System' Delegation: Systemic Transformation Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'systemic transformation' reading of Clean
 *   Air Act Section 111(d), which interprets the EPA's authority to regulate
 *   existing power plants as extending to grid-wide, generation-shifting
 *   strategies, including mandating renewable energy substitution and early
 *   coal retirement. This reading is highly contested, particularly by the
 *   fossil fuel industry and states reliant on it, who advocate for a more
 *   limited 'facility-specific' interpretation. The claimed type is
 *   'tangled_rope' because it genuinely coordinates climate action while
 *   imposing significant, asymmetric extraction on the fossil fuel sector.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.7).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.65).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA Section 111(d) 'Best System' Delegation: Systemic Transformation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, 'fb0a0694-85a8-4592-9a49-29442266786f').
narrative_ontology:cs_kernel_codification('fb0a0694-85a8-4592-9a49-29442266786f', fixed_text).
narrative_ontology:cs_authority_grounding('fb0a0694-85a8-4592-9a49-29442266786f', lineage).
narrative_ontology:cs_interpretation_layer_present('fb0a0694-85a8-4592-9a49-29442266786f').
narrative_ontology:cs_reading_relation('fb0a0694-85a8-4592-9a49-29442266786f', caa_section_111d_delegation__facility_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('fb0a0694-85a8-4592-9a49-29442266786f', foundational, best_system_includes_generation_shifting).
narrative_ontology:cs_axiom_status(best_system_includes_generation_shifting, holdable).
narrative_ontology:cs_axiom_grounding('fb0a0694-85a8-4592-9a49-29442266786f', best_system_includes_generation_shifting, conventional).
narrative_ontology:cs_axiom('fb0a0694-85a8-4592-9a49-29442266786f', foundational, epa_has_broad_delegated_authority_for_climate).
narrative_ontology:cs_axiom_status(epa_has_broad_delegated_authority_for_climate, holdable).
narrative_ontology:cs_axiom_grounding('fb0a0694-85a8-4592-9a49-29442266786f', epa_has_broad_delegated_authority_for_climate, conventional).
narrative_ontology:cs_reference_frame('fb0a0694-85a8-4592-9a49-29442266786f', broad_agency_discretion_framework).
narrative_ontology:cs_drift_state('fb0a0694-85a8-4592-9a49-29442266786f', contemporary_judicial_skepticism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fb0a0694-85a8-4592-9a49-29442266786f', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_protection_agency).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, states_with_renewable_targets).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_fired_power_plants).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, states_reliant_on_fossil_fuels).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Section 111(d) to authorize broad regulatory power to mandate state-level decarbonization pathways, including generation-shifting. Benefits from expanded authority to address climate change. Faces legal challenges from regulated industries and states.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_protection_agency, agenda_setter,
    institutional, generational, constrained, national).

% Face early retirement or significant retrofitting costs due to mandated generation-shifting. Their business model is directly targeted by this interpretation, leading to high compliance costs and potential closure. Exit options are limited by sunk costs and regulatory pressure.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_fired_power_plants, payer,
    powerful, immediate, trapped, national).

% Bears the economic costs of reduced demand for coal and other fossil fuels, as well as the political costs of defending their business model against environmental regulation. Seeks to limit EPA's authority through litigation and lobbying.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_industry, payer,
    institutional, biographical, constrained, national).

% Benefits from regulatory mandates that incentivize the transition to renewable energy sources. Receives subsidies and market advantages through compliance pathways, leading to increased investment and growth.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sector, beneficiary,
    organized, generational, mobile, national).

% Face significant economic and political challenges in transitioning away from fossil fuel-based economies. Bear the costs of grid transformation, potential job losses, and stranded assets. Their options are to comply, litigate, or seek federal relief.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, states_reliant_on_fossil_fuels, payer,
    institutional, generational, constrained, regional).

% Benefit from federal mandates that align with their existing climate goals, potentially accelerating their transition to renewable energy and attracting green investment. Their policy objectives are reinforced by this reading.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, states_with_renewable_targets, beneficiary,
    institutional, generational, mobile, regional).

% Acts as the ultimate arbiter of the legality of EPA's interpretation, weighing statutory text, legislative history, and constitutional principles of delegation. Its rulings determine the scope and persistence of this constraint.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a national strategy to reduce carbon emissions from the power sector by enabling states to implement grid-wide generation-shifting plans, thereby addressing a collective action problem of climate change mitigation.
% TRANSFER_FUNCTION: Transfers economic value from fossil fuel-based power generation (through early retirement or reduced operation) to the renewable energy sector (through increased demand and investment), driven by regulatory compliance costs.
% ABSENT_VOICES: Local communities heavily reliant on coal mining or coal-fired power plants, whose economic survival is directly threatened by decarbonization mandates, often feel their voices are not adequately represented in federal regulatory processes or judicial review.
% DISAPPEARANCE_RATIONALE: If this interpretation of Section 111(d) vanished, EPA's authority to mandate systemic decarbonization would be severely curtailed. States would lose a federal driver for renewable energy transition, leading to a slower, more fragmented, and less ambitious climate mitigation effort across the power sector. Investment in renewables would slow, and coal plants would face less pressure to retire.
% FOUNDING_PROBLEM: The Clean Air Act was designed to address air pollution, and Section 111(d) specifically aimed to regulate emissions from existing sources, including those contributing to climate change, which was an emerging concern at the time of its amendment.
% FOUNDING_PROBLEM_CORROBORATION: Environmental scientists and international climate bodies corroborate that the problem of greenhouse gas emissions from existing power sources remains a live and urgent concern. The EPA and environmental advocacy groups also attest to the problem's persistence, while the fossil fuel industry contests the severity and the proposed solutions.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.7) is high because this reading enables the EPA to impose substantial costs on fossil fuel generators, potentially forcing them out of business. Suppression (0.65) is also high, reflecting the regulatory power to compel compliance and limit alternatives for states and industries. Resistance (0.8) is very high, as this interpretation faces intense legal and political opposition. The theater ratio is low (0.1) because the EPA's actions under this reading are direct and intended to achieve substantive environmental outcomes, not merely performative. The metrics reflect the contested and impactful nature of this broad interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The EPA and environmental advocates view this reading as a necessary and effective coordination mechanism for climate action, with extraction being a justified cost of pollution. Conversely, the fossil fuel industry and allied states perceive it as an overreach of federal power, an unconstitutional taking, and pure extraction. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The EPA, renewable energy sector, and states with renewable targets are beneficiaries, as this reading expands EPA's power, creates market opportunities for renewables, and supports state climate goals. Coal-fired power plants, the fossil fuel industry, and states reliant on fossil fuels are victims, bearing the costs of compliance, early retirement, and economic transition. The Supreme Court acts as an observer, adjudicating the legality of this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_review_outcome,
    'Will the Supreme Court uphold or strike down the EPA''s broad interpretation of Section 111(d)?',
    'A definitive Supreme Court ruling on the scope of EPA''s authority under Section 111(d).',
    'If upheld, this reading''s classification as a Tangled Rope would be reinforced, with high extractiveness and suppression. If struck down, the constraint would likely revert to a more limited scope, reducing its extractiveness and potentially reclassifying it as a Rope or even a Piton if its original coordination function is deemed to have atrophied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_review_outcome, empirical, 'Uncertainty regarding the ultimate legal validity of EPA''s broad interpretation.').

omega_variable(
    economic_feasibility_of_transition,
    'Is the mandated systemic transformation economically feasible for fossil-reliant states and industries without causing undue economic disruption?',
    'Independent economic impact assessments and real-world outcomes of state-level decarbonization efforts.',
    'If the transition proves economically prohibitive, the ''extraction'' component of the Tangled Rope classification would be amplified, potentially pushing it towards a Snare. If highly feasible, the coordination aspect would be strengthened, potentially moving it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_feasibility_of_transition, empirical, 'Economic viability of the energy transition under this regulatory framework.').

omega_variable(
    delegation_doctrine_scope,
    'Does this interpretation of Section 111(d) violate the nondelegation doctrine by granting EPA excessive legislative power?',
    'Supreme Court clarification or re-evaluation of the nondelegation doctrine''s application to environmental statutes.',
    'A ruling that this interpretation violates the nondelegation doctrine would fundamentally undermine the constraint, effectively dissolving it as a legitimate exercise of administrative power and reclassifying it as a Snare (unlawful extraction) or even a Mountain (if the court asserts a fixed constitutional limit on delegation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delegation_doctrine_scope, conceptual, 'Constitutional limits on Congress''s ability to delegate legislative power to administrative agencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t1970, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(caa__tr_t1990, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(caa__tr_t2005, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(caa__tr_t2015, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(caa__tr_t2024, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(caa__be_t1970, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(caa__be_t1990, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(caa__be_t2005, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(caa__be_t2015, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(caa__be_t2024, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t1970, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(caa__su_t1990, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(caa__su_t2005, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(caa__su_t2015, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(caa__su_t2024, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'systemic transformation' reading of CAA Section 111(d) delegation, contrasting with the 'facility constraint' reading. Both are distinct constraints arising from the same statutory kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
