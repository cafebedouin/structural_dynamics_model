% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence Boundary: Infrastructure Reading
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the emergence of digital money through the lens
 *   of interbank electronic transfer infrastructure (ATMs, ACH, SWIFT). It
 *   posits that digital money became a reality when banks could move it
 *   electronically, even if consumers couldn't directly hold it in digital
 *   form. This is one reading of a contested kernel,
 *   'digital_money_emergence_boundary', which also includes
 *   conceptualization-focused and consumer-holding-focused readings. This
 *   reading places the emergence in the mid-to-late 20th century, marking a
 *   'middle boundary' in the historical timeline.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.15).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.05).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary: Infrastructure Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'ef9b384d-bf8b-4f99-991a-5fa521e27af9').
narrative_ontology:cs_kernel_codification('ef9b384d-bf8b-4f99-991a-5fa521e27af9', implicit).
narrative_ontology:cs_authority_grounding('ef9b384d-bf8b-4f99-991a-5fa521e27af9', expertise).
narrative_ontology:cs_reading_relation('ef9b384d-bf8b-4f99-991a-5fa521e27af9', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef9b384d-bf8b-4f99-991a-5fa521e27af9', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('ef9b384d-bf8b-4f99-991a-5fa521e27af9', foundational, functional_transfer_precedes_conceptualization).
narrative_ontology:cs_axiom_status(functional_transfer_precedes_conceptualization, holdable).
narrative_ontology:cs_axiom_grounding('ef9b384d-bf8b-4f99-991a-5fa521e27af9', functional_transfer_precedes_conceptualization, conventional).
narrative_ontology:cs_axiom('ef9b384d-bf8b-4f99-991a-5fa521e27af9', foundational, interbank_capacity_defines_digitality).
narrative_ontology:cs_axiom_status(interbank_capacity_defines_digitality, holdable).
narrative_ontology:cs_axiom_grounding('ef9b384d-bf8b-4f99-991a-5fa521e27af9', interbank_capacity_defines_digitality, empirically_contingent).
narrative_ontology:cs_reference_frame('ef9b384d-bf8b-4f99-991a-5fa521e27af9', traditional_monetary_aggregates).
narrative_ontology:cs_drift_state('ef9b384d-bf8b-4f99-991a-5fa521e27af9', contemporary_crypto_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ef9b384d-bf8b-4f99-991a-5fa521e27af9', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entities like SWIFT and ACH operators that build and maintain the electronic rails for interbank transfers. They benefit from the definition of digital money being tied to their operational capacity, as it validates their central role in the financial system.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, beneficiary,
    institutional, generational, constrained, global).

% Define and regulate what constitutes money within their jurisdictions. This reading provides a clear historical marker for the expansion of their oversight to electronic forms of money, even if not directly held by consumers.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, analytical, global).

% Analyze the evolution of monetary systems. This reading offers a specific, infrastructure-driven point of transition for digital money, influencing their periodization and causal narratives.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, financial_historians, observer,
    analytical, civilizational, analytical, universal).

% Develop conceptual frameworks for understanding money. This reading challenges purely conceptual definitions by grounding the emergence of digital money in concrete technological capabilities.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

% While they eventually benefit from digital money, this reading of its emergence does not center their direct interaction or holding of digital instruments, placing them outside the definitional boundary.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumers, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared historical and conceptual boundary for when money became 'digital' in a functionally meaningful sense, allowing for coordinated regulatory and theoretical responses to electronic finance.
% TRANSFER_FUNCTION: Defines the point at which the 'transfer' of money shifted from physical instruments to electronic signals between institutions, blurring the lines of traditional monetary aggregates (M4/M5).
% ABSENT_VOICES: Consumers and early digital currency advocates, who would argue that 'digital money' only truly emerged when it became directly accessible and usable by individuals, not just banks. Their perspective is excluded from this infrastructure-centric definition.
% DISAPPEARANCE_RATIONALE: This constraint describes a historical event and a conceptual boundary. Its 'disappearance' would not alter the past; rather, it would remove a specific interpretive framework for understanding the past, leading to a different historical narrative rather than a change in the underlying reality.
% FOUNDING_PROBLEM: To define the historical moment when money transitioned from purely physical or paper forms to electronic forms, enabling new forms of financial intermediation and requiring new regulatory approaches.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians and central bank economists corroborate the need for such a boundary to understand the evolution of financial systems and the scope of monetary policy. The ongoing debate over the definition of 'digital money' (e.g., CBDCs vs. stablecoins) confirms the problem's continued relevance, attested by academic literature and policy papers from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_emergence_boundary__infrastructure_reading),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it describes a historical boundary defined by technological capability, which is an irreducible physical/logical limit. Its extractiveness is low (0.15) as it primarily describes a historical fact, not an ongoing extractive mechanism. However, it declares beneficiaries (banking infrastructure providers) because this definition validates their historical and ongoing role, triggering FSM evaluation. Suppression and theater are negligible as it's a descriptive historical claim, not an actively enforced or performative constraint.
 *
 * PERSPECTIVAL GAP:
 *   While this reading is presented as a historical fact, other readings of the 'digital_money_emergence_boundary' kernel would shift the perceived emergence point, leading to different beneficiaries and victims depending on whether the focus is on theoretical conceptualization or consumer access. This highlights how even 'natural' historical boundaries can be contested and have 'beneficiaries' of a particular framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure providers are beneficiaries (d=0.0) as this reading validates their historical importance and the necessity of their systems. Central banks and financial historians are observers/agenda-setters (d=0.5) as they interpret and utilize this boundary for policy and analysis. Consumers are excluded (d=1.0) from this specific definitional boundary, as their direct interaction with digital money is not the focus of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_boundary,
    'Is the ''digital money emergence boundary'' a genuine natural law (a technological inevitability) or a constructed conceptual boundary that benefits identifiable agents?',
    'Analysis of counterfactual technological paths: could digital money have emerged without these specific infrastructure developments, or through different conceptualizations/consumer adoptions first? If so, the boundary is more constructed.',
    'If more constructed, the ''mountain'' classification is a false summit, and the constraint would reclassify as a Tangled Rope, reflecting the benefit to banking infrastructure providers from this particular historical framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_boundary, conceptual, 'Ambiguity between a natural technological boundary and a constructed historical interpretation.').

omega_variable(
    definitional_scope_ambiguity,
    'Does ''digital money'' refer to any electronic representation of value, or specifically to electronic representations of fiat currency within the traditional banking system?',
    'Analysis of how central banks and international bodies (e.g., BIS) define ''digital money'' in policy documents and regulatory frameworks. Divergence would indicate a contested scope.',
    'If the definition is broader (any electronic value), this reading''s focus on traditional banking infrastructure becomes too narrow, potentially shifting the emergence boundary and the identified beneficiaries. If narrower, this reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_scope_ambiguity, conceptual, 'Ambiguity in the scope of ''digital money'' itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 1977).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.0).
narrative_ontology:measurement(digi_tr_t1972, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1972, 0.0).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.0).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.1).
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.12).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.05).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.05).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_emergence_boundary' kernel. This 'infrastructure_reading' focuses on the development of interbank electronic transfer systems as the key emergence point, distinct from conceptual or consumer-holding perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
