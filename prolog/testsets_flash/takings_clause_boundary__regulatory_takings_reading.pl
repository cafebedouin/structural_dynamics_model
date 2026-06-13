% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central Reading)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   The 'regulatory takings' doctrine, primarily articulated in Penn Central
 *   Transportation Co. v. City of New York (1978), holds that government
 *   regulations that diminish property value 'too far' can constitute a
 *   taking requiring compensation, even without physical appropriation. This
 *   expands the scope of the Fifth Amendment's Takings Clause beyond direct
 *   physical seizures. It introduces an ad hoc, fact-specific balancing test,
 *   creating a complex and often unpredictable boundary for land-use and
 *   environmental regulations. This constraint is a reading of the broader
 *   'takings_clause_boundary' kernel, focusing on economic impact rather than
 *   physical invasion.
 *
 * KEY AGENTS:
 *   - property_owners: Primary beneficiaries (powerful/constrained) — protected from severe value diminution.
 *   - developers: Secondary beneficiaries (powerful/constrained) — benefit from limits on regulatory burdens.
 *   - local_governments: Primary targets (institutional/constrained) — face potential compensation claims for regulations.
 *   - environmental_regulators: Targets (institutional/constrained) — constrained in implementing protective regulations.
 *   - public_interest_advocates: Victims (organized/constrained) — see public welfare regulations chilled by takings claims.
 *   - courts: Agenda setters (institutional/analytical) — adjudicate takings claims, defining the 'too far' boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.65).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.45).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Penn Central Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '27aeea25-f0f1-44b6-bf66-bf152f8797e0').
narrative_ontology:cs_kernel_codification('27aeea25-f0f1-44b6-bf66-bf152f8797e0', formalized).
narrative_ontology:cs_authority_grounding('27aeea25-f0f1-44b6-bf66-bf152f8797e0', lineage).
narrative_ontology:cs_interpretation_layer_present('27aeea25-f0f1-44b6-bf66-bf152f8797e0').
narrative_ontology:cs_reading_relation('27aeea25-f0f1-44b6-bf66-bf152f8797e0', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('27aeea25-f0f1-44b6-bf66-bf152f8797e0', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('27aeea25-f0f1-44b6-bf66-bf152f8797e0', foundational, economic_value_is_property).
narrative_ontology:cs_axiom_status(economic_value_is_property, holdable).
narrative_ontology:cs_axiom_grounding('27aeea25-f0f1-44b6-bf66-bf152f8797e0', economic_value_is_property, deontological).
narrative_ontology:cs_axiom('27aeea25-f0f1-44b6-bf66-bf152f8797e0', foundational, balancing_test_for_regulatory_impact).
narrative_ontology:cs_axiom_status(balancing_test_for_regulatory_impact, holdable).
narrative_ontology:cs_axiom_grounding('27aeea25-f0f1-44b6-bf66-bf152f8797e0', balancing_test_for_regulatory_impact, conventional).
narrative_ontology:cs_reference_frame('27aeea25-f0f1-44b6-bf66-bf152f8797e0', penn_central_balancing_framework).
narrative_ontology:cs_drift_state('27aeea25-f0f1-44b6-bf66-bf152f8797e0', contemporary_judicial_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('27aeea25-f0f1-44b6-bf66-bf152f8797e0', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, developers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, local_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, environmental_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, public_interest_advocates).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, economic_liberty_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, private_property_rights).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the balance between private property rights and public welfare regulations, but does so with significant asymmetric extraction. Property owners and developers benefit from the protection against value diminution, while local governments and regulators bear the costs of potential compensation or chilled regulation. The 'too far' standard introduces uncertainty, making the coordination function less efficient and increasing the extractive potential. Active enforcement by courts is required to adjudicate claims and maintain the boundary. Extractiveness is high due to the chilling effect on public regulation and the direct compensation costs. Suppression is moderate, as regulators are not entirely prevented from acting, but face significant hurdles. Theater ratio is low, as the legal process is genuinely functional, though complex.
 *
 * PERSPECTIVAL GAP:
 *   Property owners perceive this as a vital protection of their rights, ensuring fairness against government action. Regulators and public interest advocates, however, experience it as a significant impediment, forcing public goods to be privately purchased or abandoned, and creating a chilling effect on necessary regulations. The courts, as agenda setters, navigate this tension, often producing outcomes that satisfy neither side fully, but maintain the legal framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and developers are beneficiaries (d towards 0.0) as the constraint protects their economic interests. Local governments, environmental regulators, and public interest advocates are targets (d towards 1.0) as they bear the costs of compensation or foregone regulation. Courts are agenda setters (d towards 0.5), administering the balancing test.
 *
 * MANDATROPHY ANALYSIS:
 *   The regulatory takings doctrine prevents mislabeling legitimate property protection as pure extraction, and vice versa. Its complexity, however, means that the 'mandate' of balancing private and public interests can drift towards favoring private interests due to the high cost and uncertainty for public actors. The ad hoc nature of the Penn Central test means it is always 'live' but its application can become more extractive over time if courts consistently favor property owners, leading to a form of 'mandatrophy' where the balancing function becomes a de facto barrier to public goods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_takings_kernel_reading,
    'Is this constraint a genuine protection against government overreach, or an impediment to necessary public welfare regulations?',
    'Empirical analysis of regulatory outcomes: does it primarily prevent arbitrary seizures, or does it chill legitimate public interest regulation?',
    'If primarily an impediment, its classification shifts towards Snare for public interest stakeholders; if a genuine protection, it remains a Tangled Rope balancing interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_takings_kernel_reading, conceptual, 'This constraint is the ''regulatory takings'' reading of the broader ''takings_clause_boundary'' kernel. It expands the concept of a ''taking'' beyond physical appropriation to include severe diminution of economic value, introducing an ad hoc balancing test (Penn Central factors). Sibling readings (''physical_appropriation_reading'', ''categorical_takings_reading'') offer narrower or per se rules.').

omega_variable(
    too_far_ambiguity,
    'What constitutes ''too far'' in diminishing economic value, and is this threshold consistently applied?',
    'Analysis of judicial decisions over time for consistency and predictability in applying the Penn Central factors.',
    'If ''too far'' is arbitrary or inconsistent, the constraint''s suppression and extractiveness are higher due to regulatory uncertainty and chilling effects; if predictable, it functions more as a clear boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(too_far_ambiguity, empirical, 'The core ambiguity of the regulatory takings doctrine lies in defining the threshold for ''too far'' diminution of value, leading to unpredictable outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(taki_be_t10, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(taki_su_t10, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, land_use_zoning_regulations).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, environmental_protection_laws).

% DUAL FORMULATION NOTE:
% This constraint is the 'regulatory takings' reading of the 'takings_clause_boundary' kernel. It differs from 'physical_appropriation_reading' (which requires direct physical invasion) and 'categorical_takings_reading' (which applies per se rules to total value loss or permanent physical occupation) by introducing a flexible, fact-specific balancing test for regulations that merely diminish value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
