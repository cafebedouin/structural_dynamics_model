% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Conceptualization Boundary (1960s–1985 Theoretical Emergence)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   Between 1960 and 1985, digital money transitioned from science fiction to
 *   theoretical tractability. Telecommunications advances (packet switching,
 *   fiber optics) made large-scale information transfer feasible. David
 *   Chaum's formalization in the mid-1980s — cryptographic protocols for
 *   peer-to-peer value transfer without trusted intermediaries — established
 *   the conceptual boundary that makes digital money thinkable as an economic
 *   and technical category. This reading instantiates the emergence boundary
 *   at the point of theoretical formalization: the moment when digital money
 *   became a coherent object of inquiry, not merely a speculative fantasy.
 *   The claim/metric divergence is intentional: this constraint is CLAIMED as
 *   mountain (a natural discovery of what cryptography makes possible) while
 *   the authored metrics reflect modest extractiveness (academic community
 *   benefits from establishing priority) and low theater (the theoretical
 *   work is genuinely functional, not performative). The engine's
 *   classification will measure whether the natural-law framing holds.
 *
 * KEY AGENTS:
 *   - academic_cryptography_community: Establishes the boundary through Chaum formalization and foundational publications; collects priority claims and intellectual authority
 *   - telecommunications_infrastructure_builders: Provide substrate enabling conceptual tractability; do not set the boundary
 *   - central_banks_and_regulators: Excluded from formalization phase; would contest that theoretical emergence precedes regulatory emergence
 *   - existing_payment_system_operators: Excluded; maintain non-digital infrastructure; foreshadow competition but not present at boundary
 *   - computer_science_discipline: Non-agent vindicated proposition; achieves economic applicability status through the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.28).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.15).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Conceptualization Boundary (1960s–1985 Theoretical Emergence)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, 'df09003a-3b20-4c36-af17-80913761dd90').
narrative_ontology:cs_kernel_codification('df09003a-3b20-4c36-af17-80913761dd90', distributed).
narrative_ontology:cs_authority_grounding('df09003a-3b20-4c36-af17-80913761dd90', expertise).
narrative_ontology:cs_interpretation_layer_present('df09003a-3b20-4c36-af17-80913761dd90').
narrative_ontology:cs_reading_relation('df09003a-3b20-4c36-af17-80913761dd90', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_reading_relation('df09003a-3b20-4c36-af17-80913761dd90', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('df09003a-3b20-4c36-af17-80913761dd90', foundational, theoretical_tractability_precedes_operational_emergence).
narrative_ontology:cs_axiom_status(theoretical_tractability_precedes_operational_emergence, holdable).
narrative_ontology:cs_axiom_grounding('df09003a-3b20-4c36-af17-80913761dd90', theoretical_tractability_precedes_operational_emergence, empirically_contingent).
narrative_ontology:cs_axiom('df09003a-3b20-4c36-af17-80913761dd90', foundational, cryptographic_secrecy_enables_monetary_function).
narrative_ontology:cs_axiom_status(cryptographic_secrecy_enables_monetary_function, holdable).
narrative_ontology:cs_axiom_grounding('df09003a-3b20-4c36-af17-80913761dd90', cryptographic_secrecy_enables_monetary_function, empirically_contingent).
narrative_ontology:cs_reference_frame('df09003a-3b20-4c36-af17-80913761dd90', computational_tractability_standard).
narrative_ontology:cs_drift_state('df09003a-3b20-4c36-af17-80913761dd90', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df09003a-3b20-4c36-af17-80913761dd90', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, theoretical_computer_science_discipline).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.28 at interval end) because the conceptualization boundary yields intellectual authority and citation priority to researchers, but does not yet transfer material wealth or operational control. The academic community benefits by establishing the boundary, but the benefit is status-based rather than rent-capture. Suppression is minimal (0.15) because the conceptualization operates in an open scientific discourse with low barriers to contestation — researchers can challenge the framing, propose alternative boundaries, and establish competing schools of thought without facing active enforcement. Theater is negligible (0.08) because the theoretical work is genuinely functional: Chaum's cryptographic protocols actually solve the double-spending problem, and the formalization does real conceptual work, not performative maintenance. Accessibility collapse is very high (0.92) because once the theoretical possibility is established, the boundary becomes nearly impossible to un-know — the conceptual framework, once published, becomes part of the background assumption space for all downstream work on digital money. Resistance is minimal (0.12) because the academic community holds substantial authority in its own domain and faces no organized opposition during the formalization phase. The suppression_requirement measurements rise slightly from 1960 to 1985 as central banks and regulators begin to recognize the theoretical threat and attempt (with minimal success during this interval) to establish definitional authority over money's emergence — but this suppression effort is inchoate and lightly enforced during the formalization phase itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The academic_cryptography_community sits as a low-d beneficiary (benefits from priority claims, no coercive relationship, high exit options). The telecommunications infrastructure builders are near-symmetric (enable the boundary, do not bear extraction cost, do not directly benefit from priority claims). Central banks and regulators are high-d victims in the latent sense: the boundary implicitly challenges their monopoly on money definition, but their victimhood is prospective rather than immediate. Existing payment system operators similarly face prospective threat. The computer_science_discipline is a non-agent vindicated proposition (not a real actor collecting rents), not a stakeholder with directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is currently CLAIMED as mountain (emerges_naturally: true) to reflect the natural-law framing: cryptography and information theory are discovered facts about what is computationally possible. However, the declaration of beneficiaries (academic_cryptography_community, theoretical_computer_science_discipline) triggers FSM evaluation. If the academic community has genuinely captured rents, priority claims, or resource flows from establishing the boundary, the natural-law framing fails and the constraint reclassifies as tangled_rope (coordination function + asymmetric extraction) or snare (extraction disguised as natural law). The mandate here is 'establish the natural boundary of digital money emergence,' which is still live — researchers continue to argue about what constitutes emergence. No mandatrophy is apparent yet; the founding problem remains actively contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_emergence_boundary,
    'Is the 1960s–1985 conceptualization boundary a natural law of information theory and cryptography (a discoverable fact about what is computationally possible), or a constructed boundary maintained by academic authority and citation practice?',
    'Historical counterfactual: would digital money have been conceptualized earlier or later in a world where cryptography research followed a different institutional path? Alternately: do alternative theoretical frameworks (e.g., those developed in non-Western or non-academic institutions) produce different emergence boundaries?',
    'If the boundary is natural law, the constraint is correctly classified as mountain with minimal extraction. If constructed, the classification shifts toward false-summit: the academic community benefits from establishing and maintaining the boundary (priority claims, citation authority, intellectual property), making it potentially a tangled_rope or snare using natural-law framing as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_emergence_boundary, conceptual, 'Whether the conceptualization boundary is discovered or constructed.').

omega_variable(
    definition_of_emergence_contestation,
    'What counts as ''emergence''? Does digital money emerge when it becomes theoretically thinkable (conceptualization reading), when infrastructure enables transfer (infrastructure reading), or when consumers can hold and transact with it (consumer_holdings reading)?',
    'Examine how different stakeholder communities use the term ''emergence'' in their own discourse. Central banks may define emergence by regulatory permission or consumer adoption. Researchers define it by theoretical tractability. Technologists define it by operational capability. The resolution is not empirical discovery but explicit negotiation of the definitional boundary.',
    'This omega documents the kernel contest itself: three readings coexist because they apply different criteria for emergence. Reclassification would occur if one reading''s criterion is adopted as canonical by an authority (e.g., a regulatory body) that supersedes others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_emergence_contestation, preference, 'The kernel contest: what definition of emergence is authoritative?').

omega_variable(
    beneficiary_identity_ambiguity,
    'Do the academic cryptography community and theoretical computer science discipline genuinely benefit from establishing the conceptualization boundary, or is the benefit illusory (confusing priority claims with actual value transfer)?',
    'Trace the intellectual and financial consequences of the boundary: Did researchers gain funding, employment, or patent rights from establishing the boundary? Did the discipline gain legitimacy or resources? Or did the boundary remain a theoretical contribution with no material extraction?',
    'If the beneficiaries genuinely capture rents or resources from the boundary, the mountain certification is suspect and FSM (false-summit-mountain) should flag the constraint. If the benefit is purely intellectual status with no material extraction, the classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, empirical, 'Whether beneficiaries genuinely capture extraction from the conceptualization boundary.').

omega_variable(
    sibling_reading_observability_separation,
    'Can the three readings (conceptualization, infrastructure, consumer_holdings) be simultaneously true and observable, or does adoption of one reading logically foreclose the others?',
    'Examine whether all three boundaries can coexist in a single historical account without contradiction. If a party can consistently hold all three boundaries as distinct emergence points, they coexist. If holding one forecloses another (e.g., ''if digital money emerged in 1960 theoretically, it cannot have emerged in 1990 for consumers''), they foreclose.',
    'This omega determines the cs_structure reading_relations assignment: coexists_with vs. forecloses. Currently assumed to be coexists_with (different parties hold different readings); if analysis shows logical foreclosure, the relation must change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_observability_separation, conceptual, 'Whether sibling readings logically foreclose each other or merely coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(digi_tr_t1968, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1968, 0.06).
narrative_ontology:measurement(digi_tr_t1976, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1976, 0.07).
narrative_ontology:measurement(digi_tr_t1983, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1983, 0.08).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.08).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(digi_be_t1968, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1968, 0.18).
narrative_ontology:measurement(digi_be_t1976, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1976, 0.24).
narrative_ontology:measurement(digi_be_t1983, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1983, 0.29).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(digi_su_t1968, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1968, 0.1).
narrative_ontology:measurement(digi_su_t1976, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1976, 0.12).
narrative_ontology:measurement(digi_su_t1983, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1983, 0.15).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__conceptualization_reading, 0.03).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the digital_money_emergence_boundary kernel. The kernel contest arises because 'emergence' is defined differently across stakeholder communities: academic researchers emphasize theoretical formalization (this reading); technologists emphasize infrastructure capability (infrastructure_reading); regulators and consumers emphasize operational adoption (consumer_holdings_reading). Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, and different classification. The readings are linked by network.affects_constraints to preserve the family relationship. No single reading is canonical; the three readings together illustrate how a single natural-language concept (digital money emergence) decomposes into structurally distinct constraints when the observational criteria change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__conceptualization_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
