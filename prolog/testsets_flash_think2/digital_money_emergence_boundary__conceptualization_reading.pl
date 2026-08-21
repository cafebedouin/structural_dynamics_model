% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Emergence Boundary of Digital Money (Conceptualization Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the emergence of digital money as the point it
 *   became theoretically thinkable, driven by advances in telecommunications
 *   and cryptography. This 'conceptualization reading' focuses on the
 *   intellectual breakthroughs that established the feasibility of digital
 *   value, rather than its infrastructural implementation or widespread
 *   consumer adoption. It is a Mountain because the theoretical possibility,
 *   once established, is a fixed point in intellectual history.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.15).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.1).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Emergence Boundary of Digital Money (Conceptualization Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '46417e6d-0067-4e1d-a1bf-5b843a7a194f').
narrative_ontology:cs_kernel_codification('46417e6d-0067-4e1d-a1bf-5b843a7a194f', formalized).
narrative_ontology:cs_authority_grounding('46417e6d-0067-4e1d-a1bf-5b843a7a194f', expertise).
narrative_ontology:cs_reading_relation('46417e6d-0067-4e1d-a1bf-5b843a7a194f', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_reading_relation('46417e6d-0067-4e1d-a1bf-5b843a7a194f', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('46417e6d-0067-4e1d-a1bf-5b843a7a194f', foundational, digital_value_is_information).
narrative_ontology:cs_axiom_status(digital_value_is_information, holdable).
narrative_ontology:cs_axiom_grounding('46417e6d-0067-4e1d-a1bf-5b843a7a194f', digital_value_is_information, empirically_contingent).
narrative_ontology:cs_axiom('46417e6d-0067-4e1d-a1bf-5b843a7a194f', foundational, secure_transfer_requires_cryptography).
narrative_ontology:cs_axiom_status(secure_transfer_requires_cryptography, holdable).
narrative_ontology:cs_axiom_grounding('46417e6d-0067-4e1d-a1bf-5b843a7a194f', secure_transfer_requires_cryptography, empirically_contingent).
narrative_ontology:cs_reference_frame('46417e6d-0067-4e1d-a1bf-5b843a7a194f', theoretical_feasibility_established).
narrative_ontology:cs_drift_state('46417e6d-0067-4e1d-a1bf-5b843a7a194f', contemporary_digital_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('46417e6d-0067-4e1d-a1bf-5b843a7a194f', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_researchers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, cryptographers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, computer_scientists).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, information_theory_advances).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, cryptographic_primitives_feasibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the scholars who define and validate the theoretical boundaries of what is possible, gaining intellectual capital and priority claims for establishing the conceptual framework of digital money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_researchers, agenda_setter,
    institutional, generational, analytical, universal).

% Their foundational work, particularly on secure digital signatures and blind signatures (e.g., David Chaum), directly established the theoretical possibility of digital cash, earning them significant recognition.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, cryptographers, beneficiary,
    powerful, biographical, analytical, global).

% Their contributions to telecommunications, distributed systems, and network theory provided the underlying conceptual and technological substrate that made digital money thinkable, even before practical implementation.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, computer_scientists, beneficiary,
    powerful, biographical, analytical, global).

% They analyze and document the intellectual and technological lineage of digital money, interpreting the significance of theoretical breakthroughs in its emergence.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, financial_historians, observer,
    analytical, generational, analytical, universal).

% From their operational perspective, 'money' typically refers to circulating instruments with practical implications for monetary policy and financial stability. Purely theoretical concepts, while interesting, are often considered outside their immediate domain of concern for 'money emergence'.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bankers, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared intellectual framework and vocabulary for discussing, researching, and developing digital money concepts, allowing disparate researchers to build upon common theoretical ground.
% TRANSFER_FUNCTION: Transfers intellectual priority, academic recognition, and research funding opportunities to those who first conceptualized the theoretical possibility of digital money.
% ABSENT_VOICES: Traditional economists and central bankers, whose definitions of 'money' often emphasize practical circulation and institutional backing, would argue that theoretical possibility alone does not constitute 'money emergence'. They are excluded from this conceptualization-focused definition.
% DISAPPEARANCE_RATIONALE: If the theoretical possibility of secure, transferable digital value had never emerged, the entire subsequent trajectory of digital currency development, from e-cash to cryptocurrencies and CBDCs, would be impossible. The intellectual and technological path would be fundamentally different or non-existent.
% FOUNDING_PROBLEM: The intellectual challenge of conceiving a secure, transferable, and verifiable digital representation of value that could function as money, independent of physical tokens or centralized physical control.
% FOUNDING_PROBLEM_CORROBORATION: Historians of cryptography and technology, independent of the original researchers, consistently document the significance of these theoretical breakthroughs as foundational to the modern digital economy. The ongoing research into optimal digital money systems (e.g., CBDCs) continues to build on these conceptual foundations.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The metrics reflect a conceptual boundary: extractiveness is low (0.15) as benefits are primarily intellectual capital and priority claims, not direct financial rents. Suppression is low (0.1) as ideas are difficult to suppress once articulated. Theater ratio is very low (0.05) because the work involved was genuine intellectual and scientific endeavor. Accessibility collapse is high (0.9) because once the theoretical possibility is understood, it fundamentally alters the intellectual landscape.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between those who define 'money emergence' by theoretical possibility (academics, cryptographers) and those who define it by practical infrastructure or consumer adoption (central bankers, financial institutions). The former see this constraint as the true origin; the latter might view it as a mere precursor.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic researchers, cryptographers, and computer scientists are beneficiaries as they gain intellectual priority and recognition for establishing this conceptual boundary. There are no direct 'victims' of a theoretical emergence, though other definitions of 'money emergence' might exclude these early conceptualizers from priority. Central bankers are 'excluded' as their operational mandate typically focuses on circulating money, not theoretical constructs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergence_definition_ambiguity,
    'Does ''emergence'' refer to theoretical possibility, infrastructural readiness, or widespread consumer adoption?',
    'Consensus among financial historians and economists on a primary definition of ''money emergence'', or empirical evidence showing which factor was the true bottleneck.',
    'If ''emergence'' is defined by later stages (infrastructure or consumer holdings), this conceptualization reading would be reclassified as a precursor, not the emergence itself, shifting its significance and the beneficiaries'' claims to priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergence_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''money emergence''.').

omega_variable(
    scope_of_money_ambiguity,
    'Does the concept of ''money'' include purely theoretical constructs and research prototypes, or only instruments in active circulation?',
    'A formal definition of ''money'' adopted by relevant regulatory bodies or a widely accepted academic consensus on the scope of monetary theory.',
    'If ''money'' is restricted to circulating instruments, the beneficiaries of this constraint (academics) would be seen as defining a ''proto-money'' boundary, not ''money'' itself, diminishing their claim to priority and the ''mountain'' status of the conceptualization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_money_ambiguity, conceptual, 'Ambiguity in the scope of what constitutes ''money''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement(digi_tr_t1965, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1965, 0.04).
narrative_ontology:measurement(digi_tr_t1970, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1970, 0.04).
narrative_ontology:measurement(digi_tr_t1975, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1975, 0.05).
narrative_ontology:measurement(digi_tr_t1980, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.05).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(digi_be_t1965, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1965, 0.11).
narrative_ontology:measurement(digi_be_t1970, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(digi_be_t1975, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1975, 0.13).
narrative_ontology:measurement(digi_be_t1980, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(digi_su_t1965, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1965, 0.09).
narrative_ontology:measurement(digi_su_t1970, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1970, 0.09).
narrative_ontology:measurement(digi_su_t1975, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1975, 0.1).
narrative_ontology:measurement(digi_su_t1980, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_emergence_boundary' kernel, each defining emergence at a different stage (conceptualization, infrastructure, consumer holdings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
