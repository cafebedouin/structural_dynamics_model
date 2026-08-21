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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence Boundary: Infrastructure-Enabled Transfer
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the emergence of digital money through the lens
 *   of operational infrastructure: specifically, the point at which banking
 *   systems gained the capability for electronic transfer (e.g., ATMs, ACH,
 *   SWIFT). This reading posits that money became 'digital' when it could be
 *   moved electronically by banks, even if consumers didn't yet hold it in
 *   directly digital forms. It marks a middle boundary in the broader debate
 *   about digital money's origin, emphasizing the institutional and
 *   technological prerequisites for its widespread function. The claimed type
 *   is 'mountain' because this reading presents the emergence as a natural
 *   consequence of technological and infrastructural development, largely
 *   independent of human choice once the technology was available.
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
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary: Infrastructure-Enabled Transfer").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d').
narrative_ontology:cs_kernel_codification('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d', distributed).
narrative_ontology:cs_authority_grounding('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d', expertise).
narrative_ontology:cs_interpretation_layer_present('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d').
narrative_ontology:cs_reading_relation('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d', foundational, operational_capability_defines_existence).
narrative_ontology:cs_axiom_status(operational_capability_defines_existence, holdable).
narrative_ontology:cs_axiom_grounding('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d', operational_capability_defines_existence, conventional).
narrative_ontology:cs_reference_frame('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d', bank_centric_operational_reality).
narrative_ontology:cs_drift_state('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d', contemporary_crypto_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('aaf7ddb7-eff0-4b21-8f3e-bbfb7671c55d', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities (e.g., SWIFT, ACH operators) directly benefit from the definition of digital money being tied to their operational capabilities. Their infrastructure becomes the de facto standard for what constitutes 'digital money' in this reading, granting them a central, indispensable role.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, beneficiary,
    institutional, generational, constrained, global).

% Academics and researchers who study the evolution of financial systems. They analyze the historical development of payment systems and the conceptual shifts in understanding money, often debating the precise 'moment' of digital money's emergence.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, financial_historians, observer,
    analytical, generational, analytical, global).

% Institutions responsible for monetary policy and financial stability. Their definitions of money often align with what can be measured and controlled through existing banking infrastructure, making this reading highly relevant to their operational scope.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, operationally grounded definition for the emergence of digital money, allowing financial institutions and regulators to coordinate on a shared understanding of when electronic value transfers became 'money'.
% TRANSFER_FUNCTION: This constraint primarily transfers definitional clarity and operational scope to banking infrastructure, establishing their systems as the foundational layer for digital money.
% ABSENT_VOICES: Theorists who prioritize conceptual breakthroughs over operational implementation, and early digital currency enthusiasts who might argue for earlier or later emergence based on non-bank-centric criteria, are often marginalized in this infrastructure-focused historical account.
% DISAPPEARANCE_RATIONALE: This constraint describes a historical boundary based on technological and institutional facts. If this specific reading 'disappeared', the historical events (ATMs, ACH, SWIFT) would still have occurred, and their impact on financial systems would remain, though the interpretation of 'digital money's emergence' might shift to other readings.
% FOUNDING_PROBLEM: The problem of precisely dating and defining the transition from physical to electronic forms of money, particularly for statistical and regulatory purposes, given the gradual nature of technological adoption.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians and central bank economists corroborate the ongoing challenge of defining and measuring money in an increasingly digital landscape, making the precise historical boundary a live debate for policy and analysis.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The extractiveness (0.15) is low because this reading primarily defines a historical boundary, not an active extractive mechanism. Any 'extraction' is diffuse, flowing to the banking infrastructure providers who become central to the definition. Suppression (0.05) is minimal, as this is a descriptive historical claim rather than an actively enforced rule. Accessibility collapse (0.9) is high because, within this framework, the infrastructure's existence is a prerequisite, making alternatives to this definition largely inaccessible. Resistance (0.05) is low, as the debate is academic/conceptual rather than active opposition to an extractive force. The temporal measurements show a slight increase in extractiveness and suppression as the infrastructure became more entrenched and its definitional power solidified.
 *
 * PERSPECTIVAL GAP:
 *   While this reading is presented as a 'mountain' (a natural historical progression), the existence of other readings (conceptualization, consumer holdings) highlights a perspectival gap. Different stakeholders prioritize different criteria for 'emergence,' leading to distinct classifications. This reading's 'mountain' status is contingent on accepting the primacy of operational infrastructure as the defining characteristic.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure providers are beneficiaries because this reading centers their role in the definition of digital money. Financial historians and central banks act as observers or agenda-setters, shaping the discourse around this historical boundary. There are no direct 'victims' in this reading, as it's a definitional claim rather than an active imposition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_primacy_of_infrastructure,
    'Is the operational capability of financial infrastructure the primary determinant for defining the emergence of digital money, or are conceptual breakthroughs or consumer access equally or more fundamental?',
    'Consensus among monetary historians and economists on a unified definition, or a shift in regulatory focus that prioritizes one aspect over others.',
    'If conceptualization or consumer holdings are deemed more fundamental, this ''infrastructure_reading'' would be reclassified from a Mountain to a conceptual Snare, as it would be seen as an imposed definition benefiting banking institutions rather than a natural boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definitional_primacy_of_infrastructure, conceptual, 'Ambiguity over which criterion (conceptual, infrastructural, or consumer-facing) holds definitional primacy for digital money''s emergence.').

omega_variable(
    false_summit_of_naturalness,
    'Is the ''natural emergence'' of digital money via infrastructure a genuine Mountain, or a constructed narrative that benefits banking infrastructure providers by legitimizing their historical role?',
    'Analysis of historical lobbying efforts and definitional influence by banking institutions on regulatory bodies and academic discourse. If active shaping is found, reclassify as a Tangled Rope.',
    'If found to be a constructed narrative, the constraint''s extractiveness would be re-evaluated as higher, and its classification would shift from Mountain to Tangled Rope, reflecting the active maintenance of a beneficial definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_naturalness, empirical, 'Whether the ''natural'' emergence is truly natural or a constructed narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1960, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(digi_be_t1965, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1965, 0.11).
narrative_ontology:measurement(digi_be_t1970, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(digi_be_t1975, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement(digi_be_t1980, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1980, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1960, 0.03).
narrative_ontology:measurement(digi_su_t1965, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1965, 0.04).
narrative_ontology:measurement(digi_su_t1970, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1970, 0.04).
narrative_ontology:measurement(digi_su_t1975, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1975, 0.05).
narrative_ontology:measurement(digi_su_t1980, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1980, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_emergence_boundary' kernel. This 'infrastructure_reading' focuses on the operational capabilities of banking systems. It influences the other readings by providing a concrete, institutionally-grounded historical anchor, but does not foreclose them as they operate on different definitional criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
