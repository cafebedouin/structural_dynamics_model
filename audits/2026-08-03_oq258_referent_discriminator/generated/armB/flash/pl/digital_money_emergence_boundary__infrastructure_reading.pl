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
 *   human_readable: Digital Money Emergence Boundary (Infrastructure Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the emergence of digital money through the lens
 *   of enabling infrastructure. It posits that digital money became a reality
 *   when the technical systems (like ATMs, ACH, and SWIFT) allowed for
 *   electronic transfer between financial institutions, even if direct
 *   consumer access was not yet widespread. This reading places the boundary
 *   in the mid-to-late 20th century, focusing on the operational capabilities
 *   of the banking system rather than abstract concepts or consumer-facing
 *   products. The constraint is claimed as a Mountain because it describes a
 *   historical, empirically verifiable development, not a human-constructed
 *   rule, though it benefits those who control the infrastructure.
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
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary (Infrastructure Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'ed02dc72-a840-4ed2-8ae0-c203e80529f2').
narrative_ontology:cs_kernel_codification('ed02dc72-a840-4ed2-8ae0-c203e80529f2', implicit).
narrative_ontology:cs_authority_grounding('ed02dc72-a840-4ed2-8ae0-c203e80529f2', expertise).
narrative_ontology:cs_interpretation_layer_present('ed02dc72-a840-4ed2-8ae0-c203e80529f2').
narrative_ontology:cs_reading_relation('ed02dc72-a840-4ed2-8ae0-c203e80529f2', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('ed02dc72-a840-4ed2-8ae0-c203e80529f2', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('ed02dc72-a840-4ed2-8ae0-c203e80529f2', foundational, electronic_transfer_enables_digital_money).
narrative_ontology:cs_axiom_status(electronic_transfer_enables_digital_money, holdable).
narrative_ontology:cs_axiom_grounding('ed02dc72-a840-4ed2-8ae0-c203e80529f2', electronic_transfer_enables_digital_money, empirically_contingent).
narrative_ontology:cs_reference_frame('ed02dc72-a840-4ed2-8ae0-c203e80529f2', institutional_electronic_transfer_capability).
narrative_ontology:cs_drift_state('ed02dc72-a840-4ed2-8ae0-c203e80529f2', contemporary_fintech_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ed02dc72-a840-4ed2-8ae0-c203e80529f2', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities (e.g., SWIFT, ACH operators) provide the technical rails for electronic money transfers between financial institutions. They benefit from the definition of digital money being tied to their infrastructure, as it solidifies their foundational role and revenue streams, even if they don't directly 'extract' from the definition itself.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, beneficiary,
    institutional, generational, arbitrage, global).

% These bodies define and oversee what constitutes 'money' within their jurisdictions, including digital forms. Their definitions are influenced by the capabilities of the underlying infrastructure, shaping policy and oversight frameworks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, financial_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Academics and researchers who analyze the nature and evolution of money. They observe how technological capabilities shape the practical definition of money, influencing their theoretical models and classifications (e.g., M4/M5 aggregates).
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, empirically grounded definition for when 'digital money' became a practical reality, enabling coordinated regulatory and theoretical frameworks around its existence.
% TRANSFER_FUNCTION: The constraint itself doesn't transfer value, but its definition clarifies the point at which electronic value transfer became possible between institutions, enabling subsequent transfers of digital funds from banks to other banks.
% ABSENT_VOICES: Early conceptual theorists who might argue for an earlier, purely theoretical emergence, or consumer advocates who might argue for a later emergence tied to direct consumer access. Their perspectives are not central to this infrastructure-focused definition.
% DISAPPEARANCE_RATIONALE: The historical fact of infrastructure development (ATMs, ACH, SWIFT) enabling electronic transfers is an objective historical event. Its 'disappearance' would not alter the past reality, though it would remove a specific analytical boundary for understanding digital money's evolution.
% FOUNDING_PROBLEM: To establish a clear, empirically verifiable historical boundary for the emergence of digital money, distinct from its theoretical conceptualization or consumer-facing availability.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and financial technology experts corroborate this boundary, citing the operational dates of key financial infrastructure as the point of practical emergence. This is attested by academic publications and industry records, not just by the benefiting infrastructure providers.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.15) is low because the constraint itself is a historical observation, not an active mechanism of extraction. However, the 'beneficiaries' (banking infrastructure providers) gain legitimacy and a solidified role from this definition, hence a non-zero, but low, extractiveness. Suppression (0.05) is minimal, as it's a descriptive boundary, not an enforced one. Accessibility collapse (0.9) is high because the historical facts of infrastructure development are largely fixed. Resistance (0.05) is low, as the historical facts are not actively resisted, though their interpretation is contested.
 *
 * PERSPECTIVAL GAP:
 *   While the historical facts of infrastructure development are not contested, the *significance* of this boundary relative to other potential emergence points (conceptualization or consumer access) is. This leads to different readings of the same kernel, rather than different experiences of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure providers are beneficiaries because this reading validates their historical role and the importance of their systems in the evolution of money. Financial regulators and monetary theorists are observers/agenda-setters who use this boundary for their work, but do not directly benefit from it in an extractive sense.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a historical boundary, is not subject to mandatrophy in the same way an active policy or institution would be. Its 'mandate' is to accurately describe a historical transition, which remains relevant as long as the history of money is studied. The low extractiveness and high accessibility collapse are consistent with a Mountain classification, preventing mislabeling it as a constructed constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_boundary,
    'Is this emergence boundary a ''natural'' historical fact, or a constructed analytical frame that benefits identifiable agents?',
    'Analysis of alternative historical framings: if other framings (conceptual, consumer-focused) yield significantly different beneficiary structures, it suggests a degree of construction in this reading.',
    'If more constructed, the ''mountain'' classification would be weaker, potentially shifting towards a ''tangled_rope'' if the benefits to infrastructure providers are deemed more active and less incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_boundary, conceptual, 'Ambiguity between a natural historical boundary and a constructed analytical frame.').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is one reading of the ''digital_money_emergence_boundary'' kernel. How would the classification change if a sibling reading (e.g., ''conceptualization_reading'' or ''consumer_holdings_reading'') were adopted as the primary frame?',
    'Comparative analysis of the structural properties (beneficiaries, extractiveness, suppression) of the sibling readings. The ''conceptualization_reading'' might have no beneficiaries, while the ''consumer_holdings_reading'' might benefit fintech companies.',
    'Each reading would likely yield a different classification and beneficiary structure, highlighting the perspectival nature of the ''emergence'' definition. This reading''s ''mountain'' classification is specific to its focus on infrastructure as a fixed historical development.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Impact of alternative kernel readings on constraint classification.').


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
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.13).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.03).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.04).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, m4_m5_monetary_aggregates_definition).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_emergence_boundary' kernel. This 'infrastructure_reading' focuses on the enabling technology, while the 'conceptualization_reading' focuses on theoretical breakthroughs and the 'consumer_holdings_reading' focuses on direct user access. All three are linked as they represent different perspectives on the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
