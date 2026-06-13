% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy over Constitutional Interpretation
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of the
 *   constitutional authority boundary, where courts are the final,
 *   unchallengeable arbiters of constitutional questions. This reading grants
 *   the judiciary the power to invalidate legislative and executive acts
 *   without direct remedy, effectively establishing a counter-majoritarian
 *   veto. The constraint is framed as a Tangled Rope because it provides a
 *   coordination function (finality in constitutional interpretation) but
 *   also involves significant asymmetric extraction (interpretive monopoly
 *   rents for the judiciary, constrained policy space for elected branches).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.75).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy over Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '4d8469d2-8c9e-49a3-afbc-a657fc6ba836').
narrative_ontology:cs_kernel_codification('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', fixed_text).
narrative_ontology:cs_authority_grounding('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', lineage).
narrative_ontology:cs_interpretation_layer_present('4d8469d2-8c9e-49a3-afbc-a657fc6ba836').
narrative_ontology:cs_reading_relation('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', foundational, judicial_interpretive_finality).
narrative_ontology:cs_axiom_status(judicial_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', judicial_interpretive_finality, conventional).
narrative_ontology:cs_axiom('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', foundational, constitutional_supremacy_over_ordinary_law).
narrative_ontology:cs_axiom_status(constitutional_supremacy_over_ordinary_law, holdable).
narrative_ontology:cs_axiom_grounding('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', constitutional_supremacy_over_ordinary_law, deontological).
narrative_ontology:cs_reference_frame('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4d8469d2-8c9e-49a3-afbc-a657fc6ba836', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, federal_judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, legislative_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, electorate).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high due to the judiciary's counter-majoritarian power to overturn democratically enacted laws, effectively extracting policy space from the legislative and executive branches. Suppression (0.75) is also high, as the other branches are structurally compelled to accept judicial rulings, with limited and high-friction avenues for challenge (e.g., constitutional amendment). The theater ratio (0.15) is low, indicating that the judiciary's interpretive function is largely genuine, though its application may be contested. The historical measurements show a clear increase in both extractiveness and suppression over time, reflecting the consolidation of judicial power since Marbury v. Madison (1803).
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this constraint is a necessary Rope, ensuring constitutional fidelity and stability. From the perspective of the legislative and executive branches, it often functions as a Snare, limiting their ability to respond to public will. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court Justices and the federal judiciary are clear beneficiaries (d near 0.0) as they gain interpretive monopoly and institutional prestige. The legislative and executive branches, along with the electorate, are targets (d near 1.0) as their policy-making and democratic accountability are constrained. The 'identity_locked' exit option for judges reflects that their professional identity and power are intrinsically tied to this interpretive role.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_monopoly_legitimacy,
    'Is the judiciary''s interpretive monopoly a legitimate exercise of constitutional power, or an institutional accretion that has exceeded its original mandate?',
    'Historical analysis of founding intent, comparative constitutional studies, and ongoing public debate regarding democratic accountability versus judicial independence.',
    'If deemed an accretion, it would support reclassifying the constraint closer to a Snare for the elected branches; if legitimate, it reinforces the Tangled Rope classification by emphasizing the coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_monopoly_legitimacy, conceptual, 'Legitimacy of judicial interpretive monopoly.').

omega_variable(
    counter_majoritarian_difficulty_resolution,
    'How effectively does the system provide remedies for judicial overreach, balancing judicial finality with democratic accountability?',
    'Analysis of the frequency and success rate of constitutional amendments, impeachment proceedings, and judicial appointments as mechanisms to check judicial power.',
    'If remedies are largely ineffective, the suppression metric for elected branches is higher than currently measured; if effective, it suggests a more balanced (though still extractive) system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_difficulty_resolution, empirical, 'Effectiveness of checks on judicial power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1789, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(cons_tr_t1803, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1803, 0.1).
narrative_ontology:measurement(cons_tr_t1865, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1865, 0.12).
narrative_ontology:measurement(cons_tr_t1937, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(cons_tr_t1970, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cons_tr_t2024, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t1789, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(cons_be_t1803, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1803, 0.45).
narrative_ontology:measurement(cons_be_t1865, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1865, 0.5).
narrative_ontology:measurement(cons_be_t1937, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1937, 0.6).
narrative_ontology:measurement(cons_be_t1970, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(cons_be_t2024, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1789, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(cons_su_t1803, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1803, 0.4).
narrative_ontology:measurement(cons_su_t1865, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1865, 0.5).
narrative_ontology:measurement(cons_su_t1937, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1937, 0.65).
narrative_ontology:measurement(cons_su_t1970, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(cons_su_t2024, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_authority_boundary' kernel. It is linked to other readings (coordinate_construction_reading, parliamentary_primacy_reading) via the cs_structure.reading_relations field, which describes the structural relationships between these interpretive frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
