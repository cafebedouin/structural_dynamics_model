% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: IP Category Emergence: Synchronic/Diachronic Seam
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the analytical challenge of determining
 *   whether the 'thinkability' of a new legal category (e.g., 'ownable
 *   expression' for copyright) is formally independent of its 'first-holding'
 *   (the first instance of a legal claim being successfully asserted for it).
 *   It tests if category emergence and occupancy change can vary
 *   independently or always co-occur, which determines if the kernel
 *   structure of IP is authentic or a temporal framing artifact. This reading
 *   claims the constraint is a Mountain, reflecting its status as a
 *   fundamental conceptual problem in legal history, not a human-made
 *   construct.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.3).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.2).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.3).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, mountain).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "IP Category Emergence: Synchronic/Diachronic Seam").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '5848fabb-5c6a-409e-ae21-8b7c70cd449b').
narrative_ontology:cs_kernel_codification('5848fabb-5c6a-409e-ae21-8b7c70cd449b', distributed).
narrative_ontology:cs_authority_grounding('5848fabb-5c6a-409e-ae21-8b7c70cd449b', expertise).
narrative_ontology:cs_interpretation_layer_present('5848fabb-5c6a-409e-ae21-8b7c70cd449b').
narrative_ontology:cs_reading_relation('5848fabb-5c6a-409e-ae21-8b7c70cd449b', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('5848fabb-5c6a-409e-ae21-8b7c70cd449b', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_axiom('5848fabb-5c6a-409e-ae21-8b7c70cd449b', foundational, conceptual_emergence_is_distinct_from_legal_occupancy).
narrative_ontology:cs_axiom_status(conceptual_emergence_is_distinct_from_legal_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('5848fabb-5c6a-409e-ae21-8b7c70cd449b', conceptual_emergence_is_distinct_from_legal_occupancy, empirically_contingent).
narrative_ontology:cs_reference_frame('5848fabb-5c6a-409e-ae21-8b7c70cd449b', analytical_separability_framework).
narrative_ontology:cs_drift_state('5848fabb-5c6a-409e-ae21-8b7c70cd449b', contemporary_interdisciplinary_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5848fabb-5c6a-409e-ae21-8b7c70cd449b', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, legal_historians).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, intellectual_property_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clearer understanding of how legal categories evolve and whether the 'thinkability' of a concept precedes or co-occurs with its 'first-holding' in law. Their research is directly advanced by resolving this seam.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_historians, beneficiary,
    analytical, generational, analytical, global).

% Gain foundational insights into the nature of intellectual property rights. If thinkability and first-holding are independent, it suggests a more complex, multi-causal origin for IP; if they collapse, it simplifies the historical narrative.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, intellectual_property_theorists, beneficiary,
    analytical, generational, analytical, global).

% The primary data source for this analysis. They passively 'observe' the constraint by providing the evidence that either supports or refutes the independence of thinkability and first-holding. They have no agency.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, historical_legal_texts, observer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ip_category_emergence__synchronic_diachronic_seam, historical_legal_texts).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for analyzing the historical development of legal concepts, coordinating scholarly inquiry into the origins of intellectual property by defining the terms of the debate.
% TRANSFER_FUNCTION: Transfers conceptual clarity and historical accuracy to legal scholarship, from the analysis of historical data to the theoretical understanding of legal evolution.
% ABSENT_VOICES: The historical actors who lived through the emergence of IP categories are absent; their direct testimony on the conceptual independence of 'thinkability' and 'first-holding' is unavailable, leaving interpretation to modern scholars.
% DISAPPEARANCE_RATIONALE: If the question of the synchronic/diachronic seam vanished, the entire field of historical IP jurisprudence would lose a core analytical problem, forcing a re-evaluation of its foundational assumptions and research agendas.
% FOUNDING_PROBLEM: To understand whether the legal recognition of a new category of 'ownable expression' (thinkability) necessarily coincided with the establishment of the first legal claims to such expression (first-holding), or if these were distinct historical processes.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and philosophers of law universally attest to the problem's live status, as it underpins debates about the nature of legal change and the historical contingency of property rights. This corroboration comes from the entire scholarly community, not just those who benefit from the specific framing.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, ExtMetricName, E),
    domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ip_category_emergence__synchronic_diachronic_seam),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness is low (0.3) because it primarily extracts intellectual effort from scholars, not material resources. Suppression is low (0.2) as there are no active coercive forces preventing scholars from pursuing alternative interpretations, only the inherent difficulty of the historical record. Theater ratio is low (0.1) as the scholarly pursuit is genuine, not performative. Accessibility collapse is high (0.7) because once the conceptual problem is understood, the alternatives for framing the historical relationship between thinkability and first-holding are limited. Resistance is low (0.15) because the debate is academic, not activist.
 *
 * PERSPECTIVAL GAP:
 *   As an analytical constraint, there is minimal perspectival gap among the primary beneficiaries (scholars), who largely agree on the nature of the problem, even if they disagree on its resolution. The constraint itself is the object of their shared inquiry.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal historians and IP theorists are beneficiaries, as resolving this conceptual seam directly advances their understanding and research. Historical legal texts are observers, providing the data without agency. There are no direct victims or agenda-setters in this analytical constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_independence_of_concepts,
    'Can historical evidence definitively establish the formal independence or necessary co-occurrence of ''thinkability'' and ''first-holding'' for a legal category?',
    'Discovery of historical cases where a concept was clearly ''thinkable'' but not yet ''held'' in law, or vice-versa, through exhaustive archival research and re-interpretation of existing legal texts.',
    'If independence is proven, it suggests a more complex, multi-stage process of legal evolution. If co-occurrence is necessary, it implies a tighter, perhaps causal, link between conceptualization and legal enactment, potentially simplifying the historical narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_independence_of_concepts, empirical, 'Uncertainty regarding the empirical separability of conceptual emergence and legal application in historical data.').

omega_variable(
    conceptual_framing_artifact,
    'Is the distinction between ''thinkability'' and ''first-holding'' a genuine structural feature of legal history, or an artifact of modern analytical framing imposed on historical data?',
    'Development of alternative analytical frameworks that either dissolve the distinction or reveal its constructed nature, leading to a re-evaluation by the scholarly community.',
    'If it''s an artifact, the ''Mountain'' classification might shift towards a ''Rope'' or ''Tangled Rope'' if the framing itself is found to coordinate or extract scholarly effort based on a spurious distinction. If genuine, the Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_framing_artifact, conceptual, 'Ambiguity about whether the analytical distinction reflects historical reality or a modern interpretive lens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 1710, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1710, 0.05).
narrative_ontology:measurement(ip_c_tr_t1800, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1800, 0.07).
narrative_ontology:measurement(ip_c_tr_t1900, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(ip_c_tr_t2000, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(ip_c_tr_t2024, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1710, 0.2).
narrative_ontology:measurement(ip_c_be_t1800, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(ip_c_be_t1900, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement(ip_c_be_t2000, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 2000, 0.29).
narrative_ontology:measurement(ip_c_be_t2024, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1710, 0.1).
narrative_ontology:measurement(ip_c_su_t1800, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement(ip_c_su_t1900, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(ip_c_su_t2000, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 2000, 0.19).
narrative_ontology:measurement(ip_c_su_t2024, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ip_category_emergence' kernel. This reading focuses on the relationship between conceptual emergence ('thinkability') and legal occupancy ('first-holding'), while the sibling readings focus on each aspect individually. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
