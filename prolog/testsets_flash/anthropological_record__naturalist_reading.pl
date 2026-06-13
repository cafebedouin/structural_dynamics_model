% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of Human Origins (Scientific Method)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint represents the 'naturalist reading' of the
 *   anthropological record, asserting that human origins (evolution,
 *   migration) are knowable exclusively through the scientific method, which
 *   by definition excludes supernatural or non-materialist explanations. It
 *   functions as a gatekeeper for academic legitimacy and resource allocation
 *   within scientific disciplines, while simultaneously coordinating research
 *   efforts around a shared methodological framework. The constraint is
 *   actively enforced through credentialing, peer review, and funding
 *   mechanisms, which suppress alternative interpretive frameworks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.75).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of Human Origins (Scientific Method)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, 'fa1d4e4a-ebb1-4346-bb5d-d8605a35825b').
narrative_ontology:cs_kernel_codification('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', formalized).
narrative_ontology:cs_authority_grounding('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', expertise).
narrative_ontology:cs_interpretation_layer_present('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b').
narrative_ontology:cs_reading_relation('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', foundational, materialist_causation_only).
narrative_ontology:cs_axiom_status(materialist_causation_only, holdable).
narrative_ontology:cs_axiom_grounding('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', materialist_causation_only, empirically_contingent).
narrative_ontology:cs_axiom('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', foundational, scientific_method_sole_epistemic_authority).
narrative_ontology:cs_axiom_status(scientific_method_sole_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', scientific_method_sole_epistemic_authority, conventional).
narrative_ontology:cs_reference_frame('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', enlightenment_scientific_rationalism).
narrative_ontology:cs_drift_state('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fa1d4e4a-ebb1-4346-bb5d-d8605a35825b', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_scientists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, academic_institutions).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, religious_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) stems from the high cost of entry for non-credentialed individuals and the exclusion of alternative knowledge systems from academic discourse and funding. Suppression (0.75) is high due to the active enforcement of methodological boundaries and the delegitimization of non-scientific narratives. Theater ratio (0.15) is low, as the scientific method's core functions are genuinely performed, though the boundary maintenance can be performative. Accessibility collapse (0.6) is moderate, as alternative narratives exist but are institutionally marginalized. Resistance (0.4) is present from excluded groups but often lacks institutional leverage.
 *
 * PERSPECTIVAL GAP:
 *   Credentialed scientists and academic institutions experience this as a necessary Rope for rigorous inquiry, coordinating a shared epistemic framework. Non-credentialed interpreters, indigenous knowledge holders, and religious communities experience it as a Snare, actively excluding their perspectives and extracting epistemic authority and resources. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed scientists and academic institutions are primary beneficiaries (d=0.0-0.2) as they control the production and validation of knowledge within this framework, securing funding and prestige. Non-credentialed interpreters, indigenous knowledge holders, and religious communities are victims (d=0.8-1.0) as their knowledge systems are marginalized or actively suppressed, and they bear the cost of exclusion from mainstream discourse and resources. The constraint subsidizes the scientific establishment by granting it exclusive epistemic authority over human origins.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to provide a coherent, evidence-based account of human origins. While this core function remains live, the 'mandatrophy' arises from the expansion of its scope to actively suppress alternative, non-scientific accounts, rather than merely offering a competing one. This prevents mislabeling it as a pure Rope, as the coordination function is intertwined with significant, asymmetric extraction and suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalism_vs_other_readings,
    'Is the exclusion of non-materialist explanations a necessary epistemic boundary for scientific progress, or a disciplinary gatekeeping mechanism that extracts from alternative knowledge systems?',
    'Analysis of scientific breakthroughs achieved by integrating non-materialist perspectives (if any), or by demonstrating equivalent explanatory power from alternative frameworks without materialist assumptions.',
    'If necessary, the constraint is a Rope for scientific inquiry; if gatekeeping, it is a Snare for excluded knowledge systems. This constraint is one reading of the ''anthropological_record'' kernel, specifically the ''naturalist_reading''. Sibling readings (''creationist_reading'', ''indigenous_epistemology_reading'') would change the victim set and the claimed epistemic authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalism_vs_other_readings, conceptual, 'Ambiguity between epistemic necessity and disciplinary extraction in the naturalist reading of human origins.').

omega_variable(
    suppression_of_alternative_epistemologies,
    'To what extent is the suppression of non-scientific origin narratives a structural consequence of the scientific method''s success, versus an active, institutionalized effort to delegitimize alternative epistemologies?',
    'Comparative study of funding allocations, publication biases, and academic hiring practices across institutions that explicitly promote or suppress non-naturalist origin narratives.',
    'If primarily structural, the suppression is an unavoidable byproduct of a successful knowledge system. If actively institutionalized, it indicates a higher degree of extractiveness and a more explicit Snare-like function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_epistemologies, empirical, 'Distinguishing between structural and active suppression of alternative origin narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anth_tr_t10, anthropological_record__naturalist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__naturalist_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(anth_tr_t30, anthropological_record__naturalist_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(anth_be_t10, anthropological_record__naturalist_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(anth_be_t20, anthropological_record__naturalist_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(anth_be_t30, anthropological_record__naturalist_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(anth_su_t10, anthropological_record__naturalist_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(anth_su_t20, anthropological_record__naturalist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(anth_su_t30, anthropological_record__naturalist_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is the 'naturalist_reading' of the 'anthropological_record' kernel. It is one of three distinct readings, each with its own structural properties and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
