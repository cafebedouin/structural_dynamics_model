% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus: Abolitionist Rejection Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the 'abolitionist rejection' reading of the
 *   Dharmasastra corpus, which views the entire textual framework and its
 *   associated caste system as fundamentally oppressive and without
 *   legitimate authority. From this perspective, the constraint operates as a
 *   snare, extracting from Dalits, lower castes, and women, and requiring
 *   active suppression to maintain its hierarchical structure. The
 *   abolitionist reading demands the complete dismantling of the system,
 *   rather than reinterpretation or reform. This is one reading of the
 *   'dharmasastra_corpus' kernel, with sibling readings 'orthodox_literalist'
 *   and 'reformist_contextual'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.95).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.98).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.95).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus: Abolitionist Rejection Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '7a4caddd-3864-41b6-9aa8-28f3ea0d3c29').
narrative_ontology:cs_kernel_codification('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', fixed_text).
narrative_ontology:cs_authority_grounding('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', extraction).
narrative_ontology:cs_interpretation_layer_present('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29').
narrative_ontology:cs_reading_relation('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', foundational, dharmasastra_inherently_oppressive).
narrative_ontology:cs_axiom_status(dharmasastra_inherently_oppressive, holdable).
narrative_ontology:cs_axiom_grounding('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', dharmasastra_inherently_oppressive, deontological).
narrative_ontology:cs_axiom('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', foundational, caste_system_must_be_abolished).
narrative_ontology:cs_axiom_status(caste_system_must_be_abolished, holdable).
narrative_ontology:cs_axiom_grounding('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', caste_system_must_be_abolished, deontological).
narrative_ontology:cs_reference_frame('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', pre_caste_egalitarian_society).
narrative_ontology:cs_drift_state('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', contemporary_abolitionist_movement, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('7a4caddd-3864-41b6-9aa8-28f3ea0d3c29', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, lower_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, religious_minorities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.95) and suppression (0.98) are extremely high because this reading identifies the Dharmasastra corpus as the foundational text for the caste system, which is inherently extractive and maintained through severe social, economic, and ritual coercion. Accessibility collapse (0.9) is high because the system is deeply embedded in social structures, making alternatives almost impossible for those trapped within it. Resistance (0.85) is also high, reflecting centuries of struggle against caste oppression. Theater ratio (0.05) is low because, from this perspective, the system's function is overtly oppressive, with minimal performative cover for genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally rejects the legitimacy of the Dharmasastra corpus, seeing it as a tool of oppression. This contrasts sharply with orthodox readings that view it as sacred and beneficial, and reformist readings that seek to extract an ethical core. The engine's classification as a snare from this perspective highlights the structural violence inherent in the system, which is denied or rationalized by other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalits, lower castes, and women are the primary targets (payers) of this constraint, bearing its full extractive force. Orthodox pundits and upper castes, while not explicitly listed as beneficiaries in this 'rejection' reading (as the constraint itself is seen as illegitimate), are implicitly the historical beneficiaries whose power is maintained by the system. Abolitionist activists are observers, seeking to dismantle the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   From the abolitionist perspective, the Dharmasastra corpus never had a legitimate mandate beyond establishing and maintaining an oppressive hierarchy. Therefore, it is not a case of mandatrophy (function atrophying) but rather a recognition of its inherent extractive nature from its inception. The classification as a snare prevents mislabeling it as a degraded coordination mechanism or a temporary support structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''abolitionist rejection'' of the Dharmasastra corpus, or does it implicitly retain elements of the textual framework it purports to reject?',
    'Analysis of proposed alternative frameworks and their practical implementation: if they draw on any Dharmic concepts or structures, the rejection is partial.',
    'If elements are implicitly retained, the extractiveness and suppression might be slightly lower, and the classification might shift towards a more complex ''tangled_rope'' if some coordination function is inadvertently preserved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity in the completeness of the rejection of the Dharmasastra corpus.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (economic dependency, legal barriers) or internalized (belief in karma, social pressure, identity fusion with caste)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., individuals migrating to contexts without overt caste discrimination still experience internalized caste identity or trauma), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true liberation more difficult.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the caste system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1000, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(dhar_tr_t1500, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(dhar_tr_t1800, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1800, 0.07).
narrative_ontology:measurement(dhar_tr_t1900, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1900, 0.06).
narrative_ontology:measurement(dhar_tr_t2024, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1000, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1000, 0.9).
narrative_ontology:measurement(dhar_be_t1500, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1500, 0.92).
narrative_ontology:measurement(dhar_be_t1800, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1800, 0.93).
narrative_ontology:measurement(dhar_be_t1900, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1900, 0.94).
narrative_ontology:measurement(dhar_be_t2024, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1000, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1000, 0.9).
narrative_ontology:measurement(dhar_su_t1500, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1500, 0.93).
narrative_ontology:measurement(dhar_su_t1800, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1800, 0.95).
narrative_ontology:measurement(dhar_su_t1900, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1900, 0.97).
narrative_ontology:measurement(dhar_su_t2024, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
