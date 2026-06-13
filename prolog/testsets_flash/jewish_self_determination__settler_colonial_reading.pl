% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionism as a Settler-Colonial Project (Palestinian Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint models Zionism as a European settler-colonial project,
 *   focusing on the systematic dispossession of indigenous Palestinians
 *   through violence and legal exclusion. This reading emphasizes the
 *   structural mechanisms that facilitate the transfer of land and resources
 *   to Jewish settlers and the Israeli state, while simultaneously
 *   suppressing Palestinian resistance and denying their right of return. The
 *   constraint's persistence relies heavily on active enforcement and the
 *   suppression of alternatives for the Palestinian population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.9).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.95).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as a Settler-Colonial Project (Palestinian Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, 'f72a4b1e-da22-4fd0-af06-6da565ff1a09').
narrative_ontology:cs_kernel_codification('f72a4b1e-da22-4fd0-af06-6da565ff1a09', formalized).
narrative_ontology:cs_authority_grounding('f72a4b1e-da22-4fd0-af06-6da565ff1a09', extraction).
narrative_ontology:cs_interpretation_layer_present('f72a4b1e-da22-4fd0-af06-6da565ff1a09').
narrative_ontology:cs_reading_relation('f72a4b1e-da22-4fd0-af06-6da565ff1a09', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f72a4b1e-da22-4fd0-af06-6da565ff1a09', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('f72a4b1e-da22-4fd0-af06-6da565ff1a09', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('f72a4b1e-da22-4fd0-af06-6da565ff1a09', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('f72a4b1e-da22-4fd0-af06-6da565ff1a09', foundational, zionism_is_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_is_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('f72a4b1e-da22-4fd0-af06-6da565ff1a09', zionism_is_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('f72a4b1e-da22-4fd0-af06-6da565ff1a09', foundational, indigenous_rights_are_paramount).
narrative_ontology:cs_axiom_status(indigenous_rights_are_paramount, holdable).
narrative_ontology:cs_axiom_grounding('f72a4b1e-da22-4fd0-af06-6da565ff1a09', indigenous_rights_are_paramount, deontological).
narrative_ontology:cs_reference_frame('f72a4b1e-da22-4fd0-af06-6da565ff1a09', european_colonial_expansion).
narrative_ontology:cs_drift_state('f72a4b1e-da22-4fd0-af06-6da565ff1a09', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f72a4b1e-da22-4fd0-af06-6da565ff1a09', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, displaced_palestinians).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.9) reflects the ongoing transfer of land, resources, and sovereignty from Palestinians to the Israeli state and Jewish settlers. Suppression (0.95) is extremely high due to military occupation, legal discrimination (e.g., differential citizenship rights, land laws), and restrictions on movement and political organization. The low theater ratio (0.1) indicates that the project's stated goals (security, self-determination) are largely a cover for the primary function of territorial expansion and demographic control. Accessibility collapse (0.8) is high as alternatives for Palestinians (e.g., independent statehood, right of return) are systematically foreclosed. Resistance (0.7) is also high, reflecting continuous Palestinian struggle against dispossession.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Palestinian Arabs, this constraint is a clear snare, designed for their dispossession. From the perspective of European Jewish settlers and the Israeli state, it is framed as a legitimate act of self-determination or indigenous return, often obscuring the extractive and suppressive elements. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   European Jewish settlers and the Israeli state are the primary beneficiaries (d=0.0-0.1), as they gain land, resources, and political control. Palestinian Arabs and displaced Palestinians are the primary victims (d=0.9-1.0), bearing the costs of dispossession, occupation, and legal exclusion. International bodies and human rights organizations act as observers, often documenting the extractive and suppressive aspects.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the original mandate of Zionism, if framed as a response to antisemitism and a quest for Jewish self-determination, has been co-opted or transformed into a settler-colonial project. The persistence of the constraint is not due to an unmet original need for Jewish self-determination in a non-extractive form, but rather due to the ongoing benefits of extraction for the beneficiaries and the active suppression of victims. The 'snare' classification prevents mislabeling this as a legitimate coordination mechanism for Jewish self-determination, instead highlighting its extractive core from the Palestinian perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine settler-colonial project, or is it better understood through a different reading of Jewish self-determination?',
    'Historical analysis of land acquisition, legal frameworks (e.g., Law of Return vs. Palestinian refugee rights), and demographic changes, focusing on power asymmetries and dispossession mechanisms.',
    'If confirmed as settler-colonial, the classification as ''snare'' is robust. If a different reading (e.g., indigenous return) gains explanatory power, the constraint''s classification would shift towards ''tangled_rope'' or even ''rope'' from a different perspective, with different beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''jewish_self_determination'' kernel, specifically the ''settler_colonial_reading''. Sibling readings include liberal_nationalist_reading, indigenous_return_reading, religious_covenant_reading, and diasporist_reading, each with different structural implications.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/military) or internalized (psychological impact of occupation)?',
    'Post-occupation trajectory: if suppression persists after military/legal mechanisms are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Palestinian population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__settler_colonial_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__settler_colonial_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__settler_colonial_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__settler_colonial_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__settler_colonial_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__settler_colonial_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__settler_colonial_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__settler_colonial_reading, base_extractiveness, 30, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__settler_colonial_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__settler_colonial_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__settler_colonial_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__settler_colonial_reading, suppression_requirement, 30, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_self_determination' kernel, focusing on its settler-colonial aspects. Other readings (liberal_nationalist_reading, indigenous_return_reading, religious_covenant_reading, diasporist_reading) offer alternative structural interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
