% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy: Indigenous Continuity Reading (1948 as Nakba)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint defines territorial legitimacy through the lens of
 *   continuous indigenous habitation and anti-colonial self-determination,
 *   framing the 1948 establishment of Israel as the Nakba (catastrophe)
 *   rather than a legitimate partition. It structurally asserts Palestinian
 *   sovereignty over all of historic Palestine and views the Israeli state as
 *   an illegitimate settler-colonial entity, making the right of return for
 *   1948 refugees a central and non-negotiable claim. The high extractiveness
 *   and suppression reflect the ongoing dispossession and violent enforcement
 *   required to maintain the counter-narrative and physical control by the
 *   Israeli state, which this reading fundamentally challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.95).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.98).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.99).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy: Indigenous Continuity Reading (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '37530d86-cc8a-4247-be72-015ba7b80932').
narrative_ontology:cs_kernel_codification('37530d86-cc8a-4247-be72-015ba7b80932', distributed).
narrative_ontology:cs_authority_grounding('37530d86-cc8a-4247-be72-015ba7b80932', distributed).
narrative_ontology:cs_reading_relation('37530d86-cc8a-4247-be72-015ba7b80932', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('37530d86-cc8a-4247-be72-015ba7b80932', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('37530d86-cc8a-4247-be72-015ba7b80932', foundational, indigenous_sovereignty_is_inalienable).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('37530d86-cc8a-4247-be72-015ba7b80932', indigenous_sovereignty_is_inalienable, deontological).
narrative_ontology:cs_axiom('37530d86-cc8a-4247-be72-015ba7b80932', foundational, id_1948_nakba_is_foundational_dispossession).
narrative_ontology:cs_axiom_status(id_1948_nakba_is_foundational_dispossession, holdable).
narrative_ontology:cs_axiom_grounding('37530d86-cc8a-4247-be72-015ba7b80932', id_1948_nakba_is_foundational_dispossession, empirically_contingent).
narrative_ontology:cs_reference_frame('37530d86-cc8a-4247-be72-015ba7b80932', pre_nakba_palestinian_sovereignty).
narrative_ontology:cs_drift_state('37530d86-cc8a-4247-be72-015ba7b80932', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('37530d86-cc8a-4247-be72-015ba7b80932', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_diaspora).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_resistance_movements).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_state_institutions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.95) is extremely high because this reading asserts a complete dispossession of indigenous rights and land, with the Israeli state's existence itself being the primary mechanism of extraction. Suppression (0.98) is also extremely high, reflecting the military, political, and discursive force required to maintain the status quo against this claim. Theater ratio is low (0.1) because the struggle is existential and direct, with little performative maintenance; the claims are actively contested and enforced. Resistance is near maximal (0.99) as this reading is the foundation for continuous Palestinian resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Palestinian beneficiaries, this reading is a fundamental truth and a call to justice, defining their identity and political project. From the perspective of Israeli payers, it is an existential threat that denies their right to self-determination and security, rendering their state illegitimate. The engine's classification will highlight this profound divergence, showing a snare from the Israeli seat and a foundational claim from the Palestinian seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian diaspora and resistance movements are the primary beneficiaries (d near 0.0) as this reading provides the moral and political foundation for their claims and actions. Israeli settlers and state institutions are the primary targets/victims (d near 1.0) as their legitimacy and existence are fundamentally challenged. International solidarity movements and critical international law scholars are observers who align with this reading, amplifying its claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_objectivity,
    'To what extent can historical narratives of indigenous continuity and colonial dispossession be objectively established, independent of political claims?',
    'Comprehensive, multi-disciplinary historical and archaeological research, peer-reviewed and accepted across diverse academic traditions, that establishes patterns of habitation and displacement prior to 1948.',
    'If a robust, objective historical narrative strongly supports continuous indigenous Palestinian habitation and colonial dispossession, it strengthens this reading''s empirical grounding. If historical evidence is more ambiguous or contested, it highlights the conceptual/interpretive nature of the claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_narrative_objectivity, empirical, 'The empirical basis for the indigenous continuity claim.').

omega_variable(
    self_determination_scope,
    'Does the principle of anti-colonial self-determination inherently imply sovereignty over the entirety of historic Palestine, or can it be reconciled with a partitioned or shared sovereignty?',
    'Legal and philosophical analysis of self-determination in post-colonial contexts, particularly cases involving multiple groups with historical claims to the same territory, and the evolution of international legal norms regarding partition vs. unitary states.',
    'If self-determination is found to be inherently indivisible and applicable to the entire territory, this reading''s maximalist claims are strengthened. If it can accommodate other arrangements, it opens pathways for alternative resolutions that this reading currently forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_determination_scope, conceptual, 'The scope and indivisibility of anti-colonial self-determination.').

omega_variable(
    right_of_return_feasibility,
    'What are the practical and demographic implications of implementing a full right of return for 1948 refugees, and how does this impact the viability of any future state structures?',
    'Demographic studies, urban planning analyses, and economic impact assessments of large-scale population movements, coupled with political feasibility studies regarding the integration of returning populations into a new state structure.',
    'If implementation is deemed practically impossible or destabilizing without fundamental demographic shifts, it challenges the ''holdable'' status of the right of return as a foundational axiom within this reading. If feasible, it strengthens the reading''s actionable claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_feasibility, empirical, 'Practical feasibility of the right of return.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.9).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.92).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1987, 0.93).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.94).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2014, 0.95).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.9).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.95).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1987, 0.96).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.97).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2014, 0.98).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, international_humanitarian_law__occupation_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, right_of_return__un_resolution_194_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy' kernel. It defines legitimacy through indigenous continuity and anti-colonial self-determination, framing 1948 as the Nakba. It directly challenges the 'partition_reading' and 'security_necessity_reading' of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
