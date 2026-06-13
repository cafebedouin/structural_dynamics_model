% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis (Settler-Colonial Reading)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint models Zionism as a European settler-colonial movement,
 *   focusing on its structural role in establishing an ethno-state through
 *   the displacement and subjugation of the indigenous Palestinian
 *   population. The core claim is that the colonial structure is constitutive
 *   of the state's legitimacy, rather than an incidental outcome. This
 *   reading emphasizes the ongoing nature of indigenous dispossession and the
 *   active enforcement required to maintain the existing power asymmetry.
 *
 * KEY AGENTS:
 *   - israeli_state_institutions: Agenda-setter (institutional/constrained) — enforces policies of displacement and control.
 *   - jewish_israeli_citizens: Beneficiary (organized/constrained) — benefits from preferential access and ethno-national identity.
 *   - palestinian_indigenous_population: Payer (powerless/trapped) — bears costs of displacement, dispossession, and military occupation.
 *   - international_law_frameworks: Victim (institutional/analytical) — undermined by settler-colonial practices.
 *   - anti_colonial_movements: Observer (organized/mobile) — analyze and challenge the settler-colonial framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.9).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.95).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis (Settler-Colonial Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'ac77cc1a-9d32-4661-8f82-e9b356fd4b11').
narrative_ontology:cs_kernel_codification('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', formalized).
narrative_ontology:cs_authority_grounding('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', extraction).
narrative_ontology:cs_interpretation_layer_present('ac77cc1a-9d32-4661-8f82-e9b356fd4b11').
narrative_ontology:cs_reading_relation('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', zionist_legitimacy_basis__religious_restoration_reading, forecloses).
narrative_ontology:cs_axiom('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', foundational, settler_colonialism_inherently_illegitimate).
narrative_ontology:cs_axiom_status(settler_colonialism_inherently_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', settler_colonialism_inherently_illegitimate, deontological).
narrative_ontology:cs_axiom('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', foundational, indigenous_displacement_is_constitutive).
narrative_ontology:cs_axiom_status(indigenous_displacement_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', indigenous_displacement_is_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', anti_colonial_decolonization_framework).
narrative_ontology:cs_drift_state('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ac77cc1a-9d32-4661-8f82-e9b356fd4b11', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizens).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, international_law_frameworks).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because the entire state structure is built upon the systematic transfer of land and resources from one population to another. Suppression is also very high (0.95) due to the active military occupation, legal discrimination, and denial of rights necessary to maintain this structure. Theater ratio is moderate (0.6) as the justifications for state actions (e.g., security, historical right) increasingly serve to mask the underlying extractive and suppressive mechanisms. Accessibility collapse is high (0.8) for the indigenous population, as alternatives to their current subjugated status are systematically foreclosed. Resistance is high (0.9) reflecting the ongoing struggle of the Palestinian people against this framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli state and its beneficiaries, the constraint is framed as a national liberation movement or religious restoration. From the perspective of the Palestinian indigenous population and anti-colonial movements, it is experienced as a highly extractive and suppressive settler-colonial project. The engine's classification will highlight this divergence by computing a Snare for the payer seats, contrasting with a claimed Rope or Mountain by the agenda-setter.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions and Jewish Israeli citizens are clear beneficiaries, deriving power and resources from the constraint's operation. The Palestinian indigenous population is the primary target, experiencing direct and severe extraction and suppression. International law frameworks are victims, as their principles are systematically violated. Anti-colonial movements act as analytical observers, exposing the structural dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The settler-colonial reading argues that the original mandate (Jewish self-determination) has been superseded by an extractive function (indigenous displacement and control). The constraint persists not because the initial problem is still being solved in a just manner, but because the beneficiaries actively maintain the extractive structure. The high extractiveness and suppression, coupled with the contested founding problem status, indicate a deep mandatrophy where the original coordination narrative serves as cover for ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is Zionism best understood as a settler-colonial movement, a national liberation movement, or a religious restoration process?',
    'Historical and sociological analysis focusing on land acquisition patterns, indigenous displacement, and the role of external colonial powers, as well as the self-identification and political agency of the affected populations.',
    'If the settler-colonial reading is affirmed, the constraint''s legitimacy is fundamentally challenged, leading to calls for decolonization and reparations. If other readings prevail, the classification would shift towards a Rope (national liberation) or Mountain (religious restoration), with significantly lower extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''zionist_legitimacy_basis'' kernel. This reading, ''settler_colonial_reading'', emphasizes the colonial structure and indigenous displacement as constitutive. Sibling readings (''national_liberation_reading'', ''religious_restoration_reading'') offer alternative framings.').

omega_variable(
    displacement_constitutive_vs_incidental,
    'Is the displacement of the indigenous population an incidental outcome of Zionist state-building, or is it a constitutive element of the settler-colonial project?',
    'Analysis of founding documents, historical policies, and demographic engineering efforts. If displacement was a planned and systematic feature, it is constitutive; if it was an unintended side-effect, it is incidental.',
    'If constitutive, the high extractiveness and suppression are inherent to the constraint''s operation. If incidental, the constraint might be reformable to reduce extraction without dismantling its core function, potentially shifting towards a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(displacement_constitutive_vs_incidental, empirical, 'Examines whether indigenous displacement is a core feature or a side effect of the Zionist project.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.4).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.5).
narrative_ontology:measurement(zion_tr_t1987, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1987, 0.55).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2000, 0.58).
narrative_ontology:measurement(zion_tr_t2014, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2014, 0.6).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.8).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(zion_be_t1987, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1987, 0.88).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2000, 0.89).
narrative_ontology:measurement(zion_be_t2014, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2014, 0.9).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(zion_su_t1987, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1987, 0.92).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2000, 0.93).
narrative_ontology:measurement(zion_su_t2014, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2014, 0.94).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
