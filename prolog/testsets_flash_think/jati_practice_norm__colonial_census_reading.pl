% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Jati Categories Reified by Colonial Census
 *   domain: social_anthropology/political_economy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.65).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.75).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Jati Categories Reified by Colonial Census").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, 'c28be6c8-8bcc-4d4c-98ef-7524a129a79c').
narrative_ontology:cs_kernel_codification('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', formalized).
narrative_ontology:cs_authority_grounding('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', extraction).
narrative_ontology:cs_interpretation_layer_present('c28be6c8-8bcc-4d4c-98ef-7524a129a79c').
narrative_ontology:cs_reading_relation('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_axiom('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', foundational, social_categories_are_fixed_and_legible).
narrative_ontology:cs_axiom_status(social_categories_are_fixed_and_legible, holdable).
narrative_ontology:cs_axiom_grounding('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', social_categories_are_fixed_and_legible, conventional).
narrative_ontology:cs_axiom('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', foundational, administrative_efficiency_justifies_classification).
narrative_ontology:cs_axiom_status(administrative_efficiency_justifies_classification, holdable).
narrative_ontology:cs_axiom_grounding('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', administrative_efficiency_justifies_classification, instrumental).
narrative_ontology:cs_reference_frame('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', colonial_administrative_legibility).
narrative_ontology:cs_drift_state('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', post_colonial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c28be6c8-8bcc-4d4c-98ef-7524a129a79c', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrators).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, post_colonial_governments).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, indigenous_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, local_jati_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, anthropologists_colonial_era).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposed and enforced a rigid system of jati classification through census operations and administrative decrees to simplify governance, taxation, and resource allocation. Benefited from the legibility and control this system provided.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).

% Were subjected to external categorization that often did not align with their fluid, locally negotiated social identities. Lost autonomy over self-definition and faced new forms of social stratification and discrimination based on these imposed categories.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, indigenous_communities, payer,
    powerless, generational, trapped, local).

% Had their traditional authority over social classification undermined by the colonial state. Were forced to operate within or adapt to the rigid, externally defined categories, often losing influence or seeing their communities fractured.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, local_jati_leaders, payer,
    moderate, biographical, constrained, local).

% Benefited from the 'legibility' of standardized jati categories for their research, even if these categories were artificial and distorted local realities. Their work often inadvertently reinforced the colonial administrative framework.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, anthropologists_colonial_era, beneficiary,
    analytical, biographical, analytical, global).

% Inherited the administrative apparatus and the reified jati categories from the colonial state. Often continued to use these categories for census, affirmative action, and other governance purposes, finding them administratively convenient despite their problematic origins.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, post_colonial_governments, beneficiary,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a standardized, legible system for colonial administrators to categorize and govern diverse indigenous populations, facilitating taxation, land management, and resource allocation across vast territories.
% TRANSFER_FUNCTION: Transferred administrative power and control over social classification from indigenous communities and local leaders to the colonial state, extracting social and political autonomy and imposing a new, rigid social order.
% ABSENT_VOICES: Indigenous scholars, community elders, and local practitioners who would articulate the fluidity, context-dependence, and internal logic of traditional jati categories. Their perspectives were systematically excluded in favor of a simplified, externally imposed framework.
% DISAPPEARANCE_RATIONALE: If the colonial reification of jati categories and its administrative enforcement vanished overnight, the administrative, social, and political structures built upon these rigid classifications would need to be fundamentally re-evaluated. This would likely lead to a return to more fluid, locally negotiated social identities and a significant reorganization of governance mechanisms that rely on these categories.
% FOUNDING_PROBLEM: Colonial powers faced significant challenges in administering diverse, complex, and fluid indigenous social structures, requiring a simplified, standardized system for efficient governance, taxation, and resource management.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial historians, social anthropologists, and political scientists widely corroborate that the original administrative problem was a colonial construct. While the categories persist, their continued use by post-colonial governments is often attributed to inherited administrative convenience and political inertia, rather than a genuine, live social problem requiring such rigid classification. Independent academic research from outside benefiting parties supports this shifted-function reading.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_categories,
    'To what extent were the jati categories reified by the colonial census genuinely reflective of pre-existing social divisions, versus being administrative constructs imposed for governance?',
    'Comparative historical and anthropological research analyzing pre-colonial records, oral traditions, and local practices against colonial census data to identify discrepancies and points of imposition.',
    'If primarily administrative constructs, the extractiveness and suppression are higher, as the constraint actively created the social reality it then governed. If largely reflective, the constraint''s extractiveness is lower, acting more as a formalization than an imposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_categories, empirical, 'Ambiguity between inherent social structure and colonial administrative invention.').

omega_variable(
    internal_vs_external_classification,
    'What is the long-term impact of externally imposed social classification on internal community cohesion and self-identification, compared to internally negotiated and fluid categories?',
    'Longitudinal ethnographic studies of communities where colonial categories were imposed, assessing changes in social mobility, inter-group relations, and identity formation over generations, compared to communities with more autonomous classification systems.',
    'If external classification significantly eroded internal cohesion and autonomy, the constraint''s effective suppression and long-term extractiveness are higher, indicating a deeper, more pervasive impact beyond direct administrative control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_external_classification, empirical, 'Impact of external classification on internal social dynamics.').

omega_variable(
    framing_underdetermination_jati_kernel,
    'Is the ''colonial_census_reading'' the most appropriate framing for the jati_practice_norm kernel, or would an alternative framing (e.g., focusing on post-colonial inheritance) yield a different classification pattern?',
    'Comparative analysis of constraint stories generated from alternative framings of the jati_practice_norm kernel (e.g., ''post_colonial_administrative_inertia_reading'') to identify divergences in cs_pattern and classification. The current framing emphasizes the colonial imposition.',
    'An alternative framing might shift the primary beneficiaries/victims, alter the perceived authority_grounding, or change the drift_state, potentially leading to a different cs_pattern and classification (e.g., a Piton if the original mandate is entirely dead and maintenance is purely inertial).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination_jati_kernel, conceptual, 'Framing choice for the jati_practice_norm kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 1870, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1870, jati_practice_norm__colonial_census_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement(jati_tr_t1885, jati_practice_norm__colonial_census_reading, theater_ratio, 1885, 0.18).
narrative_ontology:measurement(jati_tr_t1900, jati_practice_norm__colonial_census_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(jati_tr_t1915, jati_practice_norm__colonial_census_reading, theater_ratio, 1915, 0.22).
narrative_ontology:measurement(jati_tr_t1930, jati_practice_norm__colonial_census_reading, theater_ratio, 1930, 0.24).
narrative_ontology:measurement(jati_tr_t1947, jati_practice_norm__colonial_census_reading, theater_ratio, 1947, 0.25).

% Extraction over time
narrative_ontology:measurement(jati_be_t1870, jati_practice_norm__colonial_census_reading, base_extractiveness, 1870, 0.5).
narrative_ontology:measurement(jati_be_t1885, jati_practice_norm__colonial_census_reading, base_extractiveness, 1885, 0.55).
narrative_ontology:measurement(jati_be_t1900, jati_practice_norm__colonial_census_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(jati_be_t1915, jati_practice_norm__colonial_census_reading, base_extractiveness, 1915, 0.63).
narrative_ontology:measurement(jati_be_t1930, jati_practice_norm__colonial_census_reading, base_extractiveness, 1930, 0.64).
narrative_ontology:measurement(jati_be_t1947, jati_practice_norm__colonial_census_reading, base_extractiveness, 1947, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1870, jati_practice_norm__colonial_census_reading, suppression_requirement, 1870, 0.6).
narrative_ontology:measurement(jati_su_t1885, jati_practice_norm__colonial_census_reading, suppression_requirement, 1885, 0.65).
narrative_ontology:measurement(jati_su_t1900, jati_practice_norm__colonial_census_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(jati_su_t1915, jati_practice_norm__colonial_census_reading, suppression_requirement, 1915, 0.72).
narrative_ontology:measurement(jati_su_t1930, jati_practice_norm__colonial_census_reading, suppression_requirement, 1930, 0.74).
narrative_ontology:measurement(jati_su_t1947, jati_practice_norm__colonial_census_reading, suppression_requirement, 1947, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
