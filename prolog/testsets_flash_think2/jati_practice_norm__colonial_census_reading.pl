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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Colonial Administrative Reification of Jati Categories
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint describes the process by which colonial administrations
 *   formalized and rigidified previously fluid jati (caste/community)
 *   categories for administrative purposes, such as census-taking, taxation,
 *   and governance. This 'colonial_census_reading' of jati practices
 *   transformed dynamic social norms into fixed, enumerable units, benefiting
 *   colonial efficiency and certain aligned local elites, while imposing
 *   significant costs on local communities and suppressing indigenous forms
 *   of social organization. The constraint is claimed as a Tangled Rope
 *   because it presented a coordination function (administrative legibility)
 *   but simultaneously extracted resources and control through asymmetric
 *   enforcement.
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
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Colonial Administrative Reification of Jati Categories").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '25b0910d-92b3-43d1-9161-7b763bd5b1aa').
narrative_ontology:cs_kernel_codification('25b0910d-92b3-43d1-9161-7b763bd5b1aa', formalized).
narrative_ontology:cs_authority_grounding('25b0910d-92b3-43d1-9161-7b763bd5b1aa', extraction).
narrative_ontology:cs_interpretation_layer_present('25b0910d-92b3-43d1-9161-7b763bd5b1aa').
narrative_ontology:cs_reading_relation('25b0910d-92b3-43d1-9161-7b763bd5b1aa', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_reading_relation('25b0910d-92b3-43d1-9161-7b763bd5b1aa', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_axiom('25b0910d-92b3-43d1-9161-7b763bd5b1aa', foundational, administrative_legibility_is_paramount).
narrative_ontology:cs_axiom_status(administrative_legibility_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('25b0910d-92b3-43d1-9161-7b763bd5b1aa', administrative_legibility_is_paramount, conventional).
narrative_ontology:cs_axiom('25b0910d-92b3-43d1-9161-7b763bd5b1aa', foundational, jati_categories_are_fixed_administrative_units).
narrative_ontology:cs_axiom_status(jati_categories_are_fixed_administrative_units, holdable).
narrative_ontology:cs_axiom_grounding('25b0910d-92b3-43d1-9161-7b763bd5b1aa', jati_categories_are_fixed_administrative_units, conventional).
narrative_ontology:cs_reference_frame('25b0910d-92b3-43d1-9161-7b763bd5b1aa', colonial_administrative_legibility_framework).
narrative_ontology:cs_drift_state('25b0910d-92b3-43d1-9161-7b763bd5b1aa', post_independence_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('25b0910d-92b3-43d1-9161-7b763bd5b1aa', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, certain_local_elites).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, local_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, fluid_jati_practices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The external governing power that imposed a rigid, enumerable system of jati classification for administrative convenience, taxation, and resource allocation. Benefits from the legibility and control this system provides.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Populations whose fluid, context-dependent social categories were forcibly reified and fixed by the colonial census and administrative practices. They bear the cost of reduced autonomy, internal social friction, and being subjected to external definitions.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, local_communities, payer,
    powerless, biographical, trapped, local).

% Individuals or groups who gained power, status, or resources by aligning with the colonial administrative system, often by accepting and enforcing the reified jati categories, thereby solidifying their own position within the new hierarchy.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, certain_local_elites, beneficiary,
    powerful, biographical, mobile, regional).

% The pre-colonial, dynamic, and context-sensitive ways in which jati categories were understood and negotiated within communities. These practices were delegitimized and suppressed by the colonial administrative framework, though they might have persisted informally.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, fluid_jati_practices, excluded,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_non_agent(jati_practice_norm__colonial_census_reading, fluid_jati_practices).

% Academics and intellectuals, often from the colonized region, who critically analyze the historical impact of colonial administrative practices on social structures, documenting the shift from fluid to reified categories and its long-term consequences.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, indigenous_scholars, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To create a standardized, enumerable system for classifying populations, facilitating colonial administration, taxation, and resource allocation across diverse social landscapes.
% TRANSFER_FUNCTION: Transfers administrative power and control over social definition from local communities to the colonial state, and resources (e.g., taxes, labor, land) from categorized populations to the state. It also transfers legitimacy and power to certain local elites who align with the colonial system.
% ABSENT_VOICES: Local leaders, elders, and practitioners who understood and maintained the fluid, context-dependent nature of jati categories were systematically excluded from the colonial administrative process. Their nuanced understandings were replaced by rigid, externally imposed definitions.
% DISAPPEARANCE_RATIONALE: If the colonial administrative reification of jati categories vanished overnight, the post-colonial state would lose a foundational, albeit problematic, framework for governance and identity. Social structures would likely revert to more fluid, context-dependent forms of self-organization, and the state would need to find new bases for administrative legibility, potentially leading to significant social and political reorganization.
% FOUNDING_PROBLEM: The colonial administration faced challenges in governing a diverse and complex society with fluid social structures, requiring a simplified, legible system for control, resource extraction, and census-taking.
% FOUNDING_PROBLEM_CORROBORATION: Historians and post-colonial scholars widely corroborate that the administrative need for legibility was the primary driver for this reification. While the colonial problem is dead, the reified categories persist and continue to influence contemporary social structures, as attested by sociological studies and government reports in post-colonial nations.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-high because the system diverted resources and autonomy from local communities to the colonial state and its allies. Suppression (0.75) is high due to the active enforcement mechanisms (census, legal codes, administrative decrees) used to impose and maintain these rigid categories, actively suppressing fluid alternatives. The theater ratio (0.25) is moderate-low; while the administrative function was real, a significant portion of the effort was performative, designed to legitimize colonial rule and maintain control rather than genuinely reflect or serve local social realities. Accessibility collapse (0.60) is moderate, as formal alternatives were largely eliminated, but informal practices might have persisted. Resistance (0.50) is moderate, reflecting ongoing, often localized, challenges to the imposed system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the colonial administration, this was a necessary coordination mechanism for effective governance. From the perspective of local communities, it was an extractive imposition that distorted their social fabric. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial_administration is the primary beneficiary and agenda-setter, gaining administrative control and resources. Certain_local_elites also benefit by aligning with the colonial power structure. Local_communities are the primary victims, bearing the costs of imposed rigidity and loss of autonomy. Fluid_jati_practices, as a non-agent entity, are structurally excluded and suppressed. Indigenous_scholars act as analytical observers, documenting the historical and social impacts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_imposition_vs_indigenous_structure,
    'To what extent did the colonial administrative categories genuinely reflect pre-existing indigenous social structures, versus imposing an entirely new, rigid framework?',
    'Detailed historical and ethnographic studies comparing pre-colonial social organization with colonial census data and administrative records.',
    'If categories largely reflected indigenous structures, the extractiveness and suppression would be lower (closer to a Rope); if largely imposed, the current high extractiveness and suppression are more accurate (Tangled Rope/Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colonial_imposition_vs_indigenous_structure, empirical, 'Ambiguity regarding the origin and ''naturalness'' of the reified jati categories.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of fluid jati practices structural (colonial law, administrative enforcement) or internalized (communities adopting rigid categories as their own after generations of imposition)?',
    'Post-colonial studies examining the persistence of rigid categories after the removal of direct colonial enforcement. If rigidity persists without external coercion, it suggests internalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the affected communities carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for fluid social practices.').

omega_variable(
    localized_practice_reading_delta,
    'How would the ''localized_practice_reading'' of jati categories structurally differ from this ''colonial_census_reading''?',
    'Authoring a separate constraint story for the ''localized_practice_reading'' with its own metrics and stakeholders.',
    'The ''localized_practice_reading'' would likely classify jati as a Rope or Scaffold, with significantly lower extractiveness and suppression, emphasizing coordination and fluidity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(localized_practice_reading_delta, conceptual, 'Structural differences between the colonial administrative reading and the localized practice reading of jati norms.').

omega_variable(
    orthodox_textual_reading_delta,
    'How would the ''orthodox_textual_reading'' of jati categories structurally differ from this ''colonial_census_reading''?',
    'Authoring a separate constraint story for the ''orthodox_textual_reading'' with its own metrics and stakeholders.',
    'The ''orthodox_textual_reading'' would likely classify jati as a Mountain or Rope, grounded in scriptural authority, with lower extractiveness and different beneficiaries (e.g., religious institutions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orthodox_textual_reading_delta, conceptual, 'Structural differences between the colonial administrative reading and the orthodox textual reading of jati norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__colonial_census_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__colonial_census_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__colonial_census_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__colonial_census_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__colonial_census_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(jati_tr_t50, jati_practice_norm__colonial_census_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__colonial_census_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__colonial_census_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__colonial_census_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__colonial_census_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__colonial_census_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(jati_be_t50, jati_practice_norm__colonial_census_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__colonial_census_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(jati_su_t10, jati_practice_norm__colonial_census_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__colonial_census_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(jati_su_t30, jati_practice_norm__colonial_census_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__colonial_census_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(jati_su_t50, jati_practice_norm__colonial_census_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, post_colonial_census_categories).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, affirmative_action_policies).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jati_practice_norm' kernel, focusing on the colonial administrative reification of categories. Sibling readings include 'jati_practice_norm__orthodox_textual_reading' and 'jati_practice_norm__localized_practice_reading', which offer alternative structural interpretations of jati boundaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
