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
 * SUMMARY:
 *   This constraint describes the reification and stabilization of previously
 *   fluid jati categories by the British colonial administration through
 *   census operations. The colonial census, intended for administrative
 *   legibility, imposed rigid classifications that did not reflect local
 *   social realities, transforming dynamic social practices into fixed,
 *   hierarchical structures. This reading highlights the extractive nature of
 *   this administrative act, benefiting colonial governance at the expense of
 *   indigenous social autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.65).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.7).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Jati Categories Reified by Colonial Census").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, 'f2b4bd31-7a6f-482b-8168-e5ec5c3187f6').
narrative_ontology:cs_kernel_codification('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', formalized).
narrative_ontology:cs_authority_grounding('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', extraction).
narrative_ontology:cs_interpretation_layer_present('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6').
narrative_ontology:cs_reading_relation('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', jati_practice_norm__orthodox_textual_reading, influences).
narrative_ontology:cs_reading_relation('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_axiom('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', foundational, administrative_legibility_is_paramount).
narrative_ontology:cs_axiom_status(administrative_legibility_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', administrative_legibility_is_paramount, instrumental).
narrative_ontology:cs_axiom('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', foundational, social_categories_are_fixed_and_hierarchical).
narrative_ontology:cs_axiom_status(social_categories_are_fixed_and_hierarchical, holdable).
narrative_ontology:cs_axiom_grounding('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', social_categories_are_fixed_and_hierarchical, conventional).
narrative_ontology:cs_reference_frame('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', colonial_administrative_order).
narrative_ontology:cs_drift_state('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', post_independence_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f2b4bd31-7a6f-482b-8168-e5ec5c3187f6', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrators).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, certain_jati_elites).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, local_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, fluid_jati_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implemented and enforced the census categories, benefiting from simplified governance, taxation, and resource allocation. Their power derived from the colonial state, making exit from this administrative framework non-existent for them.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, regional).

% Were forced to conform to rigid, externally imposed jati classifications that often did not align with their fluid, context-dependent social realities. This led to loss of autonomy and internal social friction. Exit meant resisting the colonial state, which was impossible.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, local_communities, payer,
    powerless, generational, trapped, local).

% Previously had flexible, context-dependent jati identities that could shift based on occupation, marriage, or migration. The census froze these identities, often assigning them to categories that limited their social mobility or access to resources. Their identity became locked into the administrative categories.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, fluid_jati_groups, payer,
    powerless, biographical, identity_locked, local).

% Benefited from the stabilization of categories, as it often solidified their status, land rights, or access to administrative positions. They could leverage the new rigid system for their own advantage, often collaborating with colonial powers.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, certain_jati_elites, beneficiary,
    powerful, biographical, mobile, regional).

% Analyzed the impact of colonial administration on indigenous social structures, documenting the reification of jati categories and its long-term consequences. They operate outside the direct enforcement mechanism but seek to understand its structural effects.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, indigenous_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a standardized, legible system for colonial administrators to categorize and govern diverse populations, facilitating census-taking, taxation, and legal administration across vast territories.
% TRANSFER_FUNCTION: Transferred administrative efficiency and simplified governance to colonial powers, at the cost of social fluidity and self-determination for local communities, who bore the burden of rigid categorization.
% ABSENT_VOICES: Local community leaders and traditional scholars who understood the fluid, context-dependent nature of jati categories were largely excluded from the colonial administrative process; they would have argued against the reification and for local autonomy in social organization.
% DISAPPEARANCE_RATIONALE: If the colonial reification of jati categories vanished, the post-colonial administrative and political systems, which inherited these rigid classifications, would face significant challenges. Social identities and political representation would need to be renegotiated, leading to a profound reorganization of social and political structures.
% FOUNDING_PROBLEM: The colonial administration faced the problem of governing a vast, diverse population with complex, fluid social structures that were illegible to their centralized, bureaucratic methods.
% FOUNDING_PROBLEM_CORROBORATION: Colonial records and administrative reports attest to the problem of legibility. Post-colonial historians and social anthropologists, from outside the benefiting parties, corroborate that the original administrative problem is long dead, but the reified categories persist due to institutional inertia and political entrenchment.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is moderate (0.65) because the system primarily served colonial administrative efficiency, extracting social flexibility and imposing a foreign order. Suppression is high (0.70) due to the coercive power of the colonial state in enforcing these classifications. Theater ratio is low (0.20) as the census was genuinely functional for colonial governance, even if its stated purpose (e.g., 'understanding native society') was partly a cover for control. The metrics reflect the increasing entrenchment of these categories over the colonial period.
 *
 * PERSPECTIVAL GAP:
 *   Colonial administrators would have perceived this as a necessary coordination mechanism for effective governance, while local communities experienced it as an imposition that extracted their social flexibility and imposed an alien order. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administrators are clear beneficiaries, gaining simplified governance and control. Certain jati elites also benefited by solidifying their status within the new rigid system. Local communities and fluid jati groups were victims, losing social autonomy and having their identities fixed in ways that often disadvantaged them. Indigenous scholars act as observers, analyzing the structural impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_colonial_categorization,
    'To what extent did the colonial administrative categories gain internal legitimacy within the colonized societies over time, beyond mere coercion?',
    'Analysis of post-colonial social movements and political discourse regarding identity and reservation policies; ethnographic studies of how communities self-identify in the absence of external pressure.',
    'If categories gained significant internal legitimacy, the suppression metric might be over-estimated, as some ''compliance'' would be voluntary coordination. If legitimacy remained low, the constraint is more purely extractive and coercive than currently measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_colonial_categorization, empirical, 'Assesses the degree of internalized acceptance of colonial jati classifications.').

omega_variable(
    long_term_impact_on_social_mobility,
    'How did the reification of jati categories by the colonial census permanently alter patterns of social mobility and economic opportunity in the post-colonial era?',
    'Longitudinal studies tracking intergenerational mobility across different jati groups, comparing pre-colonial fluidity with post-colonial rigidity and its effects on access to education, employment, and political power.',
    'If the impact was profound and persistent, the constraint''s long-term extractiveness and suppression are higher than measured during the colonial period, indicating a deeper structural legacy. If post-colonial societies managed to re-introduce fluidity, the impact was less severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(long_term_impact_on_social_mobility, empirical, 'Measures the enduring structural legacy of colonial jati reification.').

omega_variable(
    framing_of_jati_as_fixed_vs_fluid,
    'Is the concept of ''jati'' inherently fixed and hierarchical (as in the orthodox textual reading), or is its fluidity and context-dependence its defining characteristic (as in the localized practice reading)?',
    'Conceptual analysis of historical and ethnographic data, focusing on the emic (insider) perspectives on jati identity and practice across different regions and time periods, contrasting with prescriptive textual interpretations.',
    'If jati is fundamentally fluid, the colonial census reading is a clear imposition and distortion, amplifying its extractive nature. If it has an underlying fixed structure, the colonial intervention might be seen as merely formalizing an existing, albeit less rigid, hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_jati_as_fixed_vs_fluid, conceptual, 'Examines the fundamental nature of jati categories as fixed or fluid.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 1871, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1871, jati_practice_norm__colonial_census_reading, theater_ratio, 1871, 0.1).
narrative_ontology:measurement(jati_tr_t1890, jati_practice_norm__colonial_census_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(jati_tr_t1910, jati_practice_norm__colonial_census_reading, theater_ratio, 1910, 0.18).
narrative_ontology:measurement(jati_tr_t1930, jati_practice_norm__colonial_census_reading, theater_ratio, 1930, 0.2).
narrative_ontology:measurement(jati_tr_t1947, jati_practice_norm__colonial_census_reading, theater_ratio, 1947, 0.2).

% Extraction over time
narrative_ontology:measurement(jati_be_t1871, jati_practice_norm__colonial_census_reading, base_extractiveness, 1871, 0.5).
narrative_ontology:measurement(jati_be_t1890, jati_practice_norm__colonial_census_reading, base_extractiveness, 1890, 0.58).
narrative_ontology:measurement(jati_be_t1910, jati_practice_norm__colonial_census_reading, base_extractiveness, 1910, 0.62).
narrative_ontology:measurement(jati_be_t1930, jati_practice_norm__colonial_census_reading, base_extractiveness, 1930, 0.64).
narrative_ontology:measurement(jati_be_t1947, jati_practice_norm__colonial_census_reading, base_extractiveness, 1947, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1871, jati_practice_norm__colonial_census_reading, suppression_requirement, 1871, 0.6).
narrative_ontology:measurement(jati_su_t1890, jati_practice_norm__colonial_census_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(jati_su_t1910, jati_practice_norm__colonial_census_reading, suppression_requirement, 1910, 0.68).
narrative_ontology:measurement(jati_su_t1930, jati_practice_norm__colonial_census_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(jati_su_t1947, jati_practice_norm__colonial_census_reading, suppression_requirement, 1947, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jati_practice_norm' kernel. It describes the reification of jati categories by colonial census, distinct from orthodox textual interpretations or localized practice norms, but structurally influences both by creating a new, rigid baseline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
