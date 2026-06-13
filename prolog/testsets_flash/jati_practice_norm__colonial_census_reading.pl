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
 *   This constraint describes the reification and stabilization of fluid Jati
 *   categories through the administrative apparatus of the colonial census in
 *   British India. It is one reading of the 'jati_practice_norm' kernel,
 *   focusing on how external administrative needs transformed indigenous
 *   social structures. The constraint is claimed as a Tangled Rope because it
 *   provided a coordination function for colonial governance (legibility)
 *   while simultaneously extracting autonomy and imposing rigid identities on
 *   indigenous populations, requiring active enforcement to maintain.
 *
 * KEY AGENTS:
 *   - colonial_administrators: Agenda-setter (institutional/arbitrage) — imposed and enforced the categories.
 *   - fluid_jati_groups: Payer (powerless/identity_locked) — lost autonomy and fluidity due to imposed categories.
 *   - subaltern_jati_groups: Payer (powerless/identity_locked) — had subordinate status reified by fixed categories.
 *   - dominant_jati_groups: Beneficiary (powerful/constrained) — profited from the reification of their status.
 *   - indigenous_scholars: Observer (analytical/analytical) — critiqued the impact of colonial practices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.65).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.75).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.75).
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
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, 'bf7c974c-425a-4089-9be7-c4c4e1534de2').
narrative_ontology:cs_kernel_codification('bf7c974c-425a-4089-9be7-c4c4e1534de2', formalized).
narrative_ontology:cs_authority_grounding('bf7c974c-425a-4089-9be7-c4c4e1534de2', extraction).
narrative_ontology:cs_interpretation_layer_present('bf7c974c-425a-4089-9be7-c4c4e1534de2').
narrative_ontology:cs_reading_relation('bf7c974c-425a-4089-9be7-c4c4e1534de2', jati_practice_norm__orthodox_textual_reading, influences).
narrative_ontology:cs_reading_relation('bf7c974c-425a-4089-9be7-c4c4e1534de2', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_axiom('bf7c974c-425a-4089-9be7-c4c4e1534de2', foundational, jati_categories_are_fixed_administrative_units).
narrative_ontology:cs_axiom_status(jati_categories_are_fixed_administrative_units, holdable).
narrative_ontology:cs_axiom_grounding('bf7c974c-425a-4089-9be7-c4c4e1534de2', jati_categories_are_fixed_administrative_units, conventional).
narrative_ontology:cs_axiom('bf7c974c-425a-4089-9be7-c4c4e1534de2', secondary, administrative_legibility_trumps_local_fluidity).
narrative_ontology:cs_axiom_status(administrative_legibility_trumps_local_fluidity, holdable).
narrative_ontology:cs_axiom_grounding('bf7c974c-425a-4089-9be7-c4c4e1534de2', administrative_legibility_trumps_local_fluidity, instrumental).
narrative_ontology:cs_reference_frame('bf7c974c-425a-4089-9be7-c4c4e1534de2', colonial_administrative_legibility).
narrative_ontology:cs_drift_state('bf7c974c-425a-4089-9be7-c4c4e1534de2', post_independence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bf7c974c-425a-4089-9be7-c4c4e1534de2', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrators).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, dominant_jati_groups).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, fluid_jati_groups).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, subaltern_jati_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implemented and enforced the census categories, benefiting from simplified governance, taxation, and resource allocation. Their power derived from external imperial authority, allowing them to impose a rigid classification system.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, regional).

% Previously maintained flexible, context-dependent jati identities. The colonial census forced them into rigid, externally defined categories, leading to loss of autonomy, social mobility, and internal coherence. Their identity became locked into the imposed structure.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, fluid_jati_groups, payer,
    powerless, generational, identity_locked, local).

% Were assigned fixed, often lower-status, categories by the colonial administration, which then became legally and administratively binding. This reified their subordinate position and limited their social and economic opportunities, with little to no recourse.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, subaltern_jati_groups, payer,
    powerless, generational, identity_locked, local).

% Benefited from the reification of their status, often gaining preferential access to education, employment, and political representation under the colonial system. While they did not set the categories, they adapted to and profited from the new rigid structure.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, dominant_jati_groups, beneficiary,
    powerful, generational, constrained, regional).

% Analyzed and critiqued the impact of colonial administrative practices on indigenous social structures, documenting the discrepancy between pre-colonial fluidity and colonial rigidity. Their observations often informed later post-colonial policy debates.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, indigenous_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a simplified, standardized framework for colonial administrators to categorize and govern a complex, diverse population, enabling efficient census-taking, taxation, and resource allocation.
% TRANSFER_FUNCTION: Transferred administrative legibility and control from local, fluid social practices to a centralized, rigid colonial bureaucracy, at the cost of local autonomy and social dynamism.
% ABSENT_VOICES: The voices of local community leaders and practitioners who understood the nuanced, context-dependent nature of jati boundaries were largely absent from the colonial administrative process. Their perspectives would have highlighted the artificiality and harm of reification.
% DISAPPEARANCE_RATIONALE: If the colonial reification of jati categories vanished, the social landscape would gradually revert to more fluid, locally negotiated identities, though the historical legacy of the imposed categories would likely persist as a contested memory, influencing contemporary identity politics.
% FOUNDING_PROBLEM: The colonial administration faced the problem of governing a vast, diverse population with complex, locally variable social structures that were difficult to quantify and administer using European bureaucratic methods.
% FOUNDING_PROBLEM_CORROBORATION: Colonial records and administrative reports attest to the problem of 'legibility' for governance. Post-colonial historians and anthropologists, from outside the benefiting parties, corroborate that the original administrative problem is long dead, but the reified categories persist due to institutional inertia and political mobilization around these fixed identities.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).

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
 *   The extractiveness (0.65) reflects the significant loss of social fluidity and autonomy for many groups, as well as the imposition of fixed hierarchies. Suppression (0.75) is high due to the coercive power of the colonial state, which actively enforced the census categories through legal and administrative means, with no viable exit for affected communities. The theater ratio (0.20) is relatively low, as the administrative function was genuinely served, but the 'scientific' justification for rigid classification had a performative aspect masking its political utility. Accessibility collapse (0.60) is moderate, as local practices persisted but were increasingly undermined by the official system. Resistance (0.45) was present but often localized and unable to challenge the overarching colonial power.
 *
 * PERSPECTIVAL GAP:
 *   Colonial administrators experienced this as a necessary coordination mechanism for efficient governance, a 'rope' for managing complexity. For the fluid and subaltern jati groups, it was a 'snare' that froze their identities and extracted their social mobility. Dominant jati groups, while not setting the rules, found it to be a 'tangled rope' that coordinated their elevated status while imposing some rigidity.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administrators are clear beneficiaries (d=0.0-0.1) as the constraint directly served their administrative goals. Fluid and subaltern jati groups are targets (d=0.9-1.0) as they bore the costs of reification and had their identities locked. Dominant jati groups are beneficiaries (d=0.2-0.3) as they gained status and resources, though with some loss of pre-colonial fluidity. Indigenous scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (administrative legibility for colonial rule) became obsolete with decolonization. However, the reified categories persisted due to institutional inertia and became new bases for political mobilization and identity, transforming the constraint from a colonial administrative tool into a post-colonial social reality. This prevents mislabeling it as a pure snare, acknowledging its initial coordination function for the colonial state, even as that function became extractive for the indigenous population and later atrophied into a different form of social constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_categories,
    'To what extent were the Jati categories ''discovered'' by colonial administrators versus actively ''constructed'' through the census process?',
    'Comparative historical analysis of pre-colonial indigenous texts and ethnographic accounts versus colonial administrative records and census methodologies.',
    'If primarily constructed, the constraint''s extractiveness and suppression are higher, as it represents a more direct imposition of external order. If primarily discovered, it leans more towards a ''mountain'' of pre-existing social structure, though still reified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_categories, empirical, 'Ambiguity between pre-existing social categories and colonial reification.').

omega_variable(
    identity_lock_mechanism,
    'What proportion of the ''identity_locked'' exit option for fluid/subaltern jati groups was due to external administrative enforcement versus internalized acceptance of the new categories over time?',
    'Post-colonial studies examining the persistence of these categories in the absence of direct colonial enforcement, and the emergence of new forms of identity politics based on them.',
    'If internalized acceptance is high, the effective suppression is higher and more persistent, as the constraint operates through self-identification even after external pressure diminishes. If primarily external, the constraint''s persistence is more dependent on successor institutional structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized suppression mechanism for identity lock.').

omega_variable(
    framing_under_determination,
    'Is this constraint best framed as a colonial administrative tool (this reading) or as a distortion of an underlying, more fluid indigenous social system (localized_practice_reading)?',
    'Analysis of the primary causal driver: if the administrative apparatus was the dominant force in shaping the categories, this reading holds. If local practices continued to exert stronger influence despite colonial efforts, the localized_practice_reading is more appropriate.',
    'Adopting the localized_practice_reading would likely shift the claimed_type towards a Rope or Tangled Rope with lower extractiveness, as it emphasizes the internal coordination function of jati boundaries rather than their external reification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Alternative framings of the jati categories as either colonial construct or indigenous practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 1871, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1871, jati_practice_norm__colonial_census_reading, theater_ratio, 1871, 0.1).
narrative_ontology:measurement(jati_tr_t1891, jati_practice_norm__colonial_census_reading, theater_ratio, 1891, 0.15).
narrative_ontology:measurement(jati_tr_t1911, jati_practice_norm__colonial_census_reading, theater_ratio, 1911, 0.18).
narrative_ontology:measurement(jati_tr_t1931, jati_practice_norm__colonial_census_reading, theater_ratio, 1931, 0.2).
narrative_ontology:measurement(jati_tr_t1947, jati_practice_norm__colonial_census_reading, theater_ratio, 1947, 0.2).

% Extraction over time
narrative_ontology:measurement(jati_be_t1871, jati_practice_norm__colonial_census_reading, base_extractiveness, 1871, 0.5).
narrative_ontology:measurement(jati_be_t1891, jati_practice_norm__colonial_census_reading, base_extractiveness, 1891, 0.58).
narrative_ontology:measurement(jati_be_t1911, jati_practice_norm__colonial_census_reading, base_extractiveness, 1911, 0.62).
narrative_ontology:measurement(jati_be_t1931, jati_practice_norm__colonial_census_reading, base_extractiveness, 1931, 0.65).
narrative_ontology:measurement(jati_be_t1947, jati_practice_norm__colonial_census_reading, base_extractiveness, 1947, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1871, jati_practice_norm__colonial_census_reading, suppression_requirement, 1871, 0.6).
narrative_ontology:measurement(jati_su_t1891, jati_practice_norm__colonial_census_reading, suppression_requirement, 1891, 0.68).
narrative_ontology:measurement(jati_su_t1911, jati_practice_norm__colonial_census_reading, suppression_requirement, 1911, 0.72).
narrative_ontology:measurement(jati_su_t1931, jati_practice_norm__colonial_census_reading, suppression_requirement, 1931, 0.75).
narrative_ontology:measurement(jati_su_t1947, jati_practice_norm__colonial_census_reading, suppression_requirement, 1947, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, post_colonial_affirmative_action_policies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jati_practice_norm' kernel, focusing on the colonial administrative reification of categories. It differs from the 'orthodox_textual_reading' (fixed scriptural basis) and 'localized_practice_reading' (fluid local negotiation) by emphasizing external, coercive stabilization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
