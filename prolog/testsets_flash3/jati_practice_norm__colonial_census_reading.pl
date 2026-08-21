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
 *   fluid jati categories through the imposition of colonial administrative
 *   apparatus, primarily the census. This reading highlights how external
 *   enforcement froze dynamic social boundaries, benefiting colonial
 *   governance legibility at the cost of indigenous community autonomy and
 *   flexibility. The constraint is claimed as a Tangled Rope because it
 *   provided a coordination function (administrative efficiency) but with
 *   significant asymmetric extraction and active enforcement.
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
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, 'cd1a9ba7-53be-4a75-80bc-9e92cd04d415').
narrative_ontology:cs_kernel_codification('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', formalized).
narrative_ontology:cs_authority_grounding('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', extraction).
narrative_ontology:cs_interpretation_layer_present('cd1a9ba7-53be-4a75-80bc-9e92cd04d415').
narrative_ontology:cs_reading_relation('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_axiom('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', foundational, administrative_legibility_is_paramount).
narrative_ontology:cs_axiom_status(administrative_legibility_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', administrative_legibility_is_paramount, conventional).
narrative_ontology:cs_axiom('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', foundational, social_categories_are_fixed_for_governance).
narrative_ontology:cs_axiom_status(social_categories_are_fixed_for_governance, holdable).
narrative_ontology:cs_axiom_grounding('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', social_categories_are_fixed_for_governance, instrumental).
narrative_ontology:cs_reference_frame('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', colonial_administrative_order).
narrative_ontology:cs_drift_state('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', post_independence_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('cd1a9ba7-53be-4a75-80bc-9e92cd04d415', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrators).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, local_elites_aligned_with_colonial_power).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, fluid_jati_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, indigenous_governance_structures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implemented and enforced the census categories, benefiting from simplified governance, taxation, and resource allocation. Their power derived from the colonial state, allowing them to impose a rigid classification system.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, regional).

% Were forced into rigid, externally defined categories that often did not reflect their fluid, context-dependent social identities. This led to loss of autonomy, internal disputes, and reduced social mobility. Their identity was tied to their community, making exit from the system impossible.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, fluid_jati_communities, payer,
    powerless, biographical, identity_locked, local).

% Benefited from the reification of certain jati categories, which solidified their social standing, access to resources, and political influence within the colonial administrative structure. They actively collaborated in the census process.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, local_elites_aligned_with_colonial_power, beneficiary,
    powerful, biographical, constrained, local).

% Were undermined and often replaced by the colonial administrative system, losing their authority to mediate and adapt local social norms. The imposition of external categories eroded their traditional functions.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, indigenous_governance_structures, payer,
    powerless, generational, trapped, local).

% Study the historical impact of colonial administrative practices on social structures, analyzing the long-term effects of reified jati categories on identity and power dynamics. They are external to the constraint's operation but analyze its effects.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, social_anthropologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a standardized, legible system for colonial administrators to categorize populations, enabling efficient governance, taxation, and military recruitment across diverse regions.
% TRANSFER_FUNCTION: Transferred administrative legibility and control from fluid, locally negotiated social structures to a centralized, rigid colonial bureaucracy, extracting autonomy and flexibility from indigenous communities.
% ABSENT_VOICES: The voices of communities whose identities were actively suppressed or distorted by the rigid census categories were absent from the administrative process. Their traditional leaders and scholars would have articulated the fluidity and context-dependence of jati identities.
% DISAPPEARANCE_RATIONALE: If the colonial reification of jati categories vanished, the post-colonial social and political landscape would undergo significant rearrangement. While some reified categories have become entrenched, their foundational rigidity would dissolve, potentially leading to a resurgence of fluid identities and local negotiation, challenging existing power structures and affirmative action policies.
% FOUNDING_PROBLEM: Colonial powers faced the problem of governing vast, diverse populations with complex, often unwritten, and locally variable social structures, making direct administration and resource management difficult.
% FOUNDING_PROBLEM_CORROBORATION: Colonial archives and administrative reports attest to the problem of legibility. Post-colonial historians and social anthropologists corroborate that the original administrative problem is long dead, but the reified categories persist due to institutional inertia and political utility for new elites, not because of an ongoing need for colonial-era legibility.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because the imposed categories distorted existing social structures and created new hierarchies, leading to a loss of self-determination for many communities. Suppression is also high (0.70) due to the coercive power of the colonial state in enforcing these classifications. The theater ratio is moderate (0.20) as the administrative function was real, but increasingly served to maintain colonial power rather than genuinely reflect or serve local social realities. The measurements show a rise in extractiveness and suppression as the colonial system became more entrenched, with a slight dip towards the end of the colonial period as resistance grew.
 *
 * PERSPECTIVAL GAP:
 *   From the colonial administrator's perspective, this was a necessary coordination mechanism for effective governance. From the perspective of the fluid jati communities, it was a coercive imposition that extracted their social autonomy and reified harmful hierarchies. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administrators are clear beneficiaries, gaining administrative efficiency and control. Local elites who aligned with the colonial power also benefited by solidifying their status. Fluid jati communities and indigenous governance structures were the primary victims, losing autonomy and facing imposed identities. Social anthropologists act as observers, analyzing the historical impact.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to make diverse populations legible for colonial administration. This problem is 'dead' in the post-colonial era, yet the reified categories persist due to institutional inertia and their subsequent political utility for new elites. This prevents mislabeling it as pure coordination, as its persistence is no longer tied to its original, now defunct, coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    post_colonial_persistence_mechanism,
    'What mechanisms sustain the reified jati categories in the post-colonial era, given the ''dead'' founding problem?',
    'Detailed sociological and political analysis of post-independence policies (e.g., affirmative action based on colonial categories), electoral politics, and continued social stratification.',
    'If sustained by new forms of political utility, the constraint''s classification might shift towards a Piton (inertial) or even a new Snare (if new elites actively extract from the reified categories). If it''s purely inertial, the theater ratio would be higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_colonial_persistence_mechanism, empirical, 'Examines the drivers of category persistence after colonial rule.').

omega_variable(
    fluidity_measurement_challenge,
    'How accurately can historical fluidity of jati categories be reconstructed and quantified, given the colonial administrative records are themselves a reifying force?',
    'Analysis of pre-colonial ethnographic accounts, oral histories, and linguistic evidence, triangulated with critical readings of colonial archives to identify discrepancies and silences.',
    'If pre-colonial fluidity was less than assumed, the ''extraction'' from reification might be lower. If it was greater, the extraction and suppression would be higher than currently estimated, strengthening the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fluidity_measurement_challenge, empirical, 'Assesses the challenge of measuring pre-colonial jati fluidity.').

omega_variable(
    structural_vs_internalized_suppression,
    'To what extent did the colonial imposition of jati categories lead to internalized suppression, where communities began to self-identify and enforce the rigid boundaries imposed upon them?',
    'Longitudinal studies of community identity formation and inter-jati relations across generations, examining shifts in self-perception and social practice post-independence.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as the target communities carry the suppression with them even after the direct colonial enforcement mechanism is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Structural vs. internalized suppression mechanism in jati reification.').


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
narrative_ontology:measurement(jati_tr_t1910, jati_practice_norm__colonial_census_reading, theater_ratio, 1910, 0.2).
narrative_ontology:measurement(jati_tr_t1930, jati_practice_norm__colonial_census_reading, theater_ratio, 1930, 0.25).
narrative_ontology:measurement(jati_tr_t1947, jati_practice_norm__colonial_census_reading, theater_ratio, 1947, 0.2).

% Extraction over time
narrative_ontology:measurement(jati_be_t1871, jati_practice_norm__colonial_census_reading, base_extractiveness, 1871, 0.4).
narrative_ontology:measurement(jati_be_t1890, jati_practice_norm__colonial_census_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(jati_be_t1910, jati_practice_norm__colonial_census_reading, base_extractiveness, 1910, 0.6).
narrative_ontology:measurement(jati_be_t1930, jati_practice_norm__colonial_census_reading, base_extractiveness, 1930, 0.68).
narrative_ontology:measurement(jati_be_t1947, jati_practice_norm__colonial_census_reading, base_extractiveness, 1947, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1871, jati_practice_norm__colonial_census_reading, suppression_requirement, 1871, 0.5).
narrative_ontology:measurement(jati_su_t1890, jati_practice_norm__colonial_census_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(jati_su_t1910, jati_practice_norm__colonial_census_reading, suppression_requirement, 1910, 0.68).
narrative_ontology:measurement(jati_su_t1930, jati_practice_norm__colonial_census_reading, suppression_requirement, 1930, 0.72).
narrative_ontology:measurement(jati_su_t1947, jati_practice_norm__colonial_census_reading, suppression_requirement, 1947, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, post_colonial_affirmative_action_policies).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_based_political_mobilization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jati_practice_norm' kernel, focusing on colonial administrative reification. It is linked to other readings that emphasize textual or localized practices, as they all describe different facets of the same underlying social phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
