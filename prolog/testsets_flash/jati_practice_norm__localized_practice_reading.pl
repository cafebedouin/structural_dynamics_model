% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundaries as Localized Practice Norms
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint describes Jati boundaries as fluid, locally negotiated
 *   social norms that primarily serve a coordination function within
 *   communities. This 'localized practice' reading emphasizes the empirical
 *   proliferation of Jati categories (over 3000) and their continuous
 *   adaptation, suggesting weak enforcement and low extraction. It stands in
 *   contrast to readings that emphasize fixed textual origins or colonial
 *   administrative reification.
 *
 * KEY AGENTS:
 *   - local_communities: Agenda setter (institutional/local) — continuously renegotiate and enforce norms
 *   - jati_members: Beneficiary/Payer (moderate/constrained) — benefit from social order, bear costs of adherence
 *   - social_anthropologists: Observer (analytical/analytical) — analyze the dynamic nature of Jati boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.2).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.3).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundaries as Localized Practice Norms").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/religious_studies/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '1b7809b3-ec73-44bf-ba60-e35c5990c761').
narrative_ontology:cs_kernel_codification('1b7809b3-ec73-44bf-ba60-e35c5990c761', distributed).
narrative_ontology:cs_authority_grounding('1b7809b3-ec73-44bf-ba60-e35c5990c761', practice).
narrative_ontology:cs_interpretation_layer_present('1b7809b3-ec73-44bf-ba60-e35c5990c761').
narrative_ontology:cs_reading_relation('1b7809b3-ec73-44bf-ba60-e35c5990c761', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b7809b3-ec73-44bf-ba60-e35c5990c761', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('1b7809b3-ec73-44bf-ba60-e35c5990c761', foundational, jati_boundaries_are_fluid_and_local).
narrative_ontology:cs_axiom_status(jati_boundaries_are_fluid_and_local, holdable).
narrative_ontology:cs_axiom_grounding('1b7809b3-ec73-44bf-ba60-e35c5990c761', jati_boundaries_are_fluid_and_local, empirically_contingent).
narrative_ontology:cs_axiom('1b7809b3-ec73-44bf-ba60-e35c5990c761', secondary, social_order_emerges_from_local_consensus).
narrative_ontology:cs_axiom_status(social_order_emerges_from_local_consensus, holdable).
narrative_ontology:cs_axiom_grounding('1b7809b3-ec73-44bf-ba60-e35c5990c761', social_order_emerges_from_local_consensus, conventional).
narrative_ontology:cs_reference_frame('1b7809b3-ec73-44bf-ba60-e35c5990c761', dynamic_local_consensus).
narrative_ontology:cs_drift_state('1b7809b3-ec73-44bf-ba60-e35c5990c761', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1b7809b3-ec73-44bf-ba60-e35c5990c761', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_communities).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, jati_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the primary collective actors that continuously renegotiate, interpret, and enforce Jati boundaries through social consensus and informal mechanisms. They benefit from the social cohesion and identity provided by these norms.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_communities, agenda_setter,
    institutional, generational, constrained, local).

% Individuals who identify with and participate in a specific Jati. They benefit from the social support, marriage networks, and occupational specialization that Jati norms can provide, while also bearing the costs of adherence to social expectations.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, jati_members, beneficiary,
    moderate, biographical, constrained, local).

% Researchers who study the empirical, lived reality of Jati boundaries, often documenting their fluidity, local variation, and adaptive functions, in contrast to more rigid, top-down interpretations.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, social_anthropologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interaction, marriage patterns, occupational specialization, and community identity within local groups, providing a framework for social order and mutual recognition.
% TRANSFER_FUNCTION: Facilitates the transfer of social capital, cultural knowledge, and sometimes economic opportunities within Jati groups, from one generation to the next, and between families.
% ABSENT_VOICES: Individuals or groups advocating for a complete abolition of all Jati distinctions might be marginalized in communities where these norms are deeply embedded, as their perspective challenges the very basis of local social organization.
% DISAPPEARANCE_RATIONALE: If localized Jati norms vanished overnight, local social structures, marriage patterns, and community identities would undergo significant, rapid reorganization. While some might welcome this, the immediate effect would be a loss of established social coordination mechanisms, leading to confusion and potential conflict until new norms emerged.
% FOUNDING_PROBLEM: The need for social organization, identity formation, and the regulation of marriage and occupation within diverse, often agrarian, communities in historical South Asia.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic studies and sociological research from independent academics consistently corroborate that Jati norms, in their localized and adaptive forms, continue to address fundamental needs for social organization and identity in many communities, even as their specific manifestations evolve. This corroboration comes from outside the immediate beneficiaries of the system.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.2) and suppression (0.3) are low because the boundaries are not rigidly enforced by a central authority but are subject to local consensus and adaptation. The proliferation of categories indicates that individuals and groups can often create new Jati or redefine existing ones, reducing the coercive power of any single boundary. Theater ratio is low (0.1) as the norms are genuinely functional for local social organization, with minimal performative maintenance for external audiences. Accessibility collapse is moderate (0.4) as alternatives (e.g., ignoring Jati altogether) are difficult but not impossible within a community, and resistance is low (0.25) due to the adaptive nature of the norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of local communities, these norms are essential for social cohesion and identity, experienced as a Rope. From an external, analytical observer's perspective, the fluidity and low enforcement also point to a Rope, contrasting sharply with more rigid interpretations of caste.
 *
 * DIRECTIONALITY LOGIC:
 *   Local communities (as agenda setters) benefit from the social order and coordination these norms provide, with minimal direct extraction. Jati members are primarily beneficiaries of the coordination, bearing only diffuse costs of adherence, placing them near the symmetric end of directionality. There are no identifiable 'victims' in this reading, as the system's fluidity allows for adaptation rather than rigid imposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The low extractiveness and suppression, coupled with the continuous renegotiation, prevent this constraint from being mislabeled as a Snare or Tangled Rope. Its persistence is due to its ongoing utility in local coordination, not inertia or hidden extraction. The 'localized practice' reading directly challenges the notion of a fixed, externally imposed mandate, thus resolving mandatrophy by asserting the constraint's adaptive, live function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jati_reading_ambiguity,
    'Is the localized, fluid understanding of Jati boundaries the primary structural reality, or is it an emergent property of deeper, more rigid textual or colonial administrative structures?',
    'Longitudinal ethnographic studies tracing the impact of textual interpretations or administrative classifications on local practice over generations, particularly in periods of social upheaval or state intervention.',
    'If deeper structures are primary, this constraint would be reclassified as a Tangled Rope or Snare, with localized practice serving as a performative layer masking underlying extraction or suppression. If localized practice is truly primary, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jati_reading_ambiguity, conceptual, 'Ambiguity between localized practice and deeper structural influences on Jati boundaries.').

omega_variable(
    localized_vs_global_coordination,
    'To what extent does the localized renegotiation of Jati boundaries genuinely coordinate social life, versus merely reflecting a lack of broader, more stable coordination mechanisms?',
    'Comparative analysis with other societies that have more formalized or centralized social stratification systems, assessing the relative social cohesion, conflict resolution, and economic mobility outcomes.',
    'If localized renegotiation is primarily a symptom of coordination failure, the ''rope'' classification might be too generous, suggesting a more ''piton''-like quality where the system persists due to inertia rather than active coordination. If it genuinely facilitates local order, the Rope classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(localized_vs_global_coordination, empirical, 'Whether localized Jati renegotiation is effective coordination or a symptom of broader coordination failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__localized_practice_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__localized_practice_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__localized_practice_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__localized_practice_reading, base_extractiveness, 30, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__localized_practice_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jati_su_t10, jati_practice_norm__localized_practice_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__localized_practice_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(jati_su_t30, jati_practice_norm__localized_practice_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Jati practice norm' kernel, emphasizing localized, fluid social coordination. It contrasts with 'jati_practice_norm__orthodox_textual_reading' and 'jati_practice_norm__colonial_census_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
