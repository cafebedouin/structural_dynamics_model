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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint describes jati boundaries as fluid, locally negotiated
 *   practice norms, a reading derived from extensive ethnographic and
 *   sociological research. It contrasts with textual interpretations that
 *   emphasize fixed scriptural categories (varna) and colonial administrative
 *   efforts to reify and enumerate jati for governance. This reading
 *   emphasizes the coordination function and adaptive nature of these norms,
 *   leading to low extractiveness and suppression, consistent with a 'rope'
 *   classification. The empirical proliferation to over 3000 categories
 *   across India indicates weak, localized enforcement rather than a rigid,
 *   centrally controlled system.
 *
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
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundaries as Localized Practice Norms").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/religious_studies/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, 'c44edd69-e4c3-48f3-8810-f2dff8ab9a9a').
narrative_ontology:cs_kernel_codification('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', distributed).
narrative_ontology:cs_authority_grounding('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', practice).
narrative_ontology:cs_interpretation_layer_present('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a').
narrative_ontology:cs_reading_relation('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', foundational, jati_boundaries_are_fluid_and_negotiated).
narrative_ontology:cs_axiom_status(jati_boundaries_are_fluid_and_negotiated, holdable).
narrative_ontology:cs_axiom_grounding('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', jati_boundaries_are_fluid_and_negotiated, empirically_contingent).
narrative_ontology:cs_axiom('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', foundational, local_practice_supersedes_external_codification).
narrative_ontology:cs_axiom_status(local_practice_supersedes_external_codification, holdable).
narrative_ontology:cs_axiom_grounding('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', local_practice_supersedes_external_codification, conventional).
narrative_ontology:cs_reference_frame('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', adaptive_local_social_order).
narrative_ontology:cs_drift_state('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c44edd69-e4c3-48f3-8810-f2dff8ab9a9a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_jati_groups).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups define and renegotiate their own boundaries, marriage rules, and occupational specializations. They benefit from the coordination and social cohesion these norms provide, but are also constrained by the need for local consensus.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_jati_groups, agenda_setter,
    organized, generational, constrained, local).

% Individuals within these communities gain social identity, mutual support, and clear social roles from the jati norms. Their exit options are limited by social ties and economic dependencies within the community.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, community_members, beneficiary,
    moderate, biographical, constrained, local).

% Academics who study the empirical, lived reality of jati boundaries, documenting their fluidity, local variation, and continuous renegotiation, often in contrast to textual or colonial interpretations.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, anthropological_observers, observer,
    analytical, generational, analytical, global).

% Scholars who adhere to scriptural interpretations of varna and jati, viewing deviations as ritual impurity or historical degradation. Their framework is largely incompatible with the empirical fluidity observed in practice.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, orthodox_textual_scholars, excluded,
    institutional, civilizational, identity_locked, national).

% Historical actors who attempted to fix and enumerate jati categories for administrative convenience, often imposing a rigid structure that did not reflect local realities. Their legacy continues to influence perceptions but their direct power is gone.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, colonial_administrators, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible framework for local social organization, marriage alliances, occupational specialization, and mutual support within communities, adapting to changing circumstances through continuous renegotiation.
% TRANSFER_FUNCTION: Facilitates the transfer of social capital, mutual aid, and shared identity among members, while also implicitly allocating social status and economic roles within the local context.
% ABSENT_VOICES: Orthodox textual scholars and historical colonial administrators, whose rigid interpretations of jati are contradicted by the observed fluidity and local variation. They would argue for fixed, externally imposed categories.
% DISAPPEARANCE_RATIONALE: If these localized practice norms vanished, the intricate social fabric, marriage patterns, and occupational structures of countless communities would unravel, leading to widespread social disorganization and the need for new forms of local coordination.
% FOUNDING_PROBLEM: The need for social cohesion, division of labor, and identity formation within diverse local communities, in a way that could adapt to specific regional and historical contexts.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological field studies and sociological surveys consistently document the ongoing function of these localized norms in structuring social life, even as they evolve. Community leaders and elders also attest to their continued relevance for social order.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.2) because the norms primarily serve local coordination and mutual benefit, with costs largely internalized as social friction. Suppression is also low (0.3) as enforcement is diffuse and relies on social pressure rather than coercive state power, allowing for continuous renegotiation and adaptation. Theater ratio is minimal (0.1) as the norms are genuinely functional for local social organization. Accessibility collapse is moderate (0.4) because while local norms provide clear social structures, they are not universally binding and can be adapted or circumvented, albeit with social cost. Resistance is low (0.15) because the norms are largely self-governing and adaptive, reducing the impetus for widespread opposition.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between the lived, empirical reality of jati as fluid local practice (this reading) and the rigid, fixed interpretations from textual or colonial perspectives. This reading highlights the adaptive coordination function, while other readings would emphasize hierarchy or administrative control.
 *
 * DIRECTIONALITY LOGIC:
 *   Local jati groups and community members are beneficiaries, gaining social cohesion and identity. Anthropological observers are analytical, documenting the system without direct participation. Orthodox textual scholars and colonial administrators are excluded, as their interpretations are not reflected in the lived practice this reading describes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_vs_textual_validity,
    'To what extent do localized jati practices genuinely reflect an adaptive coordination mechanism, versus being a degraded or ''impure'' form of a scripturally defined system?',
    'Further historical and textual analysis to trace the evolution of practice in relation to scriptural injunctions, alongside continued ethnographic study of local adaptations.',
    'If practices are found to be largely independent and adaptive, it strengthens the ''rope'' classification. If they are primarily deviations from a still-authoritative textual norm, it might shift towards a ''tangled_rope'' or ''snare'' for those who adhere to the textual authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_vs_textual_validity, conceptual, 'Ambiguity between empirical observation and normative textual claims regarding jati.').

omega_variable(
    colonial_influence_persistence,
    'How much of the observed ''fluidity'' and ''renegotiation'' is a genuine indigenous dynamic, and how much is a response to or subversion of the rigid categories imposed by colonial administration?',
    'Detailed historical analysis of pre-colonial jati dynamics and post-colonial shifts in local practice, disentangling indigenous evolution from reactive adaptation.',
    'If colonial influence is found to be a dominant driver, it suggests a ''tangled_rope'' or ''snare'' dynamic where local adaptation is a form of resistance to an imposed, extractive system. If indigenous dynamics are primary, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_influence_persistence, empirical, 'Disentangling indigenous jati dynamics from the lasting effects of colonial categorization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 1800, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1800, jati_practice_norm__localized_practice_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement(jati_tr_t1850, jati_practice_norm__localized_practice_reading, theater_ratio, 1850, 0.11).
narrative_ontology:measurement(jati_tr_t1900, jati_practice_norm__localized_practice_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(jati_tr_t1950, jati_practice_norm__localized_practice_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(jati_tr_t2000, jati_practice_norm__localized_practice_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(jati_tr_t2020, jati_practice_norm__localized_practice_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(jati_be_t1800, jati_practice_norm__localized_practice_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(jati_be_t1850, jati_practice_norm__localized_practice_reading, base_extractiveness, 1850, 0.22).
narrative_ontology:measurement(jati_be_t1900, jati_practice_norm__localized_practice_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(jati_be_t1950, jati_practice_norm__localized_practice_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(jati_be_t2000, jati_practice_norm__localized_practice_reading, base_extractiveness, 2000, 0.19).
narrative_ontology:measurement(jati_be_t2020, jati_practice_norm__localized_practice_reading, base_extractiveness, 2020, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1800, jati_practice_norm__localized_practice_reading, suppression_requirement, 1800, 0.35).
narrative_ontology:measurement(jati_su_t1850, jati_practice_norm__localized_practice_reading, suppression_requirement, 1850, 0.32).
narrative_ontology:measurement(jati_su_t1900, jati_practice_norm__localized_practice_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(jati_su_t1950, jati_practice_norm__localized_practice_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement(jati_su_t2000, jati_practice_norm__localized_practice_reading, suppression_requirement, 2000, 0.29).
narrative_ontology:measurement(jati_su_t2020, jati_practice_norm__localized_practice_reading, suppression_requirement, 2020, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jati_practice_norm' kernel. It focuses on the localized, empirical reality of jati as fluid social coordination, contrasting with textual and colonial interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
