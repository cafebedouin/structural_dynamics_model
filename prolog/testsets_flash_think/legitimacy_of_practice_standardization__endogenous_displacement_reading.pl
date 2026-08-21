% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Legitimacy of Practice Standardization: Endogenous Displacement Reading
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint story instantiates the 'endogenous displacement' reading
 *   of the kernel 'legitimacy_of_practice_standardization'. It posits that
 *   practice change is legitimate when it arises from voluntary adoption,
 *   driven by perceived utility or cultural evolution, rather than by
 *   external decree or compartmentalized authority. This reading expects
 *   gradual adoption curves, regional variation, and elite-to-mass diffusion,
 *   with resistance appearing as temporary friction rather than active
 *   suppression. The 'double life' of old and new practices is seen as a
 *   transitional phase.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.1).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Legitimacy of Practice Standardization: Endogenous Displacement Reading").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'fde3a523-f211-47c9-85f8-ddd95873a31e').
narrative_ontology:cs_kernel_codification('fde3a523-f211-47c9-85f8-ddd95873a31e', implicit).
narrative_ontology:cs_authority_grounding('fde3a523-f211-47c9-85f8-ddd95873a31e', practice).
narrative_ontology:cs_interpretation_layer_present('fde3a523-f211-47c9-85f8-ddd95873a31e').
narrative_ontology:cs_reading_relation('fde3a523-f211-47c9-85f8-ddd95873a31e', legitimacy_of_practice_standardization__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('fde3a523-f211-47c9-85f8-ddd95873a31e', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('fde3a523-f211-47c9-85f8-ddd95873a31e', foundational, legitimacy_derives_from_voluntary_adoption).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_voluntary_adoption, holdable).
narrative_ontology:cs_axiom_grounding('fde3a523-f211-47c9-85f8-ddd95873a31e', legitimacy_derives_from_voluntary_adoption, conventional).
narrative_ontology:cs_axiom('fde3a523-f211-47c9-85f8-ddd95873a31e', foundational, utility_drives_cultural_evolution).
narrative_ontology:cs_axiom_status(utility_drives_cultural_evolution, holdable).
narrative_ontology:cs_axiom_grounding('fde3a523-f211-47c9-85f8-ddd95873a31e', utility_drives_cultural_evolution, empirically_contingent).
narrative_ontology:cs_reference_frame('fde3a523-f211-47c9-85f8-ddd95873a31e', organic_social_evolution).
narrative_ontology:cs_drift_state('fde3a523-f211-47c9-85f8-ddd95873a31e', contemporary_modernization_studies, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fde3a523-f211-47c9-85f8-ddd95873a31e', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities voluntarily adopt new practices (e.g., calendar systems, dress codes) because they perceive them as more useful, efficient, or culturally resonant. They benefit from the perceived utility and smoother social coordination.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_communities, beneficiary,
    moderate, biographical, mobile, local).

% Individuals or groups who adhere to older practices. While not actively coerced, they bear the social cost of being out of step with evolving norms and may find their practices marginalized or less functional in a changing society. Their 'resistance' is often passive friction, not active rebellion.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditionalists, payer,
    powerless, generational, constrained, local).

% Scholars and analysts whose theories of social change, cultural evolution, and diffusion of innovation are validated by observing practices changing through voluntary adoption and perceived utility. They benefit from the explanatory power of this model.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists, beneficiary,
    analytical, civilizational, analytical, universal).

% In this reading, state authorities primarily observe and document the organic evolution of practices, rather than actively decreeing or enforcing them. They may facilitate, but not compel, adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_authorities, observer,
    institutional, generational, analytical, national).

% Advocates for top-down, decreed practice change (e.g., through legislation or administrative fiat). This reading considers their methods illegitimate, thus they are excluded from the definition of legitimate change.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, exogenous_override_advocates, excluded,
    powerful, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social practices around a shared understanding of utility and cultural evolution, allowing for organic adaptation and progress without overt coercion.
% TRANSFER_FUNCTION: Transfers social legitimacy and functional relevance from older, less useful practices to newer, more efficient or culturally resonant ones, driven by collective adoption.
% ABSENT_VOICES: Advocates for exogenous, top-down decrees (exogenous_override_advocates) would argue that organic change is too slow or inefficient for collective benefit. Advocates for dual practice (dual_practice_equilibrium_reading) would argue that traditional practices retain legitimacy in specific domains regardless of utility.
% DISAPPEARANCE_RATIONALE: If this understanding of legitimacy vanished, societies would lack a coherent framework for understanding and accepting organic practice change. All change would either be seen as imposed (and thus illegitimate by this reading's lights) or chaotic, leading to greater social friction and contestation over the very nature of progress.
% FOUNDING_PROBLEM: How to achieve social progress and adapt practices to new conditions without resorting to coercion or undermining the perceived autonomy and cultural continuity of communities.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists, anthropologists, and historians who study cultural evolution, diffusion of innovations, and the dynamics of social acceptance provide corroboration for the ongoing relevance of this problem and the mechanisms of endogenous change. Their research often highlights the limits and backlashes associated with purely top-down mandates.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the core premise of this reading: low extractiveness and suppression (0.15 and 0.10 respectively) because change is voluntary and driven by utility, not coercion. Theater ratio is minimal (0.05) as the process is genuine, not performative. Accessibility collapse is moderate (0.60) because old practices are displaced by perceived obsolescence, not outright ban. Resistance is low (0.20) as it's primarily friction from traditionalists, not active opposition to a coercive force. The temporal measurements show stability, reflecting the reading's assertion of a consistent, organic process of legitimate change.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'adopting_communities' and 'modernization_theorists', this constraint is a pure 'rope' or even a 'mountain' of social evolution, facilitating progress. For 'traditionalists', it functions more like a 'piton' or a mild 'snare', as their practices are gradually marginalized by the perceived utility of new ones, even if not actively suppressed. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   'Adopting_communities' and 'modernization_theorists' are beneficiaries (low d) as they gain from the perceived utility and explanatory power, respectively. 'Traditionalists' are targets (high d) as they bear the costs of displacement. 'State_authorities' are observers (analytical d) in this reading, not active enforcers. 'Exogenous_override_advocates' are excluded, as their methods are deemed illegitimate by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling organic social change as either pure extraction or a fixed natural law. By framing it as a 'rope', it acknowledges a coordination function (around utility) while still allowing for the identification of 'traditionalists' as those who bear the costs of this evolution, preventing the 'naturalness' of change from fully obscuring its social impact. The low theater ratio confirms it's not merely performative maintenance of an obsolete mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntariness_vs_structural_pressure,
    'How truly ''voluntary'' is adoption when strong economic or social pressures (e.g., market forces, peer pressure) favor the new practice?',
    'Detailed ethnographic studies of adoption processes, analyzing the decision-making context for individuals and communities, and quantifying the costs of non-adoption.',
    'If ''voluntariness'' is significantly modulated by structural pressures, the effective suppression and extractiveness of the constraint might be higher than measured, pushing it towards a ''tangled_rope'' or even ''snare'' for those with fewer alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_vs_structural_pressure, empirical, 'Ambiguity in the degree of voluntariness in practice adoption.').

omega_variable(
    utility_vs_power_in_legitimacy,
    'Is ''perceived utility'' an objective measure, or is it often shaped and promoted by the interests of powerful groups, making the ''endogenous'' process less neutral than it appears?',
    'Historical and sociological analysis tracing the origins and promotion of ''useful'' practices, identifying the actors and institutions that benefit from their adoption and the resources they deploy to influence perception.',
    'If perceived utility is significantly influenced by power dynamics, the constraint''s extractiveness might be higher, and its coordination function might be a cover for rent-seeking by those who define ''utility'', pushing it towards a ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_vs_power_in_legitimacy, conceptual, 'Whether perceived utility is a neutral driver or a product of power.').

omega_variable(
    kernel_legitimacy_framing_ambiguity,
    'Is the legitimacy of practice standardization *only* derived from endogenous displacement, or are other framings (exogenous decree, dual practice equilibrium) also valid descriptions of legitimate change?',
    'Cross-cultural and historical comparative analysis of successful and unsuccessful practice changes, evaluating which legitimacy framings correlate with stability and social acceptance in different contexts.',
    'If other framings are also valid, this reading''s claim to universal legitimacy for practice change is weakened, and the overall kernel ''legitimacy_of_practice_standardization'' is revealed as fundamentally contested, not reducible to a single process.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_legitimacy_framing_ambiguity, conceptual, 'This constraint is one reading of a contested kernel; other readings offer alternative sources of legitimacy for practice change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimacy_of_practice_standardization' kernel. The other readings are 'exogenous_override_reading' and 'dual_practice_equilibrium_reading', each offering a distinct structural account of how practice change gains legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
