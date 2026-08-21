% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Legitimacy of Imposed Practice: Endogenous Climb Reading
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint represents the 'endogenous climb' reading of how imposed
 *   practices gain legitimacy: they must be adopted from the bottom-up,
 *   through internalization, rather than by top-down decree. The story
 *   focuses on historical instances where state-imposed cultural changes
 *   (e.g., calendar reforms, dress codes) failed to fully displace existing
 *   practices, leading to a high degree of performative compliance and
 *   continued resistance. The constraint is classified as a Piton because the
 *   state's efforts become largely theatrical, extracting diffuse costs
 *   without achieving genuine displacement, and persisting due to
 *   institutional inertia rather than effective function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.25).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.4).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, piton).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Legitimacy of Imposed Practice: Endogenous Climb Reading").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, 'd9814dee-6b4d-4abf-b382-6fee12846b8c').
narrative_ontology:cs_kernel_codification('d9814dee-6b4d-4abf-b382-6fee12846b8c', formalized).
narrative_ontology:cs_authority_grounding('d9814dee-6b4d-4abf-b382-6fee12846b8c', lineage).
narrative_ontology:cs_interpretation_layer_present('d9814dee-6b4d-4abf-b382-6fee12846b8c').
narrative_ontology:cs_reading_relation('d9814dee-6b4d-4abf-b382-6fee12846b8c', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9814dee-6b4d-4abf-b382-6fee12846b8c', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('d9814dee-6b4d-4abf-b382-6fee12846b8c', foundational, legitimacy_requires_internalization).
narrative_ontology:cs_axiom_status(legitimacy_requires_internalization, holdable).
narrative_ontology:cs_axiom_grounding('d9814dee-6b4d-4abf-b382-6fee12846b8c', legitimacy_requires_internalization, empirically_contingent).
narrative_ontology:cs_axiom('d9814dee-6b4d-4abf-b382-6fee12846b8c', secondary, top_down_imposition_is_brittle).
narrative_ontology:cs_axiom_status(top_down_imposition_is_brittle, holdable).
narrative_ontology:cs_axiom_grounding('d9814dee-6b4d-4abf-b382-6fee12846b8c', top_down_imposition_is_brittle, empirically_contingent).
narrative_ontology:cs_reference_frame('d9814dee-6b4d-4abf-b382-6fee12846b8c', bottom_up_adoption_framework).
narrative_ontology:cs_drift_state('d9814dee-6b4d-4abf-b382-6fee12846b8c', post_colonial_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d9814dee-6b4d-4abf-b382-6fee12846b8c', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, local_communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, central_government_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_elites_adopting_selectively).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_elites_adopting_selectively).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attempt to impose new practices (e.g., Gregorian calendar, Western dress codes) to modernize the state and integrate it into global systems. They bear the cost of enforcement and the frustration of slow adoption, seeing their modernization timeline delayed.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, central_government_reformers, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from preserving traditional practices (e.g., lunar calendar, traditional dress) by maintaining cultural continuity and resisting external imposition. Their 'benefit' is the retention of their identity and autonomy, despite official decrees.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, local_communities_preserving_autonomy, beneficiary,
    powerless, generational, identity_locked, local).

% The abstract entity representing the state's progress towards its modernization goals. It 'pays' in delays and inefficiencies when imposed practices fail to take root, reflecting the opportunity cost of failed reforms.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).

% Adopt some imposed practices (e.g., Western dress in public) for social or professional advancement, but often retain traditional practices in private. They bear the cost of dual practice but gain social capital from partial adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_elites_adopting_selectively, payer,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_elites_adopting_selectively, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint attempts to coordinate national practice around a new standard (e.g., a unified calendar or dress code) to project a modern, unified national identity and facilitate international engagement.
% TRANSFER_FUNCTION: Transfers cultural capital and symbolic legitimacy from traditional practices to state-sanctioned modern practices. It also transfers enforcement resources from the central government to the maintenance of these new norms, often with little return.
% ABSENT_VOICES: Traditional religious authorities and cultural leaders, whose legitimacy is rooted in the very practices the state seeks to displace, are often excluded from the reform discourse. They would argue for the intrinsic value and social cohesion provided by existing practices.
% DISAPPEARANCE_RATIONALE: If the state's attempts to impose new practices vanished, the existing traditional practices would continue largely unchanged, and the state's modernization efforts would need to find alternative, more endogenous pathways. The 'world' of state-led reform would rearrange its strategy.
% FOUNDING_PROBLEM: The central government perceived a need to unify diverse local practices and align the nation with global standards (e.g., a universal calendar for international trade, modern dress for diplomatic representation) to overcome perceived 'backwardness' and internal fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: The central government continues to assert the problem of national unity and global alignment is live. Local communities and historical analyses, however, corroborate that the specific problem of 'backwardness' was often a construct of the reformers, and the imposed solutions frequently created new problems of cultural alienation and resistance.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).
:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the state's attempts to extract full compliance are largely unsuccessful; the 'cost' is borne by the state in wasted resources and delayed modernization, rather than by the populace in forced conformity. Suppression is moderate (0.4) as the state applies coercive measures, but these are often met with passive resistance or private non-compliance. Theater ratio is high (0.6) and rising, reflecting the increasing gap between official policy and actual practice, with much of the state's enforcement becoming performative. The persistence of lunar observance for decades despite official Gregorian calendar adoption, and the private retention of traditional dress, exemplify this dynamic.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of central government reformers, the constraint is a necessary (though difficult) Rope or Scaffold for national development. From the perspective of local communities, it is a Snare or Piton that fails to fully capture them. This reading emphasizes the latter, highlighting the limits of top-down authority without endogenous adoption pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government reformers are agenda-setters, bearing the costs of failed imposition. Local communities preserving autonomy are beneficiaries, as their resistance allows them to retain cultural practices. Urban elites are payers who selectively adopt for social gain, but also beneficiaries of maintaining some traditional identity. The 'state modernization timeline' is an abstract victim, suffering delays. The low extractiveness and high theater ratio reflect the difficulty of imposing practices without bottom-up internalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy as the original mandate (to unify and modernize) is not effectively met by the imposed practices. The persistence of the constraint is due to institutional inertia and the state's reluctance to admit failure, rather than its functional effectiveness. The high theater ratio is a key indicator of this mandatrophy, as resources are expended on maintaining a facade of compliance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_measurement_ambiguity,
    'How can ''internalization'' of a practice be reliably measured, distinct from mere performative compliance?',
    'Longitudinal ethnographic studies, analysis of private vs. public behavior, and linguistic shifts indicating cognitive adoption rather than just behavioral conformity.',
    'If internalization is found to be higher than assumed, the constraint might be reclassified as a more effective Rope or Scaffold. If lower, it reinforces the Piton classification and the reading''s core premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_measurement_ambiguity, empirical, 'Distinguishing genuine adoption from superficial compliance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, economic disincentives) or internalized (fear of social ostracism, belief in state legitimacy)?',
    'Post-exit suppression trajectory: if non-compliance persists after formal enforcement is removed, reclassify as partially internalized. Analysis of public discourse and educational curricula for ideological conditioning.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would shift the classification towards a Snare, as the extraction is more deeply embedded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in cultural imposition.').

omega_variable(
    framing_of_modernization_goals,
    'Is the ''modernization'' goal a genuine collective benefit, or a culturally specific preference imposed by the central government?',
    'Comparative analysis of diverse modernization pathways, and historical inquiry into the origins and biases of the ''modern'' ideal promoted by the state.',
    'If ''modernization'' is primarily a preference, the constraint''s coordination function is weaker, and its extractive nature (imposing one group''s preferences on others) is amplified, pushing it towards a Snare. If a genuine collective benefit, it supports a Rope or Scaffold framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_modernization_goals, conceptual, 'Whether modernization is a universal good or a culturally specific imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 50, 0.6).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
