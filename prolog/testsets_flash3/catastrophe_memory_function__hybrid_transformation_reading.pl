% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function: Hybrid Transformation Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes a 'hybrid transformation' reading of
 *   catastrophe memory rituals, such as Passover. It posits that such rituals
 *   simultaneously encode mourning practices (D1/D4 - preserving loss-memory,
 *   e.g., bitter herbs) and transmit survival competence (D5 - adaptive
 *   mechanisms, e.g., the structured seder performance). This reading
 *   emphasizes the integrated nature of these functions, where the ritual's
 *   efficacy lies in its ability to transform trauma into resilience through
 *   a combined act of remembrance and rehearsal for continuity. The
 *   constraint is claimed as a Rope due to its genuine coordination function
 *   and low extraction, benefiting the entire community.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function: Hybrid Transformation Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, 'd82be4bd-1896-4d60-a8f5-5ae874dd1e82').
narrative_ontology:cs_kernel_codification('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', formalized).
narrative_ontology:cs_authority_grounding('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', lineage).
narrative_ontology:cs_interpretation_layer_present('d82be4bd-1896-4d60-a8f5-5ae874dd1e82').
narrative_ontology:cs_reading_relation('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', foundational, trauma_transforms_through_dual_ritual_function).
narrative_ontology:cs_axiom_status(trauma_transforms_through_dual_ritual_function, holdable).
narrative_ontology:cs_axiom_grounding('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', trauma_transforms_through_dual_ritual_function, deontological).
narrative_ontology:cs_axiom('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', secondary, memory_is_adaptive_rehearsal).
narrative_ontology:cs_axiom_status(memory_is_adaptive_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', memory_is_adaptive_rehearsal, instrumental).
narrative_ontology:cs_reference_frame('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', integrated_remembrance_and_resilience).
narrative_ontology:cs_drift_state('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d82be4bd-1896-4d60-a8f5-5ae874dd1e82', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, historical_trauma_survivors).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, collective_memory_theory).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, ritual_efficacy_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, gaining both a connection to the past trauma and a sense of collective resilience. The ritual reinforces their identity and provides a framework for processing historical loss while adapting to present challenges.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, community_members, beneficiary,
    organized, biographical, identity_locked, local).

% Receive the transmitted memory of catastrophe and the adaptive strategies embedded in the ritual. Their identity is shaped by this inherited framework, making exit from the tradition difficult without losing a sense of self.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, future_generations, beneficiary,
    powerless, generational, identity_locked, local).

% Administer and interpret the ritual, ensuring its continuity and guiding the community through its dual functions of mourning and adaptation. They are responsible for maintaining the integrity of the tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Bear the emotional cost of reliving the trauma through ritual, but also find meaning and solidarity in the collective act of remembrance and the transmission of survival strategies. Their participation is essential for the ritual's authenticity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, historical_trauma_survivors, payer,
    moderate, biographical, identity_locked, local).

% Analyze the ritual's role in collective memory formation and cultural transmission, often contrasting its narrative with empirical historical accounts. They observe its function without direct participation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, secular_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory of a catastrophic event, allowing a community to simultaneously process grief and transmit adaptive strategies for future resilience, ensuring cultural continuity.
% TRANSFER_FUNCTION: Transfers historical memory, emotional processing, and practical survival knowledge across generations, from past trauma to present and future community members.
% ABSENT_VOICES: Those who reject the ritual's dual framing, perhaps emphasizing only the mourning or only the adaptive aspects, are often marginalized in the dominant narrative. They might argue for a more singular focus, but their perspectives are typically absorbed or dismissed by the hybrid framing.
% DISAPPEARANCE_RATIONALE: If this ritual vanished, the community would lose a central mechanism for collective identity, emotional processing of historical trauma, and intergenerational transmission of adaptive knowledge. The social fabric would be significantly altered, and the memory of the catastrophe might fragment or be lost.
% FOUNDING_PROBLEM: How to remember a catastrophic event and mourn its losses without being paralyzed by grief, while simultaneously ensuring the community develops and transmits the competence to survive and adapt to future threats.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and sociologists of religion, along with community elders and historians, corroborate that the problem of balancing remembrance with adaptation remains central to the community's ongoing existence. Their analyses from outside the immediate beneficiaries confirm the ritual's continued relevance.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.15) is low, representing the inherent cost of maintaining a complex, intergenerational ritual, not asymmetric extraction. Suppression (0.20) is also low, reflecting the social pressure to participate in a core identity-forming practice, but not coercive enforcement. Theater ratio (0.05) is minimal, as the ritual's functions are genuinely performed. Accessibility collapse is high (0.88) because for community members, opting out means losing a fundamental aspect of their identity and collective memory. Resistance is low (0.05) because the ritual is widely accepted as vital for cultural and spiritual continuity.
 *
 * PERSPECTIVAL GAP:
 *   While all participants generally benefit, the emotional burden on trauma survivors is a distinct cost. However, the hybrid reading argues that this 'cost' is transformed into a source of resilience and meaning through the ritual's adaptive function, making it a necessary component of the overall benefit. The analytical observer (secular historians) might focus more on the historical accuracy or social construction of memory, potentially overlooking the internal, lived experience of transformation.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are clear beneficiaries, gaining identity, memory, and adaptive capacity. Ritual leaders are agenda-setters, guiding the process. Historical trauma survivors, while benefiting, also bear the emotional cost of remembrance, placing them closer to a payer role. Secular historians are observers, analyzing the phenomenon without direct participation or benefit from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to remember catastrophe and foster survival) remains live. The hybrid reading prevents mislabeling the ritual as pure mourning (which might become a Piton if the trauma recedes) or pure survival training (which might become a Snare if it suppresses grief). By integrating both, it maintains a dynamic, relevant function, avoiding mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''hybrid_transformation_reading'' of the ''catastrophe_memory_function'' kernel?',
    'Comparative analysis with ''mourning_practice_reading'' and ''survival_competence_reading'' to confirm the distinct emphasis on integrated dual functions (D1/D4 + D5) rather than a singular focus.',
    'If misidentified, the classification would shift to reflect a different emphasis (e.g., higher extractiveness if only mourning is considered and enforced, or higher suppression if only survival is enforced without space for grief).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading of the catastrophe memory function kernel.').

omega_variable(
    balance_of_mourning_and_adaptation,
    'What is the precise balance and relative weight of the mourning-practice (D1/D4) versus survival-competence (D5) functions within the ritual, and does this balance shift over time or context?',
    'Longitudinal ethnographic studies and content analysis of ritual narratives across different historical periods or community subgroups.',
    'A significant shift towards one function over the other could alter the constraint''s classification. For instance, if mourning becomes purely performative without adaptive transmission, it might drift towards a Piton. If adaptation becomes purely instrumental, suppressing genuine grief, it might lean towards a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_mourning_and_adaptation, empirical, 'Assesses the dynamic equilibrium between the dual functions of the ritual.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds community members and future generations to this ritual, making ''identity_locked'' exit accurate?',
    'Sociological studies on collective identity formation, psychological analyses of trauma and group bonding, and interviews with individuals who have attempted to exit the community or tradition.',
    'If the identity lock is weaker than assessed, exit options might be ''constrained'' or ''mobile'', which would reduce the effective extraction (chi) for these agents, as their d-value would shift towards beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Clarifies the nature and strength of identity-based commitment to the ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 25, 0.04).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 75, 0.04).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 75, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 25, 0.19).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 75, 0.19).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
