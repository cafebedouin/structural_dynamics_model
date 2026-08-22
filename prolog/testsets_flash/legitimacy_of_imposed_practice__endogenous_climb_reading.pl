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
 *   human_readable: Endogenous Climb for Imposed Practice
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint represents the 'endogenous climb' reading of
 *   state-imposed cultural practices, where genuine displacement requires
 *   bottom-up adoption and internalization. The story focuses on the failure
 *   of top-down mandates (like calendar or dress reforms) to achieve full
 *   cultural transformation without deep societal buy-in. The state's efforts
 *   are met with resistance and partial, often superficial, adoption, leading
 *   to a high theater ratio as enforcement continues despite limited genuine
 *   change. The constraint is claimed as a Piton because the primary function
 *   (cultural transformation) has atrophied, but the state continues
 *   performative maintenance.
 *
 * KEY AGENTS:
 *   - state_modernizers: Agenda-setter (institutional/constrained) — attempts to impose new practices.
 *   - communities_preserving_autonomy: Beneficiary (organized/identity_locked) — resists imposition, maintains traditions.
 *   - state_modernization_timeline: Payer (analytical/trapped) — bears the systemic cost of failed reforms.
 *   - urban_adopters: Beneficiary/Payer (moderate/mobile) — selectively adopts practices, often superficially.
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
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Endogenous Climb for Imposed Practice").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, 'ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf').
narrative_ontology:cs_kernel_codification('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', formalized).
narrative_ontology:cs_authority_grounding('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', practice).
narrative_ontology:cs_interpretation_layer_present('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf').
narrative_ontology:cs_reading_relation('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', foundational, practice_requires_internalization).
narrative_ontology:cs_axiom_status(practice_requires_internalization, holdable).
narrative_ontology:cs_axiom_grounding('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', practice_requires_internalization, empirically_contingent).
narrative_ontology:cs_axiom('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', secondary, top_down_mandate_insufficient_for_cultural_change).
narrative_ontology:cs_axiom_status(top_down_mandate_insufficient_for_cultural_change, holdable).
narrative_ontology:cs_axiom_grounding('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', top_down_mandate_insufficient_for_cultural_change, empirically_contingent).
narrative_ontology:cs_reference_frame('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', bottom_up_cultural_evolution).
narrative_ontology:cs_drift_state('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', post_imposition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec7cd3bf-56b1-45ce-a97f-1359bbd6e9bf', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state apparatus attempting to impose new practices (e.g., a new calendar or dress code) to align with a modernization agenda. They bear the cost of enforcement and the reputational cost of failed implementation, but their institutional identity is tied to the success of the reforms.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernizers, agenda_setter,
    institutional, generational, constrained, national).

% Local communities and cultural groups who resist the imposed practices, often maintaining traditional customs in private or through subtle forms of defiance. They benefit by preserving their cultural identity and autonomy, despite state efforts. Their 'exit' is the continued practice of their traditions.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy, beneficiary,
    organized, generational, identity_locked, local).

% The abstract timeline and goals of the state's modernization project. It 'pays' in terms of delays, resource expenditure, and the failure to achieve its targets for cultural transformation. This is an analytical agent representing the systemic cost of failed imposition.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline, payer,
    analytical, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).

% Individuals or groups in urban centers who adopt some imposed practices (e.g., new dress codes) due to social pressure, economic incentives, or perceived modernity, but may retain traditional practices in private. They benefit from social integration but may bear a cost of cultural dissonance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint attempts to coordinate a national identity and standardized social practices aligned with a state-led modernization vision, replacing diverse local customs with a unified, modern cultural framework.
% TRANSFER_FUNCTION: Transfers cultural capital, social legitimacy, and adherence from traditional practices to state-sanctioned modern practices. It also transfers enforcement resources from the state to the maintenance of these new norms, and time/effort from communities to resisting or selectively adopting them.
% ABSENT_VOICES: Historical and cultural scholars who would argue that top-down cultural imposition rarely achieves genuine, lasting transformation without deep societal buy-in, and that such efforts often lead to cultural loss and resentment rather than true modernization.
% DISAPPEARANCE_RATIONALE: If the state's efforts to impose new practices vanished overnight, traditional practices would likely re-emerge more openly, and the state's modernization timeline would be significantly altered, forcing a re-evaluation of its cultural transformation goals. The social landscape would revert to a more diverse, less centrally controlled set of customs.
% FOUNDING_PROBLEM: The state perceived a lack of national unity and 'backwardness' in diverse local customs, hindering its vision of a modern, unified nation-state capable of competing on the global stage.
% FOUNDING_PROBLEM_CORROBORATION: State archives and official histories attest to the founding problem as a live concern for national development. However, independent historians and anthropologists, from outside the benefiting state apparatus, argue that the 'problem' was often a pretext for consolidating power and that the original diversity was not inherently a barrier to progress, thus contesting its status.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.25) because the state expends resources on enforcement without achieving full compliance, and communities bear the cost of resistance. Suppression is moderate (0.4) as the state has coercive power but cannot fully suppress cultural practices. Theater ratio is high (0.6) because much of the state's activity is performative enforcement, masking the underlying failure of genuine internalization. Resistance is high (0.7) due to persistent community autonomy. Accessibility collapse is low (0.3) as communities find ways to maintain alternatives. The metrics reflect a constraint that is nominally enforced but largely ineffective in its primary goal, persisting more through inertia and performance than actual function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'state_modernizers', the constraint is a necessary (if challenging) Rope or Scaffold for national development. From the 'communities_preserving_autonomy', it is a Snare or Piton that attempts to extract their cultural identity. The 'endogenous_climb_reading' aligns more with the latter, emphasizing the difficulty of top-down cultural change.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'state_modernizers' are the agenda-setters, expending resources but failing to achieve their full goals, making their directionality closer to symmetric or even slightly negative due to the high theater. 'Communities_preserving_autonomy' are beneficiaries in this reading, as their resistance allows them to retain their cultural practices, effectively subsidizing their autonomy. The 'state_modernization_timeline' is an analytical victim, bearing the costs of delay and failure. 'Urban_adopters' are mixed, benefiting from social integration but paying through partial cultural compromise.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Piton because the original mandate (cultural transformation) has largely failed, yet the state continues to expend resources on its performative maintenance. The 'endogenous_climb_reading' highlights this mandatrophy by emphasizing that genuine change requires internalization, which was not achieved. The classification prevents mislabeling this as a functioning Rope or Scaffold, instead revealing its inertial persistence despite functional atrophy. The high theater ratio and persistent resistance are key indicators of this mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_measurement_ambiguity,
    'How can ''internalization'' of practice be reliably measured, distinct from superficial compliance or performative adoption?',
    'Longitudinal ethnographic studies, analysis of private vs. public practice, and linguistic shifts over multiple generations. If private practices persist unchanged for generations despite public compliance, internalization is low.',
    'If internalization is found to be consistently low, it strengthens the Piton classification and the ''endogenous_climb_reading''. If higher than assumed, it might shift towards a Tangled Rope or even a Scaffold, suggesting more effective (though still potentially extractive) transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_measurement_ambiguity, empirical, 'Distinguishing genuine cultural adoption from mere outward compliance.').

omega_variable(
    state_intent_vs_effect,
    'Was the state''s primary intent genuinely ''modernization'' and ''unity'', or was it primarily ''control'' and ''extraction'' of cultural autonomy, with modernization as a cover story?',
    'Analysis of internal state documents, comparative studies of similar reforms in other contexts, and the long-term outcomes for state power vs. societal well-being. If the reforms consistently led to increased state control and resource extraction without demonstrable societal benefit, the ''extraction'' intent is stronger.',
    'If the primary intent was extraction, the constraint''s base extractiveness would be re-evaluated upward, and its claimed type would shift more definitively towards a Snare, regardless of the ''endogenous climb'' dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_intent_vs_effect, conceptual, 'Ambiguity of state''s true motivation behind cultural imposition.').

omega_variable(
    reading_difference_on_causality,
    'Does the ''endogenous_climb_reading'' accurately capture the causal mechanism of practice displacement, or do sibling readings offer a more complete account?',
    'Comparative historical analysis across multiple cases of cultural imposition, evaluating which reading''s predictions (e.g., persistence of lunar calendar, partial dress adoption) are most consistently borne out by evidence. This would involve directly testing the ''exogenous_override_reading'' (state decree is sufficient) and ''hybrid_scaffolding_reading'' (mandate + ideology).',
    'If the ''exogenous_override_reading'' or ''hybrid_scaffolding_reading'' is found to be more causally accurate, it would challenge the core premise of this constraint, potentially leading to a reclassification of the underlying mechanism from Piton to a more actively enforced (and potentially more effective) Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_difference_on_causality, conceptual, 'This constraint is one reading of the ''legitimacy_of_imposed_practice'' kernel. This omega documents the structural difference between this reading and its siblings, specifically on the causal role of bottom-up adoption vs. top-down decree. The ''endogenous_climb_reading'' emphasizes the necessity of bottom-up adoption, while ''exogenous_override_reading'' emphasizes top-down decree, and ''hybrid_scaffolding_reading'' emphasizes a combination. The disagreement is located in the ''authority_grounding'' and ''kernel_codification'' of the underlying commitment system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 1920, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1920, 0.4).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1930, 0.5).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1940, 0.6).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1950, 0.65).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1960, 0.63).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1970, 0.6).
narrative_ontology:measurement(legi_tr_t1980, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1980, 0.6).

% Extraction over time
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1920, 0.3).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1930, 0.28).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1940, 0.25).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1950, 0.23).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1960, 0.24).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(legi_be_t1980, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1980, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1930, 0.45).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1940, 0.4).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1950, 0.38).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1960, 0.39).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(legi_su_t1980, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1980, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimacy_of_imposed_practice' kernel. This 'endogenous_climb_reading' focuses on the necessity of bottom-up adoption for successful practice displacement, contrasting with the 'exogenous_override_reading' (state decree is sufficient) and 'hybrid_scaffolding_reading' (mandate + ideology).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
