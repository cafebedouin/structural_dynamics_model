% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Dueling as Cognitively Unthinkable (Contraction Reading)
 *   domain: historical_sociology/normative_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'contraction reading' of the honor
 *   satisfaction mechanism, arguing that dueling became a 'category-level
 *   impossibility' – not merely suppressed, but cognitively unthinkable. This
 *   reading emphasizes a fundamental shift in social cognition and normative
 *   frameworks, rather than just a decline in frequency or a composite of
 *   external factors. The constraint itself is the internalized social norm
 *   that renders dueling an unthinkable act, coordinating a new social
 *   reality where this form of violence is absent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.05).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.95).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Dueling as Cognitively Unthinkable (Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/normative_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '49121d1c-6a23-4844-af1e-80259ea37743').
narrative_ontology:cs_kernel_codification('49121d1c-6a23-4844-af1e-80259ea37743', implicit).
narrative_ontology:cs_authority_grounding('49121d1c-6a23-4844-af1e-80259ea37743', practice).
narrative_ontology:cs_interpretation_layer_present('49121d1c-6a23-4844-af1e-80259ea37743').
narrative_ontology:cs_reading_relation('49121d1c-6a23-4844-af1e-80259ea37743', honor_satisfaction_mechanism__decline_reading, influences).
narrative_ontology:cs_reading_relation('49121d1c-6a23-4844-af1e-80259ea37743', honor_satisfaction_mechanism__composite_reading, forecloses).
narrative_ontology:cs_axiom('49121d1c-6a23-4844-af1e-80259ea37743', foundational, dueling_is_category_error).
narrative_ontology:cs_axiom_status(dueling_is_category_error, holdable).
narrative_ontology:cs_axiom_grounding('49121d1c-6a23-4844-af1e-80259ea37743', dueling_is_category_error, deontological).
narrative_ontology:cs_axiom('49121d1c-6a23-4844-af1e-80259ea37743', foundational, social_reality_is_constituted_by_cognition).
narrative_ontology:cs_axiom_status(social_reality_is_constituted_by_cognition, holdable).
narrative_ontology:cs_axiom_grounding('49121d1c-6a23-4844-af1e-80259ea37743', social_reality_is_constituted_by_cognition, conventional).
narrative_ontology:cs_reference_frame('49121d1c-6a23-4844-af1e-80259ea37743', post_dueling_social_order).
narrative_ontology:cs_drift_state('49121d1c-6a23-4844-af1e-80259ea37743', post_enlightenment_era, gap(stable, severe, true)).
narrative_ontology:cs_created_at('49121d1c-6a23-4844-af1e-80259ea37743', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, society_at_large).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, state_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, former_dueling_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of endemic personal violence and the stability of a social order where honor is not satisfied through dueling. Participates in the collective maintenance of the norm through socialization and cultural reinforcement.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, society_at_large, beneficiary,
    organized, generational, mobile, national).

% Benefits from the consolidation of its monopoly on legitimate violence, no longer challenged by private honor codes. Historically enforced anti-dueling laws, but now primarily benefits from the internalized norm that makes such enforcement largely unnecessary.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Historically, this class bore the costs of losing a culturally sanctioned means of honor satisfaction. Their identity was deeply intertwined with the practice, making the shift away from dueling a profound loss of social capital and a redefinition of self. They are now largely historical figures, their descendants having internalized the new norm.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, former_dueling_class, payer,
    powerful, biographical, identity_locked, national).

% Analyze the historical processes and social mechanisms through which dueling became unthinkable. They interpret the evidence and contribute to the ongoing understanding of this normative shift, without directly participating in the constraint's operation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, legal_scholars_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a social reality where personal violence for honor is no longer a legitimate or thinkable means of conflict resolution, establishing the state's monopoly on violence and fostering a more stable civic order.
% TRANSFER_FUNCTION: Transfers the right to violence from individuals to the state, and shifts the basis of social honor from personal combat to other forms of social capital and civic engagement.
% ABSENT_VOICES: The historical voices of those who passionately defended dueling as a necessary mechanism for maintaining personal honor and social standing. Their arguments are now largely relegated to historical study, having been superseded by a fundamental shift in social cognition.
% DISAPPEARANCE_RATIONALE: If the cognitive impossibility of dueling vanished overnight, and it became a thinkable, legitimate option for resolving disputes, the social fabric around conflict resolution, state authority, and personal honor would undergo a profound and potentially violent rearrangement. The state's monopoly on violence would be challenged, and new forms of social order would emerge.
% FOUNDING_PROBLEM: The problem of endemic personal violence, challenges to state authority, and social instability posed by an honor culture that sustained dueling as a legitimate form of conflict resolution.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, sociologists, and political scientists corroborate the historical problem of dueling and the ongoing societal need for a state monopoly on violence and peaceful conflict resolution. Their analyses, independent of the state or former dueling classes, support the view that the problem dueling solved (or exacerbated) is still relevant in terms of maintaining social order.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).
:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it represents a stable, coordinated social reality (the absence of dueling) that benefits society at large. Its extractiveness is very low (0.05) because the constraint itself is the *absence* of a practice, not an active extraction mechanism. However, suppression is very high (0.95) and accessibility collapse is near total (0.98) because the core claim is that dueling became 'cognitively unthinkable' – the very idea is suppressed by internalized norms, making alternatives (dueling) virtually inaccessible. Resistance is negligible (0.02) as the norm is deeply internalized. The temporal measurements reflect the gradual but profound shift from a period where dueling was still somewhat present (higher extractiveness, lower suppression) to a state of cognitive impossibility (very low extractiveness, very high suppression).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of society at large, the constraint is a beneficial coordination mechanism. From the historical perspective of the former dueling class, it represents a profound loss and redefinition of their social world. The engine's classification will reflect the overall low extractiveness of the *absence* of dueling, while acknowledging the high suppression of the *option* of dueling.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and state authority are beneficiaries, gaining from the stability and reduced violence. The 'former dueling class' are payers, having lost a central aspect of their social identity and means of honor satisfaction. Legal scholars and historians serve as analytical observers, studying the phenomenon without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_shift_vs_external_factors,
    'To what extent was the disappearance of dueling primarily driven by a cognitive, category-level shift, versus a combination of external factors (e.g., state legal enforcement, changing bourgeois norms, insurance practices)?',
    'Detailed historical-sociological analysis comparing the explanatory power of internal cognitive shifts against the aggregate impact of multiple external mechanisms, potentially using counterfactual historical modeling.',
    'If external factors are found to be dominant, the ''contraction reading'' would be weakened, potentially shifting the constraint''s classification towards a ''composite reading'' (e.g., a Tangled Rope of state enforcement and social norms). If the cognitive shift is robustly demonstrated, this reading''s emphasis on internalized suppression and low extractiveness would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_shift_vs_external_factors, empirical, 'The primary driver of dueling''s disappearance: cognitive shift or external factors.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (dueling being ''cognitively unthinkable'') structural (e.g., lack of social support for dueling) or internalized (e.g., individuals genuinely cannot conceive of dueling as an option)?',
    'Analysis of historical narratives, personal diaries, and cultural artifacts to discern the extent of genuine internal impossibility versus external social pressure. If suppression persists even in contexts where external barriers are low, it suggests internalization.',
    'If primarily internalized, the constraint''s effective suppression is higher and more robust than purely structural measures suggest, reinforcing the ''Rope'' classification for a stable, self-enforcing norm. If more structural, the constraint might lean towards a ''Tangled Rope'' if active enforcement was still required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dueling''s unthinkability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1700, 0.15).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1850, 0.06).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1800, 0.08).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1850, 0.06).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1750, 0.75).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1850, 0.92).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1900, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_satisfaction_mechanism' kernel, focusing on the cognitive shift that made dueling unthinkable. It is distinct from the 'decline_reading' (statistical frequency) and the 'composite_reading' (multiple causal factors).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
