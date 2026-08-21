% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor Satisfaction Mechanism (Decline Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint story, 'Honor Satisfaction Mechanism (Decline Reading)',
 *   describes dueling as a practice that persisted at a declining frequency,
 *   gradually shifting from a central social institution to a fringe
 *   activity. The core idea is that the mechanism remained conceptually
 *   available for those committed to the honor code, even as its practical
 *   application became rarer and more legally suppressed. This reading
 *   emphasizes the gradual erosion of the practice's social legitimacy and
 *   frequency, rather than a sudden cognitive shift or a complex interplay of
 *   multiple, distinct mechanisms. The constraint's extractiveness declines
 *   as its social utility diminishes, while suppression by the state
 *   increases, and its theatricality (as a performance of a fading ideal)
 *   rises.
 *
 * KEY AGENTS:
 *   - gentlemanly_class_adherents: Primary beneficiary (organized/identity_locked) — benefits from symbolic capital
 *   - dueling_participants: Primary payer (moderate/constrained) — bears direct costs and risks
 *   - state_legal_system: Secondary payer (institutional/constrained) — bears enforcement costs
 *   - bourgeois_public_opinion: Excluded voice (organized/mobile) — exerts social pressure against the practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.45).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor Satisfaction Mechanism (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, 'a6e91462-1978-4f43-aa8d-7821744bb0af').
narrative_ontology:cs_kernel_codification('a6e91462-1978-4f43-aa8d-7821744bb0af', implicit).
narrative_ontology:cs_authority_grounding('a6e91462-1978-4f43-aa8d-7821744bb0af', practice).
narrative_ontology:cs_interpretation_layer_present('a6e91462-1978-4f43-aa8d-7821744bb0af').
narrative_ontology:cs_reading_relation('a6e91462-1978-4f43-aa8d-7821744bb0af', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6e91462-1978-4f43-aa8d-7821744bb0af', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('a6e91462-1978-4f43-aa8d-7821744bb0af', foundational, honor_demands_physical_satisfaction).
narrative_ontology:cs_axiom_status(honor_demands_physical_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('a6e91462-1978-4f43-aa8d-7821744bb0af', honor_demands_physical_satisfaction, conventional).
narrative_ontology:cs_axiom('a6e91462-1978-4f43-aa8d-7821744bb0af', secondary, state_law_subordinates_to_honor_code).
narrative_ontology:cs_axiom_status(state_law_subordinates_to_honor_code, overridden).
narrative_ontology:cs_axiom_grounding('a6e91462-1978-4f43-aa8d-7821744bb0af', state_law_subordinates_to_honor_code, conventional).
narrative_ontology:cs_reference_frame('a6e91462-1978-4f43-aa8d-7821744bb0af', gentlemanly_honor_code_primacy).
narrative_ontology:cs_drift_state('a6e91462-1978-4f43-aa8d-7821744bb0af', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a6e91462-1978-4f43-aa8d-7821744bb0af', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, gentlemanly_class_adherents).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, dueling_participants).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, state_legal_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the symbolic capital and social status derived from adherence to the honor code, even as dueling itself becomes rare. The mechanism provides a framework for resolving disputes that reinforces their social standing, even if rarely invoked.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, gentlemanly_class_adherents, beneficiary,
    organized, generational, identity_locked, national).

% Bears the direct costs and risks of dueling (injury, death, legal prosecution). Their participation is driven by social pressure and the perceived necessity of defending honor, despite the declining social acceptance of the practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, dueling_participants, payer,
    moderate, immediate, constrained, local).

% Incurs costs from prosecuting dueling, which is increasingly illegal. The system is forced to expend resources on a practice that is socially marginal but still occasionally occurs, creating a burden without clear benefit.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_legal_system, payer,
    institutional, generational, constrained, national).

% Increasingly views dueling as barbaric and irrational, exerting social pressure against it. While not directly targeted, their moral disapproval contributes to the declining social legitimacy of the practice, but they are not directly involved in its enforcement or participation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, bourgeois_public_opinion, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, albeit violent, mechanism for gentlemen to resolve disputes and restore perceived honor, preventing open-ended feuds and maintaining social order within a specific class.
% TRANSFER_FUNCTION: Transferred social status and perceived honor to participants who adhered to the code, while transferring risk of injury/death and legal penalties to the participants, and enforcement costs to the state.
% ABSENT_VOICES: The rising bourgeois public opinion, which increasingly condemned dueling as an anachronism, was largely excluded from the formal mechanisms of honor satisfaction, but their moral judgment eroded the practice's legitimacy.
% DISAPPEARANCE_RATIONALE: If the honor satisfaction mechanism vanished overnight, the gentlemanly class would need to find new, non-violent ways to resolve disputes and maintain social standing, leading to a reorganization of their social norms and practices.
% FOUNDING_PROBLEM: To provide a structured means for gentlemen to defend their honor and resolve grievances without resorting to uncontrolled violence or losing face, thereby maintaining social order within the elite.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and sociological analyses from outside the gentlemanly class confirm that the problem of honor defense has largely been subsumed by legal systems and changing social norms, rendering the dueling mechanism obsolete for its original purpose, though its symbolic echoes persist.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness declines over the interval as the social utility of dueling diminishes and its costs (legal, social) rise relative to its benefits. Suppression by the state legal system increases significantly, reflecting a hardening stance against the practice. The theater ratio rises, indicating that the remaining instances of dueling are increasingly performative acts of adherence to a fading code, rather than genuinely functional dispute resolution. Accessibility collapse is low because alternatives (legal recourse, social ostracism) are increasingly available, and resistance is low because the practice is already in decline.
 *
 * PERSPECTIVAL GAP:
 *   The gentlemanly class adherents experience this as a fading but still conceptually available mechanism for honor, providing symbolic benefits. Dueling participants, however, face increasing legal and social costs. The state legal system views it as a persistent, costly nuisance. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentlemanly_class_adherents are beneficiaries because the mechanism, even in decline, reinforces their social status and identity. Dueling_participants are payers due to the direct risks and legal consequences. The state_legal_system is also a payer, bearing the costs of enforcement against a declining but persistent practice. Bourgeois_public_opinion is excluded, as their moral stance is external to the mechanism's internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: its founding problem (structured honor defense) is 'dead', but the practice persists at a fringe level due to identity-locked adherence and the symbolic capital it still provides to a shrinking group. The rising theater ratio and increasing suppression by the state indicate that the constraint is maintained more by inertia and performance than by genuine coordination function. This prevents mislabeling it as a pure snare, as there is still a (declining) coordination function for the adherents, but highlights its degraded state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_vs_social_decline,
    'Did dueling become cognitively unthinkable (contraction_reading), or did it merely decline in frequency and social acceptance while remaining conceptually available (decline_reading)?',
    'Analysis of personal correspondence, legal records, and literary depictions for evidence of cognitive impossibility (e.g., expressions of genuine incomprehension at the practice) versus mere social disapproval or legal risk.',
    'If cognitively unthinkable, the constraint''s accessibility collapse would be much higher, and its persistence would be a piton of pure inertia. If merely declined, its current classification as a piton with residual adherence is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_vs_social_decline, conceptual, 'Distinguishing between a practice becoming unthinkable versus merely unpopular.').

omega_variable(
    multi_causal_vs_single_decline,
    'Was the decline of dueling due to a single, overarching process (decline_reading) or a composite of distinct, interacting mechanisms (composite_reading)?',
    'Detailed historical-sociological analysis disentangling the causal pathways of state legal suppression, the rise of bourgeois norms, the role of insurance, and the re-categorization of violence.',
    'If composite, this single constraint story would be an oversimplification, and the kernel should be decomposed into multiple, linked constraints, each representing a distinct causal mechanism. If single, this reading is sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_causal_vs_single_decline, empirical, 'Whether the decline was monocausal or multicausal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1750, 0.3).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1800, 0.45).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1850, 0.55).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1900, 0.6).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1700, 0.65).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1750, 0.58).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1800, 0.5).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1850, 0.47).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1900, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1750, 0.5).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1850, 0.65).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1900, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('decline_reading') of the 'honor_satisfaction_mechanism' kernel. Other readings ('contraction_reading', 'composite_reading') are distinct constraints that offer alternative explanations for the decline of dueling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
