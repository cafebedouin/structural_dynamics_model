% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Code as Normative Substrate (Practice Decline Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story represents the 'practice decline' reading of the
 *   honor satisfaction substrate kernel. It posits that the underlying honor
 *   code, which once legitimized dueling, largely persists as a normative
 *   substrate. However, the practice of dueling itself declined primarily due
 *   to exogenous factors: legal prohibitions, institutional barriers (e.g.,
 *   military codes forbidding duels), and rising opportunity costs. Dueling
 *   became impractical and legally risky, rather than culturally unthinkable.
 *   The constraint is classified as a Rope because it solved a coordination
 *   problem (reducing diffuse violence) but required active enforcement to
 *   shift behavior away from dueling.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.25).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Code as Normative Substrate (Practice Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '200276f4-f048-43c2-9b23-8e7869445ccf').
narrative_ontology:cs_kernel_codification('200276f4-f048-43c2-9b23-8e7869445ccf', implicit).
narrative_ontology:cs_authority_grounding('200276f4-f048-43c2-9b23-8e7869445ccf', extraction).
narrative_ontology:cs_interpretation_layer_present('200276f4-f048-43c2-9b23-8e7869445ccf').
narrative_ontology:cs_reading_relation('200276f4-f048-43c2-9b23-8e7869445ccf', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('200276f4-f048-43c2-9b23-8e7869445ccf', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('200276f4-f048-43c2-9b23-8e7869445ccf', foundational, honor_code_as_stable_substrate).
narrative_ontology:cs_axiom_status(honor_code_as_stable_substrate, holdable).
narrative_ontology:cs_axiom_grounding('200276f4-f048-43c2-9b23-8e7869445ccf', honor_code_as_stable_substrate, conventional).
narrative_ontology:cs_axiom('200276f4-f048-43c2-9b23-8e7869445ccf', foundational, exogenous_enforcement_as_primary_driver).
narrative_ontology:cs_axiom_status(exogenous_enforcement_as_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('200276f4-f048-43c2-9b23-8e7869445ccf', exogenous_enforcement_as_primary_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('200276f4-f048-43c2-9b23-8e7869445ccf', honor_code_legitimizing_dueling).
narrative_ontology:cs_drift_state('200276f4-f048-43c2-9b23-8e7869445ccf', post_legal_prohibition_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('200276f4-f048-43c2-9b23-8e7869445ccf', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, social_order_maintainers).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, individuals_avoiding_duels).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_bound_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal and institutional authorities (police, courts, military command) that actively enforced anti-dueling laws and created social barriers to dueling. They benefit from reduced violence and maintenance of state monopoly on force.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, social_order_maintainers, agenda_setter,
    institutional, generational, mobile, national).

% Individuals who, under the honor code, would feel compelled to duel to defend their reputation. They now face legal penalties and social ostracization for dueling, making the 'satisfaction' of honor through combat impractical. They bear the cost of suppressed recourse to dueling.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_bound_individuals, payer,
    moderate, biographical, constrained, local).

% Individuals who, while part of the honor culture, prefer to avoid the physical risks and social disruption of dueling. They benefit from the legal and institutional barriers that make dueling less accessible, allowing them to maintain honor without combat.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, individuals_avoiding_duels, beneficiary,
    moderate, immediate, mobile, local).

% Academics who study the evolution of honor codes and dueling practices. They analyze the historical data and interpret the causes of dueling's decline, providing the analytical framework for this constraint story.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code provided a mechanism for individuals to coordinate responses to perceived insults and maintain social standing, preventing diffuse, unpredictable violence by channeling it into ritualized combat. The exogenous enforcement coordinated society away from dueling as a legitimate form of conflict resolution.
% TRANSFER_FUNCTION: The constraint transfers the right to violent self-redress from individuals to the state, and the social cost of dueling (death, injury, social disruption) from society to the state's enforcement apparatus.
% ABSENT_VOICES: The 'dueling class' of the past, who would argue for the necessity of dueling as a means of honor satisfaction, are absent from contemporary discourse due to the success of exogenous enforcement. Their perspective is now largely historical.
% DISAPPEARANCE_RATIONALE: If the exogenous enforcement (laws, institutional barriers) against dueling vanished overnight, and the honor code remained as a substrate, there would be a significant risk of dueling re-emerging in some form, particularly in subcultures where honor remains paramount. Social norms around conflict resolution would shift, and the state's monopoly on violence would be challenged.
% FOUNDING_PROBLEM: The problem of uncontrolled violence and private retribution in societies where personal honor was paramount, leading to a need for ritualized conflict resolution (dueling) or its suppression by state authority.
% FOUNDING_PROBLEM_CORROBORATION: Cultural historians and legal scholars corroborate that while dueling as a practice is largely dead, the underlying problem of honor-based conflict and the state's role in managing violence remains live, albeit in different forms (e.g., gang violence, 'stand your ground' laws). The state's continued enforcement of its monopoly on violence attests to the persistence of this problem.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily redirects behavior rather than extracting resources, though it does extract the 'right' to private redress. Suppression is high (0.70) and rising, reflecting the increasing legal and institutional pressure against dueling. Theater ratio is low (0.10) because the enforcement was genuinely aimed at stopping dueling, not merely performing. Accessibility collapse is moderate (0.60) as alternatives to dueling (courts, social shaming) became more viable, but the underlying honor code still made dueling 'thinkable' for some. Resistance is low (0.15) because the enforcement was largely successful.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social order maintainers, the decline of dueling was a successful coordination effort to reduce violence. From the perspective of honor-bound individuals, it was a suppression of a legitimate means of defending reputation. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Social order maintainers (state, military) are beneficiaries (d near 0.0) as they gain control over violence. Individuals avoiding duels are also beneficiaries (d near 0.0) as they avoid personal risk. Honor-bound individuals are payers (d near 1.0) as their traditional means of honor satisfaction is suppressed. Cultural historians are observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the decline as a pure 'mountain' of cultural evolution. By emphasizing exogenous enforcement, it highlights that the constraint's persistence depends on active maintenance, not just natural cultural shifts. The honor code itself, as a substrate, is not fully resolved, but its most violent manifestation (dueling) was suppressed by a functional, if extractive, coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_code_persistence_degree,
    'To what extent did the honor code truly persist as a normative substrate, versus undergoing a more fundamental transformation (as argued by the cultural_contraction_reading)?',
    'Detailed sociological and anthropological studies of subcultures (e.g., military, Southern US) where honor codes remained strong, examining their internal logic and behavioral outcomes compared to historical dueling cultures.',
    'If the honor code underwent a more fundamental transformation, the constraint would lean towards a ''mountain'' of cultural evolution or a ''piton'' of vestigial norms, rather than a ''rope'' sustained by active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_code_persistence_degree, empirical, 'Ambiguity regarding the degree of honor code persistence versus transformation.').

omega_variable(
    causal_pathway_dominance,
    'Was the decline of dueling primarily due to exogenous enforcement, or were endogenous cultural shifts (e.g., rise of ''dignity culture'') equally or more causally dominant?',
    'Comparative historical analysis across societies with varying degrees of exogenous enforcement and cultural shifts, using counterfactual modeling to isolate causal contributions.',
    'If endogenous cultural shifts were dominant, the constraint would be closer to a ''mountain'' (cultural evolution) or a ''tangled_rope'' (internalized norms with diffuse extraction), rather than a ''rope'' driven by external suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_dominance, empirical, 'Ambiguity regarding the primary causal drivers of dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1800, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(hono_tr_t1830, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1830, 0.08).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(hono_tr_t1890, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(hono_tr_t1920, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement(hono_be_t1830, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1830, 0.3).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1860, 0.28).
narrative_ontology:measurement(hono_be_t1890, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1890, 0.27).
narrative_ontology:measurement(hono_be_t1920, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1920, 0.26).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1950, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(hono_su_t1830, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1830, 0.55).
narrative_ontology:measurement(hono_su_t1860, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1860, 0.65).
narrative_ontology:measurement(hono_su_t1890, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1890, 0.68).
narrative_ontology:measurement(hono_su_t1920, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1920, 0.69).
narrative_ontology:measurement(hono_su_t1950, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_satisfaction_substrate' kernel. This 'practice_decline_reading' emphasizes exogenous enforcement as the primary driver of dueling's decline, while the honor code persists as a substrate. It contrasts with the 'cultural_contraction_reading' (focus on endogenous cultural shifts) and the 'composite_overdetermined_reading' (focus on co-occurring, non-independent causes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
