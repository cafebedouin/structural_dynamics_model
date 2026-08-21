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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Decline of Dueling due to Exogenous Enforcement (Practice Decline Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story analyzes the decline of dueling as a social
 *   practice, specifically from the 'practice decline' reading of the honor
 *   satisfaction substrate kernel. This reading posits that the underlying
 *   honor code persists as a normative substrate, but the practice of dueling
 *   itself declined primarily due to exogenous enforcement, such as legal
 *   prohibitions, institutional barriers, and rising opportunity costs,
 *   rather than a fundamental transformation of the honor code itself. The
 *   constraint being classified is the *exogenous enforcement* that made
 *   dueling impractical.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.65).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.8).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Decline of Dueling due to Exogenous Enforcement (Practice Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, 'b7f66704-1209-4042-9397-f282e4ba3c29').
narrative_ontology:cs_kernel_codification('b7f66704-1209-4042-9397-f282e4ba3c29', implicit).
narrative_ontology:cs_authority_grounding('b7f66704-1209-4042-9397-f282e4ba3c29', practice).
narrative_ontology:cs_interpretation_layer_present('b7f66704-1209-4042-9397-f282e4ba3c29').
narrative_ontology:cs_reading_relation('b7f66704-1209-4042-9397-f282e4ba3c29', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7f66704-1209-4042-9397-f282e4ba3c29', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('b7f66704-1209-4042-9397-f282e4ba3c29', foundational, honor_demands_satisfaction).
narrative_ontology:cs_axiom_status(honor_demands_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('b7f66704-1209-4042-9397-f282e4ba3c29', honor_demands_satisfaction, deontological).
narrative_ontology:cs_axiom('b7f66704-1209-4042-9397-f282e4ba3c29', foundational, state_monopoly_on_violence).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence, holdable).
narrative_ontology:cs_axiom_grounding('b7f66704-1209-4042-9397-f282e4ba3c29', state_monopoly_on_violence, conventional).
narrative_ontology:cs_reference_frame('b7f66704-1209-4042-9397-f282e4ba3c29', honor_code_as_social_substrate).
narrative_ontology:cs_drift_state('b7f66704-1209-4042-9397-f282e4ba3c29', post_legal_prohibition_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b7f66704-1209-4042-9397-f282e4ba3c29', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, society_at_large).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, legal_authorities).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, individuals_bound_by_honor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, military_officers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the reduction of violence and social disorder associated with dueling. Has largely moved on from dueling as a legitimate practice, accepting legal prohibitions.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, society_at_large, beneficiary,
    organized, generational, mobile, national).

% Enforces the legal prohibition against dueling, maintaining the state's monopoly on violence. Views dueling as a criminal act and a threat to public order.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Historically felt compelled by the honor code to seek satisfaction through dueling, but faced increasing legal penalties, social ostracization, and practical barriers. Their 'right' to duel was extracted by exogenous forces.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, individuals_bound_by_honor, payer,
    powerless, biographical, identity_locked, local).

% Operated under attenuated honor codes where dueling was still a theoretical, if increasingly impractical, means of satisfaction. Faced institutional pressure against dueling but still valued honor highly.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_officers, beneficiary).

% Study the historical evolution of honor codes and the decline of dueling, analyzing the interplay of social norms, legal enforcement, and cultural shifts.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, historians_anthropologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate society away from private, violent resolution of honor disputes and towards state-sanctioned legal and social mechanisms for conflict resolution.
% TRANSFER_FUNCTION: Transfers the right to violent self-redress from individuals to the state, and transfers the social costs of violence (death, injury, disorder) from society to individuals who might have dueled (via legal penalties and social stigma).
% ABSENT_VOICES: Those who, even after legal prohibition, believed dueling was a necessary and legitimate means of upholding personal honor and social standing. Their voices were silenced by legal and social enforcement.
% DISAPPEARANCE_RATIONALE: If the exogenous enforcement (legal prohibition, social stigma) against dueling vanished overnight, it is unlikely dueling would return to its historical prevalence, but some forms of violent honor satisfaction might re-emerge in specific subcultures or contexts, challenging the state's monopoly on violence and requiring society to re-coordinate around conflict resolution.
% FOUNDING_PROBLEM: The widespread social disorder, violence, and loss of life caused by dueling as a common practice for resolving honor disputes.
% FOUNDING_PROBLEM_CORROBORATION: Legal statutes, historical records of dueling fatalities, and sociological analyses of violence and social order corroborate the problem. While dueling itself is rare, the underlying problem of private violence and the state's role in its suppression remains live, attested by legal scholars and public safety advocates.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `claimed_type` is 'rope' as per the prompt's instruction, reflecting the intended coordination function of legal prohibitions to move society away from dueling. However, the metrics reflect the actual operation of this enforcement: `extractiveness` is high (0.65) for individuals who felt compelled by honor but were denied the means of satisfaction; `suppression` is very high (0.80) due to legal penalties and social stigma; and `accessibility_collapse` is also high (0.85) as dueling became practically impossible. `Resistance` is low (0.30) because active opposition to the prohibition was limited, as society largely accepted the shift. The `theater_ratio` is low (0.10) because the enforcement was genuine and effective, not merely performative. The temporal measurements show a clear increase in extractiveness and suppression over the 18th and 19th centuries as legal and social pressures against dueling intensified.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of society and legal authorities, the constraint operates as a beneficial 'rope' that coordinates away from violence. However, from the perspective of individuals historically bound by honor, the same constraint functions as a highly suppressive and extractive force, denying them a perceived necessary means of honor satisfaction. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and legal authorities are beneficiaries, as they gain from reduced violence and increased social order (low directionality). Individuals historically bound by honor are the primary targets, as the constraint extracts their 'right' to duel and imposes costs for non-compliance (high directionality). Military officers, while still valuing honor, faced institutional constraints, placing them in a mixed position. Historians and anthropologists are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_code_persistence_vs_transformation,
    'Does the honor code truly persist as a normative substrate, or has it undergone a fundamental transformation, making dueling unthinkable rather than merely impractical?',
    'Comparative historical analysis of social norms and values across different eras and cultures, examining the internal logic and emotional salience of honor in contemporary contexts versus historical ones. This would involve detailed textual analysis of primary sources and ethnographic studies of ''cultures of honor'' where they still exist.',
    'If the honor code has fundamentally transformed (as per the ''cultural contraction'' reading), then the constraint''s classification might shift towards a ''mountain'' (if the new norm is fixed) or ''piton'' (if the old code is merely theatrical). If it persists, this ''practice decline'' reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_code_persistence_vs_transformation, conceptual, 'Ambiguity regarding the true nature of the honor code''s evolution.').

omega_variable(
    causal_primacy_of_exogenous_vs_endogenous_factors,
    'Was the decline of dueling primarily due to exogenous enforcement, or did endogenous delegitimation of the honor code also play a significant, independent causal role?',
    'Counterfactual historical analysis and detailed case studies comparing regions with varying degrees of legal enforcement and internal cultural shifts. This would involve disentangling the causal pathways and assessing their relative weights, potentially using methods from historical sociology.',
    'If endogenous delegitimation was a strong, independent factor (as per the ''composite overdetermined'' reading), the constraint''s classification might lean more towards a ''tangled_rope'' or ''snare'' if the internal transformation involved new forms of social extraction or suppression. If exogenous factors were dominant, this ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_exogenous_vs_endogenous_factors, empirical, 'Debate over the primary drivers of dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1700, 0.3).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1750, 0.45).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1800, 0.6).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1850, 0.63).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1900, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1850, 0.78).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1900, 0.8).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1700, tn=1900
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(class), 1700, 0.3).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(class), 1900, 0.75).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(individual), 1700, 0.2).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(individual), 1900, 0.85).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(organizational), 1700, 0.25).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(organizational), 1900, 0.8).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(structural), 1700, 0.4).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(structural), 1900, 0.9).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_substrate__practice_decline_reading, resistance(class), 1700, 0.4).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_substrate__practice_decline_reading, resistance(class), 1900, 0.2).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_substrate__practice_decline_reading, resistance(individual), 1700, 0.6).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_substrate__practice_decline_reading, resistance(individual), 1900, 0.1).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_substrate__practice_decline_reading, resistance(organizational), 1700, 0.5).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_substrate__practice_decline_reading, resistance(organizational), 1900, 0.15).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_substrate__practice_decline_reading, resistance(structural), 1700, 0.2).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_substrate__practice_decline_reading, resistance(structural), 1900, 0.05).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(class), 1700, 0.5).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(class), 1900, 0.8).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(individual), 1700, 0.7).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(individual), 1900, 0.9).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(organizational), 1700, 0.6).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(organizational), 1900, 0.85).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(structural), 1700, 0.4).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(structural), 1900, 0.95).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_substrate__practice_decline_reading, suppression(class), 1700, 0.4).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_substrate__practice_decline_reading, suppression(class), 1900, 0.7).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_substrate__practice_decline_reading, suppression(individual), 1700, 0.3).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_substrate__practice_decline_reading, suppression(individual), 1900, 0.8).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_substrate__practice_decline_reading, suppression(organizational), 1700, 0.35).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_substrate__practice_decline_reading, suppression(organizational), 1900, 0.75).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_substrate__practice_decline_reading, suppression(structural), 1700, 0.5).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_substrate__practice_decline_reading, suppression(structural), 1900, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor satisfaction substrate' kernel, focusing on the decline of dueling due to exogenous enforcement. It is linked to sibling readings that emphasize cultural contraction or a composite of factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
