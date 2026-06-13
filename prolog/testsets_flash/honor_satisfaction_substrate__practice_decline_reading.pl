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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Satisfaction Substrate (Practice Decline Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the decline of dueling as a practice due to
 *   exogenous enforcement, while the underlying honor code persists as a
 *   normative substrate. The 'practice_decline_reading' posits that dueling
 *   became impractical rather than unthinkable, due to legal prohibitions,
 *   institutional barriers (e.g., military codes shifting to courts-martial),
 *   and rising opportunity costs. The honor code itself did not fundamentally
 *   transform, but its expression was suppressed and redirected. This is one
 *   reading of the 'honor_satisfaction_substrate' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.2).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Satisfaction Substrate (Practice Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, 'e747cf03-d85f-4a9e-90d6-08dd4b248c9a').
narrative_ontology:cs_kernel_codification('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', implicit).
narrative_ontology:cs_authority_grounding('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', practice).
narrative_ontology:cs_interpretation_layer_present('e747cf03-d85f-4a9e-90d6-08dd4b248c9a').
narrative_ontology:cs_reading_relation('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', foundational, honor_code_persists_as_substrate).
narrative_ontology:cs_axiom_status(honor_code_persists_as_substrate, holdable).
narrative_ontology:cs_axiom_grounding('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', honor_code_persists_as_substrate, conventional).
narrative_ontology:cs_axiom('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', foundational, exogenous_enforcement_primary_driver).
narrative_ontology:cs_axiom_status(exogenous_enforcement_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', exogenous_enforcement_primary_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', honor_code_with_dueling_as_resolution).
narrative_ontology:cs_drift_state('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', post_legal_prohibition_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e747cf03-d85f-4a9e-90d6-08dd4b248c9a', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, state_legal_system).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, social_order_maintainers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_bound_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who, in a prior era, would have resorted to dueling to satisfy honor. They now face legal penalties and social stigma for such actions, forcing them to seek alternative, less direct means of honor satisfaction or to suppress the impulse entirely. Dueling remains thinkable but impractical.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_bound_individuals, payer,
    moderate, biographical, constrained, local).

% The primary enforcer of anti-dueling laws, imposing fines, imprisonment, and social sanctions. It benefits from the reduction of violence and the consolidation of its monopoly on legitimate force. Its actions directly suppressed the practice of dueling.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, state_legal_system, agenda_setter,
    institutional, generational, arbitrage, national).

% Community leaders, religious figures, and influential families who benefit from a more stable and less violent social environment. They actively discourage dueling and promote legal or social alternatives for conflict resolution, reinforcing the state's enforcement.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, social_order_maintainers, beneficiary,
    organized, generational, mobile, regional).

% Academics who study the evolution of honor codes and the decline of dueling. They analyze historical records, legal statutes, and cultural narratives to understand the mechanisms of change and persistence of the honor substrate.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a mechanism for individuals to coordinate on a public display of honor satisfaction, preventing endless cycles of insult and retaliation by establishing a clear, if violent, resolution protocol.
% TRANSFER_FUNCTION: Transferred the right to adjudicate honor disputes from individuals to the state and other social institutions, along with the associated social capital and control over violence.
% ABSENT_VOICES: Individuals who still believe in the absolute necessity of personal, violent honor satisfaction, but are marginalized by legal and social structures. They would argue that the current system fails to adequately address profound insults to honor.
% DISAPPEARANCE_RATIONALE: If the legal and social prohibitions against dueling vanished, and the opportunity costs disappeared, the practice of dueling would likely re-emerge in some form among certain groups, as the underlying honor code, though attenuated, still exists as a normative substrate. Social conflict resolution would shift.
% FOUNDING_PROBLEM: The problem of unchecked personal violence and private justice, where insults could escalate indefinitely without a recognized, if dangerous, means of resolution, leading to broader social instability.
% FOUNDING_PROBLEM_CORROBORATION: The state legal system and social order maintainers attest that the problem of violence and the need for regulated conflict resolution remains live. Cultural historians corroborate that while the specific form of dueling has declined, the underlying social dynamics of honor and insult persist, requiring ongoing management by formal and informal institutions.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).

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
 *   The constraint is classified as a Rope because it coordinates social order by suppressing a violent practice, with beneficiaries (state, social order maintainers) and payers (honor-bound individuals who lose a means of satisfaction). Extractiveness is low (0.2) because the primary function is coordination (reducing violence), not rent-seeking. Suppression is high (0.7) and increasing, reflecting the active and growing legal and institutional enforcement against dueling. Theater ratio is low (0.1) as the enforcement is genuinely aimed at suppressing the practice, not merely performing. Accessibility collapse is moderate (0.6) because while dueling is largely inaccessible, other forms of honor satisfaction or conflict resolution exist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state, the decline of dueling is a clear win for social order and the rule of law. From the perspective of honor-bound individuals, it represents a loss of a legitimate means of defending one's reputation, even if the alternatives are less violent. The constraint is experienced as a necessary imposition by the former, and a frustrating limitation by the latter.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal system and social order maintainers are beneficiaries (d near 0.0) as they gain from reduced violence and consolidated authority. Honor-bound individuals are payers (d near 1.0) as they bear the cost of suppressed traditional honor satisfaction. Cultural historians are analytical observers (d=0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_code_transformation_degree,
    'To what extent did the underlying ''honor code'' itself transform or contract, rather than merely having its expression suppressed?',
    'Comparative historical analysis of honor-related literature, legal cases, and social rituals across different periods and regions, focusing on shifts in the definition of honor and acceptable satisfaction.',
    'If the honor code underwent significant internal transformation (as per the ''cultural_contraction_reading''), the constraint would be closer to a Mountain (natural decline of a social practice) or a Rope (coordination on new norms), with lower suppression and extractiveness. If it merely shifted expression, this ''practice_decline_reading'' holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_code_transformation_degree, empirical, 'Ambiguity regarding the degree of endogenous cultural transformation versus exogenous suppression in the decline of dueling.').

omega_variable(
    causal_pathway_dominance,
    'Was the decline of dueling primarily due to exogenous enforcement (legal/institutional) or was it overdetermined by a combination of exogenous and endogenous factors?',
    'Counterfactual historical analysis and detailed case studies comparing regions with varying enforcement intensities and cultural shifts.',
    'If overdetermined (as per the ''composite_overdetermined_reading''), the ''practice_decline_reading'' would be an incomplete account, and the constraint''s classification might need to incorporate elements of both a Rope (for coordination on new norms) and a Snare (for active suppression of alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_dominance, conceptual, 'Whether exogenous enforcement was the dominant causal pathway or part of an overdetermined decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1800, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1850, 0.25).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1950, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(hono_su_t1950, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
