% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism (Composite Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint represents the 'composite_reading' of the
 *   'honor_satisfaction_mechanism' kernel. It describes the historical
 *   erosion of dueling as a primary means of honor satisfaction, not as a
 *   simple decline, but as a complex process driven by multiple, interacting
 *   pressures: the rise of the state's monopoly on violence, the ascendance
 *   of bourgeois norms, the development of insurance as a risk management
 *   tool, and a fundamental recategorization of honor itself. The mechanism,
 *   in this reading, transforms from a direct, violent form of redress into a
 *   more diffuse system where honor is satisfied through legal means, social
 *   reputation, and economic stability, with ongoing, albeit shifted,
 *   coordination and extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.57).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.85).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Composite Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '500378bf-a0b3-4666-962b-d5f135f5f618').
narrative_ontology:cs_kernel_codification('500378bf-a0b3-4666-962b-d5f135f5f618', implicit).
narrative_ontology:cs_authority_grounding('500378bf-a0b3-4666-962b-d5f135f5f618', practice).
narrative_ontology:cs_interpretation_layer_present('500378bf-a0b3-4666-962b-d5f135f5f618').
narrative_ontology:cs_reading_relation('500378bf-a0b3-4666-962b-d5f135f5f618', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('500378bf-a0b3-4666-962b-d5f135f5f618', honor_satisfaction_mechanism__decline_reading, influences).
narrative_ontology:cs_axiom('500378bf-a0b3-4666-962b-d5f135f5f618', foundational, honor_satisfaction_is_multi_causal).
narrative_ontology:cs_axiom_status(honor_satisfaction_is_multi_causal, holdable).
narrative_ontology:cs_axiom_grounding('500378bf-a0b3-4666-962b-d5f135f5f618', honor_satisfaction_is_multi_causal, empirically_contingent).
narrative_ontology:cs_axiom('500378bf-a0b3-4666-962b-d5f135f5f618', secondary, state_monopoly_undermines_private_justice).
narrative_ontology:cs_axiom_status(state_monopoly_undermines_private_justice, holdable).
narrative_ontology:cs_axiom_grounding('500378bf-a0b3-4666-962b-d5f135f5f618', state_monopoly_undermines_private_justice, empirically_contingent).
narrative_ontology:cs_reference_frame('500378bf-a0b3-4666-962b-d5f135f5f618', traditional_honor_code).
narrative_ontology:cs_drift_state('500378bf-a0b3-4666-962b-d5f135f5f618', modern_state_formation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('500378bf-a0b3-4666-962b-d5f135f5f618', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_authorities).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_society).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_companies).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, aristocracy_nobility).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, individuals_seeking_redress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically relied on dueling for honor satisfaction and dispute resolution, maintaining social status. With the erosion of the mechanism, they lost a traditional means of asserting honor and faced legal and social penalties for attempting to maintain it. Their identity was deeply tied to this system.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, aristocracy_nobility, payer,
    powerful, generational, identity_locked, national).

% Actively suppressed dueling to establish a monopoly on legitimate violence and legal redress. Benefited from increased social order and centralized authority, integrating honor disputes into the legal system. They enforced new laws and norms.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Promoted new norms of civility, legalism, and economic rationality that undermined the honor code based on dueling. Benefited from increased social stability, reduced violence, and a moral order aligned with their values. They exerted social pressure against dueling.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_society, beneficiary,
    organized, generational, mobile, national).

% Emerged as a new mechanism for managing risk, including personal injury and death, offering an alternative to the direct, violent resolution of honor disputes. They benefited from a new market for their services as society shifted away from dueling.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_companies, beneficiary,
    organized, biographical, arbitrage, national).

% Individuals who felt their honor was impugned still sought satisfaction, but faced a landscape where dueling was illegal and socially condemned. They bore the costs of navigating new, often less direct or satisfying, legal and social avenues for redress.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, individuals_seeking_redress, payer,
    moderate, immediate, constrained, local).

% Advocated for the abolition of dueling on moral and religious grounds, contributing to the shift in social norms. They actively campaigned against the practice and promoted alternative forms of conflict resolution.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, moral_reformers, agenda_setter,
    organized, generational, mobile, national).

% Analyzed the historical decline of dueling and the rise of state legal systems, documenting the structural recategorization of honor and justice. They provided intellectual frameworks for understanding the transformation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, state_authorities).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, the mechanism coordinated social status, dispute resolution, and personal reputation among elites. As it eroded, new mechanisms (state law, bourgeois norms, insurance) took over these functions, coordinating a different, more centralized and legally mediated social order.
% TRANSFER_FUNCTION: Historically, it transferred the risk of death or injury from social slights to participants, and status/reputation to those who successfully navigated it. In its eroded, composite form, it transfers social control over violence to the state, imposes costs on those who do not conform to new norms, and creates new markets for risk management.
% ABSENT_VOICES: Those who, in the transitional period, felt their honor could not be adequately satisfied by legal or bourgeois means, and were left without a legitimate avenue for redress, often facing social ostracization or legal penalty for attempting to uphold older codes.
% DISAPPEARANCE_RATIONALE: If the entire composite honor satisfaction mechanism (including its historical forms and modern replacements) vanished, the fundamental ways societies manage status, reputation, and conflict would be profoundly altered, leading to a complete reorganization of social and legal structures.
% FOUNDING_PROBLEM: To provide a means of dispute resolution and status maintenance for elites outside formal legal systems, ensuring personal honor could be defended and social hierarchy maintained through a recognized, albeit violent, code.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal prohibitions, and sociological analyses from outside the aristocratic class confirm the decline of dueling and the rise of alternative mechanisms, indicating the original problem it solved is no longer addressed by that specific means.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.57, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'tangled_rope' because the composite mechanism still coordinates social order (albeit a transformed one) while exhibiting asymmetric extraction. The extractiveness (0.57 at end) reflects the ongoing social costs and transfers associated with maintaining honor in the new system, which are lower than direct dueling but still significant. Suppression (0.85 at end) is high due to active state prohibition and strong social condemnation of dueling. Theater ratio (0.50 at end) increases as dueling becomes more symbolic or ritualistic, and the 'performance' of honor shifts to other arenas. Accessibility collapse (0.30) is low because alternatives to dueling became widely available. Resistance (0.70) reflects the historical resistance to dueling that ultimately led to its decline.
 *
 * PERSPECTIVAL GAP:
 *   The state and bourgeois society would perceive the transformation of the honor satisfaction mechanism as a positive evolution towards a more civilized and orderly society, a coordination success. For the aristocracy, it represents a loss of status and a coercive imposition on their traditional way of life, an extractive process. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities and bourgeois society are beneficiaries, gaining social control and stability. Insurance companies also benefit from new markets. The aristocracy, once primary beneficiaries of dueling, become victims as their traditional means of honor satisfaction are criminalized and socially condemned. Individuals seeking redress are payers, navigating a system that no longer offers the direct, albeit risky, satisfaction of dueling.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_erosion,
    'What was the relative causal weight of state prohibition, bourgeois norms, and economic factors (like insurance) in the erosion of dueling?',
    'Comparative historical analysis across different national contexts with varying legal and social pressures, or counterfactual historical modeling.',
    'If state prohibition was primary, the constraint''s suppression is more structurally imposed; if bourgeois norms, more internalized. If economic factors, the shift is more a market-driven adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_erosion, empirical, 'Relative importance of different factors in dueling''s decline.').

omega_variable(
    cognitive_vs_structural_shift,
    'To what extent did dueling become ''cognitively unthinkable'' (as per the contraction_reading) versus merely ''structurally impossible/prohibited'' (as per this composite_reading)?',
    'Analysis of personal diaries, literature, and philosophical texts from the period to gauge shifts in individual moral psychology and social imagination regarding dueling''s legitimacy.',
    'If primarily cognitive, the constraint''s suppression is more internalized and less dependent on active enforcement; if primarily structural, enforcement remains key.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_vs_structural_shift, conceptual, 'Distinguishing cognitive impossibility from structural prohibition.').

omega_variable(
    persistence_of_honor_code,
    'How much of the original honor code''s underlying logic persists in contemporary forms of social redress or status maintenance, even without dueling?',
    'Sociological studies of modern conflict resolution, reputation management, and legal disputes, tracing continuities in the ''grammar'' of honor.',
    'If significant persistence, the ''tangled_rope'' classification is more robust; if minimal, the constraint might be closer to a ''piton'' or fully dissolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_of_honor_code, empirical, 'Tracing the continuity of honor code logic in modern society.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1725, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1725, 0.15).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1750, 0.2).
narrative_ontology:measurement(hono_tr_t1775, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1775, 0.25).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(hono_tr_t1825, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1825, 0.35).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1850, 0.4).
narrative_ontology:measurement(hono_tr_t1875, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1875, 0.45).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.5).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1700, 0.75).
narrative_ontology:measurement(hono_be_t1725, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1725, 0.72).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1750, 0.69).
narrative_ontology:measurement(hono_be_t1775, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1775, 0.67).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement(hono_be_t1825, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1825, 0.63).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1850, 0.61).
narrative_ontology:measurement(hono_be_t1875, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1875, 0.59).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.57).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement(hono_su_t1725, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1725, 0.58).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1750, 0.65).
narrative_ontology:measurement(hono_su_t1775, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1775, 0.7).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(hono_su_t1825, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1825, 0.78).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement(hono_su_t1875, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1875, 0.82).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_satisfaction_mechanism' kernel. This 'composite_reading' emphasizes multiple, interacting causal factors in its erosion, providing a richer explanation for the observed decline (decline_reading) and the eventual cognitive impossibility (contraction_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
