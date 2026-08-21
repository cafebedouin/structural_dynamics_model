% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor Code as Unthinkable Dueling Substrate (Cultural Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the 'cultural contraction' reading of
 *   the 'honor_satisfaction_substrate' kernel. It posits that the honor code
 *   itself underwent a foundational transformation, shifting from 'cultures
 *   of honor' (where dueling was a thinkable, even required, means of
 *   satisfaction) to 'cultures of dignity' (where dueling became
 *   unthinkable). The constraint, in this reading, is the emergent cultural
 *   reality of 'unthinkability' itself, which operates as a mountain, rather
 *   than a actively enforced or extractive mechanism. The metrics reflect the
 *   end-state of this transformation, while the temporal measurements trace
 *   the erosion of the old honor code and the emergence of the new cultural
 *   mountain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.1).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Code as Unthinkable Dueling Substrate (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '2f0d0bee-dfd2-4980-aff8-381c5de5240f').
narrative_ontology:cs_kernel_codification('2f0d0bee-dfd2-4980-aff8-381c5de5240f', implicit).
narrative_ontology:cs_authority_grounding('2f0d0bee-dfd2-4980-aff8-381c5de5240f', practice).
narrative_ontology:cs_reading_relation('2f0d0bee-dfd2-4980-aff8-381c5de5240f', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('2f0d0bee-dfd2-4980-aff8-381c5de5240f', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('2f0d0bee-dfd2-4980-aff8-381c5de5240f', foundational, honor_is_internal_dignity).
narrative_ontology:cs_axiom_status(honor_is_internal_dignity, holdable).
narrative_ontology:cs_axiom_grounding('2f0d0bee-dfd2-4980-aff8-381c5de5240f', honor_is_internal_dignity, deontological).
narrative_ontology:cs_axiom('2f0d0bee-dfd2-4980-aff8-381c5de5240f', foundational, violence_is_unthinkable_for_satisfaction).
narrative_ontology:cs_axiom_status(violence_is_unthinkable_for_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('2f0d0bee-dfd2-4980-aff8-381c5de5240f', violence_is_unthinkable_for_satisfaction, conventional).
narrative_ontology:cs_reference_frame('2f0d0bee-dfd2-4980-aff8-381c5de5240f', cultures_of_dignity_framework).
narrative_ontology:cs_drift_state('2f0d0bee-dfd2-4980-aff8-381c5de5240f', post_cultural_transformation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('2f0d0bee-dfd2-4980-aff8-381c5de5240f', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, individuals_in_dignity_culture).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, former_honor_bound_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a social order where violent resolution of honor disputes is unthinkable, fostering greater stability and personal security. Their identity and social standing are deeply intertwined with the norms of this 'dignity culture', making exit from its framework inconceivable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, individuals_in_dignity_culture, beneficiary,
    organized, generational, identity_locked, national).

% Bear the cost of adapting to new social norms where their traditional means of honor satisfaction (dueling) are no longer viable or respected. Their social identity, once defined by the 'culture of honor', is now anachronistic, leading to a loss of status or relevance.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, former_honor_bound_elites, payer,
    moderate, biographical, identity_locked, local).

% Analyze the historical transformation of honor codes and the societal shift from 'cultures of honor' to 'cultures of dignity', documenting the mechanisms and consequences of this cultural contraction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__cultural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared cultural understanding that violence is an unthinkable means for resolving personal affronts, thereby coordinating social interactions towards non-violent forms of dispute resolution and status maintenance.
% TRANSFER_FUNCTION: Transfers the social cost of violent conflict (injury, death, legal repercussions) away from individuals and society, replacing it with the diffuse social costs of maintaining a 'dignity culture' (e.g., increased emphasis on self-restraint, legal recourse, and emotional regulation).
% ABSENT_VOICES: The 'culture of honor' itself, as a collective entity, is absent from the contemporary discourse, having been superseded. Its proponents, if they could speak, would argue for the necessity of violent satisfaction for maintaining personal and family reputation.
% DISAPPEARANCE_RATIONALE: If the cultural constraint making dueling unthinkable vanished overnight, it would imply a reversion to a 'culture of honor' where dueling might again become thinkable. However, the current social substrate is so fundamentally altered that such a reversion is highly improbable; the 'unthinkability' is deeply embedded, making the world effectively unchanged by its 'disappearance' because it is a fundamental aspect of the current social reality.
% FOUNDING_PROBLEM: The problem of pervasive violence and instability arising from a social system that mandated or encouraged dueling as a means of honor satisfaction.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, sociological analyses, and legal archives from outside the former 'honor-bound' elites corroborate that the problem of dueling-related violence is largely resolved due to a fundamental cultural shift, not merely legal prohibition. Contemporary society generally views dueling as an archaic and unacceptable practice.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base properties reflect the end-state of the cultural transformation by 1900: dueling is unthinkable, hence extractiveness, suppression, and theater are very low. Accessibility collapse is very high because the cultural substrate no longer provides a 'path' for dueling. Resistance is low because there's no active constraint to resist. The temporal measurements show a clear decline in extractiveness, suppression, and theater, and a rise in accessibility collapse, illustrating the process of 'mountain erosion' as the old honor code dissolved and the new cultural reality solidified.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the historical 'culture of honor' (where dueling was a legitimate, often coercive, constraint) and the 'culture of dignity' (where dueling is unthinkable). This story focuses on the latter, emergent mountain, acknowledging the former as the historical context from which it emerged. The engine's classification of this as a mountain, despite its historical origins in a potentially extractive system, captures the profound nature of the cultural shift.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals in a 'dignity culture' are beneficiaries of a social order free from dueling, but their 'benefit' is diffuse and non-extractive, reflecting the nature of a cultural mountain. Former honor-bound elites are 'payers' in the sense of losing their traditional social capital and having to adapt to new norms. Cultural historians are observers, analyzing the shift.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_legal_causation,
    'Was the ''unthinkability'' of dueling primarily caused by an endogenous cultural transformation (as this reading claims), or by exogenous legal and institutional suppression (as the ''practice_decline_reading'' suggests)?',
    'Comparative historical analysis of societies with similar legal prohibitions but differing cultural trajectories, or detailed micro-historical studies tracing the evolution of individual attitudes towards dueling independent of legal enforcement.',
    'If exogenous suppression was the primary cause, the constraint might be reclassified as a former Snare or Tangled Rope that was actively dismantled, rather than a naturally emergent mountain. If endogenous cultural shift is confirmed, this reading''s mountain classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_legal_causation, empirical, 'Distinguishing endogenous cultural shift from exogenous enforcement as the primary cause of dueling''s decline.').

omega_variable(
    false_summit_of_dignity_culture,
    'Is the ''unthinkability'' of dueling a genuine emergent cultural mountain, or a constructed norm that primarily benefits identifiable agents (e.g., those who thrive in a ''dignity culture'') and is presented as natural?',
    'Analysis of power dynamics and resource allocation within ''dignity cultures'' to identify if specific groups disproportionately benefit from the suppression of honor-based violence, and whether these benefits are actively maintained through subtle social enforcement mechanisms.',
    'If identifiable beneficiaries are found to actively maintain the ''unthinkability'' for their own gain, the constraint might be reclassified as a Tangled Rope or Snare, despite its appearance as a natural cultural limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_dignity_culture, conceptual, 'Assessing whether the ''dignity culture'' is a genuine mountain or a constructed norm benefiting specific groups.').

omega_variable(
    single_vs_overdetermined_causation,
    'Is the decline of dueling best understood through a single, foundational cultural transformation (as this reading emphasizes), or was it an overdetermined outcome of multiple, interacting factors (as the ''composite_overdetermined_reading'' suggests)?',
    'Development of multi-causal historical models that integrate cultural, legal, economic, and social factors, assessing the relative weight and interaction effects of each pathway.',
    'If overdetermination is confirmed, this reading''s emphasis on a single cause might be seen as an incomplete, though not necessarily incorrect, explanation. The classification of the ''unthinkability'' as a mountain might still hold, but its genesis would be understood as more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_vs_overdetermined_causation, conceptual, 'Debate over single vs. overdetermined causation for dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1700, 0.4).
narrative_ontology:measurement(hono_tr_t1725, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1725, 0.3).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1750, 0.2).
narrative_ontology:measurement(hono_tr_t1775, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1775, 0.15).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hono_tr_t1825, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1825, 0.08).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(hono_tr_t1875, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1875, 0.06).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1700, 0.6).
narrative_ontology:measurement(hono_be_t1725, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1725, 0.45).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.3).
narrative_ontology:measurement(hono_be_t1775, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1775, 0.2).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(hono_be_t1825, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1825, 0.1).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1850, 0.08).
narrative_ontology:measurement(hono_be_t1875, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1875, 0.06).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(hono_su_t1725, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1725, 0.55).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1750, 0.4).
narrative_ontology:measurement(hono_su_t1775, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1775, 0.3).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(hono_su_t1825, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1825, 0.2).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1850, 0.15).
narrative_ontology:measurement(hono_su_t1875, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1875, 0.12).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1900, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
