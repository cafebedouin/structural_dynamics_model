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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism (Composite Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint describes the historical evolution of the 'honor
 *   satisfaction mechanism' in Western societies, specifically focusing on
 *   the decline of dueling. This 'composite reading' argues that dueling's
 *   erosion was not due to a single cause but a confluence of distinct,
 *   independently operating extractive pressures: the state's monopoly on
 *   violence, the rise of bourgeois norms emphasizing legal recourse, the
 *   financial disincentives from insurance, and a fundamental category-shift
 *   in what constituted 'honor'. The constraint is claimed as a Tangled Rope
 *   because it initially offered a coordination function for honor disputes
 *   but became increasingly extractive as state and bourgeois interests
 *   leveraged it to consolidate power and redefine social status, requiring
 *   active enforcement to suppress alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.6).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Composite Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '4d4b73d4-cda4-487a-a2b8-144f1fe8c72c').
narrative_ontology:cs_kernel_codification('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', distributed).
narrative_ontology:cs_authority_grounding('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', extraction).
narrative_ontology:cs_interpretation_layer_present('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c').
narrative_ontology:cs_reading_relation('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', foundational, honor_satisfaction_is_multi_causal).
narrative_ontology:cs_axiom_status(honor_satisfaction_is_multi_causal, holdable).
narrative_ontology:cs_axiom_grounding('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', honor_satisfaction_is_multi_causal, empirically_contingent).
narrative_ontology:cs_axiom('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', foundational, honor_is_socially_constructed_and_redefinable).
narrative_ontology:cs_axiom_status(honor_is_socially_constructed_and_redefinable, holdable).
narrative_ontology:cs_axiom_grounding('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', honor_is_socially_constructed_and_redefinable, conventional).
narrative_ontology:cs_reference_frame('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', multi_causal_social_transformation).
narrative_ontology:cs_drift_state('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4d4b73d4-cda4-487a-a2b8-144f1fe8c72c', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_authority).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_elites).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_companies).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, honor_seekers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, lower_nobility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initially sought to monopolize violence, then to regulate and eventually suppress dueling, shifting from direct prohibition to indirect discouragement through legal and social means. Benefited from reduced internal conflict and consolidated power.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Promoted norms of civility and legal recourse over dueling, aligning with state efforts. Benefited from a more stable social order and the delegitimization of a practice associated with rival aristocratic classes, shifting honor from physical combat to economic and moral standing.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_elites, beneficiary,
    powerful, biographical, mobile, national).

% Benefited from the decline of dueling by reducing payouts for death or injury, and by promoting a risk-averse mindset that further undermined the practice. Their financial incentives aligned with the state's and bourgeois elites' efforts.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_companies, beneficiary,
    organized, biographical, arbitrage, national).

% Individuals, often from traditional aristocratic backgrounds, who felt compelled by social codes to defend their honor through dueling. Faced increasing legal penalties, social ostracization, and difficulty in finding seconds or opponents as the practice declined.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, honor_seekers, payer,
    moderate, immediate, constrained, local).

% A class particularly reliant on dueling to assert social status and defend honor in a changing social hierarchy. Their identity was deeply intertwined with the practice, making exit difficult even as the costs mounted and alternatives were suppressed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, lower_nobility, payer,
    powerless, biographical, identity_locked, regional).

% Analyze the historical evolution of legal frameworks and social norms surrounding dueling, documenting the interplay of state power, class interests, and changing conceptions of honor.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, if violent, mechanism for resolving honor disputes among elites, preventing open-ended feuds and establishing a clear, if brutal, social hierarchy.
% TRANSFER_FUNCTION: Transferred the right to adjudicate honor from individuals to the state and emerging bourgeois legal/social norms, shifting the 'cost' of honor from physical risk to legal and social compliance.
% ABSENT_VOICES: Those who continued to believe in dueling as a legitimate means of honor satisfaction, particularly from marginalized aristocratic factions, found their voices increasingly suppressed by state power and dominant bourgeois discourse. Their 'honor' was no longer recognized by the prevailing system.
% DISAPPEARANCE_RATIONALE: The decline of dueling was not a simple disappearance but a complex rearrangement of social and legal mechanisms for conflict resolution and status assertion. If the entire honor satisfaction mechanism (including its replacements) vanished, social order would be significantly disrupted, and new, potentially more violent, forms of dispute resolution might emerge.
% FOUNDING_PROBLEM: Unregulated violence and feuds among elites threatened state stability and economic development, requiring a mechanism to channel or suppress such conflicts.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and sociological analyses from independent scholars corroborate that the problem of unregulated elite violence was largely 'solved' by the rise of state monopolies on violence and bourgeois norms. The persistence of dueling in some forms was a residual practice, not a live solution to the original problem. The state and bourgeois elites, while benefiting, also provided the historical evidence for this shift.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).

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
 *   Extractiveness (0.6) reflects the increasing costs imposed on those who sought to maintain dueling as a legitimate practice, as state penalties and social ostracization mounted. Suppression (0.7) is high due to active state prohibition, legal sanctions, and the social pressure exerted by bourgeois norms and insurance practices. Theater ratio (0.4) indicates that while some elements of 'honor' remained, the public performance of dueling became increasingly ritualized and less functional as a genuine dispute resolution mechanism, eventually becoming a fringe activity. The metrics reflect the period of active decline and suppression, not the earlier period when dueling was a more robust, if violent, coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and bourgeois elites, the decline of dueling was a positive development, a shift towards a more 'civilized' and orderly society (a Rope or even a Mountain of progress). From the perspective of honor seekers and lower nobility, it was an extractive process that stripped them of a fundamental right and a means of maintaining their social standing (a Snare). This composite reading acknowledges the coordination function but emphasizes the asymmetric extraction and active enforcement that drove the decline.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority, bourgeois elites, and insurance companies are beneficiaries, as they gained power, social stability, and financial advantage from the decline of dueling. Honor seekers and lower nobility are victims, as they bore the costs of legal penalties, social marginalization, and the erosion of their traditional means of status assertion. The 'identity_locked' exit option for lower nobility highlights how their self-concept was tied to the practice, making it difficult to adapt to new norms.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to regulate elite violence) became 'dead' as the state successfully monopolized violence and bourgeois norms redefined honor. However, the mechanisms that suppressed dueling persisted and became extractive, leveraging the initial coordination function for other ends (state power, class ascendancy). This prevents mislabeling it as a simple Rope (ignoring extraction) or a pure Snare (ignoring the initial coordination and the complex, multi-faceted decline).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_weight_of_mechanisms,
    'What was the relative weight or causal contribution of each distinct mechanism (state monopoly, bourgeois norms, insurance, category-shift) to the overall decline of dueling?',
    'Detailed historical-sociological case studies comparing regions with varying strengths of each mechanism, or counterfactual historical analysis.',
    'If one mechanism was overwhelmingly dominant, the constraint might be reclassified to emphasize that single extractive force (e.g., a Snare of state power). If the composite nature is confirmed, it reinforces the Tangled Rope classification reflecting multiple, interacting pressures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_weight_of_mechanisms, empirical, 'Determining the primary drivers of dueling''s decline.').

omega_variable(
    honor_redefinition_vs_suppression,
    'To what extent was the decline of dueling a result of its active suppression, versus a genuine redefinition of ''honor'' that made dueling irrelevant or unthinkable?',
    'Analysis of primary sources (diaries, legal texts, philosophical treatises) to trace the evolution of the concept of honor and its relationship to violence, alongside enforcement records.',
    'If redefinition was primary, the constraint leans more towards a Mountain (a shift in fundamental social ontology). If suppression was primary, it reinforces the Tangled Rope/Snare classification, highlighting coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_redefinition_vs_suppression, conceptual, 'Distinguishing between active suppression and conceptual shift in the decline of dueling.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint best framed as the ''decline of dueling'' (a practice), or the ''evolution of honor satisfaction'' (a broader social function)?',
    'Analyzing the scope of historical actors'' own conceptualizations and the functional replacements that emerged. If the replacements (courts, social shaming) fully absorbed the function, the broader framing is more accurate.',
    'Framing as ''decline of dueling'' emphasizes the loss of a specific practice (more Snare-like for those who valued it). Framing as ''evolution of honor satisfaction'' emphasizes the emergence of new, potentially less extractive, mechanisms (more Rope-like for the overall system).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the scope of the ''honor satisfaction mechanism'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(hono_tr_t1650, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1650, 0.25).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1700, 0.3).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1750, 0.35).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1800, 0.4).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1850, 0.4).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1600, 0.4).
narrative_ontology:measurement(hono_be_t1650, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1650, 0.45).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1700, 0.5).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1800, 0.6).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1850, 0.6).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1600, 0.5).
narrative_ontology:measurement(hono_su_t1650, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1650, 0.55).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1750, 0.65).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
