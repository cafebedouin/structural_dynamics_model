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
 *   This constraint describes the historical process by which the honor
 *   satisfaction mechanism (dueling) was eroded and replaced by a state
 *   monopoly on violence and bourgeois norms. This 'composite reading'
 *   emphasizes multiple, distinct mechanisms (state legal suppression, rising
 *   bourgeois norms, the role of insurance, and a fundamental category-shift
 *   in how violence was perceived) that collectively led to the decline of
 *   dueling. It is a tangled rope because it involved both a genuine
 *   coordination function (reducing private violence) and asymmetric
 *   extraction (from honor-bound individuals by the state and rising
 *   bourgeois class).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.65).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.65).
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
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '30e4cee4-acb4-43a9-b7bb-8e7d09ed722a').
narrative_ontology:cs_kernel_codification('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', implicit).
narrative_ontology:cs_authority_grounding('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', extraction).
narrative_ontology:cs_interpretation_layer_present('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a').
narrative_ontology:cs_reading_relation('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', honor_satisfaction_mechanism__decline_reading, influences).
narrative_ontology:cs_reading_relation('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_axiom('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', foundational, multi_causal_decline_of_dueling).
narrative_ontology:cs_axiom_status(multi_causal_decline_of_dueling, holdable).
narrative_ontology:cs_axiom_grounding('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', multi_causal_decline_of_dueling, empirically_contingent).
narrative_ontology:cs_axiom('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', foundational, state_monopoly_on_violence_is_central).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_central, holdable).
narrative_ontology:cs_axiom_grounding('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', state_monopoly_on_violence_is_central, conventional).
narrative_ontology:cs_reference_frame('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', traditional_honor_code_with_dueling).
narrative_ontology:cs_drift_state('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', late_19th_century_europe, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('30e4cee4-acb4-43a9-b7bb-8e7d09ed722a', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_legal_system).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_elites).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, honor_bound_nobility).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, lower_gentry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively suppressed dueling through legal prohibitions and punishments, gradually establishing a monopoly on legitimate violence. Benefited from increased internal stability and centralized authority, but also had to manage residual honor culture.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_legal_system, agenda_setter,
    institutional, generational, mobile, national).

% Promoted norms of civility, economic rationality, and legal recourse over personal violence, which aligned with their class interests and helped consolidate their social position. They benefited from the decline of dueling without directly enforcing its suppression.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_elites, beneficiary,
    powerful, biographical, arbitrage, national).

% Faced increasing legal and social pressure to abandon dueling, a practice central to their traditional honor code and social status. They bore the costs of legal penalties and social ostracism if they continued, or the perceived loss of honor if they conformed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, honor_bound_nobility, payer,
    moderate, generational, identity_locked, regional).

% Often caught between the declining aristocratic honor code and the rising bourgeois norms, with fewer resources to navigate legal repercussions or social stigma. They were more vulnerable to the extractive pressures of the changing system.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, lower_gentry, payer,
    powerless, biographical, trapped, local).

% Benefited from the decline of dueling by reducing payouts for death or injury, and by promoting a culture of risk aversion and financial prudence that further undermined the honor code. They did not directly enforce anti-dueling laws but profited from their effects.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_companies, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Managed the transition from a decentralized, personal system of honor satisfaction (dueling) to a centralized, state-monopolized system of justice, reducing social disorder and violence.
% TRANSFER_FUNCTION: Transferred the right to adjudicate grievances and administer violence from individuals and honor groups to the state, and shifted social capital from traditional honor to bourgeois civility and legal compliance.
% ABSENT_VOICES: Traditionalists and proponents of the honor code, whose worldview was being systematically dismantled, were increasingly marginalized from legal and social discourse, their arguments dismissed as anachronistic or barbaric.
% DISAPPEARANCE_RATIONALE: The honor satisfaction mechanism, as a composite of dueling, state suppression, and bourgeois norms, has largely disappeared. Its re-emergence would require a fundamental shift in state authority, legal systems, and social values, which is highly unlikely.
% FOUNDING_PROBLEM: The problem of maintaining social order and resolving disputes in a society where personal honor codes frequently led to violence and challenged state authority.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists widely corroborate that the problem of widespread dueling as a challenge to state authority is dead, replaced by state monopoly on violence. The state legal system, as a beneficiary, would also attest to this, but the corroboration comes from independent academic analysis.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the state and bourgeois elites gained significant power and social capital at the expense of the traditional nobility. Suppression is also high, reflecting the active legal and social enforcement against dueling. Theater ratio is moderate, as some 'honor' rituals persisted even as their underlying function atrophied. The decline in extractiveness and suppression towards the end of the interval reflects the near-complete triumph of the new system, making active enforcement less necessary.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and bourgeois elites, this was a necessary and beneficial evolution towards a more civilized society. From the perspective of the honor-bound nobility, it was a coercive dismantling of their traditional way of life and a loss of status. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal system and bourgeois elites are beneficiaries, gaining from the centralization of power and the shift in social norms. Honor-bound nobility and lower gentry are payers, losing status and facing legal penalties. Insurance companies are indirect beneficiaries, profiting from the reduction of risk. The 'identity_locked' exit for nobility reflects the deep cultural entanglement of dueling with their self-conception.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (widespread dueling challenging state authority) is 'dead' in this reading, yet the mechanisms that replaced it (state monopoly on violence, legal systems) persist. This indicates a successful resolution of the original mandate, but the persistence of the new structures could be analyzed for new forms of extraction or inertia. The composite nature of the decline means no single mechanism became a 'piton' but rather a new, more complex 'tangled rope' emerged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_weight_of_mechanisms,
    'What was the relative causal weight of state legal suppression, bourgeois normative shifts, and economic factors (like insurance) in the decline of dueling?',
    'Detailed historical-sociological case studies comparing regions with different mixes of these pressures, or counterfactual historical analysis.',
    'If state suppression was dominant, the constraint leans more towards a Snare; if bourgeois norms were more influential, it highlights a shift in social coordination. If economic factors were primary, it points to a different kind of systemic pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_weight_of_mechanisms, empirical, 'Determining the primary drivers of dueling''s decline.').

omega_variable(
    category_shift_vs_decline,
    'Was the decline of dueling primarily a quantitative reduction in frequency, or a qualitative ''category-shift'' where it became unthinkable as a legitimate practice?',
    'Analysis of contemporary discourse, legal texts, and personal correspondence for evidence of changing cognitive frames and moral judgments regarding dueling.',
    'If a category-shift was dominant, it suggests a more profound transformation of social reality, making the constraint''s persistence less about active enforcement and more about internalized norms. If merely a decline, the ''tangled rope'' classification remains more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_shift_vs_decline, conceptual, 'Understanding the nature of dueling''s disappearance.').

omega_variable(
    honor_code_persistence,
    'To what extent did the underlying ''honor code'' persist in modified forms, even after dueling itself declined, and how did it manifest?',
    'Ethnographic studies of residual honor cultures, analysis of non-lethal forms of ''satisfaction,'' or literary analysis of honor narratives in later periods.',
    'If the honor code persisted strongly in other forms, it suggests the ''extraction'' from the nobility was less complete, and the ''identity_locked'' exit option was more fluid than initially assessed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_code_persistence, empirical, 'Tracing the evolution of honor culture beyond dueling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(hono_tr_t1650, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1650, 0.15).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1700, 0.25).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1750, 0.35).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1800, 0.4).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1850, 0.35).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.3).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1600, 0.4).
narrative_ontology:measurement(hono_be_t1650, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1650, 0.5).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1700, 0.58).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1850, 0.63).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1600, 0.3).
narrative_ontology:measurement(hono_su_t1650, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1650, 0.45).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_satisfaction_mechanism' kernel. This 'composite_reading' emphasizes multiple, distinct mechanisms (state legal suppression, rising bourgeois norms, insurance, and category-shift) that collectively led to the decline of dueling, contrasting with the 'decline_reading' (quantitative reduction) and 'contraction_reading' (cognitive impossibility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
