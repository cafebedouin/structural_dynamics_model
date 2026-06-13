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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Cultural Contraction of Honor Satisfaction Substrate
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the historical transformation of the 'honor
 *   satisfaction substrate' itself, where dueling became culturally
 *   unthinkable as 'cultures of honor' gave way to 'cultures of dignity'.
 *   This reading posits that the constraint is a 'mountain' of cultural
 *   evolution, where the very possibility of dueling as a legitimate act
 *   eroded due to internal cultural shifts, rather than external enforcement.
 *   The constraint is claimed as a mountain because the underlying cultural
 *   substrate that made dueling thinkable contracted, making it an
 *   irreducible feature of the new cultural landscape. The presence of
 *   beneficiaries (society at large) on a mountain triggers the False Summit
 *   Mountain (FSM) detection, prompting an omega variable to address the
 *   ambiguity between natural cultural evolution and a constructed benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.02).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Cultural Contraction of Honor Satisfaction Substrate").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '3cb7a593-e979-4723-b170-d61f42aae864').
narrative_ontology:cs_kernel_codification('3cb7a593-e979-4723-b170-d61f42aae864', implicit).
narrative_ontology:cs_authority_grounding('3cb7a593-e979-4723-b170-d61f42aae864', practice).
narrative_ontology:cs_interpretation_layer_present('3cb7a593-e979-4723-b170-d61f42aae864').
narrative_ontology:cs_reading_relation('3cb7a593-e979-4723-b170-d61f42aae864', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('3cb7a593-e979-4723-b170-d61f42aae864', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('3cb7a593-e979-4723-b170-d61f42aae864', foundational, honor_satisfaction_is_culturally_contingent).
narrative_ontology:cs_axiom_status(honor_satisfaction_is_culturally_contingent, holdable).
narrative_ontology:cs_axiom_grounding('3cb7a593-e979-4723-b170-d61f42aae864', honor_satisfaction_is_culturally_contingent, deontological).
narrative_ontology:cs_axiom('3cb7a593-e979-4723-b170-d61f42aae864', secondary, dueling_is_culturally_unthinkable).
narrative_ontology:cs_axiom_status(dueling_is_culturally_unthinkable, holdable).
narrative_ontology:cs_axiom_grounding('3cb7a593-e979-4723-b170-d61f42aae864', dueling_is_culturally_unthinkable, conventional).
narrative_ontology:cs_reference_frame('3cb7a593-e979-4723-b170-d61f42aae864', culture_of_honor_substrate).
narrative_ontology:cs_drift_state('3cb7a593-e979-4723-b170-d61f42aae864', culture_of_dignity_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('3cb7a593-e979-4723-b170-d61f42aae864', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, society_at_large).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, former_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the shift away from violent honor satisfaction, leading to greater social stability and reduced interpersonal violence. The 'culture of dignity' provides a more robust and less volatile framework for resolving disputes.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, society_at_large, beneficiary,
    institutional, generational, analytical, national).

% Individuals who, in a prior cultural substrate, would have resorted to dueling to satisfy honor. They now find this option unthinkable or socially unacceptable, experiencing a loss of a culturally sanctioned means of redress, though this 'cost' is framed as progress.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, former_duelists, payer,
    powerless, biographical, identity_locked, local).

% Analyze the historical shift from honor cultures to dignity cultures, examining its impact on legal systems, social norms, and the concept of individual rights. They observe the constraint's operation and its historical evolution.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social expectations around conflict resolution, shifting from violent, personal satisfaction of honor to institutionalized, dignity-based redress.
% TRANSFER_FUNCTION: Transfers the locus of legitimate grievance resolution from individual-initiated violence to state-sanctioned legal and social processes. It transfers the 'cost' of maintaining honor from personal risk to collective institutional maintenance.
% ABSENT_VOICES: Those who might still adhere to older honor codes, or who feel that dignity culture fails to adequately address certain forms of personal affront, are marginalized or silenced by the dominant cultural narrative. Their 'voice' is often expressed through historical re-evaluation or subcultural resistance.
% DISAPPEARANCE_RATIONALE: The cultural shift is so foundational that its disappearance would not cause the world to 'rearrange' in the sense of a policy being revoked. Instead, it would imply a fundamental reversal of centuries of cultural evolution, which is unthinkable. The substrate itself has contracted.
% FOUNDING_PROBLEM: The problem of pervasive interpersonal violence and instability arising from a culture where personal honor could only be satisfied through dueling or other forms of direct, often violent, confrontation.
% FOUNDING_PROBLEM_CORROBORATION: Historians and cultural anthropologists widely corroborate that the problem of dueling as a primary form of honor satisfaction is dead, replaced by a dignity-based framework. Legal codes and social norms from outside the direct beneficiaries (e.g., contemporary legal systems, human rights organizations) attest to this shift.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

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
 *   The extractiveness is very low (0.05) because the constraint is not actively extracting from anyone; rather, it represents a fundamental shift in social values. Suppression is also very low (0.02) because dueling became unthinkable, not merely illegal – the cultural substrate itself no longer supported it, reducing the need for active enforcement. Theater ratio is negligible (0.01) as there's no performative maintenance of a defunct practice. Accessibility collapse is high (0.95) because the cultural shift made dueling almost entirely inaccessible as a legitimate option. Resistance is minimal (0.01) because the shift was deeply internalized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'society at large', this cultural contraction is a clear benefit, leading to greater stability. For 'former duelists' (a conceptual group representing those whose prior cultural identity was tied to honor satisfaction), the shift represents a loss of a culturally sanctioned means of redress, though this 'cost' is framed as societal progress. The engine's classification will reflect this divergence, with society at large as a beneficiary and former duelists as a 'payer' of a cultural cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large is a beneficiary (d near 0.0) as the shift reduces violence and increases stability. Former duelists are payers (d near 1.0) as they lose a culturally sanctioned means of honor satisfaction, even if this is a 'beneficial' loss from a broader societal perspective. Legal scholars are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The concept of mandatrophy doesn't directly apply here in the sense of a mandate outliving its function, because the 'function' of dueling itself became culturally obsolete. The constraint is not a 'zombie' institution but a fundamental re-ordering of social reality. The classification as a mountain (with beneficiaries) prevents mislabeling a profound cultural shift as a mere 'snare' of legal prohibition, emphasizing the internal, emergent nature of the change rather than external coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_cultural_shift,
    'Is the shift from ''cultures of honor'' to ''cultures of dignity'' a truly emergent, ''natural'' cultural evolution, or was it actively constructed and promoted by identifiable agents (e.g., state institutions, moral reformers) who benefited from the change?',
    'Detailed historical-sociological analysis of the agency involved in promoting dignity culture, identifying specific actors, their motivations, and the mechanisms of cultural diffusion and enforcement beyond mere ''unthinkability''.',
    'If actively constructed, the constraint''s ''emerges_naturally'' claim would be weakened, potentially reclassifying it from a Mountain to a Rope or even a Tangled Rope, depending on the degree of active enforcement and asymmetric benefit. The ''beneficiaries'' would then be seen as more active agents in its construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_cultural_shift, conceptual, 'Ambiguity between emergent cultural evolution and active cultural construction.').

omega_variable(
    substrate_erosion_vs_suppression,
    'To what extent did the ''unthinkability'' of dueling arise from a genuine erosion of its cultural substrate, versus the internalization of external legal and social suppression that made it merely ''unthinkable'' to avoid punishment?',
    'Comparative historical analysis of regions with similar legal prohibitions but differing cultural trajectories regarding dueling, to isolate the effect of cultural internalization from mere compliance with external force.',
    'If primarily due to internalized suppression, the ''suppression'' metric would be higher, and the ''emerges_naturally'' claim would be weaker, pushing the classification away from Mountain towards a more constructed type like Rope or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_erosion_vs_suppression, empirical, 'Distinguishing cultural substrate erosion from internalized suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1750, 0.03).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1800, 0.02).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1850, 0.01).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1900, 0.01).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1800, 0.07).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1850, 0.06).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1750, 0.07).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1850, 0.03).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1900, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
