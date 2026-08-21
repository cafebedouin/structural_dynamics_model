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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Unthinkability of Dueling (Cultural Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Unthinkability of Dueling (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, 'e1e736a6-828e-4a69-bf7d-f790fe45d58b').
narrative_ontology:cs_kernel_codification('e1e736a6-828e-4a69-bf7d-f790fe45d58b', implicit).
narrative_ontology:cs_authority_grounding('e1e736a6-828e-4a69-bf7d-f790fe45d58b', practice).
narrative_ontology:cs_interpretation_layer_present('e1e736a6-828e-4a69-bf7d-f790fe45d58b').
narrative_ontology:cs_reading_relation('e1e736a6-828e-4a69-bf7d-f790fe45d58b', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('e1e736a6-828e-4a69-bf7d-f790fe45d58b', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('e1e736a6-828e-4a69-bf7d-f790fe45d58b', foundational, honor_as_dignity_not_reputation).
narrative_ontology:cs_axiom_status(honor_as_dignity_not_reputation, holdable).
narrative_ontology:cs_axiom_grounding('e1e736a6-828e-4a69-bf7d-f790fe45d58b', honor_as_dignity_not_reputation, conventional).
narrative_ontology:cs_axiom('e1e736a6-828e-4a69-bf7d-f790fe45d58b', secondary, dispute_resolution_via_legal_means).
narrative_ontology:cs_axiom_status(dispute_resolution_via_legal_means, holdable).
narrative_ontology:cs_axiom_grounding('e1e736a6-828e-4a69-bf7d-f790fe45d58b', dispute_resolution_via_legal_means, conventional).
narrative_ontology:cs_reference_frame('e1e736a6-828e-4a69-bf7d-f790fe45d58b', honor_as_reputation_based_violence).
narrative_ontology:cs_drift_state('e1e736a6-828e-4a69-bf7d-f790fe45d58b', contemporary_dignity_culture, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e1e736a6-828e-4a69-bf7d-f790fe45d58b', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint itself (the unthinkability of dueling) does not coordinate; rather, it represents the absence of the prior honor code's coordination function for dispute resolution. The new coordination is achieved through legal systems and norms of dignity.
% TRANSFER_FUNCTION: None. The constraint represents the cessation of the prior system's transfers (e.g., social status, reputation, or even life, through dueling).
% ABSENT_VOICES: Individuals or groups who, in earlier eras, would have upheld the 'culture of honor' and seen dueling as a legitimate or necessary means of satisfaction. Their worldview has largely been superseded.
% DISAPPEARANCE_RATIONALE: If the 'unthinkability' of dueling vanished overnight, the underlying cultural shift from honor to dignity, and the robust legal systems for dispute resolution, would still prevent dueling from re-emerging as a widespread practice. The unthinkability is a symptom of deeper cultural structures, not their sole cause.
% FOUNDING_PROBLEM: The problem of maintaining social order and individual reputation in societies where personal honor was paramount, and legal or institutional recourse was often seen as insufficient or dishonorable for certain grievances.
% FOUNDING_PROBLEM_CORROBORATION: Historians, legal scholars, and cultural anthropologists widely corroborate that the social problem dueling addressed has been largely superseded by the rise of 'cultures of dignity' and the expansion of state legal authority, rendering the old mechanisms obsolete. This corroboration comes from outside the historical beneficiaries of the honor code itself.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''cultural_contraction_reading'' of the ''honor_satisfaction_substrate'' kernel?',
    'Comparison with other readings of the kernel, ensuring that the emphasis on endogenous cultural transformation is distinct and consistent with historical evidence.',
    'If misidentified, the analysis of dueling''s decline would be incomplete or misattributed, affecting the understanding of causal pathways and the role of cultural vs. exogenous factors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint as a specific reading of the honor satisfaction kernel.').

omega_variable(
    cultural_vs_exogenous_causation,
    'To what extent was the ''unthinkability'' of dueling primarily driven by endogenous cultural transformation (as this reading claims) versus exogenous factors like legal prohibition and institutional changes?',
    'Detailed historical and sociological analysis comparing the timing and impact of cultural shifts (e.g., rise of individualism, changing notions of masculinity) with legal and state-enforced suppression of dueling across different societies.',
    'If exogenous factors were dominant, this constraint might be better understood as a consequence of a Snare (legal prohibition) or a Tangled Rope (institutional control) rather than a pure Mountain of cultural evolution. This would shift the classification of the *mechanism* of decline, though the ''unthinkability'' outcome might remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_exogenous_causation, empirical, 'Distinguishes the primary drivers of dueling''s decline.').

omega_variable(
    residual_honor_code_influence,
    'Does the honor code, in some attenuated or transformed form, still exert any ''thinkable'' influence on contemporary social interactions, even if dueling is unthinkable?',
    'Sociological studies of contemporary honor cultures (e.g., in specific subcultures or regions) and their mechanisms for dispute resolution, to identify any lingering structural or performative elements of honor satisfaction.',
    'If residual influence is significant, the ''unthinkability'' might be less absolute, or the ''Mountain'' classification might need to acknowledge a ''Piton-like'' residue in specific contexts, suggesting a more complex, multi-layered constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_honor_code_influence, empirical, 'Assesses any lingering influence of the honor code.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1750, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(hono_tr_t1790, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1790, 0.12).
narrative_ontology:measurement(hono_tr_t1830, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1830, 0.1).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1870, 0.08).
narrative_ontology:measurement(hono_tr_t1910, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1910, 0.06).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1950, 0.05).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.6).
narrative_ontology:measurement(hono_be_t1790, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1790, 0.45).
narrative_ontology:measurement(hono_be_t1830, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1830, 0.3).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1870, 0.15).
narrative_ontology:measurement(hono_be_t1910, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1910, 0.08).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1950, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(hono_su_t1790, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1790, 0.55).
narrative_ontology:measurement(hono_su_t1830, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1830, 0.4).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1870, 0.25).
narrative_ontology:measurement(hono_su_t1910, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1910, 0.1).
narrative_ontology:measurement(hono_su_t1950, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1950, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
