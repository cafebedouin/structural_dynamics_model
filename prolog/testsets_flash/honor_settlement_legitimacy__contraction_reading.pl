% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Dueling as Cognitively Unthinkable (Contraction Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint describes the cultural transformation that rendered
 *   dueling cognitively unthinkable as a legitimate means of honor
 *   settlement. It is a 'contraction reading' of the broader kernel of
 *   'honor_settlement_legitimacy', focusing on the deep cultural shift rather
 *   than mere legal prohibition or residual practice. The constraint is
 *   claimed as a Mountain because the cultural framework, once established,
 *   operates as an unchangeable feature of the social landscape, making
 *   dueling not just illegal but incomprehensible as a valid option. The
 *   beneficiaries are civil society and the legal system, which no longer
 *   bear the costs of dueling's social disruption.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.05).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.95).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Dueling as Cognitively Unthinkable (Contraction Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, 'f99a6619-f49f-462a-a35c-9a3be4096b72').
narrative_ontology:cs_kernel_codification('f99a6619-f49f-462a-a35c-9a3be4096b72', implicit).
narrative_ontology:cs_authority_grounding('f99a6619-f49f-462a-a35c-9a3be4096b72', practice).
narrative_ontology:cs_interpretation_layer_present('f99a6619-f49f-462a-a35c-9a3be4096b72').
narrative_ontology:cs_reading_relation('f99a6619-f49f-462a-a35c-9a3be4096b72', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('f99a6619-f49f-462a-a35c-9a3be4096b72', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('f99a6619-f49f-462a-a35c-9a3be4096b72', foundational, honor_culture_exits_normative_space).
narrative_ontology:cs_axiom_status(honor_culture_exits_normative_space, holdable).
narrative_ontology:cs_axiom_grounding('f99a6619-f49f-462a-a35c-9a3be4096b72', honor_culture_exits_normative_space, deontological).
narrative_ontology:cs_reference_frame('f99a6619-f49f-462a-a35c-9a3be4096b72', pre_dueling_unthinkability_era).
narrative_ontology:cs_drift_state('f99a6619-f49f-462a-a35c-9a3be4096b72', post_cultural_transformation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f99a6619-f49f-462a-a35c-9a3be4096b72', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, civil_society).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, honor_culture_adherents_historical).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of dueling as a legitimate means of conflict resolution, leading to a more stable and less violent public sphere. The cultural shift makes dueling unthinkable, reducing social friction.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, civil_society, beneficiary,
    institutional, generational, analytical, national).

% Benefits from the cultural transformation that renders dueling illegitimate, as it no longer needs to actively suppress or prosecute dueling, which was often difficult to enforce when social norms supported it. Its authority is implicitly strengthened.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legal_system, beneficiary,
    institutional, generational, analytical, national).

% Historically, individuals whose identity was deeply intertwined with honor culture found their entire framework for resolving disputes and maintaining social standing rendered obsolete and even morally repugnant by the new cultural framework. Their way of life became cognitively foreclosed.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, honor_culture_adherents_historical, payer,
    powerless, biographical, identity_locked, local).

% Study the historical processes by which dueling became unthinkable, analyzing the cultural, legal, and social shifts that led to this transformation. They observe the constraint's operation from an academic distance.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social behavior by establishing a shared understanding that personal disputes are not to be settled through ritualized violence, thereby reducing social instability and violence.
% TRANSFER_FUNCTION: Transfers the burden of conflict resolution from individual honor-based challenges to institutional legal and social mechanisms, effectively transferring the 'right' to violence from individuals to the state.
% ABSENT_VOICES: The voices of those who once saw dueling as a legitimate and necessary component of honor are now absent from the normative discourse, their framework having been culturally foreclosed. They would argue for the necessity of personal honor defense.
% DISAPPEARANCE_RATIONALE: If the cultural framework that makes dueling unthinkable were to vanish, it would imply a fundamental shift in societal values, potentially leading to a resurgence of honor-based violence or other forms of extra-legal dispute resolution. However, the constraint itself is the *absence* of dueling as a legitimate option, so its 'disappearance' would be a return to a prior state, not a new rearrangement.
% FOUNDING_PROBLEM: The problem of widespread violence and social instability arising from honor-based dueling, which undermined state authority and civil order.
% FOUNDING_PROBLEM_CORROBORATION: Legal codes and historical records from the period of dueling's decline, as well as sociological analyses of state-building and the monopolization of violence, corroborate that dueling was a significant social problem. The problem is 'dead' because the cultural framework has largely eliminated dueling as a legitimate practice, making its active suppression largely unnecessary.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint primarily operates by making dueling unthinkable, rather than by actively extracting resources. Suppression is very high (0.95) because the cultural framework effectively suppresses the very idea of dueling as legitimate, making alternatives (like legal recourse) the only cognitively available options. Accessibility collapse is near total (0.98) as the cultural framework makes dueling an inaccessible concept for legitimate action. Resistance is minimal (0.02) because the transformation is so complete that there is no significant active resistance to the illegitimacy of dueling. The temporal measurements show a decline in extractiveness and a rise in suppression, reflecting the deepening of the cultural shift over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civil society and the legal system, this constraint is a beneficial, almost natural, evolution towards a more ordered society. For historical adherents of honor culture, it represented a profound loss of a legitimate framework for self-worth and dispute resolution, effectively making them 'identity_locked' into an obsolete worldview.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil society and the legal system are beneficiaries (d near 0.0) as they gain from the absence of dueling without bearing significant costs. Historical honor culture adherents are payers (d near 1.0) as their entire framework for social interaction and self-identity was rendered invalid. Analytical historians are observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Mountain because the cultural framework transformation is so complete that dueling is no longer a live option for legitimate action. The 'mandate' of dueling (to settle honor) has not atrophied; rather, the entire cultural space in which that mandate could exist has contracted. The classification prevents mislabeling this deep cultural shift as mere legal prohibition (a Snare) or a temporary measure (a Scaffold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_legal_causation,
    'To what extent was the decline of dueling primarily driven by cultural framework transformation (as this reading claims) versus legal prohibition and state monopolization of violence?',
    'Comparative historical analysis of societies with similar legal prohibitions but different cultural trajectories regarding dueling, or detailed micro-historical studies of individual decision-making processes.',
    'If legal prohibition was the dominant factor, the constraint might be reclassified as a Snare (pure extraction by the state) or Tangled Rope (coordination with coercive enforcement). If cultural transformation was primary, the Mountain classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_legal_causation, empirical, 'Distinguishing the primary causal mechanism for dueling''s decline.').

omega_variable(
    cognitive_unthinkability_measurement,
    'How can ''cognitive unthinkability'' be empirically measured or demonstrated beyond the mere absence of dueling?',
    'Analysis of contemporary literature, philosophical treatises, and personal correspondence for explicit rejections or expressions of incomprehension regarding dueling''s legitimacy, rather than just its illegality.',
    'Strong empirical evidence of cognitive unthinkability reinforces the Mountain classification. Weak evidence might suggest the constraint is merely a strong social norm (Rope) or a successfully enforced prohibition (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_unthinkability_measurement, empirical, 'Empirical basis for ''cognitive unthinkability''.').

omega_variable(
    false_summit_ambiguity,
    'Is this constraint a genuine natural law of social order, or a constructed constraint that benefits identifiable agents (civil society, legal system) by framing a historical contingency as an inevitable cultural evolution?',
    'Analysis of counter-examples where similar cultural shifts did not occur, or where honor-based violence persists despite legal prohibition, suggesting the ''naturalness'' is context-dependent.',
    'If found to be a constructed constraint, it would be reclassified as a Tangled Rope, acknowledging the coordination function but also the beneficiaries of the framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_ambiguity, conceptual, 'Is the cultural shift a ''natural law'' or a constructed benefit?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1800, 0.07).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1850, 0.05).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1750, 0.8).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1800, 0.9).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1850, 0.95).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
