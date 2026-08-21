% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Imposed Norms via Coercion (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.8).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Imposed Norms via Coercion (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '9f957fd5-c6f9-408a-925b-1ad78875991b').
narrative_ontology:cs_kernel_codification('9f957fd5-c6f9-408a-925b-1ad78875991b', formalized).
narrative_ontology:cs_authority_grounding('9f957fd5-c6f9-408a-925b-1ad78875991b', extraction).
narrative_ontology:cs_interpretation_layer_present('9f957fd5-c6f9-408a-925b-1ad78875991b').
narrative_ontology:cs_reading_relation('9f957fd5-c6f9-408a-925b-1ad78875991b', imposition_mechanism_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('9f957fd5-c6f9-408a-925b-1ad78875991b', imposition_mechanism_kernel__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('9f957fd5-c6f9-408a-925b-1ad78875991b', foundational, state_violence_is_foundational_legitimacy).
narrative_ontology:cs_axiom_status(state_violence_is_foundational_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9f957fd5-c6f9-408a-925b-1ad78875991b', state_violence_is_foundational_legitimacy, conventional).
narrative_ontology:cs_axiom('9f957fd5-c6f9-408a-925b-1ad78875991b', foundational, compliance_is_coercion_dependent).
narrative_ontology:cs_axiom_status(compliance_is_coercion_dependent, holdable).
narrative_ontology:cs_axiom_grounding('9f957fd5-c6f9-408a-925b-1ad78875991b', compliance_is_coercion_dependent, empirically_contingent).
narrative_ontology:cs_reference_frame('9f957fd5-c6f9-408a-925b-1ad78875991b', state_monopoly_on_violence).
narrative_ontology:cs_drift_state('9f957fd5-c6f9-408a-925b-1ad78875991b', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9f957fd5-c6f9-408a-925b-1ad78875991b', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subject_population).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The centralized authority that promulgates and enforces new norms through its monopoly on violence. It benefits from the consolidation of power and the establishment of a unified social order, which it claims is for the common good.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The social and political class whose interests are often aligned with the state's consolidation of power. They benefit from the stability and order imposed by the new norms, which can reinforce their economic and social position.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite, beneficiary,
    powerful, generational, mobile, national).

% The general populace that must comply with the new state-imposed norms, often at the expense of traditional practices or personal freedoms. They face direct coercion and violence for non-compliance, with little to no avenue for dissent or influence over norm formation.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, subject_population, payer,
    powerless, biographical, trapped, regional).

% Local or regional leaders, religious figures, or clan heads whose authority and influence are directly challenged and often suppressed by the imposition of state norms. They bear the cost of losing their traditional power base and may face severe repercussions for resistance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_elites, payer,
    organized, biographical, constrained, local).

% Scholars and researchers who study the historical processes of state formation and norm imposition, analyzing the mechanisms of coercion, resistance, and the long-term impacts on society. They are external to the constraint's operation but provide critical analysis.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state claims to establish a universal legal and social order, standardizing norms across diverse populations to prevent internal conflict and facilitate centralized governance.
% TRANSFER_FUNCTION: Transfers ultimate authority and compliance from traditional, local, or informal structures to the centralized state, extracting obedience, resources (e.g., taxes, labor), and conformity from the subject population.
% ABSENT_VOICES: Local leaders, traditional authorities, and dissenting factions within the subject population whose established norms and practices were overridden. Their perspectives are suppressed by the state's coercive power, preventing their participation in norm-setting.
% DISAPPEARANCE_RATIONALE: If state coercion vanished overnight, the imposed norms would likely collapse, leading to a resurgence of traditional practices, the emergence of new social orders, and potentially significant conflict as competing authorities reassert themselves. The unified social order would fragment.
% FOUNDING_PROBLEM: To consolidate state power, establish a unified legal and social order, and eliminate competing sources of authority (e.g., local customs, religious laws) that threatened central control and hindered resource extraction.
% FOUNDING_PROBLEM_CORROBORATION: State chronicles and official histories attest to the necessity of establishing order and unity. However, independent historical analyses and records of resistance movements corroborate that the 'problem' was often a pretext for power consolidation, and its 'solution' was primarily coercive, with legitimacy remaining contested by the subject population.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_contribution_ambiguity,
    'To what extent did any pre-existing cultural acceptance or bottom-up adoption (endogenous climb) contribute to the eventual stability of the state-imposed norms, despite initial coercion?',
    'Longitudinal historical analysis examining post-coercion compliance patterns, cultural integration of state norms over generations, and the absence/presence of sustained resistance movements.',
    'If significant endogenous climb is identified, the constraint''s effective extractiveness and suppression might be lower over time than initially measured, suggesting a shift towards a Tangled Rope or even Rope as legitimacy accrues. If not, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_contribution_ambiguity, empirical, 'Ambiguity regarding the balance of exogenous coercion vs. endogenous acceptance in norm legitimation.').

omega_variable(
    hybrid_legitimation_balance,
    'What was the actual balance between direct state coercion, symbolic authority transfer (e.g., emperor''s example), and institutional incentives in securing compliance with the new norms?',
    'Comparative historical case studies analyzing different state formation processes and the specific mechanisms employed, quantifying the relative impact of violence, cultural influence, and economic incentives.',
    'If symbolic authority or incentives played a more significant role than direct coercion, the constraint might be reclassified as a Tangled Rope (hybrid legitimation) rather than a pure Snare, indicating a more complex interplay of coordination and extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_legitimation_balance, empirical, 'Ambiguity regarding the precise mix of legitimation mechanisms beyond pure coercion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is long-term compliance with state-imposed norms due to ongoing structural coercion, or has it become internalized acceptance of state authority over generations?',
    'Post-coercion compliance trajectory: if compliance persists after the immediate threat of state violence is removed (e.g., during periods of state weakness or decentralization), it suggests partial internalization. If compliance immediately collapses, it indicates continued reliance on structural coercion.',
    'If internalized acceptance is significant, the constraint''s effective suppression is lower than the structural measure suggests, as the population self-regulates. If it remains purely structural, the high suppression is maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for state-imposed norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(impo_tr_t80, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 60, 0.81).
narrative_ontology:measurement(impo_be_t80, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 100, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(impo_su_t80, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 80, 0.89).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
