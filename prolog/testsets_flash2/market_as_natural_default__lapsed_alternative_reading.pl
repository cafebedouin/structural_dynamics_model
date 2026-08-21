% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint describes the 'market as natural default' from the
 *   perspective of the 'lapsed alternative' reading. It posits that market
 *   dominance is perceived as natural not due to active enforcement or direct
 *   beneficiary capture, but because historical alternatives have simply been
 *   forgotten or rendered invisible over time. This leads to a low
 *   extractiveness and suppression, as the constraint operates primarily
 *   through cognitive inertia and a lack of imaginative alternatives, rather
 *   than active coercion. The classification as 'mountain' reflects this
 *   reading's assertion that the perceived naturalness is a deep-seated,
 *   almost geological feature of collective memory, rather than a human
 *   construct.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.05).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '8dec2f12-e5ae-4a30-8d18-4c06be2adc50').
narrative_ontology:cs_kernel_codification('8dec2f12-e5ae-4a30-8d18-4c06be2adc50', implicit).
narrative_ontology:cs_authority_grounding('8dec2f12-e5ae-4a30-8d18-4c06be2adc50', diffuse_epistemic).
narrative_ontology:cs_reading_relation('8dec2f12-e5ae-4a30-8d18-4c06be2adc50', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('8dec2f12-e5ae-4a30-8d18-4c06be2adc50', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('8dec2f12-e5ae-4a30-8d18-4c06be2adc50', foundational, historical_amnesia_drives_naturalization).
narrative_ontology:cs_axiom_status(historical_amnesia_drives_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('8dec2f12-e5ae-4a30-8d18-4c06be2adc50', historical_amnesia_drives_naturalization, empirically_contingent).
narrative_ontology:cs_reference_frame('8dec2f12-e5ae-4a30-8d18-4c06be2adc50', historical_contingency_of_economic_systems).
narrative_ontology:cs_drift_state('8dec2f12-e5ae-4a30-8d18-4c06be2adc50', contemporary_economic_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8dec2f12-e5ae-4a30-8d18-4c06be2adc50', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research and document the historical contingency of market structures and the existence of forgotten alternatives. Their work can challenge the 'natural' perception of market dominance.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% Operates within market structures, perceiving them as natural or inevitable due to a lack of historical awareness of alternatives. Bears the diffuse costs of limited choice without recognizing them as imposed.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, payer,
    powerless, biographical, identity_locked, national).

% Formulate economic policy within the perceived 'natural' constraints of market dominance, often unaware of or disinclined to explore historically suppressed alternatives. Their actions reinforce the default.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint implicitly coordinates societal expectations around existing market structures, reducing friction by presenting them as the only viable option.
% TRANSFER_FUNCTION: Diffusely transfers agency and imaginative capacity from the general public and policymakers to the 'natural' order of the market, limiting the scope for alternative economic arrangements.
% ABSENT_VOICES: Historical proponents of alternative economic systems (e.g., mutualism, guild socialism, various forms of cooperative economics) are absent from contemporary discourse, their ideas having lapsed from collective memory.
% DISAPPEARANCE_RATIONALE: If the 'lapsed alternative' aspect of market naturalization vanished overnight (i.e., everyone suddenly remembered all historical alternatives), the immediate market structures would not change. The *perception* of their naturalness would be shattered, leading to potential long-term shifts in policy and public imagination, but the physical and institutional infrastructure would remain.
% FOUNDING_PROBLEM: The problem of organizing complex societies and allocating resources efficiently.
% FOUNDING_PROBLEM_CORROBORATION: Economists and sociologists generally agree that societies face ongoing challenges in resource allocation and organization. The specific 'solution' of market dominance, however, is contested by critical historians and political economists.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_unchanged).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.12, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because no specific agent actively 'extracts' from the forgetting itself; the 'cost' is diffuse and borne by the general public in terms of limited choice. Suppression is also low (0.05) as there's no active, coercive suppression of alternatives, merely a historical amnesia. Theater ratio is low (0.08) because there's little performative maintenance; the constraint persists through inertia. Accessibility collapse is high (0.88) because, from the perspective of the general public, alternatives are effectively invisible. Resistance is low (0.03) because the constraint is not perceived as a constraint at all, but as a natural state.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the general public, the market's dominance is simply 'how things are' – a mountain. From the perspective of critical economic historians, it's a contingent historical outcome that has been naturalized through forgetting, making it a 'false summit' that could be dismantled by recovering lost knowledge. The engine's FSM detection will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Economic historians are observers, seeking to uncover the contingency of the 'natural' market. The general public and policymakers are diffuse payers, bearing the costs of limited imagination and policy options, but without a clear beneficiary to whom these costs are transferred. There are no direct beneficiaries of the 'lapsed memory' itself, only of the market structures that benefit from this naturalization.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_passive_naturalization,
    'Is the naturalization of market dominance a passive outcome of historical forgetting, or is it actively maintained and reinforced by identifiable beneficiaries?',
    'Empirical research into lobbying efforts, public relations campaigns, and educational curricula that promote market inevitability. If active maintenance is found, reclassify towards a more extractive type.',
    'If active maintenance is significant, the constraint''s extractiveness and suppression are higher than currently measured, and its true type is closer to a Snare or Tangled Rope. If purely passive, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_vs_passive_naturalization, empirical, 'Distinguishes between passive historical amnesia and active ideological maintenance of market naturalness.').

omega_variable(
    recoverability_of_alternatives,
    'To what extent are the ''lapsed'' historical alternatives genuinely recoverable and implementable in contemporary contexts, or are they merely historical curiosities?',
    'Pilot programs, policy experiments, and comparative studies of societies that have implemented or are implementing non-market or alternative market structures. If successful, the accessibility collapse is lower than currently measured.',
    'If alternatives are highly recoverable, the ''accessibility_collapse'' metric is lower, suggesting the constraint is less ''mountain-like'' and more amenable to change. If not, the mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recoverability_of_alternatives, empirical, 'Assesses the practical viability of historically forgotten economic alternatives.').

omega_variable(
    reading_framing_choice,
    'Is the ''lapsed alternative'' reading the most appropriate framing for market naturalization, or do the ''beneficiary maintained'' or ''hybrid amnesia'' readings better capture the structural dynamics?',
    'Comparative analysis of the empirical evidence supporting each reading, focusing on the presence and impact of active beneficiary defense versus passive historical forgetting. The choice of framing depends on which mechanism is dominant.',
    'Adopting a different reading would lead to a reclassification of the constraint, likely towards a more extractive type (Tangled Rope or Snare) if active beneficiary maintenance or hybrid dynamics are found to be dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Ambiguity in the primary mechanism driving market naturalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(mark_tr_t1970, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(mark_tr_t1990, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(mark_tr_t2010, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(mark_tr_t2024, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(mark_be_t1970, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1970, 0.11).
narrative_ontology:measurement(mark_be_t1990, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(mark_be_t2010, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(mark_be_t2024, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1950, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(mark_su_t1970, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(mark_su_t1990, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(mark_su_t2010, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 2010, 0.05).
narrative_ontology:measurement(mark_su_t2024, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market as natural default' kernel. This 'lapsed alternative' reading emphasizes historical forgetting, while others focus on active maintenance or hybrid dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
