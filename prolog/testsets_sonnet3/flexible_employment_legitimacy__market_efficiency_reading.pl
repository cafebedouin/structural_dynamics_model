% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__market_efficiency_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Market-Clearing Mechanism (Efficiency Reading)
 *   domain: economic/labor
 *
 * SUMMARY:
 *   This story instantiates the market-efficiency reading of the
 *   flexible-employment-legitimacy kernel: platform-mediated flexible work is
 *   treated as a genuine market-clearing mechanism, where wage convergence
 *   between platform and traditional blue-collar sectors is read as a
 *   scarcity signal, algorithmic matching is treated as neutral price
 *   discovery, and worker participation is treated as revealed preference for
 *   autonomy. This is a DISTINCT constraint from the
 *   precarity_extraction_reading (which treats the same wage convergence and
 *   algorithmic control as evidence of structural extraction) and the
 *   developmental_state_reading (which treats the arrangement as a
 *   transitional form requiring state formalization). Per the ε-invariance
 *   principle, each reading gets its own file with its own stable ε — this
 *   file's ε (0.28) reflects genuine, low-but-nonzero coordination cost as
 *   seen from this reading's own lights, not the higher ε the precarity
 *   reading would author for the same standing arrangement.
 *
 * KEY AGENTS:
 *   - platform_operators: primary agenda-setter and beneficiary, institutional power, arbitrage exit
 *   - flexible_workers_with_alternatives: beneficiary under this reading, moderate power, mobile exit
 *   - consumers_of_on_demand_services: beneficiary, organized power
 *   - labor_economists_market_school: analytical observer corroborating the efficiency framing
 *   - workers_without_realistic_alternatives: excluded from this reading's own account of who is participating and why
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.28).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.22).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market-Clearing Mechanism (Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "economic/labor").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, 'd5832d24-39c4-45b3-bcfb-d97b7fc8a4c6').
narrative_ontology:cs_kernel_codification('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', distributed).
narrative_ontology:cs_authority_grounding('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', distributed).
narrative_ontology:cs_reading_relation('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', foundational, wage_convergence_signals_genuine_scarcity).
narrative_ontology:cs_axiom_status(wage_convergence_signals_genuine_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', wage_convergence_signals_genuine_scarcity, empirically_contingent).
narrative_ontology:cs_axiom('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', secondary, algorithmic_matching_is_neutral_coordination).
narrative_ontology:cs_axiom_status(algorithmic_matching_is_neutral_coordination, holdable).
narrative_ontology:cs_axiom_grounding('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', algorithmic_matching_is_neutral_coordination, empirically_contingent).
narrative_ontology:cs_reference_frame('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', competitive_labor_market_clearing).
narrative_ontology:cs_drift_state('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', post_gig_economy_expansion, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('d5832d24-39c4-45b3-bcfb-d97b7fc8a4c6', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers_with_alternatives).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_on_demand_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate matching algorithms that connect available labor with demand spikes in real time. Under this reading, the algorithm is a neutral price-discovery mechanism, not a control instrument; the platform's returns come from solving a genuine coordination problem — reducing search and idle-time costs for both sides of the labor market.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter).

% Choose flexible platform work over standard employment because it lets them combine multiple income streams, set their own hours, or bridge between other opportunities. From this seat, wage convergence toward a market-clearing rate reflects genuine scarcity signals rather than coercion, and the ability to log off is real, not nominal.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers_with_alternatives, beneficiary,
    moderate, biographical, mobile, regional).

% Purchase on-demand labor (rides, deliveries, task work) at prices that reflect real-time supply and demand. They benefit from lower prices and faster fulfillment when labor supply is abundant, and accept higher prices when it is scarce — the price signal is treated as legitimate information, not extraction.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_on_demand_services, beneficiary,
    organized, immediate, arbitrage, national).

% Study wage convergence data across platform and traditional sectors and interpret narrowing wage gaps as evidence that flexible employment is successfully clearing previously segmented blue-collar labor markets. They treat the algorithmic wage-setting process as functionally equivalent to a competitive auction.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_economists_market_school, observer,
    analytical, generational, analytical, national).

% Depend on platform work as their primary or sole income source with few comparable alternatives nearby. This reading treats their participation as a revealed preference for flexibility, but they are not seated in this account as a distinct group with different constraints — their situation is assumed, not examined, under the efficiency framing.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, workers_without_realistic_alternatives, excluded,
    powerless, immediate, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches intermittent, geographically or temporally variable labor demand (rides, deliveries, short-term tasks) with a pool of workers who have variable availability, reducing search costs on both sides relative to fixed-schedule employment arrangements.
% TRANSFER_FUNCTION: Moves compensation from consumers and platform operators to workers at a rate the algorithm computes from real-time supply and demand; under this reading, no systematic transfer runs the other direction because the wage is a market-clearing price rather than an imposed extraction.
% ABSENT_VOICES: Workers without realistic alternatives to platform income are present in the labor pool but not distinctly seated in the market-efficiency account, which treats all participation as equally voluntary regardless of the worker's outside options; a fuller accounting would need to ask them directly whether their participation reflects a chosen flexibility premium or an absence of better options.
% DISAPPEARANCE_RATIONALE: If flexible employment matching disappeared overnight, the coordination function it performs (matching intermittent demand to available labor) would still need solving through some other mechanism — traditional employment, staffing agencies, or public labor exchanges — and consumer prices for on-demand services would likely rise as search costs returned.
% FOUNDING_PROBLEM: Traditional fixed-schedule employment could not efficiently match highly variable, spiky demand (ride requests, deliveries, seasonal retail) with available labor, leaving both demand unmet and worker time under-utilized during low-demand periods.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying matching efficiency and search-cost reduction in on-demand markets attest that variable demand still exists and still requires some matching mechanism; this corroboration comes from academic labor-market researchers rather than exclusively from platform operators, though many such studies are platform-funded, which limits full independence.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).
:- end_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because, from this reading's own lights, the wage the algorithm sets is a market-clearing price responding to genuine scarcity and abundance signals, not an imposed extraction — the small nonzero value reflects ordinary platform take-rate and matching-service costs, not rent. Suppression is low (0.22) because this reading holds that workers are not coerced into participation and retain the ability to exit to other income sources. Accessibility collapse is moderate (0.35) rather than low, because once a worker has entered the platform ecosystem, the algorithm becomes the dominant coordination mechanism for that labor segment, narrowing (without eliminating) practical alternatives even under the efficiency framing. Resistance is moderate-low (0.3): this reading acknowledges that some workers and labor advocates push back on wage-setting practices, but treats that resistance as a minority position rather than as evidence the arrangement is extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators sit near the beneficiary end: they collect transaction value from solving a real coordination problem and retain arbitrage-grade exit (they can adjust the algorithm, enter new markets, or restructure pricing). Flexible workers with genuine alternatives also sit near the beneficiary end under this reading, because their exit options are treated as mobile — they can leave for other work without severe cost. Consumers benefit from price signals reflecting real supply and demand. Workers without realistic alternatives are the structural pressure point this reading does not fully seat: their exit options would need to be authored as constrained rather than mobile, which is exactly the parameter the sibling precarity reading disputes.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy in one direction: it does not claim the coordination function (matching variable demand to variable labor supply) has disappeared while the mechanism persists — the founding problem is authored as live, corroborated by labor-market search-cost research outside the platforms themselves. The reading's vulnerability is the opposite one: by authoring the exit options of workers-without-alternatives as effectively identical to those of workers-with-alternatives, it risks treating a captured population as if it were a mobile one. This is exactly the structural element the precarity_extraction_reading disputes, and exactly why the two readings must remain separate constraint files rather than one story with a hedge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_convergence_referent_ambiguity,
    'Does the narrowing wage gap between platform and traditional blue-collar sectors reflect genuine labor scarcity being priced correctly (this reading), or does it reflect a race-to-the-bottom equilibrium where traditional employers cut wages to compete with platform rates set by algorithmic wage suppression (the precarity reading)?',
    'Longitudinal wage data disaggregated by worker outside-option availability: if wages converge upward (traditional wages rising toward platform rates) that supports scarcity-signal reading; if convergence is downward (platform rates suppressing traditional wages) that supports the extraction reading.',
    'Determines whether this reading''s core empirical premise (wage convergence as scarcity signal) survives contact with the data, or whether the constraint this reading describes is better modeled as the precarity_extraction_reading''s object.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_convergence_referent_ambiguity, empirical, 'Whether wage convergence is a genuine scarcity signal or downward wage suppression across sectors.').

omega_variable(
    worker_heterogeneity_within_reading,
    'Can a single reading legitimately treat ''flexible workers with alternatives'' and ''workers without realistic alternatives'' as one undifferentiated population sharing mobile exit options, or does this collapse two structurally distinct positions into one to preserve the efficiency framing?',
    'Survey data on worker-reported alternative income sources and exit intentions, cross-tabulated against actual platform dependency (share of income from platform work).',
    'If the population is substantially heterogeneous with a large low-alternative segment, this reading''s authored exit_options=mobile for the worker seat is descriptively false for a meaningful share of the population it claims to describe, which is precisely the gap the precarity_extraction_reading is built to capture as a separate constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_heterogeneity_within_reading, conceptual, 'Whether treating all flexible workers as similarly mobile obscures a heterogeneous population with divergent structural positions.').

omega_variable(
    algorithmic_neutrality_assumption,
    'Is the platform''s matching and pricing algorithm a neutral price-discovery mechanism (this reading), or does it embed design choices (surge pricing thresholds, acceptance-rate penalties, deactivation triggers) that function as directional control rather than neutral coordination?',
    'Algorithmic audit of platform pricing and matching logic, comparing whether wage-setting parameters are set by aggregate supply/demand alone or incorporate platform-margin-protecting adjustments.',
    'If the algorithm embeds margin-protecting adjustments beyond pure supply/demand matching, the ''neutral coordination'' premise central to this reading is undermined, and the constraint''s structural character shifts toward the tangled_rope or snare territory the sibling reading claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_neutrality_assumption, conceptual, 'Whether platform algorithms are neutral market-clearing tools or embed extractive design choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(flex_tr_t4, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(flex_tr_t16, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(flex_tr_t24, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(flex_be_t4, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 4, 0.23).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(flex_be_t16, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(flex_be_t24, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(flexible_employment_legitimacy__market_efficiency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__market_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the flexible_employment_legitimacy kernel. market_efficiency_reading (this file, ε=0.28, rope) treats wage convergence as a scarcity signal and algorithms as neutral coordination. precarity_extraction_reading (sibling, substantially higher ε, tangled_rope or snare) treats the same standing arrangement as structural extraction with an explicit victim class. developmental_state_reading (sibling, scaffold with sunset logic) treats the arrangement as a transitional form requiring state-managed formalization. All three describe the SAME standing arrangement (platform-mediated flexible employment) under different readings' own lights, per the ε-invariance principle's rule that the referent is the standing arrangement, not any reading's endorsed alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
