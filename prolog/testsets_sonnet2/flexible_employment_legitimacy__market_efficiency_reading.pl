% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)
 *   domain: labor_economics/platform_economy
 *
 * SUMMARY:
 *   This story instantiates the market-efficiency reading of the
 *   flexible-employment-legitimacy kernel: platform and gig-economy labor
 *   arrangements are read as a genuine market-clearing mechanism, in which
 *   algorithmic matching discovers efficient prices between a flexible labor
 *   supply and volatile service demand, wage convergence with traditional
 *   blue-collar work signals relative labor scarcity rather than downward
 *   pressure, and worker autonomy is treated as maximized rather than
 *   illusory. This is a distinct constraint from the
 *   precarity_extraction_reading (which reads the same arrangement as
 *   structural precarity enabling platform surplus extraction) and the
 *   developmental_state_reading (which reads it as a transitional form
 *   requiring state-managed formalization). All three share the underlying
 *   kernel — the legitimacy status of flexible employment — but author
 *   different beneficiary/victim structures and different ε, because each
 *   reading is a different constraint under the ε-invariance principle, not a
 *   different observable of one constraint.
 *
 * KEY AGENTS:
 *   - platform_workers_seeking_autonomy: primary beneficiary (moderate/mobile) — clears the market at a wage reflecting genuine preference for flexibility
 *   - platform_operators: agenda_setter (institutional/arbitrage) — builds and profits from the coordination infrastructure
 *   - consumers_of_on_demand_services and employers_facing_variable_demand: secondary beneficiaries — get liquidity and staffing elasticity
 *   - traditional_full_time_workforce and labor_regulators: observers who see the same wage-convergence data and, in this reading, interpret it as scarcity signal rather than wage suppression
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
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, 'b66ba313-d147-4b72-843e-35a14ce9a26d').
narrative_ontology:cs_kernel_codification('b66ba313-d147-4b72-843e-35a14ce9a26d', distributed).
narrative_ontology:cs_authority_grounding('b66ba313-d147-4b72-843e-35a14ce9a26d', distributed).
narrative_ontology:cs_reading_relation('b66ba313-d147-4b72-843e-35a14ce9a26d', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b66ba313-d147-4b72-843e-35a14ce9a26d', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('b66ba313-d147-4b72-843e-35a14ce9a26d', foundational, wage_convergence_reflects_scarcity_signal).
narrative_ontology:cs_axiom_status(wage_convergence_reflects_scarcity_signal, holdable).
narrative_ontology:cs_axiom_grounding('b66ba313-d147-4b72-843e-35a14ce9a26d', wage_convergence_reflects_scarcity_signal, empirically_contingent).
narrative_ontology:cs_axiom('b66ba313-d147-4b72-843e-35a14ce9a26d', foundational, algorithmic_matching_is_neutral_coordination).
narrative_ontology:cs_axiom_status(algorithmic_matching_is_neutral_coordination, holdable).
narrative_ontology:cs_axiom_grounding('b66ba313-d147-4b72-843e-35a14ce9a26d', algorithmic_matching_is_neutral_coordination, empirically_contingent).
narrative_ontology:cs_axiom('b66ba313-d147-4b72-843e-35a14ce9a26d', secondary, worker_exit_option_constitutes_genuine_consent).
narrative_ontology:cs_axiom_status(worker_exit_option_constitutes_genuine_consent, holdable).
narrative_ontology:cs_axiom_grounding('b66ba313-d147-4b72-843e-35a14ce9a26d', worker_exit_option_constitutes_genuine_consent, instrumental).
narrative_ontology:cs_reference_frame('b66ba313-d147-4b72-843e-35a14ce9a26d', competitive_labor_market_clearing).
narrative_ontology:cs_drift_state('b66ba313-d147-4b72-843e-35a14ce9a26d', post_gig_economy_expansion, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b66ba313-d147-4b72-843e-35a14ce9a26d', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_workers_seeking_autonomy).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_on_demand_services).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, employers_facing_variable_demand).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, wage_convergence_as_scarcity_signal).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, algorithmic_matching_neutrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Choose gig or platform work over traditional employment because it lets them set hours, combine income streams, or work around caregiving and study. From this reading's vantage, the wage they clear at reflects the going rate for flexible, low-commitment labor supply meeting fluctuating demand; they can log off, switch platforms, or return to traditional employment if the terms stop working for them.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_workers_seeking_autonomy, beneficiary,
    moderate, biographical, mobile, national).

% Build and run the matching algorithms that pair available workers with jobs or rides or deliveries in real time. In this reading, they are infrastructure providers solving a genuine coordination problem — discovering price and matching supply to demand faster than any prior labor-market mechanism — and they profit from the efficiency they create, not from suppressing an alternative.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, national).

% Get rides, deliveries, and services on demand at a price set by real-time supply and demand matching. They benefit from the liquidity the flexible labor pool provides and would face higher prices or longer waits under a more rigid staffing model.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_on_demand_services, beneficiary,
    moderate, immediate, mobile, national).

% Retailers, logistics firms, and seasonal businesses use flexible staffing to scale labor up or down with demand swings they cannot predict far in advance. In this reading, this is legitimate risk management, not cost-shifting — they pay the market-clearing wage for the flexibility they need.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, employers_facing_variable_demand, beneficiary,
    powerful, biographical, mobile, national).

% Work standard employment arrangements alongside the growing flexible-work sector. They observe wage convergence between flexible and traditional roles for comparable blue-collar work and, in this reading, read that convergence as a market signal — a fair-value discovery process reducing an oversupply of workers relative to the demand for fixed-schedule labor, not as downward pressure engineered by employers.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, traditional_full_time_workforce, observer,
    moderate, biographical, constrained, national).

% Monitor classification disputes (employee vs. independent contractor) and wage-floor compliance. In this reading, their role is to verify that the matching mechanism functions competitively and that entry/exit remains genuinely open — not to correct a captured market, since none is claimed to exist here.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches a fluctuating, decentralized supply of available labor-hours to a fluctuating, decentralized demand for on-demand services and staffing, using algorithmic price and dispatch signals in place of long-notice scheduling or fixed hiring.
% TRANSFER_FUNCTION: Moves compensation from consumers and businesses with variable demand to workers who supply hours flexibly, at a wage the reading treats as market-clearing; no party is understood to be extracting a surplus beyond the value of the coordination itself.
% ABSENT_VOICES: Workers who would prefer stable, benefits-bearing employment but find flexible work is the only option available in their local labor market are not centered in this reading; they would object that 'choice' language obscures a shrinking supply of traditional jobs, but this reading treats their absence from stable employment as a supply-side allocation outcome, not a grievance requiring a seat.
% DISAPPEARANCE_RATIONALE: If flexible employment arrangements were banned or heavily restricted overnight, this reading holds that on-demand services would become scarcer and costlier, many workers who value flexibility would lose a viable income option, and demand-variable employers would face higher fixed costs — a real rearrangement. Other readings dispute whether this rearrangement is a loss of genuine coordination or the collapse of an extraction arrangement, which is exactly the site of the kernel contest.
% FOUNDING_PROBLEM: Traditional fixed-schedule employment could not efficiently match highly variable, spot-market demand (ride requests, deliveries, retail surges) with a labor supply that also wanted variable hours; flexible employment arrangements and platform matching technology were built to solve that two-sided matching problem.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying two-sided matching markets and independent survey data on worker-stated preferences for schedule flexibility (cited outside platform-operator marketing) corroborate that a genuine matching problem exists and that a meaningful share of flexible workers report preferring the arrangement; this corroboration is weaker for the subset of workers with no viable traditional alternative, which is exactly where the sibling readings locate their disagreement.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, contested).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored low (0.28) because this reading holds that the wage workers clear at reflects a real, if imperfect, price discovery process rather than a captured rent; suppression is low (0.22) because exit to traditional employment or to other platforms remains structurally open under this reading's own premises. Accessibility collapse is moderate-low (0.30) and resistance is moderate (0.35): the reading acknowledges some workers face constrained alternatives, which is why it is not authored as a mountain, but it does not treat that constraint as coercive extraction. These values are authored from the market-efficiency reading's own lights, per the ε-referent rule for kernel readings — a different reading of the same underlying arrangement would author starkly higher ε for the identical facts on the ground.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators sit near the beneficiary end (institutional power, arbitrage exit, direct profit from the coordination function they built). Flexible workers under this reading are also beneficiaries — the reading's core commitment is that they are not targets of extraction but co-participants in a market-clearing exchange, so no victim group is declared. Consumers and variable-demand employers are secondary beneficiaries of the liquidity the arrangement provides. No stakeholder is declared payer/victim in this reading, which is exactly the structural delta the kernel context specifies relative to the precarity_extraction_reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy in the other direction from a typical Tangled Rope story: rather than asking whether a coordination function has decayed into pure extraction, it asserts the coordination function is intact and the founding problem (matching variable supply to variable demand) remains live. The founding_problem_status is authored 'live' rather than 'dead', which is the reading's own position — the sibling readings would author this differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_convergence_signal_or_suppression,
    'Does observed wage convergence between flexible and traditional blue-collar work reflect a genuine scarcity/oversupply signal (this reading''s premise) or algorithmically-enabled downward wage pressure enabled by information asymmetry and atomized bargaining (the precarity_extraction_reading''s premise)?',
    'Longitudinal wage data compared against independent measures of labor supply/demand tightness in the same local markets, plus analysis of whether platform algorithmic pricing responds to worker bargaining power signals or suppresses them.',
    'If convergence tracks independently-measured scarcity, this reading''s coordination-function claim is supported. If convergence persists or accelerates despite tightening labor markets, the precarity_extraction_reading''s suppression claim gains support and this constraint''s ε would need to be re-examined as a separate empirical matter (not by changing this file, but by weighing the sibling reading more heavily in downstream analysis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_convergence_signal_or_suppression, empirical, 'Whether wage convergence is a market signal or an extraction artifact — the central empirical fork between this reading and its precarity sibling.').

omega_variable(
    algorithmic_neutrality_ambiguity,
    'Are platform matching algorithms genuinely neutral price-discovery mechanisms, or do they encode systematic advantages for the platform operator (e.g., information asymmetry, dynamic pricing that extracts more from workers with fewer alternatives)?',
    'Independent algorithmic audit of dispatch and pricing logic across worker segments with different measured exit options; comparison of effective hourly pay against platform take-rate over time.',
    'Neutral coordination supports the rope/market-efficiency classification; systematic operator-favoring logic would shift the constraint toward the tangled_rope/snare territory the precarity_extraction_reading occupies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_neutrality_ambiguity, empirical, 'Whether the algorithmic matching function is structurally neutral, as this reading assumes.').

omega_variable(
    reading_selection_grounds,
    'What structural signals justify treating this arrangement under the market-efficiency framing rather than the precarity-extraction or developmental-state framing, given that all three readings describe the same underlying platform labor arrangement?',
    'This reading is selected when worker-stated preference surveys, low switching costs across platforms/traditional employment, and absence of exclusive lock-in mechanisms are dominant in the empirical record for a given labor market segment; the sibling readings would be selected on different dominant signals (concentration of platform market power, absence of viable exit, presence of formal-sector transition policy).',
    'If the dominant empirical signals for a given local labor market are lock-in and market concentration rather than genuine worker choice, the precarity_extraction_reading is the more structurally accurate constraint for that market segment, and this file''s classification would not apply there.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_grounds, conceptual, 'Documents the framing choice between kernel readings and what would flip the appropriate reading for a given empirical context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 20).

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

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(flex_be_t4, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(flex_be_t16, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 20, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(flexible_employment_legitimacy__market_efficiency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'flexible employment legitimacy' kernel into structurally distinct constraints per the ε-invariance principle. market_efficiency_reading authors low ε (0.28) with no victims declared; precarity_extraction_reading authors substantially higher ε with victims declared (platform workers as targets); developmental_state_reading is scaffold-shaped with a sunset tied to formalization policy. All three share the same kernel_id (flexible_employment_legitimacy) but are linked, not merged, per the BGS decomposition pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
