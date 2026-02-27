% ============================================================================
% CONSTRAINT STORY: gold_fomo_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fomo_cycle, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fomo_cycle
 *   human_readable: The Gold Price 'Fear of Missing Out' Cycle
 *   domain: economic
 *
 * SUMMARY:
 *   This constraint models the market dynamic during a gold price rally where
 *   media hype and rapid price appreciation create a 'fear of missing out'
 *   (FOMO) among retail investors. Late investors are often trapped when the
 *   price corrects, while early investors and gold dealers benefit. The cycle
 *   repeats periodically, driven by economic uncertainty and speculative
 *   sentiment.
 *
 * KEY AGENTS:
 *   - Late Retail Investors: Primary target (powerless/trapped) - enter the market at peak and suffer losses.
 *   - Early Investors: Primary beneficiary (institutional/arbitrage) - benefit from price increase and exit with profits.
 *   - Gold Dealers: Secondary beneficiary (powerful/mobile) - benefit from increased trading volume and higher prices.
 *   - Financial Education Initiatives: Temporary support (organized/analytical) - promote rational investment strategies and risk awareness.
 *   - Traditional Financial Advice: Degraded guidance (moderate/constrained) - inadvertently contributes to the FOMO cycle.
 *   - Analytical Observer: Global view (analytical/analytical) - sees a complex interplay of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fomo_cycle, 0.6).
domain_priors:suppression_score(gold_fomo_cycle, 0.4).
domain_priors:theater_ratio(gold_fomo_cycle, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fomo_cycle, extractiveness, 0.6).
narrative_ontology:constraint_metric(gold_fomo_cycle, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(gold_fomo_cycle, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fomo_cycle, tangled_rope).
narrative_ontology:human_readable(gold_fomo_cycle, "The Gold Price 'Fear of Missing Out' Cycle").
narrative_ontology:topic_domain(gold_fomo_cycle, "economic").

domain_priors:requires_active_enforcement(gold_fomo_cycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, early_investors).
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, gold_dealers).
narrative_ontology:constraint_victim(gold_fomo_cycle, late_retail_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Late retail investors, driven by FOMO, enter the market near its peak and are trapped when the price corrects, suffering losses.
constraint_indexing:constraint_classification(gold_fomo_cycle, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Early investors who bought gold before the rally benefit from the price increase and can exit the market with substantial profits.
constraint_indexing:constraint_classification(gold_fomo_cycle, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Gold dealers benefit from increased trading volume and higher prices during the FOMO cycle, but they also face reputational risks if they are perceived as promoting the hype.
constraint_indexing:constraint_classification(gold_fomo_cycle, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Financial education initiatives attempt to inoculate retail investors against FOMO by promoting rational investment strategies and risk awareness, providing a temporary support to avoid speculative bubbles. Has a sunset clause as better education becomes widespread. The scaffold's sunset derives from the fact that once an investor gains analytical perspective, they should become immune to the FOMO dynamic.
constraint_indexing:constraint_classification(gold_fomo_cycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Traditional financial advice, though intended to guide investors, can sometimes contribute to the FOMO cycle by inadvertently promoting gold as a safe haven asset during times of economic uncertainty, even when valuations are high. This is a degraded function — now theater rather than substance.
constraint_indexing:constraint_classification(gold_fomo_cycle, piton,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% The analytical observer sees the gold FOMO cycle as a tangled rope, characterized by a complex interplay of coordination (information sharing) and extraction (transfer of wealth from late to early investors).
constraint_indexing:constraint_classification(gold_fomo_cycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fomo_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gold_fomo_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gold_fomo_cycle, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fomo_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gold_fomo_cycle, TR),
    TR >= 0.70.

:- end_tests(gold_fomo_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High extraction due to the significant wealth transfer from late to early investors. Suppression (0.40): Moderate suppression due to the strong emotional appeal of gold as a safe haven asset and the lack of readily available information to counter the hype. Theater ratio (0.30): Moderate theater, as the cycle involves both real economic factors (uncertainty) and performative elements (media hype).
 *
 * PERSPECTIVAL GAP:
 *   Late retail investors perceive a snare as they are trapped with losses. Early investors, arbitraging the market, see a rope. Gold dealers see it as a tangled rope where they benefit, but face some reputational risk. Financial education initiatives see it as a scaffold they can address. Traditional financial advice provides a degraded function. Analytical observers see the interplays.
 *
 * DIRECTIONALITY LOGIC:
 *   Early investors and gold dealers benefit (low d), while late retail investors are targeted (high d). Financial education initiatives provide a temporary fix (medium d). Traditional financial advice, though intention is to benefit investors, reinforces the problem (high-medium d). All feeds into the chi formula.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint could be misclassified as pure extraction (Snare) if we only consider late investors. However, the early investors provide a rope, while those who profit are powerful. The financial education scaffolding to stop this is also crucial. Hence, Tangled Rope is the type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_threshold,
    'What level of financial literacy is needed to resist the FOMO effect in gold investing?',
    'Survey retail investors before, during, and after a gold rally to measure literacy and FOMO vulnerability.',
    'High level = the effect continues despite current interventions. Low level = further education will help the powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_threshold, empirical, 'Level of literacy to resist the FOMO effect').

omega_variable(
    media_influence,
    'How much do financial news outlets'' narratives influence the FOMO and extraction?',
    'Test specific narratives on samples of the population.',
    'High influence leads to increased regulation to curtail the narratives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(media_influence, empirical, 'Financial news'' influence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fomo_cycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fomo_cycle, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gold_tr_t5, gold_fomo_cycle, theater_ratio, 5, 0.25).
narrative_ontology:measurement(gold_tr_t10, gold_fomo_cycle, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fomo_cycle, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gold_be_t5, gold_fomo_cycle, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gold_be_t10, gold_fomo_cycle, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fomo_cycle, resource_allocation).
narrative_ontology:affects_constraint(gold_fomo_cycle, economic_uncertainty).
narrative_ontology:affects_constraint(gold_fomo_cycle, investor_irrationality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
