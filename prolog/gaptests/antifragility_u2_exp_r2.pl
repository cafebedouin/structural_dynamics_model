% ============================================================================
% CONSTRAINT STORY: antifragility_u2_exp_r2
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_exp_r2, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antifragility_u2_exp_r2
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability, resilience,
 *   or robustness as a result of stressors, shocks, volatility, and disorder.
 *   While it can be seen as a fundamental property of all complex adaptive
 *   systems (a Mountain), its application in social and economic contexts
 *   creates a dynamic of asymmetric extraction. Agents who can adopt
 *   'barbell' strategies benefit from upside volatility, while the costs and
 *   downsides are borne by 'fragile' agents optimized for stability.
 *
 * KEY AGENTS:
 *   - Optimized Workers (Victim): Powerless individuals in highly specialized, efficient roles that are brittle to systemic shocks.
 *   - Barbell Practitioners (Beneficiary): Moderate-power individuals or firms who can structure their affairs to have convex responses to volatility.
 *   - Fragilista Bureaucrats (Enforcer/Victim): Institutional actors who seek to eliminate all volatility, thereby creating hidden and catastrophic risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_exp_r2, 0.75).
domain_priors:suppression_score(antifragility_u2_exp_r2, 0.65).
domain_priors:theater_ratio(antifragility_u2_exp_r2, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_exp_r2, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_exp_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_exp_r2, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_exp_r2, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_exp_r2, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_exp_r2, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_exp_r2, barbell_practitioners).
narrative_ontology:constraint_victim(antifragility_u2_exp_r2, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_exp_r2, optimized_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the worker whose job is optimized for efficiency, making them fragile to shocks, the system is a Snare. Their fragility is the source of others' antifragility.
constraint_indexing:constraint_classification(antifragility_u2_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the individual who understands the principle and can structure their life to gain from volatility, it is a Rope—a tool for navigating reality.
constraint_indexing:constraint_classification(antifragility_u2_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% For the institution attempting to suppress volatility, the system is a Tangled Rope. Their actions coordinate short-term stability but generate hidden, systemic fragility and extraction.
constraint_indexing:constraint_classification(antifragility_u2_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% From a long-term, evolutionary perspective, the principle that systems gain from disorder is an unchangeable feature of reality—a Mountain.
constraint_indexing:constraint_classification(antifragility_u2_exp_r2, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (ε=0.75) represents the 'convexity transfer' where one party gains from volatility and uncertainty while another party is harmed by it. The suppression score (0.65) reflects how modern economic and social systems incentivize efficiency and optimization, which systematically creates fragility and suppresses the redundant, 'inefficient' systems that would provide resilience.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme, spanning Mountain, Rope, and Snare. It is determined by an agent's ability to choose their exposure to volatility and benefit from it. Those who can structure their exposure see a Rope (a tool). Those whose fragility is exploited see a Snare (a trap). Those observing the dynamic over evolutionary time see a Mountain (a law of nature).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are those with the knowledge and capital to create convex payoff structures (e.g., venture capitalists, traders). The victims are those locked into concave structures (e.g., salaried employees with no upside, over-leveraged homeowners), where they bear downside risk without access to the corresponding upside. The system extracts resilience and optionality from the victims to subsidize the beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by the Tangled Rope classification, which correctly identifies the dual nature of the system. It has a legitimate coordination function (organizing society for efficiency and stability) but this function is inextricably linked to a predatory, extractive dynamic. This prevents misclassifying it as a pure Snare (which would ignore the coordination benefits) or a pure Rope (which would ignore the vast population of victims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_intent_vs_emergence,
    'Is the high extraction from fragile components an intentionally engineered feature for predatory gain, or an unavoidable emergent property of complex adaptive systems?',
    'Audit of 'skin-in-the-game' distributions across the system. If beneficiaries consistently lack downside exposure, it points towards engineered, predatory extraction.',
    'If predatory: Snare. If emergent necessity: Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_intent_vs_emergence, empirical, 'Distinguishing between engineered predation and emergent necessity in the system's extractive properties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_exp_r2, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(antifragility_tr_t0, antifragility_u2_exp_r2, theater_ratio, 0, 0.15).
narrative_ontology:measurement(antifragility_tr_t10, antifragility_u2_exp_r2, theater_ratio, 10, 0.35).
narrative_ontology:measurement(antifragility_tr_t20, antifragility_u2_exp_r2, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(antifragility_be_t0, antifragility_u2_exp_r2, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(antifragility_be_t10, antifragility_u2_exp_r2, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(antifragility_be_t20, antifragility_u2_exp_r2, base_extractiveness, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
