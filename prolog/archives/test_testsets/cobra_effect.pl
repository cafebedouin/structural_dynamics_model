% ============================================================================
% CONSTRAINT STORY: cobra_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cobra_effect, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cobra_effect
 *   human_readable: The Cobra Effect (Perverse Bounty Incentive)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Cobra Effect describes a situation where an intended solution makes a
 *   problem worse. It typically results from poorly designed incentives. The
 *   canonical example involves a bounty placed on cobras in colonial India.
 *   The government intended to reduce the cobra population, but people
 *   started breeding cobras to collect the bounty, leading to an increase in
 *   cobras when the bounty was eventually removed.
 *
 * KEY AGENTS:
 *   - The Public: Primary victim (powerless/trapped) — suffers increased cobra populations and wasted tax revenue.
 *   - The Government (Initial Intention): Well-intentioned actor (institutional/arbitrage) — attempts to solve the problem, but creates perverse incentives.
 *   - Cobra Breeders: Primary beneficiary (powerful/arbitrage) — take advantage of the bounty to profit from cobra breeding.
 *   - Corrupt Officials: Secondary beneficiary (powerful/arbitrage) — may profit from the scheme through kickbacks or bribes.
 *   - Taxpayers: Victim (moderate/constrained) -- lose money in the form of bounty payments to cobra breeders
 *   - Public Safety: Victim (powerless/trapped) -- Cobra population is not reduced and public risk is increased
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cobra_effect, 0.55).
domain_priors:suppression_score(cobra_effect, 0.65).
domain_priors:theater_ratio(cobra_effect, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cobra_effect, extractiveness, 0.55).
narrative_ontology:constraint_metric(cobra_effect, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cobra_effect, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cobra_effect, tangled_rope).
narrative_ontology:human_readable(cobra_effect, "The Cobra Effect (Perverse Bounty Incentive)").
narrative_ontology:topic_domain(cobra_effect, "economic/political").

domain_priors:requires_active_enforcement(cobra_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cobra_effect, cobra_breeders).
narrative_ontology:constraint_beneficiary(cobra_effect, corrupt_officials).
narrative_ontology:constraint_victim(cobra_effect, public_safety).
narrative_ontology:constraint_victim(cobra_effect, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The public is trapped by the unintended consequences of the bounty. They experience increased cobra populations and wasted tax revenue. Their exit options are limited. d=0.95, f(d)=1.42, scope=0.8 --> chi = 0.62
constraint_indexing:constraint_classification(cobra_effect, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% Initially, the government intends to incentivize cobra reduction, creating a rope-like coordination mechanism. d=0.05, f(d)=-0.12, scope=1.0 --> chi = -0.07. But the actual implementation backfires.
constraint_indexing:constraint_classification(cobra_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical observer sees the cobra effect as a tangled rope, a combination of coordination (initial incentive) and extraction (perverse consequences). The analytical observer notes the perverse incentives lead to unintended consequences and extraction from taxpayers. d=0.72, f(d)=1.15, scope=1.2 --> chi = 0.76
constraint_indexing:constraint_classification(cobra_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The cobra breeders arbitrage the bounty. They coordinate cobra breeding to take advantage of the incentive. d=0.05, f(d)=-0.12, scope=0.8 --> chi = -0.05
constraint_indexing:constraint_classification(cobra_effect, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cobra_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cobra_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cobra_effect, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cobra_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cobra_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The Cobra Effect results in a significant extraction from taxpayers, as money is paid out for cobra breeding. Suppression (0.65): Moderate-high. The initial incentive suppresses alternative solutions to cobra control. Theater ratio (0.30): Low. The program does have a real function, but the unintended consequence is that cobra breeding is incentivized. Therefore, it performs poorly.
 *
 * PERSPECTIVAL GAP:
 *   The government views the program as a solution, a rope. The cobra breeders see it as an opportunity, a rope. The analytical observer views the program as a tangled rope, coordination combined with extraction. The public experiences the cobra effect as a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The government initially intends a rope (coordination). Cobra breeders see the bounty as a rope (coordination). Public Safety becomes a snare for the public. Taxpayers are victimized.
 *
 * MANDATROPHY ANALYSIS:
 *   The Cobra Effect can be differentiated from a pure snare because the original intention was to implement a solution. There was a coordination element. The program did provide a benefit to cobra breeders, which is a coordination element. The negative outcome was an unintended consequence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cobra_farming_detectability,
    'How easily can cobra farming be detected and prevented?',
    'Improved monitoring and enforcement mechanisms.',
    'If cobra farming is easily detectable, the cobra effect can be mitigated. If difficult to detect, the cobra effect will persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cobra_farming_detectability, empirical, 'Detectability of cobra farming.').

omega_variable(
    incentive_alignment,
    'How well aligned are the incentives with the desired outcome?',
    'Thorough analysis of potential unintended consequences.',
    'If incentives are well-aligned, the cobra effect can be avoided. If poorly aligned, the cobra effect is likely to occur.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_alignment, conceptual, 'Alignment of incentives with the desired outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cobra_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cobr_tr_t0, cobra_effect, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cobr_tr_t5, cobra_effect, theater_ratio, 5, 0.2).
narrative_ontology:measurement(cobr_tr_t10, cobra_effect, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(cobr_be_t0, cobra_effect, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cobr_be_t5, cobra_effect, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(cobr_be_t10, cobra_effect, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cobra_effect, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
