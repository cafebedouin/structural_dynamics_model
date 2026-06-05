% ============================================================================
% CONSTRAINT STORY: gamblers_ruin_stochastic_extinction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gamblers_ruin_stochastic_extinction, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gamblers_ruin_stochastic_extinction
 *   human_readable: Gambler's Ruin: Stochastic Extinction Under Finite Wealth
 *   domain: mathematical/economic
 *
 * SUMMARY:
 *   Gambler's Ruin is a foundational theorem in probability theory and
 *   mathematical economics, stating that a gambler with finite capital,
 *   playing against an opponent with infinite resources in a fair or
 *   negative-expectation game, faces certain bankruptcy (probability → 1) as
 *   time extends to infinity. The constraint is structurally identical across
 *   all observer perspectives because it emerges from first principles of
 *   probability, not from institutional design. The proof requires no
 *   specification of venue, jurisdiction, or policy — it holds in casinos,
 *   lotteries, financial speculation, insurance claims, and any repeated-bet
 *   scenario. The theater ratio is exceptionally low (0.15) because the
 *   mechanism is entirely mathematical, with no performative or ritualistic
 *   component. Suppression is near-zero (0.02) because the constraint is not
 *   enforced but derived — the gambler faces no hidden barriers or coercive
 *   suppression, only the transparent facts of stochastic dynamics. Base
 *   extractiveness is low (0.08) not because the extraction is weak, but
 *   because 'extraction' is the wrong frame: the constraint is not extracting
 *   wealth from the gambler for the house's benefit, but rather
 *   redistributing capital according to the absorbing boundary conditions of
 *   a random walk. The asymmetry (gambler finite, house infinite) is the
 *   constraint's essence, not its extraction mechanism.
 *
 * KEY AGENTS:
 *   - Individual Gambler: Structural victim (powerless/trapped) — finite capital facing infinite opponent; certain bankruptcy in limit
 *   - The House/Casino: Structural beneficiary (institutional/arbitrage) — infinite effective capital; accumulates ruin probabilities from all finite players; not exempt from theorem if facing larger opponent
 *   - Mathematical Proof Community: Analytical observer (analytical/analytical) — derives and validates the theorem; neutral on distribution of outcomes
 *   - Addiction & Policy Authorities: Organized secondary actors (organized/constrained) — can reduce exposure but cannot change fundamental mechanism; can redistribute pre-game but not prevent post-game ruin
 *   - Financial Speculators: Analogous victims (powerful or moderate, depending on capital relative to market) — face ruin dynamics identical to gamblers but in stock/currency markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gamblers_ruin_stochastic_extinction, 0.08).
domain_priors:suppression_score(gamblers_ruin_stochastic_extinction, 0.02).
domain_priors:theater_ratio(gamblers_ruin_stochastic_extinction, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, extractiveness, 0.08).
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gamblers_ruin_stochastic_extinction, mountain).
narrative_ontology:human_readable(gamblers_ruin_stochastic_extinction, "Gambler's Ruin: Stochastic Extinction Under Finite Wealth").
narrative_ontology:topic_domain(gamblers_ruin_stochastic_extinction, "mathematical/economic").

domain_priors:emerges_naturally(gamblers_ruin_stochastic_extinction).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL GAMBLER (MOUNTAIN) — From the gambler's lived experience, ruin is an inexorable force. With finite wealth and the house's infinite resources, bankruptcy approaches probability 1 regardless of fair odds or negative expectation. No exit strategy changes the terminal outcome. This is not perceived as policy or institution — it is lived as physical inevitability. ε=0.08, accessibility_collapse=0.92, resistance=0.08.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MATHEMATICAL OBSERVER (MOUNTAIN) — The analytical perspective sees Gambler's Ruin as a mathematical theorem derived from first principles: random walks with absorbing barriers, finite starting capital, and an opponent with unbounded resources. The proof is independent of any institution, policy, or human choice. It is a logical consequence of probability theory. The constraint emerges naturally from the formal structure of betting dynamics.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: TREATMENT & POLICY COMMUNITY (MOUNTAIN) — Even organized agents (recovery programs, regulation agencies, public health authorities) cannot change the fundamental stochastic law. They can reduce initiation (prevention), accelerate exit (intervention), or redistribute wealth pre-game (progressive taxation, gambling limits). But the ruin mechanism itself is immutable. Policy can lower exposure but not eliminate the constraint. Classification remains mountain because no intervention can guarantee solvency in perpetual fair/negative games.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE HOUSE (INSTITUTIONAL) (MOUNTAIN) — The casino or house sees ruin as their structural advantage, not as a constraint they manage. The mathematical law ensures eventual accumulation of finite-wealth players' capital. From the house's perspective, the constraint is an enabling law of nature, not a restrictive policy. Yet even they cannot escape it: the theorem applies universally. A house with finite wealth (finite casino, finite city, finite planet) facing a truly infinite opponent would itself face ruin. The mountain applies symmetrically.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gamblers_ruin_stochastic_extinction_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, ExtMetricName, E),
    domain_priors:suppression_score(gamblers_ruin_stochastic_extinction, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gamblers_ruin_stochastic_extinction),
    narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gamblers_ruin_stochastic_extinction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): The low value reflects that this is a pure structural theorem, not a policy-enforced extraction. The house doesn't 'extract' capital; rather, the mathematics of finite capital in infinite games redistributes it. The 0.08 value captures the minor institutional overhead: odds-setting, venue operation, rule enforcement. The core mechanism (ruin probability) is costless. Suppression (0.02): Negligible. The gambler faces no hidden suppression or coerced barriers — the constraint is fully transparent and mathematically proven. Entry and exit are formally unconstrained (though practically psychologically/addiction-driven). Theater ratio (0.15): Very low. The theorem has no performative element — it is pure mathematical proof. The minor theater reflects that real casinos add ambient experience (lights, sounds, social pressure) that disguises the theorem's coldness, but this is not essential to the constraint itself. Accessibility collapse (0.92): Very high. The constraint is maximally difficult to escape because it is a mathematical law, not a policy. No appeal, no workaround, no exception exists for sufficiently long timescales. Resistance (0.08): Very low. The mechanism faces no logical resistance — the proof is airtight. Empirical resistance (e.g., Kelly betting strategies, wealth redistribution) can only delay, not prevent, eventual absorption by the lower boundary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is rare in that ALL perspectives converge on mountain classification. There is no perspectival gap because the theorem is independent of observer position. The gambler sees ruin as fate. The house sees ruin as their structural advantage. The mathematician sees it as a theorem. The policy maker sees it as an immutable constraint on what regulation can achieve. All observers, standing in different structural positions, see the same law apply universally. This consensus suggests that Gambler's Ruin is a genuine natural law within its domain of validity (finite capital, infinite opponent, random walk dynamics), not a contingent institutional arrangement. The absence of a perspectival gap is itself informative: it confirms that the constraint is not a Rope (which would appear different to beneficiaries and victims), not a Snare (which would have hidden extraction mechanisms), not a Scaffold (which would have visible sunsets from some positions). It is a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality analysis is simplified here because beneficiary/victim language is not primary. The constraint is not an extraction mechanism but a stochastic law. However, we can map structural positions: The gambler is the 'victim' in the sense that they absorb ruin; their d≈0.95 reflects trapped exit with no structural escape. The house is the 'beneficiary' in the sense that they accumulate capital; their d≈0.05 reflects that they are on the absorbing boundary from below (bankruptcy) and the absorbing boundary from above is infinity. The mathematician is neutral (d≈0.72, analytical position). The policy maker is constrained (d≈0.50, they can redistribute pre-game but not prevent post-game absorption). All d values are structural consequences of probability theory, not negotiable positions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuous_vs_discrete_gap,
    'Does the theoretical continuous-time diffusion limit (Brownian motion with absorbing barriers) perfectly capture the empirical dynamics of discrete-time betting games?',
    'Comparison of continuous diffusion predictions with high-frequency discrete-bet data from laboratory gambling experiments; analysis of discretization error as bet size and time interval vary',
    'If gap is negligible (< 1% prediction error): continuous model fully certifies mountain classification. If gap is substantial (> 5%): discrete empirical process may have structural properties (e.g., Kelly criterion edge recovery, bet-size adaptation) that weaken the ruin guarantee.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuous_vs_discrete_gap, empirical, 'Degree of correspondence between continuous diffusion limit and discrete betting empirics').

omega_variable(
    time_subjective_vs_objective,
    'Is ''inevitability of ruin'' experienced the same way across objective time (mathematical limit as t→∞) and subjective lifetime (biographical horizon of 50-80 years)?',
    'Longitudinal study of actual gambling duration before exit or ruin; correlation between mathematical ruin timeline (computed from bet size, odds, starting capital) and observed biological/financial lifespan',
    'If expected ruin time >> human lifetime for realistic bet sizes: the constraint is mathematically mountain but practically rope or scaffold (long enough to feel like coordination/temporary). If ruin time < biological lifespan for most players: mountain classification is empirically sharp.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(time_subjective_vs_objective, empirical, 'Relationship between mathematical infinity and human biographical time').

omega_variable(
    wealth_redistribution_counterfactual,
    'If the initial wealth distribution were equalized (both players start with equal capital), does the theorem still classify as mountain or does it degrade to rope (symmetric coordination)?',
    'Formal analysis of symmetric random walk (equal starting capital, fair game); proof that symmetric walks have zero absorption probability (neither player faces certain ruin). Compare classification under symmetric vs asymmetric setup.',
    'If symmetry degrades the mountain to rope: the constraint is not about randomness per se but about wealth asymmetry. The ''natural law'' is conditional on initial inequality — revealing the constraint as a mathematical encoding of power difference, not physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wealth_redistribution_counterfactual, conceptual, 'Whether ruin is inherent to gambling or inherent to wealth asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gamblers_ruin_stochastic_extinction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gruin_tr_t0, gamblers_ruin_stochastic_extinction, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gruin_tr_t5, gamblers_ruin_stochastic_extinction, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gruin_tr_t10, gamblers_ruin_stochastic_extinction, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(gruin_be_t0, gamblers_ruin_stochastic_extinction, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(gruin_be_t5, gamblers_ruin_stochastic_extinction, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(gruin_be_t10, gamblers_ruin_stochastic_extinction, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gamblers_ruin_stochastic_extinction, information_standard).
narrative_ontology:affects_constraint(gamblers_ruin_stochastic_extinction, martingale_betting_impossibility).
narrative_ontology:affects_constraint(gamblers_ruin_stochastic_extinction, random_walk_absorption_law).

% DUAL FORMULATION NOTE:
% Gambler's Ruin is upstream of several derivative constraints: the impossibility of martingale betting strategies (no strategy can guarantee positive expectation against a fair game) depends on the ruin theorem's underlying random walk dynamics. The absorption law for symmetric random walks is the mathematical substrate from which ruin emerges when one boundary is absorbing (bankruptcy) and the other is semi-infinite (house resources).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
