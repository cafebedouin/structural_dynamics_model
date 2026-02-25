% ============================================================================
% CONSTRAINT STORY: capital_misallocation_spiral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capital_misallocation_spiral, []).

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
 *   constraint_id: capital_misallocation_spiral
 *   human_readable: The Zombie Asset Loop
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Zombie Asset Loop describes a feedback cycle where accommodative
 *   monetary policy or institutional mandates, initially intended for
 *   stabilization, lead to the sustained misallocation of capital. Capital
 *   flows to unproductive 'zombie' firms or assets, keeping them alive but
 *   generating little to no real growth. The resulting economic stagnation
 *   then justifies the continuation of the accommodative policies,
 *   reinforcing the loop. This dynamic extracts value from productive sectors
 *   of the economy, suppresses returns for savers, and inhibits the process
 *   of creative destruction.
 *
 * KEY AGENTS:
 *   - Savers and Pensioners: Primary victims (powerless/trapped) — forced to accept low returns and risk on their capital.
 *   - Productive New Entrants: Secondary victims (moderate/constrained) — starved of capital that is diverted to zombies.
 *   - Zombie Firm Executives: Primary beneficiaries (powerful/mobile) — receive cheap capital to continue operations, preserving their positions.
 *   - Central Bank Policymakers: Institutional beneficiaries (institutional/constrained) — achieve short-term stability mandates, even at the cost of long-term productivity.
 *   - Analytical Observer: Sees the full structure (analytical/analytical) — recognizes the system as a distorted coordination mechanism with severe extractive consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_misallocation_spiral, 0.65).
domain_priors:suppression_score(capital_misallocation_spiral, 0.75).
domain_priors:theater_ratio(capital_misallocation_spiral, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_misallocation_spiral, extractiveness, 0.65).
narrative_ontology:constraint_metric(capital_misallocation_spiral, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(capital_misallocation_spiral, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capital_misallocation_spiral, tangled_rope).
narrative_ontology:human_readable(capital_misallocation_spiral, "The Zombie Asset Loop").
narrative_ontology:topic_domain(capital_misallocation_spiral, "economic/technological").

domain_priors:requires_active_enforcement(capital_misallocation_spiral).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capital_misallocation_spiral, zombie_firm_executives).
narrative_ontology:constraint_beneficiary(capital_misallocation_spiral, incumbent_asset_managers).
narrative_ontology:constraint_beneficiary(capital_misallocation_spiral, central_bank_policymakers).
narrative_ontology:constraint_victim(capital_misallocation_spiral, savers_and_pensioners).
narrative_ontology:constraint_victim(capital_misallocation_spiral, productive_new_entrants).
narrative_ontology:constraint_victim(capital_misallocation_spiral, future_economic_growth).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAVER (SNARE) — Trapped in a low-yield environment, their capital is forced into unproductive assets, extracting value from their future with no recourse or alternative. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(capital_misallocation_spiral, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ZOMBIE FIRM EXECUTIVE (ROPE) — Experiences the low-rate environment as a pure coordination mechanism, providing the cheap capital necessary to continue operations and avoid default. The extractive nature of the loop is invisible from this position. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(capital_misallocation_spiral, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANK (SCAFFOLD) — Views the policy as a temporary support to prevent systemic collapse, believing it has an implicit sunset clause (i.e., it can be unwound once the economy recovers). The long-term extraction is seen as a necessary cost of short-term stability. d≈0.25, f(d)≈0.15, σ=1.0 → χ≈0.10.
constraint_indexing:constraint_classification(capital_misallocation_spiral, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PRODUCTIVE NEW ENTRANT (TANGLED ROPE) — Sees both the coordination function of capital markets and the severe extractive effect of being crowded out by zombies who absorb available capital. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(capital_misallocation_spiral, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes the dual nature of the constraint: a distorted resource allocation mechanism (coordination) that systematically transfers wealth from productive sectors and future generations to unproductive incumbents (extraction). This is the canonical classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(capital_misallocation_spiral, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_misallocation_spiral_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capital_misallocation_spiral, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_misallocation_spiral, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capital_misallocation_spiral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(capital_misallocation_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65): High. The constraint systematically transfers economic potential from savers and innovators to non-viable incumbents. This represents a significant loss of value. Suppression (0.75): High. The policy environment actively closes off alternatives. Savers cannot find safe, reasonable yields, and productive firms cannot compete with zombies for capital on a level playing field. Theater Ratio (0.40): Moderate. While the policies have a real function (preventing defaults), the public justifications often obscure the growing misallocation, becoming more performative over time as the negative consequences mount.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a saver, the system is a pure Snare, trapping their capital for extractive ends. For a zombie firm's CEO, it's a Rope, a lifeline of cheap credit that coordinates their survival. For the central banker who enacted the policy, it's a Scaffold, a temporary and necessary evil to prevent a worse crisis. The analytical observer, seeing all parts, classifies it as a Tangled Rope, acknowledging the (distorted) coordination function while focusing on the massive, asymmetric extraction it enables.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (zombie executives, policymakers) have low derived directionality (d), resulting in low or negative effective extraction (χ), hence they perceive a Rope or Scaffold. Victims (savers, new entrants) have high derived directionality, resulting in high χ, leading to a Snare or Tangled Rope classification. The powerless and trapped saver experiences the highest possible χ, while the constrained but moderate-power new entrant experiences a slightly lower, but still highly extractive, χ.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating how a policy can be simultaneously a life-saving support (Rope/Scaffold) and a predatory trap (Snare). The classification depends entirely on the agent's structural relationship to the flow of capital. The system avoids mislabeling the central bank's action as pure malice (Snare) or the saver's experience as a mere coordination problem (Rope). It captures the full, contradictory reality by indexing the classification to each observer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_intent_vs_effect,
    'Is the capital misallocation an unfortunate side effect of a necessary stability policy, or an intended (if unstated) feature to protect incumbent institutions?',
    'Econometric studies isolating policy effects from confounding factors; analysis of internal policy-making documents.',
    'If side effect: strengthens the Scaffold perspective. If intended: strengthens the Snare perspective for the entire system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_effect, empirical, 'Distinguishing between policy side effect and unstated intent').

omega_variable(
    schumpeterian_threshold,
    'At what point does the harm from suppressed creative destruction outweigh the benefit of preventing zombie firm defaults?',
    'Longitudinal studies comparing economic growth and innovation in sectors with high vs. low concentrations of zombie firms.',
    'A low threshold suggests the system is a highly extractive Snare; a high threshold suggests it functions more like a costly but potentially justifiable Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(schumpeterian_threshold, empirical, 'The threshold where preventing defaults causes more harm than good').

omega_variable(
    exit_feasibility,
    'Can central banks realistically unwind these policies without triggering a catastrophic financial crisis, or are they permanently trapped?',
    'Agent-based modeling of market reactions to policy normalization scenarios; historical analysis of past unwinding attempts.',
    'If exit is feasible: confirms the Scaffold perspective. If exit is impossible: the system is a permanent Snare, and the ''temporary'' justification is pure theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_feasibility, conceptual, 'Feasibility of unwinding the policies without systemic collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capital_misallocation_spiral, 2008, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capi_tr_t2008, capital_misallocation_spiral, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(capi_tr_t2018, capital_misallocation_spiral, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(capi_tr_t2028, capital_misallocation_spiral, theater_ratio, 2028, 0.4).

% Extraction over time
narrative_ontology:measurement(capi_be_t2008, capital_misallocation_spiral, base_extractiveness, 2008, 0.3).
narrative_ontology:measurement(capi_be_t2018, capital_misallocation_spiral, base_extractiveness, 2018, 0.5).
narrative_ontology:measurement(capi_be_t2028, capital_misallocation_spiral, base_extractiveness, 2028, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capital_misallocation_spiral, resource_allocation).
narrative_ontology:affects_constraint(capital_misallocation_spiral, technological_stagnation).
narrative_ontology:affects_constraint(capital_misallocation_spiral, sovereign_debt_crisis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
