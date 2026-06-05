% ============================================================================
% CONSTRAINT STORY: gale_shapley_variants
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gale_shapley_variants, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gale_shapley_variants
 *   human_readable: Gale-Shapley Algorithm for Market Clearing
 *   domain: economic/market_mechanisms
 *
 * SUMMARY:
 *   The Gale-Shapley algorithm represents a landmark coordination mechanism
 *   for resolving two-sided matching markets. Introduced in 1962, it
 *   guarantees the existence of stable matchings and provides a
 *   polynomial-time algorithm for computing them. The constraint it embodies
 *   is the requirement to stabilize matches in markets (college admissions,
 *   medical residency, kidney exchange, school choice) where bilateral
 *   negotiation would create inefficiencies and externalities. The GS
 *   algorithm itself is not extractive in the classical sense — it solves a
 *   genuine coordination problem that would otherwise remain unsolved.
 *   However, institutional context matters: the choice to optimize for
 *   stability rather than welfare, the decision to implement the
 *   proposer-optimal variant, and the enforcement of truthful preference
 *   revelation all introduce contingent choices that could generate
 *   extraction. The constraint exhibits rope characteristics from most
 *   perspectives, but temporal data shows a modest increase in theater ratio
 *   and extractiveness as institutional adoption matures, suggesting a slow
 *   degradation toward piton status in some contexts. The analytical
 *   perspective risks naturalizing the algorithm as a mathematical
 *   inevitability, when in fact the choice to use GS in any particular market
 *   is a policy decision with alternatives.
 *
 * KEY AGENTS:
 *   - Market Participants: Applicants, residents, transplant candidates, students — primary beneficiaries of stable matching; experience coordination benefit with minimal extraction if information access is symmetric
 *   - Market Clearing Institution: College admissions boards, NRMP, kidney procurement organizations — institutional beneficiary (arbitrage); benefits from reduced transaction costs and enhanced legitimacy
 *   - Strategic Players: Sophisticated participants with information advantages or coalition-formation capacity; limited extraction opportunity due to strategy-proofness constraints of the proposer-optimal variant
 *   - Constrained Participants: Those with information asymmetry, institutional barriers to truthful revelation, or vulnerability to preference manipulation; experience tangled_rope: benefits from stability but bears extraction through information disadvantage
 *   - Alternative Mechanism Coalition: Economists and mechanism designers advancing context-specific mechanisms (top trading cycles, probabilistic serial, kidney exchange with constraints); pushing transition beyond GS as optimality for their domain becomes clear
 *   - Legacy Institutional Operators: Mature matching markets (NRMP, medical school admissions in established countries) that maintain GS through inertia; experience piton-like dynamics with increasing theater as procedural overhead is required to enforce stability assumptions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gale_shapley_variants, 0.22).
domain_priors:suppression_score(gale_shapley_variants, 0.18).
domain_priors:theater_ratio(gale_shapley_variants, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gale_shapley_variants, extractiveness, 0.22).
narrative_ontology:constraint_metric(gale_shapley_variants, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(gale_shapley_variants, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gale_shapley_variants, rope).
narrative_ontology:human_readable(gale_shapley_variants, "Gale-Shapley Algorithm for Market Clearing").
narrative_ontology:topic_domain(gale_shapley_variants, "economic/market_mechanisms").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gale_shapley_variants, market_participants).
narrative_ontology:constraint_beneficiary(gale_shapley_variants, algorithm_adopters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL MARKET PARTICIPANT (ROPE) — An agent (applicant, resident, donor) in a GS-mediated market experiences the algorithm as pure coordination. They obtain a match that respects their preferences and stability properties. Exit options are mobile: they can propose alternative matching mechanisms or leave the market. No extraction occurs relative to the participant's legitimate interests. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(gale_shapley_variants, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: MARKET CLEARING INSTITUTION (ROPE) — A centralizing authority (college admissions board, residency matching service, organ procurement) adopts GS as the coordination mechanism. The institution benefits from reduced transaction costs, enhanced legitimacy through algorithm transparency, and elimination of bilateral negotiation overhead. Exit options are arbitrage: the institution can switch to alternative mechanisms (Boston mechanism, random serial dictatorship, centralized clearing without stability guarantees). The institution experiences the constraint as pure coordination with positive externalities. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.002. Net beneficiary.
constraint_indexing:constraint_classification(gale_shapley_variants, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: WELL-INFORMED STRATEGIC PLAYER (ROPE) — An agent with superior information about market structure, preference distributions, or algorithm implementation can extract limited advantage through strategic preference revelation (preference manipulation, coalition formation). However, the stability guarantee of GS severely constrains exploitation: any deviation that benefits a coalition must leave at least one member worse off, and the algorithm is designed to be strategy-proof in certain contexts (e.g., proposer-optimal variant). Exit options are mobile. The strategic player sees the mechanism as coordination with limited rent-seeking opportunity. d≈0.45, f(d)≈0.58, σ=1.0 → χ≈0.13.
constraint_indexing:constraint_classification(gale_shapley_variants, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTRAINED PARTICIPANT WITHOUT INFORMATION SYMMETRY (TANGLED ROPE) — A participant who cannot costlessly access preference revelation information, has limited ability to form credible coalition signals, or faces institutional barriers to truthful preference expression experiences the algorithm as mixed. The coordination mechanism works (they receive a stable match), but the stability guarantee only applies to true preferences. If they misreport due to information asymmetry or strategic uncertainty, they may be trapped in a suboptimal stable match. The algorithm requires active institutional enforcement (verification of true preferences, prevention of fraud, administration of appeals). Suppression arises from inability to costlessly access information about other participants' preferences. d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.23.
constraint_indexing:constraint_classification(gale_shapley_variants, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE MARKET DESIGN COALITION (SCAFFOLD) — Economists and mechanism designers (organized agents) view GS as a temporary coordination solution appropriate to specific market contexts (college admissions, residency matching) but not universally optimal. The coalition recognizes that GS is strategy-proof only for proposers, may suffer from preference falsification by responders, and does not maximize total welfare. Alternative mechanisms (top trading cycles, probabilistic serial, random serial dictatorship with redistribution) offer different trade-offs. GS is a scaffold: it solved the coordination crisis in 1960s-1980s markets (medical residency, college admissions) where no stable mechanism existed, but as mechanism design theory matures, the field is transitioning to context-specific optimizations. The sunset clause is real: as markets adopt specialized mechanisms (e.g., kidney exchange with edge constraints, school choice with diversity requirements), GS's dominance declines. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(gale_shapley_variants, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MATHEMATICAL EXISTENCE RESULT VIEW (MOUNTAIN) — From the civilizational/analytical perspective, the Gale-Shapley theorem is a natural law: for any two-sided market with transitive preferences, a stable matching exists and can be computed in polynomial time. This is not a policy choice; it is a mathematical fact independent of implementation details. The stability guarantee (no pair would rather match with each other) is a structural property, not an institutional design. The algorithm itself emerges naturally from the existence proof. However, this perspective risks naturalizing what is actually contingent: the choice to optimize for stability rather than welfare, the decision to use the proposer-optimal variant, the institutional enforcement of truthful preference revelation. The base metrics (ε=0.22, suppression=0.18) contradict a mountain classification — the engine will compute this as a false summit, indicating that the 'mathematical inevitability' framing conceals institutional choices.
constraint_indexing:constraint_classification(gale_shapley_variants, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: LEGACY MATCHING SYSTEM UNDER PROCEDURAL PRESSURE (PITON) — In mature matching markets (e.g., NRMP residency matching, medical school admissions in multiple countries), GS persists through institutional inertia and stakeholder consensus, not because it is functionally optimal. The algorithm is substantially performative: extensive institutional effort (appeals processes, preference counseling, outcome auditing) is required to maintain the assumption that preferences are truthfully revealed. Theater ratio is high (≈0.50) in these contexts: the 'mechanical' algorithm obscures the institutional labor required to make it work. As newer mechanisms prove superior in specific contexts (e.g., top trading cycles in school choice with diversity constraints), GS remains in place due to switching costs, familiarity, and stakeholder lock-in. d≈0.25, f(d)≈0.18, σ=1.0 → χ≈0.04. Theater_ratio ≈ 0.50 does not trigger the piton gate (requires ≥0.70), so this perspective classifies as rope-with-theatrical-elements rather than strict piton.
constraint_indexing:constraint_classification(gale_shapley_variants, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gale_shapley_variants_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gale_shapley_variants, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gale_shapley_variants, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gale_shapley_variants, TR),
    TR >= 0.70.

:- end_tests(gale_shapley_variants_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low to moderate. The algorithm itself solves a genuine coordination problem — matching without instability guarantees would leave participants worse off. However, extractiveness rises above zero due to: (1) the proposer-optimality of the standard variant (proposes-side participants receive superior matches on average), (2) institutional leverage in preference counseling and appeals, (3) information asymmetries that favor sophisticated players, (4) institutional choice to prioritize stability over welfare. The low base value reflects that the coordination benefit dominates, but extraction is not zero. Suppression (0.18): Moderate-low. Market participants face modest suppression: they must reveal preferences truthfully (or face suboptimal matches), institutional information requirements limit accessibility, and coalition formation is costly. However, suppression is not high because participants can typically exit to alternative markets or mechanism designs in most contexts. Theater ratio (0.35): Moderate. The algorithm is presented as 'mechanical' and inevitable, obscuring the institutional labor required to enforce truthful preference revelation, manage appeals, and maintain stability assumptions. As markets mature and theater ratio rises (0.15 → 0.35), the constraint approaches piton status.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects who benefits and who bears friction. Individual participants see pure coordination (rope), experiencing stable matching without awareness of the proposes-optimality asymmetry. Market institutions see beneficial coordination (rope with arbitrage), capturing reduced transaction costs. Strategic players see constrained coordination (still rope, but with limited extraction opportunity). Constrained participants see mixed coordination and suppression (tangled_rope) — they obtain stable matches but may be trapped in suboptimal outcomes due to information disadvantage or strategic uncertainty. The alternative mechanism coalition sees a temporary solution (scaffold) that is being optimized away in context-specific applications. Institutional operators in mature markets see degraded ritual (piton) — GS persists through inertia, not because it is optimal, but because switching costs are high and stakeholders are locked in. The mathematical perspective risks a false summit (mountain) — naturalizing the algorithm as inevitable when it is actually a policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual market participant: Beneficiary + mobile → d≈0.50, f(d)≈0.65. Symmetric: coordination benefit ≈ institutional constraint burden. Market clearing institution: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary — institution captures coordination externality. Strategic player: Beneficiary (through superior information) + mobile → d≈0.45, f(d)≈0.58. Moderate extraction opportunity, constrained by strategy-proofness. Constrained participant: Victim (through information disadvantage) + trapped → d≈0.70, f(d)≈1.05. Significant extraction despite stability guarantee, because suboptimal revelation traps them. Alternative mechanism coalition: Organized + constrained → d≈0.35, f(d)≈0.35. Low extraction; coalition sees sun-setting mechanism and is building exit paths. Legacy institutional operator: Institutional + constrained (cannot costlessly switch) → d≈0.25, f(d)≈0.18. Piton classification driven by theater gate, not high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Gale-Shapley algorithm resolves the mandatrophy by distinguishing coordination from pure extraction. The algorithm is fundamentally coordinate-achieving: it solves the market clearing problem that would otherwise be unsolved. The modest extraction (ε=0.22) arises not from the algorithm's core function but from contextual institutional choices (proposes-optimality variant, information asymmetries, institutional inertia). The constraint is rope-classified from institutional and analytical perspectives, with tangled_rope emerging from the perspective of constrained participants with information asymmetry, and scaffold from the perspective of the alternative mechanism coalition. No perspective sees pure snare, confirming that the algorithm is not exploitative at its core — it is coordination with institutional friction. The false summit danger (mountain classification from the civilizational/analytical view) is real: the algorithm's mathematical inevitability can naturalizes institutional choices that are actually contingent. The temporal degradation (theater_ratio 0.15 → 0.35) and rising extractiveness (0.12 → 0.22) suggest the constraint is slowly transitioning toward piton status in mature markets, as institutional labor required to maintain stability assumptions increases and context-specific alternatives prove superior in specialized contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategy_proofness_scope,
    'Is the algorithm strategy-proof only for proposers, or can responders also manipulate outcomes without detection?',
    'Empirical analysis of preference revelation patterns in mature markets (NRMP, medical school admissions); detection of systematic preference misreporting by responder-side participants; measurement of individual payoff variation under strategy-proof vs non-strategy-proof algorithms in the same market',
    'If responders can successfully manipulate: suppression rises to 0.35+, classification shifts toward snare or tangled_rope. If responder manipulation is negligible: rope classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategy_proofness_scope, empirical, 'Extent of strategy-proofness for responder-side participants').

omega_variable(
    welfare_stability_tradeoff,
    'Does the prioritization of stability over welfare maximize aggregate utility, or does it systematically disadvantage efficiency-enhancing matches?',
    'Comparison of GS stable matchings to welfare-optimal matchings in historical market data; measurement of efficiency loss (Kaldor-Hicks gains from alternative mechanisms); correlation between stability and individual regret across market sizes and preference distributions',
    'If stability sacrifices significant welfare: institutional choice becomes visible (not natural law), classification may shift toward tangled_rope with extractiveness rising to 0.35+. If welfare-stability alignment is tight: rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_stability_tradeoff, empirical, 'Empirical welfare-stability tradeoff in GS vs alternative mechanisms').

omega_variable(
    information_symmetry_requirement,
    'What fraction of market participants can access sufficient information about preference distributions, algorithm mechanics, and coalition opportunities to make truthful preference revelations?',
    'Surveys of participants post-matching (understanding of algorithm, information sources, strategic reasoning); correlation of information access with match outcomes; measurement of information asymmetry impact on preference falsification rates by demographic group',
    'If <60% can access adequate information: suppression rises to 0.45+, extractiveness rises to 0.35+, classification shifts toward snare or tangled_rope. If >80% have access: rope classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_symmetry_requirement, empirical, 'Proportion of participants with adequate information for truthful preference revelation').

omega_variable(
    context_specificity_degradation,
    'How many distinct market contexts (college admissions, medical residency, school choice, kidney exchange, etc.) now use GS vs context-optimized alternatives?',
    'Inventory of major two-sided matching markets globally; measurement of GS adoption rates over time in each context; tracking of migrations to alternative mechanisms (top trading cycles, probabilistic serial, constrained mechanisms); documentation of mechanism choice justifications',
    'If GS dominance is declining sharply (< 40% of major markets by 2030): scaffold perspective confirmed, sunset is empirically observable, extraction falls as alternatives mature. If GS remains dominant (> 70%): rope or piton classification suggests institutional stickiness rather than optimality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_specificity_degradation, empirical, 'Trend in GS adoption vs context-specific alternatives across market types').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gale_shapley_variants, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gsmc_tr_t0, gale_shapley_variants, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gsmc_tr_t10, gale_shapley_variants, theater_ratio, 10, 0.28).
narrative_ontology:measurement(gsmc_tr_t20, gale_shapley_variants, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(gsmc_be_t0, gale_shapley_variants, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(gsmc_be_t10, gale_shapley_variants, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(gsmc_be_t20, gale_shapley_variants, base_extractiveness, 20, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gale_shapley_variants, resource_allocation).
narrative_ontology:affects_constraint(gale_shapley_variants, preference_aggregation_stability).
narrative_ontology:affects_constraint(gale_shapley_variants, two_sided_market_equilibrium).

% DUAL FORMULATION NOTE:
% The Gale-Shapley algorithm is part of a constraint family covering two-sided market clearing mechanisms. The upstream constraint is the mathematical existence of stable matchings (Mountain: ε≈0.05); GS is the algorithmic realization with institutional implementation choices (Rope: ε≈0.22); downstream constraints include context-specific variants (kidney exchange with edge constraints, school choice with diversity requirements, probabilistic mechanisms for fairness). Each story in the family has its own ε reflecting the institutional choices embedded in the mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gale_shapley_variants, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
