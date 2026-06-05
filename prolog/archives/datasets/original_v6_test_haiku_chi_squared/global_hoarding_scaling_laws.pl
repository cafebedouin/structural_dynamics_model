% ============================================================================
% CONSTRAINT STORY: global_hoarding_scaling_laws
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_hoarding_scaling_laws, []).

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
 *   constraint_id: global_hoarding_scaling_laws
 *   human_readable: The Planetary Siphon: Global Hoarding Scaling Laws
 *   domain: economic/political
 *
 * SUMMARY:
 *   The planetary siphon represents the scaling of localized extraction
 *   mechanisms (predatory lending, wage suppression, commodity monopoly) into
 *   a globalized system where capital flows upward through nested hierarchies
 *   of institutions, regulatory arbitrage, and asymmetric information. Unlike
 *   feudalism (which was territorial and bounded) or industrial capitalism
 *   (which required localized labor), the planetary siphon operates through
 *   financial abstraction: money leaves resource-extraction sites as
 *   dividends and interest payments, with no reciprocal flow of reinvestment
 *   or maintenance. The constraint emerges from the 1980s onward as capital
 *   controls were dismantled, currency speculation became industrialized, and
 *   multinational debt obligations locked populations into extraction
 *   relationships. The mechanism is not a conspiracy but a structural
 *   incentive: wealth concentration is profitable for extractors, suppression
 *   costs are externalized (military/environmental damage borne by victims),
 *   and enforcement is increasingly automated (algorithms set lending rates,
 *   credit scores determine access, algorithmic trading captures arbitrage).
 *   The theater ratio (0.58) reflects that the ideological work—neoliberal
 *   economics, deservingness narratives, risk narratives—must continuously
 *   legitimize what is increasingly visible as extraction.
 *
 * KEY AGENTS:
 *   - Subsistence populations: Primary victims (powerless/trapped) — locked into resource scarcity; no exit from extraction networks
 *   - Resource-constrained nations: Secondary victims (moderate/constrained) — structural dependency on capital inflows; capital controls trigger punishment; debt servicing obligations perpetuate extraction
 *   - Concentrated wealth holders: Primary beneficiaries (institutional/arbitrage) — capture returns on global capital flows; mobility enables arbitrage extraction
 *   - Multinational extraction corporations: Hybrid actors (powerful/mobile) — simultaneously coordinate global supply chains and extract through commodity booms, regulatory arbitrage, currency manipulation
 *   - International monetary order: Institutional enforcer (institutional/arbitrage) — dollar hegemony, IMF conditionality, credit systems enforce extraction; increasingly seen as degraded (Piton properties)
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing extraction as inevitable economic law; false mountain framing obscures institutional contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_hoarding_scaling_laws, 0.68).
domain_priors:suppression_score(global_hoarding_scaling_laws, 0.72).
domain_priors:theater_ratio(global_hoarding_scaling_laws, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, extractiveness, 0.68).
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_hoarding_scaling_laws, snare).
narrative_ontology:human_readable(global_hoarding_scaling_laws, "The Planetary Siphon: Global Hoarding Scaling Laws").
narrative_ontology:topic_domain(global_hoarding_scaling_laws, "economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_hoarding_scaling_laws, concentrated_wealth_holders).
narrative_ontology:constraint_beneficiary(global_hoarding_scaling_laws, extractive_multinational_corporations).
narrative_ontology:constraint_beneficiary(global_hoarding_scaling_laws, rent_seeking_institutional_investors).
narrative_ontology:constraint_victim(global_hoarding_scaling_laws, subsistence_populations).
narrative_ontology:constraint_victim(global_hoarding_scaling_laws, resource_constrained_nations).
narrative_ontology:constraint_victim(global_hoarding_scaling_laws, future_generations).
narrative_ontology:constraint_victim(global_hoarding_scaling_laws, global_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE POPULATIONS (SNARE) — Locked into resource scarcity by wealth concentration and capital flight. No exit from systemic extraction. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.72. Trapped exit + victim status + global scope compounds effective extraction.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESOURCE-CONSTRAINED NATIONS (SNARE) — Structural dependency on global capital flows; capital controls trigger sanctions; debt obligations lock in extraction. d≈0.80, f(d)≈1.20, σ=1.0 → χ≈0.82. Constrained exit (capital flight penalties, IMF conditionality) + victim status confirms snare.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONCENTRATED WEALTH HOLDERS (ROPE) — Perceive the system as coordination: global capital mobility, arbitrage, and portfolio diversification enable efficient allocation. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.08. Beneficiary + arbitrage produces negative effective extraction (net benefit).
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL EXTRACTION CORPORATION (TANGLED ROPE) — Simultaneously benefits from global supply chains (coordination function: efficient resource networks) and extracts via commodity booms, currency manipulation, and regulatory arbitrage. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.29. Mobile exit + dual beneficiary/victim status creates hybrid classification.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL MONETARY ORDER (PITON) — Dollar-based reserve currency and multilateral debt enforcement mechanisms persist through institutional inertia despite structural obsolescence (blockchain alternatives, currency unions, capital controls emerging). theater_ratio=0.58 reflects that monetary theater (credit ratings, IMF reviews, currency valuations) performatively maintains hierarchy while underlying extraction mechanisms are degrading. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — Risk of naturalizing contingent extraction as inevitable: 'Wealth concentration follows Pareto distribution; capital flow is natural economics; resource scarcity is inherent to planetary limits.' These framings hide active extraction mechanisms. ε=0.68, suppression=0.72, theater=0.58 contradict accessibility_collapse≥0.85 gate. False summit: the constraint is institutional, not natural.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_hoarding_scaling_laws_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_hoarding_scaling_laws, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_hoarding_scaling_laws, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_hoarding_scaling_laws, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_hoarding_scaling_laws, TR),
    TR >= 0.70.

:- end_tests(global_hoarding_scaling_laws_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Capital concentration has accelerated from 1980–present (measurement trajectory from 0.32 → 0.68 reflects this). The extraction mechanism is not crude expropriation but structural: interest payments on $80+ trillion global debt, dividend extraction from resource sectors, currency speculation, and wage suppression in commodity-dependent economies. Suppression (0.72): Very high. Enforced through: debt obligation (defaulting triggers capital flight and currency collapse), regulatory capture (multinational corporations write trade agreements), and ideological suppression (economics curricula teach that inequality is natural, markets are self-correcting, capital mobility is efficiency). But suppression is not total—resistance is visible (debt cancellation movements, labor organizing, alternative currencies). Theater ratio (0.58): Moderate-high and rising. Much of the enforcement is theatrical: credit ratings that reflect political bias, GDP growth metrics that ignore ecological extraction, wealth-creation narratives that obscure accumulation as theft. The theater has increased (0.42 → 0.58) as actual enforcement mechanisms have become less visible (algorithms replace human exploiters; debt becomes abstract).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Concentrated wealth holders experience the system as benign coordination (Rope)—global capital mobility, portfolio diversification, and arbitrage appear as efficient allocation. Subsistence populations experience it as inescapable extraction (Snare)—every payment flows upward, every scarcity is deepened. Resource-constrained nations see a trap (Snare)—structural dependency on capital flows and debt servicing. Multinational corporations navigate it as hybrid (Tangled Rope)—they both organize supply chains and extract through commodity booms. The international monetary order appears to its operators as legitimate (Rope or technical scaffolding), but to its victims as degraded performance (Piton—the theater persists through institutional inertia, not functional justification). The civilizational observer risks seeing it as natural law (Mountain)—'inequality is inevitable, capital flows follow economic logic'—but the base properties (high ε, high suppression, theater involved) contradict the natural law signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Subsistence populations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction from this perspective. Resource-constrained nations: Victim + constrained → d≈0.80, f(d)≈1.20. Constrained exit (IMF conditionality, capital flight penalties) increases vulnerability. Concentrated wealth holders: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; system appears as benign coordination. Multinational corporations: Beneficiary + mobile → d≈0.35, f(d)≈0.35. Hybrid: mobile exit gives them power, but beneficiary status from extraction. International monetary order: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate (0.58), not high chi; monetary operators perceive legitimacy. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risk of naturalizing extraction as inevitable—false mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that snare classification is correct from the victim's structural position but that the beneficiary's rope experience is genuine—not a misclassification but a perspectival reality. The snare and rope are the same mechanism viewed from opposing positions. The mandatrophy emerges from asking 'Is this extraction or coordination?' The answer: it is both. For concentrated wealth holders and operators of global capital flows, the system coordinates efficient resource allocation—that's a genuine coordination function. For subsistence populations and resource-constrained nations, the same system is pure extraction—no coordination benefit, only drain. The snare classification is justified because the predominant structural position is victim (majority), not beneficiary (concentrated minority). The system's classification as Snare reflects that extraction dominates its function, with coordination benefits restricted to a narrow elite. The theater ratio (0.58) indicates that suppression increasingly requires ideological work (false narratives of deservingness, efficiency, inevitability) rather than force—a sign that the Snare's credibility is eroding and the system may transition toward Tangled Rope (where extraction must be openly negotiated) or revolution (where enforcement breaks entirely).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wealth_ceiling_mechanisms,
    'Is there a natural ceiling to wealth concentration (taxing, death, consumption limits, social instability) or does the extraction mechanism scale indefinitely as % of global GDP?',
    'Long-term historical analysis of wealth concentration across civilizations; empirical measure of whether hoarding ratio peaks or continues exponential growth; stability analysis of political systems under extreme inequality thresholds',
    'If ceiling exists: constraint becomes temporary (Scaffold with sunset). If indefinite scaling: constraint is pure extraction (Snare confirmed). If oscillatory (concentrate → revolution → reset): constraint becomes cyclical (Piton with periodicities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_ceiling_mechanisms, empirical, 'Whether wealth concentration has physical or political ceiling').

omega_variable(
    capital_escape_velocity,
    'Can capital concentration ever exceed the rate at which capital is required to maintain political control systems, creating a tipping point where extraction destabilizes its own enforcement?',
    'Structural analysis of capital requirements for military/police/judiciary across threshold inequality levels; historical precedent from late-stage empires (Rome, Ottoman, Soviet); simulation of redistribution pressure vs enforcement costs',
    'If escape velocity exists and is crossed: constraint spontaneously transitions to Tangled Rope (enforcement becomes negotiable). If enforcement can always scale: Snare remains stable indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_escape_velocity, empirical, 'Whether extraction enforcement can indefinitely scale with capital concentration').

omega_variable(
    alternative_coordination_substrate,
    'Do alternative coordination mechanisms (decentralized finance, local currencies, mutual aid networks, commons governance) provide genuine exit pathways from the planetary siphon or merely surface-level relief?',
    'Comparative analysis of community-based economies; measurement of wealth extraction rates in cooperative vs hierarchical systems; longitudinal tracking of alternative substrates under scaling pressure',
    'If genuine exit: constraint becomes Tangled Rope with visible Scaffold properties (alternatives scale). If surface relief: victims remain trapped; alternatives are performative (Piton properties embedded).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_substrate, empirical, 'Whether alternative economic substrates provide genuine exit from extraction').

omega_variable(
    enforcement_rationality_bounds,
    'Is suppression (0.72) sustainable if it requires continuous ideological work (neoliberal theory, deservingness narratives, scarcity mythology) whose credibility is eroding?',
    'Analysis of belief migration in economics curricula; survey data on trust in institutions and market ideology; correlation between suppression decay and resistance emergence',
    'If suppression credibility collapses: constraint transitions to Tangled Rope (active enforcement can no longer hide). If ideological renewal succeeds: suppression remains sticky.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_rationality_bounds, conceptual, 'Whether suppression mechanisms retain ideological credibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_hoarding_scaling_laws, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(siphon_tr_t0, global_hoarding_scaling_laws, theater_ratio, 0, 0.42).
narrative_ontology:measurement(siphon_tr_t15, global_hoarding_scaling_laws, theater_ratio, 15, 0.52).
narrative_ontology:measurement(siphon_tr_t30, global_hoarding_scaling_laws, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(siphon_be_t0, global_hoarding_scaling_laws, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(siphon_be_t15, global_hoarding_scaling_laws, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(siphon_be_t30, global_hoarding_scaling_laws, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_hoarding_scaling_laws, resource_allocation).
narrative_ontology:affects_constraint(global_hoarding_scaling_laws, commodity_superexploitation).
narrative_ontology:affects_constraint(global_hoarding_scaling_laws, sovereign_debt_trap).
narrative_ontology:affects_constraint(global_hoarding_scaling_laws, currency_colonialism).
narrative_ontology:affects_constraint(global_hoarding_scaling_laws, land_grabbing_enclosure).

% DUAL FORMULATION NOTE:
% The planetary siphon is the parent constraint under which commodity, debt, currency, and land extraction operate as specific instantiations. Each child constraint has its own ε value reflecting domain-specific extraction mechanisms; the parent siphon has ε=0.68 reflecting the scaled, automated nature of global extraction networks. Decomposition follows the principle that abstract extraction (capital flows, financial instruments) enables concrete extraction (resource seizure) at subsidiary levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_hoarding_scaling_laws, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
