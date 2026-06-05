% ============================================================================
% CONSTRAINT STORY: jevons_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jevons_paradox, []).

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
 *   constraint_id: jevons_paradox
 *   human_readable: Jevons Paradox: The Rebound Effect in Resource Consumption
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Jevons Paradox describes a fundamental tension in technological solutions
 *   to resource scarcity: efficiency improvements that reduce the effective
 *   price of a resource induce rational economic actors to increase
 *   consumption, partially or fully offsetting the intended conservation
 *   goal. Named after 19th-century economist William Stanley Jevons, who
 *   observed that coal efficiency improvements in Victorian England led to
 *   increased coal consumption rather than decreased coal use. The constraint
 *   operates at multiple scales — household, industrial, sectoral, and global
 *   — with different classifications from each perspective. The deep
 *   structure reveals a false summit: the paradox is often framed as an
 *   immutable economic law (higher prices drive lower consumption, lower
 *   prices drive higher consumption), but this 'law' depends entirely on the
 *   assumption that the externality (resource depletion, climate impact) is
 *   not priced into the resource cost. When the externality is externalized —
 *   not incorporated into the market price — rational actors have no economic
 *   signal to constrain their consumption, and efficiency improvements simply
 *   accelerate resource depletion. The constraint is thus not a law of nature
 *   but a policy failure: the failure to price the true cost of resource
 *   extraction.
 *
 * KEY AGENTS:
 *   - Resource Extractors & Fossil Fuel Industries: Primary beneficiaries (institutional/arbitrage) — efficiency improvements expand markets and accelerate resource monetization
 *   - Energy-Intensive Industries: Secondary beneficiaries (organized/constrained) — benefit from lower energy costs but locked into carbon-intensive production pathways
 *   - Consumer Households: Mixed position (moderate/constrained) — gain access to cheaper services but locked into consumption pathways that increase total resource use
 *   - The Carbon Cycle: Primary victim (powerless/trapped) — cannot exit the accumulation; each efficiency-driven consumption increase directly increases atmospheric loading
 *   - Ecological Systems & Future Generations: Secondary victim (powerless/trapped) — bear costs of accelerated resource depletion and climate impacts from consumption rebound
 *   - Climate Policy Apparatus: Institutional actor (institutional/arbitrage) — maintains efficiency-focused policies despite contradicting empirical evidence; theater increases as policy failures accumulate
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent institutional failure (non-pricing of externality) as an immutable law of economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jevons_paradox, 0.38).
domain_priors:suppression_score(jevons_paradox, 0.42).
domain_priors:theater_ratio(jevons_paradox, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jevons_paradox, extractiveness, 0.38).
narrative_ontology:constraint_metric(jevons_paradox, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jevons_paradox, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jevons_paradox, tangled_rope).
narrative_ontology:human_readable(jevons_paradox, "Jevons Paradox: The Rebound Effect in Resource Consumption").
narrative_ontology:topic_domain(jevons_paradox, "economic/technological").

domain_priors:requires_active_enforcement(jevons_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jevons_paradox, resource_extractors).
narrative_ontology:constraint_beneficiary(jevons_paradox, energy_intensive_industries).
narrative_ontology:constraint_beneficiary(jevons_paradox, fossil_fuel_infrastructure).
narrative_ontology:constraint_victim(jevons_paradox, atmospheric_carbon_budget).
narrative_ontology:constraint_victim(jevons_paradox, ecological_systems).
narrative_ontology:constraint_victim(jevons_paradox, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CARBON BUDGET (SNARE) — The atmospheric carbon system cannot exit the accumulation cycle. Each efficiency improvement that increases consumption directly increases extraction from the carbon sink. No alternative pathways; no negotiation capacity; full structural victimization. The constraint operates at civilizational scale with no agent representation.
constraint_indexing:constraint_classification(jevons_paradox, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ENERGY INDUSTRY (ROPE) — Institutional beneficiary. Efficiency improvements expand their market through lower effective prices and increased demand. The constraint is experienced as pure coordination: consumers demand more energy at lower prices, and the industry satisfies this demand profitably. No extraction perceived — the mechanism aligns incentives perfectly. The industry has maximal arbitrage options: they can choose energy sources, technologies, and markets.
constraint_indexing:constraint_classification(jevons_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CONSUMER HOUSEHOLD (TANGLED ROPE) — Moderate agent with constrained options. Efficiency improvements lower appliance costs and operating expenses, creating genuine coordination benefits (affordable comfort, mobility, productivity). But consumption increase locks households into carbon-intensive infrastructure pathways (vehicle ownership, home electrification dependent on grid mix, energy-intensive services). The constraint has both genuine coordination (lower costs enable access) and asymmetric extraction (lock-in to fossil infrastructure). Exit options are constrained by infrastructure availability and upfront capital requirements.
constraint_indexing:constraint_classification(jevons_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INDUSTRIAL MANUFACTURERS (TANGLED ROPE) — Organized agents that benefit from lower energy costs and expanded production capacity (coordination), but also face lock-in to energy-intensive production processes (extraction). Exit to low-carbon manufacturing requires major capital restructuring with significant switching costs. The constraint coordinates expansion of productive capacity while extracting a transition cost from those who attempt decarbonization.
constraint_indexing:constraint_classification(jevons_paradox, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE POLICY APPARATUS (PITON) — Institutional actor with high theater ratio. Climate policies often assume decoupling: efficiency improvements will reduce total energy/emissions. The Jevons paradox demonstrates that decoupling at the sectoral level does not guarantee absolute decoupling at the system level. Carbon pricing, efficiency standards, and renewable mandates persist despite contradicting empirical evidence that they produce the assumed outcomes. The policy machinery is maintained through institutional inertia and performance theater rather than functional effectiveness. Theater increases as policy failures accumulate but policies remain intact.
constraint_indexing:constraint_classification(jevons_paradox, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, Jevons paradox is presented as an economic law: rational actors respond to price signals by increasing consumption of cheaper goods. The constraint is described as inherent to market economics itself — immutable, inevitable, beyond policy intervention. However, this perspective naturalizes what is actually a contingent institutional arrangement: the externality (atmospheric carbon cost) is not priced into energy, so rational actors have no economic signal to internalize the true cost of consumption. The 'law' is in fact a policy failure masquerading as natural law.
constraint_indexing:constraint_classification(jevons_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jevons_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jevons_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jevons_paradox, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(jevons_paradox, TR),
    TR >= 0.70.

:- end_tests(jevons_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits genuine tension between efficiency (coordination benefit) and consumption increase (extraction from carbon budget). The initial efficiency improvement is real — lower cost genuinely benefits consumers and enables expanded access. But the consumption rebound transfers this benefit upstream to resource extractors and locks downstream actors into high-consumption pathways. The extractiveness increases over time (0.15 → 0.38) as cumulative consumption rebound reveals that efficiency without demand reduction is not decarbonization. Suppression (0.42): Moderate-high. Multiple barriers prevent exit from the rebound cycle: (1) Market structure: actors face genuine price signals for the efficiency good but no price signal for the externality (carbon/resource depletion). (2) Behavioral factors: the consumption increase feels voluntary (rational response to lower prices) even though the underlying structure removes alternatives (there is no pathway to consumption increase that does not increase emissions, because the externality is not priced). (3) Policy failure: even when the mechanism is understood, policy responses remain weak because pricing the externality creates political resistance and carbon leakage. Theater ratio (0.55): Moderate-high. Significant performative content exists in both directions. Efficiency improvement narratives (LED lightbulbs, electric vehicles, renewable energy) are marketed as decarbonization solutions despite accumulated evidence that they do not reduce total emissions without demand reduction or absolute caps. Climate policy maintains efficiency mandates as primary mechanisms despite their empirical failure to decouple energy use from growth. The theater increases over time as the gap between policy promises and outcomes widens (0.35 → 0.55).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radically different classifications from different structural positions. The energy industry sees rope (coordination) — efficiency creates demand, demand drives their business, everyone benefits. The consumer household sees tangled rope (mixed benefit and lock-in) — they gain lower costs but become locked into high-consumption infrastructure. Industrial manufacturers see tangled rope (coordination benefit with decarbonization lock-in costs) — they benefit from cheaper energy but face expensive transition to low-carbon manufacturing. The carbon cycle and future generations see snare (pure extraction, no exit) — efficiency-driven consumption increases atmospheric loading with no consent and no compensation. The climate policy apparatus sees piton (performative ritual) — efficiency policies persist despite failing to achieve stated goals, maintained through institutional inertia. The analytical observer risks seeing mountain (natural law) — 'the paradox' presented as inevitable economic truth — but the structural data reveals this as false summit: the 'law' is the institutional failure to price externalities. The perspectival gap reveals that the constraint's classification depends entirely on whether the observer's position includes or excludes the externality from their decision calculus.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's structure produces asymmetric directionality across beneficiaries and victims. Resource extractors experience very low or negative effective extraction (d ≈ 0.10, f(d) ≈ -0.08) — they are pure beneficiaries; efficiency improvements expand their markets. Consumers experience moderate extraction (d ≈ 0.55, f(d) ≈ 0.75) — they receive coordination benefits (lower costs) but experience lock-in (increased consumption commitment). The carbon cycle and future generations experience maximum extraction (d ≈ 1.0, f(d) ≈ 1.42) — they have no optionality, no compensation, no exit. The policy apparatus experiences moderate extraction (d ≈ 0.60) because it benefits from the appearance of action (efficiency policies) while bearing the reputational cost of failing to decarbonize. The directionality values reflect structural asymmetry: those who benefit from efficiency improvements face immediate gains and low lock-in costs; those who bear the extraction costs face diffuse, delayed, and cumulative harms.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing between local and system-level effects. At the local level (household, firm), efficiency improvements are genuine coordination gains — lower costs, same service, reduced local resource consumption. At the system level (global emissions, absolute resource depletion), efficiency improvements without demand reduction produce rebound effects that offset the local gains. The constraint contains genuine coordination (efficiency reduces local resource intensity) AND genuine extraction (the consumption rebound increases total resource extraction and locks agents into high-consumption pathways). The mandatrophy dissolves when the scope dimension is made explicit: local scope produces rope (pure coordination), global scope produces snare (extraction from the atmospheric carbon sink). The tangled rope classification at regional/national scope captures the intermediate case where local efficiency gains are offset by partial consumption rebound.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rebound_magnitude_threshold,
    'What is the true magnitude of rebound effects across energy sectors? Does the rebound effect fully offset efficiency gains (100% rebound), partially offset (30-70%), or leave net savings (less than 30%)?',
    'Longitudinal analysis of energy consumption data post-efficiency improvement; sector-specific accounting (direct rebound from increased use of the efficient good, indirect rebound from spending savings elsewhere, economy-wide rebound from GDP effects); controlled comparison of efficiency improvements with and without corresponding price reductions',
    'If 100% rebound: Jevons paradox is a complete constraint rendering efficiency improvements ineffective for absolute emissions reduction. If <50% rebound: efficiency improvements combined with carbon pricing or absolute caps can drive decarbonization. If >70% but <100%: mixed strategy required (efficiency + demand reduction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebound_magnitude_threshold, empirical, 'Magnitude of rebound effect across energy sectors').

omega_variable(
    substitution_vs_income_effect,
    'Do rebound effects primarily result from substitution (consuming more of a good because it is now cheaper relative to other goods) or income effects (increased real income from lower energy costs enabling consumption of other goods)?',
    'Econometric decomposition of consumption changes post-efficiency improvement; structural equation modeling isolating substitution elasticities from income effects; cross-sectional analysis of households with identical efficiency improvements but different income changes',
    'If primarily substitution: policy can dampen rebound by maintaining price through carbon taxation. If primarily income: rebound effects are harder to constrain via price policy alone; absolute supply constraints may be required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_income_effect, empirical, 'Whether rebound results from substitution or income effects').

omega_variable(
    decoupling_observability_ambiguity,
    'Is decoupling (relative decoupling: emissions per unit GDP declining) observable in data, or is it methodological artifact from accounting for traded emissions?',
    'Lifecycle accounting of embodied carbon in traded goods; consumption-based carbon footprint (what nations actually consume) vs production-based (what they produce); inclusion of supply-chain outsourcing in carbon attribution',
    'If decoupling is real: efficiency improvements can work alongside demand reduction. If decoupling is accounting artifact: high-income nations have merely outsourced extraction, not reduced it; the paradox holds at the consumption level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoupling_observability_ambiguity, empirical, 'Whether decoupling is real or accounting artifact').

omega_variable(
    exogeneity_of_resource_supply,
    'Does the rebound effect assume that resource supply is perfectly elastic (infinite supply at the market price), or do absolute resource constraints impose a natural ceiling on rebound magnitude?',
    'EROI (Energy Return on Energy Invested) decline in fossil fuels and renewables; thermodynamic limits on energy density; geological constraints on resource extraction rates; price elasticity of resource supply at the margin',
    'If supply is elastic: Jevons paradox holds indefinitely; efficiency improvements simply accelerate resource depletion. If supply is inelastic: rebound effects reach a ceiling as resource scarcity raises prices; the constraint transitions from price-driven to supply-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exogeneity_of_resource_supply, empirical, 'Whether resource supply is elastic or constrained').

omega_variable(
    policy_externality_internalization,
    'Can policy mechanisms (carbon pricing, quantity caps, Pigouvian taxation) successfully internalize the externality and eliminate the rebound effect, or do behavioral/institutional barriers prevent full price transmission?',
    'Comparison of emissions outcomes under different policy regimes (carbon tax vs cap-and-trade vs efficiency standards); analysis of behavioral responses to carbon prices vs equivalent explicit carbon costs; institutional tracking of policy slippage (exceptions, offsets, leakage)',
    'If internalization succeeds: the constraint is policy-resolvable; Jevons paradox is a failure of policy design, not economic law. If institutional barriers prevent internalization: the constraint is structural; even well-designed policy will be undermined by evasion and behavioral response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_externality_internalization, empirical, 'Whether policy can successfully internalize carbon externality').

omega_variable(
    false_summit_natural_law_framing,
    'Is Jevons Paradox genuinely a natural law of market economics, or is it a contingent institutional arrangement that depends on specific policy choices (externality non-pricing)?',
    'Counterfactual: in a regulatory regime where carbon is fully priced and consumption decisions face true marginal cost, would rational actors increase consumption as predicted? Historical comparison of demand elasticity under different pricing regimes.',
    'If natural law: decarbonization requires demand reduction or absolute caps, not efficiency. If contingent: decarbonization is achievable through policy reform (carbon pricing) plus efficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_framing, conceptual, 'Whether Jevons Paradox is natural law or policy-contingent constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jevons_paradox, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jevons_tr_t0, jevons_paradox, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jevons_tr_t50, jevons_paradox, theater_ratio, 50, 0.48).
narrative_ontology:measurement(jevons_tr_t100, jevons_paradox, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(jevons_be_t0, jevons_paradox, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(jevons_be_t50, jevons_paradox, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(jevons_be_t100, jevons_paradox, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jevons_su_t0, jevons_paradox, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(jevons_su_t50, jevons_paradox, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(jevons_su_t100, jevons_paradox, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jevons_paradox, resource_allocation).
narrative_ontology:affects_constraint(jevons_paradox, carbon_pricing_mechanism).
narrative_ontology:affects_constraint(jevons_paradox, demand_reduction_necessity).
narrative_ontology:affects_constraint(jevons_paradox, rebound_effect_behavioral).
narrative_ontology:affects_constraint(jevons_paradox, absolute_cap_constraint).

% DUAL FORMULATION NOTE:
% Jevons Paradox can be decomposed into three structurally distinct constraints: (1) local_efficiency_gain (ε=0.10, Rope) — efficiency improvements genuinely reduce resource intensity at the point of use. (2) consumption_rebound_mechanism (ε=0.55, Tangled Rope) — the induced consumption increase locks agents into high-consumption pathways. (3) absolute_extraction_accumulation (ε=0.72, Snare) — the net effect on total resource extraction and atmospheric carbon is determined by rebound magnitude. Each has different empirical status and different policy implications. This story presents the integrated constraint; the decomposition is linked via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jevons_paradox, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
