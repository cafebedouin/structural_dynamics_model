% ============================================================================
% CONSTRAINT STORY: intertemporal_responsibility_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intertemporal_responsibility_gap, []).

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
 *   constraint_id: intertemporal_responsibility_gap
 *   human_readable: The Generational Accountability Void
 *   domain: environmental/economic/technological
 *
 * SUMMARY:
 *   The intertemporal responsibility gap is a structural constraint in which
 *   current actors systematically capture benefits (cheap energy, rapid
 *   growth, debt-financed consumption, accelerated technological development)
 *   while deferring costs (climate damage, ecological collapse, financial
 *   crises, AI-driven disruption) to future generations and non-human
 *   systems. This constraint exhibits the properties of a snare from the
 *   perspective of future generations and the biosphere — they cannot exit,
 *   negotiate, or consent to the decisions that bind them. From the
 *   perspective of current institutional leadership, the constraint appears
 *   as coordination (rope), enabling optimal strategies within their time
 *   horizon. From organized advocacy coalitions, it appears as temporary
 *   (scaffold) — alternatives (renewable transition, circular economy,
 *   intergenerational justice frameworks) are emerging. The constraint's
 *   theater ratio is elevated (0.65) because much of the apparent policy
 *   response (climate pledges, ESG commitments, net-zero targets) is
 *   performative — accounting frameworks remain invisible to future costs,
 *   discount rates continue to suppress intergenerational weight, and
 *   enforcement mechanisms are weak. The extractiveness has been rising over
 *   the measurement interval (0.42 → 0.68) as carbon budgets tighten, climate
 *   impacts accelerate, and the deferral strategy becomes increasingly
 *   visible. The suppression (0.72) is high because structural barriers
 *   prevent exit: future generations have no voice in present decisions,
 *   ecosystem commons cannot organize, and institutional mechanisms (fiscal
 *   policy, monetary policy, technology development priorities) lock in
 *   extraction through inertia and path dependence.
 *
 * KEY AGENTS:
 *   - Current Extractive Industries (energy, agriculture, mining): Primary beneficiary (institutional/arbitrage) — capture present economic rent from resource depletion and carbon loading
 *   - Current Political-Economic Leadership: Secondary beneficiary (institutional/immediate) — gain from low-cost growth and deferred fiscal burden; have arbitrage options (refinance, relocate)
 *   - Future Generations (2070-2150): Primary victim (powerless/trapped) — inherit accumulated costs without consent; cannot exit or negotiate
 *   - Ecosystem Commons (biosphere, soil, water, biodiversity): Primary victim (powerless/trapped) — absorbs carbon, toxins, and habitat loss; has no recourse mechanism
 *   - Current Working-Age Population (modest income): Mixed actor (moderate/constrained) — benefits from low-cost consumption; bears some taxes and environmental exposure; cannot fully exit
 *   - Wealthy Capital-Mobile Agents: Arbitrage actor (powerful/arbitrage) — benefit from extraction; have geographic and financial exit options; exposure to systemic risk is hedged
 *   - Climate and Systemic Risk Advocacy Coalitions: Organized counter-actor (organized/constrained) — see constraint as temporary; building alternative pathways with sunset logic
 *   - International Financial and Accounting System: Institutional maintainer (institutional/arbitrage) — preserves constraint through invisibility of future costs; performative metrics; inertial dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intertemporal_responsibility_gap, 0.68).
domain_priors:suppression_score(intertemporal_responsibility_gap, 0.72).
domain_priors:theater_ratio(intertemporal_responsibility_gap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, extractiveness, 0.68).
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intertemporal_responsibility_gap, snare).
narrative_ontology:human_readable(intertemporal_responsibility_gap, "The Generational Accountability Void").
narrative_ontology:topic_domain(intertemporal_responsibility_gap, "environmental/economic/technological").

domain_priors:requires_active_enforcement(intertemporal_responsibility_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intertemporal_responsibility_gap, current_extractive_industries).
narrative_ontology:constraint_beneficiary(intertemporal_responsibility_gap, current_fiscal_beneficiaries).
narrative_ontology:constraint_beneficiary(intertemporal_responsibility_gap, current_political_leadership).
narrative_ontology:constraint_victim(intertemporal_responsibility_gap, future_generations).
narrative_ontology:constraint_victim(intertemporal_responsibility_gap, ecosystem_commons).
narrative_ontology:constraint_victim(intertemporal_responsibility_gap, intergenerational_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Structurally cannot exit or negotiate. Inherits accumulated climate/debt/AI-risk costs without choice or consent. Bears extraction maximum: constraints are locked in by institutional inertia and sunk choices. No voice in the decisions that bind them. Maximum d ~ 0.95; maximum experienced extraction.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ECOSYSTEM COMMONS (SNARE) — Cannot advocate, organize, or exit. Bears full cost of carbon loading, biodiversity loss, soil depletion, and cascade failures. Extraction is structural: the commons has no recourse mechanism. The constraint treats the biosphere as a cost-absorption buffer. No negotiating power; no alternatives.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CURRENT LEADERSHIP (ROPE) — Experiences the constraint as coordination: extracting now and deferring costs is the optimal strategy within their time horizon. They have arbitrage options (refinance debt, move industries, shift risk to insurance markets). From their perspective, the intertemporal gap is a feature, not a bug — it enables present consumption at deferred cost. Effective d ~ 0.10; experiences negative extraction (subsidy).
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CURRENT WORKERS / MODEST INCOME (TANGLED ROPE) — Partly benefits from low-cost consumption and debt-financed public goods; partly bears costs through taxes and environmental exposure. Cannot exit the constraint (embedded in employment, housing markets, infrastructure) but also not entirely trapped. Mixed extraction: some policies (carbon tax) hit harder; others (subsidized energy) benefit them. Effective d ~ 0.55-0.65. Time horizon is biographical; they will see some consequences but not the worst.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CAPITAL-MOBILE AGENTS (TANGLED ROPE) — Benefit from extraction (ownership of extractive assets, capacity to arbitrage into emerging green tech, geographic mobility). Also nominally exposed to climate/systemic risks. But arbitrage options are extensive: capital flight, climate migration, asset diversification across risk geographies. They have exit ramps the powerless lack. Experience mixed extraction — benefit from the asymmetry but face some systemic tail risks. Effective d ~ 0.35-0.45.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED ADVOCACY COALITIONS (SCAFFOLD) — NGOs, activist networks, forward-looking governments recognize the constraint as a temporary coordination failure with a potential sunset. Carbon pricing, climate accords, debt restructuring, AI safety research are emerging alternative pathways. These coalitions have real agency and see exit ramps (renewable energy transition, circular economy, intergenerational justice frameworks). Effective extraction is moderate because the coalition has visible alternatives and growing political capacity. Theater is present (COP pledges without enforcement) but declining as alternatives mature. Effective d ~ 0.50-0.60.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL FINANCIAL APPARATUS (PITON) — Maintains the constraint through performative accounting: GDP counts carbon extraction as income (not cost), sovereign debt is counted as 'growth,' externalities are invisible in national accounts. The system's primary function (allocating capital) has become secondary to its performative function (deferring costs off-books). Theater ratio is high (quarterly earnings, sovereign ratings, ESG scores) because the underlying accounting framework treats future costs as non-existent. System persists through institutional inertia — alternatives (genuine intergenerational accounting, ecosystem service valuation, dynamic debt sustainability) exist but are not integrated. Effective d ~ 0.15.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Temptation to naturalize the constraint as an inherent feature of economics (time preference, discount rates, opportunity cost). This perspective risks treating a contingent institutional choice (discounting future welfare at present rates) as a law of nature. The engine will flag this as a false summit: discounting is a policy parameter, not an immutable law. Humanity could in principle choose zero or negative discount rates for intergenerational decisions; that we do not reflects power and preference, not physics.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intertemporal_responsibility_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intertemporal_responsibility_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intertemporal_responsibility_gap, TR),
    TR >= 0.70.

:- end_tests(intertemporal_responsibility_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from future generations and the biosphere at a rate that exceeds any compensating benefit (technology, adaptation capacity, or intergenerational transfers). Current actors capture ~80-90% of present growth gains while deferring 60-70% of climate, ecological, and fiscal costs to the future. The measurement trajectory (0.42 → 0.68 over 50 years) reflects acceleration: early-period extraction was plausible under uncertainty and low impact; current extraction is increasingly visible as climate damage and debt service consume larger shares of future output. Suppression (0.72): High. Future generations cannot advocate, exit, or renegotiate. Institutional mechanisms (democratic politics, markets, corporate governance) systematically exclude future agents. Structural barriers include: (a) temporal asymmetry in decision power (present votes determine future constraints); (b) irreversibility in physical systems (carbon, species, soil) that lock in damage; (c) discounting frameworks that suppress intergenerational weight in cost-benefit analysis; (d) path dependence in infrastructure and institutions that self-reinforce extraction. Theater ratio (0.65): Moderate-high. Climate pledges (net-zero 2050, 1.5C commitments), ESG metrics, and carbon pricing are partially performative — they create the appearance of accountability without enforcement mechanisms, accounting adjustments, or binding constraints. ESG scoring has decoupled from actual emissions; climate pledges lack enforcement; carbon markets enable offsets rather than reductions. However, the theater is declining (scaffold perspective shows real alternatives emerging: renewable infrastructure locks in physical change; intergenerational justice frameworks are gaining normative weight). The theater ratio rising from 0.48 to 0.65 reflects expansion of symbolic response that hasn't yet translated to systemic change.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence — the snare and rope perspectives are nearly orthogonal. Future generations and the biosphere see pure extraction with no exit (snare: d ≈ 0.95, χ ≈ 1.35 at global scope). Current leadership sees coordination that solves their optimization problem (rope: d ≈ 0.10, χ ≈ -0.10). The working-age population sees mixed extraction proportional to their income and mobility (tangled_rope: d ≈ 0.60, χ ≈ 0.65). The wealthy see low effective extraction due to arbitrage options (tangled_rope: d ≈ 0.35, χ ≈ 0.35-0.40). Advocacy coalitions see a temporary coordination failure with visible exits (scaffold: d ≈ 0.55, χ ≈ 0.25-0.35). The financial system sees its own role as degraded but necessary (piton: d ≈ 0.15, theater ≈ 0.70). The analytical observer risks naturalizing the constraint as an inherent property of rational economics (false mountain). This perspectival range is the signature of an extractive constraint: the beneficiaries experience it as smooth coordination, while the victims experience it as structural imprisonment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps each agent's structural position relative to the extraction flow. Future generations have d ≈ 0.95 (full target) because they bear costs with zero decision power and no exit option. The biosphere has d ≈ 0.98 (full target) because it is a cost-absorption sink with no agency. Current leadership has d ≈ 0.08 because they are institutional beneficiaries with arbitrage options (refinance debt, move industries, climate migration). Working-age moderate-income agents have d ≈ 0.62 because they are partially extracted from (carbon taxes, environmental exposure) but also partly benefited (cheap energy, public goods funded by deficit spending). Wealthy capital-mobile agents have d ≈ 0.38 because they benefit from extraction but have hedged exposure to systemic risk. Advocacy coalitions have d ≈ 0.56 because they see the constraint as a problem but also have real agency (policy levers, technology development, narrative shifts). The piton perspective has d ≈ 0.18 because the financial system is a nominal beneficiary but increasingly aware of its own degradation. These d values feed the sigmoid f(d) to produce experienced extraction chi; the wide range of d values (0.08 to 0.98) produces a correspondingly wide range of experienced classifications (rope to snare).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε = 0.68 > 0.70 threshold): The constraint resolves the mandatrophy by showing that high extractiveness can coexist with partial coordination function. The rope perspective (current leadership) is genuine — the constraint does solve a coordination problem for present actors: how to achieve high growth while deferring costs. The snare perspective (future generations) is equally genuine — the constraint is extraction without consent or exit. The mandatrophy is resolved by recognizing that coordination from one temporal perspective can be extraction from another. Current actors are coordinating intratemporal consumption (solving prisoner's dilemmas in the present). Future agents inherit extraction (constraints locked in by sunk choices). The constraint is simultaneously Rope (present coordination) and Snare (future extraction). The analytical observer's false mountain (discounting-is-inevitable) is the key mandatrophy trap: if discounting is treated as a natural law, then the extraction appears to be a rational optimization problem rather than a structural choice. The framework uncovers this by showing that all eight perspectives are real, contradicting the mountain classification. The constraint is NOT a law of nature; it is an institutional arrangement that can be reformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discount_rate_legitimacy,
    'Is positive time discounting of future human welfare a rational constraint or a naturalized institutional choice that serves current interests?',
    'Philosophical and empirical analysis: (a) Intergenerational ethics frameworks (Rawls, Sen, sustainability principles); (b) Historical variation in discount rates and their correlation with power distribution; (c) Asymmetry in how discounting is applied (environmental costs vs financial assets).',
    'If discounting is rational: the constraint reflects unavoidable economic limits, and the snare classification weakens to tangled_rope across all perspectives. If discounting is institutional choice: the constraint is a pure extraction mechanism, and snare is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discount_rate_legitimacy, conceptual, 'Whether positive discounting is inherent rationality or institutional choice').

omega_variable(
    irreversibility_threshold,
    'At what point do deferred climate/ecological costs become irreversible, converting the snare into a mandatory reckoning?',
    'Empirical: tipping point analysis (permafrost melt, ocean acidification, Amazon dieback, ice sheet collapse, species extinction cascades). Structural: once irreversibility sets in, the constraint transitions from snare (future generations can theoretically exit through adaptation/technology) to mountain (physical limit that binds all agents).',
    'If threshold is near (10-30 years): scaffold perspectives become invalid, urgency rises to maximum. If threshold is distant (100+ years): current extraction rationale (adaptation technology will emerge) gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_threshold, empirical, 'Proximity to irreversible tipping points').

omega_variable(
    intergenerational_contract_enforceability,
    'Can binding institutional mechanisms (carbon budgets, debt ceilings, AI governance treaties, ecosystem restoration mandates) enforce intergenerational accountability, or are they inherently unenforceable against future political choice?',
    'Institutional analysis: (a) Legal mechanisms (constitutional environmental rights, binding international treaties with enforcement); (b) Technology locks (renewable infrastructure, restored ecosystems, AI alignment constraints); (c) Narrative/norm shifts (if future generations inherit values that enforce parent generation''s commitments).',
    'If enforceable: scaffold exits are real, and organized coalitions can lock in alternative pathways. If unenforceable: future generations inherit the institutional choice to continue extraction, and the snare persists across generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_contract_enforceability, empirical, 'Whether intergenerational contracts can be institutionally enforced').

omega_variable(
    technological_bailout_availability,
    'Will technological emergence (fusion, negative emissions, asteroid resources, geoengineering) allow future generations to adapt to or reverse accumulated damage, converting snare into tangled_rope?',
    'Forecasting: timeline and feasibility of (a) carbon removal at gigatonne scale, (b) climate resilience infrastructure, (c) circular economy scaling, (d) space-based resource extraction. Empirical tracking of technology adoption curves and cost trajectories.',
    'If bailouts are likely (median confidence 70%+): the constraint is tangled_rope (future generations suffer but can adapt). If unlikely (median confidence <40%): snare is locked in (cumulative damage exceeds adaptation capacity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_bailout_availability, empirical, 'Likelihood of technological solutions to deferred costs').

omega_variable(
    substitution_between_harm_vectors,
    'As one extraction vector (carbon) becomes constrained, do institutional players simply shift to alternative vectors (microplastics, rare earth mining, debt-financed surveillance infrastructure, uncontrolled AI scaling), maintaining net extraction?',
    'Empirical tracking: (a) Carbon emissions transition to renewable energy; does material extraction per capita decline or shift to other minerals/rare earths? (b) As climate spending rises, does debt-as-percentage-of-GDP decline or merely redistribute? (c) As AI safety research funds increase, are they genuinely allocated to alignment or diverted to capability acceleration?',
    'If substitution occurs: the constraint is not being resolved, only displaced. Net extraction and suppression remain high despite apparent policy change. If substitution is blocked: organized advocacy coalitions have real leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_between_harm_vectors, empirical, 'Whether extraction vectors substitute or decline in total').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intertemporal_responsibility_gap, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iterg_tr_t0, intertemporal_responsibility_gap, theater_ratio, 0, 0.48).
narrative_ontology:measurement(iterg_tr_t25, intertemporal_responsibility_gap, theater_ratio, 25, 0.58).
narrative_ontology:measurement(iterg_tr_t50, intertemporal_responsibility_gap, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(iterg_be_t0, intertemporal_responsibility_gap, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(iterg_be_t25, intertemporal_responsibility_gap, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(iterg_be_t50, intertemporal_responsibility_gap, base_extractiveness, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intertemporal_responsibility_gap, resource_allocation).
narrative_ontology:affects_constraint(intertemporal_responsibility_gap, climate_tipping_point_cascade).
narrative_ontology:affects_constraint(intertemporal_responsibility_gap, sovereign_debt_sustainability).
narrative_ontology:affects_constraint(intertemporal_responsibility_gap, ai_capability_acceleration).
narrative_ontology:affects_constraint(intertemporal_responsibility_gap, biodiversity_collapse_threshold).
narrative_ontology:affects_constraint(intertemporal_responsibility_gap, intergenerational_wealth_concentration).

% DUAL FORMULATION NOTE:
% The intertemporal responsibility gap decomposes into domain-specific constraints with different ε values: climate extraction (ε ≈ 0.62), fiscal extraction (ε ≈ 0.55), technology acceleration extraction (ε ≈ 0.48), ecological extraction (ε ≈ 0.72). Each has its own measurement trajectory and alternative pathways. The parent constraint represents the structural commonality: systematic deferral of costs to future agents. The domain-specific children show how the gap manifests in different institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intertemporal_responsibility_gap, powerless, 0.95).
constraint_indexing:directionality_override(intertemporal_responsibility_gap, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
