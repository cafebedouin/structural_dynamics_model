% ============================================================================
% CONSTRAINT STORY: global_carbon_pricing_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_carbon_pricing_harmonization, []).

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
 *   constraint_id: global_carbon_pricing_harmonization
 *   human_readable: Global Carbon Pricing Harmonization
 *   domain: environmental_policy/economic_coordination
 *
 * SUMMARY:
 *   Global carbon pricing harmonization represents an attempt to coordinate
 *   worldwide decarbonization through unified carbon pricing mechanisms
 *   (carbon taxes, cap-and-trade systems, hybrid models). The constraint
 *   exhibits the full tension between genuine coordination needs and
 *   asymmetric extraction mechanisms. Climate mitigation is a real collective
 *   action problem requiring international coordination — catastrophic
 *   climate change harms all actors. Yet the governance mechanisms for
 *   pricing carbon impose disproportionate costs on energy-dependent
 *   developing economies, fossil fuel workers, and carbon-intensive
 *   manufacturing sectors while concentrating benefits among wealthy
 *   low-carbon economies and clean capital owners. The constraint's rising
 *   extractiveness (0.32→0.58 over 10 years) reflects increasing
 *   sophistication of pricing mechanisms and tightening enforcement through
 *   border carbon adjustments, creating progressively fewer exit pathways for
 *   non-compliant jurisdictions. Theater ratio (0.52→0.68) reflects expanding
 *   bureaucratic apparatus: carbon accounting standards, offset verification
 *   protocols, market administration, and compliance monitoring consume
 *   significant resources with uncertain actual emissions reduction impact.
 *   The constraint decomposes into multiple structurally distinct claims: (1)
 *   the coordination necessity of pricing carbon (genuine collective action
 *   problem), (2) the specific policy design of harmonized pricing (one
 *   contingent mechanism among alternatives), (3) the distributional
 *   consequences of that design (asymmetric extraction), and (4) the
 *   sufficiency of supporting mechanisms (just transition, technology
 *   transfer, offset integrity) to make the extraction acceptable.
 *
 * KEY AGENTS:
 *   - Energy-dependent developing economies: Primary victim (powerless/trapped) — must comply with global pricing or face trade penalties; no exit capacity
 *   - Fossil fuel dependent workers: Primary victim (powerless/trapped) — employment eliminated by carbon pricing; bears costs with minimal transition support
 *   - Low-carbon capital owners: Primary beneficiary (institutional/arbitrage) — direct profit from carbon pricing mechanisms and resulting capital flows to renewables
 *   - Wealthy low-carbon economies: Primary beneficiary (institutional/arbitrage) — first-mover advantage in renewable technology; carbon pricing protects domestic markets
 *   - Mid-income manufacturing economies: Secondary victim (moderate/constrained) — bear carbon costs for competitiveness but receive technology transfer
 *   - Climate-vulnerable small island states: Mixed (organized/mobile) — existential interest in carbon pricing but weak negotiating position
 *   - Fossil fuel dependent workers: Primary victim (powerless/trapped) — structural elimination from labor market
 *   - Just transition coalition: Organized agent (organized/constrained) — advocating for sunset clauses and transitional support
 *   - International carbon bureaucracy: Institutional actor (institutional/arbitrage) — benefits from expanding administrative apparatus
 *   - Analytical observer: Universal perspective (analytical/analytical) — risks naturalizing policy contingency as physical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_carbon_pricing_harmonization, 0.58).
domain_priors:suppression_score(global_carbon_pricing_harmonization, 0.62).
domain_priors:theater_ratio(global_carbon_pricing_harmonization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_carbon_pricing_harmonization, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_carbon_pricing_harmonization, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(global_carbon_pricing_harmonization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_carbon_pricing_harmonization, tangled_rope).
narrative_ontology:human_readable(global_carbon_pricing_harmonization, "Global Carbon Pricing Harmonization").
narrative_ontology:topic_domain(global_carbon_pricing_harmonization, "environmental_policy/economic_coordination").

domain_priors:requires_active_enforcement(global_carbon_pricing_harmonization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_carbon_pricing_harmonization, low_carbon_capital_owners).
narrative_ontology:constraint_beneficiary(global_carbon_pricing_harmonization, wealthy_nations_with_existing_pricing).
narrative_ontology:constraint_victim(global_carbon_pricing_harmonization, carbon_intensive_developing_economies).
narrative_ontology:constraint_victim(global_carbon_pricing_harmonization, energy_security_dependent_states).
narrative_ontology:constraint_victim(global_carbon_pricing_harmonization, vulnerable_workers_in_fossil_fuels).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENERGY-DEPENDENT DEVELOPING NATIONS (SNARE) — Trapped within global carbon pricing mechanisms that impose compliance costs without providing legitimate exit pathways. Must either decarbonize rapidly (economically devastating) or face trade penalties and capital access restrictions. No genuine coordination benefit; extraction mechanism enforces through border carbon adjustments and capital flight.
constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FOSSIL FUEL DEPENDENT WORKERS (SNARE) — Trapped by carbon pricing mechanisms that target their employment without providing just transition funding or retraining. Bears full extraction cost through job loss while coordination benefits accrue to capital owners and wealthy nations.
constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-INCOME MANUFACTURING ECONOMIES (TANGLED ROPE) — Constrained by need to maintain export competitiveness while bearing carbon pricing costs. Genuine coordination benefit exists (avoided climate catastrophe reduces supply chain disruption), but extraction is asymmetric: carbon pricing falls disproportionately on manufacturing sectors while benefits distribute globally. Moderate agency through technology transfer negotiations but high structural cost.
constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: WEALTHY LOW-CARBON ECONOMIES (ROPE) — Benefit from first-mover advantage in renewable energy and green technology. Carbon pricing mechanisms advantage domestic renewables and create export markets for green technology. Experience the constraint as coordination: harmonizing global pricing enables predictable market conditions for clean energy deployment. Net beneficiary with significant arbitrage capacity.
constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LOW-CARBON CAPITAL OWNERS (ROPE) — Direct beneficiaries of carbon pricing mechanisms. Harmonization creates predictable pricing signals that drive capital flows toward renewable energy, electric vehicles, and carbon capture. Experience pure coordination: alignment of global carbon prices reduces market fragmentation and enables large-scale decarbonization investment. Arbitrage access through technology transfer and green finance instruments.
constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE-VULNERABLE SMALL ISLAND STATES (TANGLED ROPE) — Organized through AOSIS (Alliance of Small Island States). Genuine coordination benefit: carbon pricing is necessary for climate mitigation that prevents their territorial dissolution. But extraction occurs through negotiation asymmetry — their existential interest in climate action weakens their bargaining position on pricing mechanisms and loss-and-damage funding. Mobile through climate finance and loss-and-damage agreements but constrained by dependency on major economies' enforcement.
constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL CARBON MARKET BUREAUCRACY (PITON) — Carbon accounting, verification protocols, and market administration consume significant resources (theater_ratio 0.68) with degraded functional output. Offset crediting systems are theatrically complex (additionality verification, permanence assessment) while actual emissions reductions remain uncertain. The bureaucratic apparatus persists through institutional inertia and treaty obligations, not because it effectively coordinates decarbonization.
constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: JUST TRANSITION COALITION (SCAFFOLD) — Organized through labor unions, development NGOs, and worker-advocate governments. See global carbon pricing as a temporary coordination mechanism requiring sunset clause: just transition funds, worker retraining, and sectoral phase-out timelines. Low effective extraction because the coalition has agency and sees an exit path (gradual phase-out with support structures replacing carbon pricing as primary mechanism).
constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, carbon pricing harmonization reflects an immutable physical constraint: decarbonization requires pricing carbon at its social cost, and global coordination is thermodynamically necessary to prevent coordination failures. This perspective risks naturalizing contingent policy mechanisms (international treaties, market design, enforcement structures) as laws of nature.
constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_carbon_pricing_harmonization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_carbon_pricing_harmonization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_carbon_pricing_harmonization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_carbon_pricing_harmonization, TR),
    TR >= 0.70.

:- end_tests(global_carbon_pricing_harmonization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Carbon pricing mechanisms extract from energy-dependent economies, fossil fuel workers, and carbon-intensive sectors while concentrating benefits among low-carbon capital and wealthy nations. The extraction is substantial but not total (snare-level) because developing economies retain some negotiating capacity (technology transfer, climate finance, just transition funds). The rising trajectory (0.32→0.58) reflects how enforcement mechanisms (border carbon adjustments, capital access restrictions, compliance verification) narrow exit options over time. Suppression (0.62): High. Multiple barriers prevent alternatives: international treaty obligations make unilateral exit costly; border carbon adjustments penalize non-participation; capital access is conditioned on compliance; no legitimate alternative decarbonization mechanisms are politically viable. Workers face suppression through employment elimination with inadequate transition support; developing economies face suppression through pricing regimes they did not negotiate. Theater ratio (0.68): Moderately-high. Carbon accounting and offset verification are theatrically complex with uncertain emissions reduction impact. Additionality assessment for offsets relies on contested baseline modeling; carbon accounting allows jurisdictional arbitrage through boundary definitions; market mechanisms create new layer of financial engineering (carbon derivatives, offset trading) with limited transparency. The theater has increased as mechanisms have become more sophisticated, suggesting substitution of complexity for function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The powerless/trapped agent (energy-dependent developing economy, fossil fuel worker) experiences this as a snare: immutable extraction with no exit and minimal coordination benefit. The institutional/arbitrage agent (wealthy low-carbon economy, clean capital owner) experiences this as rope: coordination mechanism that solves their problem and enables profit. The moderate/constrained agent (mid-income manufacturer) experiences tangled_rope: genuine coordination benefit balanced against asymmetric extraction, with some agency through negotiation. The organized/mobile agent (climate-vulnerable state) experiences tangled_rope: existential interest in coordination but weak bargaining power. The institutional/arbitrage actor (carbon bureaucracy) experiences piton: their apparatus persists through inertia and treaty obligation despite degraded functional output. The analytical observer at civilizational scope risks mountain: naturalizing policy contingency as thermodynamic necessity. This perspectival spread from snare (powerless) through rope (beneficiary) to mountain (naturalized) is the diagnostic signature of hybrid coordination-extraction mechanisms with structural injustice.
 *
 * DIRECTIONALITY LOGIC:
 *   The extraction flow runs from energy-dependent developing economies and fossil fuel workers (sources of extracted value) toward wealthy low-carbon economies and clean capital owners (sinks of extracted value). This is not hidden extraction — it is explicit in the policy design: developing economies must reduce emissions to comply with global pricing while wealthy economies profit from clean technology deployment. The key asymmetry is in exit options: wealthy economies can unilaterally exit through declaring victory on decarbonization; developing economies cannot exit without incurring trade penalties and capital access restrictions. The derived directionality parameter d quantifies this: powerless/trapped agents have d→1.0 (full target), institutional/arbitrage agents have d→0.0 (full beneficiary), constrained/moderate agents have d→0.55-0.65 (mixed). The engine computes f(d) sigmoid for each perspective, producing experienced extractiveness chi. Global scope (σ=1.2) amplifies chi calculation, reflecting that the constraint's verification difficulty and enforceability complexity increase at global scale.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY DYNAMICS: The constraint exhibits classical mandatrophy between coordination necessity (climate change requires carbon pricing) and extraction mechanism (pricing imposes asymmetric costs). The resolution requires distinguishing legitimate coordination costs (the cost of decarbonization is real and must be borne somewhere) from unjust distribution (why must energy-dependent economies and workers bear disproportionate costs?). The mandatrophy resolves through recognizing that multiple mechanisms can coordinate decarbonization: carbon pricing is one choice; direct regulation, public investment, supply-side restrictions (unburnable carbon left in ground), and technology mandates are structurally equivalent alternatives. The extraction arises from CHOOSING carbon pricing as the mechanism, not from the necessity of coordination itself. A just transition requires either (1) redistributing extraction through adequately-funded transitions and technology transfer (converting snare perspectives to tangled_rope), or (2) adopting alternative coordination mechanisms that do not concentrate extraction on powerless agents. The constraint's theater ratio (0.68, increasing) indicates that the complexity of pricing mechanisms is substituting for actual resolution — as offsetting and accounting become more sophisticated, they enable extraction to continue while disguising it as legitimate market function. True mandatrophy resolution requires testing whether adequate just transition funding and technology transfer can make extraction acceptable (omega variable 1,2) and whether alternative mechanisms exist that provide equivalent coordination with lower extraction (omega variable 6).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    just_transition_funding_adequacy,
    'Are just transition funds sufficient to prevent worker immiseration, or do they merely theatrically offset extraction of dependent workers?',
    'Longitudinal tracking of fossil fuel worker earnings trajectories post-transition; comparison of transition fund allocation to actual retraining costs and income replacement needs',
    'If adequate: snare perspective downgrade to tangled_rope for workers (extraction balanced by support). If inadequate: snare classification confirmed, extraction is uncompensated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_funding_adequacy, empirical, 'Whether just transition funding adequately compensates workers').

omega_variable(
    technology_transfer_sufficiency,
    'Do technology transfer agreements enable genuine decarbonization capacity in developing economies or merely create dependency on wealthy-nation green technology?',
    'Patent licensing tracking; domestic clean technology patent generation rates in technology-receiving nations; cost of renewable energy deployment pre/post technology access',
    'If genuine transfer: developing economy perspectives upgrade from snare to tangled_rope (extraction balanced by capability gain). If dependency creation: snare classification confirmed, extraction intensified through licensing constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_transfer_sufficiency, empirical, 'Whether technology transfer creates genuine capacity or deepens dependency').

omega_variable(
    carbon_leakage_mechanism_effectiveness,
    'Do border carbon adjustments actually prevent carbon leakage or merely provide protectionist cover for wealthy-nation manufacturing while claiming climate justification?',
    'Empirical analysis of trade flow shifts; tracking of investment redirection; comparison of carbon accounting across jurisdictions pre/post harmonization',
    'If effective: carbon pricing is pure coordination (rope from more perspectives). If leakage persists: border mechanisms are extractive theater (snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_leakage_mechanism_effectiveness, empirical, 'Whether border carbon adjustments prevent actual carbon leakage').

omega_variable(
    offset_quality_credibility,
    'Are voluntary carbon offset credits genuine emission reductions or do they enable wealthy-nation permit holders to maintain emissions while claiming compliance?',
    'Third-party audit of additionality claims; baseline modeling accuracy; permanence tracking for offset projects; comparison of offset costs to marginal abatement costs',
    'If genuine: carbon pricing functions as intended (coordination). If theater: offset mechanisms enable extraction while disguising it as compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(offset_quality_credibility, empirical, 'Whether voluntary carbon offsets represent real emission reductions').

omega_variable(
    developing_economy_mitigation_investment_substitution,
    'Does carbon pricing funding for developing-economy decarbonization actually enable their domestic clean investment or substitute for (displace) development financing they would have received anyway?',
    'Counterfactual analysis of development finance flows; tracking of total capital to developing economies (climate-labeled vs traditional development aid); correlation between carbon pricing revenue and reduction in traditional development finance',
    'If enabling: carbon pricing is coordination function (rope). If substituting: carbon pricing is extraction mechanism that rebrands existing capital flows (snare/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_economy_mitigation_investment_substitution, empirical, 'Whether climate finance substitutes for or supplements development aid').

omega_variable(
    carbon_pricing_necessity_vs_contingency,
    'Is global carbon pricing the only mechanism capable of achieving decarbonization targets, or are alternative regulatory and investment approaches structurally equivalent?',
    'Comparative analysis of decarbonization pathways (direct regulation, public investment, technology mandates, supply-side restrictions); modeling of alternative coordination mechanisms; historical analysis of prior energy transitions',
    'If necessary: mountain classification justified (immutable constraint). If contingent: pricing mechanism is contingent policy choice (enables snare/tangled_rope alternatives to appear as inevitable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_pricing_necessity_vs_contingency, conceptual, 'Whether carbon pricing is necessary or one contingent mechanism among alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_carbon_pricing_harmonization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcph_tr_t0, global_carbon_pricing_harmonization, theater_ratio, 0, 0.52).
narrative_ontology:measurement(gcph_tr_t5, global_carbon_pricing_harmonization, theater_ratio, 5, 0.6).
narrative_ontology:measurement(gcph_tr_t10, global_carbon_pricing_harmonization, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(gcph_be_t0, global_carbon_pricing_harmonization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gcph_be_t5, global_carbon_pricing_harmonization, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gcph_be_t10, global_carbon_pricing_harmonization, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_carbon_pricing_harmonization, resource_allocation).
narrative_ontology:affects_constraint(global_carbon_pricing_harmonization, energy_transition_financing).
narrative_ontology:affects_constraint(global_carbon_pricing_harmonization, developing_economy_debt_sustainability).
narrative_ontology:affects_constraint(global_carbon_pricing_harmonization, technology_transfer_governance).

% DUAL FORMULATION NOTE:
% Global carbon pricing harmonization decomposes into distinct structural claims: (1) Climate mitigation coordination necessity (ε≈0.05, mountain — decarbonization genuinely required), (2) Carbon pricing as coordination mechanism (ε≈0.35, rope — pricing can coordinate collective action), (3) Distributional consequences of pricing design (ε≈0.58, tangled_rope — mechanism concentrates extraction asymmetrically). This story focuses on claim 3 and its tension with claims 1-2. Upstream constraints (climate_mitigation_necessity, energy_transition_financing) establish coordination requirements; this constraint models how one policy mechanism distributes those costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_carbon_pricing_harmonization, powerless, 0.95).
constraint_indexing:directionality_override(global_carbon_pricing_harmonization, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
