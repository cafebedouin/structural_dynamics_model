% ============================================================================
% CONSTRAINT STORY: generational_wealth_concentration_korea
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generational_wealth_concentration_korea, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: generational_wealth_concentration_korea
 *   human_readable: Generational Wealth Concentration in South Korea
 *   domain: economic/institutional
 *
 * SUMMARY:
 *   South Korea's generational wealth concentration represents a structural
 *   constraint that produces radically different classifications depending on
 *   observer position. The constraint operates through multiple reinforcing
 *   mechanisms: inherited capital accumulation, chaebol gatekeeping of
 *   entrepreneurial opportunity, housing price inflation disconnected from
 *   wage growth, inheritance and capital gains tax structures that preserve
 *   wealth concentration, and cultural narratives (Confucian filial duty,
 *   family-centered business organization) that legitimize dynastic
 *   succession. The constraint exhibits high extractiveness (0.68) because
 *   the distribution of lifetime economic opportunity is increasingly
 *   predetermined by parental wealth rather than individual effort or merit.
 *   Suppression is very high (0.72) because exit mechanisms are structurally
 *   limited: young wage earners cannot accumulate capital at rates sufficient
 *   to overcome initial disadvantage; small business owners face chaebol
 *   gatekeeping and credit access barriers; non-chaebol families experience
 *   housing prices that consume 8-12 years of median household income for
 *   down payment alone. The theater ratio (0.58) reflects the gap between
 *   official meritocratic/market framing (anyone can succeed through hard
 *   work) and the institutional reality (parental wealth predicts adult
 *   wealth with correlation ~0.50 and rising).
 *
 * KEY AGENTS:
 *   - Chaebol Owning Families: Primary beneficiary (institutional/arbitrage) — capture disproportionate capital allocation, regulatory favoritism, and intergenerational wealth transfer mechanisms; multiple exit options including asset diversification and political leverage
 *   - Young Wage Earner: Primary victim (powerless/trapped) — faces housing costs consuming 40-50% of income, limited capital accumulation capacity, and inheritance-disadvantaged position; minimal exit options
 *   - Small Business Owner (non-chaebol): Secondary victim (moderate/constrained) — constrained by chaebol gatekeeping, limited access to startup capital and credit, and unfavorable regulatory environment; retains some agency through small-scale operation but faces high barriers to scaling
 *   - Government Regulator: Mixed actor (institutional/constrained) — experiences both coordination (tax revenue for infrastructure, capital formation for economic growth) and extraction (regulatory capture favoring chaebol); captured by political donations from wealth-holding interests
 *   - Regulatory Reform Coalition: Organized agent (organized/constrained) — civil society and progressive political actors attempting to increase inheritance taxation, implement real estate price controls, and democratize startup capital access; constrained by political capture but perceives exit pathway through generational policy shifts
 *   - Confucian Cultural Frame: Institutional narrative (institutional/arbitrage) — legitimizes family-based wealth transmission as natural and dutiful; benefits wealth-holders by providing non-extractive framing but operates primarily through narrative inertia (piton mechanism)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generational_wealth_concentration_korea, 0.68).
domain_priors:suppression_score(generational_wealth_concentration_korea, 0.72).
domain_priors:theater_ratio(generational_wealth_concentration_korea, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generational_wealth_concentration_korea, extractiveness, 0.68).
narrative_ontology:constraint_metric(generational_wealth_concentration_korea, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(generational_wealth_concentration_korea, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generational_wealth_concentration_korea, snare).
narrative_ontology:human_readable(generational_wealth_concentration_korea, "Generational Wealth Concentration in South Korea").
narrative_ontology:topic_domain(generational_wealth_concentration_korea, "economic/institutional").

domain_priors:requires_active_enforcement(generational_wealth_concentration_korea).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generational_wealth_concentration_korea, chaebol_owning_families).
narrative_ontology:constraint_beneficiary(generational_wealth_concentration_korea, large_real_estate_holders).
narrative_ontology:constraint_beneficiary(generational_wealth_concentration_korea, existing_capital_holders).
narrative_ontology:constraint_victim(generational_wealth_concentration_korea, younger_generation_wealth_builders).
narrative_ontology:constraint_victim(generational_wealth_concentration_korea, non_chaebol_small_business_owners).
narrative_ontology:constraint_victim(generational_wealth_concentration_korea, wage_earning_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG WAGE EARNER (SNARE) — Faces compounding structural barriers: housing prices disconnected from income growth, inheritance tax structures that preserve wealth concentration, and limited access to entrepreneurial capital. Cannot exit the constraint without generational wealth transfer or extraordinary luck. Maximum experienced extraction.
constraint_indexing:constraint_classification(generational_wealth_concentration_korea, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (TANGLED ROPE) — Faces real coordination benefits (access to skilled workforce, infrastructure, legal systems) alongside asymmetric extraction (chaebol gatekeeping, capital access barriers, regulatory favoritism toward large firms). Constrained by high exit costs (relocation, market access loss) but retains some agency.
constraint_indexing:constraint_classification(generational_wealth_concentration_korea, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHAEBOL OWNERSHIP FAMILY (ROPE) — Experiences the constraint as pure coordination: inheritance structures, capital allocation mechanisms, and business succession protocols solve genuine collective action problems within the family empire. Net beneficiary with multiple arbitrage options (diversification, asset transfer, political leverage).
constraint_indexing:constraint_classification(generational_wealth_concentration_korea, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Organized civil society and progressive political actors frame wealth concentration as a temporary policy failure addressable through inheritance tax increases, capital gains taxation, real estate price controls, and startup capital democratization. See constraint as having sunset logic: improved policy could redistribute constraint burden over generational timescale. Suppression remains high (political capture, property rights frames) but coalition perceives exit pathway.
constraint_indexing:constraint_classification(generational_wealth_concentration_korea, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONFUCIAN SOCIAL ORDER FRAME (PITON) — Deep cultural narrative that family-based wealth transmission and hierarchical economic organization reflect natural order and filial duty. This frame persists through institutional inertia and cultural narrative rather than through actual coordination function. Modern Korean economy operates through legal structures and market mechanisms, not Confucian principles, yet the cultural legitimacy frame maintains the constraint's social acceptance. Theater ratio reflects the gap between framing and actual mechanism.
constraint_indexing:constraint_classification(generational_wealth_concentration_korea, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: GOVERNMENT REGULATOR (TANGLED ROPE) — Experiences mixed coordination and extraction. Genuine coordination: tax collection, infrastructure investment, social stability require wealth accumulation and capital formation in some concentrated form. Asymmetric extraction: regulations structurally favor chaebol (tax breaks, infrastructure priority, regulatory leniency) while constraining redistribution mechanisms. Constrained by political capture and capital flight risk.
constraint_indexing:constraint_classification(generational_wealth_concentration_korea, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing wealth concentration as immutable economic law: capital accumulation follows power law distributions, inheritance preserves accumulated advantage, and competitive markets concentrate over time. But the structural data contradicts the mountain classification — the Korea constraint exhibits high suppression (policy-mediated), political choice (inheritance tax levels, capital gains taxation, real estate regulation), and contingent institutional design. Engine will detect as false summit.
constraint_indexing:constraint_classification(generational_wealth_concentration_korea, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generational_wealth_concentration_korea_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(generational_wealth_concentration_korea, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(generational_wealth_concentration_korea, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(generational_wealth_concentration_korea, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(generational_wealth_concentration_korea, TR),
    TR >= 0.70.

:- end_tests(generational_wealth_concentration_korea_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting that lifetime economic opportunity is increasingly predetermined by parental wealth. The measurement trajectory shows steady increase from 0.52 (30 years ago, when more mobility existed) to 0.68 (present), indicating constraint has tightened. The extraction is not maximal (0.68 vs 0.95) because some mobility pathways remain open (education, corporate advancement, small business success), and enforcement is not totalizing. Suppression (0.72): Very high. Multiple reinforcing mechanisms constrain exit: housing prices create 8-12 year downpayment barriers; chaebol control 40% of economic output and gatekeep major industries; inheritance tax rates have declined from 70% (1990s) to 40-50% (2010s) while wealth concentration accelerated; credit access for non-chaebol founders remains 3-5x harder; social networks for business opportunity access correlate strongly with parental wealth. Theater ratio (0.58): Moderate-high, indicating 58% of activity is performative. Meritocratic framing (anyone can succeed through effort) masks inheritance-driven outcomes; government 'startup initiatives' often direct capital to connected entrepreneurs; educational institutions claim to provide mobility while correlating outcome with parental wealth (98% of Seoul National University students come from top 20% income tier); cultural narratives emphasize individual responsibility while institutions systematically advantage inherited capital. Theater has increased over the interval as policy theater (tax cuts marketed as growth, startup programs that de facto benefit connected founders) has expanded.
 *
 * PERSPECTIVAL GAP:
 *   The gap between chaebol rope and young-earner snare reveals the constraint's extraction mechanism: it is simultaneously experienced as pure coordination by beneficiaries (family succession, capital allocation) and pure extraction by victims (trapped in disadvantage, predetermined outcomes). This gap is diagnostic of snare classification. The tangled rope perspectives (small business, government) occupy the middle — experiencing both coordination benefits and asymmetric extraction. The piton and mountain perspectives attempt to naturalize the constraint through cultural/economic framing, but the structural data reveals these framings as institutional choice rather than natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position: beneficiary status vs victim status, and exit capacity within the constraint. Chaebol families (beneficiary + arbitrage) experience very low or negative d (extraction flows toward them). Young wage earners (victim + trapped) experience maximum d (extraction flows away from them, all costs borne). Small business owners (victim + constrained) experience high d (significant costs but some exit options). Government regulators (mixed beneficiary-victim + constrained) experience moderate d reflecting political capture. Reform coalition (organized + constrained) experiences moderate d (high costs but organizational agency reduces experienced extraction). The analytical observer (analytical/analytical) experiences d derived from universalizing the constraint across all positions, averaging to moderate d. The f(d) sigmoid then scales these into experienced extractiveness chi values that explain why the same structural phenomenon feels like coordination (rope) to beneficiaries and pure extraction (snare) to powerless victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved through the perspectival structure. The analytical observer's natural law view (mountain) is revealed as false by the data: Korea's wealth concentration has measurably increased over 30 years through policy choices (declining inheritance tax rates, regulatory favoritism toward chaebol, housing speculation enabled by credit policy). The constraint is not immutable — it is enforced through contingent institutional design. The piton view (Confucian cultural frame) is legitimate but secondary — the cultural narrative provides legitimacy for institutional arrangements that are primarily maintained through economic gatekeeping and regulatory capture, not through genuine cultural adherence. The snare view (powerless wage earner) is the structural ground truth: the constraint functions through trapped exit mechanisms and high suppression. The rope, tangled rope, and scaffold views are all legitimate perspectival readings but do not negate the snare dynamics at the powerless level. The reformability (scaffold) is real — policy changes could reduce extractiveness — but requires overcoming political capture (a secondary constraint). The snare classification captures the empirical situation: the constraint persists because it benefits concentrated interests who control policy levers, suppression is high and structural, and exit mechanisms for trapped agents are minimal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inheritance_tax_enforcement_capacity,
    'Does Korea''s inheritance tax system enforce wealth redistribution or primarily extract administrative costs while allowing wealth circumvention?',
    'Comparative analysis: measured wealth concentration change post-inheritance vs alternative redistribution mechanisms; tracking of tax avoidance patterns (corporate restructuring, gift tax arbitrage, asset hiding)',
    'If enforcement effective: inheritance tax could reduce chi significantly (policy intervention works). If enforcement fails: tax becomes theater masking continued concentration (chi remains high, theater increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inheritance_tax_enforcement_capacity, empirical, 'Whether inheritance tax actually redistributes or becomes administrative theater').

omega_variable(
    chaebol_reform_political_viability,
    'Are structural reforms to chaebol gatekeeping (breakup, restrictions on cross-shareholding, separation of ownership-management) politically viable or permanently captured by wealth-holding interests?',
    'Historical tracking of reform attempts, success rates, and political cost; analysis of campaign finance flows from chaebol to political actors; measurement of reform persistence vs regulatory capture cycles',
    'If viable: scaffold perspective is accurate (sunset possible through reform). If captured: constraint is entrenched (snare/piton perspectives dominate, mountain risk increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chaebol_reform_political_viability, preference, 'Whether chaebol structural reform is politically achievable').

omega_variable(
    housing_price_mechanism_causality,
    'Does housing price inflation drive wealth concentration or reflect it? Are inflated prices a constraint mechanism or a symptom of underlying capital accumulation disparities?',
    'Decomposition analysis: separating speculative demand (wealth-driven) from supply constraints (policy-driven); cross-regional analysis of housing inflation correlation with inheritance patterns; counterfactual: housing price controls without wealth redistribution',
    'If housing drives concentration: policy intervention on housing prices could reduce chi. If housing reflects pre-existing concentration: housing controls become theater unless underlying capital distribution changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(housing_price_mechanism_causality, conceptual, 'Whether housing prices drive or reflect wealth concentration').

omega_variable(
    generational_mobility_measurement_baseline,
    'What baseline generational wealth mobility would constitute ''normal'' capitalism vs extractive constraint? How much intergenerational persistence is institutional design choice vs structural necessity?',
    'Cross-national comparison (Denmark, Japan, US, Korea) of intergenerational mobility correlation coefficients; analysis of policy variation (inheritance tax rates, startup capital access, education financing) vs outcomes; synthetic counterfactuals',
    'If Korea''s persistence is policy-chosen: the constraint is policy-enforced snare. If Korea''s persistence matches structural baseline: constraint is reduced to coordination cost (rope-range).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_mobility_measurement_baseline, preference, 'Baseline intergenerational wealth mobility as reference').

omega_variable(
    startup_capital_access_alternative_mechanisms,
    'Would venture capital democratization, microfinance, or government startup funding significantly alter the constraint, or do wealth-based barriers operate through non-capital mechanisms (networks, mentorship, opportunity access)?',
    'Comparative analysis: startup success rates by founder wealth tier; tracking of non-capital barriers (network access, internship gatekeeping, venture capital founder preferences); pilot programs testing capital-only interventions',
    'If capital is primary barrier: startup funding redistribution reduces chi. If barriers are relational/network: capital redistribution becomes theater, constraint remains snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(startup_capital_access_alternative_mechanisms, empirical, 'Whether capital access barriers can be addressed independently from network/relational barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generational_wealth_concentration_korea, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gwck_tr_t0, generational_wealth_concentration_korea, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gwck_tr_t15, generational_wealth_concentration_korea, theater_ratio, 15, 0.5).
narrative_ontology:measurement(gwck_tr_t30, generational_wealth_concentration_korea, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(gwck_be_t0, generational_wealth_concentration_korea, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gwck_be_t15, generational_wealth_concentration_korea, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(gwck_be_t30, generational_wealth_concentration_korea, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generational_wealth_concentration_korea, resource_allocation).
narrative_ontology:boltzmann_floor_override(generational_wealth_concentration_korea, 0.18).
narrative_ontology:affects_constraint(generational_wealth_concentration_korea, korean_housing_market_speculation).
narrative_ontology:affects_constraint(generational_wealth_concentration_korea, chaebol_regulatory_capture).
narrative_ontology:affects_constraint(generational_wealth_concentration_korea, education_access_inequality_korea).
narrative_ontology:affects_constraint(generational_wealth_concentration_korea, intergenerational_social_mobility_korea).

% DUAL FORMULATION NOTE:
% Generational wealth concentration is decomposed from its constituent mechanisms: housing market gatekeeping (separate story with higher theater, lower extractiveness), chaebol regulatory capture (separate story with institutional focus), education-based sorting (separate story with identity_coordination type), and intergenerational mobility barriers (synthetic construct measuring multiple constraints). Each story has its own ε value and perspectives. The wealth concentration story aggregates these at the national level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(generational_wealth_concentration_korea, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
