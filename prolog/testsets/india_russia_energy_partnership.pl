% ============================================================================
% CONSTRAINT STORY: india_russia_energy_partnership
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_russia_energy_partnership, []).

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
 *   constraint_id: india_russia_energy_partnership
 *   human_readable: India-Russia Energy Partnership Dependency Lock
 *   domain: geopolitical/energy_economics
 *
 * SUMMARY:
 *   The India-Russia energy partnership represents a structural constraint
 *   that combines genuine coordination (energy security for India, stable
 *   market for Russia) with asymmetric extraction (dependency lock, limited
 *   strategic autonomy for India, supply conditionality leverage for Russia).
 *   The constraint has deepened over the 2014-2026 interval as Western
 *   sanctions on Russia increased the partnership's strategic importance to
 *   both sides, while simultaneously increasing the cost of alternative
 *   options for India. The theater ratio has increased over time as both
 *   governments have rhetorically framed the partnership as
 *   identity-foundational ('strategic partnership,' 'civilizational
 *   alignment') while the underlying mechanics have become more explicitly
 *   transactional (volume discounts in exchange for geopolitical positioning,
 *   pricing tied to regional behavior). The constraint exhibits all six
 *   classification types from different institutional perspectives, revealing
 *   that the question 'is this a snare or a rope?' is unanswerable without
 *   specifying the observing agent's position.
 *
 * KEY AGENTS:
 *   - Russian energy sector: Primary beneficiary (institutional/arbitrage) — captures market share, stable revenue, geopolitical leverage; can redirect supply or manage pricing
 *   - Indian energy consumers: Primary victim (powerless/trapped) — bear extraction through price volatility risk premium and limited supply alternatives; geographically and economically trapped
 *   - Indian strategic establishment: Secondary victim/beneficiary (moderate/constrained) — benefits from energy security during electoral/fiscal crises; constrained by dependency limiting independent diplomacy
 *   - Global energy transition coalition: Organized actor (organized/mobile) — international climate institutions, renewable financing mechanisms, alternative supply partnerships; building structural sunset clause
 *   - Cold War institutional legacy: Institutional inertia (institutional/constrained) — Soviet-era relationships and supply contracts maintain partnership through bureaucratic momentum rather than current strategic fit
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — sees risk of naturalizing geopolitical contingency as physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_russia_energy_partnership, 0.52).
domain_priors:suppression_score(india_russia_energy_partnership, 0.65).
domain_priors:theater_ratio(india_russia_energy_partnership, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_russia_energy_partnership, extractiveness, 0.52).
narrative_ontology:constraint_metric(india_russia_energy_partnership, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(india_russia_energy_partnership, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_russia_energy_partnership, tangled_rope).
narrative_ontology:human_readable(india_russia_energy_partnership, "India-Russia Energy Partnership Dependency Lock").
narrative_ontology:topic_domain(india_russia_energy_partnership, "geopolitical/energy_economics").

domain_priors:requires_active_enforcement(india_russia_energy_partnership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_russia_energy_partnership, russian_energy_exporters).
narrative_ontology:constraint_beneficiary(india_russia_energy_partnership, indian_government_stability).
narrative_ontology:constraint_victim(india_russia_energy_partnership, indian_energy_consumer_affordability).
narrative_ontology:constraint_victim(india_russia_energy_partnership, indian_strategic_autonomy).
narrative_ontology:constraint_victim(india_russia_energy_partnership, alternative_energy_partnerships).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN ENERGY CONSUMER (SNARE) — Trapped in energy dependency with no alternative supply sources. Bears extraction through price volatility, geopolitical risk premium, and limited negotiating leverage. Suppressed by geographic isolation from alternative suppliers and lack of domestic capacity to replace Russian oil/gas imports.
constraint_indexing:constraint_classification(india_russia_energy_partnership, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIAN STRATEGIC ESTABLISHMENT (TANGLED ROPE) — Constrained by energy security requirements and geopolitical positioning between Western sanctions regimes and Russian partnership. Genuine coordination function: Russian energy supply stabilizes domestic politics and inflation during critical electoral/fiscal periods. Active extraction: deepened dependency limits India's ability to pursue independent regional diplomacy or alternative partnerships. Constrained exit due to domestic energy deficit and capital requirements for alternative infrastructure.
constraint_indexing:constraint_classification(india_russia_energy_partnership, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RUSSIAN ENERGY SECTOR (ROPE) — Net beneficiary (arbitrage exit: can redirect supply to other buyers or manage through price signals). Experiences constraint as pure coordination: stable Indian market enables production planning and hedges against Western sanctions. Extraction concentrated and enforced through supply conditionality and pricing mechanisms, but beneficiary has significant agency.
constraint_indexing:constraint_classification(india_russia_energy_partnership, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL ENERGY TRANSITION COALITION (SCAFFOLD) — International climate commitments, renewable energy financing (IRENA, ADB, climate funds), and technological maturation of solar/wind create a structural sunset clause for fossil fuel dependency. High theater in current rhetoric about 'energy security' masking transition to alternative supply pathways. Organized actors (IFC, bilateral green partnerships, Indian renewable capacity) see the partnership as temporary stabilization during the energy transition window. Exit path is clear but requires capital and political will.
constraint_indexing:constraint_classification(india_russia_energy_partnership, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR ENERGY ALLIANCE FRAMEWORK (PITON) — Historical legacy structure (Soviet-era oil credits, supply agreements, technical debt) maintains partnership through institutional inertia despite changed geopolitical context. Theater ratio high: 'strategic partnership' rhetoric obscures declining functional complementarity as India's strategic interests diverge from Russia's. The alliance persists because replacement institutions haven't fully formed, not because contemporary geopolitics requires it.
constraint_indexing:constraint_classification(india_russia_energy_partnership, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an analytical view focused on thermodynamic and geographic constraints, energy dependency is an immutable feature: energy transport requires infrastructure (pipelines, shipping) that creates geographic lock-in. Distance between Indian consumption and global energy sources creates natural friction in energy markets. However, structural data contradicts mountain classification: the partnership is contingent on political choices (sanctions, pricing models, alternative investment pathways), not on physical law. This perspective risks naturalizing a geopolitical arrangement as a structural inevitability.
constraint_indexing:constraint_classification(india_russia_energy_partnership, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_russia_energy_partnership_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_russia_energy_partnership, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_russia_energy_partnership, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(india_russia_energy_partnership, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(india_russia_energy_partnership, TR),
    TR >= 0.70.

:- end_tests(india_russia_energy_partnership_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Measured as the proportion of India's energy security surplus captured by Russia relative to alternative market prices and supply options. In the 2014-2022 interval, Russia captured significant price discounts in exchange for volume commitments that locked Indian consumption patterns. Post-2022, Western sanctions increased Russia's dependence on India, raising Indian negotiating leverage but also deepening the partnership's geopolitical risk (association with sanctions-busting), which suppresses India's ability to diversify safely. The value has increased from 0.28 to 0.52 over the measurement interval as dependency has deepened and alternative suppliers have become more politically costly. Suppression (0.65): High. Multiple barriers prevent exit: geographic distance from alternative suppliers (requires new pipeline or LNG terminal infrastructure); political cost of abandoning a key strategic partner during geopolitical crisis; capital requirements for alternative energy infrastructure competing with other domestic spending; technical debt from Soviet-era systems lock India into Russian spare parts and expertise. Suppression does not imply explicit coercion but rather structural barriers. Theater ratio (0.58): Moderate. The partnership is discussed in civilizational/identity terms ('strategic partnership,' 'centuries of friendship') that obscure the mechanics of energy pricing and volume conditionality. Actual negotiations are transactional: volume discounts in exchange for geopolitical positioning (reducing India's cooperation with Western sanctions, positioning in UN votes, etc.), but public rhetoric frames this as cultural alignment. Theater has increased over time as geopolitical stakes have risen and both governments have found it convenient to frame energy dependency as identity rather than acknowledge explicit conditionality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The Russian energy sector experiences a Rope: they are solving the genuine coordination problem of ensuring market access and stable demand during geopolitical isolation. The Indian energy consumer experiences a Snare: trapped in dependency with no alternatives and no seat at the table where supply conditionality is negotiated. The Indian strategic establishment experiences Tangled Rope: the partnership genuinely reduces energy price volatility during critical electoral periods (coordination function) but also constrains their ability to pursue independent regional diplomacy or sanction-compliant partnerships (extraction function). The global energy transition coalition experiences Scaffold: renewable energy financing, solar manufacturing capacity, and battery storage development create a clear exit path with a 10-20 year horizon. The Cold War alliance framework experiences Piton: the partnership persists through institutional inertia (supply contracts, technical relationships, bureaucratic paths) even though contemporary geopolitics no longer requires it. The analytical observer risks experiencing Mountain (energy dependency is a law of geography and physics) but structural data reveals this as a false summit — the partnership is contingent on political choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the structural relationship between each agent and the extraction flow. Russian energy exporters begin with d ≈ 0.10 (beneficiary + arbitrage exit = low extraction experienced) — they can redirect supply, manage pricing, negotiate with multiple buyers. This produces f(d) ≈ -0.01, yielding negative effective extraction (they benefit net). Indian energy consumers begin with d ≈ 0.92 (victim + trapped exit = high extraction experienced) — they cannot exit, cannot negotiate, bear full price volatility risk. This produces f(d) ≈ 1.40, yielding maximum effective extraction (χ ≈ 0.73 before scope dampening). The Indian strategic establishment occupies d ≈ 0.55 (victim + constrained exit = mixed) — they can exit but at significant capital/geopolitical cost. This produces f(d) ≈ 0.65, moderate effective extraction. The scope modifier σ(national) = 1.0 for India's perspective, but σ(global) = 1.2 for Russia's, reflecting that Russia's extraction mechanism operates at global scale (they can always find alternative buyers) while India's dependency is nationally constrained. The directionality chain reveals why the beneficiary and victim see fundamentally different constraint types from the same base properties: they occupy different structural positions relative to the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits the full mandatrophy structure: it appears as pure coordination (Rope) from Russia's perspective, as mixed coordination-extraction (Tangled Rope) from India's strategic view, as pure extraction (Snare) from the consumer's perspective, and as temporary problem with a built-in exit (Scaffold) from the global energy transition view. The mandatrophy is resolved by recognizing that these are not competing claims about 'what the constraint really is' but legitimate descriptions of different structural positions within the same causal system. The beneficiary (Russia) genuinely does experience coordination: they are solving a real problem (market access). The victim (India's consumers) genuinely does experience extraction: they are trapped in a dependency structure. The strategic establishment genuinely does experience both: energy security is a real coordination benefit, but strategic autonomy is a real cost. The mandatrophy resolution: the constraint IS a tangled rope. It coordinates energy supply (genuine function) while extracting strategic autonomy and affordability. The piton classification emerges from the high theater ratio: Cold War-era institutional frames (Soviet alliance) are being invoked to maintain a partnership that contemporary geopolitics alone does not require. If the partnership is perceived as identity-foundational by Indian institutions (identity_locked cognitive frame), the exit options narrow further for the strategic establishment, even if structurally viable alternatives exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_autonomy_extraction_threshold,
    'At what level of energy import dependency does India''s strategic autonomy become materially compromised in regional diplomacy?',
    'Historical analysis of correlation between energy dependency ratios and foreign policy independent decision-making; comparison with peer states at similar dependency levels; modeling of decision constraints under different supply scenarios',
    'If threshold < 30% of imports: current partnership at ~40% creates meaningful constraint on diplomatic leverage. If threshold > 50%: extraction mechanism is overstated. High impact on whether classification stays Snare/Tangled Rope or shifts to Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_autonomy_extraction_threshold, empirical, 'Strategic autonomy threshold under energy dependency').

omega_variable(
    renewable_transition_timeline_credibility,
    'Is the renewable energy transition timeline (2030-2050 net-zero commitments) credible for Indian energy security, or does it remain aspirational theater?',
    'Capital flow tracking: actual investment in renewable capacity vs announced targets; grid infrastructure readiness; rare-earth supply chain for solar panels and wind turbines; dispatchability solutions (battery storage, hydrogen). Comparison between projected and realized renewable additions 2020-2026.',
    'If credible: scaffold classification confirmed, sunset clause is structural. If aspirational: scaffold classification is overstated, dependency structure persists longer, snare/tangled rope deeper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_transition_timeline_credibility, empirical, 'Credibility of renewable energy transition timeline for energy autonomy').

omega_variable(
    price_lock_enforcement_mechanism,
    'Does Russia enforce the partnership through explicit supply conditionality or implicit pricing mechanisms, and how does this distinction affect the suppression measure?',
    'Analysis of pricing data: spot market vs contract pricing deviations; supply disruption history during geopolitical crises; published vs actual delivery volumes; evidence of quid pro quo linkage between energy supply and Indian foreign policy positions',
    'If explicit conditionality: suppression is high (trapped), extraction is structural coercion (Snare). If implicit/market-based: suppression is moderate (constrained), extraction is coordination cost (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(price_lock_enforcement_mechanism, empirical, 'Whether suppression operates through explicit conditionality or implicit market mechanisms').

omega_variable(
    alternative_partnership_switching_costs,
    'What are the actual capital and geopolitical switching costs for India to replace Russian energy with alternative suppliers (Middle East, Africa, LNG markets)?',
    'Infrastructure cost modeling: pipeline construction, LNG terminal capacity, port deepening; geopolitical analysis: alliance relationship costs with alternative suppliers; market analysis: price comparisons in open vs contracted markets; timeline to buildout alternative capacity',
    'If switching costs < 5% of GDP over 10 years: mobile/constrained exit is real, suppression is moderate. If > 15%: trapped dynamics are stronger, suppression is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_partnership_switching_costs, empirical, 'Capital and geopolitical costs of switching to alternative energy suppliers').

omega_variable(
    identity_lock_strategic_partnership_rhetoric,
    'Is the India-Russia partnership an identity-locked institutional commitment or a purely contingent strategic choice that can be revised without institutional crisis?',
    'Discourse analysis: how frequently partnership is invoked as foundational to Indian identity vs contingent to energy security; institutional rhetoric analysis: statements by Indian leadership about partnership necessity; comparison with similar institutional relationships that have been revised without existential institutional rupture',
    'If identity-locked: Indian strategic establishment experiences constraint as mountain, cannot perceive exit options structurally (identity_locked exit), perspectival gap is severe. If contingent: constraint is Tangled Rope/Scaffold throughout, exit is constrained but perceivable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_strategic_partnership_rhetoric, conceptual, 'Whether partnership commitment is identity-fused or contingent strategic choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_russia_energy_partnership, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, india_russia_energy_partnership, theater_ratio, 0, 0.35).
narrative_ontology:measurement(indi_tr_t8, india_russia_energy_partnership, theater_ratio, 8, 0.48).
narrative_ontology:measurement(indi_tr_t16, india_russia_energy_partnership, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, india_russia_energy_partnership, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(indi_be_t8, india_russia_energy_partnership, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(indi_be_t16, india_russia_energy_partnership, base_extractiveness, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_russia_energy_partnership, resource_allocation).
narrative_ontology:affects_constraint(india_russia_energy_partnership, indian_renewable_energy_capacity).
narrative_ontology:affects_constraint(india_russia_energy_partnership, western_sanctions_compliance).
narrative_ontology:affects_constraint(india_russia_energy_partnership, regional_energy_market_competition).

% DUAL FORMULATION NOTE:
% The India-Russia partnership decomposes into distinct constraint families: (1) Energy pricing coordination (Rope, low ε) — genuine mutual benefit in stable supply. (2) Strategic autonomy extraction (Tangled Rope, moderate ε) — geopolitical positioning costs. (3) Consumer affordability lock-in (Snare, high ε) — structural dependency on single supplier. Each family has different ε, different temporal dynamics, and different exit mechanisms. The overall constraint story captures the mixture but should be decomposed into separate stories for fine-grained analysis of which mechanism dominates at different time horizons and institutional levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(india_russia_energy_partnership, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
