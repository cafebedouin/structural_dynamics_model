% ============================================================================
% CONSTRAINT STORY: indian_tariff_protectionism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_tariff_protectionism, []).

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
 *   constraint_id: indian_tariff_protectionism
 *   human_readable: Indian Tariff Protectionism and Industrial Development
 *   domain: economic_policy/trade
 *
 * SUMMARY:
 *   Indian tariff protectionism represents a three-quarter-century-old
 *   institutional constraint that originated as a legitimate development
 *   strategy (infant industry protection in 1947-1960) but has progressively
 *   accumulated rent-extraction layers while maintaining its infant industry
 *   justification. The constraint exhibits the classical tangled rope
 *   signature: a genuine coordination function (enabling domestic
 *   manufacturing capacity and employment) is fused with asymmetric
 *   extraction (consumers and import-dependent industries bear high costs
 *   while protected manufacturers capture excess rents). The measurement
 *   trajectory shows consistent extractiveness and theater ratio increases:
 *   as protected sectors matured and should have graduated to global
 *   competitiveness, instead tariff protection became entrenched through
 *   political coalition formation and institutional path dependency. The
 *   theater ratio increase reflects that justifications (infant industry,
 *   strategic autonomy, Make in India) have become increasingly performative
 *   as the sectors they protect face no competitiveness pressure.
 *
 * KEY AGENTS:
 *   - Indian Consumers: Primary victim (powerless/trapped) — bear costs through higher prices and reduced product availability; structurally unable to exit domestic market
 *   - Import-Dependent Manufacturers: Secondary victim (powerless/trapped) — face tariff-driven input cost increases with no exit; cannot source domestically at competitive prices
 *   - Domestic Protected Manufacturers: Primary beneficiary (moderate/constrained) — capture excess rents from tariff protection; depend on tariff shield for margin sustainability; cannot exit without market share loss
 *   - Ministry of Commerce and Industry: Institutional beneficiary (institutional/arbitrage) — maintains tariff policy; gains political credibility from protectionist coalitions; can unilaterally modify policy but chooses not to
 *   - Global Trade Partners: Constrained beneficiaries (powerful/mobile) — face Indian tariff barriers but can exit through FDI or alternative markets; negotiate multilateral reduction schedules
 *   - Regional Trade Blocs (SAFTA, ASEAN FTA): Sunset mechanism (organized/constrained) — represent gradual liberalization timelines; constrained by rules of origin complexity and implementation delays
 *   - Infant Industry Doctrine Legacy: Institutional inertia (institutional/arbitrage) — the intellectual justification persists despite sectoral maturation; performative rather than functional
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_tariff_protectionism, 0.58).
domain_priors:suppression_score(indian_tariff_protectionism, 0.65).
domain_priors:theater_ratio(indian_tariff_protectionism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_tariff_protectionism, extractiveness, 0.58).
narrative_ontology:constraint_metric(indian_tariff_protectionism, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(indian_tariff_protectionism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_tariff_protectionism, tangled_rope).
narrative_ontology:human_readable(indian_tariff_protectionism, "Indian Tariff Protectionism and Industrial Development").
narrative_ontology:topic_domain(indian_tariff_protectionism, "economic_policy/trade").

domain_priors:requires_active_enforcement(indian_tariff_protectionism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_tariff_protectionism, domestic_manufacturing_firms).
narrative_ontology:constraint_beneficiary(indian_tariff_protectionism, labor_intensive_sectors).
narrative_ontology:constraint_beneficiary(indian_tariff_protectionism, state_owned_enterprises).
narrative_ontology:constraint_victim(indian_tariff_protectionism, indian_consumers).
narrative_ontology:constraint_victim(indian_tariff_protectionism, import_dependent_industries).
narrative_ontology:constraint_victim(indian_tariff_protectionism, foreign_exporters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN CONSUMER (SNARE) — Bears full cost of tariff protection through higher prices and reduced product variety. Structurally trapped: cannot opt out of domestic market, cannot exit national economy without extraordinary cost. No coalition capacity to challenge trade policy. Suppression is structural: tariffs are legally enforced with customs authority and import restrictions.
constraint_indexing:constraint_classification(indian_tariff_protectionism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMPORT-DEPENDENT INDUSTRIES (SNARE) — Manufacturing sectors requiring imported inputs (automotive components, electronics, pharmaceuticals) face tariff-driven cost increases with no exit option. Cannot source domestically at competitive prices; cannot relocate supply chains without massive sunk costs. Trapped between rising input costs and tariff-protected upstream sectors.
constraint_indexing:constraint_classification(indian_tariff_protectionism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC MANUFACTURERS (TANGLED ROPE) — Primary beneficiary: tariffs shield them from global competition, enabling higher margins and market share. But genuine coordination function exists: tariffs allow infant industry development and local capacity building. Extraction is real (protected firms charge higher prices) but coordination benefit is also real (local manufacturing employment, supply chain development). High suppression reflects that exit from protected markets is costly: firms become dependent on tariff shield and resist liberalization.
constraint_indexing:constraint_classification(indian_tariff_protectionism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MINISTRY OF COMMERCE AND INDUSTRY (ROPE) — Institutional beneficiary with arbitrage capacity: controls tariff policy and gains political credibility through protectionist coalitions. Experiences tariffs as a coordination mechanism: solves the collective action problem of infant industry protection and manages politically powerful manufacturing lobbies. Can exit through trade agreement commitments but chooses not to (arbitrage option). Net beneficiary.
constraint_indexing:constraint_classification(indian_tariff_protectionism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GLOBAL TRADE PARTNERS (TANGLED ROPE) — Constrained by Indian tariff walls but possess mobile exit options: can shift exports to other markets or invest locally within tariff barriers (FDI). Experience both extraction (tariff barriers reduce export access) and coordination benefit (predictable market protection encourages FDI commitment). Suppression operates through multilateral trade rules, not physical barriers. Power level reflects structural capacity to negotiate WTO commitments and threaten retaliatory tariffs.
constraint_indexing:constraint_classification(indian_tariff_protectionism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL TRADE BLOC MECHANISMS (SCAFFOLD) — SAFTA and bilateral FTAs with ASEAN, Japan represent sunset mechanisms: gradual tariff reduction schedules create a sunset clause for protection. Organized negotiation (constrained exit) creates predictable timelines for liberalization. Theater ratio reflects the gap between negotiated phase-down timelines and actual implementation (rules of origin complexity, non-tariff barriers persist). Scaffold applies because the sunset mechanism is real, even if the transition timeline extends.
constraint_indexing:constraint_classification(indian_tariff_protectionism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: INFANT INDUSTRY DOCTRINE (PITON) — The intellectual justification for Indian tariff protectionism is substantially performative at this point. The infant industry doctrine was theoretically justified in 1947-1960 when India's manufacturing capacity was genuinely underdeveloped. Seventy years later, India has large, efficient domestic manufacturing sectors that no longer require tariff protection to compete globally. The doctrine persists through institutional inertia and historical framing ('India must protect domestic industry') even though the functional justification has largely atrophied. Theater ratio reflects the gap between the stated development purpose and the actual rent-extraction function.
constraint_indexing:constraint_classification(indian_tariff_protectionism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, Indian tariff protectionism is embedded in a larger system of trade interdependence that both enables and extracts from India. Global supply chains coordinate production across borders (coordination function); tariffs enable domestic value capture but also fragment supply chains (extraction function). The constraint operates bidirectionally: India's tariffs extract from consumers and importers while India itself faces tariff barriers in export markets. No escape from the system; participation is required for development.
constraint_indexing:constraint_classification(indian_tariff_protectionism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_tariff_protectionism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_tariff_protectionism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_tariff_protectionism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_tariff_protectionism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_tariff_protectionism, TR),
    TR >= 0.70.

:- end_tests(indian_tariff_protectionism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The tariff regime protects domestic manufacturers from global competition, enabling them to charge prices above world market levels. The extraction is quantifiable: the difference between Indian domestic prices and global prices for protected goods represents the consumer cost of protection. However, the extraction is not maximal (would require ≥0.66 for snare gate) because: (a) some protection translates to legitimate development benefits (domestic capacity, employment, supply chain building), (b) tariff rates vary across sectors and have declined modestly over 75 years, and (c) alternative mechanisms exist (WTO negotiations, FTAs) that allow partial exit. The measurement trajectory shows extractiveness rising from 0.35 in 1950 (when protection was newly needed for genuine infant industry development) to 0.58 in 2025 (when protected sectors are mature but tariffs persist). This indicates progressive degradation from coordination to extraction. Suppression (0.65): Moderate-high. Tariffs are legally enforced through customs authorities and import licensing; consumers have no escape route and cannot organize politically against diffuse costs (tariff cost is spread across population, benefit is concentrated in protected manufacturers). But suppression is not total (≥0.60 for snare gate): regional trade agreements reduce tariff rates on schedule, some consumers can substitute domestically-produced goods, and liberalization pressure exists from export-dependent sectors. Theater ratio (0.58): Moderate. The infant industry justification, once functionally necessary (1947-1960), has become substantially performative (2000-2025). However, theater ratio is not dominant (would require ≥0.70 for piton gate): some genuine coordination function persists (protected sectors do employ millions, do build domestic capacity), so the theater is not pure inertia. The gap reflects that the primary justification (development through protection) is stated but the actual function has shifted toward rent maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a fundamental perspectival divide between powerless trapped agents (consumers, import-dependent manufacturers) who see extraction (snare) and institutional agents with arbitrage capacity (Ministry, protected manufacturers with export options) who see coordination (rope). The gap reflects real structural differences: tariff costs are highly salient to consumers (distributed across millions, large per-capita impact) but politically diffuse (difficult to organize), while benefits are concentrated in protected manufacturers (large per-firm impact, high political organization). The Ministry occupies an arbitrage position: theoretically able to change tariffs through WTO or bilateral negotiations but politically locked in by protected manufacturing coalitions. The scaffold perspective (regional trade blocs) introduces a sunset mechanism: SAFTA and FTA commitments create gradual liberalization timelines, but implementation consistently lags (rules of origin complexity, tariff line exemptions persist). The piton perspective (infant industry doctrine) reveals that the intellectual justification has atrophied: by 2025, Indian automobile, steel, and pharmaceutical sectors are globally competitive, yet tariff protection persists and has expanded to newer sectors. This gap between justified (infant) and actual (mature) status is the signature of institutional degradation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations feed the directionality derivation chain. Domestic manufacturers are declared beneficiaries (they capture tariff-protected rents); consumers are victims (they bear tariff costs in prices). The Ministry's beneficiary status reflects institutional control of policy and political credibility gains from protectionist coalitions. Import-dependent industries are victims because their input costs rise with upstream tariffs, constraining their competitive position. This structural data enters the sigmoid f(d) function: beneficiaries with arbitrage options (Ministry, global trade partners who can invest locally) get lower d values (0.10-0.25 range), producing lower or negative χ; victims with trapped exit (consumers) get higher d values (0.85-0.95 range), producing high χ. The moderate power level of protected manufacturers reflects that they are neither powerless (they influence policy through lobbying and coalition formation) nor fully institutional (they lack formal state authority), positioning them at d ≈ 0.50-0.60, producing moderate χ values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that Indian tariff protectionism is a genuine tangled rope: it contains real coordination elements (infant industry development, domestic capacity building, employment creation) fused with real extraction elements (higher consumer prices, reduced product variety, protected manufacturers capturing excess rents). The temptation to misclassify arises because: (1) Snare misclassification: focusing only on consumer extraction costs, ignoring that some tariff protection genuinely enabled manufacturing development. (2) Rope misclassification: focusing on the original development function, ignoring that protected sectors are now mature and tariff protection serves primarily extraction rather than development. (3) Piton misclassification: treating the infant industry doctrine as pure theater/inertia, ignoring that some sectors still benefit from protection and employment effects are real. The tangled rope classification holds because beneficiaries and victims both have real structural positions — neither is phantasmal — and the constraint genuinely coordinates (reduces competition-driven displacement of workers) while genuinely extracting (concentrates rents in protected firms, raises consumer prices). The measurement trajectory (extractiveness rising 0.35→0.58 over 75 years) indicates progressive drift toward snare classification: as sectors mature and should graduate, tariff protection increasingly functions as pure extraction rather than coordination. The constraint is not failing to be tangled rope; it is degenerating from tangled rope (1950s, when coordination function was dominant) toward snare (2025, when extraction function dominates).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infant_industry_justification_threshold,
    'At what point does an industry graduate from infant status and lose the theoretical justification for permanent tariff protection?',
    'Comparative analysis of Indian manufacturing sector productivity vs. global competitors; historical tracking of which sectors actually improved competitiveness post-tariff removal; identification of sectors still improving vs. those locked into protected rental extraction',
    'If many sectors have graduated: tariff protection is largely rent-extraction (snare classification from consumer perspective is correct). If sectoral graduation is rare: infant industry function persists, validating tangled rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infant_industry_justification_threshold, empirical, 'Whether tariff-protected sectors have achieved international competitiveness').

omega_variable(
    domestic_cost_pass_through,
    'What proportion of tariff protection translates into higher domestic consumer prices vs. retained as firm profit margins?',
    'Price analysis comparing Indian domestic prices to global market prices for protected goods; firm margin tracking in protected sectors; consumer price sensitivity data',
    'If high pass-through (80%+): extraction from consumers is severe, snare classification strengthened. If low pass-through (20-40%): firms absorb tariff benefits as profit, extraction is structural but suppression of consumers is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_cost_pass_through, empirical, 'Proportion of tariff protection passed to consumers as price increases').

omega_variable(
    alternative_development_mechanism,
    'Would targeted industrial policy (R&D subsidies, labor training, infrastructure investment) achieve development goals with lower consumer cost than uniform tariff protection?',
    'Comparative case studies (South Korea, Taiwan, Vietnam): development outcomes under targeted subsidy regimes vs. broad tariff protection; cost-benefit analysis of alternative policy tools',
    'If targeted tools are more efficient: tariff protectionism is a suboptimal coordination mechanism masking rent-extraction. If tariff protection achieves comparable results: tangled rope classification is justified (genuine coordination function despite extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_development_mechanism, conceptual, 'Whether alternative development tools could replace tariff protection').

omega_variable(
    enforcement_coalitional_fragility,
    'Is the tariff protection regime maintained by a durable coalition of protected manufacturers, labor unions, and state ownership interests, or is it vulnerable to consumer/reformer pressure?',
    'Political economy analysis: coalition composition, strength of competing reform pressures, historical instances of liberalization progress and reversal; organized consumer power measurement',
    'If durable coalition: snare classification is correct (consumer powerlessness is structural). If fragile: scaffold or piton classification is more appropriate (transition mechanism or inertial degradation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_coalitional_fragility, empirical, 'Whether protectionist coalition is durable or vulnerable to reform pressure').

omega_variable(
    reciprocal_tariff_lock_in,
    'To what degree is Indian tariff protectionism locked in by reciprocal tariff barriers in trading partner countries (i.e., India faces similar barriers that make it difficult to export and thus must protect domestically)?',
    'Tariff symmetry analysis: comparison of Indian tariff rates to tariff rates India faces in partner countries; trade deficit/surplus tracking; correlational analysis of domestic protection timing and foreign trade barriers',
    'If high reciprocal lock-in: tariff protectionism is a coordination problem (symmetric prisoners'' dilemma) where all parties would benefit from mutual liberalization but cannot unilaterally exit. Suggests scaffold or rope (depending on negotiation structure) rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_tariff_lock_in, empirical, 'Degree of reciprocal tariff barriers locking in mutual protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_tariff_protectionism, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(itp_tr_t0, indian_tariff_protectionism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(itp_tr_t20, indian_tariff_protectionism, theater_ratio, 20, 0.42).
narrative_ontology:measurement(itp_tr_t40, indian_tariff_protectionism, theater_ratio, 40, 0.55).
narrative_ontology:measurement(itp_tr_t60, indian_tariff_protectionism, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(itp_be_t0, indian_tariff_protectionism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(itp_be_t20, indian_tariff_protectionism, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(itp_be_t40, indian_tariff_protectionism, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(itp_be_t60, indian_tariff_protectionism, base_extractiveness, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_tariff_protectionism, resource_allocation).
narrative_ontology:affects_constraint(indian_tariff_protectionism, indian_consumer_purchasing_power).
narrative_ontology:affects_constraint(indian_tariff_protectionism, southeast_asian_trade_competitiveness).
narrative_ontology:affects_constraint(indian_tariff_protectionism, automotive_sector_supply_chain_fragmentation).

% DUAL FORMULATION NOTE:
% Indian tariff protectionism should be decomposed into sectoral constraint stories at high granularity (automotive tariffs vs. pharmaceutical tariffs vs. textile tariffs) to capture the large variance in ε values across sectors. The aggregate constraint story presented here aggregates across heterogeneous sectoral mechanisms — some sectors (steel, chemicals) have genuinely competitive domestic suppliers and lower ε; others (automotive components, electronics) face dominant import dependency and higher ε. A constraint family analysis would show the upstream infant industry doctrine (pervasive across policy) affecting downstream sectoral constraints with different ε values and different stages of maturation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_tariff_protectionism, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
