% ============================================================================
% CONSTRAINT STORY: taiwan_semiconductor_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_semiconductor_dependency, []).

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
 *   constraint_id: taiwan_semiconductor_dependency
 *   human_readable: Taiwan Semiconductor Dependency Constraint
 *   domain: geopolitical_economic
 *
 * SUMMARY:
 *   Taiwan's dominance in advanced semiconductor manufacturing creates a
 *   structural extraction constraint that operates simultaneously as pure
 *   coordination (chip supply), economic dependency (global reliance on
 *   TSMC), geopolitical coercion (military threat), and identity lock
 *   (Taiwan's self-conception as tech leader). The constraint is
 *   fundamentally a Tangled Rope at the system level: genuine coordination
 *   function (global chip supply) overlaid with asymmetric extraction
 *   (Taiwan's vulnerability to military and economic coercion). From
 *   different structural positions, the same constraint appears as a Snare
 *   (global supply chain trapped by single source), a Piton (performative
 *   neutrality masking state direction), a false Mountain (naturalizing
 *   historical contingency), and an identity-locked immobility (Taiwan locked
 *   in to semiconductor leadership). The extractiveness has increased from
 *   0.42 to 0.58 over the measurement interval as geopolitical tensions have
 *   intensified Taiwan's vulnerability and the US has intervened more
 *   directly in TSMC operations. Theater ratio has increased modestly (0.28
 *   to 0.38) as the constraint's performative elements (Taiwan's neutrality,
 *   TSMC's independence) have become more strained under US government
 *   pressure.
 *
 * KEY AGENTS:
 *   - Global Electronics Supply Chain: Primary victim (powerless/trapped) — locked into TSMC dependency; faces allocation scarcity and geopolitical risk
 *   - Taiwan Government and Identity: Primary victim (moderate/identity_locked) — structurally mobile but identity-fused with semiconductor leadership; faces existential military and economic coercion
 *   - TSMC Corporation: Institutional beneficiary (institutional/arbitrage) — captures pricing power and allocation control; maintains performative neutrality under state pressure
 *   - United States Government: Powerful beneficiary (powerful/constrained) — gains tech leadership and military advantage but becomes dependent on Taiwan for advanced chips; enforces constraint through CHIP Act and export controls
 *   - China: Institutional victim (institutional/constrained) — needs advanced chips but cannot access them; invests in alternatives and exerts military/economic pressure
 *   - Allied Democracies: Organized actors (organized/constrained) — coordinate through supply diversification while managing geopolitical fragility; bear costs of building alternative capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_semiconductor_dependency, 0.58).
domain_priors:suppression_score(taiwan_semiconductor_dependency, 0.72).
domain_priors:theater_ratio(taiwan_semiconductor_dependency, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_semiconductor_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(taiwan_semiconductor_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(taiwan_semiconductor_dependency, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_semiconductor_dependency, tangled_rope).
narrative_ontology:human_readable(taiwan_semiconductor_dependency, "Taiwan Semiconductor Dependency Constraint").
narrative_ontology:topic_domain(taiwan_semiconductor_dependency, "geopolitical_economic").

domain_priors:requires_active_enforcement(taiwan_semiconductor_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_semiconductor_dependency, taiwan_economy).
narrative_ontology:constraint_beneficiary(taiwan_semiconductor_dependency, tsmc_shareholders).
narrative_ontology:constraint_beneficiary(taiwan_semiconductor_dependency, advanced_chip_consumers).
narrative_ontology:constraint_victim(taiwan_semiconductor_dependency, global_supply_resilience).
narrative_ontology:constraint_victim(taiwan_semiconductor_dependency, china_us_stability).
narrative_ontology:constraint_victim(taiwan_semiconductor_dependency, manufacturing_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SUPPLY CHAIN (SNARE) — Device manufacturers, automotive OEMs, and electronics companies cannot exit Taiwan's semiconductor chokepoint without massive retooling costs spanning decades. Trapped by single-source dependency with geopolitical risks. Zero alternatives at the leading-edge nodes. Maximum suppression: no viable exit path, extraction through pricing power and allocation scarcity.
constraint_indexing:constraint_classification(taiwan_semiconductor_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNITED STATES (TANGLED ROPE) — Benefits from Taiwan's cheap advanced chips for military and civilian technology leadership; coordinates with Taiwan on security and tech standards. Also experiences extraction: dependent on Taiwan for chips containing classified military technologies. High suppression due to geopolitical risk and military vulnerability. Enforces TSMC fab placement and export controls to manage the constraint.
constraint_indexing:constraint_classification(taiwan_semiconductor_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TAIWAN (SNARE WITH IDENTITY LOCK) — Trapped by economic dependence on semiconductor exports; cannot diversify without decades of reinvestment. Identity-locked: Taiwan's self-conception as a technology leader and democratic exception in the region is fused with TSMC's dominance. Structurally mobile (could develop other sectors) but identity-locked in tech leadership. Faces existential extraction: military threats, geopolitical coercion, asymmetric dependence on US protection. Suppression is extreme: constrained by military vulnerability to China, limited by small population and geography.
constraint_indexing:constraint_classification(taiwan_semiconductor_dependency, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 4: TSMC CORPORATE (PITON) — Operates as a neutral foundry claiming no allegiance to any state; the neutrality framing is largely performative theater as geopolitical pressures mount. TSMC's formal business coordination function (foundry services) persists but is increasingly subordinated to US and Taiwan government directives. Theater ratio reflects performative neutrality masking state-directed outcomes. The constraint's functional coordination (efficient chip production) is degraded by geopolitical interference.
constraint_indexing:constraint_classification(taiwan_semiconductor_dependency, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CHINA (ROPE) — From China's structural position, Taiwan's semiconductor dominance is a coordination problem: China needs advanced chips for civilian and military technology, and Taiwan's position creates information asymmetry and pricing power. China experiences the constraint as pure coordination — it is working to develop alternative supply chains and leverage Taiwan through economic interdependence. Lower suppression from China's perspective because it has exit options (investing in alternatives, leveraging ASML, developing indigenous capability).
constraint_indexing:constraint_classification(taiwan_semiconductor_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALLIED DEMOCRACIES (TANGLED ROPE) — Coordinates through CHIP Act investments, foundry partnerships, and supply diversification; also extracts value from Taiwan's low-cost edge and tech leadership. Constrained by need to maintain Taiwan's independence while managing China relations. Benefits from strategic alliance with Taiwan; bears cost of geopolitical fragility. Building alternative fabs (Intel in US, Samsung expansion) reduces extraction over time but requires generational investment.
constraint_indexing:constraint_classification(taiwan_semiconductor_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From maximum abstraction, semiconductor fabrication requires extraordinary precision, capital concentration, and skilled workforce — Taiwan's dominance emerges from physics and economics, not policy. This perspective risks naturalizing what is actually a contingent historical outcome (Taiwan's investment in fab capacity during the 1980s-1990s). The engine should flag this as a false summit: the mountain appearance dissolves when observing specific policy, investment, and geopolitical choices.
constraint_indexing:constraint_classification(taiwan_semiconductor_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_semiconductor_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_semiconductor_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_semiconductor_dependency, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_semiconductor_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_semiconductor_dependency, TR),
    TR >= 0.70.

:- end_tests(taiwan_semiconductor_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value through Taiwan's monopoly position on advanced nodes, enabling TSMC to command premium pricing and governments to exercise allocation control. Extraction is not maximal (0.72+) because genuine coordination function exists — global chip supply requires centralized production. The value has increased over 15 years as geopolitical tension has concentrated power in Taiwan's hands and enabled asymmetric pressure. Suppression (0.72): High. Multiple mechanisms: geopolitical risk of military conflict, capital barriers to alternative fabs, technological expertise concentration in Taiwan, and asymmetric military vulnerability. Suppression is not total (not 0.85+) because alternatives are technically possible — they require massive capital and time. Theater ratio (0.38): Moderate. TSMC maintains public stance as neutral foundry, but US government pressure (subsidies, fab location mandates, export controls) increasingly directs outcomes. Theater is lower than many institutional constraints because the coordination function is genuine — chip production really does require centralization. Increase over time reflects growing performative gap between claimed independence and actual state direction.
 *
 * PERSPECTIVAL GAP:
 *   The gap between supply-chain perspective (Snare) and beneficiary perspective (Piton) is maximal: one sees pure extraction with no exit, the other sees performative coordination hiding state direction. The gap between Taiwan's self-perception (identity-locked tech leader) and analytical observer's view (contingent historical outcome) reveals how identity-lock works — Taiwan cannot perceive its own trap because the trap IS its identity. The gap between US and China perspectives reflects asymmetric vulnerability: US sees manageable Tangled Rope (benefits exceed costs), while China sees constrained Rope (costs exceed benefits). These gaps are diagnostic — they reveal where the constraint's extraction mechanisms are most effective (supply chain) versus where it is most performative (TSMC neutrality).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's structural position determines directionality (d) and resulting effective extractiveness (χ). Global supply chain participants derive d ≈ 0.95 (trapped, bearing full costs) → high f(d) → high χ. Taiwan derives d ≈ 0.87 (moderate power with identity-lock reducing effective mobility) → high f(d) → high χ, despite being beneficiary in nominal terms. United States derives d ≈ 0.50 (powerful but dependent, symmetric costs and benefits) → moderate f(d) ≈ 0.65 → moderate χ. TSMC derives d ≈ 0.15 (institutional beneficiary, arbitrage available) → low f(d) ≈ -0.01 → low χ. China derives d ≈ 0.60 (constrained victim with exit options) → moderate f(d) ≈ 0.85 → moderate-high χ. The directionality overrides are not needed — structural derivation produces correct d values from the power atoms and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Taiwan's semiconductor dependency is genuinely a Tangled Rope at the system level (coordination + asymmetric extraction) but projects onto all six types from different structural positions. The mandatrophy dissolves when the analysis separates: (1) the coordination function (global chip supply genuinely requires centralized production); (2) the extraction asymmetry (Taiwan's monopoly enables rent extraction); (3) the geopolitical coercion (military threat is the real suppression mechanism, not just economic scarcity); (4) the identity lock (Taiwan's self-concept fused with semiconductor leadership reduces perceived alternatives). The false mountain (analytical/civilizational) must be flagged: the constraint looks inevitable only from positions that have not internalized the contingency of Taiwan's historical investment and geopolitical protection. The engine should detect this as a false summit because the structural base (base_properties) shows genuine coordination function and active enforcement, not natural law emergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_coercion_threshold,
    'At what point does military threat to Taiwan transition from background suppression to direct extraction mechanism?',
    'Monitoring of military posture intensity, coercive rhetoric, and actual blockade/conflict events; correlation with TSMC production decisions and Taiwan political outcomes',
    'If threshold crossed: constraint transitions from Snare to pure coercive extraction (Military Snare subtype). Taiwan''s identity-lock becomes irrelevant to classification — structural trap dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_coercion_threshold, empirical, 'Military coercion threshold in Taiwan semiconductor extraction').

omega_variable(
    alternative_fab_sufficiency,
    'When will Intel, Samsung, and other non-TSMC fabs reach equivalent advanced-node parity, and how will this change the extractiveness of Taiwan dependency?',
    'Technical capability tracking of non-TSMC advanced nodes; historical comparison to previous technology transitions (x86 to ARM, etc.); supply chain resilience modeling',
    'If parity achieved in 5-10 years: extractiveness drops sharply from 0.58 to ~0.25, constraint shifts from Snare/Tangled Rope to Rope. If parity is unachievable: extractiveness increases toward 0.75, constraint becomes pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fab_sufficiency, empirical, 'Whether alternative fabs can achieve advanced-node parity').

omega_variable(
    taiwan_identity_lock_robustness,
    'Is Taiwan''s identity-lock to semiconductor leadership structural (would persist post-conflict) or contingent (would dissolve if military threat materialize)?',
    'Empirical test: examine historical precedent of defeated or subordinated tech-leading regions (e.g., East Germany, USSR, pre-unification Korea). Does identity-lock persist through conflict or collapse?',
    'If structural: Taiwan''s classification remains Snare-with-identity-lock even under military threat. If contingent: Taiwan''s identity-lock dissolves under military pressure, leaving pure economic trap (higher suppression, higher extractiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taiwan_identity_lock_robustness, conceptual, 'Robustness of Taiwan''s semiconductor-leadership identity lock').

omega_variable(
    us_credibility_in_protection,
    'Does Taiwan''s dependence on US military protection constitute a second extraction mechanism embedded within the semiconductor dependency?',
    'Analysis of US defense budget allocation to Taiwan, quid pro quo political expectations, and military aid conditionality; comparison to historical precedent of US ally relationships',
    'If second mechanism confirmed: Taiwan experiences dual extraction (economic via TSMC + military/political via US protection dependence). Suppression increases; Taiwan''s structural position becomes Snare embedded in larger Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_credibility_in_protection, empirical, 'US military protection as secondary extraction mechanism').

omega_variable(
    china_indigenous_capability_timeline,
    'What is the realistic timeline for China to achieve indigenous advanced-node fab capability independent of external supply chains?',
    'Engineering feasibility analysis, capital requirements, skilled workforce availability, materials science bottlenecks; comparison to historical tech catch-up timelines (Japan, South Korea, Taiwan)',
    'If timeline < 10 years: China''s exit option becomes real, constraint shifts from Rope to constrained for China. If timeline > 30 years: China remains trapped, extractiveness of Taiwan dependency increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(china_indigenous_capability_timeline, empirical, 'Timeline for China to achieve indigenous advanced-node capability').

omega_variable(
    supply_resilience_vs_efficiency_tradeoff,
    'Is geographic diversification of semiconductor manufacturing fundamentally incompatible with the cost and efficiency that current global demand requires?',
    'Economic modeling of multi-fab redundancy vs. single-source optimization; historical comparison to just-in-time manufacturing transitions (automotive, electronics); empirical test via CHIPS Act investments',
    'If fundamentally incompatible: alternative fabs will always be more expensive, constraint is permanent. If compatible: supply resilience can be built within cost parity, enabling soft exit from Taiwan dependency within 15-20 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_resilience_vs_efficiency_tradeoff, preference, 'Tradeoff between supply resilience and manufacturing efficiency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_semiconductor_dependency, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taiwan_semi_tr_t0, taiwan_semiconductor_dependency, theater_ratio, 0, 0.28).
narrative_ontology:measurement(taiwan_semi_tr_t5, taiwan_semiconductor_dependency, theater_ratio, 5, 0.33).
narrative_ontology:measurement(taiwan_semi_tr_t10, taiwan_semiconductor_dependency, theater_ratio, 10, 0.38).
narrative_ontology:measurement(taiwan_semi_tr_t15, taiwan_semiconductor_dependency, theater_ratio, 15, 0.42).

% Extraction over time
narrative_ontology:measurement(taiwan_semi_be_t0, taiwan_semiconductor_dependency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(taiwan_semi_be_t5, taiwan_semiconductor_dependency, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(taiwan_semi_be_t10, taiwan_semiconductor_dependency, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(taiwan_semi_be_t15, taiwan_semiconductor_dependency, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_semiconductor_dependency, global_infrastructure).
narrative_ontology:affects_constraint(taiwan_semiconductor_dependency, us_china_tech_competition).
narrative_ontology:affects_constraint(taiwan_semiconductor_dependency, geopolitical_supply_chain_fragility).
narrative_ontology:affects_constraint(taiwan_semiconductor_dependency, taiwan_military_vulnerability).

% DUAL FORMULATION NOTE:
% Taiwan's semiconductor dependency decomposes into three structurally distinct constraints: (1) manufacturing concentration (technical/economic ε ≈ 0.35, Rope); (2) geopolitical coercion (military/political ε ≈ 0.72, Snare); (3) identity lock (psychological/institutional ε ≈ 0.50, Tangled Rope). This story integrates all three as a unified system constraint. Upstream: US-China tech competition and Taiwan's postwar development path. Downstream: military vulnerability and supply chain fragility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
