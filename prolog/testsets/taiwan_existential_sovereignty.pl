% ============================================================================
% CONSTRAINT STORY: taiwan_existential_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_existential_sovereignty, []).

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
 *   constraint_id: taiwan_existential_sovereignty
 *   human_readable: The Taiwan Existential Sovereignty Constraint
 *   domain: political/economic/technological
 *
 * SUMMARY:
 *   Taiwan's existential sovereignty constraint models the condition of a
 *   democratic state existing under persistent existential military,
 *   political, and economic threat from a rising hegemonic competitor. The
 *   constraint exhibits all six Deferential Realism types from different
 *   structural positions, making it a diagnostic exemplar for how
 *   geopolitical coercion operates. The same structural phenomenon — Taiwan's
 *   geographic position, technological centrality, and political autonomy —
 *   appears as an immutable law of geopolitical gravity (mountain), a
 *   temporary coordination problem resolvable through integration (scaffold),
 *   a degraded Cold War institutional relic (piton), a coordination mechanism
 *   for the US-led order (rope), a hybrid coordination-extraction mechanism
 *   for the PRC (tangled rope), or pure extraction for the Taiwanese
 *   population (snare), depending on the observer's structural position and
 *   exit options. The constraint's extractiveness has increased from 0.45
 *   (2000) to 0.78 (2025) as PRC military capability accumulated,
 *   semiconductor concentration intensified, and US commitment credibility
 *   faced strategic challenges. Theater ratio decreased from 0.72 to 0.55 as
 *   Cold War institutional frameworks (strategic ambiguity, One-China
 *   consensus) lost functional meaning and gave way to direct military and
 *   economic coercion mechanisms. This shift from high theater to moderate
 *   theater indicates a transition from performative institutional management
 *   to structural coercion.
 *
 * KEY AGENTS:
 *   - Taiwanese Population: Primary victim (powerless/trapped) — faces existential threat with no exit option; survival is conditional on accepting constraints on political autonomy
 *   - People's Republic of China State Apparatus: Primary beneficiary and enforcer (institutional/arbitrage) — derives strategic leverage, military modernization rationale, domestic legitimacy; faces coordination requirements to avoid supply chain collapse
 *   - US Strategic Anchor: Secondary beneficiary (institutional/arbitrage) — benefits from coordination (Taiwan autonomy stabilizes East Asian order); maintains genuine exit options
 *   - Democratic Institutions of Taiwan: Secondary victim (moderate/constrained) — institutional legitimacy and functional autonomy held hostage to threat management
 *   - Global Semiconductor Ecosystem: Tertiary victim (powerful/mobile) — trapped by Taiwan's concentration of advanced chip manufacturing; experiences existential extraction through supply chain vulnerability
 *   - International Rules-Based Order Coalition: Organized actor (organized/constrained) — treats constraint as temporary coordination failure with sunset logic; suppression expected to decline through economic integration
 *   - Cold War Legacy Institutional Framework: Institutional artifact (analytical/analytical) — performative arrangements (One-China consensus, strategic ambiguity) maintained through inertia despite structural misalignment with current reality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_existential_sovereignty, 0.78).
domain_priors:suppression_score(taiwan_existential_sovereignty, 0.82).
domain_priors:theater_ratio(taiwan_existential_sovereignty, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, extractiveness, 0.78).
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_existential_sovereignty, snare).
narrative_ontology:human_readable(taiwan_existential_sovereignty, "The Taiwan Existential Sovereignty Constraint").
narrative_ontology:topic_domain(taiwan_existential_sovereignty, "political/economic/technological").

domain_priors:requires_active_enforcement(taiwan_existential_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_existential_sovereignty, people_republic_of_china_military).
narrative_ontology:constraint_beneficiary(taiwan_existential_sovereignty, global_hegemonic_competitor).
narrative_ontology:constraint_victim(taiwan_existential_sovereignty, taiwanese_population).
narrative_ontology:constraint_victim(taiwan_existential_sovereignty, global_supply_chain_stability).
narrative_ontology:constraint_victim(taiwan_existential_sovereignty, semiconductor_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWANESE POPULATION (SNARE) — Trapped within geographical and political boundaries with no exit option. Faces existential threat apparatus (military encirclement, blockade scenarios, coercive diplomacy) that cannot be escaped through individual or collective action. Maximum suppression: no alternative sovereign state available, no practical migration for 23 million people, no credible third-party enforcer of neutrality. Experiences maximum extractiveness: survival itself becomes conditional on accepting constraints that extract political autonomy, military subordination, or economic concessions.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEMOCRATIC INSTITUTIONS OF TAIWAN (SNARE) — Electoral systems, parliamentary bodies, constitutional frameworks are constrained by existential threat calculus. Coercive pressure shapes policy space: military spending demands, diplomatic isolation, economic pressure on trade partners, technological export restrictions. Constrained exit: some migration of capital/talent possible, but institutions cannot relocate. Extraction mechanism: institutional legitimacy and functional autonomy are held hostage to threat management.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PEOPLE'S REPUBLIC OF CHINA STATE APPARATUS (TANGLED ROPE) — Benefits from constraint: maintains strategic leverage, domestic nationalist legitimacy narrative, military modernization rationale, geopolitical positioning against US. Also faces coordination requirements: cannot fully absorb Taiwan without triggering global supply chain collapse (semiconductor hostage), military costs exceed GDP growth rates, blockade escalation triggers reputational risk. Arbitrage exit: can adjust coercion intensity, negotiate conditional integration scenarios, shift between military and economic pressure. Net extraction runs toward PRC, but genuine coordination function exists (threat management, deterrence of independence movements, positioning in hegemonic competition).
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: US STRATEGIC ANCHOR (ROPE) — Experiences constraint as pure coordination: Taiwan's continued autonomy stabilizes East Asian order, enables US presence in first island chain, provides alliance signaling mechanism, limits PRC hegemonic expansion. US benefits from coordination without extraction — security guarantees are net-positive for US position without requiring net resource transfer toward Taiwan. Arbitrage exit: can modulate commitment level, shift toward strategic ambiguity, negotiate settlement scenarios. Low suppression from this perspective — US maintains genuine exit options and faces minimal coercive constraint.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL SEMICONDUCTOR ECOSYSTEM (SNARE) — Trapped by Taiwan's concentration of advanced chip manufacturing (TSMC: 92% of 3nm production). Cannot exit this dependency through diversification at relevant timescales (10-20 years). Experiences existential extraction: Taiwan's constraint directly threatens supply chain stability, which extracts geopolitical vulnerability, risk premiums, and investment uncertainty from every device manufacturer, defense sector, and critical infrastructure operator globally. Suppression is extreme for this agent: no alternative fab capacity, Taiwan manufacture cannot be displaced, geopolitical risk cannot be hedged away.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL RULES-BASED ORDER COALITION (SCAFFOLD) — Organized actors (US, Japan, Australia, EU, QUAD) treat Taiwan constraint as temporary coordination failure with sunset logic: the constraint exists as a transitional phenomenon in the process of integrating China into liberal economic order. Suppression exists but is expected to decline: economic interdependence, technological entanglement, and diplomatic institutional building (WTO, trade agreements) are supposed to make coercive constraints obsolete. Exit options are constrained but improving: if integration succeeds, coercion becomes unnecessary; if it fails, institutional alternatives (new trade blocs, semiconductor diversification) provide partial exit. Theater ratio is moderate: formal diplomatic engagement, UNCLOS adherence, rules-based language are partly functional (coordination) and partly performative (theater masking unresolved hegemonic competition).
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COLD WAR LEGACY INSTITUTIONAL FRAMEWORK (PITON) — The 1970s One-China consensus, UN Resolution 2758, strategic ambiguity framework are performative institutional arrangements maintained through inertia despite being structurally misaligned with current geopolitical reality. Original function (deferring China integration to future regime) has atrophied. Theater ratio is high: diplomatic ritualism (non-interference doctrine, private/public distinction) persists but has lost functional meaning — the constraint is now driven by direct military capability and economic leverage, not by institutional frameworks. This perspective shows piton classification: structure persists through institutional inertia, but primary function has degraded.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / GEOPOLITICAL GRAVITY (MOUNTAIN) — From a civilizational timescale, the constraint appears to emerge from irreducible geopolitical structure: two great powers competing for hegemony, geographic proximity creating zero-sum competition, population size asymmetries, technological concentration. This perspective risks naturalizing the constraint as immutable law. However, the base properties (extractiveness 0.78, suppression 0.82, theater_ratio 0.55) contradict the mountain classification — the engine will identify this as a false summit, revealing that what appears as structural inevitability is actually a contingent institutional arrangement driven by specific policy choices, military capability accumulation, and technological dependencies that could be redesigned.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_existential_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_existential_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_existential_sovereignty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_existential_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_existential_sovereignty, TR),
    TR >= 0.70.

:- end_tests(taiwan_existential_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The constraint operates through direct threat of military action (naval blockade, amphibious assault scenarios, air superiority denial), economic coercion (trade restrictions, investment pressure on partners), and technological subordination (semiconductor supply contingency planning). The extraction mechanism is multi-layered: PRC derives military-strategic leverage, geopolitical positioning, and hegemonic competition advantage; Taiwanese population experiences extraction of political autonomy and security through conditional threat; global supply chains experience extraction of stability and predictability through concentration risk. The 0.78 value reflects that the constraint's primary function is asymmetric extraction rather than coordination — Taiwan must accept constraints on defense policy, diplomatic expression, and economic policy orientation or face existential threat. Suppression (0.82): Extreme. Taiwanese population has no exit option through migration (23 million people cannot relocate), political change (PRC threat is bipartisan), or defense (military asymmetry is 15:1 in PRC's favor). No third-party enforcer credibly guarantees Taiwan's autonomy against military action. Coercive apparatus is continuously reinforced (hypersonic missiles, amphibious assault capability, air superiority modernization). Theater ratio (0.55): Moderate. Cold War institutional frameworks (strategic ambiguity, One-China consensus, UN Resolution 2758) were highly theatrical — they deferred the Taiwan question through ritualized non-commitment and private/public distinction maintenance. As PRC military capability accumulated and semiconductor concentration intensified, the functional constraint shifted from institutional theater to material coercion. The theater ratio decreased from 0.72 to 0.55 because direct military threat is now the primary constraint mechanism, not institutional performance. However, some theater persists: diplomatic language, UNCLOS adherence norms, and rules-based order rhetoric continue to frame coercion as institutional rather than hegemonic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of Deferential Realism classification from a single set of structural facts. The Taiwanese population experiences pure extraction (Snare) — their survival is conditional on accepting constraints they cannot change. Democratic institutions experience constrained snare/tangled rope (hybrid) — they retain some decision-making authority but within a coercion envelope that shapes outcomes. The PRC experiences tangled rope (hybrid coordination-extraction) — they benefit from strategic leverage but must manage coordination requirements (supply chain stability, military cost limitations, international legitimacy concerns). The US experiences rope (pure coordination) — Taiwan's autonomy serves US strategic interests without requiring net extraction from Taiwan. The global semiconductor ecosystem experiences snare (pure extraction) — they are trapped by Taiwan's concentration and face existential vulnerability they cannot escape. The international rules-based order coalition experiences scaffold (temporary coordination with sunset) — they treat the constraint as a transitional phenomenon that will be resolved through economic integration. The Cold War institutional framework experiences piton (degraded inertial artifact) — the original function (deferring Taiwan question) has become obsolete, and the institutions persist through inertia rather than functional necessity. The civilizational analytical observer risks experiencing mountain (naturalization of geopolitical gravity) — but the structural data contradicts this; the constraint is contingent on specific military capability accumulation, semiconductor concentration, and institutional choices, not immutable laws.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality varies sharply across perspectives, driven by structural position and exit options. Taiwanese population (d ≈ 0.95): Trapped victim with no exit; experiences maximum extractiveness. Democratic institutions (d ≈ 0.85): Constrained actor with degraded autonomy; high extractiveness but some residual decision-making authority. PRC state apparatus (d ≈ 0.15): Beneficiary with arbitrage exit options; low effective extraction because they can modulate coercion intensity and adjust objectives. US strategic anchor (d ≈ 0.10): Beneficiary with arbitrage exit; experiences constraint as pure coordination benefit. Global semiconductor ecosystem (d ≈ 0.90): Trapped mobile actor — paradoxically mobile in other respects but structurally dependent on Taiwan; high effective extraction. International rules-based order coalition (d ≈ 0.55): Moderate actor with constrained but improving exit through economic integration; moderate-high extractiveness. The directionality derivation reflects that power atoms are constraint-relative, not actor-absolute: the US is globally powerful but experiences this constraint as low-extraction coordination; Taiwan is globally weak but experience this constraint as maximum-extraction snare. The PRC is globally rising but experiences this constraint as moderate extraction (tangled rope) rather than pure snare because they have significant exit options (negotiation, settlement, constraint modulation).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL DECOMPOSITION: The constraint resolves mandatrophy by showing that all six types are legitimate readings of the same structural data from different observation points. The snare classification (base classification) is correct for the trapped victim (Taiwan); the tangled rope is correct for the hybrid actor (PRC); the rope is correct for the coordination beneficiary (US); the scaffold is correct for the institutional optimist (rules-based order coalition); the piton is correct for the inertial artifact (Cold War framework); the mountain is a false summit (naturalizing contingency). The analytical resolution is that Taiwan's existential sovereignty constraint is fundamentally a SNARE with TANGLED ROPE coordination problems layered on top. The snare extraction (for Taiwan) is the base mechanism; the tangled rope (for PRC) reflects secondary complications (supply chain management, military cost limitations). The rope, scaffold, and piton perspectives are either beneficiary interpretations (US) or institutional aspirations (rules-based order) that underestimate the primary extraction mechanism's persistence. The mountain perspective is a dangerous false summit that would treat the constraint as inevitable rather than contingent on reversible technological and military choices. The mandatrophy is resolved by recognizing that the constraint's primary function is asymmetric extraction (snare) with secondary coordination complications (tangled rope), and that perspectives seeing rope or scaffold are underestimating the extraction's persistence relative to integration's likely success.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semiconductor_dependency_irreversibility,
    'Is Taiwan''s role as the critical node in global semiconductor production reversible through deliberate diversification, or is it structurally locked through physics, geography, and capital accumulation?',
    'Engineering analysis of fab construction timelines and costs; assessment of alternative manufacturing locations (US, EU, Japan, South Korea) and their physical/economic constraints; historical comparison to past technology transitions and lock-in scenarios',
    'If reversible (10-year timescale): constraint on global supply chain is temporary, and Taiwan''s leverage diminishes as alternatives mature. If locked (30+ year timescale): Taiwan''s concentration in advanced chips becomes permanent structural fact, and extraction mechanism persists regardless of political settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semiconductor_dependency_irreversibility, empirical, 'Whether semiconductor concentration in Taiwan is structurally reversible').

omega_variable(
    prc_integration_regime_trajectory,
    'Is the People''s Republic of China fundamentally committed to pursuing hegemonic competition with the US over Taiwan, or is integration into liberal economic order (with Taiwan autonomy within federated structure) a plausible outcome?',
    'Analysis of PRC leadership statements on ''national rejuvenation'' and ''China dream'' goals; assessment of whether these goals are compatible with liberal order integration; historical precedent analysis of great power accommodation scenarios',
    'If hegemonic competition is primary goal: constraint is structural and will persist regardless of economic integration. If liberal order integration is feasible: scaffold perspective is correct and constraint has sunset clause. Classification shifts from Snare (if competition is irreversible) to Scaffold (if integration is plausible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prc_integration_regime_trajectory, conceptual, 'Whether PRC is pursuing hegemonic competition or liberal order integration').

omega_variable(
    us_commitment_credibility_threshold,
    'What level of military/economic cost would trigger US strategic reassessment and withdrawal of Taiwan security guarantee? Is there a threshold below which US commitment remains credible despite costs?',
    'Analysis of US military doctrine and strategic prioritization; assessment of domestic political tolerance for Taiwan-related military costs; comparison to historical cases of alliance commitment under pressure (NATO, South Korea, Philippines)',
    'If US commitment is credible up to high costs: Taiwan''s rope/scaffold perspectives are valid — external security guarantor exists. If threshold is low: Taiwan''s snare perspective dominates — external security guarantor is unreliable, and constraint persists regardless of US rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_commitment_credibility_threshold, preference, 'Threshold of US military/economic cost tolerance for Taiwan commitment').

omega_variable(
    existential_threat_perception_authenticity,
    'Is the existential threat to Taiwan a genuine structural feature of PRC intent and military capability, or is it partly performative — a threat that serves domestic political purposes but would not be executed under actual circumstances?',
    'Analysis of PRC military doctrine, capability development timelines, and public threat rhetoric; assessment of costs and benefits of actual military action vs. continued coercion; polling of PRC leadership intentions through strategic communications analysis',
    'If threat is authentic and credible: suppression remains high, and snare classification is justified. If threat is partly performative: actual suppression is lower than perceived suppression, and some exit options (negotiated settlement, international mediation) are more viable than Taiwan''s trapped perspective suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_threat_perception_authenticity, empirical, 'Authenticity of existential threat to Taiwan from PRC').

omega_variable(
    technological_resilience_of_liberal_order,
    'Can the liberal economic order and its institutions (supply chain governance, trade rules, technological standards) maintain coherence under Taiwan constraint conditions, or does Taiwan''s centrality to semiconductors force the liberal order to restructure around technological nationalism?',
    'Analysis of supply chain redundancy and resilience mechanisms; assessment of whether liberal trade rules survive geopolitical fragmentation; historical precedent analysis of technology-driven order restructuring',
    'If liberal order maintains coherence: rope and scaffold perspectives remain valid — external institutional support for Taiwan autonomy persists. If order fragments: Taiwan''s constraint becomes embedded in bipolar technological competition, and snare classification intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_resilience_of_liberal_order, empirical, 'Resilience of liberal order under Taiwan constraint conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_existential_sovereignty, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taiwan_tr_t0, taiwan_existential_sovereignty, theater_ratio, 0, 0.72).
narrative_ontology:measurement(taiwan_tr_t10, taiwan_existential_sovereignty, theater_ratio, 10, 0.62).
narrative_ontology:measurement(taiwan_tr_t20, taiwan_existential_sovereignty, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(taiwan_be_t0, taiwan_existential_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(taiwan_be_t10, taiwan_existential_sovereignty, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(taiwan_be_t20, taiwan_existential_sovereignty, base_extractiveness, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_existential_sovereignty, global_infrastructure).
narrative_ontology:affects_constraint(taiwan_existential_sovereignty, semiconductor_supply_concentration).
narrative_ontology:affects_constraint(taiwan_existential_sovereignty, us_china_hegemonic_competition).
narrative_ontology:affects_constraint(taiwan_existential_sovereignty, indo_pacific_security_order).

% DUAL FORMULATION NOTE:
% Taiwan's existential sovereignty constraint is downstream of two structural conditions: (1) semiconductor technological concentration in Taiwan (ε ≈ 0.42, Tangled Rope in some perspectives), which makes Taiwan globally indispensable; (2) US-China hegemonic competition (ε ≈ 0.55, Tangled Rope/Snare hybrid), which structures Taiwan as a strategic prize. These upstream constraints are decomposed into separate stories. The sovereignty constraint is the emergent effect of their intersection: Taiwan becomes a constraint precisely because it is simultaneously technologically critical and geopolitically contested. The three constraints are structurally linked: changes in semiconductor diversification or hegemonic competition intensity directly affect Taiwan's sovereignty constraint leverage and extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_existential_sovereignty, powerful, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
