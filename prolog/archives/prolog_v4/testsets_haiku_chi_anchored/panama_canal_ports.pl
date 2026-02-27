% ============================================================================
% CONSTRAINT STORY: panama_canal_ports
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_panama_canal_ports, []).

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
 *   constraint_id: panama_canal_ports
 *   human_readable: Panama Canal Port Control
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The Panama Canal port control constraint represents a hybrid geopolitical
 *   competition between the United States and China over chokepoint
 *   infrastructure critical to global commerce and hemispheric security. Both
 *   powers have invested in port terminals along the canal corridor, creating
 *   overlapping influence structures that nominally serve shipping efficiency
 *   but functionally serve great-power strategic positioning. Panama occupies
 *   the structural trap position: it controls the canal physically but cannot
 *   exclude either power without economic devastation and faces escalating
 *   geopolitical pressure. The extractiveness (0.58) reflects the asymmetry
 *   between Panama's nominal sovereignty and its constrained agency;
 *   suppression (0.68) reflects barriers to genuine neutrality and costs of
 *   resistance; theater (0.62) reflects diplomatic performance of 'neutral
 *   administration' masking strategic competition. The constraint exhibits
 *   all six types from different perspectives because it conflates three
 *   structurally distinct claims: (1) canal administration (rope-type
 *   coordination), (2) geopolitical competition (tangled_rope or snare
 *   depending on observer), and (3) regulatory neutrality (scaffold with
 *   sunset clause as alternative routes develop). Panama's government
 *   experiences all three simultaneously.
 *
 * KEY AGENTS:
 *   - Panama Government: Primary victim (moderate/constrained) — controls canal but cannot exit geopolitical competition without economic devastation
 *   - US Strategic Command: Primary beneficiary (institutional/arbitrage) — maintains hemispheric security position; can arbitrage between multiple commitments
 *   - Chinese Belt and Road Initiative: Primary beneficiary (institutional/arbitrage) — expands global logistics network; can arbitrage between alternative gateway ports
 *   - Panama National Sovereignty: Abstract victim (powerless/trapped) — abstract collective good (national autonomy) cannot organize or exit
 *   - Global Shipping Industry: Secondary victim (powerful/mobile) — benefits from competitive pressure on costs but incurs geopolitical risk and supply-chain uncertainty
 *   - International Maritime Regulatory Bodies: Weak coordinator (organized/constrained) — attempt neutral administration of transit; limited enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(panama_canal_ports, 0.58).
domain_priors:suppression_score(panama_canal_ports, 0.68).
domain_priors:theater_ratio(panama_canal_ports, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(panama_canal_ports, extractiveness, 0.58).
narrative_ontology:constraint_metric(panama_canal_ports, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(panama_canal_ports, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(panama_canal_ports, tangled_rope).
narrative_ontology:human_readable(panama_canal_ports, "Panama Canal Port Control").
narrative_ontology:topic_domain(panama_canal_ports, "geopolitical/economic").

domain_priors:requires_active_enforcement(panama_canal_ports).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(panama_canal_ports, united_states_strategic_interests).
narrative_ontology:constraint_beneficiary(panama_canal_ports, chinese_belt_and_road_expansion).
narrative_ontology:constraint_victim(panama_canal_ports, panama_national_sovereignty).
narrative_ontology:constraint_victim(panama_canal_ports, global_shipping_neutrality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PANAMA SOVEREIGNTY (SNARE) — Panama controls the canal physically but not strategically; cannot exit geopolitical competition without economic devastation. Trapped by dependence on port revenues and unable to maintain canal without great-power backing. d≈0.93, f(d)≈1.40, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(panama_canal_ports, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PANAMA GOVERNMENT (TANGLED ROPE) — Coordinates global shipping through canal administration while trapped between US and Chinese pressure. Benefits from port revenues and strategic positioning; bears costs of escalating geopolitical tension and sovereignty erosion. d≈0.72, f(d)≈1.08, σ=0.9 → χ≈0.57.
constraint_indexing:constraint_classification(panama_canal_ports, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US STRATEGIC COMMAND (ROPE) — Experiences port control as coordination of hemispheric security and trade flows. Can arbitrage between multiple security commitments and has exit options through alternative naval bases. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(panama_canal_ports, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINA BELT AND ROAD INITIATIVE (ROPE) — Experiences port control as coordination of global trade infrastructure and logistics networks. Can arbitrage between multiple port locations and has exit options through alternative gateway investments. d≈0.12, f(d)≈0.02, σ=1.2 → χ≈0.01. Net beneficiary but lower effective extraction than from Panama perspective.
constraint_indexing:constraint_classification(panama_canal_ports, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL SHIPPING INDUSTRY (TANGLED ROPE) — Benefits from competitive pressure between US and China on port efficiency and cost reductions; incurs costs from geopolitical risk, potential supply chain disruptions, and increased insurance premiums. Can mobilize to alternative routes (Suez, around Cape) but at significant cost. d≈0.58, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(panama_canal_ports, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: IMO / INTERNATIONAL MARITIME BODIES (SCAFFOLD) — Attempt to maintain neutral governance of canal transit as temporary coordination mechanism. Low extractiveness because regulatory authority is weak; theater_ratio reflects performative international law enforcement. Has sunset clause: as shipping demand grows, alternative routes and technological solutions (Arctic passage, autonomous shipping) create exit pathways. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(panama_canal_ports, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(panama_canal_ports_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(panama_canal_ports, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(panama_canal_ports, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(panama_canal_ports, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(panama_canal_ports_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Both powers extract strategic advantage from port positioning—control over logistics networks, military positioning, and canal administration. Panama bears the costs of geopolitical vulnerability. The extractiveness has risen over time (0.35→0.58) as Chinese investment deepened, shifting from implicit US dominance to explicit competition. Suppression (0.68): High. Significant barriers to Panama's autonomy: economic dependence on canal revenues (15% of GDP), inability to maintain security without great-power backing, diplomatic costs of excluding either power, and escalating military pressure in the region. Theater ratio (0.62): Moderate-high. Both powers perform neutral commercial interests while pursuing strategic objectives. International organizations perform neutral administration while lacking enforcement capacity. The theater has increased as diplomatic language disguises deepening competition. Suppression and extractiveness are both high because the constraint operates by denying Panama exit options while claiming commercial neutrality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals maximum perspectival divergence. The US and China see coordination (Rope) — they are solving legitimate problems of global commerce and regional security. Panama sees extraction (Snare) — trapped in a position where choosing either power isolates it from the other. Global shipping sees mixed effects (Tangled Rope) — competitive pressure reduces costs but creates supply-chain risk. International bodies see a sunset (Scaffold) — alternative routes will eventually reduce Panama's strategic value. The analytical observer risks seeing a natural law (immutable geopolitics) but the structural data reveals this as a contingent institutional arrangement where Panama's exit options are artificially constrained by economic dependence and security architecture. The perspectival gap confirms tangled_rope classification: genuine coordination (canal administration, shipping efficiency) is inseparable from asymmetric extraction (strategic positioning, geopolitical dominance).
 *
 * DIRECTIONALITY LOGIC:
 *   US Strategic Command: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; multiple exit options. Chinese BRI: Beneficiary + arbitrage → d≈0.12, f(d)≈0.02. Net beneficiary; multiple exit options; slightly higher d than US because investment is more recent and less established. Panama Government: Mixed (victim-leaning) + constrained → d≈0.72, f(d)≈1.08. Significant extraction; constrained by economic and security dependence. Panama Sovereignty (abstract): Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction; no exit option. Global Shipping: Beneficiary (competitive pressure) + mobile (can shift routes) → d≈0.58, f(d)≈0.65. Mixed; benefits from competition but incurs geopolitical risk. IMO/Regulators: Weak + constrained → d≈0.42, f(d)≈0.42. Low effective extraction but cannot mobilize enforcement; sunset clause reduces perceived severeness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by disambiguating three separate claims conflated under 'Panama Canal port control': (1) CANAL ADMINISTRATION (Rope): Neutral governance of shipping through a critical waterway—legitimate coordination problem. Both powers claim to support this. (2) GEOPOLITICAL COMPETITION (Tangled Rope/Snare): Strategic positioning, military access, and hemispheric influence—extraction mechanism disguised as commerce. Both powers pursue this. (3) NEUTRALITY REGIME (Scaffold): International law (1977 Torrijos-Carter Treaty) nominally guarantees neutral canal operation—performative; sunset clause exists as alternative routes mature. The system's classification as Tangled Rope is correct because the constraint operationally conflates coordination (shipping efficiency benefits both sides) with extraction (strategic positioning benefits neither side actually benefits Panama). The mandate-trophy (confusion of types) occurs when observers claim 'this is just neutral commerce' (Rope) or 'this is inevitable geopolitics' (natural law Mountain). The structural data (high suppression, rising extractiveness, moderate theater) shows the constraint is a hybrid in which coordination serves as cover for extraction. Panama's trapped agency is the revealing fact: if this were pure coordination (Rope), Panama would have exit options (choose which power to partner with); if this were pure extraction (Snare), suppression would be higher (both powers would openly exclude alternatives). The tangled_rope captures the hybrid structure precisely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_military_necessity_threshold,
    'What level of Chinese port control in Panama triggers US military intervention or coercive retaliation?',
    'Declassified strategic doctrine; historical precedent from Suez Crisis; threshold crossing empirical observation',
    'If threshold low: constraint becomes snare for Panama (US enforces exclusion). If threshold high: tangled_rope persists longer (both powers coexist). If threshold undefined: instability mechanism (game-theoretic spiral).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_military_necessity_threshold, preference, 'Threshold for US military response to Chinese port control').

omega_variable(
    chinese_strategic_value_computation,
    'Is Chinese port investment in Panama aimed at military positioning, economic arbitrage, or both?',
    'Analysis of investment structure, dual-use facility characteristics, PLA Navy deployment patterns, long-term economic ROI models',
    'If primarily military: snare classification for Panama confirmed (geopolitical trap). If primarily economic: tangled_rope persists (competition rather than domination). Misclassification here produces worst strategic errors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chinese_strategic_value_computation, empirical, 'Strategic intent behind Chinese port investments').

omega_variable(
    panama_exit_viability,
    'Can Panama neutralize itself (Costa Rica model) and maintain canal operations without choosing between great powers?',
    'Comparative analysis of Costa Rican neutrality outcomes; Panama economic dependency modeling; alternative power guarantor identification',
    'If viable: Panama''s exit_options upgrade from trapped to constrained (reclassifies snare to tangled_rope from Panama perspective). If not viable: snare classification confirmed (no exit exists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(panama_exit_viability, empirical, 'Whether Panama can achieve functional neutrality').

omega_variable(
    supply_chain_fragmentation_rate,
    'How quickly will global shipping develop redundant routes and reduce Panama Canal dependence?',
    'Forecast of Arctic shipping viability, Cape of Good Hope transit economics, mega-ship expansion timelines, autonomous vessel deployment',
    'If rapid (< 15 years): scaffold classification confirmed (sunset clause is real); constraint loses extraction power. If slow (> 30 years): tangled_rope persists longer; geopolitical competition remains high-stakes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_fragmentation_rate, empirical, 'Rate of supply chain diversification away from Panama Canal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(panama_canal_ports, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcp_tr_t0, panama_canal_ports, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pcp_tr_t10, panama_canal_ports, theater_ratio, 10, 0.55).
narrative_ontology:measurement(pcp_tr_t20, panama_canal_ports, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(pcp_be_t0, panama_canal_ports, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pcp_be_t10, panama_canal_ports, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pcp_be_t20, panama_canal_ports, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(panama_canal_ports, global_infrastructure).
narrative_ontology:affects_constraint(panama_canal_ports, suez_canal_geopolitical_control).
narrative_ontology:affects_constraint(panama_canal_ports, strait_of_malacca_chokepoint).
narrative_ontology:affects_constraint(panama_canal_ports, critical_minerals_supply_chain).

% DUAL FORMULATION NOTE:
% Panama Canal port control is downstream of broader chokepoint infrastructure competition. The constraint shares structural properties with Suez, Malacca, and Arctic passages but has unique features: (1) Panama is physically part of a sovereign state (unlike international straits), (2) historical US hegemony creates path-dependent expectations, (3) Chinese Belt and Road strategy explicitly targets canal-adjacent ports. Each chokepoint has distinct ε values reflecting local geographic and institutional conditions. The network links represent causal contamination: escalation in one chokepoint constrains options in others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(panama_canal_ports, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
