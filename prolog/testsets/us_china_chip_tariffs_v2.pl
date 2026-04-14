% ============================================================================
% CONSTRAINT STORY: us_china_chip_tariffs_v2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_chip_tariffs_v2, []).

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
 *   constraint_id: us_china_chip_tariffs_v2
 *   human_readable: US Tariffs on Chinese High-Tech Goods (2024)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The US tariff regime on Chinese high-tech goods (50% on semiconductors by
 *   2025, with phased escalation) represents a hybrid coordination-extraction
 *   mechanism justified on national security grounds. The constraint exhibits
 *   markedly different structural properties depending on the observer's
 *   position. US domestic semiconductor manufacturers benefit from tariff
 *   protection and CHIPS Act subsidies, experiencing the regime as
 *   coordination for reshoring and supply chain resilience. US consumers and
 *   downstream electronics manufacturers bear the extraction costs through
 *   price increases and supply constraints, experiencing it as a snare or
 *   tangled rope respectively. Global semiconductor suppliers (TSMC, Samsung,
 *   Chinese fabs) face constrained exit options under extraterritorial
 *   enforcement. The tariff regime combines genuine coordination functions
 *   (forcing supply chain diversification, reducing Taiwan concentration
 *   risk) with asymmetric extraction (consumers bear costs that subsidize
 *   domestic producers). The theater ratio reflects the gap between the
 *   national security justification (broad enough to encompass industrial
 *   policy) and the actual mechanism (tariff-based protection). The
 *   constraint's extractiveness has increased over the interval from 0.35
 *   (initial announcement, limited enforcement) to 0.58 (full enforcement,
 *   supply disruptions, price passthrough).
 *
 * KEY AGENTS:
 *   - US Domestic Semiconductor Manufacturers: Primary beneficiary (institutional/arbitrage) — gain tariff protection, CHIPS Act subsidies, forced reshoring of production. Can exit by relocating to allied countries but tariff structure incentivizes staying.
 *   - US Electronics Consumers: Primary victim (powerless/trapped) — bear tariff cost passthrough; cannot exit without losing access to affordable devices. No alternative suppliers available domestically.
 *   - US Downstream Manufacturers (appliances, phones, computers): Secondary victim (moderate/constrained) — face higher input costs for chips, constrained by limited domestic alternatives. Some benefit from long-term supply stability promises but extraction is immediate.
 *   - Chinese Semiconductor Exporters: Strategic victim (powerful/constrained) — organized companies but cannot exit US market without catastrophic loss; face extraterritorial enforcement (licensing, design tool controls, talent restrictions).
 *   - Global Supply Chain Diversification Coalition: Organized actors (organized/constrained) — multinationals and consortia see tariffs as temporary forcing mechanism for geographic redundancy. Constrained by immediate costs but see sunset path via CHIPS Act horizon.
 *   - WTO/Traditional Trade Law System: Institutional actor (institutional/arbitrage) — formal rules persist but functionally degraded; national security exceptions render the constraint regime unilateral and outside traditional dispute resolution.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing Taiwan concentration as immutable when it is partly contingent on policy choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_chip_tariffs_v2, 0.58).
domain_priors:suppression_score(us_china_chip_tariffs_v2, 0.68).
domain_priors:theater_ratio(us_china_chip_tariffs_v2, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_chip_tariffs_v2, tangled_rope).
narrative_ontology:human_readable(us_china_chip_tariffs_v2, "US Tariffs on Chinese High-Tech Goods (2024)").
narrative_ontology:topic_domain(us_china_chip_tariffs_v2, "economic/political").

domain_priors:requires_active_enforcement(us_china_chip_tariffs_v2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, us_domestic_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, us_chipmaking_equipment_suppliers).
narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, us_government_tariff_revenue).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, us_electronics_consumers).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, us_downstream_manufacturers).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, chinese_semiconductor_exporters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: US CONSUMER (SNARE) — Cannot exit the tariff regime without abandoning access to low-cost electronics. Trapped by geography and legal jurisdiction. Bears full cost of tariff pass-through (smartphone prices, computer costs, appliance affordability). No alternatives available domestically; switching costs prohibitive. Maximum extraction.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: US DOWNSTREAM MANUFACTURER (TANGLED ROPE) — Constrained by tariff costs on imported components and constrained domestic supply availability. Benefits from domestic semiconductor availability promises and long-term reshoring incentives (subsidies, tax breaks). Extraction is real (higher COGS) but mixed with coordination benefits (supply chain stability, long-term domestic capacity). Constrained exit — cannot easily relocate supply chains or production.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US DOMESTIC CHIPMAKER (ROPE) — Primary beneficiary with high arbitrage options. Gains from tariff protection (excludes low-cost competitors), subsidies (CHIPS Act funding), and increased US government procurement commitments. Experiences constraint as pure coordination: tariff regime enables market consolidation and pricing power. Can exit by relocating production to lower-cost jurisdictions, but tariff structure incentivizes staying. Net beneficiary.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SUPPLY CHAIN DIVERSIFICATION (SCAFFOLD) — Organized multinational firms and industry consortia see tariffs as a temporary forcing mechanism for supply chain reshoring and regionalization. Extraction is constrained by the explicit policy sunset: tariffs are coupled with CHIPS Act sunset provisions and Taiwan-Taiwan US alliance mechanisms that have time horizons (15-20 year subsidies). Coalition sees constraint as having a genuine exit path via geographic redundancy and allied-nation partnerships. Low effective extraction because organization and time-bounded policy create plausible pathway.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WTO/TRADE LAW FRAMEWORK (PITON) — The tariff regime is formally justified via national security exceptions to WTO rules (IEEPA), but the underlying mechanism is substantially performative: 'national security' is broad enough to encompass economic competitiveness, geopolitical advantage, and industrial policy indistinguishable from protectionism. The traditional multilateral trade system persists in rhetorical form (WTO dispute procedures ongoing) but is functionally degraded — US unilateralism has eliminated the constraint's force, leaving the rules as theater. High theater ratio reflects the gap between rule-based trade governance and actual state behavior.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CHINESE CHIPMAKERS (SNARE) — Organized but constrained by US extraterritorial enforcement (licensing controls on equipment, advanced nodes blocked, design tool access restricted). High power domestically but cannot exit US market without catastrophic loss of revenue and technology access. Suppression is extreme: OFAC licensing regime, EDA tool export controls, talent restrictions. Extraction takes form of forced technology transfer to US-allied competitors (TSMC, Samsung) and market share loss. Not powerless agents (organized companies) but strategic constraints equivalent to trappage.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TECH INEVITABILITY (MOUNTAIN) — From a civilizational view, semiconductor supply chain concentration in Taiwan is an immutable constraint of physics and economics: advanced chip fabrication requires extreme technical precision, capital intensity, and geographic specificity that cannot be easily replicated. This perspective naturalizes the tariff regime as a necessary response to an inherent fragility of global supply. However, structural data contradicts the mountain classification — Taiwan's dominance is partly contingent on policy choices (China's restrictions, US subsidies, Taiwan's historical tech investments) not immutable physical law.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_chip_tariffs_v2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_chip_tariffs_v2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_chip_tariffs_v2, TR),
    TR >= 0.70.

:- end_tests(us_china_chip_tariffs_v2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. The tariff regime extracts real economic value from consumers and downstream manufacturers ($80-150B annually estimated passthrough) while transferring it to domestic producers and government revenue. The extraction is not total (consumers have some substitution options, CHIPS Act subsidies partially offset costs) and is justified by stated coordination goals (supply resilience). Suppression (0.68): High. Enforcement is multi-modal: tariff border controls, extraterritorial licensing (OFAC), design tool export controls, talent restrictions. Chinese exporters have limited options to evade. Consumers have no practical alternatives. The suppression mechanism is comprehensive but not absolute — smuggling, tariff workarounds (assembly in Mexico/Vietnam), and technological workarounds exist at high cost. Theater ratio (0.62): Moderately high. The national security justification is substantially performative — economic competitiveness and industrial policy goals are bundled under 'national security' broadly construed. Traditional trade law dispute procedures continue but have been overridden by unilateral action. The rise from 0.48 to 0.62 reflects increasing gap between rhetorical commitment to 'rules-based trade' and actual unilateral tariff enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and reveals fundamental disagreement about the constraint's function. Domestic chipmakers and US government see rope: coordination mechanism for reshoring and supply resilience. Downstream manufacturers see tangled rope: costs offset by long-term benefits. Consumers see snare: pure extraction with no countervailing benefit. Chinese exporters see snare: forced extraction disguised as security policy. The supply chain coalition sees scaffold: temporary extraction justified by sunset. The WTO system sees degraded piton: rules exist but are functionally overridden. The analytical observer risks mountain: naturalizing Taiwan concentration as immutable. This perspectival range is characteristic of hybrid extraction mechanisms that blend real coordination benefits with asymmetric cost allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by its structural relationship to the extraction flow. US domestic chipmakers are beneficiaries with arbitrage options (can exit to Taiwan/Korea but incentives favor staying) → low d → negative experienced extraction (they see coordination). US consumers are victims with no exit options (trapped) → high d → high experienced extraction. Chinese exporters are organized but constrained by extraterritorial enforcement (cannot exit without collapse) → high d modulated upward by enforcement power → very high experienced extraction. Downstream manufacturers are victims with some mobility (can relocate supply chains, seek alternatives) → moderate d → moderate extraction. The organized supply-chain coalition has agency and sees time-bounded policy → moderate d → moderate extraction that declines over policy horizon. The WTO system is institutional but constrained by US power and policy override → moderate d but with theater masking.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is resolved by decomposing the tariff regime into its constituent functions: (1) COORDINATION: genuine supply chain diversification and Taiwan concentration reduction — real collective action problem with valid solution. (2) EXTRACTION: asymmetric cost allocation where consumers and Chinese exporters bear costs while domestic producers capture benefits — classic transfer mechanism. The constraint is tangled rope because both functions are present, both are structural (not accidental), and the enforcement mechanism supports both simultaneously. If the regime were pure coordination (rope), it would not require suppression mechanisms targeting Chinese exporters (licensing, talent controls, design tool restrictions). If it were pure extraction (snare), it would not be coupled with supply chain diversification goals and time-bounded CHIPS Act subsidies. The classification avoids both mislabeling (as pure coordination ignoring extraction) and over-generalization (as pure extraction ignoring real supply resilience gains). The rising extractiveness and theater ratio over the interval (0.35→0.58 for ε, 0.48→0.62 for theater) indicate creeping rent-seeking layered onto the original coordination mechanism — classic Goodhart drift where enforcement expands beyond the original policy goal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_capacity_achievability,
    'Can US domestic semiconductor manufacturing achieve cost-competitive parity with TSMC within the 20-year subsidy horizon, or will tariff protection become permanent?',
    'Comparative cost analysis: US fab operating costs vs Taiwan fab costs including all externalities; process node yield data; energy and labor cost trajectories; CHIPS Act subsidy adequacy assessments',
    'If achievable: scaffold sunset is real, tariffs eventually phase out. If not: tariffs become piton (permanent theater) or snare (permanent extraction). Classification shifts from tangled_rope to snare for consumers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_capacity_achievability, empirical, 'Whether US can achieve cost-competitive semiconductor manufacturing').

omega_variable(
    allied_chip_access_sufficiency,
    'Do Taiwan, South Korea, and EU partnerships provide reliable alternative chip access sufficient to bypass Chinese sourcing, or is global supply integration so deep that alternatives cannot fully substitute?',
    'Supply chain mapping: alternative fab capacity, qualification timelines, product node availability; geopolitical stability assessment of Taiwan/Korea/EU as reliable suppliers under US duress',
    'If sufficient: US downstream manufacturers'' exit options improve (mobile becomes real option), reducing their experienced extraction. If not: constraint tightens into snare for downstream. Tangled rope becomes snare for multiple perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allied_chip_access_sufficiency, empirical, 'Whether allied chip partnerships can substitute for Chinese supply').

omega_variable(
    consumer_price_elasticity_substitution,
    'Do US consumers reduce electronics consumption sufficiently (price elasticity) to offset tariff cost passthrough, or do they absorb costs through reduced discretionary spending or credit expansion?',
    'Consumer behavior data: device replacement cycles, spending diversion to non-tech categories, credit card debt trends, cross-national price comparison studies',
    'If elasticity high: tariff extraction is partially self-limiting (less stuff imported). If elasticity low: consumers absorb full cost, snare deepens. Consumer classification confidence increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_price_elasticity_substitution, empirical, 'Whether US consumer price elasticity limits tariff extraction').

omega_variable(
    geopolitical_taiwan_dependency,
    'Does the tariff regime increase or decrease structural US vulnerability to Taiwan supply disruption by forcing temporary chip shortages that reveal dependency?',
    'Supply shock scenario analysis; TSMC contingency planning documents; US Department of Defense critical supply assessments; Taiwan political stability forecasts',
    'If increases vulnerability: tariff regime is security theater (piton), extraction justified by false premise. If decreases: scaffold logic is real. If neutral: tariffs are pure transfer (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_taiwan_dependency, empirical, 'Whether tariffs increase US Taiwan supply dependency').

omega_variable(
    chinese_retaliatory_capacity,
    'Can China impose symmetric or asymmetric tariff costs on US agricultural, aerospace, or automotive exports sufficient to create political coalition for tariff reduction?',
    'Tariff reciprocity analysis; political pressure mapping; agricultural state legislative response; US export dependency statistics by sector',
    'If high capacity: suppression weakens (US farmers/aerospace lobby pressure), tangled rope may shift toward more balanced rope. If low: suppression persists, snare dynamics deepen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chinese_retaliatory_capacity, empirical, 'Whether China can impose retaliatory tariff costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_chip_tariffs_v2, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tariff_tr_t0, us_china_chip_tariffs_v2, theater_ratio, 0, 0.48).
narrative_ontology:measurement(tariff_tr_t3, us_china_chip_tariffs_v2, theater_ratio, 3, 0.55).
narrative_ontology:measurement(tariff_tr_t6, us_china_chip_tariffs_v2, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(tariff_be_t0, us_china_chip_tariffs_v2, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tariff_be_t3, us_china_chip_tariffs_v2, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(tariff_be_t6, us_china_chip_tariffs_v2, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_chip_tariffs_v2, resource_allocation).
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, taiwan_semiconductor_concentration).
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, us_manufacturing_reshoring).
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, china_advanced_chip_design_constraint).

% DUAL FORMULATION NOTE:
% The tariff regime decomposes into three interdependent constraint stories: (1) Taiwan concentration risk (ε=0.15, Mountain from national security view; ε=0.42, Tangled Rope from economic efficiency view); (2) US manufacturing reshoring (ε=0.35, Scaffold with CHIPS Act sunset); (3) China advanced chip design (ε=0.55, Snare from Chinese exporter view). The tariff regime is the enforcement mechanism that couples these constraints. Each story captures distinct structural elements; the tariff regime is the network node linking them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_chip_tariffs_v2, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
