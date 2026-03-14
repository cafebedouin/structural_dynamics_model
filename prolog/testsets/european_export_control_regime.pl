% ============================================================================
% CONSTRAINT STORY: european_export_control_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_european_export_control_regime, []).

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
 *   constraint_id: european_export_control_regime
 *   human_readable: European Export Control Regime
 *   domain: geopolitical/trade/security
 *
 * SUMMARY:
 *   The European Union's export control regime constrains the sale of
 *   dual-use technologies and strategic goods to non-EU actors to prevent
 *   military advantage leakage and maintain strategic autonomy. The regime
 *   emerged as a Cold War security coordination mechanism but has evolved
 *   into a hybrid constraint that solves a genuine collective action problem
 *   (preventing technology diffusion to strategic competitors) while
 *   extracting efficiency costs from exporters and creating asymmetric burden
 *   on small firms. The constraint exemplifies Tangled Rope classification
 *   because it requires active enforcement to maintain coordination, benefits
 *   primary actors (EU security apparatus and large firms with arbitrage
 *   options), and imposes costs on both victims (SME exporters trapped by
 *   compliance) and secondary beneficiaries (multinational firms whose
 *   competitive advantage is protected by regulatory barriers that exclude
 *   smaller competitors). The theater ratio (0.58) reflects that a
 *   significant portion of the regime is performative: many control
 *   categories reflect legacy threat assessments from the 1980s that no
 *   longer apply, and enforcement focuses on administrative compliance
 *   theater rather than verification of actual strategic risk. The
 *   extractiveness has increased over the interval (0.35 → 0.52) as
 *   compliance requirements have proliferated without corresponding reduction
 *   in controlled product categories despite changed geopolitical conditions.
 *
 * KEY AGENTS:
 *   - Small-Medium Technology Exporters: Primary victim (powerless/trapped) — face 6-12 month licensing delays, market access denial, no viable exit from European market dependence
 *   - Mid-Tier Defense Contractors: Secondary actor (moderate/constrained) — experience extraction through compliance overhead but benefit from competitor exclusion; constrained by supply chain dependencies
 *   - Large Multinational Technology Firms: Net beneficiary (powerful/arbitrage) — can relocate production, negotiate exemptions, extract advantage from regulatory barriers that exclude smaller competitors
 *   - EU Security Apparatus: Institutional beneficiary (institutional/arbitrage) — experiences constraint as coordination mechanism for maintaining collective security posture; controls member state extraction through standardized criteria
 *   - Global Supply Chain: Systemic victim (powerless/trapped) — bears extraction through efficiency losses, inventory redundancy, sourcing complexity; no alternative pathways available
 *   - Post-Conflict Reconstruction Coalition: Organized actors (organized/constrained) — see regime as temporary scaffold with security-threat-conditional sunset; constrained by EU funding dependencies
 *   - EU Member States (Legacy Perspective): Institutional inertia (institutional/arbitrage) — maintain regime through path dependence despite erosion of original Cold War threat justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(european_export_control_regime, 0.52).
domain_priors:suppression_score(european_export_control_regime, 0.65).
domain_priors:theater_ratio(european_export_control_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(european_export_control_regime, extractiveness, 0.52).
narrative_ontology:constraint_metric(european_export_control_regime, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(european_export_control_regime, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(european_export_control_regime, tangled_rope).
narrative_ontology:human_readable(european_export_control_regime, "European Export Control Regime").
narrative_ontology:topic_domain(european_export_control_regime, "geopolitical/trade/security").

domain_priors:requires_active_enforcement(european_export_control_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(european_export_control_regime, european_security_alliance).
narrative_ontology:constraint_beneficiary(european_export_control_regime, domestic_manufacturing_interests).
narrative_ontology:constraint_victim(european_export_control_regime, technology_exporters).
narrative_ontology:constraint_victim(european_export_control_regime, global_supply_chain_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL-MEDIUM TECHNOLOGY EXPORTER (SNARE) — Trapped by regulatory compliance costs, licensing delays, and market access barriers. Cannot exit the regime or relocate compliance infrastructure without abandoning European market position. Bears full extraction through licensing delays (6-12 month cycles), denied market access, competitive disadvantage against non-EU competitors with faster time-to-market. No alternatives available; exit is structural impossibility due to market dependence.
constraint_indexing:constraint_classification(european_export_control_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER DEFENSE CONTRACTOR (TANGLED ROPE) — Constrained by compliance requirements but also benefits from export control protections (prevents competitors from accessing restricted markets, maintains technical advantage through controlled information flow). Experiences extraction through bureaucratic overhead and export denials, but also coordination benefit through standardized technical criteria and predictable regulatory environment. Constrained exit due to career, supply chain, and partnership dependencies within the regime.
constraint_indexing:constraint_classification(european_export_control_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EU SECURITY APPARATUS (ROPE) — Benefits from export controls as coordination mechanism for maintaining collective security posture and preventing military advantage leakage to strategic competitors. Experiences the constraint as solving a coordination problem: how do member states collectively manage technology diffusion while maintaining internal EU trade? The apparatus has arbitrage options (can apply controls selectively, adjust enforcement intensity, negotiate bilateral exceptions). Net beneficiary through maintained strategic advantage.
constraint_indexing:constraint_classification(european_export_control_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: LARGE MULTINATIONAL TECHNOLOGY FIRM (TANGLED ROPE) — Powerful actor with arbitrage options: can relocate production to non-EU jurisdictions, establish subsidiary licensing structures, negotiate with governments for exemptions. Experiences extraction through compliance costs and sales restrictions, but also benefits from regime stability and reduced competition from smaller exporters who cannot navigate regulatory complexity. Extraction runs asymmetrically toward smaller competitors; large firms have sufficient power to extract benefit from the regime's existence.
constraint_indexing:constraint_classification(european_export_control_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-CONFLICT RECONSTRUCTION COALITION (SCAFFOLD) — Organized actors (humanitarian groups, development NGOs, EU reconstruction programs) see export controls as a temporary coordination mechanism with a sunset: the regime is justified during active security threats but should sunset as geopolitical stabilization occurs. The regime has a functional sunset clause embedded in security threat assessments — controls should ease as regional stability improves. Theater ratio is moderate; the controls are performative in peacetime but functional during acute security crises. Constrained exit due to humanitarian sector dependencies on EU funding.
constraint_indexing:constraint_classification(european_export_control_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: HISTORICAL INSTITUTIONAL INERTIA (PITON) — From a long-term institutional perspective, the regime persists through legacy momentum from Cold War-era security frameworks. Many specific control categories reflect 1980s threat assessments that no longer apply; the regime's functional core has atrophied while administrative theater has proliferated (license reviews, compliance certifications, statistical reporting). The regime has arbitrage options for institutional actors (governments can waive controls, renegotiate baselines) but continues at full stringency due to institutional inertia — path dependence on inherited frameworks rather than contemporary security calculation. Theater ratio (0.58) reflects significant performative content: many controls persist despite questionable security justification.
constraint_indexing:constraint_classification(european_export_control_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a systemic analytical view, the regime is a hybrid coordination-extraction mechanism that solves a genuine coordination problem (preventing strategic military technology diffusion) while extracting from smaller exporters and efficiency from global supply chains. The constraint is neither purely extractive (security benefits are real) nor purely coordinative (asymmetric extraction toward weaker actors is real). Classification as Tangled Rope reflects this hybrid: coordination function is present but extraction is distributed asymmetrically. The regime requires active enforcement to maintain the coordination function.
constraint_indexing:constraint_classification(european_export_control_regime, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(european_export_control_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(european_export_control_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(european_export_control_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(european_export_control_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(european_export_control_regime, TR),
    TR >= 0.70.

:- end_tests(european_export_control_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The regime imposes compliance costs (licensing delays averaging 6-12 months, administrative overhead, market access denial) that disproportionately burden exporters without corresponding security benefits for all constrained products. The 0.35 → 0.52 trajectory reflects accumulation of legacy control categories that persist despite reduced threat justification. However, extractiveness does not reach snare threshold (0.66+) because some extraction is justified by genuine security coordination benefits. Suppression (0.65): Moderately high. Barriers to exit include regulatory compliance requirements, licensing dependencies, market access concentration in EU, and competitive disadvantage for exporters who attempt to navigate dual-supply-chain strategies. SMEs face particularly high suppression (fixed compliance costs are proportionally larger). Larger firms have some arbitrage options (subsidiary structures, non-EU production), reducing their experienced suppression. Theater ratio (0.58): Moderate-high, and increasing. Significant administrative theater persists around license reviews and compliance certifications; many controlled products pose minimal contemporary security risk but remain on control lists due to legacy categorization. License approval often depends on bureaucratic timing rather than substantive security assessment. However, the regime is not pure theater (0.70+) because security concerns about strategic technology diffusion are genuine and the coordination function is real.
 *
 * PERSPECTIVAL GAP:
 *   The regime produces sharp perspectival divergence across institutional power levels. The EU security apparatus sees Rope (pure coordination — how do we maintain collective security posture?). Large multinationals see net benefit from Tangled Rope (extraction toward competitors is benefit for them; coordination protects their market position). SME exporters see Snare (extraction with no coordination benefit for them; licensing delays are pure cost). The post-conflict coalition sees Scaffold (temporary coordination with sunset when threat decreases). The historical/civilizational perspective sees Piton (legacy framework persisting through inertia). The analytical observer sees Tangled Rope (genuine coordination + asymmetric extraction). The perspectival gap reveals that 'security' framing obscures significant extraction toward smaller exporters and consolidates advantage for larger actors who can navigate complexity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent power level and exit options. SMEs (powerless/trapped) have d ≈ 0.95 — maximum target status, no exit capacity. Mid-tier contractors (moderate/constrained) have d ≈ 0.65 — victim status with some exit options. Large multinationals (powerful/arbitrage) have d ≈ 0.20 — beneficiary status with full exit options (production relocation, subsidiary licensing). EU security apparatus (institutional/arbitrage) has d ≈ 0.05 — primary beneficiary, controls regime parameters. The sigmoid f(d) function translates these d values into experienced chi: SMEs experience high effective extraction despite moderate base extractiveness due to high d; large firms experience low or negative effective extraction despite same base extractiveness due to low d. The asymmetry is structural: identical regulatory constraint, vastly different extraction magnitude depending on power/exit.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The regime resolves mandatrophy by declaring genuine coordination benefits (security alliance maintenance, technology diffusion prevention) sufficient to justify Tangled Rope classification over pure Snare, while simultaneously acknowledging that extraction is asymmetrically distributed toward powerless agents with no arbitrage options. The classification prevents mislabeling the regime as pure coordination (Rope) by noting the requirements for active enforcement and the victim declarations. It prevents mislabeling as pure extraction (Snare) by acknowledging the real coordination function that benefits the EU security alliance. The Tangled Rope designation is appropriate because both dimensions are structural: the regime genuinely solves a coordination problem AND genuinely extracts from trapped exporters. The mandatrophy is resolved not by choosing one interpretation but by documenting both simultaneously and measuring their asymmetry across agent perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_threat_threshold_ambiguity,
    'What level of geopolitical threat justifies the extraction costs imposed by export controls?',
    'Quantitative security impact assessment: correlation between export control stringency and measurable strategic threat reduction vs. measured economic harm to exporters',
    'If threat is high and extraction justified: classification shifts toward Rope (coordination dominates extraction). If threat is overstated: classification shifts toward Snare (extraction dominates coordination). Current assessment assumes moderate threat justification, supporting Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_threat_threshold_ambiguity, empirical, 'Threat level assessment for justifying extraction costs').

omega_variable(
    competitive_advantage_leakage_prevention,
    'Do export controls meaningfully prevent strategic competitor access to critical technologies, or do they primarily create market advantage for large EU firms?',
    'Technology transfer tracking: assessment of whether restricted technologies reach strategic competitors through non-EU production facilities, subsidiary licensing structures, or knowledge diffusion pathways',
    'If controls effectively prevent leakage: coordination function is justified, Tangled Rope classification sustained. If leakage continues via workarounds: controls are theater with extraction toward smaller firms (classification shifts toward Snare for exporters).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_advantage_leakage_prevention, empirical, 'Effectiveness of export controls in preventing strategic technology diffusion').

omega_variable(
    supply_chain_fragmentation_cost,
    'What portion of supply chain inefficiency attributable to export controls is justified by security benefits vs. represents pure extraction rent?',
    'Supply chain cost analysis: measurement of delays, compliance overhead, inventory holding costs, and sourcing redundancy required by control regimes vs. quantified strategic advantage',
    'If justified portion > 60%: supports Tangled Rope. If justified portion < 40%: suggests extraction dominates (Snare for supply chain actors). Current uncertainty supports omega classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_fragmentation_cost, empirical, 'Cost allocation between justified security measures and extractive overhead').

omega_variable(
    asymmetric_burden_on_small_exporters,
    'Does the regime''s fixed compliance cost structure extract disproportionately from small-medium enterprises?',
    'Comparative regulatory burden analysis: compliance cost as percentage of revenue for SMEs vs. large multinationals; differential market access outcomes',
    'If burden is proportional: supports Tangled Rope (extraction is symmetric). If burden is highly regressive: supports Snare classification for SME perspective (fixed costs overwhelm small firms). Evidence suggests significant asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_burden_on_small_exporters, empirical, 'Regressive impact of export control compliance on small exporters').

omega_variable(
    geopolitical_sunset_credibility,
    'Is the post-conflict reconstruction coalition''s scaffold perspective empirically grounded? Have export controls ever meaningfully sunset after security threats abate?',
    'Historical analysis of export control regime relaxation: tracking which controls were lifted after Cold War, which remain despite threat reduction, pattern analysis of sunset vs. persistence',
    'If controls sunset reliably: scaffold perspective is structural, sunset clause is real. If controls persist despite threat reduction: sunset is aspirational theater (classification shifts toward Piton for long-term institutional perspective). Historical evidence suggests poor sunset track record.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_sunset_credibility, empirical, 'Historical credibility of export control regime sunsets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(european_export_control_regime, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eecr_tr_t0, european_export_control_regime, theater_ratio, 0, 0.42).
narrative_ontology:measurement(eecr_tr_t10, european_export_control_regime, theater_ratio, 10, 0.52).
narrative_ontology:measurement(eecr_tr_t20, european_export_control_regime, theater_ratio, 20, 0.58).
narrative_ontology:measurement(eecr_tr_t5, european_export_control_regime, theater_ratio, 5, 0.47).

% Extraction over time
narrative_ontology:measurement(eecr_be_t0, european_export_control_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eecr_be_t10, european_export_control_regime, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(eecr_be_t20, european_export_control_regime, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(eecr_be_t5, european_export_control_regime, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(european_export_control_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(european_export_control_regime, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(european_export_control_regime, us_china_technology_decoupling).
narrative_ontology:affects_constraint(european_export_control_regime, eu_strategic_autonomy_framework).

% DUAL FORMULATION NOTE:
% European export control regime operates at the intersection of three constraint families: (1) EU-internal coordination of security posture (enforcement mechanism), (2) US-EU technology policy coupling (affects decoupling trajectory), (3) supply chain efficiency vs. security tradeoffs. Each family member has distinct ε value reflecting measurement basis. The regime story models the regime itself (ε=0.52, Tangled Rope); upstream constraints (semiconductor concentration, decoupling dynamics) have higher extractiveness; downstream constraints (supply chain fragmentation, strategic autonomy constraints) are affected by regime parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(european_export_control_regime, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
