% ============================================================================
% CONSTRAINT STORY: china_advanced_chip_design_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_advanced_chip_design_constraint, []).

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
 *   constraint_id: china_advanced_chip_design_constraint
 *   human_readable: China Advanced Chip Design Constraint
 *   domain: geopolitical/technology/semiconductor
 *
 * SUMMARY:
 *   The constraint on Chinese advanced chip design operates as a coordinated
 *   regime of technology withholding and market access restriction enforced
 *   through export controls, alliance-based exclusion, and talent mobility
 *   barriers. The constraint exhibits a genuine coordination function — it
 *   aligns US and allied interests in maintaining technological leadership,
 *   coordinates supply chain security, and enforces IP protection norms —
 *   alongside substantial extraction dynamics that concentrate technological
 *   advantage and create artificial scarcity rents for beneficiary firms. The
 *   constraint is not a natural law of economics (advanced chips require
 *   access to TSMC) but a contingent policy choice enforced through active
 *   mechanisms: EDA tool export controls (ECCN regulations restricting
 *   Cadence and Synopsys sales), foundry access restrictions (TSMC and
 *   Samsung alliance-based exclusion policies), visa screening (talent
 *   mobility barriers), and IP licensing restrictions (design methodology
 *   withholding). The structural data reveals this as a tangled rope: genuine
 *   coordination function coexists with asymmetric extraction targeting
 *   trapped researchers and constrained firms. The measurement trajectory
 *   shows rising extractiveness and suppression over the 6-year interval
 *   (2018-2024), reflecting escalation in enforcement mechanisms (Biden
 *   administration CHIPS Act, expanded ECCN controls, Quad alliance
 *   coordination). Theater ratio remains low (0.38), indicating that the
 *   extraction mechanism is relatively transparent — the constraint's
 *   enforcement is explicit policy, not performative ritual. The false summit
 *   risk is high: the constraint is frequently presented in discourse as an
 *   inevitable consequence of geography and economics ('TSMC is in Taiwan,
 *   therefore China cannot access cutting-edge chips') when it is actually a
 *   policy choice with identifiable beneficiaries and forecloseability
 *   conditions.
 *
 * KEY AGENTS:
 *   - Chinese Chip Designers: Trapped victims (powerless/trapped) — face coordinated barriers (visa restrictions, EDA tool access controls, foundry access exclusion, IP licensing withholding); no viable exit except career abandonment
 *   - Chinese Semiconductor Companies: Constrained victims (powerless/constrained, organized/constrained) — SMIC, Huawei HiSilicon, others face foundry and tool access restrictions; constrained alternatives (domestic EDA, domestic foundries) are inferior but available; active enforcement via supply chain restrictions
 *   - US Semiconductor Firms: Primary beneficiaries (institutional/arbitrage) — Cadence, Synopsys, Intel, other IDMs capture protected market share, licensing revenue, and technological moat from constraint enforcement
 *   - Western Foundry Alliance: Secondary beneficiary (institutional/arbitrage) — TSMC, Samsung, other advanced-node operators maintain technological leadership and premium pricing via alliance-based exclusion
 *   - Chinese Semiconductor Industry: Organized victim (organized/constrained) — experiences both coordination function (state-coordinated R&D, industry policy) and extraction (technological lag, efficiency penalties); constrained exit via domestic alternatives
 *   - Open-Source Chip Design Community: Mixed actor (moderate/constrained) — benefits from Chinese investment in open-source alternatives but victimized by constraint's fragmentation of global ecosystem
 *   - US Government Strategic Apparatus: Enforcer (organized/institutional) — enforces constraint via export controls, alliance coordination, talent restrictions; experiences tangled rope (genuine coordination alongside geopolitical extraction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_advanced_chip_design_constraint, 0.58).
domain_priors:suppression_score(china_advanced_chip_design_constraint, 0.72).
domain_priors:theater_ratio(china_advanced_chip_design_constraint, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_advanced_chip_design_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(china_advanced_chip_design_constraint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(china_advanced_chip_design_constraint, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_advanced_chip_design_constraint, tangled_rope).
narrative_ontology:human_readable(china_advanced_chip_design_constraint, "China Advanced Chip Design Constraint").
narrative_ontology:topic_domain(china_advanced_chip_design_constraint, "geopolitical/technology/semiconductor").

domain_priors:requires_active_enforcement(china_advanced_chip_design_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_advanced_chip_design_constraint, us_semiconductor_firms).
narrative_ontology:constraint_beneficiary(china_advanced_chip_design_constraint, allied_foundry_operators).
narrative_ontology:constraint_beneficiary(china_advanced_chip_design_constraint, western_eda_vendors).
narrative_ontology:constraint_victim(china_advanced_chip_design_constraint, chinese_chip_designers).
narrative_ontology:constraint_victim(china_advanced_chip_design_constraint, chinese_semiconductor_companies).
narrative_ontology:constraint_victim(china_advanced_chip_design_constraint, global_open_chip_design_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED CHINESE RESEARCHER (SNARE) — Faces multiple coordinated barriers: visa restrictions prevent emigration, EDA tool access requires US compliance certification, foundry access requires alliance membership, IP licensing is withheld by design. No exit option except abandoning career entirely. Maximum experienced extraction. The constraint is structurally designed to make alternatives unavailable.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSTRAINED CHINESE FIRM (SNARE) — Cannot access advanced EDA tools (Cadence, Synopsys restricted via ECCN), cannot use TSMC or Samsung foundries for advanced nodes (US alliance-based restrictions), cannot hire Western-trained talent (visa screening). The constraint's suppression is designed to foreclose alternatives. The firm experiences systematic extraction: margin compression, development delays, technological lag. No viable domestic substitute exists for critical EDA tools or advanced foundries at required performance levels.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CHINESE SEMICONDUCTOR INDUSTRY (TANGLED ROPE) — Experiences genuine coordination function (industry policy, state coordination of R&D, integration with supply chain) alongside asymmetric extraction. The constraint drives investment in domestic EDA alternatives (EDA365, HyperX) and domestic foundries (SMIC, Jiangnan), creating technological redundancy. Active enforcement through policy coordination and import substitution policy. Constrained exit: investment in alternatives is expensive and inferior, but possible. The industry is organized and strategically responding, not purely victimized — but the constraint still extracts via technological lag and efficiency penalties.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: US SEMICONDUCTOR FIRM (ROPE) — Primary beneficiary. Cadence and Synopsys capture licensing revenue from non-restricted markets while excluding Chinese competitors. US foundries (Intel, other IDMs) maintain technological advantage via restricted access to advanced tools and design methodologies. Experiences the constraint as coordination: the industry collectively maintains IP protection and export control compliance. Net beneficiary position with arbitrage options (license to allies, maintain technological moat, capture market share from competitors who cannot operate). Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WESTERN FOUNDRY ALLIANCE (ROPE) — TSMC, Samsung, and other advanced-node operators maintain technological leadership and premium pricing via alliance-based exclusion of Chinese chip designers from advanced nodes. The constraint coordinates their market position and technological advantage. Arbitrage options: license to aligned partners, maintain pricing power. The coordination function is genuine (supply chain integrity, security validation, alliance cohesion) and the beneficiary position is stable.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN CHIP DESIGN COMMUNITY (TANGLED ROPE) — Benefits from open-source EDA tools (OpenROAD, Yosys) and open-source chip designs (RISC-V, open standard cells). The constraint drives investment in open-source alternatives, creating genuine coordination benefits. However, suppression remains high: open-source tools lag commercial tools by 2-3 generations; design complexity for advanced nodes exceeds current open-source capability. Constrained exit: open-source tools are improving but not yet sufficient for cutting-edge applications. Active enforcement reduces resources available for open-source development. The community is both beneficiary (receives investment from Chinese companies and researchers pursuing alternatives) and victim (constraint fragments the global chip design ecosystem, reducing access to talent and reducing collaborative innovation).
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: US GOVERNMENT STRATEGIC APPARATUS (TANGLED ROPE) — Enforces the constraint via EDA tool export controls (ECCN regulations), foundry alliance coordination (Quad, partnership frameworks), talent mobility restrictions (visa screening, security clearance procedures). Genuine coordination function: US industrial policy, technology leadership maintenance, alliance cohesion. Active enforcement: BIS enforcement actions, CFIUS reviews, treaty negotiations. The constraint delivers both coordination benefits (allied technology sharing, aligned R&D incentives) and extraction dynamics (US firms capture protected market share, US government gains geopolitical leverage). The government's experience is tangled: it is enforcing coordination among allies while extracting geopolitical advantage from the resulting technology gap.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some technology differentiation is inherent to geographic distribution of semiconductor manufacturing: advanced chip design requires access to advanced foundries, which are concentrated in Taiwan and South Korea due to capital requirements, expertise clustering, and path dependence. This perspective risks naturalizing what is actually a coordinated institutional arrangement. The constraint is presented as inevitable consequence of technology economics, but the structural data (beneficiary coordination, active enforcement, forecloseability through policy change) reveals this as a false summit.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_advanced_chip_design_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_advanced_chip_design_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_advanced_chip_design_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantial value from trapped and constrained actors through restricted access to critical tools and foundries, but is not maximal extraction because: (1) domestic alternatives exist (inferior but available), (2) the constraint permits non-cutting-edge chip design, (3) some workarounds exist (reverse-engineering, procurement through gray markets). The rising trajectory (0.35 → 0.58 over 6 years) reflects escalating enforcement mechanisms. Suppression (0.72): High and rising. Multiple coordinated barriers foreclose alternatives: EDA tool export controls + foundry alliance exclusion + visa restrictions + IP licensing withholding. No single alternative is viable; only combinations of expensive domestic substitutes approach functionality. Rising trajectory reflects Biden administration policy intensification and quad alliance coordination. Theater ratio (0.38): Low. The constraint's enforcement is relatively transparent — explicit policy rather than performative ritual. ECCN controls are codified in law, foundry access restrictions are explicit alliance agreements, visa screening is formal procedure. The low theater indicates this is not a Piton (degraded ritual) but a functioning extraction regime. Claimed type (Tangled Rope): Justified by genuine coordination function (IP protection, supply chain security, alliance cohesion) coexisting with asymmetric extraction (market share capture, technological moat, geopolitical leverage). Active enforcement is required to maintain the tangled coordination.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries (US firms, foundry alliance) classify the constraint as Rope at biographical timescale because they experience it as legitimate coordination: IP protection, supply chain security, technological incentives for continued innovation. Trapped researchers classify it as Snare at biographical timescale because they experience total exclusion from alternatives. The constrained Chinese industry classifies it as Snare at immediate timescale (no viable alternatives for current products) but Tangled Rope at generational timescale (investment in domestic alternatives is possible but expensive). The US government at immediate timescale sees enforcement activity (Tangled Rope), but at civilizational timescale risks natural law framing ('geographic inevitability'). The analytical observer's mountain classification is a false summit — the constraint is presented as natural geography when it is policy-chosen. The gap reveals that beneficiary-coordinated extraction is frequently naturalized as inevitable rather than contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the constraint's asymmetric benefit structure. Trapped Chinese researchers occupy the maximum-extraction position (d ≈ 0.95, f(d) ≈ 1.42): they bear costs with zero alternatives. Constrained Chinese firms occupy high-extraction position (d ≈ 0.85, f(d) ≈ 1.15): expensive alternatives exist. Organized Chinese industry (d ≈ 0.60, f(d) ≈ 0.85) can invest in alternatives at industrial scale. US firms, foundry operators, and government (d ≈ 0.10-0.20, f(d) ≈ -0.05 to 0.05) experience the constraint as protective and coordinating, not extractive. No directionality overrides are necessary — the beneficiary/victim declarations and exit options fully determine d via the standard derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination function from extraction mechanism. At biographical timescale, the constraint appears as Snare to powerless agents (trapped) and Rope to institutional beneficiaries (arbitrage). At generational timescale, the organized Chinese industry sees Tangled Rope (genuine coordination via state policy alongside extraction via technological gap). At civilizational timescale, the analytical observer risks Mountain (natural law) but the structural data reveals false summit: beneficiaries are identifiable (US firms, foundry alliance, government), enforcement mechanisms are explicit (export controls, visa screening), and alternatives are forecloseably by policy (sanctions reversal, Chinese investment in advanced foundries). The mandatrophy is resolved by recognizing that all classifications are accurate from their respective positions — the constraint genuinely coordinates allied interests (Rope from beneficiary view) while genuinely extracting from trapped agents (Snare from victim view). The tangled_rope classification at the organized Chinese industry and US government perspectives captures the duality: coordination coexists with extraction, and both are structural features of the regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_substitution_feasibility,
    'Can Chinese domestic alternatives (SMIC foundries, EDA365, HyperX) achieve sufficient capability to become viable substitutes for TSMC/Cadence/Synopsys within the next 5-10 years?',
    'Technical benchmarking: compare SMIC advanced-node yield and performance vs TSMC; compare open-source EDA tool capability vs commercial tools; track Chinese chip design performance metrics over time',
    'If substitution is feasible: the constraint''s extraction becomes temporary (scaffold rather than snare). If substitution fails: the constraint''s suppression is confirmed as structural, not policy-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitution_feasibility, empirical, 'Feasibility of Chinese domestic technological substitution').

omega_variable(
    alliance_solidarity_stability,
    'Is the Western foundry and EDA vendor alliance stable, or do individual firms have economic incentives to defect (license advanced tools to China, sell foundry access) if geopolitical conditions change?',
    'Economic analysis of firm-level margin structure; historical precedent in previous technology embargoes (encryption, nuclear materials); game-theoretic modeling of defection incentives under different sanctions regimes',
    'If stable: the constraint persists indefinitely as a coordinated enforcement regime. If unstable: the constraint is vulnerable to defection and may degrade over 5-15 year horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_solidarity_stability, empirical, 'Economic stability of Western alliance-based technology withholding').

omega_variable(
    domestic_eda_scaling_limits,
    'What is the fundamental limit to domestic EDA tool development in an embargo regime? Can a single country maintain EDA tool parity with an integrated global ecosystem?',
    'Historical analysis of EDA tool development timelines; comparison of development resources (engineers, compute, testing infrastructure) required for commercial-grade tools vs embedded-in-embargo alternatives; complexity analysis of EDA tool codebases and their dependencies on external libraries',
    'If scaling is possible: tangled rope classification is accurate. If scaling is fundamentally impossible: the constraint approaches snare for the victim population (trapped without viable alternatives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_eda_scaling_limits, empirical, 'Feasibility of domestic EDA tool ecosystem sustainability').

omega_variable(
    enforcement_mechanism_sustainment,
    'What is the minimum suppression level required to sustain the constraint''s extraction function? If enforcement capacity decays (WTO challenges, US export control authority limitations, Allied defection), at what enforcement level does the constraint revert from tangled_rope to rope?',
    'Scenario analysis: model constraint classification under different enforcement levels (current = 0.72; weakened = 0.50; minimal = 0.30). Identify the floor below which the constraint''s coordination function dominates and extraction becomes negligible.',
    'If floor is high (suppression > 0.50 required): constraint is resilient to moderate weakening. If floor is low: constraint is brittle and vulnerable to erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_sustainment, conceptual, 'Minimum suppression required to sustain extraction function').

omega_variable(
    false_summit_natural_law_framing,
    'Is the constraint presented as inevitable economic geography (''advanced chips require access to TSMC, which is in Taiwan'') rather than as a contingent policy choice? If so, is the natural law framing covering for a beneficiary-coordinated extraction regime?',
    'Discourse analysis: examine US government and industry rhetoric about China chip constraints. Identify cases where geographic/economic inevitability is asserted without acknowledging policy alternatives (e.g., Chinese investment in advanced foundries, technology transfer agreements, sanctions reversal). Compare to explicit policy documents where enforcement mechanisms are revealed.',
    'If natural law framing is detected: the constraint is a false summit (genuine mountain with false appearance). The engine''s FSM signature should flag this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_framing, conceptual, 'False summit detection: is the constraint naturalized as inevitable rather than policy-chosen?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_advanced_chip_design_constraint, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cacd_tr_t0, china_advanced_chip_design_constraint, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cacd_tr_t3, china_advanced_chip_design_constraint, theater_ratio, 3, 0.33).
narrative_ontology:measurement(cacd_tr_t6, china_advanced_chip_design_constraint, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(cacd_be_t0, china_advanced_chip_design_constraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cacd_be_t3, china_advanced_chip_design_constraint, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(cacd_be_t6, china_advanced_chip_design_constraint, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cacd_su_t0, china_advanced_chip_design_constraint, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(cacd_su_t3, china_advanced_chip_design_constraint, suppression_requirement, 3, 0.64).
narrative_ontology:measurement(cacd_su_t6, china_advanced_chip_design_constraint, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_advanced_chip_design_constraint, enforcement_mechanism).
narrative_ontology:affects_constraint(china_advanced_chip_design_constraint, semiconductor_supply_chain_resilience).
narrative_ontology:affects_constraint(china_advanced_chip_design_constraint, open_source_chip_design_fragmentation).
narrative_ontology:affects_constraint(china_advanced_chip_design_constraint, taiwan_strait_technology_dependency).

% DUAL FORMULATION NOTE:
% The advanced chip design constraint should be decomposed into three structurally distinct constraints with different ε values: (1) EDA tool access control (ε ≈ 0.50, Tangled Rope) — coordination function for IP protection; (2) Foundry access exclusion (ε ≈ 0.65, Snare) — pure market access restriction; (3) Talent mobility restrictions (ε ≈ 0.72, Snare) — pure extraction with no coordination function. These are presented here as one constraint for corpus coherence, but domain decomposition would improve resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
