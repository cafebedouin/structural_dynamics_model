% ============================================================================
% CONSTRAINT STORY: semiconductor_fabrication_chokepoint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_fabrication_chokepoint, []).

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
 *   constraint_id: semiconductor_fabrication_chokepoint
 *   human_readable: The geopolitical and capital chokepoint of leading-edge semiconductor manufacturing (e.g., TSMC 2nm node)
 *   domain: technological/geopolitical/economic
 *
 * SUMMARY:
 *   The fabrication of leading-edge semiconductors (sub-3nm nodes) represents
 *   a modern chokepoint where technological, capital, and geopolitical
 *   constraints converge into a structural trap. TSMC controls >90% of
 *   sub-3nm manufacturing capacity, with competitors (Samsung, Intel, SMIC)
 *   years behind on yield and volume. The barrier to entry is extraordinary:
 *   a single advanced fab requires $20-25 billion in capital, 5-7 years of
 *   construction and ramp-up, specialized supply chains (EUV lithography from
 *   ASML, rare materials, equipment from Tokyo Electron/Lam), and proprietary
 *   process knowledge accumulated over decades. No company or country has
 *   successfully built an independent leading-edge fab in the past 15 years.
 *   Simultaneously, global demand for advanced chips is inelastic: AI
 *   accelerators, processors, mobile SoCs all require leading-edge
 *   performance. This creates a structural snare where dependent economies
 *   (US, EU, Japan, South Korea, China) must source from TSMC or face
 *   competitive disadvantage. Taiwan's geopolitical status compounds the
 *   extraction: the constraint is not just technological but also
 *   sovereign-risk dependent. The constraint has intensified over the
 *   interval (2016-2026): extractiveness rose from 0.38 to 0.58 as TSMC's
 *   share consolidated and competitors fell further behind; geopolitical
 *   weaponization increased (US sanctions on Huawei, China's inability to
 *   acquire advanced chips). Theater remains low (35%) because the constraint
 *   is functionally real—TSMC genuinely produces leading-edge chips that no
 *   competitor can replicate. However, multiple organized actors (CHIPS Act,
 *   EU fabs, Japan subsidies, South Korea investments) are explicitly
 *   attempting to build redundancy and break the chokepoint within 15 years
 *   (sunset logic), establishing the scaffold perspective as a real
 *   structural feature alongside the snare.
 *
 * KEY AGENTS:
 *   - TSMC and Taiwan state: Primary beneficiary (institutional/arbitrage) — captures geopolitical leverage, pricing power, and strategic rent from monopoly position
 *   - Global chip-dependent economies (US, EU, Japan, South Korea): Primary victims (organized/constrained) — constrained by inability to quickly build alternative capacity, bear geopolitical and supply-chain risk
 *   - Semiconductor design firms (Apple, NVIDIA, AMD, Qualcomm, Broadcom): Secondary victims (powerful/constrained) — can negotiate but cannot exit; face priority queuing and lead-time uncertainty
 *   - Broader AI/datacenter/smartphone ecosystem: Tertiary victims (powerless/trapped) — depend on chip supply chains they cannot influence; face capacity constraints and price pressure
 *   - Geopolitical coalitions (US, EU, Japan, South Korea) via CHIPS Acts and subsidies: Organized agents (organized/constrained) — attempting to build alternative capacity and reduce Taiwan dependence; see constraint as temporary with sunset
 *   - Emerging competitors (Samsung, Intel, SMIC, Taiwan emerging fabs): Constrained challengers (powerful/constrained) — investing heavily but lagging TSMC by 6-18 months in node maturity and yield
 *   - Equipment vendors (ASML, Tokyo Electron, Lam Research): Institutional gatekeepers (institutional/arbitrage) — former chokepoint holders; leverage has declined as fab makers internalized process knowledge; now depend on fab growth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_fabrication_chokepoint, 0.58).
domain_priors:suppression_score(semiconductor_fabrication_chokepoint, 0.72).
domain_priors:theater_ratio(semiconductor_fabrication_chokepoint, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_fabrication_chokepoint, extractiveness, 0.58).
narrative_ontology:constraint_metric(semiconductor_fabrication_chokepoint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(semiconductor_fabrication_chokepoint, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_fabrication_chokepoint, snare).
narrative_ontology:human_readable(semiconductor_fabrication_chokepoint, "The geopolitical and capital chokepoint of leading-edge semiconductor manufacturing (e.g., TSMC 2nm node)").
narrative_ontology:topic_domain(semiconductor_fabrication_chokepoint, "technological/geopolitical/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_fabrication_chokepoint, tsmc_shareholders).
narrative_ontology:constraint_beneficiary(semiconductor_fabrication_chokepoint, taiwan_state_apparatus).
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, global_chip_dependent_economies).
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, semiconductor_design_firms).
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, device_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT CHIP ECOSYSTEM (SNARE) — AI developers, data center operators, smartphone manufacturers have no exit from TSMC's advanced nodes. Cannot manufacture in-house (capital barrier >$20B, specialized expertise), cannot switch suppliers (TSMC holds >90% of sub-3nm capacity), cannot delay (competitive dynamics punish slowness). Trapped by technological and economic necessity. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.97 (extreme extraction).
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DESIGN FIRMS & INTEGRATORS (SNARE) — Moderate power but structurally constrained. Can in theory design around TSMC's capacity (moving to less-advanced nodes, changing architecture), but this incurs competitive penalty. Lead times stretch 18+ months; priority queuing by TSMC favors large customers (Apple, NVIDIA, Intel). No meaningful alternatives exist (Samsung's N3/N4 lag TSMC's roadmap by 6-18 months; Intel's inability to deliver external customers). d≈0.80, f(d)≈1.18, σ=1.2 → χ≈0.82.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LARGE INTEGRATED CUSTOMERS (SNARE) — Nominally powerful, but leverage is constrained by TSMC's irreplaceability and by geopolitical risk. Apple designs custom processors but depends entirely on TSMC. NVIDIA designs GPUs but manufactures nowhere else. These firms can negotiate better terms (priority access, lower prices) but cannot exit the relationship or credibly threaten to build in-house. They pay the snare toll in form of capacity competition, geopolitical hostage risk, and technology dependency. d≈0.68, f(d)≈0.95, σ=1.2 → χ≈0.70.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: GEOPOLITICAL COALITIONS (TANGLED ROPE) — Organized states view the constraint as mixed coordination + extraction. Coordination function: TSMC provides stable supply of leading-edge chips essential to defense, AI, competitiveness. Extraction function: Taiwan's geopolitical leverage over chip supply creates asymmetry (US cannot credibly diversify to equivalents; therefore must manage Taiwan relations carefully; therefore absorbs geopolitical risk). States are constrained by inability to quickly build domestic capacity but have partial agency: CHIPS Act ($39B), EU Chips Act (€42B), Japan/South Korea subsidies attempt to build alternative fabs. The constraint requires active state enforcement (export controls on chip-making equipment, subsidies, trade negotiations). d≈0.58, f(d)≈0.78, σ=1.2 → χ≈0.54.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TSMC AND TAIWAN STATE (ROPE) — Primary beneficiary. TSMC experiences the constraint as coordination: it solves the problem of coordinating global chip supply, extracting rents through pricing power and priority control. Taiwan state benefits from TSMC as primary revenue source and geopolitical asset. From this perspective, the constraint is not extraction but coordination value — TSMC provides the essential service that no competitor can. TSMC maintains arbitrage (can shift production to other countries; has considered fabs in US/Japan; holds leverage with all customers). d≈0.12, f(d)≈0.10, σ=1.2 → χ≈0.07 (net beneficiary; negative effective extraction).
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DECENTRALIZATION INITIATIVES (SCAFFOLD) — Organized actors (CHIPS Act, EU fabs, Japan/South Korea capacity building) view this as temporary supply crisis with a sunset clause. Goal: reduce TSMC concentration from >90% to <60% sub-3nm capacity within 15 years (2026-2041). Intel, Samsung, SMIC are building capacity; open-source chip design (RISC-V) reduces custom fab dependency. The constraint suppression is deliberate (supply-side constraints intentionally maintained to justify subsidies and create industry rationale), but has a formal sunset. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.35 (moderate, declining extraction).
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: EQUIPMENT VENDORS (PITON) — Historically critical but now degraded. ASML's extreme UV (EUV) lithography monopoly was THE chokepoint (2015-2022); now EUV is commodity-ish and multiple vendors compete. Equipment vendors' former gatekeeper role has attenuated as TSMC and Samsung internalized critical process knowledge. Vendors persist through institutional inertia and because fab buildouts still require massive equipment orders, but their leverage has declined. Theater ratio ≈0.65 (they perform criticality they no longer possess); Chinese sanctions (Huawei) created artificial scarcity that vendors exploited but cannot sustain indefinitely. d≈0.15, f(d)≈0.05, σ=1.0 → χ≈0.03.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From civilizational view, one might argue that semiconductor manufacturing concentration is an inherent law of process complexity and capital intensity. This perspective classifies as mountain. However: the structural data (ε=0.58, suppression=0.72, theater=0.35) contradicts mountain thresholds (ε ≤ 0.25, suppression ≤ 0.05). The apparent inevitability ('you can't make 2nm chips in a garage') naturalizes contingent policy choices (TSMC's IP secrecy, equipment export controls, subsidy allocation). The engine will flag this as a false summit. Manufacturing concentration is real but not immutable.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_fabrication_chokepoint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_fabrication_chokepoint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semiconductor_fabrication_chokepoint, TR),
    TR >= 0.70.

:- end_tests(semiconductor_fabrication_chokepoint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. TSMC extracts economic rent through pricing power (premium pricing for leading-edge nodes), priority control (preference sequencing by customer size and strategic importance), and implicit geopolitical tolls (dependent governments must manage Taiwan relations carefully to maintain access). The 0.38→0.58 trajectory reflects intensified market concentration and geopolitical weaponization. Extractiveness is not higher (0.70+) because: (1) TSMC is not purely predatory—it does provide genuine leading-edge chip production that no competitor can; (2) large customers have some negotiating power; (3) there exists a realistic competitive roadmap (Samsung N3, Intel 4/7, SMIC improvements) that provides modest hope of exit within 10-15 years. Suppression (0.72): High. Multiple barriers prevent exit: (a) capital barrier: $20B+ capital requirement is effectively infinite for non-state actors; (b) knowledge barrier: TSMC's process IP is proprietary and not transferable; (c) supply chain barrier: specialized inputs (EUV lithography) are single-source (ASML) or dual-source (equipment from Tokyo Electron, Lam); (d) time barrier: 5-7 years minimum to reach competitive volume and yield; (e) geopolitical barrier: China cannot access advanced nodes (US sanctions); Europe/Japan/South Korea have not caught up in 15+ years despite billions in investment. Theater ratio (0.35): Low. The constraint is functionally genuine—TSMC produces advanced chips that competitors truly cannot. There is minimal theater: TSMC does not overstate its capabilities, customers understand the supply situation precisely, geopolitical risk is transparent. The modest theater (28%→35% over interval) reflects secondary theater: some performance of manufacturing 'difficulty' to justify premium pricing, some state-level theater (CHIPS Act announcements of future capacity that is not yet online). The constraint claim (snare) is appropriate because the base properties align with snare thresholds: ε=0.58 ≥ 0.46, suppression=0.72 ≥ 0.60, χ values across perspectives exceed 0.66 for most trapped/constrained agents.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival gap. From TSMC's perspective (Rope), the constraint solves the problem of coordinating global chip supply—TSMC is the essential coordination node. From dependent countries' perspectives (Tangled Rope), the constraint is mixed coordination (TSMC provides necessary service) plus extraction (TSMC captures geopolitical rent and supply-chain risk transfer). From design firms' perspectives (Snare), the constraint is pure extraction: they have no meaningful exit, TSMC controls priority, they bear full cost of supply disruption. From the dependent chip ecosystem (powerless economies' perspective), the constraint is catastrophic: there is zero agency, no exit, complete dependency on Taiwan's stability and TSMC's continued operation. From the scaffold perspective (states attempting to build alternatives), the constraint is temporary and remediable via state investment and time. From equipment vendors' perspective (Piton), the constraint is their former criticality—they remember when EUV lithography was the chokepoint; now they are commodity suppliers. From the civilizational analytical observer (false Mountain), the constraint might appear to be an immutable law of physics ('you cannot make 2nm chips cheaply')—but the structural data contradicts this: the constraint is contingent on TSMC's IP secrecy, equipment export controls, and policy choices about subsidies and technology transfer.
 *
 * DIRECTIONALITY LOGIC:
 *   TSMC/Taiwan (Rope beneficiary): d≈0.12, f(d)≈0.10 — Full beneficiary (d→0), arbitrage exit (can shift production). Negative effective extraction (χ≈0.07); they experience the constraint as value creation, not constraint. Dependent economies (Tangled Rope): d≈0.58, f(d)≈0.78 — Moderate victim status (bears geopolitical risk, supply constraint), but organized power and constrained exit (can invest in alternatives, but slowly). Large customers (Apple, NVIDIA—Snare): d≈0.68, f(d)≈0.95 — Clear victims but powerful (can negotiate), constrained exit. Design firms (moderate, Snare): d≈0.80, f(d)≈1.18 — Victims, constrained exit, moderate power; no way to source alternatives. Dependent chip ecosystem (Snare): d≈0.93, f(d)≈1.40 — Full victims, trapped (no alternatives for end users), powerless (cannot organize). Decentralization initiatives (Scaffold): d≈0.45, f(d)≈0.50 — Moderate victim position (constrained by time and capital requirements), but organized actors with agency and a sunset plan. Equipment vendors (Piton): d≈0.15, f(d)≈0.05 — Near-beneficiary position but degraded leverage. Analytical observer (false Mountain): d≈0.72, f(d)≈1.15 — Would-be observer position, but mountain classification fails gates (ε=0.58 > 0.25, suppression=0.72 > 0.05).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLVES MANDATROPHY: The structural data clearly satisfies snare thresholds (ε=0.58 ≥ 0.46, suppression=0.72 ≥ 0.60) and the chi formula produces χ > 0.66 for the primary victim perspectives (dependent ecosystem χ≈0.97, design firms χ≈0.82, large customers χ≈0.70). The classification is resistant to misidentification as Rope or Tangled Rope because: (1) The beneficiary perspective (TSMC) experiences the constraint as coordination with negative χ—they do not perceive extraction, they perceive value creation. The snare classification is about what dependent agents experience, not about TSMC's experience. (2) There is no genuine coordination benefit to the victims—the constraint transfers risk and rents, it does not solve a collective action problem for the dependent ecosystem. If TSMC's nodes were genuinely coordinating chip production across competitors for mutual gain, the constraint would be Rope or Tangled Rope. Instead, TSMC is extracting economic and geopolitical rents from necessity. (3) Suppression is the key signal: the 0.72 suppression score reflects that alternatives (Samsung, Intel, SMIC) exist but are structurally unreachable due to capital/time barriers maintained by TSMC's IP secrecy and competitors' inability to catch up. This is suppression of alternatives, not technical impossibility. (4) The scaffold perspective (decentralization attempts) demonstrates that the snare is remediable—if it were truly a mountain (immutable law of physics), organized state investment could not build alternatives. The fact that CHIPS Act, EU fabs, and Japanese subsidies are explicitly attempting to break the chokepoint within 15 years shows the constraint is political/economic (snare), not physical (mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fab_buildout_substitution,
    'Can Samsung, Intel, and emerging competitors (SMIC, emerging Taiwan capacity) build sufficient sub-3nm capacity within 10-15 years to reduce TSMC''s share from >90% to <70%?',
    'Empirical: Track capital investment, yield curves, time-to-production for announced fabs (Intel Ohio, Samsung Pyeongchang, SMIC Wuhan). Monitor whether Samsung N3/N4 yield targets are met; whether Intel 7/4/20A reach customer maturity; whether SMIC achieves competitive ≤7nm production.',
    'If YES: scaffold perspective confirmed, snare constraint weakens to tangled rope within 15 years, CHIPS Act/subsidies achieve policy goal. If NO: snare classification persists, geopolitical dependence on Taiwan intensifies, risk of supply weaponization increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fab_buildout_substitution, empirical, 'Whether competing fabs can reduce TSMC''s sub-3nm dominance within 15 years').

omega_variable(
    process_node_relevance_degradation,
    'Does architectural innovation (chiplets, AI-specific designs) reduce demand for the absolute most advanced nodes, making TSMC''s 2nm advantage less economically critical than current assumptions?',
    'Market analysis: Measure fraction of shipping chip units at leading edge (2nm, 3nm, 5nm) vs leading-edge-capable (7nm and older). Track whether energy-efficient architectures on older nodes compete effectively with brute-force leading-edge designs. Monitor whether NVIDIA/AMD/Apple maintain node-competitive strategies or shift to node-tolerant designs.',
    'If YES: effective chokepoint weakens (you can build competitive chips on N5, beneficiaries have exit to older nodes), classification moves from snare toward tangled rope. If NO: concentration intensifies, absolute dependency grows, snare persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(process_node_relevance_degradation, empirical, 'Whether architectural innovation reduces dependence on absolute leading-edge nodes').

omega_variable(
    taiwan_physical_vulnerability,
    'Is the geopolitical chokepoint actually Taiwan''s geographic/military vulnerability, such that the constraint cannot be meaningfully decentralized without political change?',
    'Assess whether declared capacity additions in Japan, US, South Korea are genuinely independent or merely TSMC satellites. Analyze whether US/allied governments are willing to tolerate Taiwan-based supply concentration despite military risk, or whether decentralization is existential political requirement. Monitor whether TSMC fabs in Phoenix (Arizona) achieve genuine autonomy or remain dependent on Taiwan process IP.',
    'If Taiwan vulnerability is primary: decentralization becomes geopolitical requirement (not market driven), snare classification persists indefinitely, suppression driven by state actors (not just market structure). If manufacturing concentration can be decoupled from Taiwan risk: scaffold perspective gains credibility, 15-year sunset becomes plausible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taiwan_physical_vulnerability, conceptual, 'Whether the chokepoint is structural (capital intensity) or geopolitical (Taiwan''s vulnerability), and whether they can be decoupled').

omega_variable(
    open_source_chip_viability,
    'Can RISC-V and open-source chip designs achieve price/performance parity with proprietary designs by 2035, enabling lock-in escape through design architecture change rather than fab capacity?',
    'Track RISC-V adoption rates, performance benchmarks, software ecosystem maturity. Monitor whether open-source designs (SiFive, StarFive, others) reach data-center/AI-application readiness. Assess whether ISA switching cost (recompiling software stack, retraining developer ecosystem) is lower than fab capacity switching cost.',
    'If YES: beneficiaries (Apple, NVIDIA, Qualcomm) can escape TSMC lock-in by switching ISA and design ecosystem (long-term, 10+ years). Constraint becomes tangled rope or even scaffold (exit becomes mobile via design change). If NO: ISA lock-in complements fab lock-in; snare constraint deepens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_chip_viability, empirical, 'Whether RISC-V and open-source chips can provide price/performance parity and reduce fab lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_fabrication_chokepoint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semicon_tr_t0, semiconductor_fabrication_chokepoint, theater_ratio, 0, 0.28).
narrative_ontology:measurement(semicon_tr_t5, semiconductor_fabrication_chokepoint, theater_ratio, 5, 0.32).
narrative_ontology:measurement(semicon_tr_t10, semiconductor_fabrication_chokepoint, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(semicon_be_t0, semiconductor_fabrication_chokepoint, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(semicon_be_t5, semiconductor_fabrication_chokepoint, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(semicon_be_t10, semiconductor_fabrication_chokepoint, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_fabrication_chokepoint, global_infrastructure).
narrative_ontology:affects_constraint(semiconductor_fabrication_chokepoint, artificial_intelligence_accelerator_supply).
narrative_ontology:affects_constraint(semiconductor_fabrication_chokepoint, taiwan_geopolitical_risk).
narrative_ontology:affects_constraint(semiconductor_fabrication_chokepoint, us_china_technology_decoupling).
narrative_ontology:affects_constraint(semiconductor_fabrication_chokepoint, semiconductor_equipment_export_controls).

% DUAL FORMULATION NOTE:
% The semiconductor fabrication chokepoint decomposes into at least two structurally distinct constraints: (1) TSMC's manufacturing monopoly at the leading edge (this story, ε=0.58, Snare—capital and knowledge barriers prevent competitor catch-up), and (2) Taiwan's geopolitical vulnerability as a single point of failure (separate story, ε=0.72+, Snare—military/political risk makes the entire constraint hostage-dependent). These are linked but distinct: breaking TSMC's manufacturing dominance (via CHIPS Act decentralization) does not resolve Taiwan's geopolitical risk; conversely, securing Taiwan politically does not reduce TSMC's market concentration if competitors remain years behind. The present story focuses on the manufacturing concentration. A sibling story should address Taiwan geopolitical risk as a separate chokepoint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semiconductor_fabrication_chokepoint, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
