% ============================================================================
% CONSTRAINT STORY: india_semi_mission
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_semi_mission, []).

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
 *   constraint_id: india_semi_mission
 *   human_readable: India Semiconductor Mission 2.0
 *   domain: economic/industrial_policy
 *
 * SUMMARY:
 *   India Semiconductor Mission 2.0 represents a state-coordinated push to
 *   build domestic advanced chip fabrication capacity through subsidies
 *   (₹76,000 crore incentive structure), infrastructure development
 *   (semiconductor fabs, chip design clusters), and skill training. The
 *   constraint exhibits Tangled Rope structure: it provides genuine
 *   coordination (solving the capex and knowledge barriers to entry) while
 *   simultaneously extracting through locked capital, technology transfer
 *   requirements, and subsidy dependency. Different agents perceive it
 *   differently: multinational fabs see arbitrage opportunity (Rope); Indian
 *   manufacturers see coordination and market access (Rope); state
 *   governments see industrial transformation with stranded asset risk
 *   (Tangled Rope); import-dependent manufacturers see input cost inflation
 *   (Snare); ASEAN competitors see temporary competitive pressure with an
 *   implicit sunset (Scaffold); the WTO framework sees violations enforced
 *   performatively (Piton); the analytical observer risks naturalizing as
 *   inevitable what is actually a policy choice (Mountain).
 *
 * KEY AGENTS:
 *   - Domestic Semiconductor Manufacturers: Primary beneficiary (institutional/arbitrage) — capture subsidies, tech transfer, and market protection; can relocate or seek export markets
 *   - Import-Dependent Electronics OEMs: Primary victim (powerless/trapped) — forced to pay premiums for domesticated chips or invest in vertical integration; high switching costs
 *   - State Governments: Secondary beneficiary/victim (organized/constrained) — gain industrial hubs and development narrative but face capex lock-in and stranded asset risk if fabs relocate
 *   - Multinational Semiconductor Corporations: Powerful beneficiary with mobility (powerful/mobile) — arbitrage subsidies and market access but constrained by tech transfer commitments
 *   - ASEAN Competitors: Regional victim (organized/constrained) — face FDI fragmentation and temporary subsidy-driven competition; exit path depends on India execution risk
 *   - Indian Government / Mission Authority: Institutional beneficiary/enforcer (institutional/arbitrage) — drives extraction through subsidy conditionality and tech transfer requirements
 *   - WTO Dispute Resolution System: Institutional observer (institutional/arbitrage) — maintains rule-based facade despite limited actual enforcement of subsidy constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_semi_mission, 0.38).
domain_priors:suppression_score(india_semi_mission, 0.45).
domain_priors:theater_ratio(india_semi_mission, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_semi_mission, extractiveness, 0.38).
narrative_ontology:constraint_metric(india_semi_mission, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(india_semi_mission, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_semi_mission, tangled_rope).
narrative_ontology:human_readable(india_semi_mission, "India Semiconductor Mission 2.0").
narrative_ontology:topic_domain(india_semi_mission, "economic/industrial_policy").

domain_priors:requires_active_enforcement(india_semi_mission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_semi_mission, domestic_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(india_semi_mission, state_governments).
narrative_ontology:constraint_beneficiary(india_semi_mission, technology_transfer_entities).
narrative_ontology:constraint_victim(india_semi_mission, foreign_semiconductor_companies).
narrative_ontology:constraint_victim(india_semi_mission, import_dependent_manufacturers).
narrative_ontology:constraint_victim(india_semi_mission, competing_nations_fabs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPORT-DEPENDENT MANUFACTURERS (SNARE) — Trapped by subsidy structure that favors integrated semiconductor producers over systems manufacturers. Cannot access incentives without vertical integration; switching costs to alternative supply chains are prohibitive. Bears extraction through input cost inflation as domesticated chip production commands price premiums over global alternatives.
constraint_indexing:constraint_classification(india_semi_mission, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC SEMICONDUCTOR MANUFACTURERS (ROPE) — Primary beneficiary with arbitrage exit (can relocate fabs, seek export markets, or access global capital). Experiences constraint as pure coordination: subsidies solve the capex problem, tax incentives enable global competitiveness, and infrastructure bundling reduces market entry friction. Net benefit substantially outweighs cost.
constraint_indexing:constraint_classification(india_semi_mission, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: STATE GOVERNMENTS / INDUSTRIAL POLICY (TANGLED ROPE) — Organized actors with constrained exit (cannot abandon FDI competition without losing development narrative). Experience both coordination benefit (industrial hubs, skill clusters, export ecosystems) and extraction cost (capital lock-in, stranded assets if fabs relocate, political risk of industrial closure). Active enforcement required: state governments must commit supporting infrastructure, environmental clearances, and regulatory predictability.
constraint_indexing:constraint_classification(india_semi_mission, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ASEAN SEMICONDUCTOR ECOSYSTEM (SCAFFOLD) — Organized regional competitors (Vietnam, Thailand, Taiwan IP clusters) face temporary constraint: India's subsidies create FDI fragmentation as multinational fabs evaluate India vs ASEAN comparatively. This constraint has an implicit sunset: if India's fab builds stabilize production within 7-10 years, comparative advantage shifts to operational efficiency, not subsidies. Sunset timing depends on execution risk and geopolitical supply-chain decoupling timelines.
constraint_indexing:constraint_classification(india_semi_mission, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: WTO DISPUTE RESOLUTION FRAMEWORK (PITON) — International trade rules technically constrain subsidies (ASCM Agreement), but enforcement is performative: dispute resolution takes 5-10 years, remedies are trade-retaliation based (not structural), and larger trading blocs (US, EU) use their own industrial policy exemptions. The WTO framework persists through institutional inertia despite minimal functional verification of subsidy legality. Theater ratio reflects the gap between formal constraints and actual enforcement.
constraint_indexing:constraint_classification(india_semi_mission, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTINATIONAL SEMICONDUCTOR CORPORATIONS (TANGLED ROPE) — Powerful actors with high mobility (can invest in India or rival jurisdictions). Experience both extraction and coordination. Extraction: Indian government captures tech transfer commitments, local content requirements, and capex subsidies effectively shift some fab profitability to the state. Coordination: standardized incentive packages, clear regulatory pathways, and FDI clustering reduce project discovery costs. High mobility means chi is moderate — they can exit, but subsidies make India competitive against alternatives.
constraint_indexing:constraint_classification(india_semi_mission, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a global supply-chain physics perspective, some form of state intervention in semiconductor localization is inevitable given the strategic criticality of chips and the technological gap between established fabs and emerging producers. Capital intensity and learning curves create structural barriers that market mechanisms alone cannot overcome in developing economies. However, this naturalization risks obscuring the contingent policy choices (subsidy levels, enforcement mechanisms, sunset clauses) that actually determine whether the constraint functions as coordination or extraction.
constraint_indexing:constraint_classification(india_semi_mission, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_semi_mission_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_semi_mission, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_semi_mission, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(india_semi_mission, TR),
    TR >= 0.70.

:- end_tests(india_semi_mission_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The mission combines genuine coordination (capex financing, knowledge infrastructure) with rent-seeking (tech transfer requirements, subsidy conditionality, market protection). The 0.38 value reflects that roughly 40% of the incentive structure is pure transfer payment (extraction) while 60% is productivity-enhancing coordination. The intermediate value captures the hybrid structure. Suppression (0.45): Moderate. Entry barriers remain high (capex requirements, technical expertise, IP access) but the mission explicitly lowers suppression through infrastructure bundling and skills training. Some suppression persists (regulatory complexity, vendor lock-in, foreign ownership restrictions). Theater ratio (0.58): Moderate-high. The constraint exhibits performative elements: subsidy effectiveness is measured by FAB announcements (input metric) rather than operational efficiency (outcome metric); tech transfer commitments are announced but enforcement is unclear; skill training is counted by enrollment rather than employability; infrastructure hubs are measured by land allocation rather than operational capacity. These are characteristic of early-phase industrial policy theater. The ratio increases over the interval as initial announcements generate media coverage and political visibility independent of actual production.
 *
 * PERSPECTIVAL GAP:
 *   Import-dependent manufacturers (powerless/trapped) experience the constraint as Snare because they cannot exit: integrating upstream into chip manufacturing requires capex they don't have, and importing alternatives face tariff/policy barriers. Domestic manufacturers (institutional/arbitrage) experience Rope because they can access subsidies while maintaining global options. Multinational fabs (powerful/mobile) experience Tangled Rope because subsidies are attractive but tech transfer and local content requirements create friction — they can walk away, making chi moderate. ASEAN competitors (organized/constrained) experience Scaffold because India's subsidy advantage has an implicit sunset: if fabs mature to profitability within 10 years, the subsidy incentive collapses and comparative advantage shifts to operational efficiency and supply chain proximity, where ASEAN has advantages. State governments (organized/constrained) experience Tangled Rope because they capture development benefits but risk stranded assets if fab operators relocate post-subsidy. The WTO framework (institutional/arbitrage) experiences Piton because formal subsidy rules exist but enforcement is performative — dispute cases take years, remedies are retaliation-based, and larger trading blocs use exemptions. The analytical/civilizational observer risks Mountain by naturalizing state intervention as inevitable response to chip criticality, obscuring the contingent policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each agent derive from their structural relationship to the extraction flow. Domestic manufacturers are beneficiaries with arbitrage exit (can access global capital, relocate fabs) → low d → negative chi from their perspective. Import-dependent manufacturers are victims with trapped exit (high switching costs, tariff barriers) → high d → high chi. State governments are semi-beneficiaries with constrained exit (cannot abandon industrial development narrative) → moderate-high d → moderate chi. Multinational fabs are beneficiaries with mobile exit (can invest elsewhere) → low-moderate d → low chi. ASEAN competitors are victims with constrained exit (face FDI diversion) → moderate-high d → moderate chi. The engine derives these values from beneficiary/victim declarations and exit options, producing the perspectival gap where the same constraint appears as Rope to some agents and Snare to others.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination (infrastructure enabling, capex financing, knowledge transfer) from rent-seeking extraction (subsidy conditionality, tech transfer requirements, market protection, input tariffs). The Tangled Rope classification is justified by the simultaneous presence of coordination (it truly lowers barriers) and extraction (it distributes costs asymmetrically). The perspectival gap (Rope vs Snare vs Scaffold vs Piton depending on observer) is the correct reading — there is no single 'true' type. The constraint is not being mislabeled coordination as extraction or vice versa; it genuinely has both functions, and different agents experience the ratio differently based on their structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fab_operational_viability,
    'Will subsidized Indian fabs achieve operational efficiency parity with TSMC/Samsung within the subsidy window, or will they require indefinite support?',
    'Production cost benchmarking; comparison of yield rates, cycle times, and unit economics for 5nm/3nm nodes across Indian vs Taiwan/Korea fabs post-2028',
    'If viability achieved: constraint transitions to Rope (coordination without extraction). If viability fails: constraint becomes permanent Snare (subsidies become extraction mechanism from taxpayers to stranded assets).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fab_operational_viability, empirical, 'Whether Indian fabs reach operational efficiency parity').

omega_variable(
    geopolitical_supply_chain_decoupling,
    'Will Taiwan Strait tensions or US-China decoupling accelerate semiconductor reshoring faster than planned, collapsing the comparative-subsidy arbitrage that justifies India''s mission?',
    'Geopolitical risk indices; timeline of ITAR/EAR restrictions on chip exports; multinational FAB location announcements relative to India mission milestones',
    'If decoupling accelerates dramatically: India''s constraint becomes less extractive (fabs build regardless of subsidy efficiency). If decoupling stalls: India faces subsidy-dependent competition requiring deeper extraction to sustain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_supply_chain_decoupling, empirical, 'Pace of geopolitical supply-chain decoupling').

omega_variable(
    tech_transfer_enforceability,
    'Can India actually enforce IP and process tech transfer commitments from multinational fabs, or will subsidiaries maintain proprietary control through legal subsidiaries and parent-company licensing?',
    'Audit of tech transfer clause enforcement in signed FAB MOUs; patent filing rates from Indian entities using transferred technology; Indian government litigation records against FAB operators for IP violations',
    'If enforced: coordination benefit to Indian ecosystem is real (Rope/Tangled Rope stronger). If unenforceable: transfer becomes performative, constraint becomes pure extraction from state to multinational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tech_transfer_enforceability, empirical, 'Enforceability of tech transfer commitments').

omega_variable(
    subsidy_exit_timeline,
    'What is the implicit or explicit sunset clause for direct subsidies — will they taper post-2030/2035 or persist indefinitely?',
    'Government policy announcements; budget allocation trends; comparative analysis with US CHIPS Act sunset provisions and Taiwan subsidy phase-out timelines',
    'If sunset clause exists and is credible: constraint is Scaffold (temporary support with known exit). If open-ended: constraint is structural Tangled Rope or Snare depending on enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidy_exit_timeline, conceptual, 'Existence and credibility of subsidy sunset timeline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_semi_mission, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(india_semi_tr_t0, india_semi_mission, theater_ratio, 0, 0.42).
narrative_ontology:measurement(india_semi_tr_t4, india_semi_mission, theater_ratio, 4, 0.52).
narrative_ontology:measurement(india_semi_tr_t8, india_semi_mission, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(india_semi_be_t0, india_semi_mission, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(india_semi_be_t4, india_semi_mission, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(india_semi_be_t8, india_semi_mission, base_extractiveness, 8, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_semi_mission, resource_allocation).
narrative_ontology:affects_constraint(india_semi_mission, taiwan_semiconductor_oligopoly).
narrative_ontology:affects_constraint(india_semi_mission, us_chips_act_subsidy_race).
narrative_ontology:affects_constraint(india_semi_mission, asean_fdi_competition).

% DUAL FORMULATION NOTE:
% India Semiconductor Mission 2.0 is downstream of global supply-chain decoupling (Taiwan Strait risks, US-China tech bifurcation) but represents a distinct structural constraint. The mission's extractiveness and coordination function depend on geopolitical acceleration timelines that are exogenous to the mission's internal design. Faster decoupling makes the mission less extractive (fabs build regardless); slower decoupling requires deeper subsidy dependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
