% ============================================================================
% CONSTRAINT STORY: sotu_1991_bush_wmd_proliferation_controls
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1991_bush_wmd_proliferation_controls, []).

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
 *   constraint_id: sotu_1991_bush_wmd_proliferation_controls
 *   human_readable: WMD Proliferation Controls and Iraq Sanctions Regime (1991)
 *   domain: regulatory/geopolitical/security
 *
 * SUMMARY:
 *   The 1991 Bush administration's WMD proliferation controls and Iraq
 *   sanctions regime established a post-war regulatory framework combining
 *   military denial (weapons inspections), economic isolation (comprehensive
 *   sanctions), and regional arms control monitoring to prevent Iraq's
 *   rearmament and forestall a new Middle Eastern arms race. The constraint
 *   operates across multiple nested mechanisms: UNSC resolutions enforcing
 *   inspections, export control regimes restricting weapons and technology
 *   transfers, financing isolation preventing reconstruction, and military
 *   enforcement of no-fly zones and inspection access. This creates a
 *   structural tension between genuine coordination functions (preventing
 *   proliferation) and asymmetric extraction mechanisms (concentrating costs
 *   on Iraq while distributing benefits to security winners). The
 *   constraint's extractiveness rises from 0.42 in 1991 (immediate post-war,
 *   unclear enforcement) to 0.61 by 1996 (institutionalized sanctions,
 *   routine inspection) before moderating to 0.58 by 2003 (sanctions regime
 *   normalized but still active, inspections periodic). Theater ratio rises
 *   from 0.35 to 0.52 as inspection protocols become increasingly ritualistic
 *   — the constraint persists through institutional momentum despite
 *   accumulating evidence that chemical and biological weapons programs are
 *   either destroyed or hidden beyond inspection capacity. The constraint
 *   exemplifies how post-war security architecture transforms battlefield
 *   victory into long-term regulatory control, distributing benefits to great
 *   powers and regional allies while concentrating costs on the defeated
 *   state.
 *
 * KEY AGENTS:
 *   - Iraq State: Primary victim (powerless/trapped) — bears military degradation, economic isolation, and unfalsifiable compliance burden with no credible exit mechanism for sanctions cessation
 *   - Iraqi Civilian Population: Secondary victim (organized/constrained) — faces humanitarian costs of comprehensive sanctions; constrained from accessing reconstruction resources or normal trade
 *   - Regional Military Powers (Syria, Iran, Turkey, Saudi Arabia): Mixed agents (organized/constrained) — face export control constraints on arms procurement; simultaneously benefit from Iraqi military degradation reducing regional competition
 *   - UN Security Council & Permanent Members: Primary beneficiary (institutional/arbitrage) — designers and controllers of the regime; capture coordination benefits of proliferation prevention and great-power alliance demonstration; retain exit and modification authority
 *   - United States: Primary beneficiary and enforcement guarantor (powerful/mobile) — uses constraint as alliance-building mechanism in region; retains exit option of regime change or negotiated settlement
 *   - International Nonproliferation Regime (IAEA/UNSCOM/UNMOVIC): Institutional actor (institutional/arbitrage) — inspection authority expanded by regime; experiences constraint as mandate expansion; theater component increases over time as verification asymmetries accumulate
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees constraint as post-Cold War attempt to manage proliferation through inspections and access denial; identifies tangled-rope structure combining coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1991_bush_wmd_proliferation_controls, 0.58).
domain_priors:suppression_score(sotu_1991_bush_wmd_proliferation_controls, 0.72).
domain_priors:theater_ratio(sotu_1991_bush_wmd_proliferation_controls, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1991_bush_wmd_proliferation_controls, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1991_bush_wmd_proliferation_controls, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1991_bush_wmd_proliferation_controls, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1991_bush_wmd_proliferation_controls, tangled_rope).
narrative_ontology:human_readable(sotu_1991_bush_wmd_proliferation_controls, "WMD Proliferation Controls and Iraq Sanctions Regime (1991)").
narrative_ontology:topic_domain(sotu_1991_bush_wmd_proliferation_controls, "regulatory/geopolitical/security").

domain_priors:requires_active_enforcement(sotu_1991_bush_wmd_proliferation_controls).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1991_bush_wmd_proliferation_controls, regional_security_alliance).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_wmd_proliferation_controls, international_proliferation_regime).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_wmd_proliferation_controls, arms_export_controllers).
narrative_ontology:constraint_victim(sotu_1991_bush_wmd_proliferation_controls, iraq_state_capacity).
narrative_ontology:constraint_victim(sotu_1991_bush_wmd_proliferation_controls, iraq_economic_reconstruction).
narrative_ontology:constraint_victim(sotu_1991_bush_wmd_proliferation_controls, regional_military_modernization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRAQ STATE CAPACITY (SNARE) — Trapped by military defeat, international isolation, and weapons inspection regimes with no credible exit mechanism. Cannot rebuild military capacity, cannot access reconstruction financing, cannot demonstrate 'peaceful intent' to a satisfaction criterion externally defined by victors. Maximum suppression and extraction — bears full cost of the constraint with no discretion over compliance terms or inspection cessation conditions.
constraint_indexing:constraint_classification(sotu_1991_bush_wmd_proliferation_controls, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: IRAQ CIVILIAN POPULATION (SNARE) — Constrained exit: Iraqis cannot vote out the sanctions, cannot negotiate their own terms, cannot access international humanitarian assistance without regime compliance with inspection protocols. Reconstruction denial becomes a tool for political pressure on state actors. High suppression of economic capacity; extraction of human capital through brain drain and reduced health/education investment. Organized resistance emerges over time (sanctions committees, underground economies) but remains structurally subordinate.
constraint_indexing:constraint_classification(sotu_1991_bush_wmd_proliferation_controls, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL POWERS (TANGLED ROPE) — Constrained by export control verification and arms embargo enforcement, but also benefit from Iraqi military degradation reducing regional military competition and security threat. Mixed extraction: these powers lose access to Iraqi procurement markets and face enhanced monitoring of their own weapons programs; simultaneously, Iraq's weakened state enhances their relative strategic position. Suppression is high (monitoring, enforcement) but they retain meaningful foreign policy agency and exit options (circumvention, reorientation toward non-WMD procurement).
constraint_indexing:constraint_classification(sotu_1991_bush_wmd_proliferation_controls, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: UN SECURITY COUNCIL & ALLIED POWERS (ROPE) — Net beneficiaries experiencing the constraint as coordination mechanism: WMD controls prevent regional arms race, inspection regimes provide transparency, sanctions enforcement demonstrates great-power coordination on proliferation. These actors designed the regime, control its enforcement, and capture the benefits of demonstrated regime solidarity. Arbitrage exit options (can modify UNSC resolutions, can adjust inspection protocols, can negotiate compliance terms). Low effective extraction — the constraint solves coordination problems these actors prioritized.
constraint_indexing:constraint_classification(sotu_1991_bush_wmd_proliferation_controls, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NPT/IAEA ADMINISTRATIVE APPARATUS (PITON) — The International Atomic Energy Agency's inspection authority in Iraq is performative: inspectors lack enforcement capacity and are functionally dependent on UN/US military protection for access. The inspection ritual persists (UNSCOM, later UNMOVIC) through institutional momentum — these organizations exist to verify nonproliferation, so Iraq verification becomes their core mandate. Theater ratio high: inspections produce detailed reports and visibility theater while actual weapons development capacity remains ambiguous (chemical/biological programs largely hidden; nuclear program destroyed but verification incomplete). The administrative apparatus experiences diminishing returns over the 1990s as inspectors report inconclusive findings, yet inspection regimes persist due to institutional inertia.
constraint_indexing:constraint_classification(sotu_1991_bush_wmd_proliferation_controls, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: UNITED STATES (SCAFFOLD) — The US experiences the constraint as a temporary coordination problem with visible sunset: containing Iraqi military capacity until either regime change occurs or a credible non-proliferation agreement emerges. Mobile exit options (can shift from containment to invasion, can negotiate with successor regimes, can modify enforcement intensity). The constraint carries an implicit sunset: once Iraq poses no proliferation threat — either through verifiable disarmament, regime change, or regional accommodation — the isolation regime loosens. Theater component: containment demonstrates great-power resolve and regional protection commitment, supporting US alliance structure in the Middle East.
constraint_indexing:constraint_classification(sotu_1991_bush_wmd_proliferation_controls, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the constraint embodies the post-Cold War attempt to manage weapons proliferation through inspections and access denial. Genuine coordination function: preventing Middle Eastern WMD arms race serves collective security interests of regional states and great powers. Asymmetric extraction: costs are concentrated on Iraq (defeated, isolated) while benefits flow to security winners and proliferation-regime managers. The regime's structural logic is tangled: it coordinates nonproliferation norms while extracting political compliance through isolation mechanisms. Suppression is structural (military defeat, international enforcement) not performative.
constraint_indexing:constraint_classification(sotu_1991_bush_wmd_proliferation_controls, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1991_bush_wmd_proliferation_controls_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1991_bush_wmd_proliferation_controls, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1991_bush_wmd_proliferation_controls, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1991_bush_wmd_proliferation_controls, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1991_bush_wmd_proliferation_controls, TR),
    TR >= 0.70.

:- end_tests(sotu_1991_bush_wmd_proliferation_controls_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from Iraq through multiple mechanisms: military capability denial (weapons inspections destroying or restricting acquisition), economic isolation (sanctions preventing reconstruction and normal trade), and compliance coercion (unfalsifiable 'peaceful intent' standard with regime modification threat as enforcement). However, extraction is not total — Iraq retains state capacity, retains ability to redirect resources to civilian sectors, retains some international engagement through humanitarian exceptions. The extractiveness reflects that the constraint combines genuine proliferation prevention (coordination benefit to the international system and regional security) with asymmetric costs concentrated on the defeated state. Suppression (0.72): High. Iraq faces severe suppression mechanisms: military enforcement (no-fly zones, invasion threat), inspection access requirements at gunpoint, economic strangulation through comprehensive sanctions, and structural inability to negotiate compliance standards (these are set unilaterally by UNSC). However, suppression is not absolute — Iraq retains underground economies, retains some trade through sanctions busting, retains ability to resist inspections (which occurs in practice). Theater ratio (0.48): Moderate. Inspection protocols have significant performative content — inspectors produce voluminous reports and maintain institutional credibility through continued activity, even as verification asymmetries accumulate. However, the constraint is not primarily theatrical — the military component (no-fly zones, invasion threat) is structurally real, and the economic isolation is operationally severe. Theater ratio increases over time (0.35→0.52) as inspections become routine and less directly connected to material weapons destruction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism produces radically different experiential classifications. Iraq experiences the constraint as irreversible isolation (snare); the UN experiences it as coordination mechanism (rope); the US experiences it as temporary containment with exit options (scaffold); inspectors experience it as institutional mandate increasingly disconnected from operational function (piton); regional powers experience it as constrained competition with offsetting benefits (tangled rope). No single classification is 'correct' — the presheaf over observer positions IS the constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim structure drives directionality assignment. Iraq is the primary victim (military capability denial, economic isolation) with trapped exit options — derives high d (0.88). UN/US are primary beneficiaries (alliance coordination, great-power demonstrated solidarity, regional hegemony) with arbitrage exit options — derive low d (0.12-0.15). Regional powers are secondary victims (export control constraints) but also beneficiaries (reduced Iraqi threat) with constrained exit options — derive intermediate d (0.58). The inspection apparatus is bureaucratically beneficiary (mandate expansion, organizational resources) but functionally constrained (cannot enforce, dependent on military backing) — derives institutional d (0.35).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy through asymmetric beneficiary/victim structure. It is NOT a pure coordination mechanism (Rope) because Iraq bears irreversible costs while US/UN capture discretionary benefits (exit options, regime design authority, strategic positioning). It is NOT pure extraction (Snare) because genuine proliferation prevention benefits exist for all regional states and the international system — preventing a Middle Eastern WMD arms race is a real coordination problem. It IS a tangled rope because (1) genuine coordination function exists (proliferation prevention), (2) asymmetric extraction is embedded (costs on Iraq, benefits on victors), (3) active enforcement is required (inspections, sanctions, military enforcement), (4) the regime would not exist without the extraction component — Iraq compliance alone would not be sufficient; Iraq must remain isolated as a demonstration of great-power enforcement capacity. The extraction mechanism sustains the regime through structure (Iraq cannot credibly signal peaceful intent under surveillance) not just malice. This is the hallmark of tangled rope: the coordination function and extraction function are structurally entangled, neither reducible to the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peaceful_intent_definition,
    'What objective criteria constitutes ''peaceful intent'' sufficient to end enhanced vigilance and access denial?',
    'UNSC resolution defining compliance benchmarks; examination of whether any Iraqi government could have satisfied the criteria; comparison with other post-war regimes (Japan, Germany) and their disarmament timelines',
    'If criteria are objective and achievable: constraint has credible exit pathway, scaffold classification confirmed. If criteria are subjective/discretionary: no credible exit exists regardless of Iraqi compliance, snare classification confirmed, false promise of negotiated settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peaceful_intent_definition, conceptual, 'Whether ''peaceful intent'' has objective versus discretionary definition').

omega_variable(
    inspection_efficacy_and_verification,
    'Can weapons inspection regimes (UNSCOM, UNMOVIC) actually verify absence of WMD programs, or do they primarily confirm presence?',
    'Historical assessment of inspection findings versus post-2003 invasion discoveries; evaluation of hidden programs that escaped detection (biological, chemical weapons programs active during 1990s); analysis of verification asymmetry — easy to confirm weapons exist (one discovery) but logically impossible to confirm absence (infinite concealment strategies)',
    'If inspections can verify absence: constraint is coordination mechanism with real verification floor. If inspections can only detect presence: regime becomes extraction mechanism justified by unfalsifiable nonproliferation claims; constrained actors face moving goalpost (compliance is unfalsifiable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inspection_efficacy_and_verification, empirical, 'Whether inspections achieve verification of absence or only detection of presence').

omega_variable(
    sanctions_economic_impact_distribution,
    'Do comprehensive sanctions on Iraq primarily degrade state military capacity or primarily degrade civilian welfare and economic development?',
    'Analysis of health outcomes (child mortality, malnutrition), educational access, and infrastructure degradation during sanctions period (1991-2003); comparison with selective military-technology sanctions regimes; assessment of regime resource allocation under sanctions constraints',
    'If primarily military degradation: tangled-rope classification sustained (coordination + targeted extraction). If primarily civilian harm: snare classification confirmed, revealing that suppression operates through humanitarian leverage rather than military restriction. Strong evidence for extraction mechanism targeting state reconstruction capacity rather than weapons capability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctions_economic_impact_distribution, empirical, 'Whether sanctions target military capacity or civilian welfare').

omega_variable(
    regional_power_compliance_asymmetry,
    'Do export control regimes apply symmetrically to all regional powers or asymmetrically based on alignment with the US-led coalition?',
    'Comparative analysis of export restrictions applied to Iraq versus Syria, Iran, Saudi Arabia, Turkey, Israel during 1990s; examination of enforcement intensity and UNSC action on violations; assessment of whether allied arms programs faced equivalent scrutiny',
    'If symmetric: proliferation regime is genuinely norms-based, rope classification sustained. If asymmetric: regime functions partly as extraction mechanism concentrating restrictions on adversaries while exempting allies, tangled-rope classification confirmed with higher asymmetric-extraction component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_power_compliance_asymmetry, empirical, 'Whether export controls apply symmetrically or asymmetrically by alignment').

omega_variable(
    inspection_regime_mission_creep,
    'Do inspection protocols maintain focus on WMD capabilities or expand to broader intelligence collection and political leverage?',
    'Analysis of UNSCOM/UNMOVIC reports; assessment of inspection team composition (weapons specialists versus intelligence officers); examination of inspection targeting patterns (distribution across sites, focus on military versus civilian infrastructure); post-2003 revelations about inspection regime intelligence functions',
    'If focused on WMD: institutional framework is clean coordination mechanism. If expanded to general intelligence: inspection apparatus becomes dual-purpose tool for surveillance and political control, increasing theater_ratio and shifting piton classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspection_regime_mission_creep, empirical, 'Whether inspection regimes stay within WMD scope or expand to intelligence/leverage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1991_bush_wmd_proliferation_controls, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1991_bush_wmd_proliferation_controls, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu_tr_t3, sotu_1991_bush_wmd_proliferation_controls, theater_ratio, 3, 0.42).
narrative_ontology:measurement(sotu_tr_t6, sotu_1991_bush_wmd_proliferation_controls, theater_ratio, 6, 0.48).
narrative_ontology:measurement(sotu_tr_t12, sotu_1991_bush_wmd_proliferation_controls, theater_ratio, 12, 0.52).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1991_bush_wmd_proliferation_controls, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sotu_be_t3, sotu_1991_bush_wmd_proliferation_controls, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(sotu_be_t6, sotu_1991_bush_wmd_proliferation_controls, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(sotu_be_t12, sotu_1991_bush_wmd_proliferation_controls, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1991_bush_wmd_proliferation_controls, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1991_bush_wmd_proliferation_controls, iraqi_state_capacity_reconstruction).
narrative_ontology:affects_constraint(sotu_1991_bush_wmd_proliferation_controls, middle_east_military_balance).
narrative_ontology:affects_constraint(sotu_1991_bush_wmd_proliferation_controls, global_proliferation_regime_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is upstream to specific regional military capability constraints (Syria's access to Soviet legacy systems, Iran's procurement networks) and downstream from the broader post-Cold War proliferation management framework. The WMD controls mechanism and comprehensive sanctions are structurally distinct constraints with different ε values (controls ε≈0.42, sanctions ε≈0.65) but function as integrated enforcement system. Network decomposition would split into separate stories for weapons inspection coordination (lower ε, cleaner coordination) and economic isolation mechanism (higher ε, cleaner extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1991_bush_wmd_proliferation_controls, institutional, 0.13).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
