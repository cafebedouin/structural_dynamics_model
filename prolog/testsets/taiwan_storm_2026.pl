% ============================================================================
% CONSTRAINT STORY: taiwan_storm_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_storm_2026, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: taiwan_storm_2026
 *   human_readable: The 2026 Taiwan 'Perfect Storm' Geopolitical Convergence
 *   domain: geopolitical/political
 *
 * SUMMARY:
 *   The 2026-2027 Taiwan convergence represents a structural alignment of
 *   three geopolitical deadlines: Xi Jinping's 27th Party Congress succession
 *   timing (2027), a perceived window of U.S. military readiness and
 *   political will under specific administration, and a demographic/economic
 *   clock on Taiwan's military sustainability (conscription fatigue, chip
 *   industry erosion pressure, population aging). This constraint is a
 *   tangled rope at the analytical level because it combines genuine
 *   coordination functions (military deterrence architecture,
 *   alliance-building, strategic communication) with asymmetric extraction
 *   mechanisms (Beijing's succession pressure, U.S. extended deterrence
 *   rents, Taiwan's forced military modernization). The 'perfect storm'
 *   framing itself is performative: public discourse emphasizes the
 *   convergence to mobilize allies and deter Beijing, but the performative
 *   effect becomes part of the constraint structure — the theater shapes what
 *   is actually risky. Theater_ratio (0.48) is lower than many geopolitical
 *   constraints because the military deterrence machinery is substantive (not
 *   pure ritual), but it is not maximal because key uncertainties about
 *   escalation thresholds, command-and-control reliability, and economic
 *   spillover are officially managed through strategic ambiguity rather than
 *   explicit commitments.
 *
 * KEY AGENTS:
 *   - Taiwan Population: Primary victim (powerless/trapped) — 24 million civilians with no exit; decision architecture determined by Beijing, Washington, Taipei
 *   - Taiwan Government & Military: Secondary victim-beneficiary hybrid (organized/constrained) — receives security commitments but forced into existential military spending; genuine coordination function in deterrence but asymmetric constraints on autonomy
 *   - U.S. Strategic Leadership: Primary beneficiary (institutional/arbitrage) — extended deterrence rents, alliance management leverage, Indo-Pacific pivot position; high optionality
 *   - Beijing CCP/PLA Leadership: Mixed extractor and constrained agent (institutional/constrained) — extraction pressure from succession politics but also constrained by deterrence, economic risk, and succession uncertainty
 *   - ASEAN Regional States: Secondary victims (moderate/mobile) — exposure to disruption but partial exit through non-alignment, economic diversification, Quad-adjacent positioning
 *   - Global Supply Chain / Semiconductor Commons: Tertiary victim (powerless/trapped) — 92% of advanced semiconductors trapped in Taiwan; no exit from dependency; bears systemic disruption cost with no agency
 *   - International Rules-Based Order: Institutional actor (institutional/arbitrage) — persists performatively through strategic ambiguity; enforcement mechanisms degraded to piton status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_storm_2026, 0.58).
domain_priors:suppression_score(taiwan_storm_2026, 0.72).
domain_priors:theater_ratio(taiwan_storm_2026, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_storm_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(taiwan_storm_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(taiwan_storm_2026, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_storm_2026, tangled_rope).
narrative_ontology:human_readable(taiwan_storm_2026, "The 2026 Taiwan 'Perfect Storm' Geopolitical Convergence").
narrative_ontology:topic_domain(taiwan_storm_2026, "geopolitical/political").

domain_priors:requires_active_enforcement(taiwan_storm_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_storm_2026, beijing_leadership).
narrative_ontology:constraint_beneficiary(taiwan_storm_2026, us_military_industrial_complex).
narrative_ontology:constraint_victim(taiwan_storm_2026, taiwan_population).
narrative_ontology:constraint_victim(taiwan_storm_2026, regional_stability).
narrative_ontology:constraint_victim(taiwan_storm_2026, indo_pacific_trade).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWAN CIVIL POPULATION (SNARE) — Taiwan's 24 million residents face existential choice architecture with no exit. Military escalation scenarios dominate policy discourse; civilian evacuation planning is implicit but not discussed. This population bears maximum extraction of decision-making agency: strategic choices about their future are made in Beijing, Washington, and Taipei without meaningful public input on core tradeoffs.
constraint_indexing:constraint_classification(taiwan_storm_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TAIWAN GOVERNMENT & MILITARY (TANGLED ROPE) — Taiwan's state apparatus has genuine coordination function (force modernization, alliance-building, deterrence narrative) but faces asymmetric extraction: strategic autonomy is constrained by U.S. arms package timing, mainland military pressure, and internal political divisions. Active enforcement required: military conscription, civil defense preparations, and continuous alliance management. Both benefits (security commitments) and costs (existential vulnerability) present.
constraint_indexing:constraint_classification(taiwan_storm_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: U.S. STRATEGIC LEADERSHIP (ROPE) — The U.S. experiences the constraint as primarily coordination: maintaining deterrence balance, managing alliance commitments, and leveraging Taiwan as pivot in Indo-Pacific strategy. High arbitrage capacity: U.S. can modulate commitment levels, timing of military shipments, rhetorical positioning. The constraint enables rather than binds U.S. strategy; extraction flows toward this agent through extended deterrence rents and regional influence.
constraint_indexing:constraint_classification(taiwan_storm_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BEIJING LEADERSHIP (TANGLED ROPE) — China's leadership faces intense extraction pressure: Xi Jinping's legacy timeline (27th Congress 2027, potential succession dynamics) creates perceived deadline for Taiwan 'resolution.' Party unity requires demonstrable progress on 'national rejuvenation.' But escalation is also constrained: economic interdependence, ICBM second-strike deterrence, and U.S. commitment create genuine risks. Coordination function exists (regional hegemony narrative, military modernization), but extraction mechanism is also present (resource concentration, forced alignment of military/party objectives).
constraint_indexing:constraint_classification(taiwan_storm_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INT'L RULES-BASED ORDER (PITON) — The post-WWII security architecture (UN Charter, UNCLOS, principle of non-use of force) persists as a performative constraint on all parties. Functionally degraded: strategic ambiguity replaces clear rules; economic interdependence and military posturing coexist without producing binding commitments. Theater ratio high (0.48): formal multilateral institutions (UN, ASEAN, ARF) conduct endless dialogue that does not substantively constrain action. The order persists through institutional inertia and mutual face-saving, not because enforcement mechanisms are credible.
constraint_indexing:constraint_classification(taiwan_storm_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ASEAN & REGIONAL STAKEHOLDERS (SCAFFOLD) — Moderate-power states (Vietnam, Philippines, Indonesia, Singapore, Thailand, South Korea) see the constraint as a temporary coordination failure with partial sunset mechanisms: ASEAN centrality, Blue-dot Network, Quad-adjacent arrangements, and economic coupling create alternative pathways that bypass direct military confrontation. Meaningful exit available through economic reorientation and non-alignment rhetoric. Suppression exists but is not maximal — regional states can moderately constrain their own exposure by diversifying supply chains, maintaining equidistant positioning, and investing in non-Chinese trade corridors.
constraint_indexing:constraint_classification(taiwan_storm_2026, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: GLOBAL ECONOMIC SYSTEM (SNARE) — The 92% of global semiconductors from Taiwan (TSMC & Samsung subsidiary processes) create a structural trap: disruption would force $10+ trillion in global GDP reallocation, but no actor has incentive to prevent escalation if they believe their relative position improves. Theater ratio relevant here — public commitment to 'economic interdependence prevents war' is performative; actual mechanisms are absent. The global economic commons is a victim bearing full extraction cost with no exit or agency.
constraint_indexing:constraint_classification(taiwan_storm_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a system-dynamics perspective, the constraint exhibits genuine coordination function (military deterrence prevents escalation through mutual cost credibility), but the extraction mechanism is also structural: all sides extract commitment to preparation, military spending, and zero-sum competitive positioning. The 'perfect storm' framing reveals that the constraint functions precisely because the timing window creates extraction pressure — removal of the deadline would reduce urgency and paradoxically increase stability. The analytical observer sees that the constraint's stability is inverted: it is stable because it is extractive.
constraint_indexing:constraint_classification(taiwan_storm_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_storm_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_storm_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_storm_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_storm_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_storm_2026, TR),
    TR >= 0.70.

:- end_tests(taiwan_storm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant commitment, military spending, and strategic autonomy from multiple parties. Beijing faces extraction pressure from succession politics to demonstrate 'national rejuvenation' progress. Taiwan faces extraction of civilian agency and military resources. ASEAN faces extraction of strategic autonomy through forced alliance positioning. But the extractiveness is not maximal (0.72+) because genuine coordination also exists: deterrence architecture does prevent escalation through mutual cost credibility. The trend from 0.42→0.58 over two years reflects increasing extraction pressure as 2027 approaches. Suppression (0.72): High. Coercive barriers to exit are substantial: Taiwan cannot walk away from its geography; Beijing cannot reverse succession cycle; U.S. cannot costlessly reposition; ASEAN cannot fully neutralize. Strategic ambiguity explicitly suppresses clear communication that would enable coordinated exits. Theater ratio (0.48): Moderate-low, trending downward. The constraint's functional component (military deterrence, alliance coherence) is substantive, but performative elements exist: public escalation rhetoric, strategic ambiguity, multilateral institution dialogue. The theater ratio declines as 2027 approaches because stakes become real and substantive mechanisms must activate. If actual military action were imminent, theater would collapse toward 0.3-0.4 (mostly functional, little performance).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are extreme and revealing. Taiwan's population (powerless/trapped) experiences pure snare extraction. Taiwan's government (organized/constrained) sees mixed coordination and extraction — deterrence works but at cost of existential vulnerability. U.S. leadership (institutional/arbitrage) sees coordination with favorable power asymmetry — they set terms of deterrence. Beijing (institutional/constrained) sees genuine coordination function (military modernization, strategic narrative) but extraction pressure from internal political timeline. ASEAN (moderate/mobile) sees temporary coordination failure resolvable through economic exit and non-alignment — scaffold perspective. The rules-based order (institutional/arbitrage, viewed as piton) sees itself as persisting through theater and mutual face-saving. The analytical observer sees that the constraint's stability is inverted: it prevents escalation precisely because it creates extraction pressure that keeps all parties preparing and signaling. If the 2027 deadline were removed, paradoxically, escalation risk would increase because the urgency that keeps deterrence machinery active would evaporate. This inversion identifies the constraint as tangled rope at the system level: coordination exists only because extraction pressure maintains it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are highly differentiated by structural position. Taiwan's powerless/trapped status derives d≈0.95, producing maximum experienced extractiveness — the population has no agency in the constraint's operation. Taiwan's government (organized/constrained, victim but not powerless) derives d≈0.65, moderate-high. Beijing (institutional/constrained, extractor but not fully free agent) derives d≈0.55 due to succession pressure offsetting institutional power. U.S. (institutional/arbitrage, beneficiary) derives d≈0.10 due to high optionality. ASEAN regional states (moderate/mobile, partial victims) derive d≈0.50-0.60 depending on specific country. The analytical observer (analytical/analytical) derives d≈0.72 per canonical fallback. These differentials drive the perspectival gap: same underlying constraint, radically different experienced extractiveness based on structural position within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by exhibiting both coordination and extraction across different observational contexts. The Snare classification (from Taiwan population perspective) identifies that extraction is real: no exit exists. The Tangled Rope classification (from system-level view) identifies that coordination is also real: deterrence prevents escalation through mutual cost credibility. The Piton classification (from rules-based order perspective) identifies that performative institutional machinery persists despite functional degradation. No single perspective is 'wrong' — the constraint genuinely manifests all these properties. The diagnostic value is that the mandatrophy exposure reveals what the constraint actually is: a system where coordination and extraction are entangled such that removing the extraction pressure would destabilize the coordination. This is the definition of tangled rope: coordination cannot be maintained without the asymmetric extraction mechanism. The analytical observer (perspective 8) makes this explicit: the constraint is stable precisely because it is extractive. Removing extraction would remove urgency and paradoxically increase instability. This is the mandatrophy resolution: the constraint is a tangled rope because both dimensions are structurally necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    xi_succession_timeline_flexibility,
    'Is Xi Jinping''s 2027 legacy deadline a hard constraint (succession norms require demonstrable Taiwan progress) or a flexible narrative (CCP can accommodate indefinite status quo)?',
    'Post-2027 analysis of whether CCP leadership transitions; whether Taiwan issue is elevated or deprioritized in post-Congress period; analysis of party documents and succession patterns.',
    'If hard: extraction pressure is real and 2026-2027 is high-risk window. If flexible: headline risk is lower; constraint is theater masking status quo maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(xi_succession_timeline_flexibility, empirical, 'Whether Xi''s legacy timeline is structurally binding on Taiwan escalation').

omega_variable(
    us_force_readiness_window_validity,
    'Does the U.S. military actually possess credible rapid-deployment capacity to defend Taiwan in 2026, or is this a theater commitment without operational substance?',
    'Military capability assessment: carrier task force positioning, amphibious readiness groups, integrated air defense architecture, logistics sustainability for sustained Taiwan defense. Classified war games outcomes.',
    'If credible: U.S. deterrence is substantive. If theater: Beijing sees paper tiger; extraction pressure on Taiwan increases as U.S. commitment proves less binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_force_readiness_window_validity, empirical, 'Whether U.S. force posture enables rapid Taiwan defense').

omega_variable(
    semiconductor_disruption_escalation_logic,
    'Would military escalation over Taiwan actually disrupt 92% of global semiconductor production, or would military containment preserve TSMC operational capacity and supply chains?',
    'Scenario analysis: military strike patterns, TSMC facility resilience, supply chain redundancy, wartime logistics. Gaming exercise outcomes from defense think tanks.',
    'If high disruption: mutual economic pain creates deterrent. If contained disruption: China extracts Taiwan with limited global spillover; extraction mechanism is no longer balanced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(semiconductor_disruption_escalation_logic, empirical, 'Whether Taiwan conflict would disrupt global semiconductor supply').

omega_variable(
    taiwanese_coalition_power_emergence,
    'Can Taiwan''s civil society and opposition parties (DPP / KMT divide) form unified coalition during crisis, or will internal political fracture paralyze decision-making?',
    'Cross-party polling, historical precedent from 2018-2020 presidential cycle, analysis of ECFA (Economic Cooperation Framework Agreement) position convergence.',
    'If coalition forms: Taiwan powerless agent becomes organized (moderate power); snare classification shifts toward tangled_rope. If fracture: extraction mechanism is unopposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taiwanese_coalition_power_emergence, empirical, 'Whether Taiwan civil society can achieve political coalition').

omega_variable(
    asean_balancing_capacity,
    'Can ASEAN states genuinely remain nonaligned during major Taiwan escalation, or will security guarantees (U.S., Japan, Australia) force alignment that contradicts ASEAN centrality rhetoric?',
    'ASEAN statement consistency during crisis simulation; actual troop movement permissions (basing, overflight) if scenario occurs.',
    'If genuine non-alignment: scaffold perspective confirmed; region has real exit optionality. If forced alignment: regional states are secondary victims; extraction mechanism extends outward from Taiwan.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asean_balancing_capacity, conceptual, 'Whether ASEAN can maintain strategic autonomy during Taiwan conflict').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_storm_2026, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tw2026_tr_t0, taiwan_storm_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(tw2026_tr_t1, taiwan_storm_2026, theater_ratio, 1, 0.51).
narrative_ontology:measurement(tw2026_tr_t2, taiwan_storm_2026, theater_ratio, 2, 0.48).

% Extraction over time
narrative_ontology:measurement(tw2026_be_t0, taiwan_storm_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tw2026_be_t1, taiwan_storm_2026, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(tw2026_be_t2, taiwan_storm_2026, base_extractiveness, 2, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_storm_2026, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(taiwan_storm_2026, 0.55).
narrative_ontology:affects_constraint(taiwan_storm_2026, tsmc_geopolitical_dependency).
narrative_ontology:affects_constraint(taiwan_storm_2026, us_china_strategic_competition).
narrative_ontology:affects_constraint(taiwan_storm_2026, indo_pacific_security_architecture).

% DUAL FORMULATION NOTE:
% The Taiwan convergence decomposes into three distinct constraint families: (1) Direct military-strategic constraint (taiwan_storm_2026, this file), (2) Supply chain/economic constraint (tsmc_geopolitical_dependency, ε≈0.65, Snare), (3) Systemic great-power competition constraint (us_china_strategic_competition, ε≈0.52, Tangled Rope). The military-strategic constraint has lower extractiveness because deterrence machinery is partially functional; the supply chain constraint has higher extractiveness because no substitutes exist for Taiwan's semiconductor position; the great-power competition constraint is the largest system into which both are nested. All three share the 2026-2027 timeline but have different structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_storm_2026, powerless, 0.95).
constraint_indexing:directionality_override(taiwan_storm_2026, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
