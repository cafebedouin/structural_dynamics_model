% ============================================================================
% CONSTRAINT STORY: cop_consensus_paralysis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cop_consensus_paralysis, []).

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
 *   constraint_id: cop_consensus_paralysis
 *   human_readable: COP Consensus Paralysis in Climate Governance
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   The COP consensus paralysis constraint describes a structural deadlock in
 *   international climate governance: the requirement for unanimous consent
 *   (consensus rule) to binding climate agreements has enabled high-emission
 *   nations and fossil fuel coalitions to veto meaningful action while
 *   appearing to negotiate in good faith. This mechanism has persisted for
 *   30+ years (since the Rio Summit adopted the Framework Convention) and has
 *   produced agreements with targets (Paris 1.5°C, net-zero by 2050) that
 *   lack binding enforcement mechanisms or enforceable transition timelines.
 *   The constraint exhibits all six classification types from different
 *   observation contexts: it appears as pure extraction (snare) to vulnerable
 *   developing nations facing climate harm; as mixed coordination-extraction
 *   (tangled rope) to mid-sized economies wanting action but lacking veto
 *   power; as pure coordination (rope) to high-emission nations using the
 *   veto to maintain extraction options; as degraded performance (piton) to
 *   the UNFCCC institutional apparatus that manages endless meetings
 *   producing no binding outcomes; as a temporary problem with alternatives
 *   (scaffold) to subnational actors building parallel governance; and as an
 *   immutable natural law of international relations (mountain) to analysts
 *   who see consensus as inherent to sovereignty. The theater_ratio has risen
 *   from 0.48 (2010) to 0.68 (2025) as the gap between COP rhetoric ('keep
 *   1.5°C alive') and actual emission trajectories (accelerating toward
 *   2.7-3.0°C) has widened. The constraint is a working exemplar of how
 *   consensus institutions can be weaponized to preserve status quo
 *   extraction while maintaining the performance of democratic deliberation.
 *
 * KEY AGENTS:
 *   - Vulnerable developing nations (Small Island Developing States, Least-Developed Countries): Primary victims (powerless/trapped) — face existential climate harm with zero veto power in consensus system
 *   - Climate-displaced populations: Primary victims (powerless/trapped) — bearing 100% of extraction cost while having zero representation in COP negotiations
 *   - Mid-sized emerging economies (Mexico, Vietnam, South Africa): Secondary victims/beneficiaries (organized/constrained) — benefit from climate finance and technology transfer coordination but harmed by paralysis
 *   - High-emission industrial economies (China, USA, India, EU historically): Primary beneficiaries (institutional/arbitrage) — maintain high emissions through veto power while appearing to negotiate
 *   - Fossil fuel coalitions (OPEC, Saudi Arabia, Russia, coal-dependent states): Primary beneficiaries (institutional/arbitrage) — use consensus veto as collective blocking mechanism
 *   - UNFCCC institutional apparatus: Institutional actor (institutional/arbitrage) — maintains elaborate meeting and working-group structure with limited functional emission reduction output (piton perspective)
 *   - Subnational climate action coalitions (cities, states, regional partnerships): Organized actors (organized/constrained) — building alternative governance pathways outside COP consensus mechanism
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing consensus paralysis as immutable property of international law rather than recognizing it as a contingent design choice that benefits powerful states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cop_consensus_paralysis, 0.58).
domain_priors:suppression_score(cop_consensus_paralysis, 0.72).
domain_priors:theater_ratio(cop_consensus_paralysis, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cop_consensus_paralysis, extractiveness, 0.58).
narrative_ontology:constraint_metric(cop_consensus_paralysis, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cop_consensus_paralysis, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cop_consensus_paralysis, tangled_rope).
narrative_ontology:human_readable(cop_consensus_paralysis, "COP Consensus Paralysis in Climate Governance").
narrative_ontology:topic_domain(cop_consensus_paralysis, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cop_consensus_paralysis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cop_consensus_paralysis, high_emission_economies).
narrative_ontology:constraint_beneficiary(cop_consensus_paralysis, fossil_fuel_coalitions).
narrative_ontology:constraint_beneficiary(cop_consensus_paralysis, status_quo_preservers).
narrative_ontology:constraint_victim(cop_consensus_paralysis, vulnerable_developing_nations).
narrative_ontology:constraint_victim(cop_consensus_paralysis, climate_displaced_populations).
narrative_ontology:constraint_victim(cop_consensus_paralysis, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE DEVELOPING NATION (SNARE) — Small island states and least-developed nations face existential climate harm (rising seas, crop failure, mass displacement) but are trapped within a consensus mechanism that requires unanimous agreement. Exit is physically impossible; they cannot leave Earth's climate system. The consensus rule paralyzes action while their territory literally disappears. Maximum extraction with maximum suppression.
constraint_indexing:constraint_classification(cop_consensus_paralysis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE-DISPLACED POPULATIONS (SNARE) — Populations facing climate migration, famine, water scarcity, and resource conflict have no seat at COP negotiations (not sovereign nations), no veto power, and no exit from the constraint. They bear 100% of extraction cost (physical suffering) with zero input to the decision-making mechanism that perpetuates inaction.
constraint_indexing:constraint_classification(cop_consensus_paralysis, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-SIZED EMERGING ECONOMY (TANGLED ROPE) — Nations like Mexico, Vietnam, or South Africa benefit from the coordination function (unified climate framework enables technology transfer, climate finance, and development partnerships) but are also extracted from by the consensus rule's paralysis. They want action but lack veto power (unlike high-emission states or major blocs). Constrained exit: leaving the COP system weakens their negotiating position for finance and technology access.
constraint_indexing:constraint_classification(cop_consensus_paralysis, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-EMISSION INDUSTRIAL ECONOMY (ROPE) — Major economies (USA, China, India historically, EU in some contexts) experience the consensus mechanism as pure coordination: it enables them to maintain high emissions by delaying binding commitments while appearing to negotiate in good faith. They can arbitrage between climate commitments and economic growth by controlling the consensus veto. Net beneficiary with high exit optionality.
constraint_indexing:constraint_classification(cop_consensus_paralysis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FOSSIL FUEL COALITION (ROPE) — Oil, gas, and coal producing nations (Saudi Arabia, Russia, Australia historically) benefit from the consensus rule by using it as a collective veto mechanism. Any agreement can be blocked by one member, preventing binding mitigation requirements. Their coordination function is real: they coordinate opposition to reduce their extraction costs. They can arbitrage between COP participation (maintaining legitimacy) and blocking action (preserving profits).
constraint_indexing:constraint_classification(cop_consensus_paralysis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: UNFCCC INSTITUTIONAL STRUCTURE (PITON) — The Treaty Secretariat and UNFCCC apparatus are largely theatrical. They coordinate the logistics of COP meetings, produce lengthy technical documents, and manage working groups, but the actual decisions are blocked by consensus veto. The institution has become a performance of climate governance rather than a mechanism for climate action. Theater ratio high because the ritual (negotiations, agreements, targets, monitoring) persists without functional emission reduction. The institution persists through bureaucratic inertia — it would lose its reason-for-being if consensus paralysis were acknowledged.
constraint_indexing:constraint_classification(cop_consensus_paralysis, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: SUBNATIONAL CLIMATE ACTION COALITION (SCAFFOLD) — Cities, states, and regional coalitions (Subnational Climate Change Acceleration Partnership, Coalition of the Willing) bypass COP consensus paralysis by implementing unilateral climate action with sunset logic: 'We will act until the COP framework catches up, then harmonize.' These coalitions provide temporary coordination outside the consensus mechanism. If the COP eventually produces binding agreements, subnational actors see their scaffolding as superseded. If paralysis persists beyond 2050, the sunset never triggers and the scaffold becomes a permanent alternative governance layer.
constraint_indexing:constraint_classification(cop_consensus_paralysis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, consensus paralysis appears to be an immutable property of international law: without a supranational enforcement mechanism, nation-states cannot be compelled to abandon sovereignty to a global authority. The UN system, lacking enforcement power, produces only consensus-based agreements that any state can veto. This perspective treats the consensus rule as a structural property of the nation-state system itself — impossible to change without dissolving nation-state sovereignty. However, the structural data reveals this as a false summit: the consensus rule is a design choice (the Rio Summit adopted it deliberately), beneficiaries exist (high-emission nations benefit from blocking), and alternative governance models exist (majority-rule systems, variable-geometry coalitions, enforcement mechanisms). The 'natural law' framing naturalizes what is actually a contingent institutional arrangement that benefits powerful states.
constraint_indexing:constraint_classification(cop_consensus_paralysis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cop_consensus_paralysis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cop_consensus_paralysis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cop_consensus_paralysis, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cop_consensus_paralysis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cop_consensus_paralysis, TR),
    TR >= 0.70.

:- end_tests(cop_consensus_paralysis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The consensus rule creates asymmetric extraction: high-emission nations extract the right to maintain emissions, vulnerable nations extract physical climate harm and economic losses. The value reflects that this is not pure extraction (some coordination benefits flow through climate finance and technology transfer) but substantial asymmetric distribution. The rising trajectory (0.35→0.58 over 15 years) reflects that the gap between agreed targets and actual outcomes has widened as real-world climate impacts exceed the envelope of politically tolerable adaptation and compensation. Suppression (0.72): High. Multiple overlapping suppression mechanisms: (1) Procedural — consensus rule requires unanimity, giving one blocking coalition veto power. (2) Epistemic — climate science uncertainty is weaponized to delay binding action ('need more research before commitments'). (3) Economic — transitions away from fossil fuels are framed as economically catastrophic, justifying slowness. (4) Structural — vulnerable nations lack alternative forums for climate governance outside UN system. (5) Institutional — the UNFCCC apparatus itself is captured by high-emission states through negotiating process control. Theater ratio (0.68): High, rising. The COP meeting ritual (annual two-week negotiations, working groups, technical annexes, nationally determined contributions) produces extensive documentation and high-profile declarations but minimal binding emission reduction. The rise from 0.48 to 0.68 reflects the growing gap between rhetoric ('Paris Agreement keeps 1.5°C alive') and reality (current trajectory 2.7-3.0°C).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental perspectival divergence. The primary beneficiary (high-emission economy) sees the consensus mechanism as enabling coordination on technology sharing and finance while preserving their right to emit — a rope classification. The fossil fuel coalition sees consensus as enabling their veto power — also rope. The vulnerable nation sees the same mechanism as trapping them in a system that permits slow-motion extraction of their territory and livelihood — snare classification. The mid-sized economy sees mixed benefits (finance and tech access) and harms (continued paralysis on binding targets) — tangled rope. The UNFCCC apparatus sees its own elaborate meeting structure as largely performative — piton classification. The subnational coalition sees temporary scaffolding alternatives that can be abandoned once COP consensus produces binding agreements — scaffold. The analytical observer risks seeing consensus as inherent to sovereignty (mountain), thereby naturalizing what is actually a contingent institutional choice that benefits powerful states. The engine's false-summit detector should flag this mountain perspective as a false summit, revealing that the 'natural law' framing obscures beneficiary interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint is determined by each agent's structural position: whether they control the veto power, whether they can exit the system, and whether they benefit from paralysis. High-emission nations with veto power (d ≈ 0.05-0.15) experience low effective extraction because they are beneficiaries with arbitrage options — they can claim to negotiate while blocking binding action. Vulnerable developing nations (d ≈ 0.95) experience maximum extraction because they are trapped (cannot exit Earth's climate system) and powerless (no veto). Mid-sized emerging economies (d ≈ 0.60) experience moderate extraction because they have some negotiating capacity (organized power) but no veto and face constrained exit options (leaving COP weakens finance/technology access). The fossil fuel coalition (d ≈ 0.10) experiences low extraction because they have collective veto power and arbitrage options. The UNFCCC apparatus (d ≈ 0.20) experiences low extraction because it is institutional and can arbitrage between meeting management and actual governance (via the piton mechanism). Subnational actors (d ≈ 0.40) experience moderate extraction because they are organized but constrained — they can build alternatives but these alternatives are incomplete substitutes for supranational coordination. The analytical observer's directionality is high (d ≈ 0.75) when measured from the perspective of victims being excluded from their own fate, but the false-summit perspective assigns false directionality by treating the constraint as natural rather than designed.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by demonstrating that the consensus rule serves a genuine coordination function (enabling face-saving negotiation, preserving sovereignty, allowing countries to claim climate action while maintaining domestic autonomy) AND extracts substantial asymmetric costs (climate harm concentrated on vulnerable nations, perpetual paralysis despite agreed targets, compensation far below actual damages). The tangled-rope classification captures this hybrid: the mechanism IS doing real coordination work (technology transfer, climate finance, norm-setting around climate targets) while simultaneously enabling high-emission nations to extract the right to exceed safe climate boundaries. The mandatrophy resolves by recognizing that all six classifications are simultaneously correct from different vantage points: snare for the vulnerable (trapped with no options), rope for the beneficiaries (pure coordination benefit), tangled rope for mid-powers (mixed), piton for the bureaucracy (degraded), scaffold for the parallel coalitions (temporary), and false-summit mountain for the naturalizing perspective. The constraint does not collapse into a single type because the structural relationships are genuinely asymmetric. The paradox-resolution is that consensus-based international governance can be simultaneously a coordination mechanism (enabling multiple sovereignty-respecting nations to cooperate) and an extraction mechanism (enabling some nations to extract climate destruction rights) because it preserves power asymmetries while appearing neutral. The consensus rule itself is the mechanism that enables this hybrid: requiring unanimity means that those with the highest costs to decarbonization can veto action, converting structural power into extraction capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_rule_necessity,
    'Is the consensus rule inherent to international law and nation-state sovereignty, or is it a design choice adopted at Rio that could be changed?',
    'Historical analysis of treaty law: examine whether consensus was mandatory pre-1992 or whether it emerged as a deliberate choice by industrialized nations. Compare with non-consensus international mechanisms (WTO disputes, ICC prosecutions, IMF voting) to show alternative models exist.',
    'If design choice: consensus rule is contingent and could be replaced with majority-rule or qualified-majority systems. Reclassifies mountain to tangled_rope or snare across analytical perspectives. If inherent: the mountain classification reflects real structural constraint of sovereignty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_rule_necessity, empirical, 'Whether consensus rule is inherent to international law or a design choice').

omega_variable(
    fossil_fuel_coalition_stability,
    'How durable is the fossil fuel coalition veto, given divergent interests (Saudi Arabia vs Russia, coal vs oil) and internal pressure from energy transition?',
    'Track coalition cohesion over time: measurement of defection rates (members voting for stronger measures or abstaining from blocking), fracture points where members leave the coalition, and correlation with stranded asset risk in oil-dependent economies.',
    'If coalition destabilizes: veto power declines and majority-rule alternative becomes feasible. Snare and rope perspectives transform to scaffold perspectives. If stable: coalition persists as primary beneficiary blocking action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_fuel_coalition_stability, empirical, 'Durability of fossil fuel coalition blocking consensus').

omega_variable(
    subnational_acceleration_sufficiency,
    'Can subnational actors (cities + states + corporations) achieve Paris Agreement targets independently if COP consensus remains paralyzed, or do they require supranational coordination to overcome coordination failures and free-ridership?',
    'Emission trajectory modeling: compare Paris-aligned subnational actor pathways (Marrakech Partnership, Race to Zero) against projected global emissions without international agreement. Identify critical coordination gaps (supply chain standardization, technology transfer, stranded asset liability) that only supranational agreement can solve.',
    'If sufficient: scaffold perspective validated — subnational action provides functional alternative. COP paralysis becomes tolerable. If insufficient: subnational action is spectacle (piton-like) without global coordination. Snare extraction accelerates as deadlines pass.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subnational_acceleration_sufficiency, empirical, 'Whether subnational actors can meet climate targets without global COP coordination').

omega_variable(
    loss_and_damage_as_extraction_threshold,
    'At what magnitude of climate losses (human mortality, agricultural collapse, territory loss) does the consensus-paralysis constraint become so obviously extractive that it shatters the legitimacy of the UN-based framework?',
    'Tracking loss-and-damage funding relative to actual economic losses from climate impacts. Identification of breaking points: major island nation disappearance, refugee flows exceeding national asylum capacity, agricultural collapse in major grain-producing regions. Measurement of state defection from COP legitimacy framework.',
    'If threshold exceeded: the constraint becomes too obviously predatory to maintain consensus theater. Either consensus mechanism is abandoned, or COP framework splinters into separate agreements. Current trajectory approaches threshold by 2040-2050.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loss_and_damage_as_extraction_threshold, empirical, 'Extraction threshold beyond which COP consensus legitimacy collapses').

omega_variable(
    variable_geometry_technical_feasibility,
    'Could a variable-geometry treaty (binding core members, optional participation for others, majority-rule within core) achieve mitigation targets without requiring universal consensus?',
    'Modeling: design a core coalition (EU, UK, Canada, small island states) with binding targets and majority-rule voting. Calculate whether their combined emissions reductions + technology transfer to non-core states achieves Paris targets. Identify free-ridership incentives and verification mechanisms needed to sustain coalition.',
    'If technically feasible: scaffold perspective becomes permanent alternative. COP consensus becomes optional rather than mandatory. If infeasible: free-ridership incentives collapse variable-geometry; universal coordination remains required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(variable_geometry_technical_feasibility, empirical, 'Whether variable-geometry treaty structure could replace consensus mechanism').

omega_variable(
    common_but_differentiated_responsibility_exploitation,
    'Do high-emission economies exploit the ''common but differentiated responsibility'' principle to block binding commitments while claiming to respect historical equity?',
    'Analysis of COP voting patterns and NDC (Nationally Determined Contribution) submissions: correlate claims of ''differentiated responsibility'' with actual emission reduction trajectories and blocking behavior. Identify whether the principle is used genuinely (to accommodate legitimate development needs) or as cover for extraction.',
    'If exploited: the equity framing of COP paralysis is performative. Tangled rope classifications shift toward snare. If genuine: differentiation reflects real asymmetries and the paralyzing mechanism is more legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(common_but_differentiated_responsibility_exploitation, empirical, 'Whether differentiated responsibility is exploited to enable blocking').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cop_consensus_paralysis, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cops_tr_t0, cop_consensus_paralysis, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cops_tr_t5, cop_consensus_paralysis, theater_ratio, 5, 0.58).
narrative_ontology:measurement(cops_tr_t10, cop_consensus_paralysis, theater_ratio, 10, 0.68).
narrative_ontology:measurement(cops_tr_t15, cop_consensus_paralysis, theater_ratio, 15, 0.75).

% Extraction over time
narrative_ontology:measurement(cops_be_t0, cop_consensus_paralysis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cops_be_t5, cop_consensus_paralysis, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(cops_be_t10, cop_consensus_paralysis, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(cops_be_t15, cop_consensus_paralysis, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cops_su_t0, cop_consensus_paralysis, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(cops_su_t10, cop_consensus_paralysis, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cop_consensus_paralysis, resource_allocation).
narrative_ontology:boltzmann_floor_override(cop_consensus_paralysis, 0.25).
narrative_ontology:affects_constraint(cop_consensus_paralysis, climate_finance_asymmetry).
narrative_ontology:affects_constraint(cop_consensus_paralysis, stranded_assets_liability).
narrative_ontology:affects_constraint(cop_consensus_paralysis, climate_migration_borders).

% DUAL FORMULATION NOTE:
% COP consensus paralysis is a constraint family with three decomposed stories: (1) cop_consensus_paralysis (ε=0.58) — the voting/decision mechanism itself; (2) climate_finance_asymmetry (ε=0.72) — the extraction of funds promised but not delivered; (3) stranded_assets_liability (ε=0.45) — the distributed cost of energy transition. Each story has different ε values because the observable differs: paralysis measures governance mechanism function, finance asymmetry measures capital flows, stranded assets measure economic transition burden. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cop_consensus_paralysis, institutional, 0.08).
constraint_indexing:directionality_override(cop_consensus_paralysis, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
