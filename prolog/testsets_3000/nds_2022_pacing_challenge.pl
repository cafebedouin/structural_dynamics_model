% ============================================================================
% CONSTRAINT STORY: nds_2022_pacing_challenge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nds_2022_pacing_challenge, []).

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
 *   constraint_id: nds_2022_pacing_challenge
 *   human_readable: US National Defense Strategy 2022: Pacing Challenge Doctrine
 *   domain: geopolitical/defense_policy
 *
 * SUMMARY:
 *   The 2022 US National Defense Strategy establishes 'integrated deterrence'
 *   against China (the 'pacing challenge') and Russia (an 'acute threat') as
 *   the organizing principle for defense spending, alliance management, and
 *   technology policy. This doctrine creates a structural constraint with
 *   genuine coordination functions (pooling resources for deterrence,
 *   synchronizing allied defense capabilities, investing in
 *   deterrent-credible technologies) alongside significant extraction
 *   mechanisms (sustaining defense budgets, controlling allied strategic
 *   choices, weaponizing supply chains). The constraint exhibits the full
 *   range of DR types depending on observer position: the military-industrial
 *   complex sees pure coordination; regional allies see mixed
 *   coordination-extraction; global supply chains see pure extraction;
 *   international institutions see their functional authority sidelined; the
 *   analytical observer risks naturalizing a contingent strategic choice as
 *   geopolitical law. The extractiveness has grown from 0.38 (2022 initial
 *   framing) to 0.58 (2024-2026 implementation), driven primarily by supply
 *   chain decoupling acceleration and Taiwan contingency planning. Theater
 *   ratio has similarly increased, reflecting that 'integrated deterrence'
 *   messaging emphasizes strategic narrative (deterrence posture, unified
 *   messaging, capability demonstrations) alongside operational
 *   capacity-building.
 *
 * KEY AGENTS:
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — captures sustained defense budgets, contract continuity, technology development programs; experiences constraint as legitimate coordination mechanism
 *   - Global Supply Chain Actors: Primary victims (powerless/trapped) — nations dependent on semiconductor/rare earth supply face extraction through export controls and strategic decoupling; trapped in geographic chokepoints; cannot exit without economic cost
 *   - Regional US Treaty Allies (Japan, South Korea, Australia, Philippines): Secondary beneficiaries & constrained actors (moderate/constrained) — receive security guarantees and capacity-building assistance but must align with US doctrine and accept escalation risk
 *   - US Civilian Economy: Secondary victim (moderate/constrained) — faces opportunity cost from defense spending concentration; constrained by strategic doctrine alignment; some sectors (aerospace, semiconductors) benefit
 *   - International Institutions (UN, WTO, arms control regimes): Institutional actor (institutional/arbitrage) — functionally sidelined by unilateral US strategic action; maintain treaty obligations through inertia despite low operational relevance to pacing challenge
 *   - International Technology Standards Coalition: Organized agents (organized/constrained) — multilateral standards bodies face decoupling as temporary coordination breakdown; see sunset logic in confidence-building measures and verification treaties
 *   - Global Economic Integration Network: Victim (powerless/trapped) — abstract system-level consequence; post-Cold War globalization model faces structural reversal through technology decoupling; cannot exit or organize
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nds_2022_pacing_challenge, 0.58).
domain_priors:suppression_score(nds_2022_pacing_challenge, 0.72).
domain_priors:theater_ratio(nds_2022_pacing_challenge, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nds_2022_pacing_challenge, extractiveness, 0.58).
narrative_ontology:constraint_metric(nds_2022_pacing_challenge, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nds_2022_pacing_challenge, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nds_2022_pacing_challenge, tangled_rope).
narrative_ontology:human_readable(nds_2022_pacing_challenge, "US National Defense Strategy 2022: Pacing Challenge Doctrine").
narrative_ontology:topic_domain(nds_2022_pacing_challenge, "geopolitical/defense_policy").

domain_priors:requires_active_enforcement(nds_2022_pacing_challenge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, regional_us_allies).
narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, defense_contractor_sector).
narrative_ontology:constraint_victim(nds_2022_pacing_challenge, us_civilian_economy).
narrative_ontology:constraint_victim(nds_2022_pacing_challenge, global_economic_integration).
narrative_ontology:constraint_victim(nds_2022_pacing_challenge, peer_competitor_coordination_efforts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SUPPLY CHAIN ACTORS (SNARE) — Nations dependent on semiconductor, rare earth, and advanced manufacturing supply chains face extraction through dual-use export controls and technology decoupling. Trapped in geographic chokepoints (Taiwan, ASML manufacturing) and cannot exit without economic catastrophe. Maximum experienced extraction — strategic leverage deployed through supply chain weaponization.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL US TREATY ALLIES (TANGLED ROPE) — Japan, South Korea, Australia, Philippines benefit from US security guarantees and capacity-building assistance (coordination function), but are constrained by mandatory alignment with US strategic doctrine and exposure to escalation risk. Genuine coordination benefit + asymmetric extraction of strategic dependency. Cannot fully exit without security cost; cannot fully comply without economic exposure.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary. The pacing challenge doctrine justifies sustained defense spending ($820B+ FY2024), contract continuity, and technology development programs. Experiences the constraint as coordination: pooling resources to address shared existential threat. Net extraction flows toward this actor — they solve the coordination problem and capture the benefit.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL TECHNOLOGY STANDARDS COALITION (SCAFFOLD) — Organized multilateral bodies (OECD, ISO, regional standards groups) face the pacing challenge as a temporary coordination breakdown requiring restoration of interoperable standards. See the extraction mechanism as sunset-clause: as confidence-building measures mature (arms control verification, export coordination treaties), the decoupling extraction should decline. Theater ratio is moderate — genuine technical coordination work, but framed through strategic conflict narrative.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL INSTITUTIONS (PITON) — The UN, WTO, and arms control regimes (NPT, JCPOA framework) are largely sidelined by the pacing challenge doctrine's framing. These institutions maintained verification and dispute resolution functions but are now redundant to bilateral US strategic action. They persist through institutional inertia (treaty obligations, diplomatic courtesy) despite low functional verification in the pacing challenge context. Theater ratio high — ritual compliance with multilateral process alongside unilateral enforcement.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, great power competition for hegemonic position is an immutable feature of anarchic international systems (Thucydidean realism). The pacing challenge is not contingent policy but structural necessity. However, the extraction mechanism and beneficiary declarations reveal this as false naturalization — the doctrine is a contingent institutional choice, not a law of geopolitics.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nds_2022_pacing_challenge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nds_2022_pacing_challenge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nds_2022_pacing_challenge, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nds_2022_pacing_challenge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nds_2022_pacing_challenge, TR),
    TR >= 0.70.

:- end_tests(nds_2022_pacing_challenge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The pacing challenge doctrine extracts economic resources (defense budget concentration), strategic autonomy (allied nations must align doctrine), and supply chain leverage (weaponized dual-use controls). However, extraction is not maximal because genuine deterrence value exists — the coordination function is real (pooling military capacity, alliance synchronization, deterrent credibility). The 0.58 value reflects the mixed mechanism. Theater ratio (0.65): Moderate-high. 'Integrated deterrence' is framed as unified strategic narrative emphasizing capability demonstrations, doctrine messaging, and allied coordination signaling. However, genuine operational content exists — force posture changes, technology development, supply chain restructuring are real, not purely performative. The 0.65 reflects that theater is significant but not dominant. Suppression (0.72): High. The constraint uses significant mechanisms to suppress alternatives: asymmetric information about threat severity, framing of decoupling as irreversible/technical rather than policy-contingent, institutional control of threat assessment, diplomatic pressure on allies to align, and export control enforcement against defectors. However, suppression is not absolute — debate exists in Congress, allied nations maintain some independent foreign policy, and alternative multilateral approaches retain advocates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across six distinct views. The military-industrial complex sees legitimate deterrent coordination — pooling resources against shared threat is their genuine structural position. Regional allies see mixed experience: genuine security coordination benefit alongside coercive alignment with US strategic doctrine and exposure to Taiwan contingency risk. Global supply chain actors see pure extraction — their structural position offers no coordination benefit, only exposure to weaponized controls and geographic chokepoints. International institutions see their functional authority sidelined (piton perspective) — they maintain treaty verification roles through inertia but are bypassed by unilateral US action. The US civilian economy sees constrained burden — opportunity cost from defense concentration without clear security benefit beyond deterrence assumption. The analytical observer's attempt to naturalize the pacing challenge as geopolitical law (mountain) is undermined by the structural data: the doctrine is a policy choice, not a physical constraint. The perspectival gap reveals that 'pacing challenge' conflates three distinct claims: (a) China's military capability growth (empirical), (b) the requirement for unilateral US response vs multilateral alternatives (policy choice), and (c) the characterization of deterrence success metrics (preference).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation produces high d (extraction toward target) for powerless global supply chain actors: they have no exit options (trapped), no power to negotiate terms, and the constraint extracts strategic leverage from their geographic/industrial positions. Regional allies derive moderate d from constrained exit (genuine security benefit but cannot exit without cost) and institutional power to organize — they perceive tangled_rope, not snare. The military-industrial complex derives low d (institutional power + arbitrage exit + beneficiary status) — they capture the benefit flow and experience pure coordination. International institutions derive d slightly above 0.5 (institutional power but constrained exit in the unilateral strategic environment) — they experience piton (institutional inertia, degraded function). The analytical observer's mountain perspective derives from attempted naturalization of the doctrine as geopolitical law, but the beneficiary/victim structure reveals this as false naturalization: the extraction and coordination functions are contingent on policy choices, not physical laws.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The doctrine combines genuine coordination (integrated deterrence requires allied synchronization, shared threat assessment, pooled military capacity) with asymmetric extraction (sustained defense budgets, constrained allied autonomy, weaponized supply chains). The beneficiary/victim structure confirms this hybrid: the military-industrial complex and regional allies benefit from deterrent capacity and alliance commitment; global supply chains and international institutions bear extraction. The mandatrophy is resolved by recognizing that the constraint is NOT pure coordination (rope) despite the rhetorical framing — it extracts strategic autonomy and economic resources at a level that justifies tangled_rope classification. Nor is it pure extraction (snare) — genuine deterrent value and coordination benefits exist. The tangled_rope type captures the reality: the constraint solves a real coordination problem while simultaneously concentrating extraction flows toward beneficiaries. The theater ratio (0.65) indicates that roughly two-thirds of the constraint's activity is functional deterrence/coordination, and one-third is performative messaging and intra-bureaucratic signaling. The analytical observer risks false naturalization by treating the doctrine as geopolitical law rather than policy choice — the mountain perspective is exposed as false summit by the extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    china_pacing_or_strategic_pretext,
    'Is China structurally a ''pacing challenge'' requiring integrated deterrence, or is the pacing challenge doctrine a strategic pretext for sustaining defense budgets and military-industrial capacity absent Soviet threat?',
    'Comparative analysis of declared vs demonstrated Chinese military capability trajectories; assessment of threat timeline vs doctrine timeline; econometric analysis of defense spending growth vs threat-driven vs budget-driven cycles',
    'If genuine pacing threat: all perspectives except analyst see legitimate coordination/enforcement. If strategic pretext: snare and tangled_rope classifications upgrade in severity; rope becomes pure extraction of economic resources; mountain becomes false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_pacing_or_strategic_pretext, empirical, 'Whether China pacing justifies doctrine or doctrine justifies spending').

omega_variable(
    alliance_coercion_or_coordination,
    'Do US security commitments to regional allies constitute genuine coordination (joint defense against shared threat) or coercion (forced alignment with US strategic competition)?',
    'Analysis of ally burden-sharing ratios; assessment of whether alliance partners have independent exit options (AUKUS, Quad membership, dual-track diplomacy); longitudinal tracking of ally defense spending correlated with US pressure vs autonomous choice',
    'If genuine coordination: regional allies see rope or tangled_rope with real beneficiary status. If coercion: allies see snare or pure tangled_rope with extraction outweighing coordination benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alliance_coercion_or_coordination, empirical, 'Whether regional alliances are coordination or coercion').

omega_variable(
    supply_chain_decoupling_irreversibility,
    'Is technology decoupling (semiconductor, rare earth, advanced manufacturing) structurally irreversible, or does the pacing challenge doctrine itself create path-dependent lock-in that could be unwound by alternative policy?',
    'Cost-benefit analysis of full re-integration vs maintained decoupling; assessment of technological switching costs; identification of decoupling decisions that are unilaterally reversible vs multilaterally dependent',
    'If irreversible: decoupling is a true constraint (mountain-like); extraction in supply chains persists structurally. If reversible: decoupling is a contingent policy choice; scaffold classification is appropriate with real sunset possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_decoupling_irreversibility, empirical, 'Whether technology decoupling is structurally irreversible').

omega_variable(
    multilateral_alternatives_feasibility,
    'Could a multilateral arms control and verification framework (updated Cold War deterrence model) achieve equivalent deterrence at lower economic extraction cost than unilateral pacing doctrine?',
    'Comparative modeling of security outcomes under multilateral arms control treaties vs unilateral capacity-building; historical analysis of Cold War deterrence stability vs post-Cold War security gaps; expert consensus on verification feasibility for hypersonic, AI-enabled, space-based weapons',
    'If feasible: pacing doctrine is institutional choice favoring unilateral over multilateral; extraction is policy-dependent, not structural. If infeasible: doctrine reflects genuine security constraint; classification upgrades toward legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_alternatives_feasibility, conceptual, 'Whether multilateral alternatives could replace unilateral doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nds_2022_pacing_challenge, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nds22_tr_t0, nds_2022_pacing_challenge, theater_ratio, 0, 0.55).
narrative_ontology:measurement(nds22_tr_t2, nds_2022_pacing_challenge, theater_ratio, 2, 0.62).
narrative_ontology:measurement(nds22_tr_t4, nds_2022_pacing_challenge, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(nds22_be_t0, nds_2022_pacing_challenge, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nds22_be_t2, nds_2022_pacing_challenge, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(nds22_be_t4, nds_2022_pacing_challenge, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nds_2022_pacing_challenge, enforcement_mechanism).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, taiwan_strait_military_balance).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, semiconductor_supply_chain_decoupling).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, us_china_trade_restrictions).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, nato_burden_sharing_asymmetry).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, multilateral_arms_control_authority_erosion).

% DUAL FORMULATION NOTE:
% The pacing challenge doctrine is downstream of China's military modernization (measured separately as distinct empirical claim) but represents a distinct structural constraint centered on US strategic choice and alliance management. The upstream constraint has its own extractiveness reflecting empirical capability assessment; the pacing challenge doctrine has its own extractiveness reflecting the policy response and extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nds_2022_pacing_challenge, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
