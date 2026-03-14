% ============================================================================
% CONSTRAINT STORY: us_sanctions_coordination_requirement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sanctions_coordination_requirement, []).

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
 *   constraint_id: us_sanctions_coordination_requirement
 *   human_readable: US Sanctions Coordination Requirement
 *   domain: geopolitical/economic_statecraft
 *
 * SUMMARY:
 *   The US sanctions coordination requirement operates across multiple
 *   structural levels: as a coercive mechanism targeting foreign governments
 *   through their civilian populations (snare from target perspective), as an
 *   alignment mechanism for allied states (rope for aligned partners), as a
 *   mixed coordination-extraction hybrid for neutral third parties forced
 *   into compliance (tangled rope), and as a strategic instrument for the US
 *   foreign policy apparatus (tangled rope with beneficiary position). The
 *   constraint exhibits rising theater over the 30-year interval (0.35 to
 *   0.62) as the sanctions bureaucracy expanded to cover new domains (human
 *   rights, narcotics, terrorism, election interference) and implementation
 *   mechanisms became more elaborate. Simultaneously, base extractiveness
 *   increased (0.42 to 0.65) as accumulated sanctions layered onto existing
 *   regimes, compounding civilian costs without proportional diplomatic
 *   results. The constraint's dual nature—simultaneously coordinating allied
 *   pressure and extracting compliance through punishment—makes it a
 *   diagnostic exemplar of tangled rope classification. From the target
 *   state's perspective, extraction is severe (snare); from aligned partners'
 *   perspective, coordination benefits dominate (rope); from neutral third
 *   parties, the mix of coordination access and extraction pressure is
 *   explicit (tangled rope). The analytical observer risks naturalizing this
 *   as an immutable feature of international relations (mountain), but the
 *   structural data reveals it as contingent on dollar hegemony, allied
 *   compliance structures, and the absence of effective multilateral
 *   alternatives.
 *
 * KEY AGENTS:
 *   - US Foreign Policy Apparatus: Primary beneficiary (institutional/constrained) — enforcer and architect of sanctions regime; captures diplomatic signaling power and economic flows
 *   - Target State Civilian Population: Primary victim (powerless/trapped) — bears extraction cost through inflation, medical shortages, economic collapse with no exit or decision-making participation
 *   - Allied Coalition Partners: Secondary beneficiary (organized/mobile) — experience coordination benefits (security guarantees, intelligence, market access) with minimal extraction costs; can technically exit but face geopolitical consequences
 *   - Neutral Third-Party States: Secondary victim (moderate/constrained) — forced to abandon profitable trade relationships; face implicit coercion despite lacking stake in underlying conflict
 *   - Sanctions Bureaucracy: Institutional actor (institutional/arbitrage) — operates enforcement infrastructure; benefits from expansion and increased complexity; maintains performative compliance structures
 *   - International Sanctions Reform Coalition: Organized opposition (organized/constrained) — NGOs and progressive states building multilateral alternatives; see sunset path through international consensus mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_coordination_requirement, 0.58).
domain_priors:suppression_score(us_sanctions_coordination_requirement, 0.62).
domain_priors:theater_ratio(us_sanctions_coordination_requirement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_coordination_requirement, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_sanctions_coordination_requirement, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_sanctions_coordination_requirement, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sanctions_coordination_requirement, tangled_rope).
narrative_ontology:human_readable(us_sanctions_coordination_requirement, "US Sanctions Coordination Requirement").
narrative_ontology:topic_domain(us_sanctions_coordination_requirement, "geopolitical/economic_statecraft").

domain_priors:requires_active_enforcement(us_sanctions_coordination_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sanctions_coordination_requirement, us_foreign_policy_apparatus).
narrative_ontology:constraint_beneficiary(us_sanctions_coordination_requirement, allied_governments).
narrative_ontology:constraint_victim(us_sanctions_coordination_requirement, target_state_civilian_population).
narrative_ontology:constraint_victim(us_sanctions_coordination_requirement, neutral_third_parties).
narrative_ontology:constraint_victim(us_sanctions_coordination_requirement, sanctioning_state_domestic_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET STATE CIVILIANS (SNARE) — Trapped by geography and citizenship. Bears full cost of sanctions (medical shortages, economic collapse, inflation) with no exit option or participation in decision-making. Experienced extraction is maximal. The constraint exists specifically to inflict pressure on this population, extracting economic compliance through collective punishment.
constraint_indexing:constraint_classification(us_sanctions_coordination_requirement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEUTRAL THIRD-PARTY STATE (TANGLED ROPE) — Constrained by pressure to enforce sanctions despite lacking direct stake in the conflict. Experiences both coordination benefits (access to US markets, security partnerships, technology transfers) and extraction costs (forced abandonment of profitable trade relationships, economic retaliation threats). Significant agency but high cost to non-compliance.
constraint_indexing:constraint_classification(us_sanctions_coordination_requirement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALIGNED COALITION PARTNERS (ROPE) — NATO allies, strategic partners (Japan, South Korea, Australia) experience sanctions coordination primarily as alignment mechanism. Coordination benefits (security guarantees, intelligence sharing, market access) exceed extraction costs. Exit is technically possible but carries geopolitical cost; framed as voluntary coordination rather than coercion.
constraint_indexing:constraint_classification(us_sanctions_coordination_requirement, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: US FOREIGN POLICY APPARATUS (TANGLED ROPE) — Primary beneficiary and enforcer. Experiences the constraint as both coordination mechanism (aligning allied pressure, signaling resolve) and extraction tool (concentrating enforcement capacity, maintaining sanctions bureaucracy, directing economic flows). Genuine coordination function plus asymmetric benefit. Active enforcement required; extraction flows toward this institutional actor.
constraint_indexing:constraint_classification(us_sanctions_coordination_requirement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SANCTIONS BUREAUCRACY (PITON) — Treasury Department, State Department, OFAC operate the sanctions infrastructure. Theater ratio high (compliance certifications, audit trails, regulatory documentation) relative to actual enforcement capability. Original function (strategic coercion) degraded; maintained through institutional inertia and expanded to include sanctions for human rights, drug trafficking, terrorism. The bureaucracy persists because alternatives haven't fully replaced it, and institutional actors benefit from its expansion.
constraint_indexing:constraint_classification(us_sanctions_coordination_requirement, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL SANCTIONS REFORM COALITION (SCAFFOLD) — NGOs, humanitarian organizations, some progressive states see sanctions as a temporary coordination failure with sunset logic: multilateral alternatives (UN-sanctioned embargoes, conditional trade agreements, humanitarian carve-outs) could replace unilateral US sanctions. Low effective extraction from this perspective because the coalition sees an exit path and has agency to build alternatives. Theater remains high (performative humanitarian exceptions) but function is improving.
constraint_indexing:constraint_classification(us_sanctions_coordination_requirement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational lens, economic interdependence inevitably creates coordination problems, and sanctions are inherent to state competition. Pressure mechanisms between unequal powers are immutable features of international relations. However, this perspective risks naturalizing what is actually a contingent institutional arrangement (unilateral US enforcement capacity, dollar hegemony, allied compliance structures). The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(us_sanctions_coordination_requirement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_coordination_requirement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sanctions_coordination_requirement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sanctions_coordination_requirement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sanctions_coordination_requirement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sanctions_coordination_requirement, TR),
    TR >= 0.70.

:- end_tests(us_sanctions_coordination_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint produces significant asymmetric flows: target state bears civilizational-scale costs (collapsed healthcare, hyperinflation, migration waves) while US maintains economic access and geopolitical leverage. However, extractiveness is not at snare level (0.66+) because the mechanism includes genuine coordination functions (allied alignment, strategic signaling, human rights norms enforcement). The upward trend from 0.42 to 0.65 reflects sanctions layering and expansionism without corresponding diplomatic success. Suppression (0.62): High. Multiple barriers prevent exit: target states cannot leave their geography, currency controls restrict private movement, diplomatic isolation limits negotiating power, and military retaliation is asymmetric. Allied states face implicit pressure to comply (security guarantee conditioning). Theater ratio (0.55 to 0.62): Moderate-high and rising. Original function (diplomatic pressure for behavior change) is increasingly obscured by expanded compliance theater: humanitarian exemptions that remain blocked, OFAC compliance audits, sanctions list maintenance, certification processes that show action without proportional effect. The theater ratio increase tracks the bureaucracy's expansion into secondary domains.
 *
 * PERSPECTIVAL GAP:
 *   Six distinct experiences of the same structural phenomenon. The snare perspective (target civilians) and rope perspective (allied partners) contradict in their fundamental classification; this contradiction is not a measurement error but a structural feature. The tangled rope classifications from different positions (US apparatus as beneficiary-enforcer vs third parties as victims) represent the constraint's hybrid nature. The piton perspective (bureaucracy) reveals that enforcement mechanisms have become decoupled from strategic purpose. The scaffold perspective shows how multilateral alternatives could provide sunset mechanics. The mountain perspective risks false naturalization. The perspectival gap is the entire analytical value: it demonstrates that 'sanctions coordination' is not a single constraint type but a presheaf over multiple observational positions, and the distribution of classifications reveals the extraction pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality derives from explicit beneficiary/victim declarations plus power-level and exit-option combinations. US apparatus as institutional beneficiary with constrained exit derives low d (extraction flows toward this actor). Target civilians as powerless victims with trapped exit derive high d (extraction flows away from this actor). Allied partners with mobile exit despite moderate power derive moderate d (mixed coordination and extraction). Neutral states forced into compliance despite moderate power derive elevated d (constrained exit despite not being victims by original design). The directionality overrides are not needed here; the canonical derivation chain produces accurate differentiation across the institutional and individual perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing how tangled rope classification emerges from the genuine coexistence of coordination and extraction functions. The US apparatus genuinely coordinates allied pressure (rope-like function) while simultaneously extracting economic compliance and geopolitical leverage (snare-like extraction). From the US perspective, these functions are inseparable — the mechanism that coordinates allies (security guarantee conditioning) IS the same mechanism that extracts from neutral parties (implicit pressure to comply or lose market access). The mandatrophy is resolved by recognizing that tangled rope is precisely the classification for mechanisms that perform both functions with irreducible asymmetry. The snare classification from the target perspective is not a contradiction; it is the structural reality experienced by agents outside the coordination coalition. The scaffold classification for reform advocates reveals a genuine exit pathway (multilateral alternatives) that could shift the entire mechanism toward pure coordination if implemented. The piton classification signals that the bureaucratic enforcement apparatus has decoupled from strategic purpose — a diagnostic flag for potential degradation. The mountain naturalization is explicitly flagged as false: the mechanism depends on contingent institutions (dollar hegemony, allied structures) not immutable laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_vs_compliance_gap,
    'Do sanctions achieve their stated diplomatic objectives or primarily enforce compliance signaling?',
    'Comparison of stated US objectives vs. demonstrated behavioral change in target state; correlation between sanctions intensity and policy concessions',
    'If effective: classification shifts toward Tangled Rope (genuine coordination + extraction). If ineffective: classification shifts toward Snare (pure extraction with minimal coordination function) from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_vs_compliance_gap, empirical, 'Whether sanctions achieve diplomatic objectives or merely enforce compliance signals').

omega_variable(
    humanitarian_carve_out_adequacy,
    'Do humanitarian exemptions (food, medicine, medical devices) genuinely reach civilian populations or remain blocked by implementation barriers?',
    'On-ground assessment of medical and food availability in target state; correlation between exemption policy and actual supply flows',
    'If adequate: suppression metric should be lower (0.40-0.50 range). If inadequate: suppression metric confirmed at 0.62+ and snare classification stronger from target civilian perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_carve_out_adequacy, empirical, 'Adequacy of humanitarian carve-outs in sanctions regimes').

omega_variable(
    dollar_hegemony_dependency,
    'How much does sanctions enforcement capacity depend on dollar hegemony and SWIFT access denial? Would alternative payment systems (CIPS, SPFS, blockchain) significantly reduce enforcement power?',
    'Structural analysis of payment flow dependencies; comparison of sanctions effectiveness in dollar vs non-dollar trade flows; modeling of alternative system adoption scenarios',
    'If highly dependent on dollar hegemony: classification is contingent on current monetary order (not mountain). If relatively independent: sanctions mechanism is more structurally robust. Affects whether scaffold sunset is plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dollar_hegemony_dependency, empirical, 'Structural dependency of sanctions enforcement on dollar hegemony').

omega_variable(
    coalition_compulsion_vs_coordination,
    'Are allied governments genuinely coordinating for shared security outcomes or being compelled through implicit US coercion?',
    'Analysis of allied public statements vs private communications; study of cases where allies violated sanctions; comparison of sanctions cost to allied economies vs benefit from US security guarantees',
    'If coordination: allied perspectives shift toward Rope. If compulsion: allied perspectives shift toward Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_compulsion_vs_coordination, conceptual, 'Whether allied compliance is genuine coordination or implicit coercion').

omega_variable(
    counter_sanctions_retaliation_asymmetry,
    'Can target states impose meaningful counter-sanctions or retaliation, or is the asymmetry so severe that retaliation is theater?',
    'Assessment of target state economic and military retaliation capacity; measurement of counter-sanctions impact on US interests; analysis of cyber/proxy retaliation effectiveness',
    'If meaningful retaliation possible: extraction asymmetry is lower than claimed. If retaliation is theater: extraction asymmetry confirmed and snare classification stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counter_sanctions_retaliation_asymmetry, empirical, 'Asymmetry in counter-sanctions and retaliation capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sanctions_coordination_requirement, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sanc_tr_t0, us_sanctions_coordination_requirement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sanc_tr_t10, us_sanctions_coordination_requirement, theater_ratio, 10, 0.48).
narrative_ontology:measurement(sanc_tr_t20, us_sanctions_coordination_requirement, theater_ratio, 20, 0.55).
narrative_ontology:measurement(sanc_tr_t30, us_sanctions_coordination_requirement, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(sanc_be_t0, us_sanctions_coordination_requirement, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sanc_be_t10, us_sanctions_coordination_requirement, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sanc_be_t20, us_sanctions_coordination_requirement, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(sanc_be_t30, us_sanctions_coordination_requirement, base_extractiveness, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sanctions_coordination_requirement, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_sanctions_coordination_requirement, 0.12).
narrative_ontology:affects_constraint(us_sanctions_coordination_requirement, dollar_hegemony_enforcement).
narrative_ontology:affects_constraint(us_sanctions_coordination_requirement, multilateral_governance_alternative).
narrative_ontology:affects_constraint(us_sanctions_coordination_requirement, alliance_cohesion_mechanism).

% DUAL FORMULATION NOTE:
% US sanctions coordination requirement decomposes into three structurally distinct constraints: the enforcement mechanism (this story, ε=0.58, tangled rope), dollar hegemony dependency (upstream constraint, ε=0.15, rope enabling sanctions leverage), and multilateral governance alternatives (downstream constraint, ε=0.35, scaffold replacing unilateral enforcement). The three are linked by network edges: dollar hegemony enables this constraint's extraction capacity; this constraint's degradation creates space for multilateral alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_sanctions_coordination_requirement, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
