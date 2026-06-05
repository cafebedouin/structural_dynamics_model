% ============================================================================
% CONSTRAINT STORY: us_iran_drone_conflict
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_iran_drone_conflict, []).

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
 *   constraint_id: us_iran_drone_conflict
 *   human_readable: US-Iran Drone Conflict and Escalation Constraint
 *   domain: geopolitical/military/sanctions
 *
 * SUMMARY:
 *   The US-Iran drone conflict in the Middle East represents a hybrid
 *   extraction-coordination constraint masquerading as natural geopolitical
 *   inevitability. The US claims to have shot down an Iranian drone
 *   approaching a US military base in Syria. This incident sits within a
 *   larger structure of sanctions, military encirclement, proxy warfare, and
 *   strategic signaling between the US and Iran. The constraint operates at
 *   multiple structural levels: (1) military-technical (drone superiority,
 *   force posturing), (2) institutional (Pentagon justification for regional
 *   presence, IRGC deterrent posture), (3) economic (sanctions enforcement
 *   benefiting certain US sectors), (4) civilian-suffering (Iraqi and
 *   regional populations bearing costs of proxy conflicts without
 *   representation), and (5) diplomatic (implicit escalation protocols that
 *   prevent but also perpetuate the crisis). The constraint exhibits all six
 *   DR types from different perspectives, revealing how a geopolitical
 *   conflict can simultaneously appear as legitimate defense, enforced
 *   extraction, temporary problem with diplomatic sunset, degraded deterrence
 *   ritual, mixed coordination-coercion, and false natural law.
 *
 * KEY AGENTS:
 *   - US Military Command (CENTCOM): Primary beneficiary (institutional/arbitrage) — maintains operational freedom, regional presence justification, force posture credibility
 *   - Iranian Leadership / IRGC: Primary victim + partial beneficiary (powerful/constrained) — constrained by sanctions and military encirclement; benefits from deterrent posture and domestic legitimacy from resistance narrative
 *   - Iraqi Civilian Population and State: Primary victim (powerless/trapped) — territorial host to conflict, bears drone strike costs, cannot exit or control escalation
 *   - Proxy Militias (Houthis, PMF, IRGC-QF): Secondary actor (organized/constrained) — carry out operations; degree of autonomy affects attribution and controllability
 *   - Escalation Prevention Institutions (UN, JCPOA signatories): Organized agent (organized/constrained) — attempt to manage crisis through diplomacy; see sunset pathway via sanctions relief and confidence-building
 *   - Defense Contractors / Military-Industrial Complex: Structural beneficiary (institutional/arbitrage) — profits from sustained regional tension and force modernization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (sanctions, military positioning, domestic politics) as immutable geopolitical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_iran_drone_conflict, 0.58).
domain_priors:suppression_score(us_iran_drone_conflict, 0.72).
domain_priors:theater_ratio(us_iran_drone_conflict, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_iran_drone_conflict, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_iran_drone_conflict, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_iran_drone_conflict, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_iran_drone_conflict, tangled_rope).
narrative_ontology:human_readable(us_iran_drone_conflict, "US-Iran Drone Conflict and Escalation Constraint").
narrative_ontology:topic_domain(us_iran_drone_conflict, "geopolitical/military/sanctions").

domain_priors:requires_active_enforcement(us_iran_drone_conflict).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_iran_drone_conflict, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_iran_drone_conflict, regional_proxy_powers).
narrative_ontology:constraint_victim(us_iran_drone_conflict, civilian_populations_regional).
narrative_ontology:constraint_victim(us_iran_drone_conflict, escalation_prevention_regime).
narrative_ontology:constraint_victim(us_iran_drone_conflict, iraqi_state_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRAQI CIVILIANS AND STATE SOVEREIGNTY (SNARE) — Iraq's sovereignty is trapped between US military presence and Iranian regional power. Iraqi civilians bear the costs of drone strikes, proxy warfare, and US bases on their territory with minimal ability to exit or influence escalation. d≈0.93, f(d)≈1.40, σ=0.9 → χ≈0.59.
constraint_indexing:constraint_classification(us_iran_drone_conflict, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: IRANIAN LEADERSHIP / IRGC (TANGLED ROPE) — Constrained by US sanctions, military encirclement, and threat of strike. Extraction: must maintain deterrent posture and proxy networks as insurance against regime change. Coordination: participates in (coerced) escalation ladder signaling and implicit de-escalation protocols. d≈0.68, f(d)≈1.00, σ=1.1 → χ≈0.64.
constraint_indexing:constraint_classification(us_iran_drone_conflict, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US MILITARY COMMAND / CENTCOM (ROPE) — Benefits from drone superiority, technical advantage, operational freedom in regional airspace, and continued security justification. Experiences constraint as coordination: managing escalation signaling, maintaining deterrent credibility, and calibrating force to avoid uncontrolled spiraling. d≈0.12, f(d)≈0.05, σ=1.2 → χ≈0.03. Net beneficiary.
constraint_indexing:constraint_classification(us_iran_drone_conflict, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESCALATION PREVENTION REGIME (SCAFFOLD) — UN Security Council, JCPOA signatories, and diplomatic channels see the drone conflict as a temporary coordination failure with potential sunset through diplomatic de-escalation (JCPOA restoration, sanctions relief, confidence-building measures). d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.14. Low effective extraction because these institutions have agency and visibility; organized actors can construct alternative pathways.
constraint_indexing:constraint_classification(us_iran_drone_conflict, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL DETERRENCE / MAD FRAMEWORK (PITON) — Mutual Assured Destruction logic and Cold War-era escalation ladders persist in US-Iran strategic planning despite changed technological and political context. Drone strikes serve partly as performative deterrent signaling rather than functional security measures. theater_ratio≈0.68 reflects that much of the rhetorical and operational activity (public claims, base alerts, force posturing) is theatrical rather than functional — actual escalation control relies on implicit communication channels, not on declared doctrine. The framework persists through institutional inertia and because alternatives haven't fully matured.
constraint_indexing:constraint_classification(us_iran_drone_conflict, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT RISK) — From a civilizational view, the constraint appears as an immutable consequence of geopolitical multipolarity: regional powers with grievances will always contest hegemonic powers; drone technology enables low-cost probing; escalation spirals are inherent to security dilemmas. However, the structural data (ε=0.58, suppression=0.72, theater=0.68, requires_active_enforcement=true) contradicts a pure mountain classification. This reveals the false summit: what appears as natural geopolitical law is actually a contingent institutional arrangement maintained by sanctions enforcement, military positioning, and domestic political incentives for conflict. ε≤0.25 would be required for true mountain; ε=0.58 indicates significant extraction and engineered suppression.
constraint_indexing:constraint_classification(us_iran_drone_conflict, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_iran_drone_conflict_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_iran_drone_conflict, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_iran_drone_conflict, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_iran_drone_conflict, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_iran_drone_conflict, TR),
    TR >= 0.70.

:- end_tests(us_iran_drone_conflict_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from Iraqi sovereignty (US bases without Iraqi veto), from Iranian strategic autonomy (sanctions regime forcing deterrent investments), and from regional populations (drone strikes, proxy warfare). However, it is not pure extraction — the US does provide security services (counterterrorism), deterrence works (no all-out war), and some regional actors benefit from weapons flows and reconstruction contracts. The 0.58 reflects that the primary beneficiary (US military establishment) captures asymmetric advantage during the constraint period, but alternatives exist (diplomacy, sanctions relief, JCPOA restoration). Suppression (0.72): High. Significant barriers prevent exit or challenge: (1) Military: US drone and air superiority; Iranian asymmetric capabilities (proxies) that are distributed and hard to counter. (2) Economic: Sanctions regime enforced globally; Iranian banking system constrained; alternative trade routes limited. (3) Political: US domestic politics favor military posture; Iranian regime legitimacy tied to resistance narrative; proxy forces operate with partial autonomy. (4) Informational: Competing claims about drone identity and intent; limited independent verification; state-controlled media dominates narratives. Theater ratio (0.68): Moderate-high. Much activity is performative: public threat statements serve signaling functions rather than operational necessity; force posturing (base alerts, carrier movements) communicates resolve; rhetoric about deterrence conveys messages more than doctrine. However, the threat is genuine — drone/missile capabilities are real, casualties are possible, and escalation can occur. The theater reflects that signaling and coordination now dominate over actual combat operations, yet the signaling apparatus could break down. Trending upward (0.42→0.68) indicates increasing reliance on rhetoric and symbolism as actual incidents become rarer.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximal perspectival divergence. (1) US military sees Rope — legitimate coordination of deterrence and escalation signaling; maintaining credible defense posture solves the problem of regional stability under multipolarity. (2) Iranian leadership sees Tangled Rope — forced into deterrent posture by sanctions and encirclement, but the deterrent strategy (proxies, missiles, rhetoric) also provides domestic legitimacy and regional influence; mixed extraction and coordination. (3) Iraqi civilians see Snare — no choice in hosting the conflict, no veto over operations, no seat at negotiating table; pure victimization. (4) Escalation prevention institutions see Scaffold — recognize diplomatic pathways (JCPOA restoration, confidence-building) with potential 5-10 year sunset as norms stabilize. (5) Cold War deterrence framework sees Piton — mutual deterrence rhetoric persists (MAD logic) though technology, alliances, and proxy structures have changed; the framework is maintained through institutional inertia. (6) Civilizational analyst risks seeing Mountain — geopolitical inevitability, multipolarity producing conflict — but the structural data reveals this is a false summit: the constraint requires active enforcement (sanctions, military positioning, proxy funding), has significant extractiveness (not a law of nature), and shows theater ratio rising (performative activity increasing). If it were a true natural law, extractiveness would be ≤0.25 and suppression would be minimal (only inherent friction, not engineered barriers).
 *
 * DIRECTIONALITY LOGIC:
 *   US Military (CENTCOM): Beneficiary + arbitrage exit → d≈0.10, f(d)≈0.02. Slight beneficiary; benefits from regional security mission justification, force projection capability, and deterrent credibility. Iranian Leadership (IRGC): Victim + constrained exit → d≈0.68, f(d)≈1.00. Constrained by sanctions (cannot unilaterally lift), military encirclement (cannot exit region), and domestic politics (cannot abandon resistance narrative). Partial beneficiary status (deterrence legitimacy, regional influence) reduces d somewhat from pure victim. Iraqi Civilians: Victim + trapped exit → d≈0.93, f(d)≈1.40. No option to refuse hosting US bases or prevent drone strikes; no leverage in escalation decisions; maximum extraction. Escalation Prevention Institutions: Organized + constrained exit → d≈0.40, f(d)≈0.40. Have agency (can broker talks) and visibility (can propose alternatives), but constrained by great-power politics and state sovereignty; can see a path forward (diplomacy) hence lower effective extraction. Deterrence Framework (institutional tradition): Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Institutional frame benefits from crisis perpetuation (justifies military budgets, validates Cold War planning) and can select how to measure success; net beneficiary through selection bias.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that extractiveness and coordination are not opposed properties but can coexist in a single structure. (1) Genuine coordination: Deterrence signaling, escalation protocols, implicit de-escalation channels all work — there has been no all-out war. The US and Iran have communicated through backchannel, and strikes have been calibrated. (2) Genuine extraction: Iraqi civilians bear costs without consent. US military captures disproportionate budget/influence. Iranian populations suffer sanctions. Regional stability becomes a commodity traded among elites. (3) The Tangled Rope classification captures both: it is a coordination mechanism (deterrence works, prevents spiral into total war) AND an extraction mechanism (distributes costs asymmetrically, prevents exit by weaker parties). Neither aspect can be removed without changing the constraint's identity. If you remove the coordination function (break deterrence protocols), the spiral becomes uncontrolled and extraction becomes even higher (Snare). If you remove the extraction function (equalize power, remove sanctions), the constraint dissolves into pure Rope. The mandatrophy is resolved by recognizing that real-world constraints in conflict domains combine both functions, and the engine's classification reflects the mixed reality. The risk of false natural law (mountain perspective) is precisely that it naturalizes this mixed extraction-coordination as inevitable, when it is contingent on maintained institutions (sanctions, military positioning, proxy funding). Sunset pathway: JCPOA restoration + sanctions relief + diplomatic confidence-building → constraint transitions to pure Rope or dissolves entirely (12-20 year timeline). This is the Scaffold classification's basis: the open-science coalition of international institutions sees a realistic path to sunset, even if it is not currently being pursued.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drone_provenance_verification,
    'Was the downed drone actually Iranian, and was it actually approaching the US base with hostile intent, or is the claim part of escalation theater?',
    'Independent technical analysis of wreckage; satellite imagery corroboration; third-party testimony from Iraqi authorities and regional observers; flight path reconstruction',
    'If verified hostile approach: constraint legitimizes as defensive coordination (Rope classification stronger). If unverified or misidentified: constraint is primarily extractive signaling (Snare/Tangled Rope from all perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drone_provenance_verification, empirical, 'Verification of drone identity and hostile intent').

omega_variable(
    proxy_network_attribution_chain,
    'Does the Iranian government directly control the drone operations, or are these proxy militias (Houthis, PMF factions) acting with degrees of autonomy?',
    'Intelligence analysis linking command structures; communications intercepts; comparative timing with Iranian official statements; escalation correlation with IRGC policy shifts',
    'If direct control: Iran is active belligerent (escalation spiral more controllable through state-level communication). If proxy autonomy: deterrence fails against distributed actors; suppression increases as control mechanisms degrade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_network_attribution_chain, empirical, 'Attribution of drone operations to Iranian state vs proxy autonomy').

omega_variable(
    domestic_political_incentives_us,
    'To what degree does the US military''s operational tempo in the region reflect genuine security requirements vs. institutional interests in sustaining the regional mission and defense budget allocations?',
    'Comparative analysis of threat assessments before/after incidents; correlation with Congressional budget cycles; internal DoD planning documents; alternative security analyses from independent institutes',
    'If threat-driven: constraint is legitimate deterrent coordination. If institution-driven: extractiveness rises (Snare/Tangled Rope); theater_ratio increases; suppression reflects self-perpetuating conflict machinery rather than genuine defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_political_incentives_us, preference, 'Whether US military tempo reflects security needs or institutional interests').

omega_variable(
    escalation_spiral_controllability,
    'Are implicit de-escalation protocols sufficient to prevent uncontrolled spiraling into broader regional war?',
    'Game-theoretic analysis of signaling robustness; historical precedents (1973 war, 1981-88 tanker war, recent proxy exchanges); backchannel communication capacity; third-party mediation effectiveness (Oman, Qatar channels)',
    'If controllable: constraint resolves as Scaffold (temporary, with sunset via diplomacy). If uncontrollable: Mountain (geopolitical logic makes war inevitable) or high-extraction Snare (elites profit from perpetual crisis while populations suffer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_spiral_controllability, conceptual, 'Whether escalation protocols prevent uncontrolled spiraling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_iran_drone_conflict, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usiran_tr_t0, us_iran_drone_conflict, theater_ratio, 0, 0.42).
narrative_ontology:measurement(usiran_tr_t6, us_iran_drone_conflict, theater_ratio, 6, 0.58).
narrative_ontology:measurement(usiran_tr_t12, us_iran_drone_conflict, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(usiran_be_t0, us_iran_drone_conflict, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(usiran_be_t6, us_iran_drone_conflict, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(usiran_be_t12, us_iran_drone_conflict, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_iran_drone_conflict, enforcement_mechanism).
narrative_ontology:affects_constraint(us_iran_drone_conflict, sanctions_enforcement_regime).
narrative_ontology:affects_constraint(us_iran_drone_conflict, regional_proxy_warfare_ecosystem).
narrative_ontology:affects_constraint(us_iran_drone_conflict, iraqi_state_sovereignty_erosion).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the sanctions regime (upstream constraint) which creates the economic suppression that forces Iran into deterrent posture. It is upstream of regional proxy warfare which it enables through military technology flows. It directly affects Iraqi sovereignty by imposing foreign military presence without Iraqi veto.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_iran_drone_conflict, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
