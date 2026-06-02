% ============================================================================
% CONSTRAINT STORY: democratic_legitimacy_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_democratic_legitimacy_arbitrage, []).

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
 *   constraint_id: democratic_legitimacy_arbitrage
 *   human_readable: Democratic Legitimacy Arbitrage in Populist Movements
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The democratic legitimacy arbitrage constraint emerges when populist
 *   supporters simultaneously endorse representative democracy (as a source
 *   of legitimacy) and strong-leader governance without parliamentary or
 *   judicial interference (as a mode of action). This creates an arbitrage
 *   opportunity for populist leadership: claim democratic mandate from
 *   electoral victory while dismantling the institutional checks that
 *   representative democracy requires. Survey data across multiple
 *   democracies experiencing populist ascent (Hungary, Poland, Turkey,
 *   Brazil, India, United States) shows this pattern: populist supporters
 *   express high favorability for 'democracy' in the abstract while also
 *   endorsing statements like 'a strong leader who doesn't have to bother
 *   with parliament' or 'the will of the people should not be constrained by
 *   courts.' The simultaneity is the mechanism — if supporters endorsed only
 *   strong-leader governance, the movement would lack democratic cover; if
 *   they endorsed only representative democracy, they would resist
 *   institutional erosion. The arbitrage exploits the gap between the two
 *   endorsements. The constraint's theater_ratio (0.58) reflects that much of
 *   the populist leadership's democratic rhetoric is performative: elections
 *   are held, parliaments convene, courts issue rulings, but the substantive
 *   checks on executive power erode through norm violation, institutional
 *   capture, and strategic non-compliance. The base's identity lock is the
 *   binding mechanism: their identity is constituted through the populist
 *   frame ('we the people' vs 'the corrupt elite'), making exit from the
 *   movement psychologically equivalent to abandoning their political
 *   identity. The constraint is downstream of populist_as_class_realignment
 *   (the prior tangled rope that channels economic grievances into populist
 *   mobilization) but represents a distinct structural phenomenon: the class
 *   realignment creates the base; the legitimacy arbitrage is the mechanism
 *   that enables institutional extraction.
 *
 * KEY AGENTS:
 *   - Populist Base Supporter: Primary target (powerless/identity_locked) — identity fused with populist movement; cannot exit without abandoning political self-concept; bears extraction cost (institutional erosion) while experiencing constraint as liberation
 *   - Opposition Voter: Secondary victim (powerless/trapped) — trapped by electoral outcome; faces suppression of opposition channels and institutional protections; no coordination benefit
 *   - Populist Leadership: Primary beneficiary (institutional/arbitrage) — captures legitimacy from democratic endorsement while dismantling checks; arbitrages between democratic rhetoric and executive action
 *   - Swing Voter: Mixed position (moderate/constrained) — benefits from grievance channeling but bears institutional erosion cost; constrained by dissatisfaction with status quo
 *   - Civil Society Coalition: Organized resistance (organized/mobile) — building alternative accountability mechanisms; sees constraint as temporary crisis with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees recurring pattern in democratic backsliding; identifies structural ambiguity between coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(democratic_legitimacy_arbitrage, 0.48).
domain_priors:suppression_score(democratic_legitimacy_arbitrage, 0.62).
domain_priors:theater_ratio(democratic_legitimacy_arbitrage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, extractiveness, 0.48).
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(democratic_legitimacy_arbitrage, snare).
narrative_ontology:human_readable(democratic_legitimacy_arbitrage, "Democratic Legitimacy Arbitrage in Populist Movements").
narrative_ontology:topic_domain(democratic_legitimacy_arbitrage, "political_economy/comparative_politics/democratic_theory").

domain_priors:requires_active_enforcement(democratic_legitimacy_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(democratic_legitimacy_arbitrage, populist_leadership).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, institutional_checks_and_balances).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, opposition_parties).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, judicial_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULIST BASE SUPPORTER (SNARE) — Identity-locked within the populist frame that defines 'the people' against 'the elite.' Cannot exit without abandoning the identity constructed through movement membership. Experiences the constraint as liberation (strong leader will finally act for us) while bearing the extraction cost (erosion of institutional protections they nominally value). The simultaneity of endorsing both representative democracy and unchecked executive power is not experienced as contradiction but as pragmatic necessity — the system is rigged, so we need someone strong enough to break it. Maximum extraction because the identity lock prevents recognition of the structural cost.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION VOTER (SNARE) — Trapped by the electoral outcome and the institutional erosion that follows. Cannot exit the polity; faces suppression of opposition channels, judicial independence, and parliamentary oversight. Experiences pure extraction: the legitimacy arbitrage enables the populist leadership to claim democratic mandate while dismantling the institutional checks that protect minority rights. No coordination benefit — the constraint exists to suppress alternatives.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POPULIST LEADERSHIP (ROPE) — Primary beneficiary. Experiences the constraint as coordination: the base's simultaneous endorsement of democratic legitimacy and strong-leader governance provides a stable mandate for institutional restructuring. Can arbitrage between democratic rhetoric (we won the election) and executive action (the people demand results, not procedural delays). Net beneficiary — the legitimacy arbitrage is the mechanism that enables extraction from institutional checks without losing democratic cover.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SWING VOTER (TANGLED ROPE) — Constrained by dissatisfaction with status quo institutions but also wary of unchecked executive power. Benefits from the coordination function (populist movement channels legitimate grievances about elite capture) but bears extraction cost (institutional erosion reduces future accountability mechanisms). Mixed experience: the constraint both enables political expression and degrades the system's self-correction capacity.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL SOCIETY COALITION (SCAFFOLD) — Organized agents (NGOs, independent media, judicial reform advocates) see the legitimacy arbitrage as a temporary crisis with a sunset: the contradiction between democratic endorsement and authoritarian practice will eventually surface as the populist leadership's performance fails to match its promises. The coalition is building alternative accountability mechanisms (investigative journalism, legal challenges, grassroots organizing) that bypass the captured institutions. Estimated sunset: 8-15 years for the contradiction to become untenable and for institutional norms to reassert.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the legitimacy arbitrage is a recurring pattern in democratic backsliding: electorally successful movements that claim democratic mandate while eroding institutional checks. The constraint has both coordination function (channels real grievances about elite unresponsiveness) and extraction mechanism (enables institutional capture by conflating electoral victory with unlimited mandate). The analytical observer sees the structural ambiguity: is this a temporary correction of elite capture (coordination) or a stable extraction mechanism (snare)? The answer depends on whether the institutional erosion is reversible.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(democratic_legitimacy_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(democratic_legitimacy_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(democratic_legitimacy_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The populist leadership captures substantial benefit from the legitimacy arbitrage (electoral mandate + institutional restructuring authority) while the base bears the cost of institutional erosion (reduced future accountability, weakened minority protections). The extraction is not maximal because some coordination function exists (the movement does channel real grievances about elite unresponsiveness), but the asymmetry is significant: the leadership benefits structurally while the base's long-term interests (institutional protections) are degraded. Suppression (0.62): Moderate-high and rising. The constraint suppresses alternatives through multiple mechanisms: opposition parties face institutional disadvantages (captured courts, biased media, electoral manipulation); civil society faces legal harassment and funding restrictions; the base's identity lock suppresses internal dissent (questioning the leader = betraying the people). Suppression increases over the interval as institutional capture deepens and norm violations become routine. Theater ratio (0.58): Moderate-high. Democratic forms persist (elections, parliamentary sessions, judicial rulings) but substantive checks erode. The theater is not total (some institutional resistance remains, especially early in the interval) but is substantial and increasing as performative compliance replaces genuine accountability.
 *
 * PERSPECTIVAL GAP:
 *   The populist base supporter sees the constraint as liberation (finally, a leader strong enough to fight the elite) while the opposition voter sees pure extraction (democratic cover for authoritarian practice). The populist leadership sees coordination (the base's mandate enables effective governance) while the civil society coalition sees a temporary crisis (the contradiction will surface and institutions will recover). The swing voter sees mixed coordination and extraction (legitimate grievances channeled but institutions degraded). The analytical observer sees structural ambiguity (is this correcting elite capture or creating populist capture?). The gap is not a disagreement about facts but a difference in structural position: the base's identity lock prevents recognition of the extraction they bear; the leadership's beneficiary position makes the extraction invisible; the opposition's trapped position makes the extraction maximal; the coalition's organized position makes the sunset visible; the swing voter's constrained position makes the trade-off explicit; the analytical observer's civilizational view makes the recurring pattern visible.
 *
 * DIRECTIONALITY LOGIC:
 *   The populist base supporter is identity_locked rather than trapped because the binding mechanism is cognitive/identity-based rather than material. A trapped agent faces external barriers to exit (economic dependency, legal prohibition, physical confinement); an identity_locked agent's identity is constituted through the constraint. The base supporter could structurally exit (vote for opposition, disengage from politics, emigrate) but cannot do so without abandoning the identity frame that defines their political self-concept. The identity lock is the populist frame itself: 'we the people' vs 'the corrupt elite' is not just a political position but an identity claim. Exit would require becoming a different kind of person — someone who accepts elite legitimacy, someone who trusts institutions, someone who is not 'one of us.' This is the diagnostic signal for identity_locked: the agent has structural mobility but functional immobility due to identity fusion. The populist leadership is institutional/arbitrage because they hold institutional power and can exit the constraint without cost (if the legitimacy arbitrage fails, they retain wealth, connections, and international mobility). The opposition voter is powerless/trapped because they lack both institutional power and exit options (cannot leave the polity, cannot change the electoral outcome, face suppression of opposition channels). The swing voter is moderate/constrained because they have some agency (can shift electoral support) but face high costs to exit (dissatisfaction with all available options). The civil society coalition is organized/mobile because they have collective agency and can build alternative structures. The analytical observer is analytical/analytical by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the legitimacy arbitrage is simultaneously coordination (channels real grievances) and extraction (enables institutional capture). The mandatrophy is not 'which type is correct?' but 'which structural position are you measuring from?' The base supporter's experience (liberation) is genuine but incomplete (does not account for long-term institutional cost). The opposition voter's experience (pure extraction) is genuine but partial (does not account for the coordination function that makes the movement electorally successful). The leadership's experience (coordination) is genuine but self-serving (does not account for the extraction they capture). The civil society coalition's experience (temporary crisis) is genuine but uncertain (depends on whether institutional resilience holds). The swing voter's experience (mixed) is the most structurally complete (recognizes both coordination and extraction) but also the most unstable (the trade-off may shift over time). The analytical observer's experience (structural ambiguity) is the most comprehensive but also the most abstract (does not capture the lived experience of any particular agent). All perspectives are valid readings of the same structural data; the presheaf over the observation site is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contradiction_recognition_threshold,
    'At what point does the base recognize the contradiction between endorsing representative democracy and supporting unchecked executive power?',
    'Longitudinal survey data tracking simultaneous endorsement rates over time; correlation with populist leadership performance failures or institutional crisis events',
    'If recognition occurs within 5-10 years: scaffold perspective confirmed — the arbitrage is temporary. If recognition never occurs or takes >20 years: snare perspective confirmed — the identity lock is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contradiction_recognition_threshold, empirical, 'Timeline for base recognition of democratic contradiction').

omega_variable(
    institutional_resilience_capacity,
    'Do institutional checks (judiciary, parliament, media) retain sufficient independence to constrain executive overreach, or has the legitimacy arbitrage enabled irreversible capture?',
    'Comparative analysis of institutional independence metrics (judicial rulings against executive, parliamentary oversight effectiveness, media pluralism indices) across populist vs non-populist regimes; identification of tipping points where erosion becomes self-reinforcing',
    'If institutions retain resilience: tangled rope from more perspectives (coordination with extractive overhead). If capture is irreversible: snare from more perspectives (pure extraction mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_resilience_capacity, empirical, 'Whether institutional checks can recover from populist erosion').

omega_variable(
    elite_capture_vs_populist_capture,
    'Is the legitimacy arbitrage correcting a prior elite capture of institutions (coordination function) or creating a new populist capture (extraction function)?',
    'Historical analysis of institutional responsiveness pre- and post-populist ascent; comparison of policy outcomes for median voter vs elite interests vs populist leadership interests',
    'If correcting elite capture: rope or tangled rope from more perspectives (genuine coordination with some extraction). If creating new capture: snare from more perspectives (extraction mechanism replacing prior extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_capture_vs_populist_capture, conceptual, 'Whether populism corrects or replaces elite capture').

omega_variable(
    identity_lock_mechanism,
    'Is the base''s identity lock cognitive (internalized framing that strong leader = democracy) or strategic (conscious acceptance of contradiction to achieve policy goals)?',
    'Qualitative interviews and experimental survey designs testing whether supporters recognize the contradiction when framed differently; analysis of whether recognition changes endorsement patterns',
    'If cognitive: identity_locked exit is accurate — the base cannot see the contradiction from within their frame. If strategic: constrained exit is more accurate — the base sees the contradiction but accepts it as a necessary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity lock is cognitive or strategic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(democratic_legitimacy_arbitrage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dem_arb_tr_t0, democratic_legitimacy_arbitrage, theater_ratio, 0, 0.45).
narrative_ontology:measurement(dem_arb_tr_t3, democratic_legitimacy_arbitrage, theater_ratio, 3, 0.52).
narrative_ontology:measurement(dem_arb_tr_t6, democratic_legitimacy_arbitrage, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(dem_arb_be_t0, democratic_legitimacy_arbitrage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dem_arb_be_t3, democratic_legitimacy_arbitrage, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(dem_arb_be_t6, democratic_legitimacy_arbitrage, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(dem_arb_su_t0, democratic_legitimacy_arbitrage, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(dem_arb_su_t3, democratic_legitimacy_arbitrage, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(dem_arb_su_t6, democratic_legitimacy_arbitrage, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(democratic_legitimacy_arbitrage, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of populist_as_class_realignment (the prior tangled rope that channels economic grievances into populist mobilization). The class realignment creates the base; the legitimacy arbitrage is the mechanism that enables institutional extraction. The two constraints have different epsilon values (class realignment ε ≈ 0.35-0.40 reflecting mixed coordination and extraction in economic grievance channeling; legitimacy arbitrage ε = 0.48 reflecting higher extraction in institutional erosion) and different victim sets (class realignment victims: displaced workers, precarious labor; legitimacy arbitrage victims: institutional checks, opposition parties, judicial independence). They are linked but structurally distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
