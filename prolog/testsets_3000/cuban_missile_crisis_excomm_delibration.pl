% ============================================================================
% CONSTRAINT STORY: cuban_missile_crisis_excomm_delibration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cuban_missile_crisis_excomm_delibration, []).

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
 *   constraint_id: cuban_missile_crisis_excomm_delibration
 *   human_readable: The ExComm Multi-Channel Deliberation Protocol
 *   domain: political/military
 *
 * SUMMARY:
 *   The Cuban Missile Crisis (October 1962) presented an unprecedented
 *   challenge: nuclear-armed Soviet missiles detected 90 miles from U.S.
 *   coast, with decision timelines compressed to hours. President Kennedy
 *   established ExComm — a multi-channel deliberation structure bringing
 *   together State Department, Defense, Joint Chiefs, CIA, and close advisors
 *   — to process intelligence, generate options, and recommend courses of
 *   action. The constraint is not merely the crisis itself, but the
 *   institutional protocol created to manage it: the structured deliberation
 *   format that simultaneously enabled coordinated decision-making and
 *   systematically excluded Congress from war-powers authority. What began as
 *   emergency necessity in October 1962 (genuine nuclear crisis requiring
 *   rapid decision-making) persisted as institutional inertia in subsequent
 *   administrations, accruing extractive properties as the original
 *   justification atrophied. The protocol exhibits all six constraint types
 *   depending on observer position: Congress sees exclusion from war powers
 *   (snare), cabinet sees mixed access-and-subordination (tangled rope),
 *   presidency sees enabling coordination (rope), military sees civilian
 *   control with constrained voice (tangled rope), intelligence sees
 *   temporary emergency role (scaffold), institutional stability sees
 *   degraded theater (piton), and the civilizational analyst risks
 *   naturalizing nuclear decision speed as immutable law.
 *
 * KEY AGENTS:
 *   - President Kennedy: Primary beneficiary (institutional/arbitrage) — consolidates decision authority, distributes responsibility among multiple advisors, maintains secrecy while appearing consultative
 *   - Congress: Primary victim (powerless/trapped) — systematically excluded from real-time nuclear decision despite constitutional war powers; cannot exit due to crisis timeline compression
 *   - State Department/Dean Rusk: Secondary actor (institutional/constrained) — integrated into deliberation but subordinated to presidential prerogative; benefits from access, constrained by civilian control doctrine
 *   - Joint Chiefs of Staff/Maxwell Taylor: Secondary actor (organized/constrained) — significant military expertise represented but overruled on military options; benefit from deliberation access, extraction toward political authority
 *   - CIA/John McCone: Secondary actor (institutional/constrained) — essential intelligence role (U-2 photography provides crisis definition) but intelligence interpretation subject to political framing
 *   - Congress (War Powers Authority): Victim (powerless/trapped) — constitutional authority to declare war is systematically suppressed through secrecy and speed
 *   - Deliberative Transparency (Democratic Process): Victim (powerless/trapped) — abstract collective good; excluded from real-time crisis management
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cuban_missile_crisis_excomm_delibration, 0.35).
domain_priors:suppression_score(cuban_missile_crisis_excomm_delibration, 0.42).
domain_priors:theater_ratio(cuban_missile_crisis_excomm_delibration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_delibration, extractiveness, 0.35).
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_delibration, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_delibration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cuban_missile_crisis_excomm_delibration, tangled_rope).
narrative_ontology:human_readable(cuban_missile_crisis_excomm_delibration, "The ExComm Multi-Channel Deliberation Protocol").
narrative_ontology:topic_domain(cuban_missile_crisis_excomm_delibration, "political/military").

domain_priors:requires_active_enforcement(cuban_missile_crisis_excomm_delibration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cuban_missile_crisis_excomm_delibration, executive_presidency).
narrative_ontology:constraint_beneficiary(cuban_missile_crisis_excomm_delibration, joint_chiefs_of_staff).
narrative_ontology:constraint_victim(cuban_missile_crisis_excomm_delibration, congress_war_powers).
narrative_ontology:constraint_victim(cuban_missile_crisis_excomm_delibration, cabinet_institutional_autonomy).
narrative_ontology:constraint_victim(cuban_missile_crisis_excomm_delibration, deliberative_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONGRESS (SNARE) — Excluded from real-time deliberation on nuclear war decision. Congressional war powers authority is systematically suppressed through secrecy and speed. Representatives cannot exit: the crisis timeline compresses their ability to assert constitutional authority. Bears full cost of restricted deliberative input while President captures unilateral decision-making.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CABINET DEPARTMENTS (TANGLED ROPE) — State Department, Defense Secretary, CIA directors are integrated into ExComm but with constrained exit. Benefit from access to crisis deliberation and influence on decision, but authority is subordinated to Presidential prerogative. Cannot openly dissent without career risk. Mixed coordination (needs cabinet expertise) and extraction (presidential dominance of outcome).
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE PRESIDENCY (ROPE) — Primary beneficiary. ExComm solves the coordination problem of rapid, confidential deliberation in nuclear crisis. President experiences the protocol as enabling function: consolidating advice, legitimizing decision through multiple-perspective input, creating plausible deniability through distributed authorship of options. Extracts unilateral authority while distributing responsibility.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JOINT CHIEFS (TANGLED ROPE) — Organized military hierarchy with significant institutional power, but constrained by civilian control doctrine and Presidential authority during crisis. ExComm provides coordination function (multiple service perspectives, integrated planning) but also subordinates military judgment to political decision-making. Benefit from access and influence, but cannot override civilian policy choice. Extraction runs toward political authority.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTELLIGENCE AGENCIES (SCAFFOLD) — CIA and NSA provide essential reconnaissance (U-2 photography, signals intelligence) that structures the crisis decision space. Intelligence gathering role is genuinely needed; ExComm coordinates fact-finding. However, the constraint has sunset logic: as crisis resolves, intelligence role normalizes to standing operations rather than crisis deliberation. Theater increases during crisis (dramatic briefings, classified handling) but decreases postcrisis. Intelligence organizations see this as temporary emergency protocol.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL INSTITUTIONAL VIEW (PITON) — The ExComm protocol persists as inertial structure even after the Cuban Missile Crisis. The deliberative form — small group of executives, classified proceedings, distributed authority — outlasts the immediate justification (nuclear crisis management). Subsequent administrations use ExComm-like structures for non-crisis decisions. Theater ratio is high: the protocol maintains performative trappings of cabinet consultation while centralizing presidential authority. The coordination function that made it necessary in 1962 (rapid nuclear decision-making) is largely theatrical in routine use.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a civilizational perspective, nuclear weapons create an irreducible constraint: decision timelines measured in minutes (ICBM flight time ~30 minutes) are incompatible with full congressional deliberation (~hours minimum). This could be framed as a natural limit on democratic process under nuclear conditions. However, this perspective risks naturalizing what is actually a contingent technological and institutional choice: submarine-based deterrence, negotiation protocols, and arms control treaties are alternatives that could decompress timelines. The false summit detector will reveal whether this is true natural law or naturalized contingency.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cuban_missile_crisis_excomm_delibration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cuban_missile_crisis_excomm_delibration, TR),
    TR >= 0.70.

:- end_tests(cuban_missile_crisis_excomm_delibration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The ExComm protocol does extract congressional war-powers authority and limits cabinet autonomy. However, the extraction is not total: legitimate coordination value exists (multiple expert perspectives improve crisis decision-making), and the constraint is partially justified by genuine nuclear decision-speed requirements. The moderate value reflects that extraction and coordination coexist. Over the interval, extractiveness increases (0.22→0.40) as the constraint persists beyond the immediate crisis, suggesting that the original crisis-necessity justification weakens while the institutional inertia strengthens. Suppression (0.42): Moderate. Congressional authority is suppressed through classified operations and compressed timelines, creating significant barriers to institutional participation. However, suppression is not total — Congress can (and historically does) reassert authority postcrisis through legislation. Cabinet actors face suppression through subordination to presidential prerogative but have organizational capacity to push back. Theater ratio (0.58→0.72): Increases over the interval. During acute crisis (days 0-7), deliberation is mostly functional — genuine uncertainty about Soviet intentions, multiple viable options under serious consideration. Postcrisis (days 7-14), the protocol becomes more theatrical as the crisis resolves but the structure persists: meetings continue with less real deliberative content, decision options narrow to implementation rather than strategy, and the multi-channel format serves mainly to legitimate a largely predetermined course of action.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal here. Congress (powerless/trapped) sees pure extraction of constitutional authority — they are excluded from a decision about war and bear the costs of potential escalation without deliberative input. The presidency (institutional/arbitrage) sees coordination — ExComm solves the problem of integrating multiple expert perspectives rapidly. Cabinet actors (institutional/constrained) experience ambivalence: they benefit from access and influence on options, but their autonomy is subordinated to presidential prerogative. Military (organized/constrained) sees constrained voice: expert military judgment is heard but overruled on strategic choices. Intelligence (institutional/constrained) sees temporary emergency role, with restoration of standing operations postcrisis. The analytical observer at civilizational scale risks seeing nuclear decision speed as immutable constraint (mountain), but the structural data reveals this as naturalization: alternative command architectures, arms control treaties, and negotiation protocols could decompress timelines, making ExComm an institutional choice rather than a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural position within the constraint. Congress is a trapped victim of the compressed timeline and excluded deliberation — derives high d (~0.90) → high f(d) → experiences maximum extraction. Executive presidency is a beneficiary with arbitrage options (can choose different deliberation formats, can invoke emergency authority, can make decisions unilaterally) — derives low d (~0.10) → negative f(d) → experiences coordination benefit. Cabinet actors are mixed: beneficiaries of access (low d on participation dimension), victims of subordination (high d on authority dimension) — split perspective (tangled rope) reflects this duality. Joint Chiefs are organized (higher structural power to resist), constrained (cannot override civilian authority), so derive moderate-high d (~0.55) → moderate f(d). Intelligence agencies are institutional beneficiaries (their expertise is needed, their information structures the decision space) but with temporary role — low d for crisis period (~0.20), reverting to baseline (~0.50) postcrisis as emergency protocols normalize.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that ExComm is genuinely a tangled_rope structure: it performs a real coordination function (integrating multiple expert perspectives, processing complex intelligence, generating options) while simultaneously extracting congressional authority and constraining cabinet autonomy. The constraint cannot be reduced to pure coordination (rope) because the extraction of war powers is structural, not incidental. It cannot be reduced to pure extraction (snare) because the coordination function is genuine — the multi-channel deliberation produces better decisions than unilateral presidential choice would. The challenge is that observing from a powerless agent's perspective (Congress) makes it appear pure snare; observing from the presidency makes it appear pure rope. The tangled_rope classification unifies these: yes, there is genuine coordination; yes, there is genuine extraction. The constraint's mandate requires both functions. The theater ratio increasing postcrisis (0.58→0.72) is diagnostic: as the original coordination necessity weakens (crisis resolves), the performative aspect (legitimation of presidential authority) becomes more visible, but does not change the fundamental classification — it confirms that extraction persists even after coordination justification decays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decision_speed_vs_deliberation_tradeoff,
    'Is the compression of deliberative timeline an inherent physical constraint of nuclear deterrence, or a contingent technological-institutional choice?',
    'Comparative analysis of alternative nuclear command architectures (submarine-based strategic patrols, launch-on-warning vs launch-on-command, negotiated communication protocols); modeling of decision timelines under different institutional structures',
    'If inherent constraint: ExComm is structurally necessary (mountain or rope). If contingent: ExComm is an institutional choice that extracts congressional authority (snare or tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decision_speed_vs_deliberation_tradeoff, empirical, 'Whether nuclear decision speed is physical constraint or institutional choice').

omega_variable(
    congressional_participation_feasibility,
    'Could Congress participate effectively in real-time nuclear crisis decisions without compromising secrecy or extending decision timelines to unsafe durations?',
    'Historical case studies of Congressional notification in subsequent crises (Cuban Missile Crisis II scenarios, Korean ship seizures, Iranian hostage crisis); technical analysis of secure communication and rapid convening capacity; polling of defense specialists on feasibility',
    'If feasible: ExComm suppression of congressional authority is extractive choice, not necessity (snare/tangled rope confirmed). If infeasible: ExComm constraints on congress are structural necessity (rope or mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_participation_feasibility, empirical, 'Whether real-time Congressional participation is technically/politically feasible').

omega_variable(
    executive_prerogative_vs_constitutional_authority,
    'Does presidential emergency power during nuclear crisis represent legitimate executive necessity or unconstitutional extraction of legislative war powers?',
    'Constitutional scholar consensus on emergency power doctrine; longitudinal tracking of Congressional acquiescence vs assertion in crisis scenarios; comparison with international democratic practices (UK, France, Germany) in similar conditions',
    'If legitimate necessity: ExComm is rope (needed coordination). If unconstitutional: ExComm is snare (extraction of war powers). If mixed: tangled rope confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(executive_prerogative_vs_constitutional_authority, conceptual, 'Whether executive prerogative in crisis is legitimate or extractive').

omega_variable(
    deliberative_theater_function,
    'Does the multi-channel deliberation format (including military, intelligence, diplomatic perspectives) produce better decisions, or is it primarily theater legitimizing a predetermined presidential choice?',
    'Decision outcome analysis: Cuban Missile Crisis choices vs alternatives considered; counterfactual analysis of what would have happened without ExComm structure; comparison of crisis outcome quality to unilateral executive decisions in other scenarios',
    'If genuinely improves decisions: rope or tangled rope. If largely theater: piton confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_theater_function, empirical, 'Whether multi-channel deliberation improves crisis decision quality or provides theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cuban_missile_crisis_excomm_delibration, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(excomm_tr_t0, cuban_missile_crisis_excomm_delibration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(excomm_tr_t7, cuban_missile_crisis_excomm_delibration, theater_ratio, 7, 0.58).
narrative_ontology:measurement(excomm_tr_t14, cuban_missile_crisis_excomm_delibration, theater_ratio, 14, 0.72).

% Extraction over time
narrative_ontology:measurement(excomm_be_t0, cuban_missile_crisis_excomm_delibration, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(excomm_be_t7, cuban_missile_crisis_excomm_delibration, base_extractiveness, 7, 0.35).
narrative_ontology:measurement(excomm_be_t14, cuban_missile_crisis_excomm_delibration, base_extractiveness, 14, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cuban_missile_crisis_excomm_delibration, enforcement_mechanism).
narrative_ontology:affects_constraint(cuban_missile_crisis_excomm_delibration, nuclear_command_authority_delegation).
narrative_ontology:affects_constraint(cuban_missile_crisis_excomm_delibration, presidential_war_powers_usurpation).

% DUAL FORMULATION NOTE:
% The ExComm protocol is structurally distinct from the underlying nuclear decision-speed constraint and from subsequent institutionalization of executive prerogative. It represents an institutional solution to a coordination problem (how to rapidly process expert advice in crisis) that becomes an extraction mechanism (how to concentrate presidential authority). Decomposition: nuclear_command_authority_delegation (ε=0.12, rope) is the purely technical problem of getting commands to nuclear forces reliably; cuban_missile_crisis_excomm_deliberation (ε=0.35, tangled_rope) adds the political-military deliberation layer; presidential_war_powers_usurpation (ε=0.55, snare) tracks the longer-term extraction of congressional authority across multiple crises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cuban_missile_crisis_excomm_delibration, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
