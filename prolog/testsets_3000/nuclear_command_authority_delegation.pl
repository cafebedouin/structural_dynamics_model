% ============================================================================
% CONSTRAINT STORY: nuclear_command_authority_delegation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_command_authority_delegation, []).

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
 *   constraint_id: nuclear_command_authority_delegation
 *   human_readable: Nuclear Command Authority Delegation and Decision Suppression
 *   domain: geopolitical/military/strategic_doctrine
 *
 * SUMMARY:
 *   Nuclear command authority delegation represents the institutional
 *   transfer of existential decision-making power from centralized political
 *   leadership to dispersed military commanders. The constraint arises from a
 *   genuine strategic problem: if central command facilities are destroyed in
 *   a nuclear strike, the ability to retaliate requires pre-delegated
 *   authority to launch weapons. The system extracts authority downward while
 *   suppressing alternative strategic approaches and civilian participation.
 *   The theater ratio (0.58) reflects extensive command exercises, strategic
 *   reviews, and procedural oversight that create appearance of control over
 *   delegated authority while the core mechanism—decentralized launch
 *   capability—remains structurally unchanged. This constraint demonstrates
 *   how snare classification encompasses both genuine strategic rationale and
 *   extractive institutional effects: the field commander is trapped within a
 *   hierarchy that uses strategy as justification for suppression.
 *
 * KEY AGENTS:
 *   - Central Military Command and Political Executive: Primary beneficiary (institutional/arbitrage) — maintains strategic authority and political accountability; experiences constraint as coordination solution to communication loss scenario
 *   - Field Commanders: Primary victim (powerless/trapped) — receive delegated launch authority but cannot refuse orders or question strategic rationale; career destruction follows refusal; subject to suppression of alternative decision frameworks
 *   - Civilian Populations: Secondary victim (powerless/trapped) — zero agency in delegation chain; bear existential risk from decisions made without their participation; suppression is absolute through information asymmetry and institutional secrecy
 *   - Arms Control Community: Organized secondary actor (organized/constrained) — advocate for constraints on delegation but benefit from deterrence stability that delegation provides; constrained by geopolitical backlash against abolition proposals
 *   - Strategic Doctrine Establishment: Institutional persistence mechanism (institutional/arbitrage) — Cold War doctrine persists through procedural ritual despite changed geopolitical context; sees own function (strategic continuity) as increasingly theoretical
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy choice as strategic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_command_authority_delegation, 0.68).
domain_priors:suppression_score(nuclear_command_authority_delegation, 0.75).
domain_priors:theater_ratio(nuclear_command_authority_delegation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_command_authority_delegation, extractiveness, 0.68).
narrative_ontology:constraint_metric(nuclear_command_authority_delegation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nuclear_command_authority_delegation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_command_authority_delegation, snare).
narrative_ontology:human_readable(nuclear_command_authority_delegation, "Nuclear Command Authority Delegation and Decision Suppression").
narrative_ontology:topic_domain(nuclear_command_authority_delegation, "geopolitical/military/strategic_doctrine").

domain_priors:requires_active_enforcement(nuclear_command_authority_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_command_authority_delegation, central_military_command).
narrative_ontology:constraint_beneficiary(nuclear_command_authority_delegation, political_executive).
narrative_ontology:constraint_victim(nuclear_command_authority_delegation, field_commanders).
narrative_ontology:constraint_victim(nuclear_command_authority_delegation, civilian_populations).
narrative_ontology:constraint_victim(nuclear_command_authority_delegation, strategic_rationality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD COMMANDER (SNARE) — Trapped within the delegation hierarchy. Receives delegated launch authority but cannot refuse orders or question strategic rationale without career destruction and potential court-martial. Theater of 'decision authority' masks absence of real choice. Maximum experienced extraction through enforced obedience under existential pressure.
constraint_indexing:constraint_classification(nuclear_command_authority_delegation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS (SNARE) — Zero agency in delegation chain. Bear existential risk from automated or delegated launch decisions made without their participation. No exit option; suppression is absolute. Maximum extraction of risk onto those with zero decision authority.
constraint_indexing:constraint_classification(nuclear_command_authority_delegation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CENTRAL COMMAND & POLITICAL EXECUTIVE (ROPE) — Primary beneficiaries. Delegate decision authority downward to maintain command coherence during communication loss while retaining political accountability at top level. Experiences constraint as pure coordination: delegating launch authority solves the genuinely difficult problem of maintaining strategic continuity if command centers are destroyed. Net beneficiary.
constraint_indexing:constraint_classification(nuclear_command_authority_delegation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ARMS CONTROL ADVOCATES (TANGLED ROPE) — Organized agents see genuine coordination function (preventing paralysis under attack) but demand enforcement constraints to limit extraction (authorization protocols, fail-safes, thresholds). High suppression of alternative proposals; but also benefit from the very system they critique through deterrence stability. Constrained exit — advocating abolition triggers backlash; advocating reform gains partial traction.
constraint_indexing:constraint_classification(nuclear_command_authority_delegation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR DOCTRINE PERSISTENCE (PITON) — Delegated launch authority persists through institutional inertia despite geopolitical changes. Theater: extensive procedural oversight, war games, and strategic reviews create appearance of control while core mechanism (decentralized launch authority) remains unchanged. Function (strategic continuity) has atrophied; ritual (command exercises, nuclear posture reviews) remains. High theater ratio (0.58) reflects performative nature of modern oversight.
constraint_indexing:constraint_classification(nuclear_command_authority_delegation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRATEGIC NECESSITY (MOUNTAIN) — From a civilizational view, delegation appears structurally necessary: if central command is destroyed, only pre-delegated authority can maintain deterrence and prevent adversary escalation. The constraint appears as an immutable requirement of nuclear strategy itself — no state with second-strike capability can afford paralysis. However, the structural data contradicts this naturalization: delegation is a policy choice with alternatives (single-authority launch, automated systems, reduced reliance on hair-trigger readiness).
constraint_indexing:constraint_classification(nuclear_command_authority_delegation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_command_authority_delegation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_command_authority_delegation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_command_authority_delegation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_command_authority_delegation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_command_authority_delegation, TR),
    TR >= 0.70.

:- end_tests(nuclear_command_authority_delegation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The constraint extracts authority from field commanders and decision-making agency from civilian populations. However, it is not pure extraction — the coordination function (maintaining retaliation capability if central command is destroyed) is structurally real. The value reflects that extraction is embedded within a genuine strategic problem, not merely extractive overlay. The measurement trajectory shows increasing extractiveness over 60 years: Cold War scenario that justified delegation has become less plausible, yet institutional extraction has accumulated through procedural accretion. Suppression (0.75): High. Field commanders face career destruction for questioning orders; civilian populations are suppressed through secrecy and information asymmetry; alternative strategic doctrines are suppressed through institutional dominance of nuclear-armed state establishments. The suppression is not absolute (some debate occurs, some information exists) but highly constraining. Theater ratio (0.58): Moderate-high and increasing over time. Strategic reviews, war games, nuclear posture assessments, and command exercises create substantial performative content around delegated authority. However, theater is lower than pure piton constraints (0.70+) because the underlying mechanism retains functional significance even if justification has weakened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a perspectival gap between institutional beneficiaries and powerless victims. Central command sees rope (pure coordination problem solved elegantly). Field commanders see snare (trapped authority with no real choice). Arms control advocates see tangled rope (genuine coordination function but unacceptable extraction). The analytical observer at civilizational scale risks seeing mountain (immutable requirement of nuclear strategy) but structural data contradicts this: alternatives exist (automated retaliation, centralized authority with redundancy, conventional deterrence enhancement) but are suppressed through institutional preference for current system. The gap reveals that much of the 'necessity' framing is institutional rationalization rather than technical inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Central command and political executive are primary beneficiaries (institutional/arbitrage): delegation allows them to maintain control over strategic doctrine while pushing implementation authority downward. They face arbitrage options (automation, centralization, treaty-based elimination) but benefit from current delegation system through preserved political accountability. Derived d ≈ 0.10–0.15 (low). Field commanders are trapped victims (powerless/trapped): they receive decision authority that they cannot meaningfully exercise without accepting orders from above. Career consequences for refusal mean exit options are not genuinely mobile. Derived d ≈ 0.95 (maximum). Civilian populations are trapped victims (powerless/trapped): absolute suppression and zero exit capacity. Derived d ≈ 0.95 (maximum). Arms control advocates are organized but constrained (organized/constrained): they can propose alternatives but face institutional resistance and geopolitical backlash. Some exit exists (change professions, change advocacy focus) but at high cost. Derived d ≈ 0.55–0.65 (moderate-high).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint is structurally a snare (high extraction, high suppression, trapped victims) that uses strategic necessity as its legitimation narrative. The genuine coordination problem (maintaining retaliation capability) does not require this specific institutional form. Alternatives exist: centralized authority backed by redundant command centers; automated systems with human authorization checkpoints; reduced reliance on hair-trigger readiness through declaratory policy. The current system persists because it concentrates authority and extracts decision-making power from field commanders and civilian populations. The strategic rationale is partially valid but does not justify the extraction levels observed. Classification as snare is appropriate because alternatives are systematically suppressed and victims have no exit capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_vs_extraction,
    'Is delegated launch authority genuinely necessary for strategic stability or is it an extractive policy justified by exaggerated scenarios of communication loss?',
    'Historical analysis of near-miss incidents where delegation authority was exercised; modeling of communication survivability under nuclear attack; comparison of states with and without delegation protocols',
    'If genuinely necessary: snare classification may be too harsh — system is tangled rope with high suppression but real coordination function. If extraction-driven: snare classification confirmed — necessity is rationalization for maintaining command authority concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_vs_extraction, empirical, 'Whether delegation is strategic necessity or extractive rationalization').

omega_variable(
    automation_vs_delegation_tradeoff,
    'Would automated launch systems (pre-programmed retaliation based on sensor data) reduce or increase extraction and suppression compared to human-delegated authority?',
    'Technical analysis of false alarm rates, sensor reliability, and irreversibility; game-theoretic modeling of adversary incentives under automation vs human delegation; historical comparison with automated defense systems',
    'If automation reduces extraction: delegation is not immutable — alternatives exist but are suppressed for political/institutional reasons (higher piton component than currently modeled). If automation increases extraction: delegation represents least-bad option, validating current system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(automation_vs_delegation_tradeoff, empirical, 'Automation as alternative to delegation').

omega_variable(
    field_commander_agency_reality,
    'Do field commanders actually exercise delegated authority as autonomous agents or do they receive orders that the delegation framework merely authorizes retroactively?',
    'Declassified command protocols; testimony from retired commanders; analysis of communication chains and authentication procedures; comparison between peacetime training and crisis behavior',
    'If genuinely autonomous: field commanders experience tangled rope (some agency, some constraint). If purely authorized execution of pre-determined orders: experience is snare (theater of authority without actual choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(field_commander_agency_reality, empirical, 'Whether delegated authority represents genuine decision-making or authorized execution').

omega_variable(
    civilian_risk_awareness_suppression,
    'To what extent is civilian population awareness of delegation risk (and thus their structural victimhood) actively suppressed vs naturally limited by information asymmetry?',
    'Content analysis of government communications about nuclear command authority; comparison of public knowledge with classified doctrine; tracking of declassification patterns and suppression of accident/incident information',
    'If actively suppressed: suppression metric (0.75) is conservative — actual suppression higher. If information asymmetry: suppression is structural but not enforced; civilians are victims through ignorance rather than coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_risk_awareness_suppression, empirical, 'Active suppression vs information asymmetry in civilian awareness').

omega_variable(
    delegation_sunset_feasibility,
    'Could delegated launch authority be eliminated through treaty, doctrine change, or technological alternatives without undermining strategic deterrence?',
    'Game-theoretic analysis of deterrence stability with and without delegation; technical feasibility studies of alternatives (centralized authority with redundancy, automated response, conventional deterrence); negotiation history with peer states',
    'If feasible: constraint could be reframed as temporary (scaffold with sunset clause) rather than immutable. If infeasible: snare classification confirmed and mandatrophy resolved (extraction is strategically necessary, even if suppressive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_sunset_feasibility, conceptual, 'Feasibility of eliminating delegation without compromising deterrence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_command_authority_delegation, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ncad_tr_t0, nuclear_command_authority_delegation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ncad_tr_t20, nuclear_command_authority_delegation, theater_ratio, 20, 0.48).
narrative_ontology:measurement(ncad_tr_t40, nuclear_command_authority_delegation, theater_ratio, 40, 0.58).
narrative_ontology:measurement(ncad_tr_t60, nuclear_command_authority_delegation, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(ncad_be_t0, nuclear_command_authority_delegation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ncad_be_t20, nuclear_command_authority_delegation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ncad_be_t40, nuclear_command_authority_delegation, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(ncad_be_t60, nuclear_command_authority_delegation, base_extractiveness, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_command_authority_delegation, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_command_authority_delegation, nuclear_escalation_automation).
narrative_ontology:affects_constraint(nuclear_command_authority_delegation, command_chain_reliability_under_attack).
narrative_ontology:affects_constraint(nuclear_command_authority_delegation, civilian_consent_in_deterrence).

% DUAL FORMULATION NOTE:
% Nuclear command authority delegation exists as part of a constraint family encompassing strategic communications robustness, deterrence theory, and institutional military hierarchy. The upstream constraint (command chain reliability under attack) justifies delegation; the downstream constraints (escalation automation risk, civilian consent deficit) are affected by delegation policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_command_authority_delegation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
