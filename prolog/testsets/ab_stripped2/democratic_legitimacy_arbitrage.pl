% ============================================================================
% CONSTRAINT STORY: democratic_legitimacy_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-15
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
 *   human_readable: Democratic Legitimacy Arbitrage in Populist Governance
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The democratic legitimacy arbitrage emerges when populist supporters
 *   simultaneously endorse representative democracy as a normative ideal and
 *   strong-leader governance without institutional constraints as a practical
 *   necessity. Survey data across multiple democracies shows this dual
 *   endorsement is significantly more common among populist supporters than
 *   non-populist citizens, creating a legitimacy reservoir that populist
 *   leaders can draw upon selectively. The arbitrage mechanism works by
 *   exploiting the conceptual gap between procedural democracy (elections,
 *   majority rule) and constitutional democracy (institutional checks,
 *   minority rights, rule of law). Populist leaders invoke democratic mandate
 *   to claim legitimacy while dismissing institutional constraints as
 *   anti-democratic elite obstruction. The constraint has intensified over
 *   the interval (2014-2024) as populist movements have learned to
 *   operationalize the arbitrage more effectively, with rising extractiveness
 *   (0.32 → 0.48) reflecting increasing institutional erosion, rising
 *   suppression (0.45 → 0.62) reflecting intensifying delegitimization of
 *   opposition and checks, and rising theater ratio (0.42 → 0.58) reflecting
 *   the growing gap between democratic rhetoric and authoritarian practice.
 *
 * KEY AGENTS:
 *   - Populist Supporter: Primary target (powerless/identity_locked) — identity constituted through the movement; cannot see the tension between democratic norms and unchecked executive power
 *   - Institutional Checks and Balances: Primary victim (powerless/trapped) — trapped by the legitimacy arbitrage; resistance to executive overreach is framed as anti-democratic
 *   - Opposition Political Actor: Secondary victim (moderate/constrained) — constrained by asymmetric costs but benefits from democratic procedural framework
 *   - Populist Leadership: Primary beneficiary (institutional/arbitrage) — extracts legitimacy from dual endorsement while bypassing institutional constraints
 *   - Civil Society Coalition: Organized agents (organized/mobile) — NGOs, media, academics, international monitors; mobile exit options but constrained by national context
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination function (representation failure correction) and extraction (institutional erosion)
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
narrative_ontology:human_readable(democratic_legitimacy_arbitrage, "Democratic Legitimacy Arbitrage in Populist Governance").
narrative_ontology:topic_domain(democratic_legitimacy_arbitrage, "political_economy/comparative_politics/democratic_theory").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(democratic_legitimacy_arbitrage, populist_leadership).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, institutional_checks_and_balances).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, opposition_political_actors).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, judicial_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULIST SUPPORTER (SNARE) — Identity-locked within the populist frame that treats democratic legitimacy and strong-leader authority as compatible. Cannot exit because their political identity is constituted through the movement. Experiences the constraint as natural — the leader embodies the people's will, so concentrated power IS democracy. The cognitive lock prevents recognition of the extraction mechanism.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL CHECKS AND BALANCES (SNARE) — Trapped by the legitimacy arbitrage. When populist supporters endorse both democratic norms and unchecked executive power, institutional resistance to executive overreach is framed as anti-democratic elite obstruction. The check cannot exit — it must either enforce constraints (and be delegitimized) or acquiesce (and cease to function). Maximum extraction with no viable exit path.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: OPPOSITION POLITICAL ACTOR (TANGLED ROPE) — Constrained by the legitimacy arbitrage but also benefits from the democratic procedural framework that the populist movement claims to honor. Can contest elections and invoke democratic norms, but faces asymmetric costs when the executive bypasses institutional constraints while claiming popular mandate. Mixed extraction — some coordination benefit from the democratic frame, significant extraction from the arbitrage mechanism.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POPULIST LEADERSHIP (ROPE) — Primary beneficiary with arbitrage exit options. Experiences the constraint as pure coordination: the simultaneous endorsement of democratic legitimacy and strong-leader governance creates a legitimacy reservoir that can be drawn upon selectively. Can invoke democratic mandate when convenient and dismiss institutional constraints as elite obstruction when inconvenient. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL SOCIETY COALITION (TANGLED ROPE) — Organized agents (NGOs, media watchdogs, academic institutions, international democracy monitors) see the legitimacy arbitrage as a coordination problem with extractive overlay. Benefits from the democratic procedural framework that enables organizing and advocacy, but bears costs from the erosion of institutional checks. Mobile exit options — can shift resources internationally or pivot to different advocacy strategies — but constrained by national political context.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the legitimacy arbitrage represents a genuine tension in democratic theory between popular sovereignty and constitutional constraints. Some coordination function exists — the populist movement is solving a real representation failure (captured in upstream constraint populist_as_class_realignment). But the arbitrage mechanism extracts from institutional integrity by exploiting the conceptual gap between procedural democracy and substantive checks. The analytical observer sees both the coordination function and the extraction, hence tangled_rope.
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
    constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.48): Moderate-high. The legitimacy arbitrage extracts from institutional integrity by delegitimizing checks and balances while claiming democratic mandate. The extraction is substantial but not total — some institutional constraints persist, and the democratic procedural framework continues to function (elections occur, opposition exists). The value reflects that the arbitrage is an ongoing erosion process rather than a completed authoritarian consolidation. Suppression (0.62): Moderate-high. Significant suppression of institutional alternatives through delegitimization (courts and parliaments framed as elite obstruction), media pressure, civil society constraints, and opposition harassment. But suppression is not total — institutional checks can still resist in some domains, and opposition can still contest elections. The rising trajectory (0.45 → 0.62) reflects intensifying enforcement as the arbitrage mechanism matures. Theater ratio (0.58): Moderate-high. Substantial gap between democratic rhetoric (invoking popular will, electoral mandate, majority rule) and authoritarian practice (bypassing institutional constraints, delegitimizing opposition, eroding judicial independence). The theater is functional — it maintains the legitimacy reservoir that enables the arbitrage — but the gap between claim and reality is widening.
 *
 * PERSPECTIVAL GAP:
 *   The populist supporter sees the constraint as mountain (identity_locked perspective) — the leader's authority is natural and legitimate, institutional resistance is elite obstruction. The institutional check sees snare (trapped perspective) — delegitimization with no exit path. The opposition actor sees tangled_rope (constrained perspective) — benefits from democratic procedures but bears asymmetric costs from the arbitrage. The populist leadership sees rope (arbitrage perspective) — pure coordination, the legitimacy reservoir enables effective governance. The civil society coalition sees tangled_rope (mobile perspective) — genuine representation problem being addressed alongside institutional extraction. The analytical observer sees tangled_rope (analytical perspective) — both coordination function (correcting representation failure) and extraction (eroding institutional checks). The perspectival gap between the identity_locked supporter (mountain) and the analytical observer (tangled_rope) at the same biographical time horizon reveals that the binding mechanism is cognitive rather than structural — the constraint is changeable, but the supporter cannot see this from within their identity frame.
 *
 * DIRECTIONALITY LOGIC:
 *   The populist supporter is identity_locked rather than trapped because the binding mechanism is cognitive rather than structural. The supporter has structural mobility (can vote for opposition, can access alternative information sources, faces no legal prohibition on exit) but cannot exercise it because their political identity is constituted through the populist movement. The identity frame makes the tension between democratic norms and strong-leader governance literally invisible — the leader embodies the people's will, so concentrated power IS democracy. This is the diagnostic signature of identity_locked: the agent perceives the constraint as mountain (unchangeable, natural) at biographical time horizon, while an agent with the same structural position but different identity frame (the opposition supporter) perceives it as snare (changeable, extractive). The gap reveals that the binding is perceptual rather than material. Institutional checks are trapped rather than identity_locked because their barriers to exit are structural: they must either enforce constraints (and be delegitimized) or acquiesce (and cease to function). The populist leadership is the primary beneficiary with arbitrage exit options — can invoke democratic legitimacy when convenient, dismiss institutional constraints when inconvenient, and exit to international safe havens if the regime collapses. Opposition actors are constrained rather than trapped because they face high but surmountable costs to exit (career damage, social penalty, potential harassment) but can still contest elections and invoke democratic norms.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the legitimacy arbitrage is neither pure coordination (rope) nor pure extraction (snare) but depends on the observer's structural position. The populist leadership genuinely experiences it as coordination — they are solving a representation failure (upstream constraint populist_as_class_realignment) by mobilizing previously excluded constituencies. The institutional checks genuinely experience it as extraction — their capacity to constrain executive power is being systematically eroded. The analytical observer sees both: a real coordination function (addressing representation failure) layered with a real extraction mechanism (institutional erosion). The mandatrophy is resolved by recognizing that all three perspectives are structurally valid readings of the same phenomenon, and the presheaf over the observation site captures the full structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_vs_strategic_endorsement,
    'Do populist supporters genuinely hold both democratic and strong-leader preferences as cognitively compatible, or is the dual endorsement a strategic response to survey framing?',
    'Experimental survey design with randomized question ordering, implicit association tests, longitudinal tracking of preference stability, qualitative interviews probing reasoning',
    'If cognitive: identity_locked classification confirmed — supporters cannot see the tension. If strategic: supporters are constrained rather than identity_locked — they see the tension but endorse both for instrumental reasons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_vs_strategic_endorsement, empirical, 'Whether dual endorsement reflects cognitive compatibility or strategic framing response').

omega_variable(
    legitimacy_arbitrage_reversibility,
    'Can the legitimacy arbitrage be reversed through institutional design (e.g., constitutional reforms that make the tension explicit), or does it represent a stable attractor in populist governance?',
    'Comparative analysis of institutional reforms in post-populist transitions; tracking of legitimacy arbitrage persistence across regime changes; experimental constitutionalism studies',
    'If reversible: scaffold logic applies — the arbitrage is a temporary coordination failure with institutional solutions. If stable attractor: snare classification confirmed — the arbitrage is self-reinforcing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_arbitrage_reversibility, empirical, 'Whether legitimacy arbitrage can be reversed through institutional design').

omega_variable(
    representation_failure_threshold,
    'At what threshold of representation failure does the legitimacy arbitrage shift from extractive mechanism to legitimate democratic correction?',
    'Historical analysis of populist movements that successfully corrected representation failures vs those that consolidated authoritarian power; identification of structural differences in initial conditions',
    'If threshold is identifiable and current cases exceed it: tangled_rope classification from more perspectives — genuine coordination function alongside extraction. If no threshold or current cases below it: snare classification from more perspectives — extraction dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(representation_failure_threshold, conceptual, 'Threshold at which legitimacy arbitrage becomes legitimate democratic correction').

omega_variable(
    international_diffusion_mechanism,
    'Does the legitimacy arbitrage spread through ideological diffusion (populist movements learning from each other) or through structural convergence (similar economic conditions producing similar political dynamics)?',
    'Network analysis of populist movement connections and information flows; comparative timing analysis of legitimacy arbitrage emergence; structural economic similarity analysis',
    'If ideological diffusion: the arbitrage is a transmitted strategy that could be countered through counter-messaging. If structural convergence: the arbitrage is an emergent property of post-industrial political economy (upstream constraint post_industrial_spatial_extraction) and requires structural economic intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_diffusion_mechanism, empirical, 'Whether legitimacy arbitrage spreads through ideology or structural convergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(democratic_legitimacy_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dem_arb_tr_t0, democratic_legitimacy_arbitrage, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dem_arb_tr_t3, democratic_legitimacy_arbitrage, theater_ratio, 3, 0.48).
narrative_ontology:measurement(dem_arb_tr_t6, democratic_legitimacy_arbitrage, theater_ratio, 6, 0.54).
narrative_ontology:measurement(dem_arb_tr_t10, democratic_legitimacy_arbitrage, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(dem_arb_be_t0, democratic_legitimacy_arbitrage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dem_arb_be_t3, democratic_legitimacy_arbitrage, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(dem_arb_be_t6, democratic_legitimacy_arbitrage, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(dem_arb_be_t10, democratic_legitimacy_arbitrage, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(dem_arb_su_t0, democratic_legitimacy_arbitrage, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dem_arb_su_t3, democratic_legitimacy_arbitrage, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(dem_arb_su_t6, democratic_legitimacy_arbitrage, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(dem_arb_su_t10, democratic_legitimacy_arbitrage, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(democratic_legitimacy_arbitrage, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of post_industrial_spatial_extraction (mountain — structural economic transformation creating geographic winners and losers) and populist_as_class_realignment (tangled_rope — populist movements as both representation correction and extractive mechanism). The legitimacy arbitrage is the political-institutional manifestation of the underlying economic and class dynamics captured in the upstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
