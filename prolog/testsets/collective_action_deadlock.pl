% ============================================================================
% CONSTRAINT STORY: collective_action_deadlock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_action_deadlock, []).

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
 *   constraint_id: collective_action_deadlock
 *   human_readable: The Infinite Deliberation Loop
 *   domain: political/social
 *
 * SUMMARY:
 *   The collective action deadlock emerges when a group adopts decision rules
 *   that grant veto power to any single stakeholder (or small bloc), creating
 *   a structural situation where the group cannot coordinate a response to an
 *   external threat without unanimous consent. This constraint represents a
 *   hybrid extraction mechanism: the rule structure simultaneously enables
 *   minority protection (a genuine coordination function) and prevents
 *   majority from acting (an extraction of collective action capacity). The
 *   deadlock is not inherent to groups or democracy — it is a contingent
 *   property of specific institutional rules. Over time, as the constraint
 *   persists, the deliberation process becomes increasingly theatrical:
 *   meetings occur, positions are restated without change, consensus-building
 *   rituals proliferate while actual decision-making capacity atrophies
 *   (Piton dynamics). The threat-responsive capacity of the group is
 *   extracted by actors who benefit from status quo preservation, even when
 *   those actors themselves recognize the threat. The constraint demonstrates
 *   how coordination rules can be weaponized into extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Immobilized Collective: Primary victim (powerless/trapped) — the group's decision-making capacity is entirely extracted; bears cost of inability to respond to threats
 *   - Veto-Wielding Minority: Primary beneficiary (organized/arbitrage) — captures protection from majority overrule; benefits from rule structure; can exit via accepting majority position
 *   - Ordinary Coalition Members: Secondary actors (moderate/constrained) — experience mixed coordination (minority protection) and extraction (immobility); benefits depend on whether they are in minority or majority on this specific issue
 *   - Threat-Responsive Groups: Secondary victim — actors outside the coalition who bear costs of the group's inability to act (environmental degradation, unaddressed crises, externalized harms)
 *   - Reform Movement: Organized changemakers (organized/mobile) — pushing for supermajority rules, emergency protocols, or sunset clauses; see the deadlock as temporary institutional failure
 *   - Consensus Ritual Maintainers: Institutional actors (institutional/constrained) — facilitators, process designers who maintain the deliberation theater; may recognize its performative nature but continue it through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the deadlock as inherent to democratic processes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_action_deadlock, 0.58).
domain_priors:suppression_score(collective_action_deadlock, 0.68).
domain_priors:theater_ratio(collective_action_deadlock, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_action_deadlock, extractiveness, 0.58).
narrative_ontology:constraint_metric(collective_action_deadlock, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(collective_action_deadlock, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_action_deadlock, tangled_rope).
narrative_ontology:human_readable(collective_action_deadlock, "The Infinite Deliberation Loop").
narrative_ontology:topic_domain(collective_action_deadlock, "political/social").

domain_priors:requires_active_enforcement(collective_action_deadlock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_action_deadlock, veto_wielding_minorities).
narrative_ontology:constraint_beneficiary(collective_action_deadlock, status_quo_preservers).
narrative_ontology:constraint_victim(collective_action_deadlock, collective_action_capacity).
narrative_ontology:constraint_victim(collective_action_deadlock, threat_responsive_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILIZED COLLECTIVE (SNARE) — The group as a whole experiences total extraction of decision-making capacity. Trapped within the deliberation framework with no exit. Unable to act despite consensus on threat urgency. Maximum experienced extraction; collective action potential is entirely suppressed by the veto mechanism.
constraint_indexing:constraint_classification(collective_action_deadlock, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: VETO-WIELDING MINORITY (ROPE) — Experiences the constraint as pure coordination mechanism: the veto rule gives them voice proportional to their size, enabling minority protection. Low extraction cost; they can exit via accepting the majority position (arbitrage option). Net beneficiary of the rule structure.
constraint_indexing:constraint_classification(collective_action_deadlock, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: ORDINARY COALITION MEMBER (TANGLED ROPE) — Constrained by the need for consensus but also benefits from the protection mechanism if they become a minority position later. Mixed experience: benefits from minority veto protection (coordination), but pays cost of immobility in urgent situations (extraction). Constrained exit — leaving the coalition is costly but possible.
constraint_indexing:constraint_classification(collective_action_deadlock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM MOVEMENT (SCAFFOLD) — Organized actors proposing supermajority rules, sunset clauses on veto rights for existential threats, or emergency decision protocols. See the deadlock as a temporary institutional failure with a sunset: rule reforms or threat escalation will eventually force new decision structures. Mobile exit — they can shift to alternative decision frameworks or splinter groups.
constraint_indexing:constraint_classification(collective_action_deadlock, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSENSUS RITUAL (PITON) — The deliberation process itself has become largely performative. Meetings occur, positions are restated, no new information changes minds, no decision emerges. The ritual persists through institutional inertia — consensus-building is maintained as a legitimacy theater despite failing its core function (enabling collective action). Theater dominates function.
constraint_indexing:constraint_classification(collective_action_deadlock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT — Risks naturalizing the deadlock as an immutable feature of democratic groups: 'Consensus by definition takes time; urgent decisions require authoritarianism.' But the structural data contradicts this mountain claim — the deadlock is not inherent to democracy or collective decision-making; it is a contingent property of the specific rule structure (unrestricted veto). Other democracies with supermajority rules or emergency protocols do not experience the same deadlock. Engine will flag this as false summit: naturalization of institutional choice as natural law.
constraint_indexing:constraint_classification(collective_action_deadlock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_deadlock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_action_deadlock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_deadlock, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_action_deadlock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(collective_action_deadlock, TR),
    TR >= 0.70.

:- end_tests(collective_action_deadlock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts approximately 58% of the collective's decision-making capacity. This is not a high-extraction snare (>0.66) because minority members genuinely benefit from the veto rule when they are in minority, and some actors retain the arbitrage option of accepting majority positions. However, the extraction is substantial because the group as a whole loses its ability to coordinate, and external parties (threat-responsive groups) bear costs of inaction. The temporal measurement shows extractiveness rising from 0.38 to 0.58 as the deadlock persists — the constraint becomes worse over time as deliberation patterns entrench. Suppression (0.68): High. Significant barriers prevent escaping the deadlock without breaking the coalition: (1) unanimous consent requirement eliminates normal majority action; (2) defection costs are high (loss of coalition membership, reputational damage, loss of future minority protections); (3) no rule amendment mechanism that doesn't require veto. Actors are suppressed by the rule structure itself. Theater ratio (0.64): Moderate-high. As deadlock persists, the deliberation process becomes increasingly performative. Meetings and discussions continue but with zero probability of changing outcomes — positions remain fixed, same arguments repeat, ritual consensus-building replaces genuine deliberation. Theater rises from 0.35 to 0.64 over the interval, indicating metric substitution (Goodhart drift): the system measures 'deliberation quality' by frequency and inclusivity of meetings rather than by actual decision output.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The veto-wielding minority sees the rule as coordination (Rope) — protecting their interests and enabling their voice. The immobilized collective sees it as pure extraction (Snare) — loss of all decision-making capacity. Ordinary members experience it as hybrid (Tangled Rope) — protection and extraction simultaneously. The reform movement sees it as temporary (Scaffold) — a solvable institutional problem with a sunset. The consensus ritual maintainers see it as performative degradation (Piton) — a form that persists despite lost function. The analytical observer risks seeing it as inevitable (Mountain) — but this is a false summit. The perspectival gap is fully explained by the agent's structural position (beneficiary vs victim, trapped vs mobile, organized vs powerless). No additional contextual axis is needed — the four canonical axes fully capture the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by stakeholder position. Veto-wielding minorities (beneficiaries with arbitrage options) derive low or negative d-values — they experience the rule as beneficial coordination, not extraction. They can exit the veto requirement by accepting majority positions, giving them arbitrage optionality. Ordinary coalition members experience moderate d-values: they benefit from minority protection but pay extraction costs when they are in majority. Constrained exit options (leaving the coalition is costly) mean they cannot fully arbitrage. The immobilized collective (powerless/trapped) experiences maximum d-value approaching 1.0 — no exit, maximum extraction. Threat-responsive groups outside the coalition have no formal voice in the decision structure, experiencing it as pure external constraint. The derivation shows why the same rule structure produces rope-type benefits for minorities and snare-type harm for the majority: the directionality computation is agent-relative, and veto grants directionality advantage to small groups.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The Infinite Deliberation Loop resolves the mandatrophy by exposing how a coordination mechanism (minority protection) can be weaponized into an extraction mechanism (immobility). The rule satisfies both definitions: it genuinely enables coordination for minority protection (Rope function) AND it systematically extracts collective action capacity from the majority (Snare function). These are not two different interpretations of the same data — they are simultaneous structural realities for different stakeholders. The Tangled Rope classification captures this dual nature: the rule requires active enforcement (minorities must invoke the veto), it produces genuine coordination benefits (minority protection), it extracts from identifiable victims (the immobilized collective), and it generates suppression (defection costs). The mandatrophy is not resolved by choosing one type; it is resolved by recognizing that the constraint is genuinely hybrid, and the perspectival gap (Rope for beneficiaries, Snare for victims) is the diagnostic signature of Tangled Rope. The false summit (analytical observer's Mountain) serves as a cautionary example: the deadlock appears inevitable and immutable only if you naturalize the specific rule choice. Other decision structures (supermajority, emergency delegation, time-limited veto) avoid the deadlock entirely, proving it is contingent institutional design, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_urgency_threshold,
    'At what threat severity do rational actors abandon consensus requirements in favor of majority rule or delegation?',
    'Empirical analysis of historical coalitions facing escalating threat levels; correlation between threat magnitude and rule change incidence',
    'If threshold is low: deadlock dissolves quickly under pressure, constraint classification shifts toward Scaffold. If threshold is high or absent: deadlock persists despite severe threats, constraint shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_urgency_threshold, empirical, 'Threat severity threshold triggering rule abandonment').

omega_variable(
    veto_bloc_unification,
    'Is the veto-wielding minority a unified strategic bloc or a collection of actors with divergent interests using veto tactically?',
    'Game-theoretic analysis of veto-wielder incentive alignment; historical tracking of veto coalition stability across different threat scenarios',
    'If unified bloc: veto power is stable, extraction is systematic, constraint is durable Snare/Tangled Rope. If tactically divided: veto power is fragile, defection risk is high, constraint collapses under threat or side-payments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_bloc_unification, empirical, 'Whether veto minority is strategically unified or tactically divided').

omega_variable(
    exit_costliness_asymmetry,
    'Do different coalition members face asymmetric costs of exiting the consensus framework (coalition exit vs framework exit)?',
    'Structural analysis of coalition interdependencies; measurement of defection costs by stakeholder group; historical examples of coalition dissolution',
    'If exit is symmetric: all members experience equal constraint, classification is uniform. If exit is asymmetric: some actors are trapped while others are mobile, creates conditions for coalition fragmentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_costliness_asymmetry, empirical, 'Asymmetry in exit costs across coalition members').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_action_deadlock, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colact_tr_t0, collective_action_deadlock, theater_ratio, 0, 0.35).
narrative_ontology:measurement(colact_tr_t2, collective_action_deadlock, theater_ratio, 2, 0.48).
narrative_ontology:measurement(colact_tr_t5, collective_action_deadlock, theater_ratio, 5, 0.64).

% Extraction over time
narrative_ontology:measurement(colact_be_t0, collective_action_deadlock, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(colact_be_t2, collective_action_deadlock, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(colact_be_t5, collective_action_deadlock, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_action_deadlock, enforcement_mechanism).
narrative_ontology:affects_constraint(collective_action_deadlock, regulatory_capture).
narrative_ontology:affects_constraint(collective_action_deadlock, committee_tyranny_of_minority).
narrative_ontology:affects_constraint(collective_action_deadlock, consensus_requirement_coordination).

% DUAL FORMULATION NOTE:
% The Infinite Deliberation Loop is upstream of specific policy deadlocks but represents a distinct structural constraint on decision-making itself. It affects multiple downstream constraints that inherit its immobility signature: regulatory capture uses deadlock to preserve status quo, committee tyranny uses veto to extract minority benefits, consensus-based coordination mechanisms inherit the deadlock risk. The constraint family shares the property that decision rules can be weaponized into extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(collective_action_deadlock, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
