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
 *   human_readable: Democratic Legitimacy Arbitrage in Populist Governance
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The democratic legitimacy arbitrage constraint emerges when populist
 *   supporters simultaneously endorse representative democracy (elections,
 *   representation, accountability) and strong-leader governance without
 *   parliamentary or judicial interference. This creates a legitimacy
 *   arbitrage opportunity for populist leadership: invoke democratic mandate
 *   when convenient (electoral victories, referenda, popular support metrics)
 *   while dismissing institutional constraints as anti-democratic obstacles
 *   when inconvenient (judicial review blocking executive action, legislative
 *   oversight, bureaucratic independence, media criticism). The constraint is
 *   downstream of post-industrial spatial extraction (geographic
 *   concentration of economic opportunity creates the grievance base) and
 *   populist-as-class-realignment (the political mobilization structure that
 *   channels grievance into populist support). The arbitrage mechanism
 *   operates through identity fusion: 'the people' and 'the leader' are
 *   treated as identical within the populist frame, so what the leader does
 *   IS democratic by definition, and institutional checks that constrain the
 *   leader are anti-democratic by definition. Survey data shows this
 *   simultaneous endorsement pattern is empirically robust across multiple
 *   populist movements, not a measurement artifact or cognitive dissonance.
 *
 * KEY AGENTS:
 *   - Populist Leadership: Primary beneficiary (institutional/arbitrage) — exploits the legitimacy arbitrage to concentrate power while claiming democratic mandate
 *   - Institutional Checks and Balances: Primary victim (powerless/trapped) — constitutional constraints, separation of powers, judicial independence systematically delegitimized as elite obstacles
 *   - Opposition Political Actors: Secondary victim (moderate/constrained) — face asymmetric legitimacy costs when invoking procedural norms or institutional constraints
 *   - Populist Supporters: Identity-locked agents (moderate/identity_locked) — simultaneously endorse contradictory governance models without perceiving contradiction due to identity fusion of 'people' and 'leader'
 *   - Civil Society and Media: Organized victims (organized/constrained) — targeted as 'enemies of the people' when challenging leadership, but retain some agency through organization
 *   - Analytical Observer: Sees both coordination function (representation of real grievances) and extraction function (institutional degradation, power concentration)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(democratic_legitimacy_arbitrage, 0.58).
domain_priors:suppression_score(democratic_legitimacy_arbitrage, 0.68).
domain_priors:theater_ratio(democratic_legitimacy_arbitrage, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(democratic_legitimacy_arbitrage, snare).
narrative_ontology:human_readable(democratic_legitimacy_arbitrage, "Democratic Legitimacy Arbitrage in Populist Governance").
narrative_ontology:topic_domain(democratic_legitimacy_arbitrage, "political_economy/comparative_politics/democratic_theory").

domain_priors:requires_active_enforcement(democratic_legitimacy_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(democratic_legitimacy_arbitrage, populist_leadership).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, institutional_checks_and_balances).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, opposition_political_actors).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, judicial_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTIONAL CHECKS (SNARE) — Constitutional constraints and separation of powers cannot exit the legitimacy contest. Populist leadership claims democratic mandate while systematically weakening judicial review, legislative oversight, and bureaucratic independence. The institutions bear full extraction — delegitimized as 'elitist obstacles' while unable to defend their coordinating function without appearing anti-democratic.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION ACTORS (SNARE) — Face asymmetric legitimacy costs. When they invoke procedural norms or institutional constraints, they are framed as anti-democratic elites blocking 'the will of the people.' Exit options are constrained by the same democratic framework being arbitraged — leaving the system delegitimizes their position, staying in the system subjects them to rules selectively enforced. High extraction despite moderate power.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POPULIST SUPPORTERS (TANGLED ROPE) — Identity-locked within the populist frame. Simultaneously endorse representative democracy (coordination function — elections, representation, accountability) AND strong-leader governance without institutional interference (extraction function — concentration of power, weakening of checks). The contradiction is not perceived as such from within the identity frame because 'the people' and 'the leader' are fused — what the leader does IS democratic by definition. Genuine coordination need (political representation) coexists with extraction (institutional degradation). The supporter cannot exit the frame without dissolving their political identity.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 4: POPULIST LEADERSHIP (ROPE) — Primary beneficiary with arbitrage exit options. The simultaneous endorsement of democratic legitimacy and strong-leader governance creates a legitimacy arbitrage opportunity: claim democratic mandate when convenient (elections, referenda, popular support) while dismissing institutional constraints as anti-democratic obstacles when inconvenient (judicial review, legislative oversight, bureaucratic independence). Can exit to international allies, alternative power bases, or economic networks if domestic legitimacy erodes. Experiences the constraint as pure coordination — the contradiction in supporter beliefs is a resource, not a cost.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL SOCIETY (TANGLED ROPE) — Organized actors (independent media, NGOs, professional associations) face mixed extraction. They benefit from democratic norms (freedom of association, speech, assembly) while being targeted as 'enemies of the people' when they challenge the leadership. The arbitrage mechanism extracts from their legitimacy — they are framed as elitist or foreign-funded when they invoke institutional norms — but they retain some agency through organization and can build alternative narratives over generational time. Constrained exit: leaving the national context is costly but possible.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint has genuine coordination function (populist movements do represent real grievances and mobilize previously excluded groups) AND asymmetric extraction (institutional degradation, concentration of power, selective enforcement). The simultaneous endorsement pattern is not cognitive dissonance but a structural feature of how populist legitimacy operates: 'the people' as a category is defined through opposition to 'the elite,' and institutional checks are coded as elite obstacles. The arbitrage is the mechanism — democratic legitimacy is invoked to justify anti-democratic concentration of power.
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
 *   Extractiveness (0.58): Moderate-high. The arbitrage mechanism extracts from institutional checks, opposition actors, and civil society by delegitimizing their coordinating functions. The extraction is not total because some institutional resistance persists and the coordination function (political representation of excluded groups) is genuine. Suppression (0.68): High. Institutional constraints face systematic delegitimization, opposition actors face asymmetric enforcement, media and civil society face 'enemy of the people' framing. Suppression increases over the interval as the arbitrage mechanism matures and institutional resistance is worn down. Theater ratio (0.64): Moderate-high. Democratic rituals (elections, referenda, parliamentary procedures) are maintained but increasingly performative — outcomes are predetermined through media control, opposition suppression, and selective enforcement. The theater increases over time as the gap between democratic form and autocratic function widens.
 *
 * PERSPECTIVAL GAP:
 *   The populist leadership sees pure coordination (Rope) — the simultaneous endorsement pattern is a resource that enables effective governance by removing elite obstacles. Institutional checks see pure extraction (Snare) — they are systematically delegitimized while unable to defend their coordinating function. Opposition actors also see extraction (Snare) — they face asymmetric legitimacy costs with constrained exit. Populist supporters see mixed coordination and extraction (Tangled Rope) — they genuinely benefit from political representation while participating in institutional degradation, but cannot perceive the extraction from within their identity frame. Civil society sees mixed coordination and extraction (Tangled Rope) — they benefit from democratic norms while being targeted as enemies. The analytical observer sees the full structure (Tangled Rope) — genuine coordination function (representation of excluded groups) coexists with asymmetric extraction (institutional degradation, power concentration). The perspectival gap reveals that the arbitrage mechanism operates differently depending on structural position: beneficiaries experience coordination, victims experience extraction, identity-locked agents experience contradiction without perceiving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Populist leadership is the primary beneficiary with arbitrage exit options — they can shift to international allies, alternative economic networks, or authoritarian governance models if domestic legitimacy erodes. Derived d ≈ 0.05, producing negative effective extraction (the constraint subsidizes this agent). Institutional checks are powerless and trapped — they cannot exit the legitimacy contest and bear full extraction. Derived d ≈ 0.95, producing maximum effective extraction. Opposition actors are moderate power with constrained exit — they face high costs for leaving the system but are not absolutely trapped. Derived d ≈ 0.85, producing high extraction. Populist supporters are moderate power but identity-locked — their exit options are constrained by identity fusion with the populist frame. Derived d ≈ 0.89 (identity-locked modulation), producing high extraction despite their role as nominal beneficiaries of representation. The identity lock is the key mechanism: supporters cannot perceive the contradiction between democratic and strong-leader endorsements because their identity is constituted through the populist frame. Civil society actors are organized with constrained exit, and are victims of the delegitimization mechanism. Derived d ≈ 0.55, producing moderate extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the simultaneous endorsement pattern is not cognitive dissonance but a structural feature of how populist legitimacy operates. The coordination function is real — populist movements do represent previously excluded groups and mobilize genuine grievances. The extraction function is also real — institutional checks are degraded, power is concentrated, enforcement is selective. The arbitrage mechanism is the link: democratic legitimacy is invoked to justify anti-democratic concentration of power. The identity fusion of 'the people' and 'the leader' makes this coherent from within the populist frame — what the leader does IS democratic because the leader IS the people. The analytical classification (Tangled Rope) captures both functions without collapsing into pure coordination (Rope) or pure extraction (Snare). The victim perspectives (institutional checks, opposition actors) correctly identify the extraction they experience. The beneficiary perspective (populist leadership) correctly identifies the coordination they exploit. The identity-locked perspective (populist supporters) correctly identifies their inability to perceive the contradiction from within their frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endorsement_stability,
    'Is the simultaneous endorsement of representative democracy and strong-leader governance stable over time, or does it resolve toward one pole as populist governance matures?',
    'Longitudinal survey data tracking the same populist supporter cohorts over 5-10 year periods; comparison of early-stage vs mature populist regimes',
    'If stable: the arbitrage is a persistent structural feature of populist legitimacy. If resolves toward strong-leader pole: the democratic endorsement was transitional cover. If resolves toward representative pole: the strong-leader endorsement was protest vote, not preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endorsement_stability, empirical, 'Temporal stability of simultaneous democratic and strong-leader endorsement').

omega_variable(
    cross_national_variance,
    'Does the arbitrage mechanism operate identically across different institutional contexts (presidential vs parliamentary systems, common law vs civil law, federal vs unitary states)?',
    'Comparative analysis of populist movements in different constitutional frameworks; identification of institutional features that resist or enable the arbitrage',
    'If context-invariant: the arbitrage is a universal feature of populist legitimacy. If context-dependent: specific institutional designs can block the mechanism, suggesting the snare classification applies only in vulnerable institutional contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_national_variance, empirical, 'Cross-national institutional variance in arbitrage mechanism').

omega_variable(
    elite_vs_mass_framing,
    'Is the ''elite vs people'' framing that enables the arbitrage a top-down leadership construction or a bottom-up mass belief that leadership exploits?',
    'Analysis of framing emergence: timeline of elite-vs-people rhetoric in leadership communication vs mass survey data; experimental manipulation of framing in survey instruments',
    'If top-down: the arbitrage is a leadership extraction mechanism (snare from more perspectives). If bottom-up: the arbitrage reflects genuine mass belief structure (tangled rope from more perspectives, with coordination function more prominent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_vs_mass_framing, empirical, 'Direction of elite-vs-people framing construction').

omega_variable(
    institutional_recovery,
    'Can institutional checks recover their legitimacy after populist leadership exits, or does the arbitrage mechanism create permanent degradation?',
    'Post-populist regime analysis: institutional trust trajectories, judicial independence metrics, legislative effectiveness after populist leadership turnover',
    'If recoverable: the constraint has scaffold properties (temporary extraction with sunset). If permanent: the snare classification is confirmed — institutional degradation persists beyond the leadership that exploited it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_recovery, empirical, 'Institutional recovery capacity post-populist governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(democratic_legitimacy_arbitrage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dem_arb_tr_t0, democratic_legitimacy_arbitrage, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dem_arb_tr_t3, democratic_legitimacy_arbitrage, theater_ratio, 3, 0.56).
narrative_ontology:measurement(dem_arb_tr_t6, democratic_legitimacy_arbitrage, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(dem_arb_be_t0, democratic_legitimacy_arbitrage, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dem_arb_be_t3, democratic_legitimacy_arbitrage, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(dem_arb_be_t6, democratic_legitimacy_arbitrage, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dem_arb_su_t0, democratic_legitimacy_arbitrage, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(dem_arb_su_t3, democratic_legitimacy_arbitrage, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(dem_arb_su_t6, democratic_legitimacy_arbitrage, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(democratic_legitimacy_arbitrage, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of post_industrial_spatial_extraction (geographic concentration of economic opportunity creates the grievance base that populist movements mobilize) and populist_as_class_realignment (the political mobilization structure that channels spatial grievance into populist support). The legitimacy arbitrage is the governance mechanism that emerges once populist movements gain power — it is structurally distinct from the upstream constraints but causally dependent on them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
