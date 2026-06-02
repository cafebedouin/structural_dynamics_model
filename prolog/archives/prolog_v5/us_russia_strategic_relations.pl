% ============================================================================
% CONSTRAINT STORY: us_russia_strategic_relations
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_russia_strategic_relations, []).

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
 *   constraint_id: us_russia_strategic_relations
 *   human_readable: US-Russia Strategic Relations Constraint
 *   domain: geopolitical/international_relations
 *
 * SUMMARY:
 *   US-Russia strategic relations constitute a structured extraction
 *   mechanism disguised as coordination for collective security. The
 *   constraint emerged clearly after the 1991 Soviet collapse and has
 *   intensified significantly despite the absence of ideological bipolarity
 *   that originally justified it. Two nuclear superpowers maintain military
 *   competition, nuclear modernization programs, intelligence operations, and
 *   geopolitical positioning that consumes enormous institutional resources,
 *   benefits defense establishments on both sides, and subordinates
 *   developing economies and regional states to great-power strategic
 *   competition. The constraint exhibits genuine coordination elements
 *   (accidental war prevention through deterrence protocols, arms control
 *   negotiations, crisis communication channels) alongside structural
 *   extraction (military budgets justified through threat inflation, NATO
 *   expansion benefiting member states, sanctions regimes benefiting
 *   sanctioning powers through market realignment). Eastern European buffer
 *   states experience the constraint as nearly pure extraction — trapped
 *   between NATO encirclement logic and Russian sphere-of-influence doctrine,
 *   bearing costs of proxy conflicts and strategic instrumentalization. The
 *   theater ratio has increased over the measurement interval, indicating
 *   growing performative content as institutions maintain competitive
 *   positioning despite reduced ideological justification.
 *
 * KEY AGENTS:
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — defense spending justified, technology development accelerated, procurement pipelines expanded through Russian threat narrative
 *   - Russian Defense Establishment: Primary beneficiary (institutional/arbitrage) — nuclear deterrent provides geopolitical status despite economic constraints, strategic competition justifies institutional budgets
 *   - NATO Alliance: Secondary beneficiary/mixed (organized/constrained) — collective defense coordination is genuine but expansion benefits select members asymmetrically, burden-sharing disputes generate internal extraction
 *   - Eastern European Buffer States: Primary victim (powerless/trapped) — structurally trapped between NATO and Russian spheres, bear costs of proxy conflicts, sanctions regimes, and strategic instrumentalization with no genuine exit
 *   - Nuclear Non-Proliferation Regime: Primary victim (powerless/trapped) — strategic competition incentivizes proliferation, bilateral deterrence logic undermines collective non-proliferation norms, abstract collective good cannot organize
 *   - Multilateral Trade Institutions: Mixed victim/coordinator (powerful/constrained) — genuine coordination function in managing trade flows subordinated to sanctions regimes that serve strategic positioning
 *   - Cold War Institutional Legacy: Institutional inertia actor (institutional/constrained) — intelligence agencies, deterrence protocols, adversarial frameworks persist through institutional momentum despite changed material conditions
 *   - Arms Control Advocates: Organized reform coalition (organized/mobile) — recognize constraint as containing solvable coordination problem; building alternative pathways (New START, ICBM transparency) with genuine sunset potential
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_russia_strategic_relations, 0.58).
domain_priors:suppression_score(us_russia_strategic_relations, 0.72).
domain_priors:theater_ratio(us_russia_strategic_relations, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_russia_strategic_relations, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_russia_strategic_relations, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_russia_strategic_relations, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_russia_strategic_relations, tangled_rope).
narrative_ontology:human_readable(us_russia_strategic_relations, "US-Russia Strategic Relations Constraint").
narrative_ontology:topic_domain(us_russia_strategic_relations, "geopolitical/international_relations").

domain_priors:requires_active_enforcement(us_russia_strategic_relations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_russia_strategic_relations, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_russia_strategic_relations, russian_defense_establishment).
narrative_ontology:constraint_beneficiary(us_russia_strategic_relations, nato_expansion_advocates).
narrative_ontology:constraint_beneficiary(us_russia_strategic_relations, sanctions_regime_beneficiaries).
narrative_ontology:constraint_victim(us_russia_strategic_relations, developing_economies).
narrative_ontology:constraint_victim(us_russia_strategic_relations, eastern_european_buffer_states).
narrative_ontology:constraint_victim(us_russia_strategic_relations, global_nuclear_stability).
narrative_ontology:constraint_victim(us_russia_strategic_relations, bilateral_trade_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL NUCLEAR STABILITY (SNARE) — Cannot exit the strategic competition framework; bears full cost of escalation risks and proliferation incentives. The non-proliferation commons is subordinated to bilateral strategic positioning. Maximum extraction with no exit — abstract collective good cannot organize or resist.
constraint_indexing:constraint_classification(us_russia_strategic_relations, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EASTERN EUROPEAN BUFFER STATES (SNARE) — Structurally trapped between NATO expansion logic and Russian sphere-of-influence doctrine. Cannot exit without existential risk from either direction; bear full cost of proxy conflicts, sanctioning regimes, and strategic instrumentalization. No genuine exit option despite nominal sovereignty.
constraint_indexing:constraint_classification(us_russia_strategic_relations, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: NATO ALLIANCE (TANGLED ROPE) — Experiences genuine coordination function (collective defense against perceived threats) AND asymmetric extraction (NATO expansion benefits select members, nuclear guarantee concentrates power in US hands, burden-sharing disputes). Organized agents with constrained exit — cannot withdraw from alliance without strategic vulnerability, but benefit from deterrence coordination.
constraint_indexing:constraint_classification(us_russia_strategic_relations, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary from strategic competition. Experiences constraint as pure coordination: military spending is justified, technology development accelerates, procurement pipelines expand. Net beneficiary through defense contracts, research funding, and institutional prestige. Exit is arbitrage — they can shift to civilian technology or defense for other adversaries, but competition with Russia maximizes their power.
constraint_indexing:constraint_classification(us_russia_strategic_relations, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RUSSIAN DEFENSE ESTABLISHMENT (ROPE) — Mirrors US beneficiary position. Primary extractor. Experiences constraint as coordination: strategic competition justifies military spending, nuclear deterrence sustains geopolitical status, NATO encirclement framing consolidates institutional power. Net beneficiary despite economic constraints. Exit is arbitrage — they can pivot to other conflicts or partnerships, but US opposition maximizes their relevance.
constraint_indexing:constraint_classification(us_russia_strategic_relations, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTILATERAL TRADE INSTITUTIONS (TANGLED ROPE) — Experience both genuine coordination function (managing trade flows, development coordination across ideological divides) and asymmetric extraction (sanctions regimes subordinate trade neutrality to strategic positioning; sanctions targeting countries benefit sanctioning powers through market realignment). Constrained exit — institutions must operate within great-power strategic competition framework.
constraint_indexing:constraint_classification(us_russia_strategic_relations, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COLD WAR INSTITUTIONAL LEGACY (PITON) — Strategic frameworks (nuclear deterrence protocols, intelligence agencies, adversarial positioning) persist through institutional inertia despite changed material conditions. Much of the constraint's enforcement is performative: threat assessment rituals, diplomatic posturing, military exercises demonstrate strength rather than prepare for actual conflict. Theater ratio reflects that substantial activity maintains the strategic competition frame without genuine coordination or extraction — the institutional machinery perpetuates itself.
constraint_indexing:constraint_classification(us_russia_strategic_relations, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ARMS CONTROL AND MULTILATERAL ADVOCATES (SCAFFOLD) — See strategic competition as a temporary coordination failure with a sunset: New START renewal, ICBM transparency measures, space weaponization limits, and joint non-proliferation frameworks are building alternative constraint pathways. Mobile agents with real exit options through diplomatic channels. This perspective recognizes the constraint as containing a genuine coordination problem (preventing accidental escalation) that can be resolved through institutional redesign.
constraint_indexing:constraint_classification(us_russia_strategic_relations, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a civilizational analytical lens, strategic competition between nuclear-armed peer powers is a structural inevitability of great-power systems: distribution of capabilities creates security dilemmas that incentivize competitive positioning. This perspective sees the constraint as an immutable property of multipolar power systems. However, the empirical data contradicts this naturalization — strategic competition intensified significantly after Cold War end, suggesting the constraint is contingent on specific policy choices rather than inherent to bipolarity or great-power systems.
constraint_indexing:constraint_classification(us_russia_strategic_relations, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_russia_strategic_relations_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_russia_strategic_relations, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_russia_strategic_relations, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_russia_strategic_relations, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_russia_strategic_relations, TR),
    TR >= 0.70.

:- end_tests(us_russia_strategic_relations_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant resources from multiple actors — global military spending justified through Russian threat, market realignment benefits from sanctions, NATO expansion concentrating power in Western institutions, developing economies bearing costs of great-power competition. However, extractiveness is not as severe as pure snare (≥0.66) because genuine coordination functions exist: deterrence protocols prevent accidental escalation, crisis communication channels manage risk, arms control negotiations reduce some weapons categories. The extraction is real but partially offset by coordination. Suppression (0.72): High. Eastern European states face severe barriers to exit — NATO membership creates strategic dependency, but remaining outside creates vulnerability to Russian coercion. Sanctions regimes limit economic alternatives for targeted states. Nuclear modernization creates technology barriers. Career incentives within defense establishments suppress alternative policies. However, some exit pathways exist through multilateral institutions and diplomatic engagement. Theater ratio (0.65): Moderate-high. Significant institutional activity serves performative functions: military exercises demonstrate strength, threat assessments maintain internal justification, diplomatic posturing signals resolve. However, core deterrence machinery and intelligence networks have genuine functional content — not purely theatrical. The ratio has increased over time as Cold War ideological justification faded and institutional maintenance became more explicitly performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates acute perspectival divergence across power levels and structural positions. The US and Russian defense establishments see primarily rope (coordination justifying military budgets and strategic status). NATO alliance members see primarily tangled_rope (genuine collective defense mixed with asymmetric expansion benefits). Eastern European buffer states see primarily snare (structural trap with no exit). The global non-proliferation regime sees snare (strategic competition incentivizing proliferation). Arms control advocates see scaffold (solvable coordination problem with sunset pathways). Cold War institutional machinery sees piton (performative maintenance of frameworks). The analytical observer risks seeing mountain (great-power competition as immutable structural necessity) but empirical analysis suggests this is false summit — naturalization of contingent institutional arrangements. The perspectival gap between beneficiaries and victims is maximum: the constraint enriches defense establishments while impoverishing developing economies and endangering nuclear stability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values map agent structural positions to extraction flow. US military-industrial complex and Russian defense establishment are primary extractors (d~0.1-0.15, low/negative χ) — they benefit from strategic competition and have arbitrage options; framework existence favors them. NATO as collective entity is mixed (d~0.4-0.5, moderate χ) — coordinated deterrence benefits members but expansion concentrates power. Eastern European states are full targets (d~0.9-0.95, high χ) — trapped by strategic positioning with no arbitrage. Non-proliferation regime is full target (d~0.95, maximum χ) — abstract collective good cannot organize. Multilateral trade institutions are constrained victims (d~0.65-0.70, high χ) — forced to operate within strategic framework. Arms control advocates have mobile exit (d~0.40-0.45, moderate χ) — can pursue alternative frameworks. Cold War institutions are institutional beneficiaries through inertia (d~0.1-0.20, low χ) — persistence justifies funding. Analytical observer is pure observer (d~0.72, χ scales to 1.15) — sees full structure from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that tangled_rope classification captures the core structural reality: US-Russia relations contain both genuine coordination (preventing accidental escalation, managing nuclear risks) and structural extraction (military budget justification, market realignment through sanctions, NATO expansion benefits). The constraint cannot be classified as pure rope (too much asymmetric extraction) or pure snare (too much genuine deterrence coordination). The mandatrophy dissolves when perspectives are fully specified: beneficiaries see rope, victims see snare, the balanced observer sees tangled_rope. The false mountain perspective (naturalizing great-power competition as immutable) is revealed by empirical data showing extractiveness has increased despite reduced ideological justification — if competition were structural inevitability, it should show steady state, not accumulation. The theater ratio's rise indicates institutional maintenance is becoming more performative relative to functional content, supporting piton classification for Cold War machinery while overall constraint remains tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_paradox,
    'Does nuclear deterrence coordination between US and Russia stabilize the system or create mutual vulnerability that increases accident risk?',
    'Historical analysis of near-misses and escalation incidents; comparison of accident rates during Cold War vs post-Cold War periods; modeling of decision-maker cognitive load under crisis scenarios',
    'If stabilizing: constraint is primarily rope (coordination function prevents war). If destabilizing: constraint is primarily snare (mutual vulnerability extraction masked as deterrence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_paradox, empirical, 'Whether deterrence coordination stabilizes or destabilizes nuclear security').

omega_variable(
    nato_expansion_necessity,
    'Is NATO expansion toward Russian borders a genuine security response to Russian threat or a strategic extraction mechanism that itself creates threat perception?',
    'Longitudinal analysis of NATO expansion timing relative to Russian actions; counterfactual analysis of Eastern European security outcomes with/without NATO membership; Russian threat perception studies before/after each expansion wave',
    'If genuine response: NATO perspective is rope (collective defense coordination). If extraction mechanism: NATO perspective is tangled_rope or snare (expansion benefits select members at cost to regional stability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nato_expansion_necessity, empirical, 'Whether NATO expansion is security response or strategic extraction').

omega_variable(
    sanctions_efficacy_vs_retaliation,
    'Do sanctions regimes against Russia achieve stated policy objectives (behavior change, economic coercion) or primarily generate retaliatory escalation while benefiting sanctions-enforcing powers through market realignment?',
    'Comparative analysis of Russian policy changes pre/post-sanctions with counterfactual scenarios; measurement of sanctions-imposed economic costs vs benefits to sanctioning coalition members; tracking of geopolitical escalation correlation with sanctions rounds',
    'If efficacious: sanctions are coordination mechanism for collective security (rope). If retaliation-generating: sanctions are extraction disguised as enforcement (snare/tangled_rope from victim perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_efficacy_vs_retaliation, empirical, 'Whether sanctions achieve objectives or generate escalatory retaliation').

omega_variable(
    institutional_path_dependency,
    'To what degree do Cold War-era institutional structures (NATO, bilateral intelligence networks, nuclear protocols) persist because they genuinely solve coordination problems vs because organizational inertia and career incentives maintain them?',
    'Comparative institutional analysis of new coordination mechanisms for post-Cold War threats (cyber security, climate/resource competition, pandemics); measurement of Cold War institutional activity levels on genuine vs performative tasks; career incentive analysis within strategic agencies',
    'If problem-solving: piton classification is inaccurate — institutions are rope. If inertial: theater_ratio will remain elevated as institutions perform rather than function. Affects sunset clause viability for scaffold perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_path_dependency, empirical, 'Whether institutional persistence reflects functional necessity or inertia').

omega_variable(
    great_power_competition_inevitability,
    'Is strategic competition between US and Russia an immutable feature of great-power systems or a contingent institutional arrangement that could be restructured?',
    'Historical comparison of non-competitive great-power coexistence periods (Concert of Europe, brief post-Cold War moments); modeling of alternative institutional frameworks for managing capability asymmetries; analysis of zero-sum vs positive-sum framings in strategic culture',
    'If immutable: mountain classification partially justified — constraint reflects structural limits of multipolar systems. If contingent: mountain is false summit — naturalization of policy choices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(great_power_competition_inevitability, conceptual, 'Whether great-power competition is structural inevitability or contingent institutional design').

omega_variable(
    eastern_european_agency_window,
    'What fraction of Eastern European countries'' foreign policy autonomy is constrained by the US-Russia competition framework vs independently chosen?',
    'Comparative foreign policy analysis of countries with security dilemma exposure; measurement of policy divergence from great-power preferences; survey data on perceived strategic autonomy; analysis of successful policy independence incidents',
    'If minimal autonomy: snare classification confirmed — states are fully trapped. If significant: classification shifts to tangled_rope (mixed coordination/extraction) — states have agency within constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eastern_european_agency_window, empirical, 'Degree of Eastern European foreign policy autonomy under great-power competition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_russia_strategic_relations, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usrus_tr_t0, us_russia_strategic_relations, theater_ratio, 0, 0.35).
narrative_ontology:measurement(usrus_tr_t10, us_russia_strategic_relations, theater_ratio, 10, 0.55).
narrative_ontology:measurement(usrus_tr_t20, us_russia_strategic_relations, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(usrus_be_t0, us_russia_strategic_relations, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(usrus_be_t10, us_russia_strategic_relations, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(usrus_be_t20, us_russia_strategic_relations, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_russia_strategic_relations, enforcement_mechanism).
narrative_ontology:affects_constraint(us_russia_strategic_relations, nato_expansion).
narrative_ontology:affects_constraint(us_russia_strategic_relations, nuclear_proliferation).
narrative_ontology:affects_constraint(us_russia_strategic_relations, sanctions_regimes).
narrative_ontology:affects_constraint(us_russia_strategic_relations, eastern_european_sovereignty).

% DUAL FORMULATION NOTE:
% US-Russia strategic relations decomposes into multiple structurally distinct constraints: NATO expansion (ε=0.65, snare from Eastern European perspective), nuclear deterrence (ε=0.35, rope from both principals), sanctions regimes (ε=0.52, tangled_rope from trade perspective), and Cold War institutional persistence (ε=0.20, piton). Each component has distinct beneficiaries, victims, and classification patterns. The parent constraint aggregates these into a coherent extraction flow.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_russia_strategic_relations, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
