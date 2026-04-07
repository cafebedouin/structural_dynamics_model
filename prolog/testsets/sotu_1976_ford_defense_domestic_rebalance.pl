% ============================================================================
% CONSTRAINT STORY: sotu_1976_ford_defense_domestic_rebalance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1976_ford_defense_domestic_rebalance, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1976_ford_defense_domestic_rebalance
 *   human_readable: Ford Administration Defense/Domestic Budget Reallocation (1976)
 *   domain: military/political_economy
 *
 * SUMMARY:
 *   In 1976, President Gerald Ford proposed a fundamental reallocation of
 *   federal budget resources from domestic social programs toward military
 *   and defense spending, explicitly framed as restoring 'a new balance' in
 *   the face of perceived Soviet military buildup and the need to maintain
 *   strategic deterrence. This constraint exemplifies how a single policy
 *   mechanism generates radically different classification outcomes depending
 *   on the observer's structural position within the political economy. For
 *   defense contractors and allied nations, the reallocation is a
 *   coordination mechanism solving the collective action problem of
 *   deterrence. For social program beneficiaries facing service cuts, it is
 *   pure extraction. For the organized opposition, it is a contested
 *   reallocation constrained by countervailing power. For the Ford
 *   administration and Cold War strategic analysts, it appears as an
 *   immutable structural necessity of bipolar superpower competition — a
 *   false summit that naturalizes a political choice as a geopolitical
 *   imperative. The theater ratio (0.48) reflects that the reallocation was
 *   presented through rhetoric of strategic necessity and Soviet threat
 *   rather than direct discussion of the welfare tradeoff — the actual debate
 *   about whose programs would be cut and why was subordinated to threat
 *   narratives.
 *
 * KEY AGENTS:
 *   - Defense Contractors: Primary beneficiary (institutional/arbitrage) — capture expanded procurement contracts and R&D funding with significant market power to influence procurement strategy
 *   - Social Program Beneficiaries: Primary victim (powerless/trapped) — face cuts in healthcare, education, poverty assistance, housing, with minimal exit options or collective leverage
 *   - Labor Unions & Domestic Sector Workers: Secondary victim/moderate actor (moderate/constrained) — some workers benefit (military manufacturing), others harmed (social service sector); possess collective bargaining and legislative voice but constrained by overall political economy
 *   - NATO Allied Nations: Secondary beneficiary (institutional/arbitrage) — gain enhanced U.S. strategic commitment but have options to adjust own defense spending and alliance relationships
 *   - Congressional Democrats & Anti-War Coalition: Organized opposition (organized/constrained) — provide countervailing power through legislative process and electoral leverage; constrained by institutional rules and Cold War consensus
 *   - Ford Administration/Strategic Deterrence Advocates: Primary institutional decision-maker (institutional/arbitrage) — frames reallocation as necessary response to exogenous geopolitical threat; benefits from expanded strategic capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1976_ford_defense_domestic_rebalance, 0.52).
domain_priors:suppression_score(sotu_1976_ford_defense_domestic_rebalance, 0.58).
domain_priors:theater_ratio(sotu_1976_ford_defense_domestic_rebalance, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1976_ford_defense_domestic_rebalance, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1976_ford_defense_domestic_rebalance, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1976_ford_defense_domestic_rebalance, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1976_ford_defense_domestic_rebalance, tangled_rope).
narrative_ontology:human_readable(sotu_1976_ford_defense_domestic_rebalance, "Ford Administration Defense/Domestic Budget Reallocation (1976)").
narrative_ontology:topic_domain(sotu_1976_ford_defense_domestic_rebalance, "military/political_economy").

domain_priors:requires_active_enforcement(sotu_1976_ford_defense_domestic_rebalance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1976_ford_defense_domestic_rebalance, defense_contractors).
narrative_ontology:constraint_beneficiary(sotu_1976_ford_defense_domestic_rebalance, military_allied_nations).
narrative_ontology:constraint_beneficiary(sotu_1976_ford_defense_domestic_rebalance, strategic_deterrence_advocates).
narrative_ontology:constraint_victim(sotu_1976_ford_defense_domestic_rebalance, social_program_beneficiaries).
narrative_ontology:constraint_victim(sotu_1976_ford_defense_domestic_rebalance, domestic_infrastructure_users).
narrative_ontology:constraint_victim(sotu_1976_ford_defense_domestic_rebalance, low_income_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOCIAL PROGRAM BENEFICIARIES (SNARE) — Trapped within a political economy where reallocation is presented as necessary and inevitable; face reduction in healthcare, education, housing, and poverty assistance with minimal exit options. No mechanism for collective resistance or meaningful participation in the decision. Maximum experienced extraction — cannot exit federal system, cannot substitute services with alternatives, cannot organize effective countervailing power at the scale where the budget decision is made.
constraint_indexing:constraint_classification(sotu_1976_ford_defense_domestic_rebalance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR UNIONS & DOMESTIC SECTOR WORKERS (TANGLED ROPE) — Constrained by political economy and labor market dependence, but possess some collective bargaining power and legislative voice. The reallocation benefits some workers (military manufacturing) while harming others (domestic service providers). Mixed extraction: the constraint coordinates labor market response to geopolitical threat while extracting from workers in non-defense sectors through job reductions and wage stagnation. Constrained exit — retrain into defense sector or exit labor force.
constraint_indexing:constraint_classification(sotu_1976_ford_defense_domestic_rebalance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE CONTRACTORS (ROPE) — Experiences the reallocation as coordination mechanism for solving the strategic problem of Soviet parity. Captures expanded contracts, R&D funding, and production capacity through predictable procurement cycles. High arbitrage options — can pivot to new weapons systems, maintain excess capacity for escalation, coordinate lobbying across administration transitions. Net beneficiary with significant agency. Extraction flows toward this sector.
constraint_indexing:constraint_classification(sotu_1976_ford_defense_domestic_rebalance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIED NATIONS (ROPE) — Benefit from U.S. strategic commitment to deter Soviet expansion, secured through enhanced military capacity. Experience the constraint as coordination mechanism (U.S. burden-sharing in collective security). Have arbitrage options — can adjust own defense spending, reallocate within defense budgets, or shift alignment. Net beneficiary, though secondary to direct contractors. Extraction secured through military alliance mechanism.
constraint_indexing:constraint_classification(sotu_1976_ford_defense_domestic_rebalance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPPOSITION COALITION (TANGLED ROPE) — Organized legislative actors and anti-war constituencies see both coordination and extraction. The constraint requires active enforcement through appropriations and rhetoric about Soviet threat; organized opposition provides genuine countervailing power that constrains how extreme the reallocation can be. Neither pure Rope (opposition prevents pure coordination logic) nor pure Snare (organization provides exit options through legislative process). Constrained by institutional rules and electoral cycles but possess real agency through legislative majority, committee power, and electoral leverage.
constraint_indexing:constraint_classification(sotu_1976_ford_defense_domestic_rebalance, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (MOUNTAIN) — From a civilizational/universal view of superpower competition, the reallocation appears as an immutable structural constraint: the bipolar Cold War order requires military parity or risks strategic collapse. Nuclear deterrence logic dictates that relative military capacity determines national security. From this framing, the reallocation is not a policy choice but a structural imperative of the international system itself. However, the beneficiary/victim declarations reveal this as a false summit — the constraint naturalizes a political-economy choice as an international-structure necessity.
constraint_indexing:constraint_classification(sotu_1976_ford_defense_domestic_rebalance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1976_ford_defense_domestic_rebalance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1976_ford_defense_domestic_rebalance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1976_ford_defense_domestic_rebalance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1976_ford_defense_domestic_rebalance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1976_ford_defense_domestic_rebalance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reallocation extracts from domestic programs and their beneficiaries to fund military expansion. However, the extraction is not maximal because the reallocation is presented as a genuine response to a perceived geopolitical threat (Soviet military buildup) rather than pure rent-seeking. Some portion of the budgetary shift plausibly responds to a real strategic problem, though the magnitude and necessity are contested. The extractiveness value reflects that while domestic constituencies bear clear costs, the justification (deterrence, strategic parity) has legitimate structural appeal even if contestable. Suppression (0.58): Moderate-high. The reallocation is enforced through presidential authority, congressional appropriations, and the ideological consensus around Cold War deterrence logic. Opposition exists but operates within constrained channels (Congress, elections, protest). Media and policy discourse privilege threat narratives over welfare impact narratives, suppressing explicit debate about winners and losers. Suppression is not total because organized opposition can challenge spending through legislative process, but the Cold War consensus creates strong barriers to effective resistance. Theater ratio (0.48): Moderate. The reallocation is justified through rhetoric about Soviet threat and strategic necessity, which contains both genuine strategic analysis and performative threat narratives. The actual budgetary impact on domestic beneficiaries is less prominently discussed than the strategic justification. Theater is not high because the administration does not hide the reallocation itself — it explicitly frames it as a deliberate policy shift — but the welfare impact is subordinated to threat framings.
 *
 * PERSPECTIVAL GAP:
 *   Defense contractors see the reallocation as pure coordination (Rope): the military-industrial complex views its role as solving the collective action problem of deterrence. The administrative apparatus experiences it as necessary policy coordination in response to geopolitical threat. Social program beneficiaries see it as pure extraction (Snare): their programs are cut, their exit options are minimal, and the decision process excludes their voice. Congressional opposition sees it as contested reallocation (Tangled Rope): genuine security coordination mixed with extractive defense industry lobbying; they have organized power through legislative process but face institutional constraints. The analytical observer risks naturalizing the reallocation as an immutable law of bipolar competition (Mountain perspective) — but the structural data reveals this as false summit: the reallocation is enforced by active political choice, enabled by suppression of welfare-impact debate, and opposed by organized actors who could theoretically prevent it through legislative coalition. The 'necessity' is institutional (Cold War consensus) not structural (laws of physics).
 *
 * DIRECTIONALITY LOGIC:
 *   Defense contractors experience low directionality (d ≈ 0.15): they are net beneficiaries with arbitrage options (can pivot to new systems, maintain excess capacity). Social program beneficiaries experience high directionality (d ≈ 0.85): they are net victims with trapped or highly constrained exit options (cannot exit federal system, cannot organize at appropriate scale). Congressional opposition experiences mid-range directionality (d ≈ 0.55): organized but constrained, with mixed positions (some benefit from military jobs, others harmed by service cuts). The sigrectangular function f(d) maps these d values to effective extraction experienced: beneficiaries with arbitrage experience low chi despite high base extractiveness; trapped victims experience high chi. The perspectival gap reflects that the same structural reallocation is experienced as Rope by beneficiaries, Snare by victims, and Tangled Rope by organized actors with partial agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy through genuine ambiguity about whether the reallocation is pure coordination (solving deterrence collective action problem) or pure extraction (defense industry capturing budget resources). The tangled_rope classification holds because: (1) Genuine coordination function exists: deterrence does require military capacity, and the reallocation plausibly responds to this. (2) Genuine extraction asymmetry exists: defense contractors benefit disproportionately, social program beneficiaries bear concentrated costs. (3) Active enforcement required: both rhetoric about threat necessity and legislative appropriations must be actively maintained to prevent reversal. The false summit signal fires in Perspective 6 because the Mountain (structural necessity) classification obscures the fact that the reallocation is enforced through active political choice and opposed by organized actors. The mountain perspective demonstrates how strategic necessity narratives can naturalize contingent political choices. The constraint resolves mandatrophy by demonstrating that all six classifications are legitimate perspectival readings: no single type captures the full structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soviet_threat_credibility,
    'Was the Soviet military threat assessment in 1976 accurate enough to justify the magnitude of domestic reallocation, or did threat inflation serve as cover for defense spending preferences?',
    'Declassified intelligence estimates; comparison of actual Soviet capabilities against contemporaneous claims; ex-post analysis of threat materialization',
    'If threat accurately assessed: reallocation is coordinated response to real geopolitical shift (Rope classification more justified). If threat inflated: reallocation is extractive through manufactured necessity (Snare classification strengthened), and false summit signal triggers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_threat_credibility, empirical, 'Whether Soviet threat assessment justified reallocation magnitude').

omega_variable(
    domestic_program_substitutability,
    'Could domestic social programs be cut without catastrophic welfare loss, or were the programs providing irreplaceable services?',
    'Comparative state-level analysis of program impacts; longitudinal health/poverty outcomes in high-cut states; examination of private sector substitution effectiveness',
    'If substitutable: extraction is moderate (services continue through alternate means). If irreplaceable: extraction is severe (vulnerable populations lose essential services permanently). Affects classification from powerless perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_program_substitutability, empirical, 'Whether cuts substitutable or created irreplaceable service gaps').

omega_variable(
    democratic_mandate_legitimacy,
    'Did the reallocation reflect genuine democratic preference through elections, or was it imposed through elite consensus obscuring public preferences?',
    'Public opinion polling on defense vs domestic spending priorities; electoral analysis of anti-defense candidates; examination of whether reallocation was debated in campaigns',
    'If mandate clear: suppression is lower (democratic process provides exit mechanism and voice). If elite-imposed: suppression is higher (powerless agents have no democratic recourse). Affects suppression metric and Rope vs Snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_mandate_legitimacy, conceptual, 'Whether reallocation reflected or circumvented democratic preferences').

omega_variable(
    military_industrial_complex_autonomy,
    'How much agency did defense contractors exert in shaping the reallocation decision versus passive response to administration-initiated strategic shift?',
    'Lobbying records and political action committee spending; congressional testimony patterns; revolving-door analysis of Ford administration defense officials; comparison with alternative threat-response scenarios',
    'If high contractor agency: constraint is Snare of defense industry extracting via threat manipulation (high chi for contractors, benefits as extractors). If low: constraint is pure coordination response to exogenous geopolitical shift (Rope logic holds). Affects beneficiary directionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_industrial_complex_autonomy, empirical, 'Contractor agency in shaping reallocation versus responding to administration initiative').

omega_variable(
    reallocation_permanence,
    'Is this reallocation a temporary adjustment to Cold War conditions (Scaffold with sunset), or a permanent structural shift in U.S. budget allocation?',
    'Longitudinal budget trends post-Ford; subsequent administrations'' budget priorities; end of Cold War impact on defense spending; structural analysis of whether defense-to-domestic ratio ever re-equilibrates',
    'If temporary: classify as Scaffold with sunset (reallocation reverses when geopolitical conditions change). If permanent: tangled rope or snare depending on other factors. Affects temporal classification and constraint longevity model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reallocation_permanence, empirical, 'Whether reallocation is temporary Cold War adjustment or permanent structural shift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1976_ford_defense_domestic_rebalance, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ford_def_dom_tr_t0, sotu_1976_ford_defense_domestic_rebalance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ford_def_dom_tr_t2, sotu_1976_ford_defense_domestic_rebalance, theater_ratio, 2, 0.45).
narrative_ontology:measurement(ford_def_dom_tr_t4, sotu_1976_ford_defense_domestic_rebalance, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(ford_def_dom_be_t0, sotu_1976_ford_defense_domestic_rebalance, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ford_def_dom_be_t2, sotu_1976_ford_defense_domestic_rebalance, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(ford_def_dom_be_t4, sotu_1976_ford_defense_domestic_rebalance, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1976_ford_defense_domestic_rebalance, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1976_ford_defense_domestic_rebalance, cold_war_deterrence_arms_race).
narrative_ontology:affects_constraint(sotu_1976_ford_defense_domestic_rebalance, welfare_state_legitimacy_1970s).
narrative_ontology:affects_constraint(sotu_1976_ford_defense_domestic_rebalance, military_industrial_complex_procurement).

% DUAL FORMULATION NOTE:
% This constraint operates at the intersection of three structural domains: (1) Cold War geopolitical competition requiring military coordination; (2) domestic political economy of budget allocation; (3) defense industry lobbying and procurement. Decomposition would separate the strategic necessity argument from the interest-capture mechanism, but they are empirically entangled in the single 1976 Ford proposal. Related constraints in the network capture: upstream deterrence logic (cold_war_deterrence_arms_race) that is cited to justify reallocation; downstream welfare impacts (welfare_state_legitimacy_1970s) as beneficiaries face service cuts; procurement mechanism (military_industrial_complex_procurement) through which contractors extract value from appropriations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
