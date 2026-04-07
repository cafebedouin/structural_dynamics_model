% ============================================================================
% CONSTRAINT STORY: sotu_1987_reagan_soviet_military_containment_budget
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1987_reagan_soviet_military_containment_budget, []).

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
 *   constraint_id: sotu_1987_reagan_soviet_military_containment_budget
 *   human_readable: Reagan Defense Budget for Soviet Military Containment (1987)
 *   domain: military/geopolitical/budgetary
 *
 * SUMMARY:
 *   Reagan's 1987 defense budget allocation ($500 billion annual expenditure,
 *   justified by a claimed $500 billion gap between U.S. and Soviet military
 *   spending) functions as a structural constraint on American domestic
 *   resource allocation and global geopolitical posture. The constraint
 *   operates through Congressional appropriation authority, with the implicit
 *   threat that underfunding Soviet containment invites expansion in proxy
 *   conflicts (Afghanistan, Central America, Angola). The mechanism exhibits
 *   genuine coordination (providing security for NATO allies and allied
 *   states) alongside significant extraction (crowding out domestic social
 *   spending, sustaining military-industrial interests, directing resources
 *   through politically-protected Congressional districts). The constraint's
 *   theater ratio increases over the interval as Soviet economic decline
 *   becomes visible to intelligence analysts (CIA economic intelligence
 *   showing Soviet GDP contraction by 1986) while political rhetoric on the
 *   Soviet threat remains maximized—a classic Goodhart drift where the
 *   performance of containment (measured by rhetoric and appropriations)
 *   diverges from the objective (Soviet military capacity already declining).
 *   By 1989-1991, the constraint's binding force dissolves as the Soviet
 *   Union ceases to exist, validating the Scaffold perspective's prediction
 *   that the constraint has a sunset, but revealing the Piton perspective's
 *   observation: substantial Cold War institutional infrastructure persists
 *   long after the original threat disappears.
 *
 * KEY AGENTS:
 *   - Reagan Administration: Primary organizer (institutional/arbitrage) — frames defense budget as necessary containment; benefits from unified command authority over vast military apparatus
 *   - U.S. Defense Contractors and Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — receives $500 billion annual revenue; has vested interest in sustained Soviet threat narrative
 *   - NATO Allied States (West Germany, Japan, South Korea, Israel): Secondary beneficiary (institutional/arbitrage) — receive security guarantee without proportional cost; can exit through rearmament but face high cost
 *   - Anti-Communist Client Regimes (Afghanistan Mujahideen, Nicaraguan Contras, UNITA Angola): Secondary beneficiary (moderate/trapped) — receive weapons and training; trapped in proxy conflict structure between superpowers
 *   - American Domestic Social Programs (Education, Healthcare, Infrastructure): Primary victim (moderate/constrained) — face budget cuts due to defense crowding-out; constrained by Congressional political economy that protects defense spending
 *   - Soviet-Aligned and Non-Aligned Developing States: Secondary victim (powerless/trapped) — caught in proxy warfare zones; face extraction through superpower competition
 *   - Congress: Institutional actor (organized/arbitrage) — wields appropriation authority; benefits from defense contractor campaign contributions and district-level defense spending; maintains arbitrage option to alter allocation but faces political pressure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing Cold War competition as immutable geopolitical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1987_reagan_soviet_military_containment_budget, 0.58).
domain_priors:suppression_score(sotu_1987_reagan_soviet_military_containment_budget, 0.62).
domain_priors:theater_ratio(sotu_1987_reagan_soviet_military_containment_budget, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1987_reagan_soviet_military_containment_budget, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1987_reagan_soviet_military_containment_budget, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_1987_reagan_soviet_military_containment_budget, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1987_reagan_soviet_military_containment_budget, tangled_rope).
narrative_ontology:human_readable(sotu_1987_reagan_soviet_military_containment_budget, "Reagan Defense Budget for Soviet Military Containment (1987)").
narrative_ontology:topic_domain(sotu_1987_reagan_soviet_military_containment_budget, "military/geopolitical/budgetary").

domain_priors:requires_active_enforcement(sotu_1987_reagan_soviet_military_containment_budget).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_soviet_military_containment_budget, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_soviet_military_containment_budget, nato_allied_states).
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_soviet_military_containment_budget, anti_communist_client_regimes).
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_soviet_military_containment_budget, defense_contractors).
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_soviet_military_containment_budget, congressional_defense_districts).
narrative_ontology:constraint_victim(sotu_1987_reagan_soviet_military_containment_budget, domestic_social_spending).
narrative_ontology:constraint_victim(sotu_1987_reagan_soviet_military_containment_budget, soviet_influence_targets).
narrative_ontology:constraint_victim(sotu_1987_reagan_soviet_military_containment_budget, american_domestic_infrastructure).
narrative_ontology:constraint_victim(sotu_1987_reagan_soviet_military_containment_budget, non_aligned_states_caught_between_superpowers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET-ENCIRCLED DEVELOPING STATES (SNARE) — Non-aligned nations face extraction through proxy warfare and superpower competition. Cannot exit superpower competition; face containment expenditure on both sides draining resources. Maximum suppression: choices between Soviet client status or Western containment. No genuine autonomy in this constraint structure.
constraint_indexing:constraint_classification(sotu_1987_reagan_soviet_military_containment_budget, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AMERICAN DOMESTIC SOCIAL PROGRAMS (TANGLED ROPE) — Genuine coordination function exists: defense budgets fund scientific research, manufacturing capacity, technological development that has civilian spillover (internet, materials science, aerospace). But extraction is severe: $500 billion annual spending crowds out education, infrastructure, healthcare funding. Constrained exit: programs cannot simply reallocate without Congressional reauthorization and sustained political pressure.
constraint_indexing:constraint_classification(sotu_1987_reagan_soviet_military_containment_budget, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATO AND SECURITY BENEFICIARIES (ROPE) — West Germany, Japan, South Korea, Israel receive security guarantee and weapons transfer without bearing proportional cost. U.S. military umbrella functions as pure coordination: solves collective security problem for alliance members. Arbitrage exit: can exit through own rearmament or realignment, but cost is high. Experience the constraint as enabling their security, not extracting from them.
constraint_indexing:constraint_classification(sotu_1987_reagan_soviet_military_containment_budget, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANTI-COMMUNIST COALITION (SCAFFOLD) — Organized actor (Reagan administration, Congress, allied states) sees the constraint as temporary: defeat Soviet expansion in proxy zones (Afghanistan, Central America, Angola), rebuild Soviet economy cannot sustain competition, and the containment spending has a sunset. Theater is moderate: explicit doctrine justifies spending. Mobile exit: once Soviets are deterred or economically weakened, constraint dissolves. By 1989-1991, this perspective proves partially correct — Soviet military competition did end, reducing the constraint's binding force.
constraint_indexing:constraint_classification(sotu_1987_reagan_soviet_military_containment_budget, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL COLD WAR INFRASTRUCTURE (PITON) — Military bases, intelligence networks, weapons caches, alliance structures that existed since 1945 persist through institutional inertia even as the original threat (Soviet expansion) diminishes. Theater high: the 1987 'Soviet military expansion' framing masks that Soviet economy was already contracting by 1986, making the $500 billion gap comparison partially moot. The constraint persists through organizational rigidity, not functional necessity. Piton classification: institutional actors continue performing the containment function long after the structural justification deteriorates.
constraint_indexing:constraint_classification(sotu_1987_reagan_soviet_military_containment_budget, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / REALIST VIEW (MOUNTAIN) — From a civilizational/universal perspective, bipolarity is an immutable structural fact of international relations: two superpowers with incompatible ideologies must maintain deterrent military capacity. The budget constraint is read as a natural law of anarchic interstate competition — no arbiter above states means each must achieve security through military capability. However, the beneficiary declarations and suppression metrics contradict this mountain classification. The engine flags this as false summit: the 'inevitability' of Cold War competition naturalizes what is actually a contingent political arrangement sustained through institutional interest.
constraint_indexing:constraint_classification(sotu_1987_reagan_soviet_military_containment_budget, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1987_reagan_soviet_military_containment_budget_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1987_reagan_soviet_military_containment_budget, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1987_reagan_soviet_military_containment_budget, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1987_reagan_soviet_military_containment_budget, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1987_reagan_soviet_military_containment_budget, TR),
    TR >= 0.70.

:- end_tests(sotu_1987_reagan_soviet_military_containment_budget_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint produces significant extraction in three directions: (1) crowding out of domestic spending (estimated $200-300 billion annual opportunity cost in foregone social investment), (2) concentration of manufacturing and scientific capacity in military rather than civilian applications, (3) geopolitical extraction from developing states caught in proxy competition. However, extractiveness is not at Snare levels (χ ≥ 0.66) because genuine coordination benefits exist — NATO security, technological development, deterrent capability — and because the beneficiaries (NATO, allies, contractors) are not pursuing pure predation, but rather solving a legitimate security coordination problem (albeit with asymmetric distribution of costs and benefits). Suppression (0.62): High. Strong barriers to exit include: (1) Congressional institutional rigidity (defense spending is protected by bipartisan consensus and contractor lobbying), (2) international commitment structures (NATO requires U.S. military guarantees), (3) ideological framing (challenging the Soviet threat narrative carries political cost), (4) path dependency (military-industrial infrastructure cannot be rapidly redirected). Theater ratio (0.65): Moderate-high. Theater increases over the interval from 0.48 to 0.65, indicating Goodhart drift: as Soviet economic decline becomes visible (CIA estimates in 1986-1987), political rhetoric on the Soviet threat does not proportionally decline. The $500 billion gap becomes increasingly theatrical as an objective justification even as underlying Soviet capacity contracts. This is not complete theater (the deterrent function remains real) but a significant performative component that masks the constraint's transition from necessary deterrent to institutionalized inertia. Extractiveness peaks at time point 4 (year 1987 in the narrative) when the political commitment to containment is strongest but Soviet economic decline is still deniable, then slightly decreases by time point 6 as the falsifiability of the threat becomes unavoidable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of perspectival variance produced by the same structural mechanism. The NATO security beneficiary sees pure Rope coordination — the U.S. commitment solves their security problem. The anti-communist client regime sees Tangled Rope — genuine weapons supply (coordination) alongside proxy warfare that may extend their conflict indefinitely (extraction). The American domestic program sees Snare — crowding-out with no offsetting benefit in domestic context. The Congressional actor sees Scaffold — temporary containment measure with a sunset (when Soviets are defeated or economically exhausted). The Cold War institutional infrastructure sees Piton — performative military-readiness rituals persisting through inertia after the original threat function diminishes. The analytical observer risks seeing Mountain — immutable bipolarity — but the structural data (identified beneficiaries, clear extraction flow, Congressional political economy) reveals this as false summit. The perspectival gap is maximal because the same appropriation mechanism produces security coordination for some agents and pure resource extraction for others, with the beneficiaries (contractors, military leadership, allied states) having far greater influence over the constraint's narrative framing than the victims (domestic programs, developing states in proxy zones, future generations who inherit the deficit).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are computed from beneficiary/victim declarations and exit capacity. Defense contractors and military leadership (beneficiaries with secure institutional positions and arbitrage options) derive d ≈ 0.10-0.20, producing low or slightly negative f(d), yielding low chi. NATO allied states (beneficiaries with constrained exit requiring rearmament) derive d ≈ 0.25-0.35, producing f(d) ≈ 0.3-0.5. Domestic programs (victims with constrained exit requiring Congressional reauthorization) derive d ≈ 0.70-0.80, producing f(d) ≈ 1.1-1.3, yielding high chi scaled by scope modifier (σ(national) = 1.0 for domestic programs, σ(global) = 1.2 for geopolitical constraints). Proxy conflict states (victims with trapped exit) derive d ≈ 0.95, producing f(d) ≈ 1.42, yielding near-maximum chi. The asymmetry in directionality across agent positions is the engine's measurement of how unequally the constraint's costs and benefits are distributed — in this case, extremely asymmetrically, with beneficiaries experiencing the constraint as enabling (negative chi) and victims experiencing it as extractive (high chi).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that all six types are perspectivally valid but that the constraint's canonical classification (Tangled Rope) is correct because it is the classification from the agent most affected by the structural mechanism (American domestic budgetary allocation). The false summit (Mountain/analytical observer) naturalizes what is actually a contingent political arrangement: Soviet military competition was real but was already declining by 1987, and the budget constraint persists through institutional inertia and beneficiary interest rather than objective necessity. The Scaffold perspective is partially validated: the constraint does have a sunset (1989-1991) when Soviet collapse removes the original threat. The Piton perspective is also validated: by 1995-2000, the Cold War military infrastructure persists through institutional inertia even after the original threat disappears, with NATO expansion recreating similar constraint logic. The mandatrophy is not 'which type is correct?' but 'how do we recognize that the constraint's framing (necessary deterrent) masks its actual function (resource redistribution toward military-industrial beneficiaries)?' The perspectival gap itself IS the answer: if beneficiaries and victims experienced the constraint identically, the constraint would be pure coordination. The fact that beneficiaries see Rope and victims see Snare reveals the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soviet_economic_capacity_threshold,
    'At what point does Soviet GDP contraction make the $500 billion gap irrelevant to actual military parity?',
    'CIA economic intelligence; Soviet defense spending as percentage of GDP; comparison of actual vs. projected Soviet production capacity in 1987-1991',
    'If Soviet economic decline began by 1985-1986 (as evidence suggests): the containment budget is addressing a threat already in terminal decline, reclassifying the constraint from necessary deterrent (Rope) to extraction theater (Piton). If Soviet economy was genuinely robust: containment budget is legitimate Rope coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soviet_economic_capacity_threshold, empirical, 'Soviet economic capacity relative to defense spending sustainability').

omega_variable(
    domestic_spillover_quantification,
    'What percentage of defense R&D produces net-positive civilian economic spillover vs. military-only capability?',
    'Patent analysis; technology transfer tracking; civilian adoption rates of military technologies (internet, materials, semiconductors); comparison to civilian R&D spending productivity',
    'If spillover > 40%: defense budget functions as genuine Tangled Rope (mixed coordination and extraction). If spillover < 20%: defense budget approaches pure Snare extraction with minimal civilian benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_spillover_quantification, empirical, 'Degree of civilian economic spillover from defense R&D').

omega_variable(
    proxy_conflict_necessity,
    'Did Reagan-era weapons transfers to anti-communist proxies (Contras, Mujahideen, UNITA) achieve their stated goal of degrading Soviet influence, or did they primarily extract resources while extending conflict duration?',
    'Outcome analysis of proxy conflicts: timeline to Soviet withdrawal, comparative costs to U.S. vs. Soviets, post-conflict stability in Afghanistan/Angola/Nicaragua, casualty ratios',
    'If proxy strategy shortened Soviet engagement: validates Scaffold perspective (temporary constraint with sunset). If proxies extended conflicts without strategic endpoint: reclassifies as Snare extraction disguised as containment strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_conflict_necessity, empirical, 'Effectiveness of proxy warfare in achieving containment objectives').

omega_variable(
    false_summit_natural_law_ambiguity,
    'Is bipolar military competition an immutable feature of anarchic international relations, or a contingent Cold War institutional arrangement that could dissolve with different governance structures?',
    'Comparative analysis: how quickly military budgets contracted post-1989; whether NATO expansion (1999+) recreated similar containment logic; analysis of EU integration as alternative to military deterrent; game-theoretic modeling of cooperation pathways under different institutional assumptions',
    'If bipolar competition is inevitable: Mountain classification is correct, constraint is natural law. If institutional alternatives existed: Mountain is false summit, constraint is Tangled Rope or Snare dressed as necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law_ambiguity, conceptual, 'Whether Cold War bipolarity is immutable natural law or contingent institutional arrangement').

omega_variable(
    congressional_district_extraction,
    'To what degree does defense spending distribution across Congressional districts function as political extraction — directing federal resources to swing districts regardless of strategic efficiency?',
    'Geographic analysis of defense contractor concentration; correlation between defense spending per capita and swing-district status; comparison of strategic value vs. budgetary allocation across weapon systems; tracking of cost overruns in politically protected programs',
    'If distribution is geographically rational: defense budget is Rope coordination solving security problem. If distribution follows political logic: budget is Tangled Rope with significant extraction component going to politicians/districts, not to strategic capability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_district_extraction, empirical, 'Degree to which defense spending follows political vs. strategic logic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1987_reagan_soviet_military_containment_budget, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu87_tr_t0, sotu_1987_reagan_soviet_military_containment_budget, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sotu87_tr_t2, sotu_1987_reagan_soviet_military_containment_budget, theater_ratio, 2, 0.58).
narrative_ontology:measurement(sotu87_tr_t4, sotu_1987_reagan_soviet_military_containment_budget, theater_ratio, 4, 0.68).
narrative_ontology:measurement(sotu87_tr_t6, sotu_1987_reagan_soviet_military_containment_budget, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(sotu87_be_t0, sotu_1987_reagan_soviet_military_containment_budget, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sotu87_be_t2, sotu_1987_reagan_soviet_military_containment_budget, base_extractiveness, 2, 0.54).
narrative_ontology:measurement(sotu87_be_t4, sotu_1987_reagan_soviet_military_containment_budget, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(sotu87_be_t6, sotu_1987_reagan_soviet_military_containment_budget, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1987_reagan_soviet_military_containment_budget, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1987_reagan_soviet_military_containment_budget, soviet_military_modernization).
narrative_ontology:affects_constraint(sotu_1987_reagan_soviet_military_containment_budget, proxy_conflict_escalation).
narrative_ontology:affects_constraint(sotu_1987_reagan_soviet_military_containment_budget, nato_alliance_cohesion).
narrative_ontology:affects_constraint(sotu_1987_reagan_soviet_military_containment_budget, american_deficit_accumulation).
narrative_ontology:affects_constraint(sotu_1987_reagan_soviet_military_containment_budget, cold_war_institutional_persistence).

% DUAL FORMULATION NOTE:
% This constraint is part of the broader Cold War structural competition. Upstream constraints include Soviet military capability (independent assessment) and ideological bipolarity (structural international relations). Downstream constraints include specific proxy conflicts (Afghanistan, Nicaragua, Angola), Congressional district-level defense contractor dependencies, and post-Cold War institutional persistence in the form of NATO expansion. The constraint family models how geopolitical structure (bipolarity) produces budget structures (defense appropriations) which produce institutional dependencies (defense contractors, Congressional districts) which persist long after the original geopolitical threat disappears.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1987_reagan_soviet_military_containment_budget, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
