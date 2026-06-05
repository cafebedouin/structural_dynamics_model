% ============================================================================
% CONSTRAINT STORY: rotation_seven_kubo_ranking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rotation_seven_kubo_ranking, []).

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
 *   constraint_id: rotation_seven_kubo_ranking
 *   human_readable: R7 Kubo Credit and Ranking System
 *   domain: economic/social
 *
 * SUMMARY:
 *   The Kubo Credit and Ranking System operates on the Rotation Seven
 *   generation ship as a primary labor-allocation and resource-distribution
 *   mechanism. Ostensibly meritocratic, it gamifies crew performance across
 *   dozens of dimensions (labor shifts, skill certifications, social
 *   compliance, maintenance quotas) and assigns ranks (Tier 1-10) that
 *   directly determine resource access (residential quarters quality, food
 *   rations, water allocation, reproductive licensing, recreational
 *   privileges). The system has evolved over seven crew rotations
 *   (approximately 250-300 years of ship time) from explicit authoritarian
 *   labor quotas into a gamified compliance mechanism that achieves
 *   comparable extraction through internalized participation rather than
 *   overt coercion. Crew members voluntarily invest time and effort in rank
 *   optimization because survival resources are scarce and rank-contingent.
 *   The constraint exhibits the structural signatures of a snare: high
 *   extractiveness (0.68), high suppression (0.72 — enforced through resource
 *   termination and isolation), low-to-moderate coordination benefit (the
 *   system coordinates labor, but this function could be achieved through
 *   transparent democratic allocation), and increasing theater ratio over
 *   generational time (0.38 → 0.65) indicating that ranking mechanics have
 *   become increasingly performative and disconnected from actual ship
 *   operational needs. From different structural positions, the same system
 *   appears variously as pure extraction (to trapped low-ranked crew), as
 *   coordination (to command elite), as temporary scaffolding (to mid-tier
 *   reformists), as degraded institutional theater (to historical analysts),
 *   and as latent coalition conflict (to organized crew networks).
 *
 * KEY AGENTS:
 *   - Ship Command Elite (~150 people, Tiers 1-3): Institutional/arbitrage. Beneficiaries of ranking system. Experience coordination function; access resources outside formal ranking; control tier advancement criteria.
 *   - Low-Ranked Crew (Tier 7-10, ~1,500 people): Powerless/trapped. Primary victims. Survival-resource contingent on ranking. No exit options; cannot refuse labor without resource termination.
 *   - Mid-Tier Crew (Tier 4-6, ~1,200 people): Organized/constrained. Secondary victims but organized. Believe system is reformable. Actively push for transparency and merit-based advancement. Have some collective power but constrained by suppression.
 *   - Ranking Tier Controllers (~100 administrative crew): Institutional/arbitrage. Secondary beneficiaries. Administer ranking systems; control advancement appeals; have discretionary authority over border-case rankings.
 *   - Crew Aspiration Economy (abstract collective): Powerless/trapped. Victim of performative competition (crew optimize for ranking metrics rather than ship function). Internalization of ranking legitimacy maintains system stability.
 *   - Meritocratic Reformists (subset of mid-tier, ~200 people): Organized/constrained. Activists for ranking transparency. See the system as fixable scaffolding rather than irreformable snare. Their existence provides pressure valve preventing coalition formation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotation_seven_kubo_ranking, 0.68).
domain_priors:suppression_score(rotation_seven_kubo_ranking, 0.72).
domain_priors:theater_ratio(rotation_seven_kubo_ranking, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, extractiveness, 0.68).
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotation_seven_kubo_ranking, snare).
narrative_ontology:human_readable(rotation_seven_kubo_ranking, "R7 Kubo Credit and Ranking System").
narrative_ontology:topic_domain(rotation_seven_kubo_ranking, "economic/social").

domain_priors:requires_active_enforcement(rotation_seven_kubo_ranking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rotation_seven_kubo_ranking, ship_command_elite).
narrative_ontology:constraint_beneficiary(rotation_seven_kubo_ranking, ranking_tier_controllers).
narrative_ontology:constraint_victim(rotation_seven_kubo_ranking, low_ranked_crew).
narrative_ontology:constraint_victim(rotation_seven_kubo_ranking, rotation_seven_laborers).
narrative_ontology:constraint_victim(rotation_seven_kubo_ranking, crew_aspiration_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-RANKED LABORER (SNARE) — Trapped in the ship with no escape. Kubo ranking directly determines resource access (quarters quality, food rations, water allocation, reproductive licenses). Cannot exit or refuse labor. The ranking system is presented as meritocratic gamification ('improve your score through work!') but operates as pure extraction: resources are scarce by design, ranking advancement requires not just labor but zero-defect performance in arbitrary metrics, and lower tiers subsidize elite consumption. Behavioral compliance is enforced through rank-contingent survival resource access. Maximum experienced extraction.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CREW COLLECTIVE / COALITION POTENTIAL (TANGLED ROPE) — The 3,000+ low and mid-rank crew members vastly outnumber the elite, creating latent coalition power. The system functions through both coordination (crew labor IS necessary for ship survival) and extraction (the ranking structure concentrates benefits upward despite this necessity). Constrained exit: organized mutiny is theoretically possible but carries extreme penalties (derank, isolation, resource termination). The perspective classifies as tangled rope because the coordination function (crew labor essential to ship operation) is genuine, but the extraction (resource concentration via ranking) is asymmetric and enforced. Active coordination exists (labor org, information networks among crew) but remains suppressed through rank-contingent penalties.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: SHIP COMMAND ELITE (ROPE) — The ~150-person upper command tier experiences the Kubo system as a coordination mechanism: it organizes labor allocation, incentivizes specialized skill development, and distributes roles without constant coercive oversight. The elite perceive legitimate meritocracy ('we advanced through performance too'). Arbitrage options: command officers can negotiate tier placement, lateral moves between departments, and resource access outside formal ranking (access to restricted compartments, exemptions from labor rotation). The system appears as effective coordination with minimal overhead from their perspective. The ranking structure solves their coordination problem (how to manage 3,000+ crew without direct authoritarian control). Low experienced extraction because they see proportional benefit from the system's function.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: MERITOCRATIC REFORMISTS (SCAFFOLD) — Mid-tier crew members (Tiers 4-6) who believe the system CAN work fairly believe it is a temporary structure pending better information systems. They see ranking as scaffolding toward genuine meritocracy: 'once we have better sensors, training records, and peer evaluation, the current rough proxy metrics will be replaced.' They actively work to reform the system (advocating for transparency, pushing for appeal mechanisms, documenting performance metrics). This perspective sees the Kubo system as high-theater (rank algorithms are not actually visible to crew; advancement criteria are opaque; appeals are handled in closed command meetings) but transitional. The sunset clause is implicit: 'once ship resources become less scarce' or 'once AI-assisted management arrives' the ranking system will become unnecessary. Low effective extraction because they have organized agency and see an exit path (reformed meritocracy). Theater ratio is high (0.65) because the ranking mechanics are performative — they present as mathematical/fair while operating through opaque criteria.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: HISTORICAL MAINTENANCE (PITON) — From a civilizational view of multi-generational ship operations, the Kubo system appears as a degraded residual of more explicit control mechanisms. Ship logs show earlier eras (Rotation 1-3) used explicit forced labor quotas and punishment hierarchies. The Kubo system replaced these through gamification — the same extraction with performative appeal mechanics. It persists not because it's optimal but because alternatives (pure authoritarian quotas, democratic resource allocation) were tried and abandoned. The ranking system has atrophied from a genuine performance-management tool (early rotations, small crew) into a theater-heavy extraction mechanism (later rotations, crew familiarity with the game's rules, ossified advancement criteria). Theater ratio 0.65 reflects that much crew time is spent on rank-optimization activities (reporting metrics, strategizing advancement) rather than productive work. Functional decay is visible: rankings no longer correlate with competence; senior crew hold top ranks through seniority, not performance; the system's original intent (performance incentive) has been replaced by performative participation (maintaining rank becomes the goal, not improving ship operations).
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL VIEW (SNARE) — From an analytical standpoint examining the structural data without observant bias, the Kubo system is a snare: it extracts labor and compliance through mechanism design rather than explicit coercion. The gamification layer (ranking, score displays, advancement narratives) is the critical mechanism — it transforms resource scarcity (objectively necessary on a closed ship) into a behavioral extraction tool. Crew are motivated to maximize cooperation and suppress dissent not because command forces them (suppression is 0.72 but not total) but because the ranking system ties survival resources to compliance. The system works because crew internalize the ranking metrics as legitimate even when they're arbitrary. This is pure extraction: the resource allocation could be transparent and democratic; instead it's opaque and elite-favoring. The 'meritocratic' framing naturalizes extraction as fairness. The analytical view reveals that this is not a mountain (natural law of scarcity requiring ranking) but a snare: scarcity is real, but the choice to allocate via hidden ranking is contingent and extractive.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotation_seven_kubo_ranking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rotation_seven_kubo_ranking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rotation_seven_kubo_ranking, TR),
    TR >= 0.70.

:- end_tests(rotation_seven_kubo_ranking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The Kubo system concentrates resource access upward despite universal crew labor contribution. Low-ranked crew contribute essential maintenance labor but receive minimal resources; elite contribute specialized labor (command, engineering, medical) and receive disproportionate resources. The gap persists because ranking advancement is opaque and suppression of advancement (via criteria opacity, discretionary appeals, elite family preference) is systematic. The extractiveness is not as high as a slavery system (0.85+) because crew retain some agency in ranking mechanics (they can develop skills, earn certifications) and some rare individuals do advance. But the baseline is high because the system is designed to extract compliance through resource scarcity rather than to fairly distribute according to contribution. Suppression (0.72): High. Barriers include: (1) information suppression — ranking criteria are published but advancement mechanisms are opaque; (2) resource termination — refusal to participate in ranking metrics results in lower tier and reduced resource allocation; (3) isolation mechanisms — low-ranked crew live in confined quarters and have restricted movement privileges; (4) social pressure — crew internalize ranking legitimacy and police each other's compliance. Suppression is not total (crew cannot be coerced into involuntary labor, and some information channels exist) but is severe enough to make exit functionally impossible for most crew. Theater ratio (0.65): Moderate-high. Ranking systems are presented as mathematical and meritocratic but operate through opaque criteria. Crew spend significant time on rank-optimization activities (reporting metrics, strategizing advancement, petitioning for recognition) that do not directly contribute to ship operations. The early-rotation theater ratio was lower (0.38) because the ranking system was newer and had higher correlation with actual ship needs (new systems need performance optimization). Over time (by Rotation 7), the criteria have ossified (seniority dominates, specialization preferences harden) and the system has become increasingly performative. The theater increase signals institutional decay: the original function (performance incentive) has atrophied and been replaced by performative participation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival structure reveals how a single extractive mechanism can appear as coordination, scaffolding, degradation, and snare simultaneously from different structural positions. The gaps are not measurement disagreements but structural reality: the system has both genuine coordination properties (crew labor is necessary for ship survival; ranking does allocate specialized tasks) and genuine extraction properties (resources are concentrated upward; advancement is opaque; exit is suppressed). The mandatrophy is resolved by recognizing that all perspectives are structurally true: the system IS coordination for command (who benefit from organized labor allocation), IS snare for low-ranked crew (who are trapped and extracted from), IS tangled rope for mid-tier crew (who experience both coordination and extraction), IS temporary scaffolding for reformists (who see an exit path through transparency), IS degraded theater for historians (who see atrophied function), and IS snare for analytical observers (who see pure extraction hidden by gamification). The perspectival divergence is maximal because the structural positions differ across all four axes (power, time, exit, scope), and the constraint operates through internalized legitimacy rather than explicit coercion. If the system were purely authoritarian (all perspectives saw the same ranking as extraction), perspectival gap would be minimal. Because it's gamified and meritocratic-framed, it appears differently to those who benefit (coordination) and those who bear costs (extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values encode each agent's structural position relative to the extraction flow. Command elite: d ≈ 0.05 (full beneficiaries with arbitrage exit — derived from beneficiary status + arbitrage exit options → low d → negative effective extraction f(d) ≈ -0.12). They experience the system as coordination because they sit above the extraction point. Low-ranked crew: d ≈ 0.92 (nearly full extraction targets with trapped exit — derived from victim status + trapped exit options + dependent survival resources → high d → high f(d) ≈ 1.38). They experience maximum extraction because they are the extraction target. Mid-tier organized crew: d ≈ 0.58 (mixed position with constrained exit — derived from mixed beneficiary/victim status + organized power + constrained exit → moderate d → moderate f(d) ≈ 0.73). They experience extraction but retain some agency. Ranking tier controllers: d ≈ 0.15 (beneficiaries with moderate power over the system — derived from beneficiary status + administrative arbitrage → low d → negative/minimal f(d)). The suppression value (0.72) applies uniformly across all perspectives — it is a structural property of the system, not observer-dependent. But the experienced effective extractiveness (χ) differs dramatically by directionality: for command elite, χ ≈ 0.68 × (-0.12) × 1.0 ≈ -0.08 (system subsidizes them); for low-ranked crew, χ ≈ 0.68 × 1.38 × 1.0 ≈ 0.94 (system extracts from them). The same base extractiveness (0.68) and suppression (0.72) produce opposite valences depending on position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The Kubo system is classified as a snare (not coordination, not scaffolding) despite genuine coordination properties and legitimacy-internalization. The resolution process: (1) Identify genuine coordination function: crew labor allocation is necessary for ship survival; ranking DOES reduce information costs for task assignment and skill matching. (2) Identify genuine extraction: resources are concentrated upward despite universal contribution; ranking opaqueness is systematic; low-ranked crew subsidize elite consumption. (3) Assess asymmetry: the coordination benefit is achievable through transparent, democratic allocation; the extraction is contingent on ranking's opacity and gaming. (4) Apply mandate test: would this system exist without the extraction benefit to elite? Historical evidence suggests yes — explicit labor allocation (Rotations 1-3) existed. Would it exist without the coordination function? Possibly but less efficiently. Conclusion: the coordination function is real but secondary; the extraction is primary. (5) Final classification: snare. The system achieves high suppression (0.72) and extractiveness (0.68) through gamification and legitimacy theater rather than through explicit coercion. The mandatrophy is resolved by showing that the 'meritocratic' framing is the mechanism of extraction, not a reason to classify it as rope. The system is a snare precisely because it disguises extraction as fair coordination through ranking mechanics. The scaffolding perspective (reformists) is structurally sound but operationally marginal — transparency reforms are theoretically possible but blocked by beneficiaries (command elite, tier controllers) who would lose extraction power. The piton perspective is valid but not primary — the system is not yet degraded enough to be maintaining only theater; it still delivers extraction effectively. The mountain perspective (scarcity as natural law) is a false summit — it naturalizes a contingent institutional choice (to allocate via hidden ranking rather than transparent distribution) as a law of physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_authenticity,
    'Is resource scarcity on the ship genuinely physical (ship capacity, recycling limits) or artificially maintained by command for control purposes?',
    'Comparative analysis of Rotation 7 resource production vs historical logs from Rotations 1-3; archaeological survey of ship''s material stores; carbon/nitrogen cycle accounting. If earlier rotations had more per-capita resources with similar crew size, scarcity is artificial.',
    'If artificial: the Kubo system is pure coordination theater masking engineered scarcity (snare intensity increases to 0.85+). If genuine: ranking remains extraction but loses part of its legitimacy for elite hoarding (snare intensity remains 0.68 but mandatrophy shifts from ''disguised pure extraction'' to ''fair rationing of real scarcity'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_authenticity, empirical, 'Whether resource scarcity is physical necessity or artificially maintained').

omega_variable(
    ranking_algorithm_transparency,
    'Do ranking advancement criteria actually match the published metrics, or are hidden factors (crew attitude, family connections, command discretion) the primary drivers?',
    'Statistical analysis of advancement rates vs published criteria; interview crew with identical published metrics who advanced at different rates; audit command decision logs (if accessible). Correlation analysis between declared criteria and actual outcomes.',
    'If criteria match outcomes: meritocratic scaffolding perspective is structurally sound (ranking system CAN be reformed toward transparency, sunset clause is credible). If hidden factors dominate: scaffolding is aspirational theater, and the system is a pure snare without genuine reform pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ranking_algorithm_transparency, empirical, 'Whether ranking criteria are actually applied as published').

omega_variable(
    crew_coalition_threshold,
    'At what crew organization level (information networks, leadership coordination, collective action readiness) does the latent coalition power become manifest challenge to command authority?',
    'Sociometric analysis of crew communication networks; identification of informal leadership structures; historical precedent from ship''s log of any organized crew action (work slowdowns, strikes, mutiny attempts). Assessment of command''s capacity to suppress organized action (security force size, isolation mechanisms, resource termination capacity).',
    'If critical mass threshold is <1,000 crew members: the organized coalition perspective''s constrained exit may become mobile exit (mutiny becomes feasible), reclassifying the system as tangled rope with active threat. If threshold >2,000: suppression is sufficient to maintain snare despite latent power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crew_coalition_threshold, empirical, 'Coalition formation threshold for crew organized challenge').

omega_variable(
    gamification_internalization,
    'Do crew accept ranking metrics as legitimate performance measures, or is compliance performative (appearing to accept ranking while covertly rejecting its values)?',
    'Ethnographic observation of crew discourse about ranking; analysis of private crew communications (logs, messages) vs public expressions; psychological assessment of ranking''s internalized vs external motivation. Surveys on ''do you believe ranking reflects true merit?''',
    'If legitimacy is internalized: snare operates through soft power (beliefs), and psychological exit becomes possible (crew could reject rankings'' legitimacy, shifting from snare to tangled rope or rope depending on suppression response). If compliance is performative: snare is maximally stable because crew maintain the performance despite disbelief, making the extraction invisible and nearly unbreakable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gamification_internalization, conceptual, 'Whether crew internalize ranking legitimacy or performatively comply').

omega_variable(
    elite_reproduction_closure,
    'Can low-ranked crew actually advance to top tiers, or is elite status functionally hereditary (children of commanders default to high starting ranks)?',
    'Intergenerational advancement tracking (parents'' rank vs. children''s final rank); advancement rate comparison for elite-family crew vs. non-elite crew with identical performance metrics. Identification of barriers (access to training, command sponsorship, early-life resource advantages).',
    'If advancement is possible: meritocratic scaffolding is credible, and the system could be reformed. If elite status is functionally closed: the system is a breeding mechanism for permanent class separation, intensifying extraction (snare evolves toward aristocratic lock-in). Mandatrophy shifts from ''exploitative but mobilizable'' to ''irreformable class system''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_reproduction_closure, empirical, 'Whether elite status is hereditary or genuinely meritocratic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotation_seven_kubo_ranking, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kubo_tr_t0, rotation_seven_kubo_ranking, theater_ratio, 0, 0.38).
narrative_ontology:measurement(kubo_tr_t150, rotation_seven_kubo_ranking, theater_ratio, 150, 0.52).
narrative_ontology:measurement(kubo_tr_t300, rotation_seven_kubo_ranking, theater_ratio, 300, 0.65).

% Extraction over time
narrative_ontology:measurement(kubo_be_t0, rotation_seven_kubo_ranking, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(kubo_be_t150, rotation_seven_kubo_ranking, base_extractiveness, 150, 0.58).
narrative_ontology:measurement(kubo_be_t300, rotation_seven_kubo_ranking, base_extractiveness, 300, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotation_seven_kubo_ranking, resource_allocation).
narrative_ontology:affects_constraint(rotation_seven_kubo_ranking, generational_reproduction_licensing).
narrative_ontology:affects_constraint(rotation_seven_kubo_ranking, elite_quarters_access_distribution).
narrative_ontology:affects_constraint(rotation_seven_kubo_ranking, crew_skill_training_bottleneck).

% DUAL FORMULATION NOTE:
% The Kubo system is downstream of physical scarcity (ship capacity, recycling limits) but upstream of specific resource allocation constraints (quarters access, training access, reproductive licensing). The scarcity itself is not debated; the choice to allocate via opaque ranking rather than transparent democratic means is the active constraint. If scarcity is artificial (omega_scarcity_authenticity resolves toward 'artificial'), the snare classification intensifies toward 0.85+. If scarcity is genuine, the snare remains at 0.68 but shifts from 'extraction through engineered scarcity' to 'extraction through hidden allocation of real scarcity.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rotation_seven_kubo_ranking, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
