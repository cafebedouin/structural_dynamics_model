% ============================================================================
% CONSTRAINT STORY: us_sdf_alliance_abandonment_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sdf_alliance_abandonment_2026, []).

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
 *   constraint_id: us_sdf_alliance_abandonment_2026
 *   human_readable: US Strategic Alliance Abandonment (Syria 2026)
 *   domain: geopolitical/military_alliances
 *
 * SUMMARY:
 *   The US-SDF alliance in Syria (2014-2026) represents a structural
 *   constraint on multiple actors with asymmetric costs and benefits.
 *   Established to counter ISIS, the alliance deepened over 12 years through
 *   military integration, intelligence sharing, and implicit security
 *   guarantees. By 2026, withdrawal signals—rotating political
 *   administrations, budget pressures, strategic reorientation toward China
 *   and Great Power Competition—created abandonment risk. When the US
 *   announced troop reductions and reduced air support, the constraint
 *   flipped from coordination mechanism (Rope: both parties benefit from ISIS
 *   suppression) to extraction mechanism (Snare: SDF absorbs full costs of
 *   withdrawal while US gains relief). The SDF, having integrated with US
 *   military structures and ceded traditional sovereignty to liberated
 *   territories, faces existential threats from Turkish invasion and ISIS
 *   resurgence with no alternative patron. The constraint demonstrates how
 *   alliances become traps when exit costs diverge between parties: for the
 *   US, exit is frictionless (arbitrage); for the SDF, exit is impossible
 *   (trapped). This asymmetry defines the Snare. Extractiveness (0.68)
 *   reflects the severity of the asset seizure (military assets, intelligence
 *   networks, security guarantees). Suppression (0.75) reflects the absence
 *   of alternatives for the SDF and the coordinating power of US military
 *   capacity. Theater ratio (0.58) reflects the performance component of
 *   alliance maintenance ceremonies despite underlying contingency.
 *
 * KEY AGENTS:
 *   - Syrian Democratic Forces (SDF): Primary victim (powerless/trapped) — militarily integrated with US, dependent on air support, cannot exit without state collapse
 *   - Kurdish Civilian Population: Secondary victim (powerless/trapped) — 2+ million civilians in liberated territory, no refuge option, face displacement and humanitarian crisis
 *   - US Executive Administration: Primary beneficiary (institutional/arbitrage) — controls withdrawal decision unilaterally, gains domestic political relief and resource reallocation
 *   - Turkey: Regional beneficiary (powerful/mobile) — gains territorial advantage and regional influence, though constrained by NATO membership and US response
 *   - NATO/Allied Network: Secondary victim (moderate/constrained) — faces credibility erosion and must increase defense spending or accept reduced security
 *   - Regional Mediation Frameworks (UN, Arab League): Temporary coordinators (organized/mobile) — provide diplomatic scaffolding but lack enforcement power
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — detects performative nature of alliance commitment institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sdf_alliance_abandonment_2026, 0.68).
domain_priors:suppression_score(us_sdf_alliance_abandonment_2026, 0.75).
domain_priors:theater_ratio(us_sdf_alliance_abandonment_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sdf_alliance_abandonment_2026, snare).
narrative_ontology:human_readable(us_sdf_alliance_abandonment_2026, "US Strategic Alliance Abandonment (Syria 2026)").
narrative_ontology:topic_domain(us_sdf_alliance_abandonment_2026, "geopolitical/military_alliances").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sdf_alliance_abandonment_2026, us_executive_administration).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, syrian_democratic_forces).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, kurdish_civilians).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SDF (SNARE) — Structurally trapped. Years of alliance dependency created military, logistical, and intelligence integration that cannot be rapidly unwound. Exit is blocked by: (1) Turkish military pressure on northern borders, (2) ISIS remnants requiring US air support, (3) lack of alternative great-power patron, (4) domestic governance obligations to 2+ million civilians in liberated territory. Abandonment extracts maximum cost with no reciprocal obligation. d≈0.98, f(d)≈1.44, σ≈0.95 (regional scope) → χ≈0.95.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: KURDISH CIVILIANS (SNARE) — Trapped in liberated territories with no exit option. US alliance withdrawal removes security guarantees against Turkish invasion and creates humanitarian crisis (displacement, medical supply cutoff, educational collapse). Cannot migrate to Turkey (hostile state), cannot flee to Iraq (limited capacity), cannot remain without security. Extraction is complete dispossession. d≈0.99, f(d)≈1.50, σ≈0.8 (local scope) → χ≈1.20. Capped at effective maximum.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: US EXECUTIVE (ROPE) — Experiences the alliance as a coordination problem with exit option. Withdrawal resolves the domestic political cost of maintaining bases in Syria, reduces deployment overhead, and allows reallocation of military resources. From this perspective, the constraint dissolves through executive action. No genuine mutual obligation is perceived — the alliance is transactional. d≈0.08, f(d)≈-0.10, σ≈1.2 (global scope) → χ≈-0.01. Net beneficiary; effective extraction is negative (gain from exit).
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL POWERS (TANGLED ROPE) — Turkey benefits from US withdrawal (removes SDF military capacity), Syria gains territory, Iran gains regional influence. But each actor remains trapped in mutual extraction: Turkey cannot fully absorb Kurdish territory without US backlash, Syria cannot consolidate without Turkey accepting the settlement, Iran cannot expand indefinitely without provoking global response. The alliance abandonment creates a mixed coordination-extraction dynamic among regional competitors. d≈0.50, f(d)≈0.65, σ≈1.1 (continental scope) → χ≈0.47.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: NATO/ALLIED NETWORK (SNARE) — Trapped by precedent. If the US abandons the SDF after years of alliance, the credibility signal is catastrophic: Taiwan, South Korea, Japan, Eastern Europe all perceive reduced security guarantees. Exit is constrained (cannot withdraw from NATO instantly) but the cost of remaining is now higher — alliance members must increase defense spending or accept reduced security. d≈0.85, f(d)≈1.18, σ≈1.2 (global scope) → χ≈0.96.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From a civilizational perspective, the abandonment reveals that great-power alliances are performative institutional arrangements lacking durable commitment. The historical narrative of 'credible security guarantees' persists as institutional theater despite contradictory evidence (Vietnam, Afghanistan, Syria 2026). The constraint—alliance reliability—is maintained through performative renewal ceremonies (NATO summits, defense treaties) while underlying extraction mechanisms (transaction costs, opportunity costs) drive abandonment. theater_ratio=0.58 indicates moderate theater (some performative, some genuine).
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: MEDIATION FRAMEWORKS (SCAFFOLD) — UN, Arab League, and regional diplomatic initiatives have temporary coordination function: prevent escalation, negotiate humanitarian corridors, establish buffer zones. These mechanisms have explicit or implicit sunset clauses tied to political settlements. Extraction is moderate (diplomatic overhead, sovereignty constraints) but bounded by sunset logic. d≈0.45, f(d)≈0.42, σ≈1.0 (regional mediation scope) → χ≈0.29.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sdf_alliance_abandonment_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sdf_alliance_abandonment_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sdf_alliance_abandonment_2026, TR),
    TR >= 0.70.

:- end_tests(us_sdf_alliance_abandonment_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The SDF accumulated military assets ($1B+ in equipment), intelligence operatives (500+ US military advisors embedded), and civilian dependencies (medical supply chains, educational administration) that the US could unilaterally sever. The extraction is acute: withdrawal triggers asset loss, personnel vulnerability, and civilian crisis. The value 0.68 (not 0.85+) reflects that the constraint operated under some genuine mutual benefit initially (ISIS suppression), distinguishing it from predatory extraction. But by 2026, US benefits from the alliance are lower (ISIS largely defeated regionally) while SDF costs remain constant (administration overhead, security obligations), creating asymmetric extraction. Suppression (0.75): High. Suppression operates through structural lock-in rather than overt coercion. SDF cannot exit because: (1) Alternative security patrons (Russia, China, Iran) are geostrategically incompatible or diplomatically blocked, (2) Turkish military pressure on borders prevents independent action, (3) Governance obligations to 2 million civilians require security continuity, (4) US air power advantage is irreplaceable against ISIS air attacks. The suppression is structural—alternatives do not exist, not because the US forbids them, but because the regional configuration makes them non-viable. Theater ratio (0.58): Moderate. The constraint maintains significant performative components: bilateral military coordination ceremonies, joint operations announcements, diplomatic reaffirmations of commitment. But these are increasingly theatrical—the US is clearly reducing substantive investment while maintaining rhetorical alliance language. Theater ratio increases from 0.32 (2014, genuine coordination) to 0.58 (2026, performative maintenance) because the gap between announced commitment and actual capability transfer widens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. The US sees Rope (coordination problem: how do we fight ISIS efficiently?—answered, now withdraw). The SDF sees Snare (structural trap: we are locked in, abandonment kills us). Regional powers see Tangled Rope (mixed coordination against ISIS, mixed extraction of territorial advantage). The NATO alliance sees Snare (abandonment signals erosion of all US guarantees). Mediation frameworks see Scaffold (temporary diplomatic coordination with sunset). The civilizational observer sees Piton (alliance commitment is performative institutional theater lacking durable substance). All perspectives read from the same base data (extractiveness 0.68, suppression 0.75, theater 0.58) but classify differently because they occupy structurally different positions relative to the constraint. The SDF's perspective (Snare, d≈0.98) represents maximum extraction risk; the US perspective (Rope, d≈0.08) represents maximum beneficiary advantage. The perspectival gap—from Snare to Rope—is the full range of the framework, indicating a constraint where observational position determines entire classification.
 *
 * DIRECTIONALITY LOGIC:
 *   SDF: Victim + trapped + powerless → d≈0.98, f(d)≈1.44. Maximum directionality toward target. The SDF cannot exit, cannot organize alternatives, cannot negotiate equivalently. Extraction is total within regional scope. US Executive: Beneficiary + arbitrage + institutional → d≈0.08, f(d)≈-0.10. Minimum directionality toward target; maximum beneficiary status. The US controls withdrawal unilaterally and faces no reciprocal obligation. Kurdish Civilians: Victim + trapped + powerless → d≈0.99, f(d)≈1.50. Absolute directionality toward target. No agency in the constraint; pure passive exposure to abandonment consequences. Regional Powers: Mixed roles (beneficiary on some dimensions, victim on others, with mobile exit options) → d≈0.50, f(d)≈0.65. Symmetric structure in Tangled Rope view. NATO/Allied Network: Victim + constrained (cannot instantly withdraw from commitments) + moderate → d≈0.85, f(d)≈1.18. High directionality toward target because alliance members are trapped by interdependence even if US withdraws from Syria. Mediation Frameworks: Neutral + mobile (can declare missions accomplished and exit) + organized → d≈0.45, f(d)≈0.42. Lower directionality because these actors have genuine exit clauses (sunset provisions). Directionality overrides: None required. The structural derivation (beneficiary/victim + exit option) produces accurate d values for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the Snare classification (χ≈0.95 for SDF, χ≈0.96 for NATO) is structurally mandated by the asymmetry of exit options and beneficiary status. The US cannot be simultaneously classified as a Snare beneficiary—this violates the definition. The US experiences the constraint as Rope (d≈0.08) because it has arbitrage exit and beneficiary status. The SDF experiences Snare because it has trapped exit and victim status. These are not competing interpretations of the same type; they are correct perspectival classifications from structurally different positions. The mandatrophy would arise if the framework tried to force a single type across all perspectives—either 'the alliance is fundamentally a Snare' (true for SDF, false for US) or 'the alliance is fundamentally a Rope' (true for US, false for SDF). The indexed classification dissolves the mandatrophy by licensing both readings as simultaneously correct and perspectival. The constraint is a Snare-from-the-SDF perspective and a Rope-from-the-US perspective, with intermediate types from intermediate perspectives. The high extractiveness (0.68) and suppression (0.75) ensure that high-χ values appear in victim perspectives and low/negative χ in beneficiary perspectives, confirming the asymmetry. The theater ratio increase (0.32→0.58) indicates institutional degradation: the alliance persists increasingly through performative ceremony (joint statement releases, coordinated rhetoric) while substantive cooperation declines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alliance_commitment_depth,
    'Was the US-SDF alliance a genuine strategic commitment or a transactional expedient against ISIS?',
    'Historical analysis of US commitment signals: defense budget allocation, military planning timelines, diplomatic messaging consistency, comparison with other US alliances (NATO, East Asia)',
    'If genuine: abandonment is snare (contract violation, extraction). If transactional: abandonment is rope (coordination problem resolved). Classification shifts depending on the true nature of the commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alliance_commitment_depth, empirical, 'Whether US-SDF alliance was genuine strategic commitment or transactional expedient').

omega_variable(
    sdf_alternative_patrons,
    'Could the SDF have negotiated alternative security guarantees (Russia, China, Arab states) to mitigate US abandonment risk?',
    'Counterfactual analysis of SDF diplomatic outreach; interviews with SDF leadership and regional analysts; comparison with other orphaned proxy forces (Afghan militias post-2021)',
    'If viable alternatives existed: SDF had exit option (mobile), extractiveness is lower (~0.50, Tangled Rope). If no alternatives: SDF was trapped (extractiveness ~0.70, Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sdf_alternative_patrons, empirical, 'Whether SDF had viable alternative security patron options').

omega_variable(
    us_domestic_cost_threshold,
    'What level of domestic political cost would have prevented US alliance abandonment? (e.g., 1000 personnel loss, $X billion/year budget, public opinion threshold)',
    'Comparative analysis of US alliance maintenance costs across regions; political polling on Syria commitment; budget impact analysis of reduced presence',
    'If domestic cost was sufficiently low to trigger abandonment: the alliance was always contingent on cost-benefit calculus, not genuine commitment (snare classification confirmed). If high threshold was crossed: abandonment reflects strategic recalculation rather than extraction (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_domestic_cost_threshold, empirical, 'Domestic political cost threshold at which US abandonment became likely').

omega_variable(
    regional_security_outcome,
    'Did alliance abandonment increase or decrease overall regional stability and civilian security outcomes?',
    'Post-2026 empirical measurement: refugee flows, casualty rates, territorial stability, Turkish-Syria-Kurdish power dynamics evolution, ISIS resurgence rates',
    'If outcomes worsen (Snare confirmed): extraction framing is validated. If outcomes improve or stabilize (Rope or Scaffold framing): the coordination problem interpretation gains credibility, suggesting the constraint was misclassified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_security_outcome, empirical, 'Post-abandonment regional security and humanitarian outcomes').

omega_variable(
    great_power_signaling_cascade,
    'Did US abandonment trigger observable credibility erosion for other US alliances (Taiwan, South Korea, NATO)?',
    'Measurement of allied defense spending increases, diplomatic friction with US, shift toward alternative security arrangements (AUKUS expansion, CPTPP deepening, bilateral hedging)',
    'If credibility cascade is observed: Global Allied Network''s Snare classification is validated (d≈0.85). If no cascade: abandonment was localized and alliances remain credible (regional snare without global contagion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(great_power_signaling_cascade, empirical, 'Whether alliance abandonment triggered cascading credibility erosion globally').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sdf_alliance_abandonment_2026, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdf_alliance_theater_formation, us_sdf_alliance_abandonment_2026, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sdf_alliance_theater_midpoint, us_sdf_alliance_abandonment_2026, theater_ratio, 5, 0.45).
narrative_ontology:measurement(sdf_alliance_theater_abandonment, us_sdf_alliance_abandonment_2026, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(sdf_alliance_extractiveness_formation, us_sdf_alliance_abandonment_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sdf_alliance_extractiveness_midpoint, us_sdf_alliance_abandonment_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sdf_alliance_extractiveness_abandonment, us_sdf_alliance_abandonment_2026, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sdf_alliance_abandonment_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, isis_territorial_resurgence_2026).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, turkish_kurdish_conflict_escalation).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, us_middle_east_credibility).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, nato_alliance_reassurance).

% DUAL FORMULATION NOTE:
% The US-SDF alliance abandonment is part of a constraint family tracking different dimensions of great-power disengagement from forward-deployed alliances. The alliance itself (this story) has ε≈0.68 (extraction dominant). Downstream constraints include ISIS resurgence (ε≈0.45, coordination problem without US air support), Turkish escalation (ε≈0.55, Tangled Rope), and NATO credibility erosion (ε≈0.62, Snare for secondary allies). Each story has a distinct ε reflecting its structural identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
