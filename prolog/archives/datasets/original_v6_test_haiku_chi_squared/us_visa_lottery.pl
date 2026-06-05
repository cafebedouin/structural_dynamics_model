% ============================================================================
% CONSTRAINT STORY: us_visa_lottery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_visa_lottery, []).

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
 *   constraint_id: us_visa_lottery
 *   human_readable: US Diversity Visa Lottery
 *   domain: political/economic
 *
 * SUMMARY:
 *   The US Diversity Visa (DV) lottery allocates 50,000 immigrant visas
 *   annually to citizens of countries with historically low rates of
 *   immigration to the United States. Established by the 1990 Immigration Act
 *   as a response to perceived bias favoring European and Asian immigrants,
 *   the lottery presents a structural tension between its stated
 *   diversification goal and its actual operation as an extraction mechanism.
 *   The constraint operates simultaneously as: (1) a coordination mechanism
 *   that distributes access to 50,000 legal residencies (rope perspective),
 *   (2) a temporary safety valve that reduces pressure for more expansive
 *   employment-based or family-based immigration reform (scaffold-adjacent
 *   perspective), (3) an institutional apparatus that concentrates visa
 *   allocation power in the State Department while disguising choices as
 *   random chance (piton perspective), (4) a mechanism that extracts labor
 *   value from low-cost workers while maintaining legal status uncertainty
 *   (snare perspective from worker viewpoint), and (5) a system that
 *   perpetuates global inequality of opportunity by offering minimal,
 *   randomized access to economic mobility (snare perspective from
 *   civilizational view). The constraint's theater ratio (0.58) reflects the
 *   performative character of the 'lottery' framing — the administrative
 *   apparatus around randomness certification, fairness claims, and
 *   anti-fraud measures constitutes substantial overhead relative to the
 *   actual coordination function. Over its 30-year history, extractiveness
 *   has risen from 0.28 to 0.38 as political pressure to expand
 *   employment-based visas has made the lottery a substitute rather than
 *   supplement, and as global inequality has increased the extraction value
 *   (desperation premium) of visa access.
 *
 * KEY AGENTS:
 *   - Unsuccessful Applicants: Primary victims (powerless/trapped) — face random exclusion with no alternative pathway
 *   - High-Immigration Countries (India, Mexico, Philippines): Structural victims (powerless/trapped) — proportionally excluded despite larger applicant pools
 *   - Employment-Based Visa Applicants: Secondary victims (moderate/constrained) — lottery reduces pressure to expand employment visas, extending backlogs
 *   - DV Lottery Winners: Primary beneficiaries (institutional/arbitrage) — access legal residence unavailable through other means
 *   - US Labor Market Employers: Secondary beneficiaries (institutional/arbitrage) — assured diverse, lower-cost labor supply without advocacy pressure for wage increases
 *   - US Congress/Immigration Apparatus: Institutional managers (institutional/constrained) — maintain lottery as political compromise; constrained by competing interests
 *   - Immigration Advocacy Coalition: Organized actors (organized/constrained) — perceive mixed benefits (expanded total immigration) and extraction (low odds, unpredictability)
 *   - International Diplomatic System: Institutional observer (institutional/arbitrage) — lottery maintains fairness narrative while actual allocation reflects geopolitical interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_visa_lottery, 0.38).
domain_priors:suppression_score(us_visa_lottery, 0.65).
domain_priors:theater_ratio(us_visa_lottery, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_visa_lottery, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_visa_lottery, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_visa_lottery, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_visa_lottery, tangled_rope).
narrative_ontology:human_readable(us_visa_lottery, "US Diversity Visa Lottery").
narrative_ontology:topic_domain(us_visa_lottery, "political/economic").

domain_priors:requires_active_enforcement(us_visa_lottery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_visa_lottery, us_labor_market_interests).
narrative_ontology:constraint_beneficiary(us_visa_lottery, visa_lottery_winners).
narrative_ontology:constraint_victim(us_visa_lottery, excluded_high_immigration_countries).
narrative_ontology:constraint_victim(us_visa_lottery, unsuccessful_applicants).
narrative_ontology:constraint_victim(us_visa_lottery, employment_based_visa_queue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNSUCCESSFUL APPLICANT (SNARE) — Trapped by random selection and structural exclusion. 50,000 visas allocated to ~200 countries = extremely low per-country odds. No alternative legitimate immigration pathway for many applicants. Complete suppression of exit options; bears full cost of arbitrary exclusion. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(us_visa_lottery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HIGH-IMMIGRATION COUNTRIES (SNARE) — Structurally excluded from lottery. High-immigration nations receive proportionally fewer DV visas despite larger applicant pools. No formal exit option; suppression occurs through regulatory design. d≈0.90, f(d)≈1.36, σ=0.9 → χ≈0.45.
constraint_indexing:constraint_classification(us_visa_lottery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYMENT-BASED VISA APPLICANTS (TANGLED ROPE) — Constrained by long backlogs (some categories 10+ years). DV lottery both creates alternative pathway (coordination benefit) and substitutes for employment-based expansion (extraction cost). d≈0.68, f(d)≈1.08, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(us_visa_lottery, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DV LOTTERY WINNERS (ROPE) — Pure beneficiaries. Receive legal residence pathway unavailable through other means. Lottery coordinates access to diverse immigration streams. No suppression experienced; can exit into US labor market freely. d≈0.12, f(d)≈0.08, σ=1.2 → χ≈0.04.
constraint_indexing:constraint_classification(us_visa_lottery, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: US LABOR MARKET EMPLOYERS (ROPE) — Net beneficiary. Lottery ensures diverse labor supply across sectors (domestic services, agriculture, tech, healthcare) that might otherwise face shortage or advocacy for wage pressure. Coordination function: randomizes selection, reducing political capture by specific industries. d≈0.08, f(d)≈-0.04, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(us_visa_lottery, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: US IMMIGRATION ADVOCACY COALITION (TANGLED ROPE) — Organized agents (immigrant rights groups, ethnic diaspora organizations) perceive lottery as mixed coordination (expands overall immigration) and extraction (unpredictable, low per-country odds creates dependency on advocacy for success). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.29.
constraint_indexing:constraint_classification(us_visa_lottery, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: US CONGRESS / IMMIGRATION APPARATUS (TANGLED ROPE) — Institutional perspective with constrained exit. Lottery coordinates among competing interests (industry labor demand, ethnic constituency demands, legal immigration targets) but also requires active enforcement (annual lottery administration, visa cap management, fraud prevention). Theater ratio reflects complex administrative theater around fairness and randomness claims. d≈0.45, f(d)≈0.53, σ=1.0 → χ≈0.20.
constraint_indexing:constraint_classification(us_visa_lottery, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: INTERNATIONAL DIPLOMATIC PROTOCOL (PITON) — Lottery mechanism carries historical rationale (post-1965 Act rebalancing) but functions largely as institutional theater. The randomness claim and 'diversity' framing maintain diplomatic legitimacy while actual allocation patterns reflect underlying political economy. Theater ratio 0.58 reflects sustained administrative performance around lottery integrity. d≈0.35, f(d)≈0.33, σ=1.2 → χ≈0.23.
constraint_indexing:constraint_classification(us_visa_lottery, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the lottery is a mechanism that naturalizes global inequality of opportunity by offering minimal, random access. 50,000 visas to ~8 billion people ≈ 0.0006% annual chance globally. Reframes structural exclusion as 'fairness' (equal per-entry odds, ignoring population size). ε=0.38, suppression=0.65 classify as snare. d≈0.80, f(d)≈1.22, σ=1.2 → χ≈0.57.
constraint_indexing:constraint_classification(us_visa_lottery, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_visa_lottery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_visa_lottery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_visa_lottery, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_visa_lottery, TR),
    TR >= 0.70.

:- end_tests(us_visa_lottery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The lottery functions both as a legitimate diversification mechanism and as labor market arbitrage. The base extraction reflects: (a) asymmetry between winners and vast majority of applicants (randomness masks institutional selection), (b) use of visa access as disciplinary mechanism (applicants cannot challenge unfair outcomes due to 'randomness'), (c) extraction of labor value from successful applicants who face visa status uncertainty and employer monopsony power. Not as extreme as pure snare (0.66+) because genuine legal pathways exist for some applicants and lottery winners do receive actual legal status. Suppression (0.65): High. Extremely limited odds (50,000 visas to ~8 billion people globally), no formal appeal mechanism, randomness framing precludes challenge, geopolitical allocation despite stated neutrality. But not total suppression (0.85+) because some alternatives exist (employment-based, family-based sponsorship) and advocacy can influence program parameters. Theater ratio (0.58): Moderate. The administrative apparatus around lottery randomness (certification, fraud prevention, fairness claims) constitutes real overhead, but the lottery also has genuine coordination function (does allocate 50,000 visas). Theater has increased over time as the gap between 'diversity' rhetoric and actual patterns (concentration in underutilized visas from specific regions) has widened.
 *
 * PERSPECTIVAL GAP:
 *   The lottery generates maximum perspectival divergence. From the unsuccessful applicant's view (powerless/trapped), it is pure extraction (snare): random selection disguises institutional power, providing no recourse. From the DV lottery winner's view (institutional/arbitrage), it is pure coordination (rope): they gain legal status through fair random access. From the US labor market employer's view (institutional/arbitrage), it is positive coordination: lottery diversifies labor supply while maintaining political neutrality and reducing demands for wage increases. From the advocacy coalition's view (organized/constrained), it is mixed (tangled rope): expanded immigration (benefit) but unreliable odds and dependency on chance (cost). From the employment-based applicant's view (moderate/constrained), it is extraction: visa slots used for lottery reduce pressure to expand employment categories, extending their own backlogs. From the international diplomatic view (institutional/arbitrage), it maintains facade (piton): framed as fairness while actual allocation reflects geopolitical interests and Congressional pressure for specific diaspora groups. The constraint classifies as snare, rope, tangled rope, and piton simultaneously — the perspectival range reveals that the constraint's actual function (labor arbitrage disguised as randomness) is obscured by its stated function (diversity coordination).
 *
 * DIRECTIONALITY LOGIC:
 *   Unsuccessful applicants: Victim + trapped → d≈0.92. Maximum extraction. No alternative pathway for majority; random exclusion provides no recourse. DV winners: Beneficiary + arbitrage → d≈0.12. Minimal extraction (net negative χ). Free exit into employment, family formation, legal status. US employers: Beneficiary + arbitrage → d≈0.08. Net beneficiary; guaranteed labor supply with minimal political cost. Employment-based applicants: Victim + constrained → d≈0.68. Significant extraction; lottery functions as substitute, not supplement. Immigration advocacy: Organized + constrained → d≈0.55. Mixed; advocacy has some leverage but lottery's randomness constrains their effectiveness. High-immigration countries: Victim + trapped → d≈0.90. Structural exclusion despite larger applicant pools; no formal appeal mechanism. Congress/immigration apparatus: Institutional + constrained → d≈0.45. Moderate extraction; constrained by competing interests, but retains allocation power. International diplomacy: Institutional + arbitrage → d≈0.35. Low extraction; operates through geopolitical interest alignment rather than coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint classifies as tangled rope because it exhibits both genuine coordination function (allocates 50,000 visas that would not otherwise be allocated, serves diversification purpose) AND asymmetric extraction (labor value capture, suppression of alternatives through randomness framing, geopolitical allocation despite neutrality claims). The mandatrophy—'is this coordination or extraction?'—is resolved by showing that both are true simultaneously from different perspectives. The coordination benefit flows to lottery winners and employers (reduced wage pressure). The extraction cost flows to unsuccessful applicants (desperation premium on visa value) and employment-based visa queue (extended backlogs). The active enforcement requirement (annual lottery administration, fraud prevention, quota management) is substantial and necessary to maintain the mixed mechanism. Without enforcement, the lottery would collapse into either pure coordination (if randomness were genuinely neutral) or pure extraction (if selection became transparent). The theater ratio (0.58) reflects the ongoing investment in randomness certification and fairness claims required to sustain the hybrid: the administrative cost per visa allocated (0.58 theater ratio) is moderate, not high, because the coordination function is real (visas are genuinely allocated). If the lottery were purely performative (piton), theater ratio would exceed 0.70. The rising trajectory of extractiveness (0.28→0.38) over 30 years indicates Goodhart drift: as political pressure to expand employment-based visas has increased without resolution, the lottery has drifted from supplementary mechanism toward substitute, increasing extraction from the employment-based queue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lottery_randomness_enforcement,
    'Is the lottery genuinely random, or does the US Department of State exercise discretion in selection/processing that biases outcomes toward specific countries or demographics?',
    'Statistical analysis of historical DV visa grants vs. applications by country, region, and demographic category; FOIA disclosure of selection algorithms and processing discretion; independent audit of randomness claims',
    'If truly random: lottery is pure coordination mechanism (rope). If discretionary: lottery is extraction mechanism disguised as randomness (snare), with institutional beneficiaries choosing winners nominally by chance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lottery_randomness_enforcement, empirical, 'Whether lottery selection is genuinely random or subject to hidden discretion').

omega_variable(
    labor_market_extraction_mechanism,
    'Does the DV lottery primarily serve labor market arbitrage (employer extraction) or genuine diversification of immigration stock?',
    'Longitudinal employment and wage data for DV visa recipients vs. employment-based immigrants; sectoral distribution analysis; employer turnover and dependency patterns; comparison with stated diversification goals vs. actual sectoral concentration',
    'If labor extraction: lottery is tangled rope with high asymmetry (snare from worker perspective). If genuine diversification: lottery is rope with balanced coordination benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_market_extraction_mechanism, empirical, 'Whether DV lottery serves labor market extraction or true diversification').

omega_variable(
    political_capture_of_allocation,
    'Does the lottery allocation respond to Congressional pressure favoring specific diaspora constituencies or geopolitical allies, despite randomness framing?',
    'Analysis of quota adjustments over time; correlation between DV allocations and Congressional district ethnic composition; comparison with countries claiming ''low immigration history'' but receiving exceptional allocations (e.g., Ireland, Eastern Europe)',
    'If capture confirmed: lottery is snare (institutionalized extraction by political interests); if randomness maintained: lottery is rope (genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_capture_of_allocation, empirical, 'Whether lottery allocation is subject to political pressure despite randomness claims').

omega_variable(
    alternative_immigration_pathway_sufficiency,
    'How many applicants genuinely lack alternative legal immigration pathways, making the lottery their only option vs. supplementary pathway?',
    'Surveys of DV applicants on employment sponsorship eligibility, family-based visa access, and other pathways; occupational and education-level analysis of DV winners vs. other visa classes',
    'If lottery is only pathway for majority: suppression is extreme (snare). If lottery supplements existing pathways for semi-privileged cohorts: suppression is moderate (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_immigration_pathway_sufficiency, empirical, 'Whether DV lottery is sole or supplementary pathway for applicants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_visa_lottery, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dvl_tr_t0, us_visa_lottery, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dvl_tr_t15, us_visa_lottery, theater_ratio, 15, 0.5).
narrative_ontology:measurement(dvl_tr_t30, us_visa_lottery, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(dvl_be_t0, us_visa_lottery, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dvl_be_t15, us_visa_lottery, base_extractiveness, 15, 0.33).
narrative_ontology:measurement(dvl_be_t30, us_visa_lottery, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_visa_lottery, resource_allocation).
narrative_ontology:affects_constraint(us_visa_lottery, employment_based_visa_backlog).
narrative_ontology:affects_constraint(us_visa_lottery, family_immigration_sponsorship_queue).

% DUAL FORMULATION NOTE:
% The DV lottery is downstream of broader US immigration policy architecture. The employment-based visa backlog constraint (with ε≈0.55, snare) is partially caused by lottery's substitution effect; the family immigration sponsorship queue (ε≈0.48, tangled rope) experiences reduced political pressure to expand due to lottery's existence. These constraints form a family linked through resource allocation substitution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_visa_lottery, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
