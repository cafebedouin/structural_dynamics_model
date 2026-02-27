% ============================================================================
% CONSTRAINT STORY: s1_airbnb
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_s1_airbnb, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: s1_airbnb
 *   human_readable: Short-Term Rental (STR) Regulatory Framework
 *   domain: economic/political
 *
 * SUMMARY:
 *   The STR regulatory framework is a global patchwork of municipal laws,
 *   zoning restrictions, licensing requirements, and tax obligations that
 *   govern platforms like Airbnb and Vrbo. It represents a structural tension
 *   between property rights (individual hosts' ability to monetize spare
 *   capacity), urban housing stability (long-term rental market viability),
 *   platform coordination (standardized rules enabling global operation), and
 *   municipal fiscal interests (tax revenue and tourism economy). The
 *   constraint exhibits all six DR types from different perspectives, making
 *   it a diagnostic case for how institutional power asymmetries shape
 *   classification. From the perspective of long-term renters facing housing
 *   scarcity, STR regulation is insufficient extraction mitigation (Snare).
 *   From municipal governments trying to collect taxes and preserve
 *   neighborhoods, it is a mixed coordination-extraction challenge (Tangled
 *   Rope). From platform operators and compliant hosts, it is a manageable
 *   coordination overhead (Rope). From housing advocates pushing for
 *   permanent affordability solutions, it is a temporary support mechanism
 *   (Scaffold). From traditional lodging industry using regulation to protect
 *   market share without enforcing it, it is degraded institutional theater
 *   (Piton). The analytical observer risks naturalizing a contingent policy
 *   arrangement as inevitable (false Mountain). The constraint's
 *   extractiveness has risen from 0.15 (2014-2016, early permissive era) to
 *   0.52 (current) as platforms consolidated power and regulatory enforcement
 *   lagged behind adoption.
 *
 * KEY AGENTS:
 *   - Long-term renters and housing-insecure populations: Primary victims (powerless/trapped) — displaced by landlord conversion to STR, no alternative local housing access
 *   - Individual host-entrepreneurs: Primary beneficiaries (powerful/arbitrage) — earn income from spare capacity; benefit from platform coordination; experience regulation as low-cost overhead
 *   - Platform operators (Airbnb/Vrbo/Booking): Secondary beneficiaries (institutional/arbitrage) — extract network value, coordinate globally, arbitrage across jurisdictions, lobby for weak enforcement
 *   - Municipal housing authorities and city planners: Mixed actor (moderate/constrained) — constrained by limited enforcement budgets and platform lobbying; benefit from tourism tax revenue; harmed by tax evasion and neighborhood degradation
 *   - Housing advocacy organizations: Organized collective (organized/constrained) — build alternative housing supply while advocating for STR caps with sunset logic
 *   - Traditional hotel and lodging industry: Institutional actor using regulation performatively (institutional/arbitrage) — lobbies for STR restrictions but doesn't rely on enforcement; maintains market position through brand and service quality
 *   - Analytical observer: Risks naturalizing policy as law (analytical/analytical) — temptation to see housing scarcity as inevitable rather than as a result of STR dynamics and regulatory capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(s1_airbnb, 0.52).
domain_priors:suppression_score(s1_airbnb, 0.65).
domain_priors:theater_ratio(s1_airbnb, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(s1_airbnb, extractiveness, 0.52).
narrative_ontology:constraint_metric(s1_airbnb, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(s1_airbnb, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(s1_airbnb, tangled_rope).
narrative_ontology:human_readable(s1_airbnb, "Short-Term Rental (STR) Regulatory Framework").
narrative_ontology:topic_domain(s1_airbnb, "economic/political").

domain_priors:requires_active_enforcement(s1_airbnb).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(s1_airbnb, platform_operators).
narrative_ontology:constraint_beneficiary(s1_airbnb, individual_hosts).
narrative_ontology:constraint_beneficiary(s1_airbnb, tourist_economy).
narrative_ontology:constraint_victim(s1_airbnb, long_term_rental_market).
narrative_ontology:constraint_victim(s1_airbnb, residential_neighborhood_stability).
narrative_ontology:constraint_victim(s1_airbnb, municipal_housing_authorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LONG-TERM RENTERS (SNARE) — Trapped by shrinking affordable rental stock as landlords convert units to STR. No exit from local housing market without displacement. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.57.
constraint_indexing:constraint_classification(s1_airbnb, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL AUTHORITIES (TANGLED ROPE) — Constrained by enforcement costs, tax collection gaps, and political pressure from hospitality/tourism lobbies. Also benefit from tax revenue and tourism stimulus. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.49.
constraint_indexing:constraint_classification(s1_airbnb, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Benefit from regulatory arbitrage: comply with different jurisdictions, extract coordination value. Experience STR regulation as coordination overhead (standardize listings, payment processing, liability). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(s1_airbnb, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIVIDUAL HOSTS (ROPE) — Benefit from platform coordination (access to demand, payment processing, liability insurance). Experience regulation as a compliance overhead, but the coordination value exceeds compliance cost. d≈0.25, f(d)≈0.15, σ=0.9 → χ≈0.07. Low extraction; net beneficiary.
constraint_indexing:constraint_classification(s1_airbnb, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HOUSING ADVOCATES (SCAFFOLD) — Organized coalition (tenants unions, affordable housing nonprofits, community boards) pushing for restrictions with sunset logic: temporary STR caps while transit-oriented affordable housing is built. See the regulation as temporary support for long-term housing security. d≈0.55, f(d)≈0.74, σ=1.0 → χ≈0.38. Moderate extraction that declines as alternative housing supply grows.
constraint_indexing:constraint_classification(s1_airbnb, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL LODGING (PITON) — Uses STR regulation as performative protection. Lobbies for restrictions but maintains market share through brand loyalty despite regulations' weak enforcement. theater_ratio=0.58 (regulations exist but enforcement gaps are notorious). d≈0.35, f(d)≈0.33, σ=1.1 → χ≈0.21. Degraded institutional protection maintained by lobbying theater, not real market enforcement.
constraint_indexing:constraint_classification(s1_airbnb, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN CLAIM) — Observer might naturalize STR regulation as inevitable tension between housing access and property rights, as if it were a law of nature. But ε=0.52, suppression=0.65 contradicts the mountain gate (ε ≤ 0.25). This is a contingent institutional arrangement, not a natural law. Engine flags as false summit.
constraint_indexing:constraint_classification(s1_airbnb, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(s1_airbnb_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(s1_airbnb, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(s1_airbnb, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(s1_airbnb, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(s1_airbnb, TR),
    TR >= 0.70.

:- end_tests(s1_airbnb_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint extracts value from long-term renters by reducing housing supply and increasing rents (direct extraction). Platforms extract coordination value from hosts and travelers. The measurement trajectory (0.15 → 0.52 over 10 years) reflects the shift from permissive early regulation to enforcement attempts, but enforcement remains weak (theater_ratio 0.58). The rising extractiveness indicates that platforms have consolidated market power faster than regulatory capacity has grown. Suppression (0.65): High. Long-term renters face high barriers to exit (housing scarcity, relocation costs). Hosts face regulatory uncertainty and compliance costs. Cities face lobbying pressure and enforcement resource constraints. But suppression is not total — platforms operate openly, hosts can delist, and some cities enforce aggressively. Theater ratio (0.58): Moderate-high. Many STR regulations exist on paper but are weakly enforced. Licenses are easy to obtain; fines are rare; platforms share data inconsistently with cities. Cities produce regulatory theater (licensing requirements, inspection regimes) that creates compliance perception without effective scarcity reduction. The theater has risen over the interval as regulations accumulated without enforcement scaling.
 *
 * PERSPECTIVAL GAP:
 *   Long-term renters see extraction (Snare): the constraint reduces housing supply available to them, raising rents, with no exit option. Individual hosts see coordination (Rope): they benefit from platform tools and regulation is a compliance cost they absorb. Platforms see coordination (Rope): regulation standardizes the ecosystem and justifies their governance role. Municipal authorities see mixed extraction and coordination (Tangled Rope): they collect taxes and boost tourism, but lose rental supply and face enforcement costs. Housing advocates see a temporary coordination support (Scaffold): STR caps preserve units while new affordable housing is built. Traditional lodging sees degraded institutional protection (Piton): regulations exist but enforcement is weak, so they maintain market share through other means. The analytical observer risks seeing natural law (false Mountain): housing scarcity appears inevitable, but it is the outcome of specific institutional choices about STR supply and regulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term renters: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; cannot exit local housing market. Individual hosts: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit platform but choose not to. Platforms: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; operate in multiple jurisdictions, can shift focus. Municipal authorities: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction (tax evasion, enforcement costs, housing loss); constrained by lobbying and resource limits. Housing advocates: Organized + constrained → d≈0.55, f(d)≈0.74. Moderate extraction (temporary measure only); organized to reduce it through sunset-based policy. Traditional lodging: Institutional + arbitrage → d≈0.35, f(d)≈0.33. Low extraction (piton classification from theater gate); can arbitrage between STR regulation and their own market position. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Risks naturalizing contingent policy as inevitable law.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy tension: Is STR regulation a coordination mechanism (Rope) solving collective action problems in housing markets, or an extraction mechanism (Snare) where platforms and hosts capture value from renters? The framework resolves this by perspectival decomposition. For platforms and compliant hosts, regulation IS coordination — it standardizes rules and enables trust. For long-term renters, regulation FAILS as coordination because it does not prevent supply loss. For cities, regulation is HYBRID — it coordinates tourism and tax collection while extracting from the long-term rental market. The mandatrophy is resolved by showing that no single classification fits all perspectives simultaneously. The regulation is Rope from the host perspective, Snare from the renter perspective, Tangled Rope from the municipal perspective. The 'true' classification is the presheaf over the observation site, not a single type. This prevents conflating two different claims: (1) Does STR regulation coordinate hosts and platforms? (Yes, Rope.) (2) Does STR regulation preserve housing affordability? (No, Snare for renters.) The mandatrophy forbids answering both as the same classification, forcing precision about which structural question is being asked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_vs_compliance_gap,
    'What fraction of STR units actually comply with municipal regulations, and does this compliance rate materially affect the extraction dynamics?',
    'Audit of licensed vs unlicensed listings in major cities; correlation between enforcement capacity and compliance rates; shadow economy modeling',
    'If compliance > 70%: regulation is binding constraint (high suppression justified). If compliance < 40%: regulation is performative theater (theater_ratio rises, classification shifts toward Piton from municipal perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_vs_compliance_gap, empirical, 'Actual compliance rate vs regulatory requirement gap').

omega_variable(
    long_term_conversion_causality,
    'To what extent does STR regulation prevent net conversion of units from long-term to short-term, versus merely delaying conversion or shifting it geographically?',
    'Natural experiment analysis (jurisdictions with/without STR caps); longitudinal tracking of unit conversion rates; econometric isolation of STR causality from other housing supply factors',
    'If STR regulation reduces permanent conversion: regulation benefits long-term market (beneficiary gate confirmed). If landlords convert anyway or shift to unregulated areas: regulation is performative (theater_ratio rises, victim perspective shifts from Snare to Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_conversion_causality, empirical, 'Whether STR regulation causally prevents long-term unit loss').

omega_variable(
    regulation_benefit_incidence,
    'Who actually benefits from STR regulation: existing long-term renters, or new market entrants/affordable housing programs?',
    'Rent trajectory analysis in regulated vs unregulated markets; demographic tracking of who benefits from preserved affordable stock; affordability index correlation',
    'If benefit accrues to existing renters only: regulation is wealth transfer (extraction from future tenants). If benefit funds new affordable units: regulation is true coordination. Affects whether victims perspective sees extraction or coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulation_benefit_incidence, empirical, 'Incidence of regulatory benefits across tenant populations').

omega_variable(
    platform_regulatory_capture,
    'To what degree have platform operators (Airbnb/Vrbo) successfully captured municipal regulatory processes, shaping weak enforcement?',
    'Analysis of campaign contributions, lobbying expenditure, and timing of weak permit regimes; comparison of cities with strong platform lobbying vs independent housing advocacy',
    'If platform capture is significant: classification shifts for municipal perspective from Tangled Rope toward Snare (platforms extracting from city tax base). If capture is minimal: municipal authority perspective holds (genuine coordination-extraction hybrid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_regulatory_capture, empirical, 'Degree of platform regulatory capture in municipal rule-setting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(s1_airbnb, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(str_tr_t0, s1_airbnb, theater_ratio, 0, 0.35).
narrative_ontology:measurement(str_tr_t5, s1_airbnb, theater_ratio, 5, 0.48).
narrative_ontology:measurement(str_tr_t10, s1_airbnb, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(str_be_t0, s1_airbnb, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(str_be_t5, s1_airbnb, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(str_be_t10, s1_airbnb, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(s1_airbnb, resource_allocation).
narrative_ontology:boltzmann_floor_override(s1_airbnb, 0.25).
narrative_ontology:affects_constraint(s1_airbnb, housing_affordability_crisis).
narrative_ontology:affects_constraint(s1_airbnb, platform_regulatory_capture).
narrative_ontology:affects_constraint(s1_airbnb, municipal_tax_evasion).

% DUAL FORMULATION NOTE:
% STR regulation is part of a constraint family decomposed by perspective. The upstream constraint is housing_affordability_crisis (ε=0.68, Snare from renter perspective) — the structural mismatch between housing supply and demand. STR regulation is a middle-layer response (ε=0.52, Tangled Rope from municipal perspective) that partially addresses but does not solve the affordability crisis. Downstream is platform_regulatory_capture (ε=0.71, Snare from public interest perspective) — platforms' successful lobbying to weaken STR enforcement. These constraints are linked because each upstream constraint drives the configuration of the downstream one: housing scarcity incentivizes STR conversion, which platforms profit from and lobby to protect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(s1_airbnb, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
