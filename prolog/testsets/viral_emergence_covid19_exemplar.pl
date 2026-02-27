% ============================================================================
% CONSTRAINT STORY: viral_emergence_covid19_exemplar
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_viral_emergence_covid19_exemplar, []).

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
 *   constraint_id: viral_emergence_covid19_exemplar
 *   human_readable: Societal Response to SARS-CoV-2 Emergence
 *   domain: social/political/health
 *
 * SUMMARY:
 *   The emergence of SARS-CoV-2 in late 2019 triggered an unprecedented
 *   coordinated global response combining public health authority, economic
 *   intervention, surveillance expansion, and supply chain reorganization.
 *   This constraint story models the evolving 'societal response' as a
 *   structural phenomenon that exhibits multiple classification types
 *   depending on the observer's structural position. From the perspective of
 *   non-essential workers, the response appears as a snare: mandatory income
 *   loss without exit, high suppression, and asymmetric bearing of costs.
 *   From the perspective of public health authorities, the same response
 *   appears as rope: a coordination mechanism solving the legitimate
 *   collective action problem of preventing healthcare system collapse. From
 *   the analytical global perspective, it appears as tangled rope: genuine
 *   coordination benefit paired with severe asymmetric extraction benefiting
 *   wealthy nations and harming low-income populations. The constraint's
 *   extractiveness increased over time (0.25 → 0.52) as initial emergency
 *   measures calcified into persistent mandates, and theater_ratio increased
 *   (0.35 → 0.61) as performative measures (asymptomatic testing,
 *   certification theater, vaccination theater) accumulated despite declining
 *   functional benefit. The constraint demonstrates how identical policies
 *   can be coordinating mechanisms or extraction schemes depending entirely
 *   on the observer's structural relationship to enforcement and benefit.
 *
 * KEY AGENTS:
 *   - Public Health Authorities: Institutional beneficiary (institutional/arbitrage) — primary architects of response, design authority, access to best available information
 *   - Non-Essential Workers: Primary victim (powerless/trapped) — mandatory income loss, no exit, bear full cost of institutional coordination choices
 *   - Small Business Operators: Secondary victim (moderate/constrained) — constrained by closures but some access to support programs; disproportionate burden relative to large corporations
 *   - Pharmaceutical Manufacturers: Institutional beneficiary (institutional/arbitrage) — benefit from regulatory acceleration, intellectual property protection, guaranteed procurement
 *   - Essential Sector Employers: Beneficiary (institutional/arbitrage) — maintain operations while competitors close; access to preferred labor pools; reduced competition
 *   - Education System & Students: Mixed victim/agent (organized/constrained) — benefit from legitimate infection prevention coordination but bear severe learning loss and mental health costs
 *   - Global Poor & Vaccine-Excluded Populations: Victim (powerless/trapped) — locked out of vaccine access, locked down without support, suffer both disease and economic collapse
 *   - Testing & Surveillance Infrastructure: Institutional maintainer (institutional/arbitrage) — benefits from permanent expansion; theater ratio increases as core function atrophies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(viral_emergence_covid19_exemplar, 0.52).
domain_priors:suppression_score(viral_emergence_covid19_exemplar, 0.68).
domain_priors:theater_ratio(viral_emergence_covid19_exemplar, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, extractiveness, 0.52).
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(viral_emergence_covid19_exemplar, tangled_rope).
narrative_ontology:human_readable(viral_emergence_covid19_exemplar, "Societal Response to SARS-CoV-2 Emergence").
narrative_ontology:topic_domain(viral_emergence_covid19_exemplar, "social/political/health").

domain_priors:requires_active_enforcement(viral_emergence_covid19_exemplar).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(viral_emergence_covid19_exemplar, public_health_authorities).
narrative_ontology:constraint_beneficiary(viral_emergence_covid19_exemplar, essential_sector_employers).
narrative_ontology:constraint_beneficiary(viral_emergence_covid19_exemplar, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, non_essential_workers).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, small_business_operators).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, students_educational_access).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, mental_health_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ESSENTIAL WORKER (SNARE) — Trapped without exit. Loses income through mandated closure while bearing full health and economic risk. No agency in constraint design or timing. Suppression is extreme: cannot work, cannot access unemployment benefits immediately, cannot contest the mandate without risking social stigma. Extraction runs unidirectionally toward the institutional apparatus — the worker's costs subsidize public health coordination efforts.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GLOBAL POOR & VACCINE-EXCLUDED (SNARE) — Trapped in regions with vaccine access restrictions and manufacturing constraints. Bear health burden and lockdown costs without vaccine access. No exit option: cannot move to vaccine-rich regions, cannot obtain doses through normal channels. Maximum experienced extraction — abstract global coordination mechanism (vaccine equity) subordinated to national interest.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALL BUSINESS OPERATORS (TANGLED ROPE) — Constrained by lockdown mandates but also benefit from some coordinated support (PPP loans, moratoriums). Coordination function: businesses are part of integrated supply chains; collective closure prevents health system collapse. But extraction is asymmetric: large corporations receive more support per employee; small businesses face disproportionate closure risk. Agency is limited but not zero — some negotiation over essential designations, some adaptation capacity.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITIES (ROPE) — Primary beneficiary with institutional power and arbitrage options. Constraint functions as coordination mechanism: centralized decision-making prevents healthcare system collapse and enables resource allocation. Extraction is minimal from this perspective because authorities are solving the genuine collective action problem. Can exit through deference to medical advice; can arbitrage by claiming scientific authority. Experiences the constraint as legitimate coordination.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PHARMACEUTICAL MANUFACTURERS (ROPE) — Institutional beneficiary. Benefits from vaccine development urgency, regulatory fast-tracking (Operation Warp Speed), intellectual property protection, and guaranteed government procurement. Experiences constraint as coordination mechanism enabling vaccine development at scale. Has arbitrage options: can choose which vaccines to produce, which markets to serve. Extraction is minimal from this perspective — the constraint aligns manufacturer incentives with public health objectives.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EDUCATION SYSTEM & STUDENTS (TANGLED ROPE) — Organized institutional agent constrained by closure mandates. Genuine coordination function: preventing school transmission protects vulnerable populations and reduces healthcare demand. But extraction is severe: student learning loss, teacher burnout, digital divide deepens educational inequality. Shutdown theaters (temperature checks, hybrid learning) provide low functional benefit. Constrained exit: schools cannot fully reopen without risking transmission surges; cannot stay closed indefinitely without harming development. Mixed experience — real coordination benefit paired with significant imposed costs.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: TESTING & SURVEILLANCE INFRASTRUCTURE (PITON) — Institutional actor. Theater ratio is high (0.61): rapid antigen tests have low sensitivity; PCR tests are delayed; surveillance systems produce data without clear action protocols; testing theater (frequent testing of asymptomatic individuals) persists after effectiveness plateaued. The constraint maintains itself through inertia — testing capacity built during emergency persists long after utility declines. Primary function has atrophied (early pandemic: essential for case finding; mid-pandemic: ritualized compliance signaling) but infrastructure persists.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global scope, the constraint is a mixed coordination-extraction hybrid. Coordination function: pandemic requires centralized information sharing, resource allocation, and supply chain management. Genuine public health good. But asymmetric extraction: benefits concentrate in wealthy nations (vaccine access, economic support); costs distribute to low-income populations (disease burden, economic collapse). Theater is significant: global coordination theater (WHO declarations, vaccine diplomacy) masks resource hoarding and inequality. Effective extraction chi remains moderate to high even after accounting for genuine coordination benefit.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viral_emergence_covid19_exemplar_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(viral_emergence_covid19_exemplar, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(viral_emergence_covid19_exemplar, TR),
    TR >= 0.70.

:- end_tests(viral_emergence_covid19_exemplar_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint extracts significant resources from non-essential workers and small businesses to fund public health coordination and economic support for essential sectors. The extraction is asymmetric — non-essential workers and small businesses bear disproportionate costs; essential employers and pharmaceutical manufacturers realize disproportionate benefits. The trajectory from 0.25 (early emergency: genuine coordination) to 0.52 (late pandemic: calcified extraction) reflects the transformation from emergency response to institutional routine. Suppression (0.68): High. Multiple suppression mechanisms: (1) legal mandates with criminal penalties for violations, (2) social suppression (ostracism of dissidents), (3) occupational suppression (employment termination for non-compliance), (4) informational suppression (suppression of alternative medical voices and cost-benefit analyses). Suppression increases over time as initial voluntary cooperation gives way to enforced compliance. Theater ratio (0.61): Moderate-high. Performance components include: rapid antigen testing (low sensitivity, high theater), asymptomatic testing (declining marginal benefit), vaccination theater (frequent boosters despite declining effectiveness), certification systems (produce compliance signaling without health benefit), contact tracing (low implementation effectiveness masked by visible effort). Theater increased over the interval as genuine emergency measures (intensive care coordination, triage protocols) were replaced by ritualized compliance signaling.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full perspectival gap across all six types. Public health authorities experience rope (pure coordination) — solving collective action problems, legitimate authority. Non-essential workers experience snare (pure extraction) — mandatory cost-bearing without benefit or exit. Pharmaceutical manufacturers experience rope (pure coordination) — vaccine development aligns private and public interest. Global poor experience snare (pure extraction) — lockdown and vaccine exclusion without support. Small businesses and education systems experience tangled rope (mixed coordination and extraction) — genuine pandemic response coordination paired with asymmetric cost distribution. Surveillance infrastructure experiences piton (degraded inertia) — theater persists after core function atrophies. The gaps are not perspectival differences in interpretation but genuine structural differences in how the constraint functions for different agents. Public health authorities genuinely solve coordination problems; non-essential workers genuinely bear extraction costs. Both perspectives are structurally accurate for their respective agents. The perspectival gap reveals the constraint as an asymmetric institutional arrangement subordinating individual costs to collective coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from beneficiary/victim status combined with exit options. Non-essential workers: victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). Public health authorities: beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 (negative/beneficial extraction from their perspective). Small business operators: victim status + constrained exit (can adapt, relocate, or transition but with high friction) → d ≈ 0.65 → f(d) ≈ 1.00 (moderate extraction). Essential employers: beneficiary status + arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01 (minimal extraction). These d values drive the classification differences: powerless trapped victims experience snare (high chi), institutional arbitrage beneficiaries experience rope (negative chi), moderate constrained actors experience tangled rope (moderate chi). The directionality chain is the mechanism by which the same constraint produces different classifications for different observers.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that legitimate coordination coexists with significant extraction. The temptation is to classify the entire response as either (a) pure coordination (rope) — pandemic legitimately requires centralized authority — or (b) pure extraction (snare) — the response is an authoritarian power grab. The reality is tangled: the constraint solves genuine collective action problems (preventing healthcare collapse, coordinating vaccine distribution) AND imposes severe extraction on those with no exit options (mandated income loss, occupational suppression). The mandatrophy is resolved by accepting that both descriptions are structurally accurate. The response is rope from the beneficiary/authority perspective (genuine coordination), snare from the trapped victim perspective (pure extraction), and tangled rope from the analytical perspective (mixed coordination and extraction). The classification type is determined by the observer's structural relationship, not by a single objective property. The analytical observer's tangled rope classification is the most complete because it acknowledges both the genuine coordination function and the asymmetric extraction. False mandatrophy resolution would claim 'it's really rope' (ignoring victim extraction) or 'it's really snare' (ignoring genuine coordination benefit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_virus_severity,
    'What was the true baseline severity of SARS-CoV-2 in naive populations without pre-existing cross-reactive immunity or pre-vaccination?',
    'Serological studies of early outbreaks; age-stratified infection fatality rate estimates from random sampling; comparison to endemic coronavirus burden',
    'If IFR < 0.2%: response severity appears excessive relative to threat (extraction emphasis increases). If IFR > 0.5%: response severity aligns with genuine pandemic threat (coordination emphasis increases). Classification could shift from Tangled Rope toward Rope if threat was severe, or toward Snare if threat was moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_virus_severity, empirical, 'Baseline infection fatality rate of SARS-CoV-2 in naive populations').

omega_variable(
    vaccine_efficacy_against_transmission,
    'Did vaccines substantially reduce transmission, or primarily protect individual recipients from severe disease?',
    'Controlled studies comparing vaccinated/unvaccinated transmission rates in controlled settings; analysis of vaccination impact on population-level case curves accounting for variant emergence',
    'If vaccine-induced sterilizing immunity: mandates are coordination mechanism (Rope/Tangled Rope with lower extraction). If vaccine primarily prevents severe disease: mandates are extraction mechanism subordinating individual autonomy to collective protection (Snare/high-extraction Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vaccine_efficacy_against_transmission, empirical, 'Whether vaccines reduce transmission or primarily prevent severe disease').

omega_variable(
    lockdown_counterfactual_health,
    'What would health outcomes (mortality, disease incidence) have been absent lockdowns, given the actual behavioral response to perceived threat?',
    'Comparison to jurisdictions with minimal lockdowns (Sweden, Japan, Georgia); modeling of voluntary behavior change absent mandates; meta-analysis of lockdown effectiveness across heterogeneous populations',
    'If counterfactual health outcomes similar: lockdowns were extraction mechanism with minimal coordination benefit (Snare). If counterfactual health outcomes substantially worse: lockdowns were necessary coordination mechanism (Rope/Tangled Rope). Shifts evidence-base for classification type across all perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lockdown_counterfactual_health, empirical, 'Health outcomes under counterfactual scenario without lockdowns').

omega_variable(
    costs_lockdown_containment,
    'What were the full accounting of costs imposed by lockdowns — mortality from delayed care, educational loss, mental health burden, economic loss — relative to disease-specific mortality prevented?',
    'Comprehensive cost-benefit analysis including disability-adjusted life years (DALYs) from lockdown-induced harms; longitudinal health tracking of closure cohorts; economic productivity analysis',
    'If lockdown costs > prevented disease costs: constraint appears as extraction (Snare). If lockdown costs < prevented disease costs: constraint appears as justified coordination (Rope/Tangled Rope). This omega determines whether victims'' snare perspective or beneficiaries'' rope perspective has empirical priority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(costs_lockdown_containment, empirical, 'Full cost-benefit accounting of lockdown harms versus prevented disease mortality').

omega_variable(
    surveillance_mission_creep,
    'Did surveillance infrastructure built for pandemic response persist and expand beyond health applications?',
    'Documentation of surveillance capabilities retained post-emergency; analysis of mission creep precedents in other crises; legal challenges to surveillance authority retention',
    'If substantial mission creep: theater_ratio was understated, piton classification confirmed (institutional inertia masquerading as health infrastructure). If surveillance dismantled: theater_ratio was correct, temporary emergency function (Scaffold). Affects piton vs scaffold classification for surveillance actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_mission_creep, empirical, 'Persistence and expansion of surveillance capabilities beyond emergency response').

omega_variable(
    institutional_capture_dynamics,
    'To what extent did public health authorities become captured by pharmaceutical manufacturers, hospital systems, or political interests — suppressing dissent and alternative approaches?',
    'Analysis of regulatory decisions favoring particular manufacturers; documentation of dissent suppression; comparison to pre-pandemic institutional norms; examination of revolving-door employment',
    'If substantial capture: beneficiary actors (authorities + manufacturers) extracted more value than coordination function justified (extraction emphasis increases). If minimal capture: beneficiary authority decisions were motivated by genuine health coordination (coordination emphasis increases). Affects directionality for institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_dynamics, conceptual, 'Degree of institutional capture by industry and political interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viral_emergence_covid19_exemplar, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(covid_response_theater_0, viral_emergence_covid19_exemplar, theater_ratio, 0, 0.35).
narrative_ontology:measurement(covid_response_theater_6, viral_emergence_covid19_exemplar, theater_ratio, 6, 0.58).
narrative_ontology:measurement(covid_response_theater_24, viral_emergence_covid19_exemplar, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(covid_response_extract_0, viral_emergence_covid19_exemplar, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(covid_response_extract_6, viral_emergence_covid19_exemplar, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(covid_response_extract_24, viral_emergence_covid19_exemplar, base_extractiveness, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viral_emergence_covid19_exemplar, enforcement_mechanism).
narrative_ontology:affects_constraint(viral_emergence_covid19_exemplar, supply_chain_fragility).
narrative_ontology:affects_constraint(viral_emergence_covid19_exemplar, institutional_trust_erosion).
narrative_ontology:affects_constraint(viral_emergence_covid19_exemplar, surveillance_authority_expansion).

% DUAL FORMULATION NOTE:
% The 'societal response to viral emergence' decomposes into three constraint families: (1) Disease-specific constraints (transmission dynamics, vaccine efficacy) — these are mountain-to-rope depending on empirical severity and intervention effectiveness. (2) Institutional response constraints (lockdown mandates, vaccine mandates) — these are tangled rope to snare depending on exit options and cost distribution. (3) Structural consequence constraints (supply chain disruption, trust erosion, surveillance expansion) — these are pitons with high theater. This story focuses on constraint family (2) — the institutional response apparatus — and its downstream effects on families (1) and (3).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
