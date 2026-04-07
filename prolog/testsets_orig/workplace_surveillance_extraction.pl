% ============================================================================
% CONSTRAINT STORY: workplace_surveillance_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_workplace_surveillance_extraction, []).

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
 *   constraint_id: workplace_surveillance_extraction
 *   human_readable: Workplace Surveillance as Extraction and Control Mechanism
 *   domain: labor/organizational_power/technology
 *
 * SUMMARY:
 *   Workplace surveillance has evolved from task-based monitoring (measuring
 *   deliverables, attendance, task completion) into comprehensive behavioral
 *   surveillance that extends far beyond work coordination: keystroke
 *   logging, email content scanning, video monitoring, application tracking,
 *   location monitoring, mood detection via facial analysis, and predictive
 *   behavioral profiling. This constraint exhibits the full spectrum of DR
 *   classification types depending on structural position. For management and
 *   the organization, surveillance is rope (pure coordination). For hourly
 *   workers with no job alternatives, it is snare (pure extraction). For
 *   salaried workers with some mobility, it is tangled_rope (mixed
 *   coordination and extraction). For the surveillance technology industry,
 *   it is piton (degraded ritual justified by performative expansion). For
 *   organized labor and privacy advocates, it is scaffold (temporary
 *   institutional arrangement with a legislative sunset). The constraint's
 *   extractiveness has increased from 0.28 to 0.58 over the measurement
 *   interval (approximately 10 years, 2016-2026) as surveillance technology
 *   has become cheaper, more invasive, and more normalized. The theater ratio
 *   has increased from 0.35 to 0.64, indicating that the proportion of
 *   monitoring activity that serves no actual coordination function has
 *   nearly doubled — much current surveillance is maintained for
 *   psychological compliance (visible monitoring that increases behavioral
 *   conformity) rather than functional productivity measurement.
 *
 * KEY AGENTS:
 *   - Hourly and Precarious Workers: Primary victims (powerless/trapped) — economic dependency creates structural inability to refuse surveillance conditions; bear full extraction cost without coordination benefit
 *   - Salaried Knowledge Workers: Secondary victims (moderate/constrained) — face career mobility costs and negotiation asymmetry; experience mixed coordination and extraction
 *   - Management and Organization Leadership: Primary beneficiary (institutional/arbitrage) — leverage surveillance for task coordination, resource allocation, and behavioral control; perceive only coordination function
 *   - Surveillance Technology Vendors: Secondary beneficiary (institutional/arbitrage) — profit from expanding surveillance capabilities; maintain piton classification through performative feature expansion ('security', 'wellness', 'predictive analytics')
 *   - Worker Rights Advocates and Labor Organizers: Organized agent (organized/constrained) — building alternative coordination mechanisms and regulatory constraints; pursuing scaffold sunset through legislation and collective bargaining
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies tangled_rope classification: genuine coordination function layered with extraction mechanisms that serve only control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(workplace_surveillance_extraction, 0.58).
domain_priors:suppression_score(workplace_surveillance_extraction, 0.68).
domain_priors:theater_ratio(workplace_surveillance_extraction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(workplace_surveillance_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(workplace_surveillance_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(workplace_surveillance_extraction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(workplace_surveillance_extraction, tangled_rope).
narrative_ontology:human_readable(workplace_surveillance_extraction, "Workplace Surveillance as Extraction and Control Mechanism").
narrative_ontology:topic_domain(workplace_surveillance_extraction, "labor/organizational_power/technology").

domain_priors:requires_active_enforcement(workplace_surveillance_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(workplace_surveillance_extraction, management_hierarchy).
narrative_ontology:constraint_beneficiary(workplace_surveillance_extraction, surveillance_technology_vendors).
narrative_ontology:constraint_victim(workplace_surveillance_extraction, hourly_workers).
narrative_ontology:constraint_victim(workplace_surveillance_extraction, remote_workers).
narrative_ontology:constraint_victim(workplace_surveillance_extraction, worker_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOURLY WORKER (SNARE) — Trapped by economic dependency and the normalization of surveillance as employment condition. No meaningful exit: job loss means loss of healthcare, housing stability, and income. Full surveillance without consent or ability to negotiate. Maximum coercion with minimal coordination benefit — the worker experiences extraction without any genuine advantage from the constraint.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SALARIED KNOWLEDGE WORKER (TANGLED ROPE) — Constrained by career mobility costs and professional reputation risk, but possesses some exit options and some control over their surveillance footprint. Experiences both genuine coordination (productivity measurement enables resource allocation) and asymmetric extraction (invasive monitoring of non-work activity, email surveillance, activity logging). Can negotiate remote-work arrangements or job change but at career cost. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANAGEMENT/ORGANIZATION (ROPE) — Experiences surveillance as pure coordination: task assignment, performance measurement, resource allocation, and quality control. The constraint solves legitimate collective action problems (ensuring work occurs, detecting bottlenecks, preventing theft). No coercion perceived from this perspective — the mechanism is seen as neutral measurement infrastructure. Maximum benefit, minimal extraction cost.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SURVEILLANCE TECHNOLOGY VENDORS (PITON) — The industry has evolved from functional purpose (task tracking) to performative expansion (keystroke logging, video monitoring, behavior prediction, mood detection). Theater ratio high: much of the infrastructure measures activity proxies rather than actual productivity, creating a market for increasingly invasive tools justified by 'security' and 'efficiency' narratives. The primary function (coordination) has atrophied; the surveillance apparatus persists through vendor incentives and organizational inertia despite evidence that invasive monitoring decreases productivity and increases turnover.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WORKER RIGHTS ADVOCACY (SCAFFOLD) — Organized labor, privacy advocates, and regulatory bodies see surveillance constraints as temporary institutional arrangements with a sunset trajectory. GDPR, emerging US state privacy laws, and worker organizing are building alternative coordination mechanisms (algorithmic transparency, consent requirements, surveillance audits, collective bargaining on monitoring terms) that will reduce the extractive overlay while maintaining legitimate productivity coordination. Organized agents have agency and see a legislative/organizing exit path. Classified as scaffold because suppression is declining and the constraint is being actively reshaped.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, workplace surveillance coordinates legitimate functions (task assignment, quality assurance, safety compliance) while enabling asymmetric extraction (behavioral control beyond work requirements, psychological pressure through constant observation, digital panopticon effects). The constraint has a genuine coordination function but is systematically layered with extractive mechanisms that serve no coordination purpose. The classification remains tangled_rope because both dimensions are structurally present and causally relevant.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(workplace_surveillance_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(workplace_surveillance_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(workplace_surveillance_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(workplace_surveillance_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(workplace_surveillance_extraction, TR),
    TR >= 0.70.

:- end_tests(workplace_surveillance_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from workers in the form of behavioral control (compliance beyond work requirements), psychological pressure (panopticon effect), and labor intensification (monitored activity is inherently more exhausting). However, it is not at maximum (≥0.70) because management's coordination interest is partially genuine — surveillance does provide real information about task completion and resource utilization. The value reflects the asymmetry: management benefits from both coordination AND extraction, while workers bear extraction costs with minimal coordination benefit. Suppression (0.68): High. Multiple reinforcing barriers prevent exit: economic dependency (healthcare, housing, income), normalization of surveillance as employment condition, legal employment-at-will doctrine in most US jurisdictions, and information asymmetry (workers often unaware of surveillance extent). Suppression has increased as surveillance has become ubiquitous — workers can no longer vote with their feet by choosing less invasive employers. Theater ratio (0.64): Moderate-high. Significant portion of current workplace surveillance is performative: keystroke monitoring does not measure actual productivity (users learn to appear busy while accomplishing little); video monitoring creates psychological compliance without improving output quality; mood detection and predictive analytics are primarily performative security theater with minimal actual fraud/safety detection. The theater has increased as the industry has monetized invasive capability expansion rather than functional improvement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR types from power-related divergence, not observational ambiguity. The divergence is not about how to measure surveillance (it is directly observable) but about structural position's effect on experienced constraint type. A worker with identical surveillance exposure classifies the constraint as snare (pure extraction) while their manager classifies it as rope (pure coordination). Both are empirically accurate descriptions of their structural experience. The gap reveals how the constraint's function differs by position: it genuinely coordinates some management activities while genuinely extracting from worker autonomy. The tangled_rope classification at the analytical level captures this structural duality — the constraint is NOT a rope that some mistake for a snare, nor a snare that some rationalize as rope. It is authentically both, and the perspectival distribution reflects its true hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) flows from structural position. Beneficiaries (management, vendors) have low d (around 0.15-0.25 for institutional actors with arbitrage options) — surveillance subsidizes their interests. Trapped workers have maximum d (~0.95) — they bear extraction costs without exit. Constrained workers have moderate d (~0.65-0.75) — they have some exit options but at significant cost. Organized advocates have moderate d (~0.55-0.65) — they have agency through collective action and legislative influence, but the constraint still affects them structurally. The sigmoid f(d) maps these values to experienced extractiveness: trapped workers experience χ approaching maximum (1.42), constrained workers experience moderate χ (~1.00), beneficiaries experience negative χ (≈-0.01), institutional advocates experience moderate χ (~0.75). The directionality computation reveals the extraction asymmetry: the constraint runs FROM workers TO management/vendors, not bidirectionally.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED TANGLED ROPE: This constraint is tangled_rope rather than rope or snare because it exhibits THREE critical properties simultaneously: (1) Genuine coordination function — surveillance enables real task assignment, quality measurement, and resource allocation that management legitimately needs. Removing all monitoring would create actual coordination problems. (2) Asymmetric extraction — the surveillance apparatus extracts behavioral compliance, psychological pressure, and labor intensification that exceed what coordination requires. No-monitoring alternatives (transparent task outcomes, worker autonomy in scheduling, outcome-based metrics) could achieve coordination without the extraction component. (3) Requires active enforcement — the constraint persists because management actively enforces it and the technology industry actively expands it. It is not naturally arising; it is actively maintained. Mandatrophy is resolved by distinguishing the coordination component (legitimate) from the extraction component (contingent institutional choice) in the commentary, establishing that both are structurally present and causally relevant, not that one is mislabeled as the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_measurement_validity,
    'Do invasive surveillance mechanisms (keystroke logging, video monitoring, application tracking) actually improve productivity measurement or do they merely create an illusion of control through behavioral compliance?',
    'Comparison of productivity outcomes in organizations with varying surveillance intensity; meta-analysis of controlled studies on monitoring and performance; measurement of ''presenteeism'' vs actual output quality',
    'If surveillance improves measurement: constraint is rope (coordination mechanism). If surveillance is performative: constraint is piton (degraded ritual). If surveillance decreases productivity: constraint is snare (pure extraction mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productivity_measurement_validity, empirical, 'Whether invasive surveillance improves or degrades actual productivity').

omega_variable(
    consent_collapse_boundary,
    'At what level of surveillance pervasiveness does worker consent become structurally impossible (trapped transition point)?',
    'Analysis of labor market elasticity: proportion of workers with realistic exit options as surveillance intensity increases; longitudinal tracking of job changes correlated with surveillance implementation; worker survey on experienced coercion thresholds',
    'If collapse point is near current levels: majority of workers are trapped, classification shifts uniformly to snare. If collapse point is far higher: most workers remain constrained (not trapped), classification depends on worker type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_collapse_boundary, empirical, 'Threshold where worker consent becomes structurally impossible').

omega_variable(
    technology_neutral_substitutes,
    'Can coordination functions (task assignment, quality measurement, resource allocation) be achieved through non-surveillance means (transparency, worker autonomy, outcome-based metrics) without recreating the extraction mechanism?',
    'Case studies of organizations using alternative coordination: outcome-based management, worker-directed scheduling, transparent algorithms; comparison of extractiveness and worker autonomy outcomes',
    'If substitutes work: surveillance is contingent institutional choice, not necessary coordination mechanism — supports scaffold sunset. If substitutes fail: surveillance may be inevitable coordination cost — supports rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutral_substitutes, empirical, 'Whether non-surveillance coordination mechanisms can replace current surveillance infrastructure').

omega_variable(
    regulatory_capture_in_privacy_law,
    'Will emerging privacy regulations (GDPR, state laws, worker protections) create genuine constraints on surveillance extraction or will they become captured by corporate influence, resulting in surveillance-washing (performative compliance)?',
    'Analysis of regulatory enforcement: number of enforcement actions, fines relative to corporate gains, regulatory agency resource levels; tracking of regulatory gaps created by corporate lobbying; measurement of actual behavioral change in organizations post-regulation',
    'If regulations are enforced: scaffold sunset is real and constraint will transition to rope. If regulations are captured: constraint remains tangled_rope or snare with theatrical compliance layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_in_privacy_law, empirical, 'Whether privacy regulations will constrain surveillance or be captured').

omega_variable(
    psychological_suppression_internalization,
    'Is the suppression experienced by workers primarily structural (economic barriers to exit, legal employment terms) or internalized (workers accept constant monitoring as normal, necessary, deserved)?',
    'Worker surveys on perceived agency and consent; analysis of post-exit psychological adjustment (do workers continue self-monitoring after employment ends); comparison of suppression levels across cultural contexts with different surveillance norms',
    'If structural: removing barriers (job alternatives, legal restrictions) reduces suppression immediately. If internalized: suppression persists after exit; requires cognitive reframing; constraint''s true suppression is higher than structural measures suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_suppression_internalization, empirical, 'Whether suppression is structural or internalized in worker psychology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(workplace_surveillance_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wksurveil_tr_t0, workplace_surveillance_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wksurveil_tr_t5, workplace_surveillance_extraction, theater_ratio, 5, 0.52).
narrative_ontology:measurement(wksurveil_tr_t10, workplace_surveillance_extraction, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(wksurveil_be_t0, workplace_surveillance_extraction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wksurveil_be_t5, workplace_surveillance_extraction, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(wksurveil_be_t10, workplace_surveillance_extraction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(workplace_surveillance_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(workplace_surveillance_extraction, 0.18).
narrative_ontology:affects_constraint(workplace_surveillance_extraction, labor_market_power_asymmetry).
narrative_ontology:affects_constraint(workplace_surveillance_extraction, digital_panopticon_normalization).
narrative_ontology:affects_constraint(workplace_surveillance_extraction, workplace_autonomy_degradation).

% DUAL FORMULATION NOTE:
% Workplace surveillance operates as a unified constraint but its effects decompose into distinct downstream constraints: labor market power (surveillance increases dependency on current employer, reducing exit capacity), normalization (constant monitoring changes expectations about privacy and consent), and autonomy degradation (monitored work becomes inherently more controlling). These downstream constraints share the upstream surveillance mechanism but have independent extractiveness signatures reflecting different agent populations and time horizons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(workplace_surveillance_extraction, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
