% ============================================================================
% CONSTRAINT STORY: digital_divide_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_divide_exclusion, []).

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
 *   constraint_id: digital_divide_exclusion
 *   human_readable: Digital Divide Exclusion from Economic and Social Participation
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The digital divide represents a structural constraint that excludes
 *   billions of people from full economic and social participation due to
 *   lack of broadband access, device affordability, and digital skills.
 *   Unlike many market failures that self-correct, the digital divide
 *   exhibits properties of a snare: it creates a locked-out population with
 *   no meaningful exit option, suppresses alternatives (analog services are
 *   being eliminated), and concentrates benefits among infrastructure owners
 *   and high-connectivity populations. The constraint is neither purely
 *   technological nor purely economic — it reflects policy choices about
 *   infrastructure investment, service regulation, and the pace of
 *   digitization of essential services. The extractiveness has increased over
 *   the interval as governments and institutions have accelerated
 *   digital-first service delivery without ensuring universal access, turning
 *   the divide from inconvenience to systemic exclusion. The theater ratio
 *   reflects the performative character of 'digital transformation' mandates
 *   that declare victory while excluding populations from the transformation.
 *
 * KEY AGENTS:
 *   - Unconnected Populations: Primary victims (powerless/trapped) — no broadband access, cannot access employment markets, educational services, government functions, financial services, healthcare coordination. Full extraction without coordination benefit.
 *   - Intermittently Connected Populations: Secondary victims (moderate/constrained) — mobile data provides patchy access at high cost; data caps and service tier segmentation create functional exclusion despite technical connectivity.
 *   - Digital Infrastructure Owners: Primary beneficiaries (institutional/arbitrage) — ISPs, telecommunications companies, equipment manufacturers benefit from service monopolies, high pricing in captive markets, and minimal infrastructure investment in low-density regions. Arbitrage options enable selective deployment.
 *   - High-Connectivity Populations: Secondary beneficiaries (powerful/mobile) — benefit from network effects, employment opportunities, educational access, social services. Their access reinforces status and opportunities.
 *   - Digital Equity Advocates: Organized agents (organized/mobile) — public sector broadband initiatives, non-profit digital literacy programs, universal access advocates. See the divide as solvable through policy but face political and budget constraints.
 *   - Legacy Institutions: Institutional actors (institutional/arbitrage) — governments, banks, utilities that have transitioned to digital-first operations without maintaining parallel analog channels. Theater ratio reflects the institutional performance of 'modernization' that actually excludes.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choices as technological inevitability or structural limitation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_divide_exclusion, 0.58).
domain_priors:suppression_score(digital_divide_exclusion, 0.68).
domain_priors:theater_ratio(digital_divide_exclusion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_divide_exclusion, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_divide_exclusion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(digital_divide_exclusion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_divide_exclusion, snare).
narrative_ontology:human_readable(digital_divide_exclusion, "Digital Divide Exclusion from Economic and Social Participation").
narrative_ontology:topic_domain(digital_divide_exclusion, "economic/social/technological").

domain_priors:requires_active_enforcement(digital_divide_exclusion).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_divide_exclusion, digital_infrastructure_owners).
narrative_ontology:constraint_beneficiary(digital_divide_exclusion, high_connectivity_populations).
narrative_ontology:constraint_victim(digital_divide_exclusion, unconnected_populations).
narrative_ontology:constraint_victim(digital_divide_exclusion, low_bandwidth_regions).
narrative_ontology:constraint_victim(digital_divide_exclusion, digital_skill_underinvestment_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNCONNECTED HOUSEHOLD (SNARE) — No meaningful access to broadband; cannot participate in employment markets, educational opportunities, or government services that assume digital access. Trapped by cost, geography, and infrastructure absence. Full extraction without coordination benefit.
constraint_indexing:constraint_classification(digital_divide_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERMITTENTLY CONNECTED (TANGLED ROPE) — Mobile phone data provides some access but at high cost relative to income; patchy coverage; data caps create effective exclusion. Genuinely benefits from digital coordination (remote work, social connection) but extraction occurs through high per-byte costs and service tier segregation.
constraint_indexing:constraint_classification(digital_divide_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INFRASTRUCTURE OPERATORS (ROPE) — Experience digital divide as a coordination mechanism: building shared infrastructure enables network effects that benefit all participants. Arbitrage options: operate profitably in connected markets, defer investment in low-density regions. Net beneficiary but coordination is genuine.
constraint_indexing:constraint_classification(digital_divide_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EQUITY ADVOCATES (SCAFFOLD) — See the divide as a temporary policy failure being solved through universal broadband initiatives, subsidized access programs, and digital literacy investments. High suppression today (political resistance, budget constraints) but declining sunset clause: infrastructure rollout, device subsidies, and skill programs target elimination within 15-20 years.
constraint_indexing:constraint_classification(digital_divide_exclusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY INSTITUTIONS (PITON) — Government agencies, banks, and utilities maintain digital-first operations and have largely abandoned analog channels. This performs institutional modernization but functionally excludes those without connectivity. Theater_ratio reflects that the theatrical performance of 'digital transformation' masks the infrastructure they inherited and did not replace. The constraint persists through inertia despite better alternatives.
constraint_indexing:constraint_classification(digital_divide_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TECH DETERMINISM (MOUNTAIN) — From civilizational scale, sees digital divide as structurally inevitable: technology adoption always generates temporary inequality, and catching-up is a natural feature of technological diffusion. Geography and infrastructure are unchangeable constraints. However, the false summit detector identifies this as naturalization: the divide is not immutable — it reflects contingent policy choices (infrastructure investment, subsidy allocation, service regulation) that could be different.
constraint_indexing:constraint_classification(digital_divide_exclusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_divide_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_divide_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_divide_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_divide_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_divide_exclusion, TR),
    TR >= 0.70.

:- end_tests(digital_divide_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The digital divide extracts through multiple mechanisms: (1) Infrastructure owners capture monopoly rents from captive populations, (2) excluded populations cannot access employment and services available to connected populations, (3) skill investment in connectivity-dependent domains is inaccessible to the excluded. The extractiveness has increased from 0.42 to 0.58 over the interval as digital-first service adoption has accelerated without ensuring universal access. This is not a pure technology adoption curve — it reflects policy choices to digitize services faster than infrastructure reaches excluded regions. Suppression (0.68): High. Multiple barriers prevent exit: (1) infrastructure costs are beyond individual household reach ($100-300/month in developed regions, prohibitive for households earning $1-3/day globally), (2) geographic barriers (rural/remote areas lack providers), (3) device costs, (4) cognitive barriers (skills, language, digital literacy), (5) service barriers (many institutions have eliminated analog access channels). The suppression is not insurmountable in principle — public investment could eliminate it — but structural suppression is high and rising. Theater ratio (0.55): Moderate. Institutions performing digital transformation declare success while excluding populations; governments announce universal broadband goals while underfunding deployment; equity advocates propose solutions while facing budget constraints. The theater is not as high as a fully performative constraint (which would show 0.7+) because genuine digital access does exist for connected populations and some real coordination occurs. But the gap between announced commitments and actual coverage represents performative theater.
 *
 * PERSPECTIVAL GAP:
 *   Original analysis shows snare classification from excluded perspective but rope from beneficiary perspective. The gap is not small — snare implies pure extraction while rope implies genuine coordination. This divergence reveals that the constraint mechanisms operate differently depending on connection status. For connected populations, digital infrastructure genuinely coordinates activity and produces network effects (rope). For excluded populations, the same infrastructure provides no coordination benefit and functions as pure exclusion (snare). The constraint is not uniform — it is a snare for some and rope for others. The perspectival gap is not a measurement problem but a structural feature: the constraint's mechanism depends on whether you have access.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Infrastructure owners (beneficiary + arbitrage) experience low d (approximately 0.10-0.15), producing negative effective extraction — they benefit from the constraint. Excluded populations (victim + trapped) experience high d (approximately 0.92-0.98), producing maximum experienced extraction chi via the sigmoid. Intermittently connected populations (both beneficiary and victim + constrained) experience moderate d (approximately 0.50-0.60), producing moderate chi. The piton perspective derives from high theater ratio rather than from high experienced extraction — institutions maintain digital-first operations as performative modernization despite alternatives. The mountain perspective is prospectively flagged as a false summit: directionality data shows the constraint is distributive (benefits concentrate upward) rather than structural (inherent to technology). This pattern — beneficiary directionality near 0.0, victim directionality near 1.0, victim d rising despite technological advancement — signals that the constraint's mechanism is political, not technological.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how mandatrophy is resolved through perspectival decomposition. The snare classification (pure extraction for excluded) is not contradicted by rope/scaffold classifications for other perspectives — they are measuring different populations' experiences of the same constraint. The analytical observer's mountain (technological inevitability) is resolved as a false summit via directionality analysis: if the divide were inherent to technology, d values would cluster around 0.50 (symmetric), but instead they polarize (high d for victims, low d for beneficiaries). The polarization indicates a distributive mechanism, not a structural/technological one. Mandatrophy is avoided by recognizing that 'is digital divide a mountain or snare?' is a false dichotomy — it is a snare for excluded populations, a rope for infrastructure owners, and potentially a scaffold if policy choices change. The constraint's type depends on the observer's position within the divide itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_investment_threshold,
    'What level of public infrastructure investment would transition digital divide from snare to rope or scaffold across different regions?',
    'Comparative analysis of broadband rollout programs (US rural, EU, India, Africa); correlation between public investment levels and adoption trajectories; cost-per-household modeling for full coverage',
    'If threshold is achievable (< 5% of GDP): divide is a policy problem solvable within current state capacity (scaffold perspective confirmed). If threshold exceeds capacity (> 10% of GDP): divide reflects structural inequality beyond near-term remedy (snare classification maintained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_investment_threshold, empirical, 'Infrastructure investment level needed to transition divide from snare to scaffold').

omega_variable(
    skill_underinvestment_causality,
    'Does digital skill deficit cause exclusion or result from it? Is the causal arrow infrastructure→skills or skills→infrastructure investment allocation?',
    'Longitudinal analysis of skill development timing relative to access; comparison of skill adoption rates when infrastructure precedes vs follows skill training; natural experiments with rapid deployment to previously excluded regions',
    'If infrastructure-first: skills respond to access (snare classification focuses on infrastructure barrier). If skills-first: access remains low because demand is not generated (snare classification reflects cognitive/aspirational barrier, not material). Different mechanisms require different interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_underinvestment_causality, empirical, 'Causality between digital skills and access barriers').

omega_variable(
    digital_necessity_threshold,
    'Has digital access crossed the threshold from optional convenience to mandatory for survival-level services (banking, government, employment, healthcare)?',
    'Audit of which critical services have eliminated non-digital access pathways; documentation of analog channel retention vs closure over time; impact assessment of closure on excluded populations',
    'If mandatory: suppression is high and rising (snare classification strengthened). If still optional: exclusion reflects disadvantage but not total deprivation (tangled rope from broader perspective). Threshold crossing represents shift in the constraint''s mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_necessity_threshold, empirical, 'Whether digital access has become mandatory for essential services').

omega_variable(
    network_effects_beneficiary_mismatch,
    'Do trapped populations experience any network benefits from others'' connectivity, or does the constraint fully prevent benefit realization?',
    'Ethnographic documentation of excluded household connection mechanisms (borrowed access, communal connectivity, family mediation); quantification of indirect benefit flows from digital economy to excluded communities',
    'If benefits flow: constraint is mixed extraction-coordination (tangled rope classification possible). If benefits do not flow: pure extraction (snare confirmed). Determines whether excluded populations are parasites on the system or completely separated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(network_effects_beneficiary_mismatch, empirical, 'Whether trapped populations receive indirect benefits from others'' connectivity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_divide_exclusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dde_tr_t0, digital_divide_exclusion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dde_tr_t5, digital_divide_exclusion, theater_ratio, 5, 0.45).
narrative_ontology:measurement(dde_tr_t10, digital_divide_exclusion, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(dde_be_t0, digital_divide_exclusion, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dde_be_t5, digital_divide_exclusion, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(dde_be_t10, digital_divide_exclusion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_divide_exclusion, global_infrastructure).
narrative_ontology:affects_constraint(digital_divide_exclusion, labor_market_access).
narrative_ontology:affects_constraint(digital_divide_exclusion, educational_credential_verification).
narrative_ontology:affects_constraint(digital_divide_exclusion, financial_services_access).
narrative_ontology:affects_constraint(digital_divide_exclusion, government_service_delivery).

% DUAL FORMULATION NOTE:
% Digital divide exclusion is downstream of infrastructure investment policy choices and service digitization decisions. Upstream constraints include broadband deployment policy, telecommunications regulation, and government digital-first mandates. The divide is a meta-constraint that emerges from the intersection of multiple sectoral digitization choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
