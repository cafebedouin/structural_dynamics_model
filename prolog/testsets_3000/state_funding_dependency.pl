% ============================================================================
% CONSTRAINT STORY: state_funding_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_funding_dependency, []).

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
 *   constraint_id: state_funding_dependency
 *   human_readable: State Funding Dependency in Institutional Governance
 *   domain: political_economy/institutional_capture
 *
 * SUMMARY:
 *   State funding dependency creates a structural extraction mechanism
 *   embedded in the governance of public institutions. Organizations that
 *   depend on state funding for operational survival lose autonomy over
 *   mission, priorities, and resource allocation. The constraint exhibits
 *   hybrid characteristics: legitimate coordination (the state uses funding
 *   to align institutions toward public priorities, stable baseline funding
 *   enables long-term planning) coexists with pure extraction (budget
 *   gatekeeping, compliance overhead, implicit policy conditioning). The
 *   measurement trajectory shows increasing extractiveness and theater ratio
 *   over 30 years, reflecting the shift from post-WWII public investment
 *   models toward neoliberal performance metrics and privatization-threat
 *   conditioning. The constraint appears differently to trapped institutions
 *   (pure snare), moderately autonomous ones (tangled rope), state budget
 *   authorities (pure rope coordination), elite institutions with alternative
 *   funding (tangled rope at high power), and analysts who risk naturalizing
 *   the dependency as immutable law.
 *
 * KEY AGENTS:
 *   - Public universities and service providers: Primary victims (powerless/trapped at 70-90% funding) — lose autonomy, face suppression through budget cycles and implicit policy conditioning
 *   - State legislature and budget authority: Primary beneficiary (institutional/arbitrage) — uses funding control to coordinate institutional behavior and capture public resources for state priorities
 *   - Mid-tier administrators and department heads: Secondary victims (moderate/constrained) — experience genuine coordination benefits (stable baseline) alongside extraction (compliance burden)
 *   - Elite research institutions: Secondary beneficiary (powerful/arbitrage) — capture disproportionate research funding and prestige; extract from other institutions through competitive funding concentration
 *   - Democratic accountability mechanisms: Victim collective (powerless/trapped) — appear to constrain extraction through oversight but often lack enforcement power against budget gatekeeping
 *   - Analytical observer: Sees risk of false mountain (civilizational/analytical) — dependency appears natural until revealed as contingent post-WWII political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_funding_dependency, 0.58).
domain_priors:suppression_score(state_funding_dependency, 0.72).
domain_priors:theater_ratio(state_funding_dependency, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_funding_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_funding_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_funding_dependency, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_funding_dependency, tangled_rope).
narrative_ontology:human_readable(state_funding_dependency, "State Funding Dependency in Institutional Governance").
narrative_ontology:topic_domain(state_funding_dependency, "political_economy/institutional_capture").

domain_priors:requires_active_enforcement(state_funding_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_funding_dependency, state_apparatus).
narrative_ontology:constraint_beneficiary(state_funding_dependency, budget_gatekeeping_institutions).
narrative_ontology:constraint_victim(state_funding_dependency, funded_organizations).
narrative_ontology:constraint_victim(state_funding_dependency, democratic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT ORGANIZATION (SNARE) — A public university, nonprofit, or service provider depends on state funding for 60-90% of operations. Exit is impossible without existential collapse. The organization must conform to budget cycles, reporting requirements, and implicit policy preferences. Suppression is structural: to lose funding is to cease functioning. Maximum experienced extraction — the organization bears full cost of the dependency relationship.
constraint_indexing:constraint_classification(state_funding_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER ADMINISTRATOR (TANGLED ROPE) — A department head or program director receives genuine coordination benefits (stable baseline funding, access to shared infrastructure, participation in state planning) alongside extraction (compliance burden, loss of autonomy, reporting overhead). Exit is costly but possible (private sector, foreign institution, retirement). Suppression is high but not absolute — constrained rather than trapped.
constraint_indexing:constraint_classification(state_funding_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE BUDGET AUTHORITY (ROPE) — From the state's perspective, funding dependency is a pure coordination mechanism. The state uses budget allocation to coordinate behavior of distributed institutions (universities, hospitals, social services) toward state policy objectives. No significant suppression of the state's options — it can adjust funding, retract it, restructure institutions, or reallocate resources freely. Net beneficiary.
constraint_indexing:constraint_classification(state_funding_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HISTORICAL UNIVERSITY GOVERNANCE (PITON) — The post-WWII state university model emerged as genuine public coordination: states funded institutions to serve public goods (research, mass higher education). The functional model persists through institutional inertia and legislative path-dependence, but its coordination function has largely atrophied. State funding now subsidizes workforce training for private employers; universities compete for private research grants; teaching quality depends on adjunct labor markets. Theater ratio (0.65) reflects that budget justifications still invoke public good language while actual mechanisms serve workforce production and research concentration. The piton model is maintained through accreditation requirements and historical naming, not functional necessity.
constraint_indexing:constraint_classification(state_funding_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ELITE RESEARCH INSTITUTION (TANGLED ROPE) — Universities like Stanford, MIT, or Harvard receive state funding (through federal research grants, state tax benefits, infrastructure investment) but have sufficient private endowment, corporate partnerships, and prestige to reduce dependency. They experience the funding system as coordination with embedded extraction: state funds require compliance reporting and some deference to state priorities, but exit is possible (private funding, endowment growth). They capture asymmetric benefits (federal research funding concentration, intellectual property from publicly-funded research) while bearing low suppression costs. Moderately organized with arbitrage options.
constraint_indexing:constraint_classification(state_funding_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN?) — From a civilizational analytical view, state control of public resource allocation appears as an immutable structural feature of modern political economy. No institution can entirely escape state funding or state authority. But this perspective risks naturalizing a contingent historical arrangement (the post-WWII welfare state funding model) as if it were a law of nature. The analytical observer should recognize this as a false summit: the dependency is real, but not immutable.
constraint_indexing:constraint_classification(state_funding_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_funding_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_funding_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_funding_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_funding_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_funding_dependency, TR),
    TR >= 0.70.

:- end_tests(state_funding_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting significant asymmetric control of resources and decision-making. The state captures institutional behavior through funding conditionality without bearing the operational costs or risks of the institutions. Over 30 years, extractiveness has increased from 0.35 (when public funding was abundant and less conditional) to 0.58 (neoliberal era with performance metrics and privatization threats). Suppression (0.72): High. Organizations cannot exit state funding without existential collapse; they must comply with budget cycles, reporting requirements, and implicit policy preferences. Suppression is structured through both material barriers (no alternative funding sources) and institutional capture (regulatory requirements, accreditation tied to state recognition). Theater ratio (0.65): Moderate-high and rising. Public funding systems maintain democratic accountability language (transparent budgets, legislative oversight, public mission) while actual mechanisms serve budget gatekeeping and policy control. The rise from 0.40 to 0.65 reflects increasing performative content: outcome metrics, efficiency measures, and 'accountability' frameworks that measure compliance rather than mission achievement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The state sees coordination (Rope) — funding achieves alignment and policy goals. Dependent organizations see extraction (Snare) — they lose autonomy and bear suppression. Mid-tier actors see hybrid dynamics (Tangled Rope) — genuine coordination benefits alongside extraction burden. Elite institutions see favorable tangled rope — they capture benefits while retaining exit options. The piton perspective reveals the constraint is increasingly performative — democratic language masks budget gatekeeping. The analytical observer risks false mountain — seeing state funding dependency as inherent to governance rather than a contingent post-WWII choice. The perspectival gap reveals that the constraint is not objectively 'one type' but rather a presheaf of readings indexed by agent power and exit capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the state apparatus and budget-gatekeeping institutions (legislative committees, finance ministries) that control resource allocation and capture institutional behavior as a coordination benefit. Their exit options are arbitrage — they can shift funding priorities, redirect resources, or restructure institutions without existential consequence. This produces low d values and negative chi from the state's perspective. Victims are the funded organizations (universities, hospitals, social services) and the abstract collective good of democratic accountability. Their exit options are trapped (existential collapse without state funding) or constrained (costly transition to private funding with political barriers). This produces high d values and high chi from their perspective. The derived directionality chain: beneficiaries (state, budget gatekeepers) + arbitrage exit → d ≈ 0.05-0.15 → f(d) ≈ -0.08 to 0.05 → low chi (experienced as rope coordination). Victims (dependent organizations) + trapped exit → d ≈ 0.92-0.95 → f(d) ≈ 1.38-1.42 → high chi (experienced as snare extraction). Elite institutions + arbitrage exit + partial beneficiary status → d ≈ 0.30-0.40 → f(d) ≈ 0.30-0.55 → moderate chi (experienced as tangled rope with escape velocity).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that tangled rope classification is accurate from the aggregate institutional perspective: genuine coordination function (stable baseline funding, policy alignment) coexists with asymmetric extraction (loss of autonomy, budget conditioning, compliance burden). The mandatrophy arises from the temptation to call this 'pure coordination' (rope) because democratic framing emphasizes public good, or 'pure extraction' (snare) because institutions lose control. The DN resolution is that BOTH are true from different structural positions: the state experiences it as coordination; dependent organizations experience it as extraction; the constraint itself requires enforcement to maintain (state capacity to withhold funding, institutional compliance mechanisms). The theater ratio rising to 0.65 signals increasing mandatrophy risk: as the constraint becomes more explicitly extractive (performance metrics, privatization threats), the democratic framing becomes thinner. If theater continues rising above 0.75, the constraint risks cascading to pure snare as the coordination function fully atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_threshold,
    'At what funding percentage does an organization transition from constrained to trapped?',
    'Historical case analysis: organizations that exited state funding at 40%, 60%, 80% thresholds; cost analysis of creating alternative revenue streams at different initial dependency levels',
    'If threshold is 50%: many moderately-funded organizations misclassified as mobile when they are trapped. If threshold is 80%: only fully-dependent organizations are classified as snare, missing mid-tier extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_threshold, empirical, 'Percentage of state funding that constitutes structural trap vs constrained dependency').

omega_variable(
    alternative_funding_viability,
    'Can institutions genuinely replace state funding through private endowment, corporate partnerships, or international funding, or does political pressure prevent true exit?',
    'Comparative institutional analysis: institutions that attempted private funding transitions; documentation of political opposition or regulatory barriers to reduced state dependency; international case studies of private-funded equivalents',
    'If alternatives are viable: exit_options should upgrade from trapped to constrained; classification becomes Tangled Rope rather than pure Snare. If alternatives face political barriers: exit is theoretically possible but practically suppressed; identity_locked exit mechanism may apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_viability, empirical, 'Whether organizational exit from state funding is structurally or politically constrained').

omega_variable(
    identity_lock_mechanism,
    'Is institutional identity fused with state funding dependency (self-concept as public institution), or is the constraint purely material (budget requirements)?',
    'Institutional culture analysis: whether leadership, mission statements, and strategic planning frame the organization around state mission or private sustainability; historical trajectory of institutions that successfully transitioned to private funding; employee surveys on organizational identity',
    'If identity_locked: some organizations will be unable to exit even if alternative funding exists; suppression is partially internalized. If purely material: exit is blocked by budget requirements alone; organizations could shift direction if funding became available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether institutional dependency is identity-constituted or materially structural').

omega_variable(
    democratic_accountability_cost,
    'What is the extractive cost of maintaining public accountability mechanisms (board meetings, budget transparency, legislative oversight) vs the extraction cost of state control?',
    'Comparative cost analysis: administrative burden of public accountability vs discretionary power retained by state funding gatekeepers; measurement of actual policy constraint imposed by legislative oversight vs freedom lost to budget conditionality',
    'If accountability cost exceeds state control cost: the constraint is legitimately coordinating public interests alongside extraction. If state control dominates: accountability mechanisms are theater, and the constraint is pure snare despite democratic framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_accountability_cost, preference, 'Trade-off between democratic accountability cost and state extraction benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_funding_dependency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sfund_tr_t0, state_funding_dependency, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sfund_tr_t15, state_funding_dependency, theater_ratio, 15, 0.55).
narrative_ontology:measurement(sfund_tr_t30, state_funding_dependency, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(sfund_be_t0, state_funding_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sfund_be_t15, state_funding_dependency, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(sfund_be_t30, state_funding_dependency, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_funding_dependency, resource_allocation).
narrative_ontology:boltzmann_floor_override(state_funding_dependency, 0.18).
narrative_ontology:affects_constraint(state_funding_dependency, public_university_autonomy).
narrative_ontology:affects_constraint(state_funding_dependency, healthcare_institutional_capture).
narrative_ontology:affects_constraint(state_funding_dependency, regulatory_budget_hostage_dynamics).

% DUAL FORMULATION NOTE:
% State funding dependency is upstream to more specific institutional capture mechanisms. Organizations with high funding dependency are structurally vulnerable to regulatory capture, mission drift, and political conditioning. Each downstream constraint (university autonomy loss, healthcare budget cycles) represents a specific instantiation of the general funding dependency mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_funding_dependency, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
