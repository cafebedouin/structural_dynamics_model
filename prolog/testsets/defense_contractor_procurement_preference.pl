% ============================================================================
% CONSTRAINT STORY: defense_contractor_procurement_preference
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_defense_contractor_procurement_preference, []).

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
 *   constraint_id: defense_contractor_procurement_preference
 *   human_readable: Defense Contractor Procurement Preference
 *   domain: political_economy/defense_spending
 *
 * SUMMARY:
 *   Defense contractor procurement preference represents a structural
 *   constraint where military supply security concerns (legitimate but
 *   time-bounded) have calcified into permanent cost-extraction mechanisms.
 *   The constraint originated during the Cold War as a genuine coordination
 *   solution to supply chain vulnerability and strategic risk — concentrating
 *   production with established, cleared, reliable contractors made
 *   operational sense when supply disruption posed existential threat.
 *   However, the original strategic conditions have substantially changed:
 *   supply chains are now globalized with redundancy, strategic materials are
 *   more diversified, and manufacturing complexity has shifted toward systems
 *   integration rather than single-vendor dominance. Yet the preference
 *   system persists with increasing extractiveness and theater ratio,
 *   suggesting the institutional mechanism has outlived its coordination
 *   function and now operates primarily as rent extraction. The constraint
 *   exhibits all six types across different perspectives, making it a
 *   diagnostic case for institutional ossification — where a genuine Rope
 *   (coordination mechanism) has degraded into a Snare (pure extraction
 *   dressed in security language) while maintaining institutional theater
 *   that prevents reform.
 *
 * KEY AGENTS:
 *   - Taxpayers: Primary victim (powerless/trapped) — bear cost of inflated procurement prices with zero exit option
 *   - Established Defense Contractors: Primary beneficiary (institutional/arbitrage) — capture sustained profit margins above competitive rates; can arbitrage to commercial markets if domestic preference erodes
 *   - Emerging Suppliers: Secondary victim (powerful/mobile) — face structural barriers to entry despite commercial competitiveness; can exit to non-defense markets but forgo higher margins
 *   - Procurement Officials: Structural agent (moderate/constrained) — constrained by regulatory justification requirements; institutional pressure and career risk maintain preference even when competitiveness would be preferred
 *   - Department of Defense: Mixed agent (powerful/mobile) — benefits from reduced procurement friction but suffers inflated budgets and reduced competitive pressure for innovation
 *   - Congressional Reform Coalition: Organized challenger (organized/constrained) — GAO, CBO, defense reformers can identify reform pathways but face suppression from contractor lobbying and political capture of defense committees
 *   - Cold War Supply Security Model: Institutional artifact (institutional/arbitrage) — the original security rationale; persists through inertia despite changed conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(defense_contractor_procurement_preference, 0.58).
domain_priors:suppression_score(defense_contractor_procurement_preference, 0.65).
domain_priors:theater_ratio(defense_contractor_procurement_preference, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(defense_contractor_procurement_preference, extractiveness, 0.58).
narrative_ontology:constraint_metric(defense_contractor_procurement_preference, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(defense_contractor_procurement_preference, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(defense_contractor_procurement_preference, tangled_rope).
narrative_ontology:human_readable(defense_contractor_procurement_preference, "Defense Contractor Procurement Preference").
narrative_ontology:topic_domain(defense_contractor_procurement_preference, "political_economy/defense_spending").

domain_priors:requires_active_enforcement(defense_contractor_procurement_preference).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(defense_contractor_procurement_preference, established_defense_contractors).
narrative_ontology:constraint_beneficiary(defense_contractor_procurement_preference, procurement_officials).
narrative_ontology:constraint_beneficiary(defense_contractor_procurement_preference, incumbent_technology_providers).
narrative_ontology:constraint_victim(defense_contractor_procurement_preference, taxpayers).
narrative_ontology:constraint_victim(defense_contractor_procurement_preference, military_operational_effectiveness).
narrative_ontology:constraint_victim(defense_contractor_procurement_preference, emerging_competitive_suppliers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAXPAYER (SNARE) — Trapped in the extraction mechanism through mandatory taxation with no exit option. Bears cost of inflated procurement prices with no ability to refuse contribution or select alternative vendors. Maximum suppression: no voice in procurement decisions, no knowledge of actual cost differentials, no mechanism for collective exit.
constraint_indexing:constraint_classification(defense_contractor_procurement_preference, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY PROCUREMENT OFFICER (SNARE) — Structurally constrained by regulatory requirement to justify preference for incumbent contractors through cost-benefit analyses that embed sunk cost fallacies and switching cost assumptions. Career risk of recommending unproven suppliers if performance issues emerge; institutional pressure to minimize procurement friction. Extraction flows toward the established contractor through officer's constrained choices, not toward the officer themselves.
constraint_indexing:constraint_classification(defense_contractor_procurement_preference, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMERGING SUPPLIER (TANGLED ROPE) — Mobile and powerful enough to compete in commercial markets, but faces high barriers to defense procurement entry: qualification requirements, facility certifications, security clearances, incumbent relationship advantages. The constraint coordinates genuine security and reliability concerns (small suppliers pose real integration risk) while simultaneously extracting through preference mechanisms that outlast their security justification. Can exit to commercial market but forgoes higher-margin defense contracts.
constraint_indexing:constraint_classification(defense_contractor_procurement_preference, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED DEFENSE CONTRACTOR (ROPE) — Net beneficiary with arbitrage options. The constraint coordinates genuine military needs (integration, reliability, long-term support) while providing sustained profit margins above competitive rates. Can exit to commercial or international markets if domestic preference erodes, but domestically captures rent. Experiences constraint as enabling coordination of complex procurement with supplier reliability.
constraint_indexing:constraint_classification(defense_contractor_procurement_preference, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DOD INSTITUTIONAL ACTOR (TANGLED ROPE) — Powerful organization with mobile options (change procurement rules, diversify suppliers, shift to government manufacturing). The constraint both coordinates legitimate military supply security AND extracts through embedded cost-inflation. DoD's own interests are mixed: preferences reduce procurement friction but inflate budgets and reduce competitive pressure for innovation. Theater component reflects performative compliance with competition rules while maintaining preference through justified-but-circular reasoning.
constraint_indexing:constraint_classification(defense_contractor_procurement_preference, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: COLD WAR PROCUREMENT ARCHITECTURE (PITON) — The preference system originated in genuine Cold War supply security concerns (Soviet disruption risk, strategic material scarcity). Those original conditions have largely expired, but the institutional arrangement persists through inertia and revolving-door relationships between contractors and procurement officials. Theater ratio is high because actors maintain the architecture through rhetorical references to security risks that no longer substantively justify the cost premium. The mechanism is degraded: it persists not because it solves the security problem it was designed for, but because changing it creates political friction.
constraint_indexing:constraint_classification(defense_contractor_procurement_preference, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: REFORM COALITION (SCAFFOLD) — Organized agents (Government Accountability Office, Congressional Budget Office, defense reform advocates) can see the constraint as a temporary coordination failure with a sunset mechanism: competitive sourcing pilots, dynamic supplier qualification, performance-based contract awards that shift from incumbent preference to measurable outcomes. Constraint classifications as Scaffold because the coalition has agency and identifies a time-bound reform path, though suppression from established contractor lobbying is high.
constraint_indexing:constraint_classification(defense_contractor_procurement_preference, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a global civilizational view, all states with defense industrial bases face coordination problems between competitive pricing and supply security. The tension between cost efficiency and strategic resilience is sometimes naturalized as an immutable law of military procurement: you must choose between cheap suppliers and reliable suppliers. However, structural data reveals this as a false summit — the apparent natural law conflates security legitimacy (real but bounded) with perpetual cost-inflation (contingent on institutional design). The engine will flag this as naturalization of a policy choice, not a natural limit.
constraint_indexing:constraint_classification(defense_contractor_procurement_preference, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(defense_contractor_procurement_preference_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(defense_contractor_procurement_preference, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(defense_contractor_procurement_preference, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(defense_contractor_procurement_preference, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(defense_contractor_procurement_preference, TR),
    TR >= 0.70.

:- end_tests(defense_contractor_procurement_preference_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The preference system extracts through sustained cost-inflation estimated at 15-30% above competitive rates for equivalent capabilities, measured across platforms with both preference-sourced and competitively-sourced analogues. The extraction has increased over the 20-year interval as the original security rationale has weakened but institutional mechanisms have strengthened through accumulated relationships and contract embedding. Initial value (0.35) reflects genuine Cold War-era supply security justification; final value (0.58) reflects degraded coordination base with dominant extraction mechanism. Suppression (0.65): High. Powerful barriers prevent competitive alternatives: security clearance requirements, facility certifications, incumbent relationship advantages, regulatory compliance costs that disadvantage new entrants, revolving-door relationships between contractors and procurement officials that naturalize preference as coordination rather than extraction. Theater ratio (0.68): Moderately high. The preference system maintains rhetorical cover through references to supply security and integration risk that no longer substantively justify the cost premium. Procurement justifications invoke switching costs that have declined, security threats that have diversified, and reliability risks that are manageable through contract performance requirements. The theater has increased over the interval as original coordination rationale weakened but institutional rhetoric intensified.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap is nearly maximal. The established contractor perceives Rope (genuine coordination of military needs). The taxpayer perceives Snare (pure extraction). The procurement official perceives Snare (constrained to justify preference). The emerging supplier perceives Tangled Rope (mixed legitimate barriers and extractive preference). The DoD perceives Tangled Rope (mixed coordination benefit and cost inflation). The Cold War model perceives Piton (degraded original function). The reform coalition perceives Scaffold (solvable through competitive sourcing pilots and outcome-based contracting). The analytical observer risks perceiving Mountain (immutable cost-security tradeoff). These gaps reveal that there is no neutral observation point — the classification depends entirely on structural position, and each position generates a classification consistent with its interests and constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position relative to extraction. Established contractors occupy beneficiary position (arbitrage exit, institutional power) → low d → negative or near-zero f(d) → net beneficiary. Taxpayers occupy trapped victim position (mandatory contribution, no exit, powerless) → high d → high f(d) ≈ 1.42 → maximum experienced extraction. Emerging suppliers occupy constrained victim position (powerful enough to compete commercially but blocked from defense market) → moderate-high d → moderate f(d) → significant experienced extraction despite market power. Procurement officers occupy constrained structural position where their own interests align with efficient sourcing but their institutional role is defined by preference justification → d derived from victim status of their constrained capacity despite moderate power → higher d than their nominal institutional status would suggest. The piton classification derives from high theater ratio (0.68) indicating performative maintenance rather than functional necessity. The mountain classification at analytical context represents naturalization risk — what appears as immutable law of military/industrial tension is actually contingent institutional design.
 *
 * MANDATROPHY ANALYSIS:
 *   INSTITUTIONAL OSSIFICATION: This constraint resolves mandatrophy by demonstrating how a genuine Rope (original Cold War coordination) has degraded into a Snare (contemporary extraction) while maintaining institutional theater that prevents recognition of the shift. The original coordination function — concentrating supply with qualified, cleared contractors to reduce supply disruption risk — was real and necessary under Cold War conditions. The mandate was legitimate. As conditions changed (supply chain globalization, strategic material diversification, manufacturing complexity shift), the coordination function weakened but the institutional arrangement persisted. The system now extracts (inflated prices, reduced innovation pressure) while maintaining theater (security justifications that no longer bind). The mandatrophy is resolved by showing that BOTH readings are correct within their structural context: the system IS genuine coordination from the contractor perspective (they do provide reliable, integrated systems), AND it IS pure extraction from the taxpayer perspective (they are forced to fund inflated procurement). The resolution is to recognize that what appears as 'necessity' is actually 'path dependency dressed as necessity' — the constraint persists because changing it creates political friction, not because the original security rationale still applies. The Scaffold perspective identifies the exit pathway (competitive sourcing reform) but indicates it is suppressed by institutional inertia and contractor lobbying rather than structural impossibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_empirical_validation,
    'Are the switching costs and integration risks cited to justify contractor preference empirically grounded, or are they inflated post-hoc rationalizations?',
    'Comparative cost analysis of actual switching events; audit of integration risk claims against documented failure rates for new suppliers post-qualification; contractor testimony under audit questioning whether preference structure was present during original product development vs added later',
    'If switching costs substantially real: constraint reclassifies as higher-fidelity Tangled Rope with genuine coordination base. If inflated: constraint is pure Snare dressed in coordination language — no real switching risk, just path dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_empirical_validation, empirical, 'Empirical validation of switching cost claims').

omega_variable(
    operational_performance_correlation,
    'Does contractor preference system correlate with better military operational effectiveness, or is performance independent of procurement incumbency?',
    'Longitudinal study of system performance (reliability, capability, time-to-deployment) for platforms sourced through preference vs competitive mechanisms; control for confounds (legacy vs new systems, mission requirements, maintenance protocols)',
    'If strong positive correlation: preference system coordinates genuine operational needs (Rope or Tangled Rope classification strengthened). If null/negative correlation: preference is pure extraction with no functional coordination base (Snare classification strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_performance_correlation, empirical, 'Whether contractor preference correlates with operational effectiveness').

omega_variable(
    revolving_door_identity_lock_institutional,
    'Is the procurement preference system maintained through structural incentives or through institutional identity fusion where procurement officials have internalized contractor interests as their own?',
    'Career path analysis of procurement officials post-government employment; comparison of lobbying intensity for contractors with high vs low revolving-door traffic; interviews with procurement officials about perceived necessity of preference vs their explicit justifications',
    'If structural incentives dominant: standard institutional capture analysis applies. If identity lock significant: procurement officials may perceive no alternative even when structural conditions would permit one — affects classification of their perspective from constrained to identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolving_door_identity_lock_institutional, empirical, 'Whether institutional identity fusion maintains preference beyond structural necessity').

omega_variable(
    congressional_dysfunction_barrier,
    'Is procurement preference reform blocked by genuine military necessity arguments or by congressional dysfunction and political capture of defense committees?',
    'Analysis of Congressional debate transcripts; comparison of military testimony supporting preference vs GAO/CBO competitive sourcing recommendations; tracking of reform bill introduction and amendment patterns across legislative cycles',
    'If military necessity genuine: reform timeline longer but Scaffold classification valid (sunset requires genuine coordination redesign). If political capture dominant: Scaffold may be aspirational rather than structural — suppression from lobbying prevents sunset even when reform coalition exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_dysfunction_barrier, conceptual, 'Whether procurement preference reform is blocked by necessity or political dysfunction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(defense_contractor_procurement_preference, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcpp_tr_t0, defense_contractor_procurement_preference, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dcpp_tr_t10, defense_contractor_procurement_preference, theater_ratio, 10, 0.58).
narrative_ontology:measurement(dcpp_tr_t20, defense_contractor_procurement_preference, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(dcpp_be_t0, defense_contractor_procurement_preference, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dcpp_be_t10, defense_contractor_procurement_preference, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(dcpp_be_t20, defense_contractor_procurement_preference, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(defense_contractor_procurement_preference, resource_allocation).
narrative_ontology:affects_constraint(defense_contractor_procurement_preference, military_innovation_incentive_structure).
narrative_ontology:affects_constraint(defense_contractor_procurement_preference, defense_industrial_base_consolidation).
narrative_ontology:affects_constraint(defense_contractor_procurement_preference, congressional_defense_committee_capture).

% DUAL FORMULATION NOTE:
% The procurement preference system decomposes into two structurally distinct constraints: (1) military_supply_security_coordination (ε≈0.20, genuine Mountain/Rope at civilizational scale), representing the legitimate Cold War-origin coordination problem; (2) defense_contractor_procurement_preference (ε=0.58, contemporary Tangled Rope/Snare), representing the accumulated extraction that has accumulated on top of the original coordination. The second constraint is downstream of the first — the preference system was justified by supply security needs and has persisted through institutional inertia even as those needs have diversified and weakened. The two constraints have different ε values, different time horizons, and different classification patterns. This story focuses on the contemporary constraint (ε=0.58) and its degradation of the original coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(defense_contractor_procurement_preference, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
