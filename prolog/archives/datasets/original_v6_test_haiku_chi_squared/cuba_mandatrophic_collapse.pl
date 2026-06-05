% ============================================================================
% CONSTRAINT STORY: cuba_mandatrophic_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cuba_mandatrophic_collapse, []).

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
 *   constraint_id: cuba_mandatrophic_collapse
 *   human_readable: Cuban Mandatrophy (The GAESA-Infrastructure Divergence)
 *   domain: political/economic/technological
 *
 * SUMMARY:
 *   Cuban mandatrophy describes the terminal wasting of national
 *   infrastructure, food security, and public health caused by the structural
 *   prioritization of the 'Military-Tourism Mandate' (GAESA) over the
 *   essential margins of the civilian state. From 1960-1991, this mandate was
 *   functionally rational: Soviet subsidies (~USD 5-6 billion/year) allowed
 *   Cuba to maintain both military deterrence and civilian provisioning. The
 *   mandate coordinated survival under external blockade. Post-1991, Soviet
 *   collapse removed the subsidy, yet the mandate persisted as institutional
 *   inertia. GAESA now appropriates ~60% of tourism revenue (USD 3+
 *   billion/year pre-COVID) and most hard-currency enterprise, leaving
 *   civilian state with residual resources. The result is accelerating
 *   divergence: GAESA-controlled sectors (resorts, nickel extraction,
 *   remittance channels, pharma) have modernized infrastructure and
 *   hard-currency supply chains. Civilian sectors (agriculture, public
 *   health, electricity grid, water systems) have experienced compounding
 *   underinvestment — spare parts scarcity, fuel rationing, deferred
 *   maintenance, brain drain. Mandatrophy is the causal mechanism: the
 *   mandate prevents rebalancing because GAESA has institutional veto over
 *   resource allocation. The centrally planned state apparatus maintains the
 *   mandate through performative planning (five-year plans, output targets,
 *   state media rationalization) that masks the reality that capital flows
 *   follow GAESA priority, not plan. The constraint exhibits all six DR types
 *   from different observer positions, but the dominant classification is
 *   Snare — the civilian population is trapped by exit restrictions, the
 *   informal economy is contingently tolerated but legally suppressed, and
 *   the military-tourism elite experience no constraint.
 *
 * KEY AGENTS:
 *   - GAESA Military Enterprise: Primary beneficiary (institutional/arbitrage) — controls ~60% of tourism revenue and hard-currency enterprises; experiences mandate as coordination solution
 *   - Rural Agricultural Workers: Primary victim (powerless/trapped) — requisitioned land, fuel scarcity, no investment capital; trapped by emigration barriers
 *   - Urban Civilian Population: Primary victim (powerless/trapped) — energy rationing (14-hour blackout windows), food scarcity (1,200 cal/day rations), medicine shortages; trapped by legal exit restrictions
 *   - Public Health and Agricultural Workforce: Secondary victim (moderate/constrained) — skilled professionals (doctors, agronomists) trapped by state appointment; expertise appropriated for minimal compensation (~USD 15-30/month state salary); fuel and spare parts scarcity prevent service delivery
 *   - Informal Economy Coalition: Organized secondary victim (organized/constrained) — remittance handlers, private farmers (usufruct plots), black market repair networks; tolerated conditionally, legally suppressed; benefits from tourism supply chains but subject to arbitrary enforcement
 *   - Centrally Planned State Apparatus: Institutional degraded actor (institutional/constrained) — maintains mandate through inertia; once functionally rational (Soviet subsidy era), now vestigial; theater ratio rising (performative planning masking GAESA priority)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing mandatrophy as inherent to revolutionary regimes; engine false summit detector reveals contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cuba_mandatrophic_collapse, 0.78).
domain_priors:suppression_score(cuba_mandatrophic_collapse, 0.82).
domain_priors:theater_ratio(cuba_mandatrophic_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, extractiveness, 0.78).
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cuba_mandatrophic_collapse, snare).
narrative_ontology:human_readable(cuba_mandatrophic_collapse, "Cuban Mandatrophy (The GAESA-Infrastructure Divergence)").
narrative_ontology:topic_domain(cuba_mandatrophic_collapse, "political/economic/technological").

domain_priors:requires_active_enforcement(cuba_mandatrophic_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cuba_mandatrophic_collapse, gaesa_military_apparatus).
narrative_ontology:constraint_beneficiary(cuba_mandatrophic_collapse, tourism_sector_elite).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, rural_agriculture_workers).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, urban_civilian_population).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, public_health_system).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, energy_grid_maintenance).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, food_security_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL AGRICULTURAL WORKER (SNARE) — Trapped within Cuba's territory; cannot exit without extraordinary risk (balsero migration). Bears full extraction cost: requisition of productive land for tourism resorts, fuel scarcity for machinery, lack of credit and seed investment. No coordination benefit. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈1.10. Pure extraction.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: URBAN CIVILIAN POPULATION (SNARE) — Trapped by legal exit restrictions and economic non-viability of emigration. Bears extraction through energy rationing, food scarcity (14-hour blackout windows, 1,200 cal/day rations reported), medicine shortages. GAESA siphons foreign exchange from tourism; civilian sector receives residual. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈1.07. Structural dependency with no exit.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INFORMAL ECONOMY COALITION (TANGLED ROPE) — Black market remittance handlers, private farmers (usufruct plots), clandestine repair networks, casa particular operators. These agents benefit from access to GAESA's foreign exchange (remittances, tourism service supply chains) but are constrained by legal prohibition and arbitrary enforcement. Mixed structure: coordination function (they keep the civilian economy functioning) plus extraction (GAESA appropriates rents through tax and arbitrary confiscation). d≈0.58, f(d)≈0.80, σ=1.0 → χ≈0.62. Unstable hybrid.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GAESA MILITARY ENTERPRISE (ROPE) — Primary beneficiary. Controls ~60% of tourism revenue, foreign exchange, and hard-currency enterprises. Experiences the constraint as pure coordination within military logistics: infrastructure theft, fuel diversion, and civilian asset appropriation solve the military's operational cash-flow problem. GAESA has arbitrage exit — it can shift resources to remittance control, nickel extraction, or pharmaceutical exports. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.09. Negative effective extraction = net beneficiary. The constraint is functional for GAESA.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CENTRALLY PLANNED STATE APPARATUS (PITON) — The mandate (military-tourism prioritization) was structurally functional during the Soviet subsidy era (1960s-1991). It coordinated economic survival under external pressure. But post-1991, the mandate persists as inertia: the central planning machinery still allocates resources to GAESA first, civilian needs second, despite the coordination function having evaporated. Theater ratio 0.68: extensive performative planning (five-year plans, output targets, state media rationalization) masks that actual capital flows follow GAESA priority, not planning. The machinery is degraded — it was once a rational response to blockade; now it is vestigial. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.27. Low effective extraction because the apparatus itself is weakly enforced.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC HEALTH AND AGRICULTURAL WORKFORCE (SNARE) — Skilled professionals (doctors, agronomists, engineers) are trapped by state appointment and emigration restrictions, yet their labor is systematically undersupplied: fuel scarcity prevents agricultural extension, medicine shortages prevent clinical care, spare parts scarcity prevents infrastructure maintenance. They experience the constraint as pure extraction: their expertise is appropriated for minimal compensation (state salary ~USD 15-30/month) while GAESA captures the surplus. Limited exit: professional licensing does not transfer internationally without retraining; family ties constrain departure. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.90. High extraction with slight coalition potential.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN READING) — At civilizational timescale and global scope, an observer might naturalize the constraint as an inherent property of revolutionary regimes: 'survival under embargo requires military prioritization; this is a law of authoritarian statecraft.' However, the structural data (ε=0.78, suppression=0.82, theater=0.68, requires_active_enforcement=true, mandatrophy=true) contradicts the mountain classification. The constraint is not a natural law but a contingent institutional choice. The false summit detector flags this perspective as aspirational naturalization.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cuba_mandatrophic_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cuba_mandatrophic_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cuba_mandatrophic_collapse, TR),
    TR >= 0.70.

:- end_tests(cuba_mandatrophic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high and rising. GAESA's prioritization diverts 60%+ of hard-currency revenue and most capital inputs away from civilian infrastructure. The extraction is not primarily through overt taxation but through systematic underinvestment of civilian sectors combined with forced appropriation of civilian assets (land for tourism, state enterprise output redirected to military). The measurement shows extractiveness rising from 0.15 (1960, Soviet subsidy era where mandate coordinated survival) to 0.78 (2023, post-Soviet where mandate enables outright extraction). The upward trend reflects that the mandate's coordination rationale has evaporated — it now purely extracts. Suppression (0.82): Very high. Barriers to exit include legal restrictions on emigration (visas required, exit permits costly, family separation penalties), economic non-viability of legal emigration (wages insufficient for visa and passage), and physical risk (balsero migration deaths are endemic). Barriers to voice include state monopoly on media, suppression of independent journalism, arrest of dissidents, and surveillance of internet (VPN restrictions). Barriers to collective action include prohibition of independent unions and civil society organizations. Theater ratio (0.68): Moderate-high and rising. The central planning apparatus maintains performative activity (five-year plans, output targets, state media justification of mandate) that masks the actual capital flows following GAESA priority. Theater has increased from 0.35 (1960, when mandate matched Soviet subsidy reality) to 0.68 (2023, when performative planning is increasingly divorced from actual outcomes). The rise in theater with rise in extractiveness signals classic Goodhart drift: the planning apparatus optimizes for targets (plan fulfillment, production statistics) rather than actual civilian provisioning.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits extreme perspectival divergence. GAESA (institutional/arbitrage) sees Rope — the mandate coordinates military logistics and foreign exchange generation. The rural agricultural worker (powerless/trapped) sees Snare — pure extraction with no coordination benefit. The informal economy (organized/constrained) sees Tangled Rope — mixed coordination (it keeps civilians alive through remittances and private agriculture) plus extraction (GAESA appropriates rents and suppresses formal operation). The centrally planned state apparatus (institutional/constrained) sees Piton — a once-functional mandate now maintained through inertia. The analytical observer (analytical/analytical) risks seeing Mountain — naturalizing mandatrophy as inherent to authoritarian statecraft — but the structural data contradicts this. The perspectival gap is extreme because the constraint has fundamentally changed its character post-1991: from Rope-like (mandate solves coordination problem under Soviet subsidy) to Snare-like (mandate enables pure extraction in post-Soviet scarcity). The mandatrophy resolves this gap by identifying that the constraint has undergone a phase transition: it is structurally a different constraint in 1985 (ε=0.35, Rope-ish) than in 2023 (ε=0.78, Snare).
 *
 * DIRECTIONALITY LOGIC:
 *   GAESA beneficiary: Beneficiary + arbitrage exit → d≈0.05, f(d)≈-0.12. Net beneficiary. GAESA can shift resources to remittance control, nickel extraction, pharmaceutical exports — it has genuine exit options. Rural agricultural worker: Victim + trapped exit → d≈0.95, f(d)≈1.42. Maximum extraction. No exit: balsero migration is lethal (estimated 20-30% mortality on crossings); family separation is enforced penalty; legal emigration requires government permission and hard currency. Urban civilian: Victim + trapped exit → d≈0.92, f(d)≈1.38. Near-maximum extraction. Trapped by combination of legal restrictions and economic non-viability. Public health/agriculture workforce: Victim + constrained exit (not fully trapped, not fully mobile) → d≈0.85, f(d)≈1.15. High extraction. Professional licensing is internationally recognized for some fields (medicine), but language barriers, retraining requirements, and family ties constrain departure; state salary appropriates expertise for minimal compensation. Informal economy coalition: Mixed victim/beneficiary status, constrained exit → d≈0.58, f(d)≈0.80. They benefit from access to GAESA's foreign exchange (remittances, tourism service supply chains) but are legally suppressed and subject to arbitrary enforcement; exit is possible but carries legal and financial costs. Centrally planned apparatus: Institutional + constrained (not fully trapped because it has formal policy authority, but politically constrained by GAESA veto) → d≈0.40, f(d)≈0.40. The apparatus maintains the mandate through inertia, not genuine beneficiary status; its 'exit' would be economic restructuring, which it lacks political capital to pursue.
 *
 * MANDATROPHY ANALYSIS:
 *   Cuban mandatrophy RESOLVED through historical decomposition and lifecycle analysis. The constraint has undergone a fundamental phase transition from 1960 to 2023. In 1960-1991 (Soviet subsidy era), the Military-Tourism Mandate was functionally Rope-like: it coordinated economic survival under external blockade by concentrating resources on military deterrence and hard-currency generation (tourism, nickel, sugar). Soviet subsidies (~USD 5-6 billion/year) allowed simultaneous maintenance of military and civilian sectors. Extractiveness was moderate (ε≈0.15-0.35); suppression was high (external blockade) but not directed at the mandate itself. The mandate had genuine coordination function. Post-1991, Soviet collapse removed the subsidy infrastructure. The mandate persisted as institutional inertia — the central planning apparatus continued to prioritize GAESA-controlled sectors out of path dependency, not rational coordination. GAESA now appropriates ~60% of tourism revenue with no corresponding civilian benefit. Extractiveness rose to 0.78. The constraint became Snare-like: pure extraction with suppression of exit (legal emigration restrictions, visa costs), voice (media monopoly), and collective action (no independent unions). Theater ratio rose from 0.35 to 0.68 as performative planning (five-year plans, output targets) increasingly masked actual capital flows following GAESA priority — a signature Goodhart drift. Mandatrophy is the result: civilian infrastructure (agriculture, public health, electricity grid) has undergone compounding underinvestment while GAESA-controlled sectors modernized. The constraint's classification depends entirely on temporal framing: snapshot views at 1985 (institutional Rope) vs 2023 (structural Snare) produce contradictory conclusions. The resolution: the constraint is legitimately both types, but at different lifecycle stages. The mandatrophy is real (civilian infrastructure wasting) because the constraint has transitioned from a functionally rational mandate to an extractive lock-in. Recovery would require political decision to rebalance resource allocation (Scaffold dynamics with sunset), but GAESA's institutional veto prevents this. The high mandatrophy confidence (true) reflects that this is not an ambiguous classification but a genuine phase transition with empirically measurable consequences: blackout hours increasing, agricultural production declining, health outcomes deteriorating, brain drain accelerating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gaesa_revenue_ceiling,
    'What is the maximum foreign exchange GAESA can extract from tourism before the civilian infrastructure collapses so severely that tourism infrastructure itself becomes unmaintainable?',
    'Historical modeling of tourism arrivals vs electricity blackout hours, vs GAESA reported revenue; marginal analysis of when tourist infrastructure (hotels, airports, roads) begins to degrade; comparison with 2020-2024 data showing visitor recovery patterns and simultaneous infrastructure failure',
    'If ceiling is near current extraction levels (~USD 3 billion/year): mandatrophy cannot be reversed without core economic restructuring. If ceiling is significantly higher: GAESA has extraction capacity without mandate collapse, suggesting political choice rather than structural inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gaesa_revenue_ceiling, empirical, 'Maximum foreign exchange GAESA can extract before tourism infrastructure fails').

omega_variable(
    informal_economy_coordination_threshold,
    'At what point does the informal economy''s coordination function (keeping civilians alive through remittances, private agriculture, clandestine repair networks) become so essential that it converts from Tangled Rope to Rope, requiring de facto recognition?',
    'Measurement of informal economy share of actual calories, energy, and service delivery vs state sector; tracking whether state apparatus begins selective toleration (licensing private farmers, opening remittance windows) as informal share approaches 50%+ of civilian needs',
    'If informal economy is <30% of civilian provisioning: state repression can continue (Snare stability). If >50%: mandatrophy creates structural pressure to formalize, potentially converting the constraint into a negotiation (Rope with sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_economy_coordination_threshold, empirical, 'Threshold at which informal economy coordination becomes politically undeniable').

omega_variable(
    succession_mandate_persistence,
    'Will post-succession leadership (post-Díaz-Canel) maintain the military-tourism mandate as institutional lock-in, or use succession as opportunity to rebalance resource allocation?',
    'Examination of succession regimes in comparable contexts (Vietnam, China, Laos); internal Party documents on economic reform proposals; behavior of military-linked commercial actors during transition periods; investor signaling and remittance patterns as indicators of regime legitimacy',
    'If mandate persists: mandatrophy is path-dependent, not contingent — suggests deep institutional inertia beyond individual leadership. If mandate rebalances: mandatrophy was a policy choice, reversible through political decision. Changes the long-term classification from Snare (structural) to Scaffold (transitional).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(succession_mandate_persistence, preference, 'Whether post-succession leadership will maintain military-tourism mandate').

omega_variable(
    agricultural_recovery_viability,
    'Can Cuban agriculture recover to pre-1990 production levels (export-oriented sugarcane + food security) if GAESA extraction were reduced by 50% and capital were redirected to farm inputs?',
    'Agricultural productivity modeling: soil depletion analysis (is arable land recoverable?); fuel-to-yield elasticity for Cuban conditions; comparison with agricultural recoveries in post-Soviet contexts (Ukraine grain recovery post-2015); expert agronomic assessment of infrastructure repair timelines',
    'If recovery is viable within 5-10 years: rebalancing the mandate becomes a live option (Scaffold with real sunset). If recovery requires 25+ years or is structurally impossible: mandatrophy may be terminal, and the constraint becomes path-dependent (irreversible Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agricultural_recovery_viability, empirical, 'Whether Cuban agriculture can recover with reduced GAESA extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cuba_mandatrophic_collapse, 1960, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cuba_theater_1960, cuba_mandatrophic_collapse, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(cuba_theater_1985, cuba_mandatrophic_collapse, theater_ratio, 1985, 0.42).
narrative_ontology:measurement(cuba_theater_2000, cuba_mandatrophic_collapse, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(cuba_theater_2015, cuba_mandatrophic_collapse, theater_ratio, 2015, 0.62).
narrative_ontology:measurement(cuba_theater_2023, cuba_mandatrophic_collapse, theater_ratio, 2023, 0.68).

% Extraction over time
narrative_ontology:measurement(cuba_extractiveness_1960, cuba_mandatrophic_collapse, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(cuba_extractiveness_1985, cuba_mandatrophic_collapse, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(cuba_extractiveness_2000, cuba_mandatrophic_collapse, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(cuba_extractiveness_2015, cuba_mandatrophic_collapse, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement(cuba_extractiveness_2023, cuba_mandatrophic_collapse, base_extractiveness, 2023, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cuba_mandatrophic_collapse, resource_allocation).
narrative_ontology:affects_constraint(cuba_mandatrophic_collapse, caribbean_remittance_dependency).
narrative_ontology:affects_constraint(cuba_mandatrophic_collapse, soviet_embargo_collapse).
narrative_ontology:affects_constraint(cuba_mandatrophic_collapse, informal_economy_shadow_provisioning).

% DUAL FORMULATION NOTE:
% Cuban mandatrophy decomposes into two structurally distinct constraints: (1) the original Military-Tourism Mandate (ε≈0.35, Rope-like, post-1960, during Soviet subsidy era), and (2) the post-1991 Mandate Persistence (ε≈0.78, Snare-like, after Soviet collapse). These are not the same constraint viewed from different angles — their base extraction values differ by a factor of 2.2, reflecting a fundamental change in the constraint's coordination function and extraction mechanism. The first is an institutional coordination response to blockade and subsidy dependence. The second is an extractive lock-in resulting from path dependency when the coordination rationale evaporated. They are linked by causation (the first created the institutional structures that locked in the second) but represent distinct constraint stories with different analytical foci. This story focuses on the post-1991 Snare-like constraint and its mandatrophic consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cuba_mandatrophic_collapse, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
