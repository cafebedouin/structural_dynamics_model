% ============================================================================
% CONSTRAINT STORY: export_control_reversibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_export_control_reversibility, []).

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
 *   constraint_id: export_control_reversibility
 *   human_readable: Export Control Reversibility in Advanced Semiconductor Policy
 *   domain: technology_governance/surveillance_studies/export_control_policy
 *
 * SUMMARY:
 *   Export controls on advanced semiconductors represent a policy instrument
 *   subject to administration change, industry lobbying, and geopolitical
 *   pressure. Unlike the upstream constraint (compute as fundamental brake on
 *   AI capability), export control reversibility is a contingent
 *   institutional arrangement. The constraint exhibits tangled_rope
 *   structure: genuine coordination function (preventing compute-enabled
 *   authoritarianism, maintaining technological advantage) coupled with
 *   substantial extraction (surveillance asymmetry, research disruption,
 *   policy whiplash). The reversibility creates asymmetric harm: controls can
 *   be relaxed quickly under industry pressure, enabling surveillance
 *   infrastructure deployment, but tightening controls does not dismantle
 *   already-deployed systems. Theater ratio (0.48) reflects moderate
 *   performative content: licensing reviews involve genuine technical
 *   assessment but also political theater around 'national security' framing
 *   that obscures market-access motivations. Suppression (0.62) reflects
 *   enforcement infrastructure (export licensing, end-use verification,
 *   entity lists) and barriers to alternative compute access, though
 *   suppression is not total—some actors can access compute through third
 *   countries or domestic production.
 *
 * KEY AGENTS:
 *   - Dissidents in Client States: Primary victim (powerless/trapped) — bear full cost of surveillance enabled during permissive export windows; cannot exit jurisdiction
 *   - Foreign Research Institutions: Secondary victim (moderate/constrained) — face research disruption from policy shifts; benefit from open access periods but bear planning costs
 *   - Semiconductor Manufacturers: Primary beneficiary (institutional/arbitrage) — lobby for relaxed controls, exploit licensing loopholes, benefit from market segmentation
 *   - Domestic AI Labs: Secondary beneficiary (institutional/arbitrage) — benefit from competitor restrictions and domestic compute access
 *   - National Security Apparatus: Mixed position (institutional/constrained) — wields export controls as strategic tool but constrained by industry lobbying and administration turnover
 *   - AI Safety Coalition: Organized agents (organized/mobile) — see current unilateral controls as temporary; building toward multilateral compute governance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination problem coupled with substantial extraction from policy instability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(export_control_reversibility, 0.58).
domain_priors:suppression_score(export_control_reversibility, 0.62).
domain_priors:theater_ratio(export_control_reversibility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(export_control_reversibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(export_control_reversibility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(export_control_reversibility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(export_control_reversibility, tangled_rope).
narrative_ontology:human_readable(export_control_reversibility, "Export Control Reversibility in Advanced Semiconductor Policy").
narrative_ontology:topic_domain(export_control_reversibility, "technology_governance/surveillance_studies/export_control_policy").

domain_priors:requires_active_enforcement(export_control_reversibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(export_control_reversibility, nvidia_shareholders).
narrative_ontology:constraint_beneficiary(export_control_reversibility, domestic_ai_labs).
narrative_ontology:constraint_beneficiary(export_control_reversibility, national_security_apparatus).
narrative_ontology:constraint_victim(export_control_reversibility, dissidents_in_client_states).
narrative_ontology:constraint_victim(export_control_reversibility, foreign_research_institutions).
narrative_ontology:constraint_victim(export_control_reversibility, global_ai_safety_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSIDENT IN CLIENT STATE (SNARE) — Trapped by surveillance infrastructure enabled during permissive export windows. Cannot exit jurisdiction; bears full cost of compute-enabled repression. When export controls relax, surveillance capacity flows to authoritarian clients; when controls tighten, existing deployments persist. The reversibility is asymmetric: restrictions can be lifted faster than surveillance infrastructure can be dismantled.
constraint_indexing:constraint_classification(export_control_reversibility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FOREIGN RESEARCH INSTITUTION (TANGLED ROPE) — Constrained by unpredictable policy shifts that disrupt multi-year research programs. Benefits from periods of open access to compute hardware but bears planning costs and research discontinuity when controls tighten. The coordination function (preventing military AI development) is genuine but extraction is substantial: legitimate research is collateral damage of blunt policy instruments.
constraint_indexing:constraint_classification(export_control_reversibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SEMICONDUCTOR MANUFACTURER (ROPE) — Arbitrage position: can lobby for relaxed controls, shift product lines, or exploit licensing loopholes. Experiences the constraint as coordination: export controls create a regulatory framework that, while sometimes restrictive, provides predictable rules and protects domestic market position. Net beneficiary during both tight and loose control regimes through market segmentation.
constraint_indexing:constraint_classification(export_control_reversibility, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI SAFETY COALITION (SCAFFOLD) — Organized actors (AI safety researchers, international governance advocates) see current export controls as temporary coordination mechanism with implicit sunset: the goal is multilateral compute governance treaties, not unilateral US export policy. Current reversibility is a bug to be fixed through international coordination. Low effective extraction because coalition has agency and sees path to durable multilateral framework.
constraint_indexing:constraint_classification(export_control_reversibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NATIONAL SECURITY APPARATUS (TANGLED ROPE) — Benefits from export controls as strategic tool but constrained by industry lobbying and administration turnover. The coordination function (maintaining technological advantage) is genuine, but extraction occurs through policy whiplash: each administration shift forces recalibration of enforcement priorities, creating compliance costs and strategic uncertainty. The apparatus both wields and is constrained by the reversibility.
constraint_indexing:constraint_classification(export_control_reversibility, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, export control reversibility reflects genuine coordination problem (preventing compute-enabled authoritarianism) coupled with substantial extraction (policy instability, surveillance asymmetry, research disruption). The reversibility is structural: democratic policy processes allow course correction, but this same flexibility enables regulatory capture and creates planning uncertainty. Not a mountain (policy is contingent, not natural law) and not pure extraction (coordination function is real).
constraint_indexing:constraint_classification(export_control_reversibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(export_control_reversibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(export_control_reversibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(export_control_reversibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(export_control_reversibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(export_control_reversibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple channels: (1) surveillance asymmetry—dissidents bear costs of compute-enabled repression enabled during permissive windows, (2) research disruption—foreign institutions face planning uncertainty and program discontinuity, (3) policy whiplash—each administration shift forces recalibration. However, extraction is not maximal because coordination function is genuine: preventing military AI development and maintaining technological advantage are real security goals, not pure cover stories. Suppression (0.62): Moderate-high. Enforcement infrastructure includes export licensing requirements, end-use verification, entity lists, and extraterritorial reach of US semiconductor supply chain dominance. Barriers to alternative compute access are substantial but not total—some actors access compute through third countries, domestic production (China's semiconductor development), or cloud services. Suppression increased mid-interval (t4-t6) as enforcement tightened, then decreased slightly as industry lobbying created licensing loopholes. Theater ratio (0.48): Moderate. Licensing reviews involve genuine technical assessment of compute capabilities and end-use risk, but also political theater: 'national security' framing often obscures market-access motivations, and enforcement priorities shift with lobbying pressure rather than threat assessment. Theater increased mid-interval as gap between stated security rationale and actual licensing decisions widened, then stabilized as enforcement patterns became predictable.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival divergence driven by structural position. Dissidents see pure extraction (Snare)—they are trapped by surveillance infrastructure enabled during permissive export windows, and the reversibility is asymmetric (easy to enable repression, hard to disable). Foreign research institutions see mixed coordination and extraction (Tangled Rope)—the security goal is legitimate but blunt policy instruments create substantial collateral damage. Semiconductor manufacturers see coordination (Rope)—export controls create regulatory framework that protects domestic market position. AI safety coalition sees temporary coordination mechanism (Scaffold)—current unilateral controls are transitional step toward multilateral compute governance. National security apparatus sees mixed coordination and extraction (Tangled Rope)—wields strategic tool but constrained by industry lobbying and administration turnover. Analytical observer sees tangled_rope at civilizational scale—genuine coordination problem coupled with substantial extraction from policy instability and surveillance asymmetry. The gap reveals that 'export control reversibility' is not a single phenomenon but a presheaf over observation sites: extraction for the powerless, coordination for the powerful, and transition for the organized.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Dissidents in client states are victims with trapped exit—they experience maximum extraction (d→1.0). Foreign research institutions are victims with constrained exit—they experience high but not maximal extraction (d→0.7-0.8). Semiconductor manufacturers are beneficiaries with arbitrage exit—they experience low or negative extraction (d→0.1-0.2), as the constraint creates market segmentation that benefits them. National security apparatus is in mixed position: benefits from strategic tool but constrained by industry lobbying, yielding moderate directionality (d→0.4-0.5). AI safety coalition is organized with mobile exit—they experience low extraction (d→0.2-0.3) because they have agency and see path to multilateral framework. The analytical observer recognizes the coordination function but also sees the extraction mechanisms, yielding moderate directionality (d→0.4-0.5). No overrides needed—the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled_rope is the structurally correct classification from most perspectives, including the analytical observer. The coordination function (preventing compute-enabled authoritarianism, maintaining technological advantage) is genuine—this is not a snare masquerading as coordination. But extraction is substantial: surveillance asymmetry (dissidents bear costs of infrastructure enabled during permissive windows), research disruption (foreign institutions face planning uncertainty), and policy whiplash (administration shifts force recalibration). The reversibility itself is the extraction mechanism: democratic policy processes allow course correction, but this same flexibility enables regulatory capture and creates planning uncertainty. The scaffold perspective (AI safety coalition) is also structurally valid—organized actors with agency see path to multilateral framework that would reduce reversibility. The snare perspective (dissidents) is their genuine structural reality—they are trapped by asymmetric harm. No single type is 'the' answer; the presheaf over observation sites captures the full structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    surveillance_infrastructure_persistence,
    'Does surveillance infrastructure deployed during permissive export windows persist functionally after controls tighten, or does it degrade without ongoing hardware refresh?',
    'Longitudinal tracking of surveillance system capabilities in client states before/after export control tightening; technical analysis of hardware refresh requirements for facial recognition, social credit systems, and network monitoring',
    'If infrastructure persists: export control reversibility creates asymmetric harm (easy to enable repression, hard to disable). If infrastructure degrades: reversibility is symmetric and extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_infrastructure_persistence, empirical, 'Persistence of surveillance infrastructure after export controls tighten').

omega_variable(
    multilateral_coordination_feasibility,
    'Is multilateral compute governance achievable within a generational timeframe, or is unilateral export control the durable equilibrium?',
    'Analysis of historical multilateral technology control regimes (Wassenaar Arrangement, MTCR, NPT); assessment of current US-EU-China coordination attempts; game-theoretic modeling of defection incentives',
    'If multilateral coordination is feasible: scaffold perspective is structurally correct and current reversibility is transitional. If infeasible: scaffold is aspirational and tangled_rope is the durable state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_coordination_feasibility, empirical, 'Feasibility of multilateral compute governance within generational timeframe').

omega_variable(
    industry_capture_threshold,
    'At what level of industry lobbying pressure does export control policy shift from security-driven to market-access-driven?',
    'Quantitative analysis of lobbying expenditure vs policy outcomes; identification of administration transitions where control relaxation correlated with industry pressure rather than threat assessment changes; comparison of stated security rationale vs actual licensing decisions',
    'If threshold is low: extraction from regulatory capture is high and coordination function is compromised. If threshold is high: coordination function is robust and extraction is primarily from policy uncertainty rather than capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_capture_threshold, empirical, 'Industry lobbying threshold for policy capture').

omega_variable(
    legitimate_research_collateral_damage,
    'What proportion of foreign compute access restrictions affect legitimate research vs dual-use or military applications?',
    'Analysis of denied export licenses by stated end-use; tracking of research program disruptions in allied vs adversary nations; comparison of licensing criteria vs actual military AI development pathways',
    'If collateral damage is high (>40%): extraction from blunt policy instruments is substantial and tangled_rope classification is confirmed. If low (<20%): coordination function is precise and rope classification from more perspectives is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_research_collateral_damage, empirical, 'Proportion of export restrictions affecting legitimate research').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(export_control_reversibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(export_rev_theater_t0, export_control_reversibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(export_rev_theater_t2, export_control_reversibility, theater_ratio, 2, 0.42).
narrative_ontology:measurement(export_rev_theater_t4, export_control_reversibility, theater_ratio, 4, 0.48).
narrative_ontology:measurement(export_rev_theater_t6, export_control_reversibility, theater_ratio, 6, 0.52).
narrative_ontology:measurement(export_rev_theater_t8, export_control_reversibility, theater_ratio, 8, 0.48).
narrative_ontology:measurement(export_rev_theater_t10, export_control_reversibility, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(export_rev_extract_t0, export_control_reversibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(export_rev_extract_t2, export_control_reversibility, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(export_rev_extract_t4, export_control_reversibility, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(export_rev_extract_t6, export_control_reversibility, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(export_rev_extract_t8, export_control_reversibility, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(export_rev_extract_t10, export_control_reversibility, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(export_rev_suppress_t0, export_control_reversibility, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(export_rev_suppress_t2, export_control_reversibility, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(export_rev_suppress_t4, export_control_reversibility, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(export_rev_suppress_t6, export_control_reversibility, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(export_rev_suppress_t8, export_control_reversibility, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(export_rev_suppress_t10, export_control_reversibility, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(export_control_reversibility, enforcement_mechanism).
narrative_ontology:affects_constraint(export_control_reversibility, ai_safety_coordination_fragility).
narrative_ontology:affects_constraint(export_control_reversibility, semiconductor_supply_chain_concentration).

% DUAL FORMULATION NOTE:
% Export control reversibility is downstream of compute_constraint_as_brake (the upstream mountain constraint that compute is a fundamental brake on AI capability). The upstream constraint establishes that compute access matters; this constraint addresses the policy reversibility of compute access restrictions. The two constraints have different ε values: compute_constraint_as_brake has negligible extraction (it is a natural law), while export_control_reversibility has substantial extraction (it is a contingent policy instrument subject to capture and whiplash).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
