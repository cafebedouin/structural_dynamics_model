% ============================================================================
% CONSTRAINT STORY: hardware_software_inversion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hardware_software_inversion, []).

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
 *   constraint_id: hardware_software_inversion
 *   human_readable: Hardware-Software Inversion in Autonomous Vehicle Deployment
 *   domain: technology_governance/autonomous_vehicles/platform_economics
 *
 * SUMMARY:
 *   The hardware-software inversion in autonomous vehicle deployment
 *   represents a structural reversal of traditional product development:
 *   manufacturing vehicles with no manual fallback controls before the
 *   autonomous software is validated for unsupervised operation. Tesla's
 *   Cybercab production start (Feb 2026) precedes the earliest plausible
 *   validation timeline for Full Self-Driving unsupervised capability
 *   (mid-2027, based on July 2026 data threshold plus 12-month validation
 *   window). This creates a 12-18 month gap where customers possess vehicles
 *   that cannot be legally or safely operated without supervision, but lack
 *   the hardware (steering wheel, pedals) to provide that supervision. The
 *   constraint exhibits high extraction (0.68) because it transfers technical
 *   and safety risk from manufacturer to customer while capturing revenue and
 *   market valuation before capability is proven. Suppression is high (0.72)
 *   because customers have limited exit options once purchase is made,
 *   regulatory frameworks lag deployment timelines, and information asymmetry
 *   prevents informed consent. Theater ratio (0.65) reflects that regulatory
 *   oversight is substantially performative: agencies issue voluntary
 *   guidelines and advisory committees but lack enforcement mechanisms or
 *   validation capacity. The constraint's temporal trajectory shows
 *   increasing extraction and theater as the deployment-validation gap widens
 *   and regulatory capture deepens.
 *
 * KEY AGENTS:
 *   - Early Fleet Customers: Primary victim (powerless/trapped) — purchased vehicles with no manual controls based on promised timeline; bear full safety and usability risk during validation gap with no exit option
 *   - Public Road Safety: Secondary victim (powerless/trapped) — abstract collective good bearing externalized risk of unvalidated systems in mixed traffic; cannot organize or exit exposure
 *   - Tesla Shareholders (Short-Term): Primary beneficiary (institutional/arbitrage) — capture revenue and valuation before validation; can exit before timeline clarity emerges
 *   - Executive Compensation Structure: Secondary beneficiary (institutional/arbitrage) — stock-based compensation tied to production milestones rather than validation milestones; incentivizes hardware-first strategy
 *   - Later Adopters: Mixed position (moderate/constrained) — benefit from early adopter-funded development but face infrastructure lock-in and social pressure; can delay purchase at cost of reduced mobility options
 *   - Regulatory Agency (Constrained): Institutional victim (institutional/constrained) — faces coordination problem and industry capture; cannot fully exit oversight role but lacks validation capacity
 *   - Captured Regulator: Institutional victim (institutional/identity_locked) — identity fused with enabling domestic industry; maintains performative oversight while actual capacity atrophies
 *   - Open Autonomy Standards Coalition: Organized agents (organized/mobile) — building transparent validation frameworks; see inversion as temporary market failure with regulatory sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies hybrid coordination-extraction structure; genuine technical benefits coupled with unjustified risk transfer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hardware_software_inversion, 0.68).
domain_priors:suppression_score(hardware_software_inversion, 0.72).
domain_priors:theater_ratio(hardware_software_inversion, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hardware_software_inversion, extractiveness, 0.68).
narrative_ontology:constraint_metric(hardware_software_inversion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hardware_software_inversion, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hardware_software_inversion, snare).
narrative_ontology:human_readable(hardware_software_inversion, "Hardware-Software Inversion in Autonomous Vehicle Deployment").
narrative_ontology:topic_domain(hardware_software_inversion, "technology_governance/autonomous_vehicles/platform_economics").

domain_priors:requires_active_enforcement(hardware_software_inversion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hardware_software_inversion, tesla_shareholders_short_term).
narrative_ontology:constraint_beneficiary(hardware_software_inversion, executive_compensation_structure).
narrative_ontology:constraint_victim(hardware_software_inversion, early_fleet_customers).
narrative_ontology:constraint_victim(hardware_software_inversion, public_road_safety).
narrative_ontology:constraint_victim(hardware_software_inversion, regulatory_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY FLEET CUSTOMER (SNARE) — Purchased vehicle with no manual controls based on promised autonomous capability timeline. Cannot exit: vehicle delivered, payment made, no steering wheel to fall back on. Bears full safety and usability risk during the 12-18 month validation gap. Maximum extraction: paid for autonomous capability that does not yet exist, with no alternative use mode.
constraint_indexing:constraint_classification(hardware_software_inversion, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC ROAD SAFETY (SNARE) — Abstract collective good with no advocate and no exit. Bears externalized risk of unvalidated autonomous systems operating in mixed traffic. Cannot organize or escape exposure. The inversion creates a validation-by-deployment pattern where the public becomes involuntary beta testers.
constraint_indexing:constraint_classification(hardware_software_inversion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: LATER ADOPTER (TANGLED ROPE) — Can observe early fleet performance before purchase decision. Benefits from accelerated autonomous capability development funded by early adopters, but faces social pressure and infrastructure lock-in (charging networks, service centers). Constrained exit: can choose not to buy, but at cost of reduced mobility options in autonomous-optimized infrastructure.
constraint_indexing:constraint_classification(hardware_software_inversion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TESLA SHAREHOLDERS SHORT-TERM (ROPE) — Primary beneficiary. Hardware-first strategy captures revenue and market valuation before software validation, transferring technical risk to customers. Arbitrage exit: can sell shares before validation timeline becomes clear. Experience the constraint as coordination: aggressive timeline claims coordinate market expectations and capital allocation.
constraint_indexing:constraint_classification(hardware_software_inversion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCY (TANGLED ROPE) — Faces coordination problem (need to enable innovation) and extraction (industry pressure, revolving door dynamics, information asymmetry). Constrained exit: cannot fully withdraw from oversight role, but also cannot effectively verify claims before deployment. Benefits from industry data sharing but bears reputational cost of validation failures.
constraint_indexing:constraint_classification(hardware_software_inversion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OPEN AUTONOMY STANDARDS COALITION (SCAFFOLD) — Organized actors (SAE, ISO, academic consortia) building transparent validation frameworks and open testing protocols. See the inversion as a temporary market failure with sunset logic: as validation standards mature and regulatory frameworks catch up, hardware-first deployment without validated software becomes legally and commercially unviable. Mobile exit: can shift focus to other autonomy domains if automotive regulation fails.
constraint_indexing:constraint_classification(hardware_software_inversion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: CAPTURED REGULATOR (PITON) — Regulatory agency whose identity has fused with enabling the domestic autonomous vehicle industry. Maintains performative oversight (press releases, advisory committees, voluntary guidelines) while actual validation capacity has atrophied. Identity-locked: cannot exit the enabling posture without abandoning institutional mission as constructed. Theater ratio high: oversight ritual persists through inertia and industry capture, not function.
constraint_indexing:constraint_classification(hardware_software_inversion, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the inversion exhibits both genuine coordination function (hardware-first enables faster iteration, larger training dataset, real-world edge cases) AND asymmetric extraction (risk transfer to customers, externalization to public, regulatory arbitrage). The coordination function is real but does not justify the extraction magnitude or suppression level. Tangled Rope classification reflects that this is neither pure innovation (Rope) nor pure rent-seeking (Snare) but a hybrid where legitimate technical strategy is coupled with extractive risk transfer.
constraint_indexing:constraint_classification(hardware_software_inversion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hardware_software_inversion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hardware_software_inversion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hardware_software_inversion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hardware_software_inversion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hardware_software_inversion, TR),
    TR >= 0.70.

:- end_tests(hardware_software_inversion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The inversion captures customer payments and market valuation 12-18 months before capability is validated, transferring technical risk, safety risk, and opportunity cost to customers. The extraction is not total (0.90+) because there is a genuine coordination function: hardware deployment enables real-world data collection that accelerates capability development. But the magnitude of risk transfer and the information asymmetry (customers cannot assess true timeline) justify high extraction. Suppression (0.72): High. Once vehicle is purchased and delivered, customer exit options are severely limited: no manual controls mean no fallback use mode, resale market is thin for unvalidated autonomous vehicles, and switching costs are high (capital loss, infrastructure lock-in). Regulatory frameworks lag deployment, creating information asymmetry and preventing informed consent. But suppression is not total (0.90+) because some customers can delay purchase, and organized advocacy (consumer protection groups, safety organizations) can apply pressure. Theater ratio (0.65): Moderate-high. Regulatory oversight is substantially performative: voluntary guidelines, advisory committees, and press releases create appearance of validation without enforcement mechanisms. Agencies lack technical capacity to verify autonomous capability claims and face revolving-door capture. But theater is not total (0.90+) because some genuine oversight exists (crash investigation, recall authority) and standards coalitions are building alternative validation frameworks. The temporal trajectory shows all three metrics increasing as deployment scales, validation delays accumulate, and regulatory capture deepens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence driven by structural position and exit options. Early fleet customers and public road safety see pure extraction (Snare) — they bear risk with no exit and minimal benefit. Short-term shareholders see coordination (Rope) — the hardware-first strategy solves their capital allocation problem and they can exit before validation clarity. Later adopters see mixed coordination-extraction (Tangled Rope) — they benefit from accelerated development but face lock-in costs. The constrained regulatory agency also sees Tangled Rope — genuine coordination problem (enabling innovation) coupled with extraction (industry capture, information asymmetry). The captured regulator sees degraded ritual (Piton) — oversight persists through inertia and identity fusion, not function. The open standards coalition sees temporary problem with sunset (Scaffold) — regulatory maturation will make hardware-first deployment unviable. The analytical observer sees Tangled Rope at civilizational scale — genuine technical coordination function (real-world data collection) coupled with unjustified extraction magnitude (risk transfer, information asymmetry). The gap is not 'which type is correct?' but 'which structural position are you measuring from?' The presheaf over observation sites IS the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Early fleet customers are primary victims with trapped exit options, yielding high directionality (d ≈ 0.95) and maximum experienced extraction. They paid for capability that does not yet exist and cannot exit once delivery occurs. Public road safety is an abstract collective victim with no exit, also yielding maximum directionality. Tesla shareholders (short-term) are primary beneficiaries with arbitrage exit, yielding low directionality (d ≈ 0.05) and negative experienced extraction — they capture value before validation and can exit before timeline clarity. Executive compensation structure is a secondary beneficiary with similar low directionality. Later adopters are mixed: they benefit from early adopter-funded development (beneficiary component) but face infrastructure lock-in (victim component), with constrained exit options yielding moderate directionality (d ≈ 0.55). The constrained regulatory agency is an institutional victim with moderate directionality (d ≈ 0.40) — faces extraction through industry pressure and information asymmetry but retains some agency. The captured regulator is identity-locked (d ≈ 0.89) — structurally could exit the enabling posture but cannot do so without abandoning institutional identity as constructed. The analytical observer uses canonical analytical directionality (d ≈ 0.73), experiencing the constraint as a structural phenomenon to be classified rather than as direct extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY ANALYSIS: The constraint risks mandatrophy collapse if the coordination function (hardware-first enables faster capability development through real-world data) is used to justify the extraction magnitude (risk transfer to customers and public). The mandatrophy question is: 'Is real-world fleet deployment necessary for validation, or is it a strategy to externalize testing costs?' Omega variable fleet_learning_necessity directly addresses this. If alternative validation pathways exist (simulation, closed-course testing, geofenced deployment), then the coordination function is weaker and the extraction is less justified — the constraint moves toward pure Snare. If fleet deployment provides data that cannot be obtained through lower-risk methods, then the coordination function is stronger and Tangled Rope classification is appropriate. The analytical perspective's Tangled Rope classification reflects this ambiguity: the constraint exhibits BOTH genuine coordination (real-world edge cases, diverse conditions, scale) AND asymmetric extraction (risk externalization, information asymmetry, regulatory arbitrage). The resolution mechanism is empirical: compare capability development timelines and validation quality across deployment strategies. Current evidence suggests mixed: some real-world data is necessary (long-tail edge cases), but the magnitude of deployment (tens of thousands of vehicles) and the elimination of manual controls (no fallback) exceed what coordination requires. The excess is extraction. The mandatrophy is resolved by recognizing that coordination function existence does not justify extraction magnitude — the Tangled Rope classification captures this precisely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    validation_timeline_slippage,
    'What is the actual timeline from hardware deployment to validated unsupervised capability, and how does slippage affect the extraction magnitude?',
    'Empirical tracking of FSD capability milestones, regulatory approval dates, and intervention rate data. Compare promised timeline (mid-2027) against actual validation dates.',
    'If validation occurs by mid-2027: extraction window is 12-18 months as modeled. If validation slips to 2029+: extraction magnitude increases significantly, potentially crossing into pure Snare territory for all perspectives. If validation never occurs: hardware is stranded asset, total extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validation_timeline_slippage, empirical, 'Actual validation timeline vs promised timeline').

omega_variable(
    intervention_rate_threshold,
    'What intervention rate threshold distinguishes ''unsupervised'' capability from ''supervised with low intervention''? Is the threshold technical or regulatory?',
    'Comparison of industry claims, regulatory definitions (NHTSA, CPSC), and insurance actuarial standards. Identify divergence between marketing claims and legal/safety thresholds.',
    'If threshold is permissive (e.g., 1 intervention per 100 miles): validation timeline shortens, extraction window narrows. If threshold is strict (e.g., 1 intervention per 10,000 miles, comparable to human driver): validation timeline extends, extraction increases. Regulatory ambiguity enables threshold gaming.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_rate_threshold, conceptual, 'Definition of ''unsupervised'' capability threshold').

omega_variable(
    liability_allocation_ambiguity,
    'Who bears liability for accidents during the validation gap: manufacturer, customer, or distributed across insurance pool?',
    'Analysis of terms of service, insurance policy language, and case law development. Track whether courts treat customers as beta testers (assumption of risk) or consumers (product liability).',
    'If liability stays with manufacturer: extraction is partially internalized, reducing net extraction to customers. If liability transfers to customers: extraction magnitude increases. If distributed to insurance pool: externalization to third parties, increasing total extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_allocation_ambiguity, empirical, 'Liability allocation during validation gap').

omega_variable(
    fleet_learning_necessity,
    'Is real-world fleet deployment actually necessary for validation, or is it a strategy to externalize testing costs?',
    'Comparison with alternative validation pathways: simulation environments, closed-course testing, geofenced deployment. Assess whether hardware-first deployment provides data that cannot be obtained through lower-risk methods.',
    'If fleet deployment is necessary: coordination function is stronger, Tangled Rope classification more appropriate. If alternative pathways exist: coordination function is weaker, extraction is less justified, Snare classification more appropriate. This omega directly affects the mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fleet_learning_necessity, empirical, 'Necessity of real-world fleet deployment for validation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hardware_software_inversion, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hwsw_inv_theater_2024, hardware_software_inversion, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hwsw_inv_theater_2025, hardware_software_inversion, theater_ratio, 12, 0.55).
narrative_ontology:measurement(hwsw_inv_theater_2026, hardware_software_inversion, theater_ratio, 24, 0.65).

% Extraction over time
narrative_ontology:measurement(hwsw_inv_extract_2024, hardware_software_inversion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hwsw_inv_extract_2025, hardware_software_inversion, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(hwsw_inv_extract_2026, hardware_software_inversion, base_extractiveness, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hardware_software_inversion, global_infrastructure).
narrative_ontology:affects_constraint(hardware_software_inversion, regulatory_capture_autonomous_vehicles).
narrative_ontology:affects_constraint(hardware_software_inversion, liability_shield_beta_testing).
narrative_ontology:affects_constraint(hardware_software_inversion, infrastructure_lock_in_charging_networks).

% DUAL FORMULATION NOTE:
% The hardware-software inversion is structurally distinct from but causally linked to regulatory capture (affects regulatory agency perspectives), liability allocation ambiguity (affects customer risk exposure), and infrastructure lock-in (affects later adopter exit options). Each linked constraint has its own extractiveness value reflecting its specific mechanism. The inversion is the upstream constraint that creates the structural conditions for the downstream extraction patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hardware_software_inversion, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
