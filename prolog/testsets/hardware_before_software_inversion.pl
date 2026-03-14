% ============================================================================
% CONSTRAINT STORY: hardware_before_software_inversion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hardware_before_software_inversion, []).

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
 *   constraint_id: hardware_before_software_inversion
 *   human_readable: Hardware-Before-Software Inversion in Autonomous Vehicle Production
 *   domain: technology_governance/automotive_industry/autonomous_systems
 *
 * SUMMARY:
 *   The hardware-before-software inversion in autonomous vehicle production
 *   represents a structural reversal of traditional product development:
 *   manufacturing and selling vehicles whose core promised function
 *   (autonomous operation) does not yet exist at claimed safety levels, with
 *   no manual fallback for that function. Tesla began producing vehicles with
 *   'Full Self-Driving' hardware in 2016, selling the FSD software package
 *   ($8k-$15k) as a future capability. As of February 2026, FSD crash rates
 *   remain approximately 4x human baseline, and independent safety
 *   certification does not exist. The constraint exhibits high extraction
 *   (0.68) because early adopters paid for a capability that may never
 *   materialize at safe levels, bearing both financial loss and crash risk.
 *   Suppression is high (0.72) because exit options are blocked: vehicle
 *   depreciation, sunk FSD costs, contractual arbitration clauses, and no
 *   refund mechanism. Theater ratio is high (0.78) because the regulatory
 *   framework (NHTSA voluntary reporting) provides appearance of oversight
 *   without enforcement, and the 'beta' label on FSD creates legal cover
 *   while implying imminent completion. The constraint's measurements show
 *   monotonic increase in both theater and extraction over the 2020-2026
 *   interval as the gap between promise and delivery has widened rather than
 *   closed.
 *
 * KEY AGENTS:
 *   - Early Adopters: Primary victims (powerless/trapped) — paid for non-existent capability, bear crash risk, cannot exit due to sunk costs and contractual barriers
 *   - Regulatory Agencies: Secondary victims (moderate/constrained) — NHTSA and state DMVs face asymmetric information, high evidentiary bars for intervention, political pressure against 'stifling innovation'
 *   - Tesla Shareholders (Short-Term): Primary beneficiaries (institutional/arbitrage) — capture gains from production volume and market valuation based on promised future capability, can exit at any time
 *   - Tesla Shareholders (Long-Term): Mixed position (institutional/constrained) — benefit from near-term revenue but bear long-term liability risk if FSD never achieves safety parity
 *   - Competing Manufacturers: Mixed position (moderate/constrained) — face competitive pressure to adopt hardware-before-software model but also bear reputational cost of degraded safety standards
 *   - Safety Advocacy Coalition: Organized agents (organized/mobile) — Consumer Reports, IIHS, trial lawyers see temporary regulatory gap with sunset (either FSD achieves safety or litigation collapses the model)
 *   - Public Road Safety: Abstract victim (powerless/trapped) — collective good that cannot organize or exit; bears crash risk from all autonomous system deployments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hardware_before_software_inversion, 0.68).
domain_priors:suppression_score(hardware_before_software_inversion, 0.72).
domain_priors:theater_ratio(hardware_before_software_inversion, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hardware_before_software_inversion, extractiveness, 0.68).
narrative_ontology:constraint_metric(hardware_before_software_inversion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hardware_before_software_inversion, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hardware_before_software_inversion, snare).
narrative_ontology:human_readable(hardware_before_software_inversion, "Hardware-Before-Software Inversion in Autonomous Vehicle Production").
narrative_ontology:topic_domain(hardware_before_software_inversion, "technology_governance/automotive_industry/autonomous_systems").

domain_priors:requires_active_enforcement(hardware_before_software_inversion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hardware_before_software_inversion, tesla_shareholders_short_term).
narrative_ontology:constraint_beneficiary(hardware_before_software_inversion, executive_compensation_structure).
narrative_ontology:constraint_victim(hardware_before_software_inversion, early_adopters).
narrative_ontology:constraint_victim(hardware_before_software_inversion, regulatory_agencies).
narrative_ontology:constraint_victim(hardware_before_software_inversion, public_road_safety).
narrative_ontology:constraint_victim(hardware_before_software_inversion, competing_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY ADOPTER (SNARE) — Purchased vehicle based on promised autonomous capability (FSD) that does not exist at claimed safety levels. Cannot exit: vehicle depreciation, sunk cost of $8k-$15k FSD package, no refund mechanism, contractual arbitration clauses prevent class action. Bears full crash risk (currently 4x human baseline) with no manual fallback for core promised function. Maximum extraction: paid for capability that may never materialize at safe levels.
constraint_indexing:constraint_classification(hardware_before_software_inversion, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATORY AGENCY (SNARE) — NHTSA and state DMVs face asymmetric information: manufacturer controls crash data, software updates occur without approval, and regulatory frameworks assume human-driven vehicles. Constrained exit: cannot ban vehicles already sold, cannot force recalls without proving imminent hazard (high evidentiary bar), political pressure from innovation narrative. Experiences extraction through enforcement costs, crash investigation burden, and reputational damage when failures occur under their watch.
constraint_indexing:constraint_classification(hardware_before_software_inversion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPETING MANUFACTURER (TANGLED ROPE) — Traditional automakers (GM, Ford, Mercedes) face coordination problem: if one manufacturer ships hardware-before-software and captures market share, others must follow or lose competitive position. But also victims: the inversion normalizes unsafe deployment practices, creating regulatory race-to-bottom pressure and reputational contamination when any autonomous system fails. Mixed extraction: benefits from accelerated autonomous vehicle adoption timeline, but bears cost of degraded safety standards and public trust erosion.
constraint_indexing:constraint_classification(hardware_before_software_inversion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: SHAREHOLDER SHORT-TERM (ROPE) — Benefits from production volume, revenue recognition, and market valuation based on promised future capability rather than current function. Arbitrage exit: can sell shares at any time, capturing gains from the promise without bearing the liability risk. Experiences constraint as coordination: the hardware-before-software model solves the capital efficiency problem of R&D-then-production by monetizing the development process itself. Low effective extraction because extraction flows toward this agent.
constraint_indexing:constraint_classification(hardware_before_software_inversion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SHAREHOLDER LONG-TERM (TANGLED ROPE) — Institutional investors and index funds cannot easily exit (constrained by portfolio requirements, fiduciary duty, market impact of large sales). Experience mixed extraction: benefit from near-term revenue and valuation, but bear long-term liability risk if FSD never achieves safety parity and mass tort litigation materializes. The inversion creates genuine coordination value (capital efficiency) but also asymmetric risk distribution (liability concentrates as time passes without safety achievement).
constraint_indexing:constraint_classification(hardware_before_software_inversion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SAFETY ADVOCACY COALITION (SCAFFOLD) — Consumer Reports, IIHS, trial lawyers, and insurance actuaries see the inversion as a temporary regulatory gap with a sunset: either FSD achieves safety parity (resolving the extraction) or mass litigation and regulatory intervention force recall/refund (collapsing the model). Organized agents with mobile exit (can shift advocacy focus) experience low extraction because they see a resolution path. The constraint is temporary because the hardware-before-software model is unsustainable — the gap between promise and delivery must close or the model collapses.
constraint_indexing:constraint_classification(hardware_before_software_inversion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: NHTSA VOLUNTARY REPORTING (PITON) — The existing autonomous vehicle regulatory framework is largely theatrical: manufacturers self-report crashes, no pre-deployment safety certification required, software updates bypass approval. The framework persists through institutional inertia despite providing minimal actual safety verification. High theater ratio: the reporting ritual creates appearance of oversight without enforcement mechanism. NHTSA sees its own process as degraded but cannot exit (constrained by statutory authority limits and political pressure to avoid 'stifling innovation').
constraint_indexing:constraint_classification(hardware_before_software_inversion, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the hardware-before-software inversion represents a genuine innovation in capital-efficient R&D (coordination function: monetize development process, distribute risk across early adopters who accept it). But it also represents asymmetric extraction: the model transfers safety risk and financial loss to buyers while concentrating gains with manufacturers and short-term shareholders. The analytical classification is Tangled Rope because both functions are structurally real — this is not pure extraction (snare) nor naturalized law (false mountain), but a hybrid where coordination and extraction are inseparable.
constraint_indexing:constraint_classification(hardware_before_software_inversion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hardware_before_software_inversion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hardware_before_software_inversion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hardware_before_software_inversion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hardware_before_software_inversion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hardware_before_software_inversion, TR),
    TR >= 0.70.

:- end_tests(hardware_before_software_inversion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Early adopters paid $8k-$15k for FSD capability that does not exist at safe levels (4x human crash rate as of Feb 2026). The financial extraction is compounded by crash risk transfer: buyers bear the safety cost of an incomplete system. The value reflects that this is not maximum extraction (buyers did receive a vehicle with some autonomous features, even if not at promised safety levels) but is severe extraction (core promised function may never materialize safely). Suppression (0.72): High. Exit barriers include: vehicle depreciation (cannot sell without loss), sunk FSD cost (no refund mechanism), contractual arbitration clauses (prevent class action), and information asymmetry (manufacturer controls crash data and software updates). Regulatory exit is also suppressed: NHTSA cannot force recall without proving imminent hazard (high evidentiary bar), and political pressure frames intervention as 'stifling innovation'. Theater ratio (0.78): High. Multiple theatrical elements: (1) NHTSA voluntary reporting framework provides appearance of oversight without enforcement, (2) 'beta' label on FSD creates legal cover while implying imminent completion (has been in beta for 6+ years), (3) over-the-air updates create perception of continuous improvement without independent safety certification, (4) crash reporting is manufacturer-controlled and selective. The theater has increased over time as the gap between promise and delivery has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Early adopters experience pure extraction (snare) — they are trapped in a vehicle whose core promised function may never safely exist. Short-term shareholders experience coordination (rope) — the hardware-before-software model solves capital efficiency problems and they can exit at any time. Competing manufacturers experience mixed coordination and extraction (tangled rope) — they face competitive pressure to adopt the model but also bear reputational costs. The safety advocacy coalition sees a temporary problem with a sunset (scaffold) — either FSD achieves safety or litigation collapses the model. Regulatory agencies experience extraction (snare) but from a constrained rather than trapped position. The analytical observer sees tangled rope — both the coordination function (capital efficiency) and the extraction (risk transfer) are structurally real and inseparable. The gap reveals that 'innovation' framing naturalizes what is actually asymmetric risk distribution: those who capture gains (shareholders) can exit, while those who bear risks (buyers, public) cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters are victims with trapped exit options, yielding high d (0.92) and maximum experienced extraction. They paid for a capability that does not exist at safe levels and cannot exit due to sunk costs and contractual barriers. Regulatory agencies are victims with constrained exit (cannot easily ban or recall), yielding moderately high d (0.78). They bear enforcement costs and reputational damage but have some agency. Competing manufacturers are mixed: victims of competitive pressure and reputational contamination, but also beneficiaries of accelerated autonomous adoption timeline. Their constrained exit and mixed position yields moderate d (0.55). Short-term shareholders are beneficiaries with arbitrage exit, yielding low d (0.08) and negative experienced extraction — they capture gains from the promise without bearing liability risk. Long-term shareholders are beneficiaries with constrained exit (cannot easily sell large positions), yielding low-moderate d (0.28) — they benefit from revenue but bear long-term liability risk. The safety advocacy coalition is organized with mobile exit, yielding moderate d (0.48) — they see a resolution path (either safety achievement or litigation collapse) and can shift focus if needed. The analytical observer sees both coordination function (capital-efficient R&D model) and extraction (asymmetric risk transfer), yielding moderate d (0.58) for the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the hardware-before-software inversion is neither pure coordination (it transfers crash risk and financial loss to buyers) nor pure extraction (it does solve genuine capital efficiency problems in R&D-intensive industries). The analytical classification is tangled rope because both functions are structurally real. The coordination function: monetizing the development process distributes R&D costs across early adopters who accept the risk, enabling faster iteration than traditional safety-first deployment. The extraction function: the model transfers safety risk to buyers while concentrating gains with manufacturers and short-term shareholders, with no mechanism to internalize liability if the promised capability never safely materializes. The perspectival gap is diagnostic: those with exit options (shareholders, safety advocates) see coordination or temporary problems; those without exit options (early adopters, public road safety) experience pure extraction. The mandatrophy resolution is that the constraint IS both things simultaneously — the presheaf over observation sites captures this irreducible multiplicity rather than forcing a single classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsd_safety_achievability,
    'Can Tesla FSD achieve human-parity safety (1x crash rate) within the vehicle lifetime of current production units (10-15 years)?',
    'Longitudinal crash rate data with statistical significance; independent safety certification; comparison to Waymo/Cruise geofenced performance extrapolated to general driving',
    'If achievable within 3-5 years: extraction window is temporary, scaffold perspective confirmed. If unachievable or requires 10+ years: snare classification hardens, early adopters bear permanent loss. If fundamentally unachievable with current sensor suite: constraint becomes fraud rather than extractive coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fsd_safety_achievability, empirical, 'Whether FSD can achieve safety parity within vehicle lifetime').

omega_variable(
    liability_distribution_mechanism,
    'When FSD-involved crashes occur, does liability fall on the driver (current legal default) or the manufacturer (product liability)?',
    'Tort litigation outcomes; insurance industry loss allocation; regulatory guidance on autonomous system liability',
    'If driver liability persists: extraction from early adopters is maximal (they paid for the system AND bear the crash risk). If manufacturer liability: extraction partially internalizes, changing shareholder risk profile and potentially forcing recall/refund.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_distribution_mechanism, preference, 'Legal liability allocation for autonomous system failures').

omega_variable(
    regulatory_capture_depth,
    'Is NHTSA''s voluntary reporting framework a temporary regulatory gap or evidence of deep capture preventing mandatory safety certification?',
    'Analysis of NHTSA-industry personnel flows; lobbying expenditure correlation with regulatory forbearance; comparison to EU autonomous vehicle regulatory stringency',
    'If temporary gap: scaffold perspective for regulatory framework is valid. If deep capture: the piton perspective hardens, and the constraint''s suppression score should be revised upward (regulatory exit is blocked, not just constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Whether regulatory forbearance is temporary or structural capture').

omega_variable(
    competitive_contagion_threshold,
    'At what market share does hardware-before-software deployment by one manufacturer force competing manufacturers to adopt the same model?',
    'Market share analysis; competitor deployment timeline correlation; investor pressure on traditional automakers to match Tesla''s capital efficiency',
    'If threshold is low (10-15% market share): the tangled rope perspective for competing manufacturers hardens — they are structurally coerced into the inversion. If threshold is high (30%+): competitors retain agency to pursue safety-first deployment, reducing their victim status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_contagion_threshold, empirical, 'Market share threshold for competitive deployment pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hardware_before_software_inversion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hbsi_theater_2020, hardware_before_software_inversion, theater_ratio, 0, 0.65).
narrative_ontology:measurement(hbsi_theater_2022, hardware_before_software_inversion, theater_ratio, 2, 0.7).
narrative_ontology:measurement(hbsi_theater_2024, hardware_before_software_inversion, theater_ratio, 4, 0.75).
narrative_ontology:measurement(hbsi_theater_2026, hardware_before_software_inversion, theater_ratio, 6, 0.78).

% Extraction over time
narrative_ontology:measurement(hbsi_extract_2020, hardware_before_software_inversion, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(hbsi_extract_2022, hardware_before_software_inversion, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(hbsi_extract_2024, hardware_before_software_inversion, base_extractiveness, 4, 0.64).
narrative_ontology:measurement(hbsi_extract_2026, hardware_before_software_inversion, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hardware_before_software_inversion, resource_allocation).
narrative_ontology:affects_constraint(hardware_before_software_inversion, regulatory_capture_nhtsa).
narrative_ontology:affects_constraint(hardware_before_software_inversion, liability_shield_beta_label).
narrative_ontology:affects_constraint(hardware_before_software_inversion, over_the_air_update_bypass).

% DUAL FORMULATION NOTE:
% The hardware-before-software inversion is upstream of several related constraints: regulatory capture at NHTSA (which enables the voluntary reporting framework), liability shielding via beta labels (which transfers crash risk to drivers), and over-the-air update regulatory bypass (which allows software changes without approval). Each of these constraints has its own extractiveness value, but they form a constraint family where the inversion is the structural foundation enabling the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hardware_before_software_inversion, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
