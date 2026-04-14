% ============================================================================
% CONSTRAINT STORY: regulatory_capture_autonomous_vehicles
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_autonomous_vehicles, []).

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
 *   constraint_id: regulatory_capture_autonomous_vehicles
 *   human_readable: Regulatory Capture in Autonomous Vehicle Governance
 *   domain: transportation/regulation/technology
 *
 * SUMMARY:
 *   Regulatory capture in autonomous vehicle governance creates a structural
 *   extraction mechanism where technology companies have systematized their
 *   influence over the regulatory agencies tasked with overseeing their
 *   safety. The constraint exhibits the classic features of institutional
 *   capture: manufacturers employ or fund the technical experts regulators
 *   depend on, control access to testing data and simulation results, and
 *   have moved standard-setting into private domains (internal testing,
 *   industry consortiums) while maintaining nominal compliance with public
 *   regulatory theater. Public safety oversight has become trapped: agencies
 *   lack independent testing capacity, lack technical personnel who are not
 *   trained by or indebted to manufacturers, and have internalized the
 *   industry's framing of 'safe enough' as the proper regulatory standard.
 *   The constraint is not a temporary coordination problem solved by clearer
 *   standards, but a systematic extraction that concentrates early benefits
 *   (market access, profit, venture returns) on manufacturers while
 *   distributing safety risk to the public, with particular burden on
 *   low-income populations who cannot opt out of autonomous vehicle exposure
 *   and liability bearers who bear legal costs of accidents. The
 *   theater_ratio rising from 0.35 to 0.75 reflects degradation: as
 *   regulatory capture deepens, the performative aspects of safety testing
 *   increase while genuine independent oversight decreases.
 *
 * KEY AGENTS:
 *   - Autonomous Vehicle Manufacturers: Primary beneficiary (institutional/arbitrage) — capture standard-setting, accelerate deployment, extract market value during regulatory uncertainty phase
 *   - Public Safety Oversight Agencies: Primary victim (powerless/trapped) — lack independent testing capacity, depend on manufacturer-provided data, personnel flow between agencies and companies, internalize captured standards as natural
 *   - Accident Victims and Liability Bearers: Primary victim (powerless/trapped) — distributed exposure to under-tested technology, bear medical and insurance costs, cannot opt out geographically or financially
 *   - Low-Income Commuters: Secondary victim (moderate/constrained) — benefit from potential autonomous ride-sharing but exposed to safety risk they cannot afford to avoid, concentrated in areas where manufacturers deploy cheapest solutions
 *   - Safety Advocacy Groups and Insurance Industry: Organized opposition (organized/constrained) — have resources to push back but lack technical expertise to challenge manufacturer claims, constrained by manufacturer dominance of research and talent
 *   - Regulators (as individual institutional actors): Captured institution (institutional/arbitrage-constrained duality) — experience regulatory capture as mixture of resource constraint (budget, staffing) and cognitive capture (absorbing manufacturer framing of safety adequacy) that blurs into identity fusion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — reveals the constraint as not inevitable coordination problem but systematic extraction enabled by information asymmetry and institutional capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_autonomous_vehicles, 0.58).
domain_priors:suppression_score(regulatory_capture_autonomous_vehicles, 0.62).
domain_priors:theater_ratio(regulatory_capture_autonomous_vehicles, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_autonomous_vehicles, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_autonomous_vehicles, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(regulatory_capture_autonomous_vehicles, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_autonomous_vehicles, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_autonomous_vehicles, "Regulatory Capture in Autonomous Vehicle Governance").
narrative_ontology:topic_domain(regulatory_capture_autonomous_vehicles, "transportation/regulation/technology").

domain_priors:requires_active_enforcement(regulatory_capture_autonomous_vehicles).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_autonomous_vehicles, autonomous_vehicle_manufacturers).
narrative_ontology:constraint_beneficiary(regulatory_capture_autonomous_vehicles, wealthy_early_adopters).
narrative_ontology:constraint_victim(regulatory_capture_autonomous_vehicles, public_safety_oversight).
narrative_ontology:constraint_victim(regulatory_capture_autonomous_vehicles, liability_bearers).
narrative_ontology:constraint_victim(regulatory_capture_autonomous_vehicles, low_income_commuters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC SAFETY OVERSIGHT (SNARE) — Cannot exit verification requirements; bears full cost of regulatory capture. Public agencies lack resources to conduct independent testing at scale required for autonomous vehicle certification. Suppression is structural: manufacturers control testing data, employ regulators' technical talent, and fund the academic expertise on which agencies depend. No alternatives exist for certifying vehicle safety at scale.
constraint_indexing:constraint_classification(regulatory_capture_autonomous_vehicles, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ACCIDENT VICTIMS AND LIABILITY BEARERS (SNARE) — Cannot opt out of exposure to autonomous vehicles. Trapped by geography, income, and lack of alternatives. Bear the cost of under-regulation through injury, death, and insurance premium increases. Suppression is total: no choice of exposure, no choice of vehicle, no voice in certification standards. Effective extraction through externalized safety costs.
constraint_indexing:constraint_classification(regulatory_capture_autonomous_vehicles, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LOW-INCOME COMMUTERS (TANGLED ROPE) — Constrained by transportation costs and availability. Benefit from potential autonomous vehicle deployment through lower-cost shared mobility. But also victimized by under-regulation that concentrates early benefits on wealthy adopters while distributing safety risk broadly. Mixed experience: genuine coordination function (solving transportation access) alongside asymmetric extraction (safety risk distributed to those who cannot afford to opt out).
constraint_indexing:constraint_classification(regulatory_capture_autonomous_vehicles, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AUTONOMOUS VEHICLE MANUFACTURERS (ROPE) — Experience constraint as pure coordination problem: need regulatory clarity to operate, build products to standards, and scale deployments. Extract significant value through regulatory arbitrage (capturing definition of 'safe enough') but frame this as solving a coordination problem. Beneficiary with maximum exit optionality: can relocate to permissive jurisdictions, can influence standards through technical expertise and lobbying, can exit through market dominance.
constraint_indexing:constraint_classification(regulatory_capture_autonomous_vehicles, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SAFETY ADVOCACY AND INSURANCE INDUSTRY (TANGLED ROPE) — Organized agents with constrained exit. Insurance companies benefit from autonomous vehicles (potential reduction in human-error accidents) while bearing liability risk from under-regulation. Safety advocates benefit from clarity and reduced uncertainty but are constrained by resource limits and manufacturer dominance of technical expertise. Both have agency but face significant barriers to enforcing higher standards.
constraint_indexing:constraint_classification(regulatory_capture_autonomous_vehicles, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY SAFETY REGULATORY REGIME (PITON) — Existing safety testing frameworks (NHTSA crash testing, pedestrian protection standards) persist as theater despite being designed for human-operated vehicles. Autonomous vehicle manufacturers initially comply nominally then lobby for exemptions. The regulatory apparatus maintains the theater of safety certification while actual gatekeeping power has shifted to manufacturers' internal testing. Performance of regulation persists longer than function.
constraint_indexing:constraint_classification(regulatory_capture_autonomous_vehicles, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Full structural view reveals genuine coordination function (autonomous vehicles require clear safety standards to deploy) coexisting with asymmetric extraction (manufacturers capture standard-setting while distributing safety risk to public). Extracted value flows from public safety oversight and liability bearers to manufacturers and early adopters. Suppression mechanism is structural and internalized: regulators accept manufacturers' safety claims partly from resource dependence, partly from cognitive capture (absorbing industry's framing of 'safe enough').
constraint_indexing:constraint_classification(regulatory_capture_autonomous_vehicles, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_autonomous_vehicles_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_autonomous_vehicles, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_autonomous_vehicles, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_autonomous_vehicles, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_autonomous_vehicles, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_autonomous_vehicles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over measurement interval. Initial value (0.25) reflects a genuine coordination function: manufacturers and regulators need to cooperate to establish safety standards for a novel technology. Current value (0.58) reflects captured regulatory standard-setting where manufacturers' preferred safety thresholds are accepted as objective requirements rather than negotiated points. The increase reflects accumulation of capture mechanisms: manufacturer influence over research funding, personnel flow between industry and agencies, normalization of manufacturer testing as sufficiency standard. Suppression (0.62): High. Structural suppression includes: public agencies lack independent testing capacity (expensive, specialized equipment); manufacturers control primary data and internal testing results; technical talent flows preferentially toward companies (better salaries, more interesting problems); regulatory agencies are chronically under-resourced relative to manufacturer engineering capacity. Internalized suppression: regulators have absorbed manufacturer framing that 'perfect is the enemy of good' and that 'safe enough for deployment' is a reasonable standard. Theater ratio (0.68): High and increasing. NHTSA crash testing standards were designed for human-operated vehicles; autonomous vehicle testing requires fundamentally different frameworks (scenario-based testing, simulation validation, edge-case performance). Instead of redesigning standards, agencies maintain nominal compliance theater (manufacturers run legacy tests) while actual gatekeeping has shifted into manufacturer internal testing and private consortium standards. The theater allows regulators to claim they are 'doing their job' while lacking capacity to verify actual safety.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival gap between manufacturer (Rope) and powerless agent (Snare) perspectives. Manufacturers see coordination: 'Standards enable deployment.' Powerless agents see extraction: 'Standards are set by extractors, enforcement is theater, risk is distributed to me.' The gap reveals the capture: if this were pure coordination, standards would be neutral and publicly defensible. The gap's existence indicates extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position. Manufacturers with arbitrage exit options (can relocate, can influence standards, can operate with minimal overhead) get low d → beneficiary experience. Public safety agencies with trapped exit (must certify, cannot refuse to regulate, cannot build independent capacity) get high d → victim experience. The captured regulator is the critical case: nominally they are institutional actors with arbitrage, but capture has partially converted their exit options from arbitrage to constrained. They retain some agency (can theoretically demand higher standards) but face material barriers (budget, technical capacity) and cognitive barriers (identity fusion with captured standards). Directionality override is not needed here because the derivation captures the duality: regulators appear to have arbitrage (they set standards) but actually face constrained exit (captured by resource and cognitive dependence). Low-income commuters are constrained by geography and income — cannot avoid autonomous vehicle exposure even if desired.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: This constraint cannot yet be classified as pure Snare (would require proving manufacturers deliberately hide unsafe technology and regulators knowingly allow it) or pure Rope (would require proving autonomous vehicles are genuinely safe and standards are adequately enforced). The constraint is tangled because: (1) genuine coordination function exists (autonomous vehicles do require safety standards); (2) genuine extraction mechanism exists (manufacturers capture standard definition and shift risk); (3) empirical ambiguity about whether autonomous vehicles are actually safer than human drivers remains unresolved (omega variable: safety_metric_sufficiency). Mandatrophy resolution requires: (a) empirical clarity on safety outcomes (are autonomous vehicles net-safer?), (b) clarity on whether regulators are merely constrained by resources or cognitively captured (identity-locked), and (c) clarity on whether capture is reversible (path-dependent lock-in or temporary imbalance). Current status: Tangled Rope pending empirical resolution of core safety claims. If autonomous vehicles prove net-safer: extraction mechanism is justified as coordination cost. If net-unsafe or safety claims prove theater: constraint becomes Snare. If regulators are identity-locked: constraint becomes more path-dependent and harder to reverse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_metric_sufficiency,
    'Do manufacturer-provided safety metrics (miles driven without incident, scenario-based test results, simulation outcomes) actually predict real-world safety performance, or do they constitute theater that creates false confidence?',
    'Longitudinal tracking of autonomous vehicle accident rates and safety outcomes against pre-deployment claimed metrics; analysis of failure modes that metrics failed to capture; comparison with safety predictions from manufacturer testing',
    'If metrics predictive: extraction is lower than measured (safety oversight has real coordination function). If metrics are theater: extraction is higher (captured regulators validate unproven safety claims for manufacturers'' benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_metric_sufficiency, empirical, 'Whether manufacturer safety metrics predict real-world safety outcomes').

omega_variable(
    regulatory_recapture_timeline,
    'Can meaningful independent oversight be re-established once regulatory capture is complete, or does the constraint become path-dependent and locked in?',
    'Historical analysis of regulatory recapture in other transportation domains (aviation, maritime, autonomous rail); examination of institutional friction costs to shifting governance back toward public oversight; assessment of manufacturer technical lock-in and political capital',
    'If recapture possible: constraint is temporary extraction (scaffold framing). If locked in: constraint is structural snare for powerless agents (public oversight permanently degraded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_recapture_timeline, conceptual, 'Whether regulatory capture can be reversed after establishment').

omega_variable(
    safety_improvement_direction,
    'Are autonomous vehicles actually safer than human drivers, such that capture-driven regulatory laxity is offset by genuine safety gains? Or is the technology fundamentally unsafe and captured regulation conceals this?',
    'Long-term comparative accident rate analysis across deployment jurisdictions with varying regulatory stringency; analysis of accident types where autonomous vehicles fail vs succeed; assessment of whether safety gains come from technology or from biased sample (wealthy areas, favorable weather, known routes)',
    'If autonomous vehicles are net-safer: extraction is justified as coordination cost. If net-unsafe or technology-dependent: extraction is unjustified predation on public safety.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_improvement_direction, empirical, 'Whether autonomous vehicles are actually safer than human drivers').

omega_variable(
    captured_regulator_identity_lock,
    'Have regulators internalized the manufacturers'' framing of ''safe enough'' through cognitive capture, such that meaningful oversight would require identity-breaking recalibration?',
    'Analysis of regulator communications, internal deliberations, and career trajectories; interview data on how regulators rationalize capture; examination of whether regulators can envision alternative safety frameworks or have fused identity with manufacturer-friendly standards',
    'If identity-locked: regulatory recapture requires institutional redesign and personnel change (higher path dependence). If merely constrained: recapture is possible with resource and political realignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(captured_regulator_identity_lock, conceptual, 'Whether regulators have identity-fused with captured standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_autonomous_vehicles, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcav_tr_t0, regulatory_capture_autonomous_vehicles, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rcav_tr_t3, regulatory_capture_autonomous_vehicles, theater_ratio, 3, 0.52).
narrative_ontology:measurement(rcav_tr_t6, regulatory_capture_autonomous_vehicles, theater_ratio, 6, 0.68).
narrative_ontology:measurement(rcav_tr_t9, regulatory_capture_autonomous_vehicles, theater_ratio, 9, 0.75).

% Extraction over time
narrative_ontology:measurement(rcav_be_t0, regulatory_capture_autonomous_vehicles, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(rcav_be_t3, regulatory_capture_autonomous_vehicles, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(rcav_be_t6, regulatory_capture_autonomous_vehicles, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(rcav_be_t9, regulatory_capture_autonomous_vehicles, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_autonomous_vehicles, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_autonomous_vehicles, autonomous_vehicle_liability_distribution).
narrative_ontology:affects_constraint(regulatory_capture_autonomous_vehicles, insurance_system_asymmetric_risk).

% DUAL FORMULATION NOTE:
% Regulatory capture in autonomous vehicles is upstream of liability distribution constraints and insurance system risk asymmetries. The capture determines what safety standards manufacturers must meet; downstream constraints are shaped by the captured regulatory framework. The constraint family has three members: (1) regulatory_capture_autonomous_vehicles (ε≈0.58, Tangled Rope) — the capture mechanism itself; (2) autonomous_vehicle_liability_distribution (ε≈0.65, Snare) — how liability flows given under-regulation; (3) insurance_system_asymmetric_risk (ε≈0.55, Tangled Rope) — how insurance markets respond to regulatory ambiguity. All three are linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_autonomous_vehicles, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
