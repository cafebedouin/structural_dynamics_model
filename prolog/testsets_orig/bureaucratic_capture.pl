% ============================================================================
% CONSTRAINT STORY: bureaucratic_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_capture, []).

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
 *   constraint_id: bureaucratic_capture
 *   human_readable: Bureaucratic Capture: Regulatory Agency Mission Drift
 *   domain: political_economy/regulatory_capture
 *
 * SUMMARY:
 *   Bureaucratic capture describes the structural inversion of regulatory
 *   agency mission: an institution created to constrain an industry gradually
 *   becomes an instrument for industry preference advocacy. The constraint
 *   exhibits the full tangled rope structure — genuine regulatory
 *   coordination functions (standard-setting, information aggregation,
 *   dispute resolution) coexist with asymmetric extraction (preferential rule
 *   interpretation, selective enforcement, regulatory forbearance). The
 *   mechanism operates through three reinforcing pathways: (1) industry
 *   expertise concentration — the regulated industry holds specialized
 *   knowledge that agencies depend on, creating information asymmetry; (2)
 *   career circulation — regulatory staff rotate into industry positions,
 *   creating exit pathways and future incentives; (3) institutional inertia —
 *   regulatory processes accumulate industry-friendly precedents that
 *   constrain successors. The extractiveness value of 0.58 reflects that the
 *   mechanism is neither pure coordination (which would be rope) nor pure
 *   extraction without coordination function (which would be snare). The
 *   theater ratio of 0.68 indicates substantial performative activity:
 *   regulatory hearings, impact assessments, and public comment periods
 *   maintain the appearance of public-interest review while industry
 *   preferences effectively determine outcomes. The measurements show
 *   acceleration over the interval — both extractiveness and theater increase
 *   as the capture mechanism solidifies and requires more elaborate
 *   legitimating performance.
 *
 * KEY AGENTS:
 *   - Regulated Industry: Primary beneficiary (institutional/arbitrage) — captures favorable regulatory interpretation; can arbitrage regulatory differences across jurisdictions
 *   - Regulated Population: Primary victim (powerless/trapped) — bears costs of regulatory favoritism through higher prices, reduced safety, or environmental damage; cannot exit the jurisdiction
 *   - Enforcement Staff: Secondary victim (moderate/constrained) — experience mission drift and cognitive dissonance; constrained exit (career investment); also receive benefits (stability, funding, predictability from captured equilibrium)
 *   - Reform Coalition: Organized victim (organized/constrained) — consumer advocates, environmental groups, labor unions; have organizing capacity but face funding disadvantages and information asymmetries vs. industry
 *   - Regulatory Institution: Structural mechanism (institutional/arbitrage) — maintains public legitimacy while favoring regulated industry; benefits from legitimacy of capture mechanism
 *   - Transparency Advocates: Reform agents (organized/constrained) — push for disclosure mandates and multi-stakeholder processes; constrained by political and resource barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_capture, 0.58).
domain_priors:suppression_score(bureaucratic_capture, 0.62).
domain_priors:theater_ratio(bureaucratic_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(bureaucratic_capture, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bureaucratic_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_capture, tangled_rope).
narrative_ontology:human_readable(bureaucratic_capture, "Bureaucratic Capture: Regulatory Agency Mission Drift").
narrative_ontology:topic_domain(bureaucratic_capture, "political_economy/regulatory_capture").

domain_priors:requires_active_enforcement(bureaucratic_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_capture, regulated_industry).
narrative_ontology:constraint_victim(bureaucratic_capture, public_interest).
narrative_ontology:constraint_victim(bureaucratic_capture, enforcement_mandate).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGULATED POPULATION (SNARE) — Citizens subject to regulatory regime have no exit option. They cannot opt out of the jurisdiction or escape the regulatory mechanism. The extraction runs entirely toward the beneficiary (regulated industry). Maximum suppression — the public cannot organize alternative regulatory structures or defect from the system.
constraint_indexing:constraint_classification(bureaucratic_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENFORCEMENT STAFF (TANGLED ROPE) — Career regulators experience genuine coordination function (information gathering, standard-setting, institutional consistency) alongside asymmetric extraction. They benefit from career advancement within captured agencies but bear costs of mission drift and cognitive dissonance. Exit costs are high (sunk career investment) but not absolute (can move to NGOs, academia, or different agencies).
constraint_indexing:constraint_classification(bureaucratic_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATED INDUSTRY (ROPE) — Net beneficiary from captured agency. Experiences the constraint as pure coordination: regulatory agency provides certainty, predictability, and industry-friendly rule interpretation. Industry can arbitrage regulatory differences across jurisdictions. For regulated firms, the captured agency IS the coordination mechanism.
constraint_indexing:constraint_classification(bureaucratic_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (TANGLED ROPE) — Organized agents (consumer advocates, environmental groups, labor unions) perceive both coordination function (multi-stakeholder regulatory process) and asymmetric extraction (access advantages favor industry). They have agency to push for reform but face structural barriers: funding disadvantages vs industry, information asymmetries, and entrenched institutional practices. High suppression through complexity and resource disparity.
constraint_indexing:constraint_classification(bureaucratic_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY INSTITUTION (PITON) — The agency itself maintains the ritual of impartiality and public interest representation while actual decision-making favors regulated industry. Theater ratio high: regulatory proceedings, impact assessments, and public comment periods are performed but outcomes predetermined. The institution persists through formal legitimacy despite degraded function, sustained by legal mandate and bureaucratic inertia.
constraint_indexing:constraint_classification(bureaucratic_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: TRANSPARENCY ADVOCATES (SCAFFOLD) — Open records mandates, FOIA requirements, and disclosed lobbying activities create temporary scaffolding that reduces capture opacity without dismantling the capture mechanism itself. High theater in transparency theater (lots of documents, little effect on outcomes), but the scaffold perception sees this as a path toward eventual accountability. Sunset logic: as disclosure mechanisms mature and digital tools enable cross-referencing, the cost of maintaining capture rises.
constraint_indexing:constraint_classification(bureaucratic_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, regulatory capture is a recurrent structural feature of complex governance: agencies need industry expertise, industry has incentive to influence regulation, and no purely organizational mechanism prevents capture. However, the constraint is not immutable (not a mountain) because institutional redesigns (rotating personnel, transparency mechanisms, multi-stakeholder boards) demonstrably reduce capture severity. The analytical perspective sees the tangled rope structure: genuine coordination function (regulation IS needed) intertwined with structural extraction (industry dominance is not inevitable but is contingent on institutional design choices).
constraint_indexing:constraint_classification(bureaucratic_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_capture, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regulated industry captures significant value through favorable regulatory interpretation, selective enforcement, and rule postponement during political windows. However, the agency must still provide genuine regulation (industry cannot operate without rules), so some coordination function persists. The measurement trajectory (0.32 → 0.58) shows capture acceleration over time as informal relationships solidify and industry-friendly precedents accumulate. Suppression (0.62): Moderate-high. The public cannot exit the regulated system, enforcement staff face career barriers to resistance, and reform coalitions face resource disadvantages. However, suppression is not total — formal democratic mechanisms (electoral pressure, FOIA, public advocacy) provide some countervailing power. Theater ratio (0.68): High. Regulatory processes are substantially performative. Public comment periods are held but comments on disfavored policies are rarely determinative. Impact assessments are conducted but findings are rarely allowed to block industry-preferred outcomes. The theater has increased (0.38 → 0.68) because capture requires increasingly elaborate legitimating performance as the mechanism becomes more transparent and contested.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (regulated industry) perceives rope — the agency provides valuable coordination function and predictable rule environment. The trapped population perceives snare — they bear the costs of regulatory favoritism with no exit. The enforcement staff perceive tangled rope — they work on genuinely important coordination tasks but within a system designed to favor the regulated. The reform coalition perceives tangled rope — the regulatory system has coordination function but the extraction flow is asymmetric. The agency itself perceives piton — it maintains the ritual of impartial review despite knowing outcomes are predetermined. The transparency advocates perceive scaffold — disclosure mechanisms are gradually raising the cost of capture, creating a path toward reform. The analytical observer perceives tangled rope structure as recurrent but not immutable — institutional design choices (rotation policies, multi-stakeholder boards, external audits) demonstrably reduce capture severity, suggesting the constraint is contingent on institutional arrangements rather than inherent to governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values reflect each agent's structural position relative to the extraction flow. The regulated industry benefits from capture and has high exit optionality (arbitrage across jurisdictions) → low d → negative effective extraction chi. The trapped population bears costs and has no exit → high d → high chi. The enforcement staff have constrained exit (career investment) and bear extraction costs (mission drift) but also receive benefits (stability, predictability) → moderate d. The reform coalition has agency (organizing capacity) but faces barriers (resources, information asymmetry) → moderate d but with tension between power and exit options. The analytical perspective overrides the tendency to naturalize capture as inevitable by showing that institutional designs demonstrably reduce it — this breaks the false summit logic and confirms the tangled rope structure rather than a mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint resolves mandatrophy by distinguishing the coordination function (genuine agency role in setting standards, aggregating information, managing disputes) from the extraction mechanism (industry capture of that coordination function). The error case would be classifying regulatory capture as either pure rope (ignoring industry dominance) or as pure snare (ignoring that regulation itself is coordination work). The tangled rope classification holds both: the agency coordinates on behalf of industry rather than the public, creating an extraction mechanism layered onto genuine coordination function. Reform strategies differ by whether they target the coordination function (strengthen multi-stakeholder processes) or the capture mechanism (rotate personnel, reduce information asymmetry, increase transparency). Mandatrophy is resolved by recognizing that institutional redesigns that modify how coordination happens WITHOUT eliminating regulatory function demonstrably reduce extractiveness, confirming that the constraint is tangled rope (redesignable) rather than mountain (immutable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_threshold_ambiguity,
    'At what point does agency consideration of industry preferences become regulatory capture vs. legitimate stakeholder engagement?',
    'Outcome analysis: compare regulatory decisions to disclosed preferences of all stakeholder groups (industry, public interest, enforcement staff); measure deviation from median stakeholder position',
    'If threshold is tight (strict = capture): many agencies classified as captured that see themselves as balanced. If threshold is loose (permissive = balance): capture mechanism misses cases of effective industry dominance without explicit quid pro quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_threshold_ambiguity, conceptual, 'Definitional threshold for capture vs. stakeholder engagement').

omega_variable(
    identity_lock_in_enforcement_staff,
    'Are regulatory staff trapped by external barriers (career cost of exit) or identity-locked (professional identity constituted within captured agency)?',
    'Tracking of staff mobility and post-exit behavior: do staff who leave captured agencies change enforcement priorities? Do they report identity reorientation or just material improvement?',
    'If mostly trapped (external barriers): reform through rotation and outside hire possible. If mostly identity-locked: staff resistance to reform will persist even after capturing influence is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_enforcement_staff, empirical, 'Whether enforcement staff are materially trapped or identity-locked').

omega_variable(
    transparency_mechanism_efficacy,
    'Do disclosure mandates (lobbying records, revolving door tracking, algorithm audits) actually reduce capture or merely shift it into less visible channels?',
    'Measurement of capture before/after transparency implementation; analysis of decision outcomes vs disclosed lobbying intensity; identification of shift to informal channels',
    'If efficacy high: scaffold perspective validated — transparency is real pathway to reform. If efficacy low: transparency is pure theater, and capture persists because suppression mechanism (information asymmetry + resource disparity) remains intact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_mechanism_efficacy, empirical, 'Whether transparency mechanisms reduce or merely relocate capture').

omega_variable(
    institutional_redesign_stability,
    'Do organizational reforms that reduce capture (rotation policies, merit-based promotion, multi-stakeholder boards) persist or degrade back to captured equilibrium?',
    'Longitudinal study of reformed agencies: measure capture severity 5, 10, 20 years after reform; identify variables predicting regression',
    'If reforms persist: capture is contingent on institutional design (tangled rope structure validated). If reforms degrade: capture may be a basin attractor (mountain-like property of governance systems).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_redesign_stability, empirical, 'Persistence of institutional reforms against capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(burcap_tr_t0, bureaucratic_capture, theater_ratio, 0, 0.38).
narrative_ontology:measurement(burcap_tr_t10, bureaucratic_capture, theater_ratio, 10, 0.55).
narrative_ontology:measurement(burcap_tr_t20, bureaucratic_capture, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(burcap_be_t0, bureaucratic_capture, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(burcap_be_t10, bureaucratic_capture, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(burcap_be_t20, bureaucratic_capture, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_capture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bureaucratic_capture, 0.12).
narrative_ontology:affects_constraint(bureaucratic_capture, regulatory_forbearance).
narrative_ontology:affects_constraint(bureaucratic_capture, information_asymmetry_in_rulemaking).
narrative_ontology:affects_constraint(bureaucratic_capture, revolving_door_structural_bias).

% DUAL FORMULATION NOTE:
% Bureaucratic capture decomposes into three structurally distinct constraints: (1) regulatory_forbearance (ε≈0.45) — the specific policy of not enforcing rules; (2) information_asymmetry_in_rulemaking (ε≈0.38) — the structured knowledge imbalance in rule development; (3) revolving_door_structural_bias (ε≈0.52) — the career circulation mechanism. Each has different reform strategies and different measurement signatures. The parent story (this file) represents the integrated capture mechanism; the decomposed stories enable targeted analysis of specific reform pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bureaucratic_capture, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
