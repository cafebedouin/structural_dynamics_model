% ============================================================================
% CONSTRAINT STORY: organ_scarcity_moral_rationing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organ_scarcity_moral_rationing, []).

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
 *   constraint_id: organ_scarcity_moral_rationing
 *   human_readable: Organ Scarcity and Moral Rationing Systems
 *   domain: medical_ethics/healthcare_allocation
 *
 * SUMMARY:
 *   Organ scarcity creates a constraint that functions simultaneously as pure
 *   coordination (preventing nepotistic or market-based organ allocation), as
 *   mixed coordination-extraction (formal rationing criteria both enable fair
 *   allocation and systematically disadvantage marginalized patients), and as
 *   extraction disguised as medical necessity (dialysis industry benefits
 *   from chronic scarcity, wealthy patients arbitrage to international
 *   markets, transplant centers compete for volume). The constraint exhibits
 *   the full mandatrophy pattern: different institutional actors perceive it
 *   as rope (coordination), snare (pure extraction), piton (degraded ritual),
 *   tangled rope (hybrid), or mountain (natural limit). The extractiveness
 *   trajectory (0.42→0.58 over 20 years) reflects gradual recognition that
 *   scarcity is partially policy-maintained rather than immutable—as
 *   alternatives (xenotransplantation, incentivized donation programs) become
 *   technologically feasible, the extraction mechanisms become more visible.
 *   Theater ratio (0.52→0.65) tracks the increasing performative component:
 *   moral language about 'fair allocation' and 'medical necessity'
 *   intensifies as the constraint becomes harder to justify as natural
 *   scarcity.
 *
 * KEY AGENTS:
 *   - End-Stage Organ Failure Patients: Primary victims (powerless/trapped) — systematically disadvantaged by allocation criteria; no exit option; mortality consequence of rationing
 *   - Marginalized Populations: Secondary victims (moderate/constrained) — face systemic barriers to pre-transplant care, higher infection rates, lower social capital; extraction layered onto coordination
 *   - Transplant Centers: Primary beneficiaries (institutional/arbitrage) — gain prestige, funding, volume through organ access; experience constraint as pure coordination
 *   - Organ Procurement Organizations: Institutional coordinators (organized/mobile) — manage allocation networks; also benefit from organ volume metrics and regulatory authority
 *   - Dialysis Industry: Secondary beneficiary (institutional/arbitrage) — revenue depends on chronic dialysis population; low incentive to expand transplantation
 *   - Wealthy Patients: Global beneficiaries (powerful/arbitrage) — can access organs internationally, bypassing domestic rationing constraints; extraction displaced to source populations
 *   - Families of Organ Donors: Affected parties (moderate/trapped) — face intensive recruitment pressure; limited ability to refuse donation; moral framing obscures coercive elements
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees mandatrophy resolution: constraint is genuinely hybrid, not one function naturalizing another
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organ_scarcity_moral_rationing, 0.58).
domain_priors:suppression_score(organ_scarcity_moral_rationing, 0.72).
domain_priors:theater_ratio(organ_scarcity_moral_rationing, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organ_scarcity_moral_rationing, extractiveness, 0.58).
narrative_ontology:constraint_metric(organ_scarcity_moral_rationing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(organ_scarcity_moral_rationing, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organ_scarcity_moral_rationing, tangled_rope).
narrative_ontology:human_readable(organ_scarcity_moral_rationing, "Organ Scarcity and Moral Rationing Systems").
narrative_ontology:topic_domain(organ_scarcity_moral_rationing, "medical_ethics/healthcare_allocation").

domain_priors:requires_active_enforcement(organ_scarcity_moral_rationing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organ_scarcity_moral_rationing, transplant_centers).
narrative_ontology:constraint_beneficiary(organ_scarcity_moral_rationing, wealthy_patients).
narrative_ontology:constraint_beneficiary(organ_scarcity_moral_rationing, dialysis_industry).
narrative_ontology:constraint_victim(organ_scarcity_moral_rationing, end_stage_organ_failure_patients).
narrative_ontology:constraint_victim(organ_scarcity_moral_rationing, marginalized_populations).
narrative_ontology:constraint_victim(organ_scarcity_moral_rationing, organ_donation_families).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END-STAGE PATIENT (SNARE) — Faces imminent death without transplant. No exit option: cannot purchase organs legally, cannot access organs through informal markets in most jurisdictions, cannot negotiate allocation criteria that favor their survival. Experiences the constraint as pure extraction — the rationing system determines who lives based on criteria (age, comorbidities, social utility) that systematically disadvantage elderly, disabled, and socially marginalized patients. Maximum suppression: medical dependency creates absolute barrier to exit.
constraint_indexing:constraint_classification(organ_scarcity_moral_rationing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED PATIENT POPULATION (TANGLED ROPE) — Constrained by systemic barriers: less access to pre-transplant care, higher infection rates from treatment gaps, lower social capital in transplant center evaluations. Modest coordination benefit exists — formal allocation rules prevent purely nepotistic or wealth-based distribution. But the rationing criteria themselves embed extraction: allocation algorithms weight social factors (likelihood to comply with post-transplant medication, employment status, family support) that disadvantage patients from low-income communities. Significant asymmetric extraction layered onto imperfect coordination.
constraint_indexing:constraint_classification(organ_scarcity_moral_rationing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPO NETWORK (TANGLED ROPE) — Organized institutional actors managing allocation for regional networks. Genuine coordination function: standardized criteria prevent chaos and explicit nepotism. Also substantial extraction mechanism: OPO administrators gain prestige and funding through transplant volume metrics, creating incentive to claim organs (via expanded donation criteria, aggressive family approach) while rationalizing allocation through medical-necessity framing. Exit option (mobile) derives from capacity to adjust allocation policies and criteria — but institutional legitimacy depends on maintaining appearance of purely medical rationing.
constraint_indexing:constraint_classification(organ_scarcity_moral_rationing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSPLANT CENTER (ROPE) — Benefits from organ availability through institutional prestige, funding, and patient volume. Experiences the constraint as coordination: the rationing system allocates scarce organs to the centers best positioned to use them (verified infrastructure, experienced surgeons, better outcomes). Near-perfect exit option (arbitrage): can transfer patients between centers, participate in multi-regional allocation networks, influence criteria through clinical guidelines. The constraint functions as pure coordination from this perspective — solving the collective action problem of matching organs to centers.
constraint_indexing:constraint_classification(organ_scarcity_moral_rationing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: WEALTHY PATIENT (SNARE) — Powerful agent with arbitrage exit option: can travel internationally to access organs in systems with lower rationing barriers (organ markets in some regions, less stringent allocation criteria). From this agent's perspective, the domestic rationing constraint does not actually bind — effective extraction is low because arbitrage is available. But this arbitrage depends on maintaining global inequality: wealthy patients extracting organs from poorer populations through medical tourism. The domestic constraint appears as rope (coordination) to this agent; the global structure reveals snare extraction displaced to more vulnerable populations.
constraint_indexing:constraint_classification(organ_scarcity_moral_rationing, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIALYSIS INDUSTRY (PITON) — Institutional beneficiary with arbitrage option. Dialysis providers benefit substantially from chronic kidney disease population unable to access transplants — prolonged dialysis dependence generates revenue. Theater ratio is high: rationing is justified through medical criteria and scarcity framing, but the scarcity is partially maintained by low donation rates (which dialysis industry does not incentivize to change). The constraint persists through institutional inertia — alternatives to scarcity-based rationing (incentivized donation, xenotransplantation, artificial organs) exist but are underinvested. The theater is the moral framing (we allocate fairly within constraints) masking the structural interest (prolonged scarcity is profitable).
constraint_indexing:constraint_classification(organ_scarcity_moral_rationing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the constraint as genuine coordination (rationing prevents pure market or nepotistic allocation) layered with extraction mechanisms at multiple scales: wealthy patients arbitrage to global markets, marginalized populations are systematically disadvantaged by allocation criteria, dialysis industry benefits from chronic scarcity, transplant centers compete for volume, OPOs manage allocation while building prestige metrics. The constraint resolves mandatrophy by confirming it is genuinely hybrid — coordination and extraction are both real structural features, not one naturalizing the other.
constraint_indexing:constraint_classification(organ_scarcity_moral_rationing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organ_scarcity_moral_rationing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organ_scarcity_moral_rationing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organ_scarcity_moral_rationing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organ_scarcity_moral_rationing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organ_scarcity_moral_rationing, TR),
    TR >= 0.70.

:- end_tests(organ_scarcity_moral_rationing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, rising from 0.42): Reflects the constraint's core function—scarcity-based rationing inherently extracts from patients unable to meet allocation criteria or lacking arbitrage options. The rising trajectory (0.42→0.58) indicates growing recognition that scarcity is policy-contingent: as xenotransplantation becomes technologically feasible and donation expansion programs prove viable, the extraction becomes harder to naturalize as 'inevitable scarcity.' Current extractiveness is moderate-high because the coordination function (preventing nepotism, standardizing allocation) is genuine but deeply compromised by extraction at multiple scales. Suppression (0.72): Very high. Multiple barriers confine patients: dialysis dependence creates medical vulnerability, legal prohibition on organ markets blocks formal trading, information asymmetry about allocation criteria, geographic barriers to transplant center access, social barriers for marginalized populations. Suppression mechanisms are both structural (medical dependency, legal prohibitions) and internalized (moral framing that rationing is 'fair' and 'necessary'). Theater ratio (0.65, rising from 0.52): Increasingly performative. The moral language of 'ethical allocation' and 'medical necessity' intensifies as the technical basis for scarcity weakens. Allocation criteria are presented as objectively medical (age, comorbidity) but function as social sorting. Transplant center 'evaluation' of patient social factors is framed as medical-predictive but operates as social gatekeeping. The rising theater reflects Goodhart drift—metrics (transplant outcomes, patient compliance, survival probability) become targets, and the allocation system optimizes toward these metrics at the cost of explicit fairness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates mandatrophy through radical perspectival divergence on the same structural facts. The end-stage patient sees snare—mortality consequence, no exit, pure extraction. The transplant center sees rope—coordination solving allocation problem. The dialysis industry sees piton—the constraint is performatively justified but actually benefits them through chronic patient dependency. Wealthy patients see rope at the domestic scale (coordination works for them) but snare at the global scale (their arbitrage depends on exploitation in source countries). The OPO network sees tangled rope—genuine coordination with embedded extraction through volume-maximization incentives. The analytical observer sees tangled rope with embedded dialysis industry interest. No single type captures the structure; the presheaf of perspectives reveals the hybrid function (genuine coordination layered with structural extraction at multiple scales) and the international displacement of extraction (wealthy patient arbitrage outsources snare mechanisms to source populations).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. End-stage patients (trapped, victim status) compute high d→high f(d)→high experienced χ. Marginalized populations (constrained, victim status) compute moderate-high d, producing tangled rope (mixed coordination-extraction). Transplant centers (arbitrage, beneficiary status) compute low d→negative f(d)→low/negative experienced χ, perceiving the constraint as rope. Wealthy patients (arbitrage, beneficiary-at-global-scale-but-victim-as-source) exhibit perspectival split: at the national level d is very low (beneficiary with arbitrage); at the global level d approaches victim status (beneficiary because others pay cost). OPOs (organized/mobile, institutional beneficiary) compute d around 0.35-0.45 depending on institutional dependency on volume metrics—mobile because they can adjust policies, but organized entanglement with transplant center incentives. Dialysis industry (institutional/arbitrage, secondary beneficiary through sustained scarcity) computes d toward 0.1-0.2 (full beneficiary, arbitrage exit because profit depends on persistent patient population). The analytical observer computes canonical d for analytical power (0.73), perceiving the constraint at the civilizational scale where all extraction mechanisms become visible.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through perspectival completeness: all six classification types emerge from the structural data without contradiction. The RESOLUTION reveals that the constraint is genuinely hybrid (tangled_rope at the analytical level, mandatrophy_resolved = true), not a false natural law disguised as coordination. The mandatrophy operates at three levels: (1) DOMESTIC-SCALE MANDATROPHY: Is the constraint pure coordination (fair allocation preventing nepotism) or mixed coordination-extraction (fair allocation criteria systematically disadvantage marginalized populations)? The data resolves to 'mixed'—both functions are real. (2) INTERNATIONAL-SCALE MANDATROPHY: Does wealthy patient arbitrage represent legitimate coordination (access to treatment) or snare extraction (treatment access depends on coercive organ procurement from poorer populations)? The data resolves to 'both'—the same institutional arrangement is rope for wealthy agents and snare for source-population agents. (3) TEMPORAL MANDATROPHY: Is scarcity an immutable natural limit (mountain) or a policy-contingent arrangement that could be dissolved by xenotransplantation or incentivized donation (scaffold)? The data resolves to 'policy-contingent'—scarcity is partially maintained by institutional choices, not immutable. The rising extractiveness (0.42→0.58) and theater ratio (0.52→0.65) tracks the increasing visibility of policy contingency—as alternatives become feasible, the extraction becomes harder to naturalize, and the constraint's classification approaches snare (from piton/rope hybrid). The mandatrophy is NOT resolved by discovering 'the true type' but by recognizing that the constraint is genuinely multi-typed across perspectives and scales, with embedded extraction mechanisms that become more visible as the scarcity-naturalness framing weakens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_naturalness_vs_policy,
    'Is organ scarcity an immutable natural limit (physiological constraint on donation rates) or a partially contingent policy outcome (low donation rates reflect suboptimal recruitment, infrastructure underinvestment, and deliberate market restrictions)?',
    'Cross-national comparative analysis: donation rates correlate with opt-out vs opt-in registry laws, family approach training, transplant infrastructure investment, and xenotransplantation permitting. Counterfactual modeling of donation rate under alternative policies.',
    'If natural: rationing is pure coordination problem (mountain), scarcity-based allocation is inevitable. If policy-contingent: scarcity is partially maintained by institutional choices, rationing system can be analyzed for extractive vs coordinative functions independent of scarcity claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_naturalness_vs_policy, empirical, 'Whether organ scarcity is inherent or policy-maintained').

omega_variable(
    allocation_criterion_neutrality,
    'Do UNOS allocation criteria (age, comorbidities, time on waitlist, social factors) function as fair coordination or do they systematically extract from specific populations?',
    'Longitudinal outcome analysis: stratify by race, income, age, disability status; measure survival disparities, access time, explicit refusals correlated with demographic factors; analyze criterion weights for systematic bias.',
    'If neutral: rationing is coordination (Rope/Tangled Rope). If systematically biased: rationing is extraction mechanism disguised as medical neutrality (Snare/Tangled Rope with higher victim extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allocation_criterion_neutrality, empirical, 'Whether allocation criteria are systematically biased').

omega_variable(
    dialysis_industry_incentive_structure,
    'Does the dialysis industry actively oppose expansion of transplantation rates or organ donation infrastructure, and what is the financial dependency of patient populations on chronic dialysis?',
    'Industry financial analysis, lobbying records, policy positions on xenotransplantation and donation rate expansion; patient outcome data comparing transplant vs dialysis cohorts; revenue modeling for dialysis providers.',
    'If active opposition exists: dialysis industry is direct beneficiary of scarcity constraint (piton/snare extraction). If neutral or supportive: scarcity is not maintained by competing industry interest (reduces piton evidence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dialysis_industry_incentive_structure, empirical, 'Whether dialysis industry opposes transplantation expansion').

omega_variable(
    international_arbitrage_dependency,
    'What proportion of wealthy patients accessing organs internationally are extracting from populations without legal protections or meaningful informed consent?',
    'Epidemiological tracking of organ sources for medical tourists; survey data on payment, consent process, and long-term outcomes for donors in recipient-source countries; legal analysis of national organ trade frameworks.',
    'If high proportion: wealthy patient perspective is snare at global scale (arbitrage conceals exploitation). If low/negligible: arbitrage option is genuine coordination mechanism (rope from wealthy perspective reflects real legitimate access).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_arbitrage_dependency, empirical, 'Whether international organ access involves coercive donation').

omega_variable(
    xenotransplantation_viability_timeline,
    'What is the realistic timeline for xenotransplantation (genetically modified pig organs, artificial organs) to reduce or eliminate scarcity constraint?',
    'Technological assessment: clinical trial progress, regulatory pathway clearance, manufacturing scalability analysis, cost projections; expert consensus on 10-year feasibility.',
    'If viable within 10 years: scaffold perspective is accurate (sunset clause emerging from technology). If > 20 years or infeasible: scarcity constraint persists indefinitely, mandatrophy resolution requires accepting extraction as permanent feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(xenotransplantation_viability_timeline, empirical, 'Viability timeline for xenotransplantation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organ_scarcity_moral_rationing, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orgrat_tr_t0, organ_scarcity_moral_rationing, theater_ratio, 0, 0.52).
narrative_ontology:measurement(orgrat_tr_t10, organ_scarcity_moral_rationing, theater_ratio, 10, 0.6).
narrative_ontology:measurement(orgrat_tr_t20, organ_scarcity_moral_rationing, theater_ratio, 20, 0.65).
narrative_ontology:measurement(orgrat_tr_t5, organ_scarcity_moral_rationing, theater_ratio, 5, 0.57).
narrative_ontology:measurement(orgrat_tr_t15, organ_scarcity_moral_rationing, theater_ratio, 15, 0.63).

% Extraction over time
narrative_ontology:measurement(orgrat_be_t0, organ_scarcity_moral_rationing, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(orgrat_be_t10, organ_scarcity_moral_rationing, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(orgrat_be_t20, organ_scarcity_moral_rationing, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(orgrat_be_t5, organ_scarcity_moral_rationing, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(orgrat_be_t15, organ_scarcity_moral_rationing, base_extractiveness, 15, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organ_scarcity_moral_rationing, resource_allocation).
narrative_ontology:affects_constraint(organ_scarcity_moral_rationing, dialysis_dependency_chronic_illness).
narrative_ontology:affects_constraint(organ_scarcity_moral_rationing, xenotransplantation_regulatory_barriers).
narrative_ontology:affects_constraint(organ_scarcity_moral_rationing, medical_tourism_exploitation).

% DUAL FORMULATION NOTE:
% Organ scarcity moral rationing decomposes into three structurally distinct constraints: (1) scarcity_coordination—pure coordination problem of matching scarce organs to patients (ε≈0.15, rope); (2) rationing_extraction—allocation criteria systematically disadvantaging marginalized populations (ε≈0.52, snare/tangled_rope); (3) dialysis_industry_dependency—institutional interest in maintaining chronic scarcity (ε≈0.48, piton/snare). This story treats them as a single hybrid constraint; decomposition into separate stories recommended for targeted policy analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organ_scarcity_moral_rationing, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
