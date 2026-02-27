% ============================================================================
% CONSTRAINT STORY: sludge_bureaucratic_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sludge_bureaucratic_friction, []).

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
 *   constraint_id: sludge_bureaucratic_friction
 *   human_readable: Sludge (Intentional Administrative Friction)
 *   domain: political/economic/administrative
 *
 * SUMMARY:
 *   Sludge—intentional administrative friction designed to discourage access
 *   to legally available benefits, rights, or services—operates as a
 *   mechanism of soft rationing that extracts through discouragement rather
 *   than explicit denial. Unlike formal eligibility restrictions (which are
 *   transparent) or means-testing (which is explicit), sludge creates
 *   barriers of complexity, delay, and documentation burden that leave
 *   eligible individuals bearing the cost of proving their entitlement. The
 *   constraint exemplifies a hybrid extraction mechanism: budget gatekeepers
 *   design sludge as a rationing solution while maintaining a public fiction
 *   that all eligible parties can claim; caseworkers are simultaneously
 *   constrained by the sludge they administer; eligible non-claimants bear
 *   the full cost through de facto benefit denial; and welfare rights
 *   organizations work to dismantle sludge through legal challenges and
 *   simplified application pathways. The constraint's theater ratio (0.68)
 *   reflects the gap between formal administrative rhetoric (accessibility,
 *   equity, customer service) and functional design (discouragement,
 *   friction, complexity). Sludge has intensified over the 30-year interval
 *   as agencies have compounded requirements, digitized systems without
 *   usability testing, and used compliance metrics that penalize rapid claim
 *   processing.
 *
 * KEY AGENTS:
 *   - Eligible Non-Claimants: Primary victims (powerless/trapped) — legally entitled to benefits but face overwhelming administrative barriers; cannot exit
 *   - Budget-Constrained Agency: Primary beneficiary (institutional/arbitrage) — uses sludge to ration limited budget without making explicit denials; experiences as coordination necessity
 *   - Caseworkers: Secondary actors (moderate/constrained) — simultaneously constrained by caseload/compliance metrics and agents of sludge enforcement
 *   - Policy Architects: Powerful beneficiaries (powerful/mobile) — deliberately design sludge as soft rationing mechanism; receive political benefit of appearing fiscally responsible
 *   - Welfare Rights Coalition: Organized actors (organized/constrained) — building alternative pathways through litigation, legal aid, and advocacy for simplified applications
 *   - Administrative Apparatus: Institutional actor (institutional/arbitrage) — maintains formal accessibility rhetoric while functional mechanism is discouragement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing sludge as inevitable cost of administration rather than design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sludge_bureaucratic_friction, 0.58).
domain_priors:suppression_score(sludge_bureaucratic_friction, 0.72).
domain_priors:theater_ratio(sludge_bureaucratic_friction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, extractiveness, 0.58).
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sludge_bureaucratic_friction, snare).
narrative_ontology:human_readable(sludge_bureaucratic_friction, "Sludge (Intentional Administrative Friction)").
narrative_ontology:topic_domain(sludge_bureaucratic_friction, "political/economic/administrative").

domain_priors:requires_active_enforcement(sludge_bureaucratic_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sludge_bureaucratic_friction, budget_gatekeepers).
narrative_ontology:constraint_beneficiary(sludge_bureaucratic_friction, bureaucratic_administrators).
narrative_ontology:constraint_victim(sludge_bureaucratic_friction, eligible_claimants).
narrative_ontology:constraint_victim(sludge_bureaucratic_friction, policy_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELIGIBLE NON-CLAIMANT (SNARE) — Legally entitled to benefits but faces sludge barriers (forms, documentation, wait times, office visits). Cannot exit; choosing not to claim is functionally forced abandonment of legal entitlement. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82. Maximum extraction.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUDGET-CONSTRAINED AGENCY (ROPE) — Faces genuine coordination problem: limited budget must be distributed; sludge serves as rationing mechanism via discouragement. Experiences constraint as necessity rather than extraction. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary (negative χ indicates institutional advantage).
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CASEWORKER (TANGLED ROPE) — Benefits from job security and procedural structure (coordination function); constrained by caseload and compliance metrics that incentivize sludge maintenance. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61. Mixed coordination-extraction.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WELFARE RIGHTS COALITION (SCAFFOLD) — Organized actors (legal aid, nonprofits, advocacy groups) building alternative pathways: simplified applications, application assistance, litigation against sludge. See sludge as temporary barrier with sunset via legal/procedural reform. d≈0.35, f(d)≈0.28, σ=1.0 → χ≈0.16. Low effective extraction; coalition has agency.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ADMINISTRATIVE APPARATUS (PITON) — Formal goal is equitable benefit distribution; actual mechanism is discouragement via friction. theater_ratio=0.68 reflects gap between official accessibility rhetoric and functional barriers. Process persists through institutional inertia despite documented failures to reach intended beneficiaries.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POLICY ARCHITECT (TANGLED ROPE) — Powerful actors (legislators, agency heads) design sludge intentionally as a soft form of rationing, avoiding explicit denials. Coordinates on budget constraint while extracting political benefit (appearing fiscally responsible). d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.24. Deliberate hybrid.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risks naturalizing sludge as inherent to administration: 'All bureaucracy requires friction; this is just the cost of order.' But structural data (ε=0.58, suppression=0.72, theater=0.68) contradicts naturalization. Sludge is not inherent; it is designed. Engine flags as false summit.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sludge_bureaucratic_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sludge_bureaucratic_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sludge_bureaucratic_friction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sludge_bureaucratic_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sludge_bureaucratic_friction, TR),
    TR >= 0.70.

:- end_tests(sludge_bureaucratic_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Sludge extracts through discouragement—eligible individuals forfeit legally available benefits. The extraction is not as total as pure denial (which would be ε≈0.90) because some eligible parties successfully navigate sludge and some agencies provide application assistance. The value reflects documented claim-rate suppression of 30-50% in high-sludge jurisdictions, representing significant extraction from the eligible population. Over the interval, extractiveness has increased as agencies compounded requirements. Suppression (0.72): High. Significant coercive elements include: mandatory documentation proving eligibility status, time-consuming application procedures, long wait times for determination, limited office hours and locations, and lack of appeal accessibility. Few alternatives exist for accessing the benefit. Switching to alternative programs is often not possible (e.g., disability benefits have no substitute). The eligible population cannot organize collective exit. Theater ratio (0.68): High. Formal administrative rhetoric emphasizes accessibility and customer service, but functional design contradicts this. Agency websites claim 'simple application process' while actual forms require specialized knowledge and documentation. Agencies advertise 'rapid processing' while actual timelines exceed statutory requirements. This gap between claimed and actual functionality—the performative nature of accessibility rhetoric—drives the high theater value.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival disagreement across institutional and individual levels. The budget-constrained agency sees a legitimate coordination problem (Rope)—they have limited funds and must distribute them; sludge is an implicit rationing mechanism that avoids explicit denial and political conflict. The eligible non-claimant sees pure extraction (Snare)—they are legally entitled to benefits but face insurmountable barriers, with no alternative mechanism to access support. The caseworker experiences a hybrid (Tangled Rope)—their job requires them to administer complex requirements, which constrains their ability to help, but also provides job security and procedural protection. The policy architect deliberately designs this hybrid (Tangled Rope)—they coordinate on the budget constraint while extracting political benefit of appearing fiscally conservative. The welfare rights coalition sees a temporary problem being solved (Scaffold)—litigation and advocacy are building pathways to simplified applications and sludge elimination. The administrative apparatus sees its own process as degraded (Piton)—formal mission is equitable access, but actual mechanism is discouragement; the gap persists through institutional inertia. The analytical observer risks naturalizing sludge as inevitable (Mountain)—'all bureaucracy creates friction'—but the structural data reveals this as a false summit: sludge is designed, not inherent.
 *
 * DIRECTIONALITY LOGIC:
 *   Eligible non-claimants: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction—no alternative benefit source, cannot organize, cannot exit without forfeiting legal entitlement. Budget-constrained agency: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary—can arbitrage between implicit rationing (sludge) and explicit denials (litigation risk), and can adjust sludge intensity as needed. Caseworkers: Mixed victim/agent + constrained → d≈0.68, f(d)≈1.05. Significant extraction—caseload limits and compliance metrics constrain their ability to assist while requiring them to maintain sludge mechanisms. Policy architects: Deliberate hybrid + mobile → d≈0.42, f(d)≈0.42. Moderate extraction—intentional extraction but with public coordination benefit (appearing fiscally responsible, avoiding explicit benefit denials). Welfare rights coalition: Organized + constrained → d≈0.35, f(d)≈0.28. Low effective extraction—coalition has legal and advocacy resources and sees clear path forward (litigation, regulatory change). Administrative apparatus: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (≥0.70 required, we have 0.68, near threshold); institutional actors maintain process through inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolved through explicit decomposition of perspectives. The constraint resolves to Snare from the eligible non-claimant perspective (d≈0.95, χ≈0.82 > 0.66 snare threshold), which is the correct observation point for detecting extraction. From the budget-constrained agency perspective, it appears as Rope (negative χ), which is internally coherent but incomplete—the agency perspective naturalizes sludge as necessary coordination when it is actually designed extraction. From the policy architect perspective, it is deliberately hybrid Tangled Rope (d≈0.42, χ≈0.24)—intentional coordination on budget constraint PLUS intentional extraction via discouragement. The falsity of the Mountain perspective (analytical observer naturalizing sludge) is detected by the engine: formal accessibility claims (emerges_naturally=false, accessibility_collapse < 0.85, resistance > 0.15) contradict the mountain gate. Sludge is not a natural law; it is a designed mechanism that persists through institutional inertia (piton properties: theater=0.68, degraded function). The constraint exhibits the classic mandatrophy pattern: legitimate coordination problem (budget constraint) used as cover for extraction mechanism. Resolving requires: (1) acknowledging the snare perspective as the primary observation point, (2) recognizing that the coordination problem could be solved through transparent mechanisms (explicit caps, lottery, means-testing) that eliminate sludge, and (3) understanding that sludge persistence indicates extraction motivation, not coordination necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_detection,
    'Is sludge intentionally designed to discourage claims, or is it an unintended byproduct of complex systems?',
    'Historical document analysis (memoranda, budget notes, agency communications); comparison of sludge levels across agencies with similar functions but different administrative cultures; field interviews with policy architects',
    'If intentional: Snare classification confirmed. If accidental: may degrade to Tangled Rope or even Rope. Classification outcome depends on deliberation chain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentionality_detection, empirical, 'Whether sludge is intentionally designed to discourage claims').

omega_variable(
    discouragement_rate_threshold,
    'What rate of claim discouragement (by eligible non-claiming percentage) constitutes extraction vs normal administrative friction?',
    'Comparative cross-program data: eligible population vs actual claimants; international benchmarks for similar benefits programs; econometric analysis of sludge intensity vs claim rates',
    'If >40% discouragement: Snare classification strengthened. If <20%: may reclassify as Rope. Threshold determines whether χ value is empirically justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discouragement_rate_threshold, empirical, 'Claim discouragement rate indicating extraction threshold').

omega_variable(
    alternative_rationing_mechanism,
    'Do alternative rationing mechanisms (explicit eligibility caps, lottery, means-testing, time limits) exist that would replace sludge without loss of budget control?',
    'Policy design analysis; comparison with jurisdictions using transparent rationing; cost-benefit study of simplified application + higher claim rates vs sludge + lower claim rates',
    'If alternatives exist: sludge classification as intentional extraction strengthened (Snare). If no feasible alternative: may downgrade to necessary Scaffold. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_rationing_mechanism, conceptual, 'Whether transparent alternatives to sludge-based rationing exist').

omega_variable(
    caseworker_autonomy_constraint,
    'Are caseworkers themselves constrained by sludge (caseload limits, compliance metrics, restricted discretion), or are they agents of sludge?',
    'Caseworker surveys and interviews; analysis of performance metrics and reward structures; comparison of sludge intensity across agencies with different caseworker autonomy levels',
    'If constrained: caseworkers are secondary victims, tangled rope perspective correct. If agential: caseworkers are partial beneficiaries, extraction mechanism more direct. Affects inter-institutional directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caseworker_autonomy_constraint, empirical, 'Whether caseworkers are constrained by or agents of sludge').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sludge_bureaucratic_friction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sludge_tr_t0, sludge_bureaucratic_friction, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sludge_tr_t15, sludge_bureaucratic_friction, theater_ratio, 15, 0.62).
narrative_ontology:measurement(sludge_tr_t30, sludge_bureaucratic_friction, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(sludge_be_t0, sludge_bureaucratic_friction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sludge_be_t15, sludge_bureaucratic_friction, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(sludge_be_t30, sludge_bureaucratic_friction, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sludge_bureaucratic_friction, resource_allocation).
narrative_ontology:affects_constraint(sludge_bureaucratic_friction, welfare_access_equity).
narrative_ontology:affects_constraint(sludge_bureaucratic_friction, administrative_burden_health_costs).
narrative_ontology:affects_constraint(sludge_bureaucratic_friction, means_testing_stigma).

% DUAL FORMULATION NOTE:
% Sludge is upstream of specific benefit programs (SNAP, disability, unemployment) but represents a distinct structural constraint on administrative access. Downstream constraints (welfare_access_equity, health impacts of administrative burden) have their own ε values reflecting outcome severity; sludge_bureaucratic_friction has ε=0.58 reflecting the intentional design of administrative barriers as a soft rationing mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sludge_bureaucratic_friction, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
