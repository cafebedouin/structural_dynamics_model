% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Platform Worker Hybrid Security Framework
 *   domain: labor_policy/social_protection
 *
 * SUMMARY:
 *   Platform work in the 2010s–2020s occupies a structural ambiguity: workers
 *   control their own schedule (contractor-like) yet depend on algorithmic
 *   allocation for income opportunity and face uninsured catastrophic risk
 *   (employment-like). The hybrid security reading institutionalizes a third
 *   category: platforms provide selected protections (injury coverage 86.2%,
 *   medical coverage 91.5%) without full employment obligations (pensions,
 *   unemployment insurance, career development). This reading claims the
 *   arrangement solves a genuine coordination problem while partially
 *   acknowledging extraction—platforms avoid full employment costs, workers
 *   receive some protection but lack security career paths. The tension is
 *   intentional: the constraint simultaneously ENABLES worker protection and
 *   ENABLES platform cost avoidance. The rising theater ratio (0.25→0.48)
 *   reflects growing disjunction between the coordination narrative (portable
 *   protections for precarious workers) and the operational reality
 *   (protection tiers institutionalize precarity while platforms extract
 *   algorithmic rent).
 *
 * KEY AGENTS:
 *   - platform_operators: institutional power, arbitrage exit — set terms, control algorithms, benefit from hybrid classification
 *   - platform_workers: powerless, constrained exit — receive partial protections, bear algorithmic control and income volatility
 *   - labor_unions: excluded, organized power — would shift classification toward full employment; voice suppressed in framework governance
 *   - labor_regulators: observer, analytical exit — validate framework legitimacy, audit protection coverage claims
 *   - worker_advocates: observer, moderate power — provide independent verification of protection adequacy; outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.58).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.62).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Platform Worker Hybrid Security Framework").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_policy/social_protection").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, '5a755f5a-f4d7-4062-8abd-25862cd3a86d').
narrative_ontology:cs_kernel_codification('5a755f5a-f4d7-4062-8abd-25862cd3a86d', distributed).
narrative_ontology:cs_authority_grounding('5a755f5a-f4d7-4062-8abd-25862cd3a86d', extraction).
narrative_ontology:cs_interpretation_layer_present('5a755f5a-f4d7-4062-8abd-25862cd3a86d').
narrative_ontology:cs_reading_relation('5a755f5a-f4d7-4062-8abd-25862cd3a86d', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a755f5a-f4d7-4062-8abd-25862cd3a86d', employment_boundary__substantive_employment_reading, influences).
narrative_ontology:cs_axiom('5a755f5a-f4d7-4062-8abd-25862cd3a86d', foundational, platform_worker_distinct_category_justified).
narrative_ontology:cs_axiom_status(platform_worker_distinct_category_justified, holdable).
narrative_ontology:cs_axiom_grounding('5a755f5a-f4d7-4062-8abd-25862cd3a86d', platform_worker_distinct_category_justified, instrumental).
narrative_ontology:cs_axiom('5a755f5a-f4d7-4062-8abd-25862cd3a86d', foundational, hybrid_protection_tier_adequate).
narrative_ontology:cs_axiom_status(hybrid_protection_tier_adequate, holdable).
narrative_ontology:cs_axiom_grounding('5a755f5a-f4d7-4062-8abd-25862cd3a86d', hybrid_protection_tier_adequate, empirically_contingent).
narrative_ontology:cs_reference_frame('5a755f5a-f4d7-4062-8abd-25862cd3a86d', three_tier_employment_classification).
narrative_ontology:cs_drift_state('5a755f5a-f4d7-4062-8abd-25862cd3a86d', contemporary_regulatory_maturation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5a755f5a-f4d7-4062-8abd-25862cd3a86d', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the marketplaces connecting workers to tasks. Set the terms of engagement, algorithmic matching rules, and dispute processes. Obligated to provide selected protections (injury insurance, basic medical coverage) but retain control over work allocation, rate-setting, and termination. Benefit from hybrid classification by avoiding full employment obligations (pensions, career development, unemployment insurance) while claiming worker-protection legitimacy.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Perform task work on digital platforms. Receive basic protections under hybrid framework (injury coverage 86.2%, medical coverage 91.5% across major platforms) but lack career development paths, pension contributions, and unemployment security. Face algorithmic management without traditional employment protections (grievance procedures, dismissal safeguards). Can exit to other platforms or employment, but switching costs are high and alternative income sources in same wage tier are scarce.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_workers, beneficiary).

% Operate under full employment obligations including mandatory pensions, unemployment insurance contributions, and dismissal safeguards. Would benefit from hybrid classification if extended to their workforce but are excluded by design—the hybrid framework applies only to platform-mediated work. Their absence from the governance table means they do not shape the framework's evolution.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employers, excluded,
    organized, generational, constrained, national).

% Represent traditional employees and would extend full employment classification to platform workers. Structurally excluded from hybrid framework governance; their voice would shift the classification boundary toward substantive employment. Mounting pressure through organizing campaigns and legislative testimony.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Lack access to platform work entirely (no smartphone, no digital literacy, no algorithm approval, geographic exclusion). The hybrid framework's protections are not available to them; their absence from the conversation reflects exclusion from the platform economy itself.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, workers_without_platform_access, excluded,
    powerless, biographical, trapped, global).

% Monitor platform worker protections, conduct audits of coverage claims (medical 91.5%, injury 86.2%), and adjudicate disputes. Can mandate expansions or contractions of the hybrid framework through regulation; currently validate the framework as a legitimate alternative to employment/independent contractor binary.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% Nonprofit and research organizations documenting worker experiences. Provide independent verification of protection coverage claims; produce testimony contesting the adequacy of hybrid protections relative to career risk and income volatility. Outside the beneficiary set; their corroboration carries weight in regulatory proceedings.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_worker_advocates, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The hybrid framework solves a genuine coordination problem: traditional employment law is poorly fitted to gig work's task-scale and worker-driven scheduling; pure independent contracting leaves workers exposed to catastrophic injury and health shocks with no insurance access. The framework offers portable, task-agnostic protections (injury coverage, basic medical) that travel with the worker across platforms, reducing the transaction cost of providing per-platform insurance and decoupling protection access from employment status.
% TRANSFER_FUNCTION: Moves mandatory insurance contributions from platforms to a pooled protection system; captures the choice of WHICH protections workers receive (basic medical/injury, explicitly NOT pensions/unemployment/career development) by institutionalizing a three-category boundary (employment, hybrid, independent contractor) and assigning platform workers to the middle tier. Transfers the choice of whether to remain in the framework to the platform operators, who can offer supplementary protections but face no regulatory obligation beyond the base tier.
% ABSENT_VOICES: Labor unions and traditional employers are structurally excluded from the governance table where the hybrid framework's boundaries are set and revised. Workers without platform access (no phone, no algorithm approval) are completely absent; the framework was not designed to reach them and provides no voice for extending it. Within the platform worker population, voices from precarious workers, migrants, and those dependent on single-platform income are underrepresented in governance structures.
% DISAPPEARANCE_RATIONALE: If the hybrid framework disappeared overnight, two immediate shifts would cascade: platform workers would either reclassify as employees (triggering full pension/unemployment/dismissal obligations for platforms) or revert to independent contractor status (losing the basic protections). Platforms would choose independent contractor to minimize obligations, workers would face uninsured injury risk, and a secondary regulatory scramble would ensue to either extend employment classification or rebuild hybrid protections. The entire three-category boundary would collapse into binary employment/independent contractor, reorganizing the legal and financial structure of the platform economy.
% FOUNDING_PROBLEM: Platform work emerged in the 2010s as a form of labor that did not fit traditional employment categories: workers controlled their schedule (non-employee-like) but depended on platform algorithms for income opportunity and faced catastrophic injury/health risk with no insurance (non-independent-contractor-like). Neither traditional employment law nor independent contractor frameworks provided meaningful protection. The founding problem was: how do you provide portable, affordable, work-agnostic protections to people who coordinate their own hours but depend on a single platform algorithm for income stability?
% FOUNDING_PROBLEM_CORROBORATION: Platform operators attest the founding problem is live and the hybrid framework is the solution—workers need portable protections without full employment obligations. Labor unions and substantive employment advocates attest the founding problem was MISFRAMED—economic dependence and algorithmic control make workers employees by any reasonable test, and the hybrid framework is a cover story for avoiding full obligations. Independent audits (academic research on platform worker earnings volatility, injury rates, and income risk) confirm the founding problem's original face: workers face genuine catastrophic risk without traditional insurance. Legislative testimony from worker advocates outside the benefiting parties confirms the problem was real but dispute whether the hybrid response is adequate.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint performs real coordination (portable injury coverage reduces transaction costs) AND enables platforms to avoid full employment obligations (pensions, unemployment, dismissal safeguards worth ~20–30% of wage in equivalent employment). The constraint is not pure extraction (workers receive protections they would lack under independent contractor status) but it is not pure coordination (platforms retain algorithmic control over work allocation and rate-setting, which concentrate income volatility risk on workers). Suppression is substantial (0.62) because the three-category boundary is actively maintained through regulatory adjudication, classification litigation, and algorithmic platform design—the framework persists by excluding full-employment classification from the conversation, not by worker choice. Theater is rising (0.25→0.48) because the 'portable protections' narrative increasingly masks the real operation: platforms claim the framework protects workers from catastrophic risk (true at the medical/injury tier) while actively suppressing the reclassification that would provide career development, pension contributions, and income security (false or evasive narrative). The measurement series tracks the 11-year arc from framework emergence through institutionalization—extractiveness plateaus after 2021 (protections are stable), theater rises continuously (performative worker-protection language increases while substantive protection scope narrows relative to employment obligations).
 *
 * PERSPECTIVAL GAP:
 *   The three stakeholder seats compute divergent types from the same structural data: (1) PLATFORM OPERATOR SEAT: the arrangement is genuine coordination (solved the portable-protection problem; worker-protective narrative is credible from this position). (2) PLATFORM WORKER SEAT: the arrangement is tangled rope verging on snare (coordination benefit is real but asymmetrical—platforms extract rate control and algorithmic allocation while providing selective protections; worker exit is suppressed by income dependence). (3) LABOR REGULATOR SEAT: the arrangement is tangled rope with theater (coordination is real; extraction is present but institutionally bounded; theater is rising as the framework matures). The engine computes this divergence from the power/exit/beneficiary/victim data; the narrative does not adjudicate which seat is 'correct'—all three readings are valid from their respective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are near the beneficiary end (d≈0.15): they set the framework, control the algorithms, retain rate-setting power, and avoid full employment obligations (net benefit ~20–30% of wage equivalent). Platform workers are split: they are beneficiaries of the coordination (injury coverage, medical access they would lack as pure contractors; d≈0.35 beneficiary direction) AND targets of extraction (income volatility, algorithmic allocation, suppressed career development; d≈0.70 target direction). The hybrid status institutionalizes this split—workers are partially protected (beneficiary) and partially extracted-from (victim). Labor unions are excluded (not in the frame) and would shift the d calculation if admitted—their presence would move worker d sharply toward the target end (d→0.85+) by introducing full employment comparison. The directionality override is not needed; the structural derivation from beneficiary + victim + constrained exit produces the correct asymmetry (operators near 0, workers split between 0.35 and 0.70, with the framework explicitly designed to institutionalize the split rather than resolve it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (portable protections for uninsured platform workers) remains LIVE in the structural sense—workers still lack traditional insurance and face catastrophic risk—but the SOLUTION (hybrid framework) now functions partly as a CONTAINMENT MECHANISM that prevents reclassification to employment status. The framework was built to solve a protection gap; it persists because it also solves a platform-cost problem. This is textbook mandatrophy: the original mandate (worker protection) remains real, but the arrangement's operation increasingly institutionalizes precarity (preventing the full-employment reclassification that would provide career security) in order to preserve the cost advantage to platforms. The theater ratio rising from 0.25 to 0.48 is the signature: as the framework matures, the gap between the worker-protection narrative and the operational reality (controlled precarity) widens. This is not a constraint that has become irrelevant (world_unchanged verdict) but one where the mandate has partially outlived its function—the framework now PREVENTS the transition to full employment status that would actually solve the founding problem completely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_of_hybrid_protections,
    'Are the protection tiers offered in the hybrid framework (medical 91.5%, injury 86.2%) sufficient to address the foundational risk that motivated the framework, or do they institutionalize precarity by excluding pension/unemployment/career development?',
    'Longitudinal study of platform worker outcomes (injury rates, health shocks, income volatility, career transitions) compared to employment and independent contractor baseline; regulatory review of protection adequacy relative to worker earnings volatility and biographical risk.',
    'If protections are sufficient, the hybrid framework is a legitimate category with asymmetric but workable extraction. If insufficient, the framework functions primarily as a cost-avoidance mechanism that suppresses full-employment reclassification, and the constraint reclassifies from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_of_hybrid_protections, empirical, 'Whether the hybrid framework''s protection tier is adequate for the risk it purports to address.').

omega_variable(
    algorithmic_control_and_employment_status,
    'Does algorithmic control over work allocation, rate-setting, and task availability constitute ''economic dependence'' sufficient to trigger employment status in substantive employment law, or is algorithmic management structurally distinct from traditional supervisory control?',
    'Judicial determination in employment classification litigation; comparative analysis of algorithmic control mechanisms vs. traditional employment control in cases that have established employment status; regulatory guidance from labor authorities on the employment-dependence threshold.',
    'If algorithmic control constitutes sufficient dependence, the substantive employment reading forecloses the hybrid reading (within a single legal framework, you cannot simultaneously hold ''algorithmic control is not employment'' and ''economic dependence defines employment''). If algorithmic control is sui generis, the hybrid reading coexists with substantive reading as a live policy choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_control_and_employment_status, conceptual, 'Whether algorithmic work allocation constitutes economic dependence in the legal sense.').

omega_variable(
    mandate_obsolescence_vs_institutionalization,
    'Is the rising theater ratio (0.25→0.48) evidence that the hybrid framework''s founding problem is being solved (workers are protected, precarity is managed) or that the mandate is being inverted (the framework now functions to PREVENT full-employment transition that would actually address foundational insecurity)?',
    'Tracking worker progression pathways: if workers transition from platform work to employment at baseline rates (suggesting the founding problem—catastrophic risk—is solved), the framework is fulfilling its mandate. If transitions are blocked or suppressed relative to historical contractor→employment rates, the mandate is inverted.',
    'If the mandate is inverted, base_properties.mandatrophy_resolved should be `true` and the constraint''s classification narrative shifts from ''solving a coordination problem'' to ''institutionalizing precarity under the guise of protection''—a snare-adjacent dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_vs_institutionalization, empirical, 'Whether the hybrid framework is solving the founding problem or preventing its complete solution.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the axioms of this hybrid reading (platform_worker_distinct_category_justified; hybrid_protection_tier_legitimate) logically foreclose the substantive employment reading''s foundational axiom (algorithmic_control_sufficient_dependence), or do the readings occupy different policy frames that coexist?',
    'Analysis of legal reasoning in jurisdiction-level employment classification debates; examination of whether courts or legislatures have attempted to hold both readings simultaneously (suggesting coexistence) or have chosen one over the other (suggesting foreclosure or ordered influence hierarchy).',
    'If readings coexist, they are in contingent competition and either could prevail given different policy contexts or voter coalitions. If one forecloses the other, the kernel has been partially resolved and the losing reading is overridden (not holdable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether this reading''s axioms foreclose sibling readings or coexist with them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 2015, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t2015, employment_boundary__hybrid_security_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(empl_tr_t2017, employment_boundary__hybrid_security_reading, theater_ratio, 2017, 0.32).
narrative_ontology:measurement(empl_tr_t2019, employment_boundary__hybrid_security_reading, theater_ratio, 2019, 0.39).
narrative_ontology:measurement(empl_tr_t2021, employment_boundary__hybrid_security_reading, theater_ratio, 2021, 0.44).
narrative_ontology:measurement(empl_tr_t2023, employment_boundary__hybrid_security_reading, theater_ratio, 2023, 0.47).
narrative_ontology:measurement(empl_tr_t2026, employment_boundary__hybrid_security_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(empl_be_t2015, employment_boundary__hybrid_security_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(empl_be_t2017, employment_boundary__hybrid_security_reading, base_extractiveness, 2017, 0.48).
narrative_ontology:measurement(empl_be_t2019, employment_boundary__hybrid_security_reading, base_extractiveness, 2019, 0.53).
narrative_ontology:measurement(empl_be_t2021, employment_boundary__hybrid_security_reading, base_extractiveness, 2021, 0.56).
narrative_ontology:measurement(empl_be_t2023, employment_boundary__hybrid_security_reading, base_extractiveness, 2023, 0.58).
narrative_ontology:measurement(empl_be_t2026, employment_boundary__hybrid_security_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2015, employment_boundary__hybrid_security_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement(empl_su_t2017, employment_boundary__hybrid_security_reading, suppression_requirement, 2017, 0.56).
narrative_ontology:measurement(empl_su_t2019, employment_boundary__hybrid_security_reading, suppression_requirement, 2019, 0.59).
narrative_ontology:measurement(empl_su_t2021, employment_boundary__hybrid_security_reading, suppression_requirement, 2021, 0.61).
narrative_ontology:measurement(empl_su_t2023, employment_boundary__hybrid_security_reading, suppression_requirement, 2023, 0.62).
narrative_ontology:measurement(empl_su_t2026, employment_boundary__hybrid_security_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__hybrid_security_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__substantive_employment_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel decomposes into three readings. FORMALIST_EMPLOYMENT_READING treats the classification as settled by contract form and direct supervision (mountain-adjacent, no extraction). SUBSTANTIVE_EMPLOYMENT_READING treats algorithmic control and economic dependence as definitional of employment (snare-adjacent, high extraction). HYBRID_SECURITY_READING (this constraint) treats the three-tier classification as a justified institutional response to platform work's unique risk profile (tangled_rope, moderate extraction). All three readings share the same referent (what counts as employment in platform contexts) but diverge on whether the three-category boundary is legitimate, necessary, or a cover story. Each reading has a different ε: formalist ε≈0.05 (no extraction, classification is objective fact), hybrid ε≈0.58 (real coordination + real extraction through cost avoidance), substantive ε≈0.72 (pure extraction masked by classification evasion). The three readings form a kernel family linked by network.affects_constraints; each story carries omegas documenting the reading-level uncertainties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__hybrid_security_reading, powerless, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
