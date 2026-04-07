% ============================================================================
% CONSTRAINT STORY: sunk_cost_fallacy_institutionalized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sunk_cost_fallacy_institutionalized, []).

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
 *   constraint_id: sunk_cost_fallacy_institutionalized
 *   human_readable: Sunk Cost Fallacy Institutionalized
 *   domain: organizational_behavior/institutional_rationality
 *
 * SUMMARY:
 *   Sunk Cost Fallacy Institutionalized describes the structural constraint
 *   created when organizations systematize the cognitive bias of sunk cost
 *   reasoning into formal decision-making procedures. Rather than treating
 *   the fallacy as a correctable cognitive error, the institution embeds it
 *   as a coordination mechanism: 'We must continue this program because we
 *   have already invested X dollars/years in it.' This constraint exhibits
 *   the full spectrum of DR types depending on observational position. For
 *   the employee or stakeholder, it is a Snare — they are trapped by
 *   accumulated sunk investment and cannot exit without absorbing large
 *   costs. For executive leadership, it is a Rope — it provides a
 *   justification mechanism that coordinates organizational stability and
 *   protects their decision-making authority. For the audit function, it is a
 *   Piton — performative procedural accountability while substantive
 *   decision-making is paralyzed. For the analytical observer, it risks
 *   appearing as a Mountain (immutable cognitive law) when it is actually a
 *   contingent institutional choice. The constraint's effectiveness relies on
 *   suppressing the recognition that sunk costs are irrelevant to rational
 *   future allocation. This suppression is enforced through organizational
 *   culture, career consequences, and identity fusion with the institution's
 *   mission — not through formal legal prohibition.
 *
 * KEY AGENTS:
 *   - Executive Leadership: Primary beneficiary (institutional/arbitrage) — sunk cost institutionalization protects their previous investment decisions, prevents the reputational damage of admitting error, and maintains their authority to allocate resources
 *   - Incumbent Program Managers: Secondary beneficiary (powerful/mobile) — sunk cost commitment protects their program budget and career trajectory despite potentially lower performance than alternatives
 *   - Committed Employees: Primary victim (powerless/trapped) — sunk time investment creates career path dependence and identity fusion; forced to publicly rationalize unproductive commitments
 *   - Stakeholder Communities: Secondary victim (moderate/constrained) — suppliers, contractors, and dependent communities face high switching costs and reputational barriers to exit
 *   - Rival Program Advocates: Secondary victim (organized/constrained) — systematically blocked from resource access because capital is locked into incumbent projects justified by sunk cost reasoning
 *   - Middle Managers: Mixed (powerful/mobile but constrained by sunk investment in program portfolio) — experience both coordination function and extraction pressure
 *   - Audit and Accountability Function: Institutional actor maintaining performative (piton) framework — asks procedural questions ('Are we executing efficiently?') rather than existential ones ('Should this continue?')
 *   - Analytical Observer: Universalizing the constraint as natural law rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sunk_cost_fallacy_institutionalized, 0.58).
domain_priors:suppression_score(sunk_cost_fallacy_institutionalized, 0.62).
domain_priors:theater_ratio(sunk_cost_fallacy_institutionalized, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sunk_cost_fallacy_institutionalized, extractiveness, 0.58).
narrative_ontology:constraint_metric(sunk_cost_fallacy_institutionalized, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sunk_cost_fallacy_institutionalized, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sunk_cost_fallacy_institutionalized, tangled_rope).
narrative_ontology:human_readable(sunk_cost_fallacy_institutionalized, "Sunk Cost Fallacy Institutionalized").
narrative_ontology:topic_domain(sunk_cost_fallacy_institutionalized, "organizational_behavior/institutional_rationality").

domain_priors:requires_active_enforcement(sunk_cost_fallacy_institutionalized).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sunk_cost_fallacy_institutionalized, incumbent_leadership).
narrative_ontology:constraint_beneficiary(sunk_cost_fallacy_institutionalized, sunk_investment_holders).
narrative_ontology:constraint_victim(sunk_cost_fallacy_institutionalized, decision_making_rationality).
narrative_ontology:constraint_victim(sunk_cost_fallacy_institutionalized, resource_reallocation_capacity).
narrative_ontology:constraint_victim(sunk_cost_fallacy_institutionalized, exit_option_exercise).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMITTED EMPLOYEE (SNARE) — Trapped by sunk time investment, career path dependence, and identity fusion with the organization's mission. Cannot exit without abandoning years of accumulated expertise and social capital. Maximum extraction: the employee bears the cognitive and emotional cost of cognitive dissonance (publicly defending investments they privately recognize as unproductive) while the organization extracts continued labor at below-market rates justified by 'already invested.' No viable exit.
constraint_indexing:constraint_classification(sunk_cost_fallacy_institutionalized, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: STAKEHOLDER COMMUNITY (SNARE) — Constrained by reputational cost (admitting sunk investment was wasted) and collective action barriers. Communities dependent on the institutionalized project (suppliers, contractors, local economies) face high switching costs. The constraint extracts compliance through sunk social cost rather than legal force. Exits exist but are prohibitively expensive.
constraint_indexing:constraint_classification(sunk_cost_fallacy_institutionalized, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE MANAGER (TANGLED ROPE) — Mobile enough to exit the organization but constrained by sunk investment in the specific program portfolio and career trajectory within it. Experiences both coordination function (their role is to execute the program efficiently) and extraction (mandatory participation in false-consensus building about program viability). Can exercise exit but at a cost — the sunk cost fallacy constrains even powerful agents when internalized.
constraint_indexing:constraint_classification(sunk_cost_fallacy_institutionalized, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE LEADERSHIP (ROPE) — Experiences the constraint as coordination: justifying continued investment maintains institutional stability and prevents the destabilizing admission that previous decisions were errors. Arbitrage option available (can reallocate resources, can exit the commitment, can restructure). Net beneficiary from the constraint — sunk cost fallacy institutionalizes their previous decisions as rational, protecting their reputation and authority. Extraction flows toward this agent.
constraint_indexing:constraint_classification(sunk_cost_fallacy_institutionalized, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RIVAL PROGRAM ADVOCATES (TANGLED ROPE) — Organized agents with alternative visions for resource allocation. Constrained by the institutional enforcement of sunk cost commitment to incumbents. Experience both coordination (the institution does coordinate resource flows) and extraction (their proposals are systematically rejected because resources are tied up in unproductive incumbent projects). This constraint benefits the incumbent program at their expense.
constraint_indexing:constraint_classification(sunk_cost_fallacy_institutionalized, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: AUDIT AND ACCOUNTABILITY FUNCTION (PITON) — Maintains performative commitment to rational resource allocation while operationally institutionalizing sunk cost commitment. The audit process asks 'Are we executing the program efficiently?' (functional) but never 'Should this program continue?' (existential). Theater ratio 0.68 reflects the gap between procedural accountability language and substantive decision-making paralysis. The function persists through institutional inertia despite failing its nominal purpose.
constraint_indexing:constraint_classification(sunk_cost_fallacy_institutionalized, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COGNITIVE LAW VIEW (MOUNTAIN) — From a civilizational perspective, the sunk cost fallacy is an immutable law of human cognition: decision-makers are neurologically hardwired to weight past investments as relevant to future allocation decisions, despite economic rationality saying otherwise. This perspective naturalizes the constraint as inherent to human decision-making. However, the structural data reveals this as a false summit: the fallacy is cognitively universal but institutionally contingent — organizations can design decision architectures that decouple past costs from future allocation (sunset clauses, external review boards, ring-fenced budgets). The 'natural law' framing masks institutional choices.
constraint_indexing:constraint_classification(sunk_cost_fallacy_institutionalized, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sunk_cost_fallacy_institutionalized_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sunk_cost_fallacy_institutionalized, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sunk_cost_fallacy_institutionalized, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sunk_cost_fallacy_institutionalized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sunk_cost_fallacy_institutionalized, TR),
    TR >= 0.70.

:- end_tests(sunk_cost_fallacy_institutionalized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts resources from the broader organization by locking capital into programs justified by past investment rather than future return. The extraction is not maximal (0.70+) because decision-making authority still exists to reallocate (organizational leadership retains formal discretion) — but the institutionalization of sunk cost reasoning makes exercise of that authority organizationally costly. Suppression (0.62): Moderate-high. Suppression operates through multiple mechanisms: career consequences for questioning the commitment (whistleblower penalty), cultural enforcement of the narrative ('we don't quit'), reputational barriers to exit (admitting failure), and resource switching costs (retraining, system migration). Theater ratio (0.68): High and rising. The trajectory shows increasing theater over the interval. Early phase (0.42) reflects relatively honest cost-benefit analysis. Middle phase (0.55) shows growing performative language ('strategic commitment,' 'long-term vision') masking declining actual justification. Late phase (0.68) reflects pure theater — continuation is justified entirely through sunk cost narrative with no rational basis. The rising theater reflects the Goodhart drift: as the program's underlying metrics deteriorate, the institution invests more heavily in procedural justification language.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional structure produces radically different classifications depending on structural position. The organization's formal decision-making authority (executive/institutional) sees the sunk cost commitment as rational coordination (Rope — justifies continued investment, maintains authority, prevents institutional disruption). The employee trapped by accumulated sunk time sees pure extraction (Snare — their continued labor is extracted at below-market rates justified by 'already invested'). The rival program advocate sees asymmetric extraction (Tangled Rope — the system does allocate resources, but systematically favors incumbents over new proposals). The audit function sees its own degradation (Piton — asks procedural questions but cannot ask existential questions, maintaining performance theater while substantive function atrophies). The civilizational analyst risks seeing immutable cognitive law (Mountain — humans are hardwired to weight sunk costs) when the constraint is actually a choice: organizations can design decision architectures that decouple past costs from future allocation. The perspectival gap reveals that 'sunk cost fallacy is natural' is a naturalizing narrative protecting institutional choices from scrutiny.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional flow analysis: Sunk cost commitment functions as a transfer mechanism from (a) employees, stakeholders, and rival programs (high-d agents) to (b) executive leadership and incumbent programs (low-d agents). Employees experience high d (0.85-0.95: trapped victims bearing full suppression and extraction cost). Executives experience low d (0.10-0.20: beneficiaries with arbitrage options). The sigmoid f(d) amplifies this asymmetry: high-d agents experience χ × 1.25-1.40, low-d agents experience χ × -0.10 to 0.20. The constraint works by institutionalizing this directionality asymmetry as 'rational governance' rather than acknowledging it as structured extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID COORDINATION-EXTRACTION STRUCTURE: Sunk Cost Fallacy Institutionalized presents a genuine mandatrophy case. The organization must coordinate resource allocation (a coordination problem exists) AND extract value from subordinates (extraction is structurally embedded). The tangled rope classification resolves the mandatrophy by acknowledging both functions: (1) COORDINATION FUNCTION: The sunk cost commitment does solve a stability problem — organizations that continuously reassess every program would face chaotic reallocation, morale collapse, and decision paralysis. Some commitment mechanism is necessary for organizational coherence. (2) EXTRACTION FUNCTION: The sunk cost mechanism is an INEFFICIENT solution to this coordination problem — it privileges historical decisions over rational evaluation. The extraction occurs because the mechanism overcommits to incumbents and underallocates to new programs, regardless of merit. (3) ACTIVE ENFORCEMENT: The constraint requires continuous enforcement through cultural messaging, career consequences, and reputational management. Without enforcement, employees would rationally reallocate resources to higher-return activities. (4) ASYMMETRIC DISTRIBUTION: The coordination benefit flows toward executives (authority protection, stability) while the extraction cost flows toward employees and rival programs (opportunity costs, constrained careers, resource starvation). This asymmetry is the signature of Tangled Rope. The constraint is NOT pure coordination (would lack the victim class and explicit victims). The constraint is NOT pure extraction (the organization does solve a real coordination problem, even if inefficiently). The constraint IS Tangled Rope: genuine coordination function wrapped in extractive institutions that concentrate the coordination benefit upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_vs_institutional_mechanism,
    'Is the sunk cost fallacy an immutable cognitive bias or an institutionalized choice to weaponize the bias?',
    'Comparative institutional analysis: organizations with sunset clauses, external review boards, and decision segregation show dramatically lower sunk cost entrenchment. This reveals the mechanism is institutional enforcement of the cognitive bias, not the bias itself.',
    'If institutional: extractiveness drops to 0.25-0.35 (Rope/Scaffold). If cognitive/immutable: extractiveness stays at 0.58+ (Tangled Rope/Snare). If hybrid: current classification (0.58, Tangled Rope) is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_vs_institutional_mechanism, empirical, 'Cognitive vs institutional mechanism of sunk cost entrenchment').

omega_variable(
    exit_cost_measurement_ambiguity,
    'What portion of the observed constraint''s suppression derives from real resource switching costs versus reputational or psychological costs?',
    'Post-exit trajectory analysis: if employees/organizations exit the sunk cost trap, does their productivity recover to market baseline? If yes, switching costs were primarily psychological/reputational (extractive). If no, switching costs are real (coordination challenge).',
    'If psychological: suppression is largely internalized, making the constraint more amenable to cognitive reframing. If real: suppression reflects genuine resource constraints and requires structural redesign to overcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement_ambiguity, empirical, 'Real vs internalized suppression mechanisms').

omega_variable(
    coordination_function_authenticity,
    'Does the sunk cost fallacy institutionalization actually solve a coordination problem, or does it only appear to by creating false consensus?',
    'Organizational experiments: bifurcate resource decisions into (a) continued investment in established programs (sunk cost decision frame) and (b) new investments (rational allocation frame). Measure output quality, innovation rate, and resource utilization efficiency across the two frames.',
    'If genuine coordination: classification remains Tangled Rope (mixed coordination and extraction). If false consensus only: classification shifts to pure Snare (extraction with no real coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Whether sunk cost institutionalization solves a genuine coordination problem').

omega_variable(
    identity_lock_scope,
    'Among the agents classified as trapped or constrained, how many are experiencing material barriers versus internalized identity fusion with the sunk cost commitment?',
    'Exit interview and career trajectory analysis post-exit: if agents who exit report rapid reframing of the sunk cost narrative, identity lock was primary. If reports show persistent psychological cost even after material exit, identity fusion was strong.',
    'If identity-locked agents dominate: the constraint operates through cognitive capture more than structural extraction, making it amenable to reframing therapies. If materially trapped agents dominate: structural redesign of decision architecture is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_scope, empirical, 'Scope of identity lock versus material constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sunk_cost_fallacy_institutionalized, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sunkc_tr_t0, sunk_cost_fallacy_institutionalized, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sunkc_tr_t3, sunk_cost_fallacy_institutionalized, theater_ratio, 3, 0.55).
narrative_ontology:measurement(sunkc_tr_t6, sunk_cost_fallacy_institutionalized, theater_ratio, 6, 0.68).
narrative_ontology:measurement(sunkc_tr_t9, sunk_cost_fallacy_institutionalized, theater_ratio, 9, 0.72).

% Extraction over time
narrative_ontology:measurement(sunkc_be_t0, sunk_cost_fallacy_institutionalized, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sunkc_be_t3, sunk_cost_fallacy_institutionalized, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sunkc_be_t6, sunk_cost_fallacy_institutionalized, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(sunkc_be_t9, sunk_cost_fallacy_institutionalized, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sunk_cost_fallacy_institutionalized, resource_allocation).
narrative_ontology:boltzmann_floor_override(sunk_cost_fallacy_institutionalized, 0.18).
narrative_ontology:affects_constraint(sunk_cost_fallacy_institutionalized, escalation_of_commitment).
narrative_ontology:affects_constraint(sunk_cost_fallacy_institutionalized, institutional_inertia).
narrative_ontology:affects_constraint(sunk_cost_fallacy_institutionalized, organizational_failure_invisibility).

% DUAL FORMULATION NOTE:
% Sunk Cost Fallacy Institutionalized is upstream of escalation of commitment (which is the behavioral manifestation) and institutional inertia (which is the structural persistence mechanism). The network captures the causal flow: cognitive bias → institutional embedding → behavioral escalation → persistent inertia.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sunk_cost_fallacy_institutionalized, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
