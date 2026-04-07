% ============================================================================
% CONSTRAINT STORY: asynchronous_work_viability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asynchronous_work_viability, []).

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
 *   constraint_id: asynchronous_work_viability
 *   human_readable: Asynchronous Work Viability in Distributed Organizations
 *   domain: organizational_structure/labor/technology
 *
 * SUMMARY:
 *   The viability of asynchronous work represents a structural tension
 *   between geographic flexibility and real-time coordination. Organizations
 *   increasingly adopt asynchronous-first policies to access global talent,
 *   reduce real estate costs, and enable flexible schedules. However, the
 *   constraint exhibits mixed properties: it coordinates genuine benefits
 *   (schedule flexibility, talent pool expansion, deep focus time) alongside
 *   extraction mechanisms (mentorship loss, career visibility gaps, decision
 *   latency, timezone-based labor asymmetries). The constraint's
 *   classification varies dramatically across perspectives because different
 *   agents experience different ratios of coordination benefit to extractive
 *   cost. Knowledge workers with flexibility needs see rope; time-sensitive
 *   role workers see snare; junior employees see tangled rope with asymmetric
 *   costs. The theater ratio (0.58) reflects that async-first organizations
 *   create performative overhead: lengthy documentation processes, decision
 *   trees, async-first mandates that require synchronous workarounds, and
 *   cultural ideology asserting async superiority despite mixed evidence.
 *
 * KEY AGENTS:
 *   - Knowledge Workers with Flexible Constraints: Primary beneficiary (institutional/arbitrage) — access flexibility, deep focus, geographic mobility without major exit costs
 *   - Workers in Time-Sensitive Roles: Primary victim (powerless/trapped) — require synchronous coordination but trapped in async organizational design; cannot exit without role change
 *   - Global Talent Pool: Secondary beneficiary (moderate/mobile) — expanded employment opportunities in distributed labor markets; some exit optionality through competitive job search
 *   - Junior Employees and Underrepresented Demographics: Secondary victim (moderate/constrained) — lose informal mentorship and network access; constrained by career dependence on organizational investment
 *   - Organizational Management: Tertiary beneficiary (institutional/arbitrage) — cost reduction, talent pool expansion, geographic reach; high exit optionality through policy adjustment
 *   - Synchronous-Dependent Workflows: Abstract victim (powerless/trapped) — certain work types (debugging, rapid prototyping, complex negotiation) degrade in async environments; no agency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asynchronous_work_viability, 0.52).
domain_priors:suppression_score(asynchronous_work_viability, 0.48).
domain_priors:theater_ratio(asynchronous_work_viability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asynchronous_work_viability, extractiveness, 0.52).
narrative_ontology:constraint_metric(asynchronous_work_viability, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(asynchronous_work_viability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asynchronous_work_viability, tangled_rope).
narrative_ontology:human_readable(asynchronous_work_viability, "Asynchronous Work Viability in Distributed Organizations").
narrative_ontology:topic_domain(asynchronous_work_viability, "organizational_structure/labor/technology").

domain_priors:requires_active_enforcement(asynchronous_work_viability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asynchronous_work_viability, knowledge_workers_with_flexible_constraints).
narrative_ontology:constraint_beneficiary(asynchronous_work_viability, global_talent_pool).
narrative_ontology:constraint_beneficiary(asynchronous_work_viability, organizations_with_geographic_reach).
narrative_ontology:constraint_victim(asynchronous_work_viability, workers_in_time_sensitive_roles).
narrative_ontology:constraint_victim(asynchronous_work_viability, organizational_cohesion).
narrative_ontology:constraint_victim(asynchronous_work_viability, junior_employees_without_tacit_knowledge).
narrative_ontology:constraint_victim(asynchronous_work_viability, synchronous_dependent_workflows).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TIME-SENSITIVE ROLE WORKER (SNARE) — Trapped in asynchronous-first organizational design despite their actual work requiring synchronous coordination. Cannot exit without abandoning employment or role change. Bears full cost of communication lag, context switching, and reduced real-time problem solving while benefiting minimally from async flexibility. Maximum extraction experienced.
constraint_indexing:constraint_classification(asynchronous_work_viability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: KNOWLEDGE WORKER WITH FLEXIBILITY (TANGLED ROPE) — Genuinely benefits from asynchronous work enabling schedule flexibility, reduced commute, and deep focus time. But also experiences extraction through reduced real-time mentorship, slower decision-making, career visibility gaps, and social isolation. Constrained by needing employment; moderate because options exist (job market for async-friendly roles, negotiation capacity).
constraint_indexing:constraint_classification(asynchronous_work_viability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ORGANIZATIONAL MANAGEMENT (ROPE) — Experiences asynchronous-first policy as pure coordination: expands talent pool, reduces real estate costs, enables follow-the-sun development, and distributes cognitive labor across time zones. Net beneficiary with high exit optionality (can shift policy, transition to hybrid, manage through culture). Low extraction experienced.
constraint_indexing:constraint_classification(asynchronous_work_viability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JUNIOR EMPLOYEE / UNDERREPRESENTED DEMOGRAPHIC (TANGLED ROPE) — Nominally benefits from async flexibility and reduced bias in synchronous meeting participation. But experiences severe extraction through loss of spontaneous mentorship, reduced informal learning, invisible career progression, and lower access to informal power networks. Constrained by career stage dependence on organizational investment.
constraint_indexing:constraint_classification(asynchronous_work_viability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ASYNC-NATIVE CULTURE NARRATIVE (PITON) — The ideological assertion that 'synchronous work is outdated' persists through institutional inertia and tribal commitment despite weak evidence that async-first actually scales to complex coordination. Theater is high: lengthy async decision processes, decision trees, documentation overhead, and performative responsiveness masquerade as efficiency. The constraint is maintained because a committed cohort has identity-fused with 'async culture' even as the actual function (productive asynchronous coordination) degrades.
constraint_indexing:constraint_classification(asynchronous_work_viability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION PROPAGATION VIEW (MOUNTAIN) — From a deep analytical angle, some verification and coordination lag is inherent to asynchronous systems: information must propagate through asynchronous channels, context must be transmitted documentarily rather than through real-time interaction. This perspective risks naturalizing contingent organizational choices (async-first mandates, synchronous meeting restrictions) as universal laws of distributed work. The engine should flag this as a false summit — the structural data reveals institutional choices, not physical limits.
constraint_indexing:constraint_classification(asynchronous_work_viability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asynchronous_work_viability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asynchronous_work_viability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asynchronous_work_viability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asynchronous_work_viability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asynchronous_work_viability, TR),
    TR >= 0.70.

:- end_tests(asynchronous_work_viability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The asynchronous work constraint extracts through multiple channels: delay costs in time-sensitive work, mentorship loss for junior employees, career visibility compression, timezone-based labor timing asymmetries, and decision latency in complex coordination. However, the extraction is not total because significant coordination benefit is real — schedule flexibility, deep work time, and talent pool access are genuine. The value reflects that the extraction masks legitimate coordination function. Suppression (0.48): Moderate. Barriers to rejecting async-first include organizational policy, cultural commitment, career risk of visible non-compliance, and lack of alternative role options. But suppression is not absolute — some workers can negotiate flexibility, remote workers can relocate to align time zones, organizations can transition to hybrid. Theater ratio (0.58): Moderate-high. Async-first organizations create substantial performative overhead: the requirement to document all context (that would be conveyed synchronously), lengthy decision-making processes optimized for async review, and cultural narratives asserting async superiority despite mixed evidence. This theater increased over the interval as organizations committed more deeply to async-first ideology and required more documentation and async processes to maintain the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between institutional beneficiaries (org/arbitrage) and individual victims in time-sensitive or junior roles (powerless/trapped or moderate/constrained). The organization experiences asynchronous work as solving coordination problems; the individual in a time-sensitive role experiences it as imposed extraction. Knowledge workers with flexibility needs occupy the middle ground — they see genuine benefits but also real costs. The junior employee perspective is particularly diagnostic: async-first organizations often claim this is *fair and inclusive* (no synchronous meeting dominance by overlapping time zones), but the actual outcome is extraction through invisible career trajectories and lost mentorship. The theater ratio of 0.58 suggests the constraint maintains itself partly through performative commitment ('we are a modern, distributed-first organization') rather than through demonstrated functional necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by role and structural relationship. Knowledge workers with genuine flexibility needs experience low d (beneficiaries with exit options via job market negotiation) — they perceive the constraint as serving them. Time-sensitive role workers experience high d (victims with limited role-specific options) — they perceive the constraint as extraction. Junior employees experience moderate-high d (victims constrained by career dependence, not absolute barriers) — they perceive asymmetric costs. Organizations experience low d (beneficiaries with policy arbitrage) — they can shift to hybrid if async fails. The constraint's effective extractiveness (χ) is modulated by these position-specific d values: what counts as coordination for the organization (low d → low χ → rope) counts as extraction for the junior employee (high d → high χ → tangled rope / snare).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint exemplifies mandatrophy between the organization's claim that 'asynchronous work is viable and preferable' and the empirical evidence from role-specific and demographic perspectives. The organization experiences asynchronous work as pure coordination (Rope), which is a valid observational claim from their structural position. But the constraint simultaneously exhibits snare-like properties (forced async in time-sensitive work), piton-like properties (performative async theater that persists despite functional cost), and tangled-rope properties (mixing coordination benefit with asymmetric extraction). The mandatrophy is resolved by recognizing that the question 'Is asynchronous work viable?' has different answers depending on: (1) the worker's role (time-sensitive vs knowledge work), (2) the worker's career stage (junior needing mentorship vs established), (3) the specific work type (debugging requires sync; documentation does not), and (4) the organization's actual implementation (sync-first with async documentation vs async-first with sync exceptions). The constraint is not a single claim but a presheaf of role-dependent viability claims, each with its own classification. The analytical observer's 'mountain' claim risks naturalizing this as inherent to distributed work when it is actually a set of specific organizational choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_transmission_viability,
    'Can tacit knowledge, context, and nuanced judgment transfer effectively through asynchronous documentation or does it require synchronous apprenticeship?',
    'Longitudinal cohort analysis comparing learning outcomes and task competency for async-trained vs synchronously-mentored junior employees; measurement of error rates and decision quality by mentorship mode',
    'If tacit transmission via async is viable: junior employee extraction is overstated; asynchronous scaling is more feasible. If it requires sync: junior employee perspective is diagnostic of deep organizational dysfunction; async-first mandates are extractive for career development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_transmission_viability, empirical, 'Whether asynchronous documentation can transmit tacit professional knowledge').

omega_variable(
    coordination_complexity_threshold,
    'At what organizational complexity level does asynchronous decision-making become infeasible or produce worse outcomes than synchronous coordination?',
    'Correlation analysis of decision latency, rework rate, and project success by organizational size and async depth; case studies of organizations that reverted from async-first; measurement of decision quality under time pressure',
    'If threshold is low (small teams): async-first is sustainable but limited; most organizations should hybrid. If threshold is high (large teams): async-first is genuinely viable at scale; extraction is containable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_complexity_threshold, empirical, 'Complexity threshold where async coordination degrades').

omega_variable(
    identity_lock_in_async_culture,
    'Is organizational commitment to async-first a genuine functional choice or an identity-fused ideology that persists despite performance costs?',
    'Measurement of actual async-to-sync ratio in critical decisions; comparison of stated async policy vs actual meeting frequency; analysis of whether policy rigidity (punishing synchronous collaboration) persists despite dysfunction',
    'If genuine choice: piton classification is overstated; theater is lower than measured. If identity-locked: piton is diagnostic; the constraint is maintained through ideological commitment rather than functional necessity, suggesting high mandatrophy risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_in_async_culture, empirical, 'Whether async-first commitment is functional or identity-based ideology').

omega_variable(
    career_visibility_compensation_mechanisms,
    'Do asynchronous organizations effectively compensate for reduced informal visibility with documented performance metrics and transparent advancement criteria, or does async invisibility persist despite formal mechanisms?',
    'Analysis of promotion patterns (demographic representation, timing, sponsorship pathways) in async-first vs hybrid orgs; survey data on career satisfaction by role and seniority; measurement of whether advancement is decoupled from synchronous visibility',
    'If compensation is effective: junior employee extraction is overstated. If informal networks persist: async environment masks but does not solve bias problems, and junior employee victims classification is precise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_visibility_compensation_mechanisms, empirical, 'Whether async orgs compensate for visibility loss in career progression').

omega_variable(
    timezone_exploitation_gradient,
    'Does asynchronous coordination systematically require certain time zones to work at off-hours or to wait disproportionately for response, creating structural timezone-based extraction?',
    'Temporal analysis of meeting times, response latencies, and synchronous vs async work distribution by time zone; measurement of whether some zones consistently work outside standard hours; survey of timezone-related fatigue and schedule compression',
    'If yes: beneficiary classification is more precise; extraction flows toward organization (efficiency) and away from distributed workers in specific time zones. If no: global reach benefit is genuine and not masked by hidden timezone cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(timezone_exploitation_gradient, empirical, 'Whether async coordination creates timezone-based work extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asynchronous_work_viability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(async_tr_t0, asynchronous_work_viability, theater_ratio, 0, 0.38).
narrative_ontology:measurement(async_tr_t3, asynchronous_work_viability, theater_ratio, 3, 0.5).
narrative_ontology:measurement(async_tr_t6, asynchronous_work_viability, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(async_be_t0, asynchronous_work_viability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(async_be_t3, asynchronous_work_viability, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(async_be_t6, asynchronous_work_viability, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asynchronous_work_viability, resource_allocation).
narrative_ontology:affects_constraint(asynchronous_work_viability, real_time_decision_making_in_distributed_teams).
narrative_ontology:affects_constraint(asynchronous_work_viability, junior_employee_career_development).
narrative_ontology:affects_constraint(asynchronous_work_viability, timezone_labor_equity).

% DUAL FORMULATION NOTE:
% Asynchronous work viability decomposes into role-specific constraints: (1) async feasibility for knowledge work (high ε, Tangled Rope), (2) async feasibility for time-sensitive coordination (high ε, Snare), (3) mentorship transmission in async environments (high ε, Snare), (4) timezone equity in distributed organizations (moderate ε, Tangled Rope). Each has distinct empirical status and victim/beneficiary profiles. The present story is the parent constraint aggregating across roles; downstream stories detail specific domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(asynchronous_work_viability, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
