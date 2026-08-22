% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__developmental_state_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Flexible Employment as Developmental Transition (State-Managed Reading)
 *   domain: labor_economics/social_policy
 *
 * SUMMARY:
 *   Flexible employment (gig, platform, contract-based work without formal
 *   employment status) is treated under the developmental-state reading as a
 *   TEMPORARY institutional form — economically functional during a growth
 *   phase when state capacity and social infrastructure are being built, but
 *   architecturally scheduled to be superseded by formalization once
 *   conditions are met. The constraint in this reading is the state-managed
 *   transition schedule itself: a 12-point plan with 2027 target for
 *   standardization, incrementally raising employer compliance obligations
 *   (benefits provision, contract terms, social insurance contributions) and
 *   expanding worker protections. The reading frames flexible employment as
 *   STAGE 2 of a sequential development model (Stage 1: informal, Stage 2:
 *   flexible/transitional, Stage 3: formalized). The constraint's legitimacy
 *   depends entirely on credible delivery of the transition — if the 2027
 *   target is missed, the reading collapses into precarity-extraction
 *   (permanent flexibility without growth). If formalization is achieved, the
 *   constraint self-extinguishes (sunsets by success). This reading opposes
 *   both the market-efficiency reading (which treats flexible work as optimal
 *   steady-state) and the precarity-extraction reading (which treats it as
 *   permanent exploitative arrangement).
 *
 * KEY AGENTS:
 *   - State labor agencies (agenda-setter, enforcers of the 12-point plan)
 *   - Platform operators (payers, compliance-cost escalation)
 *   - Flexible workers (beneficiaries of the transition pathway)
 *   - Organized labor (beneficiary + observer, plan participation)
 *   - Excluded workers (trapped outside the formalization pathway)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.38).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.42).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, scaffold).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as Developmental Transition (State-Managed Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "labor_economics/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:has_sunset_clause(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, 'ee4b47e9-4f75-4238-8897-e85495ce0ef7').
narrative_ontology:cs_kernel_codification('ee4b47e9-4f75-4238-8897-e85495ce0ef7', fixed_text).
narrative_ontology:cs_authority_grounding('ee4b47e9-4f75-4238-8897-e85495ce0ef7', extraction).
narrative_ontology:cs_interpretation_layer_present('ee4b47e9-4f75-4238-8897-e85495ce0ef7').
narrative_ontology:cs_reading_relation('ee4b47e9-4f75-4238-8897-e85495ce0ef7', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee4b47e9-4f75-4238-8897-e85495ce0ef7', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('ee4b47e9-4f75-4238-8897-e85495ce0ef7', foundational, flexible_employment_is_temporary_institutional_form).
narrative_ontology:cs_axiom_status(flexible_employment_is_temporary_institutional_form, holdable).
narrative_ontology:cs_axiom_grounding('ee4b47e9-4f75-4238-8897-e85495ce0ef7', flexible_employment_is_temporary_institutional_form, instrumental).
narrative_ontology:cs_axiom('ee4b47e9-4f75-4238-8897-e85495ce0ef7', foundational, state_managed_formalization_schedule_is_credible_commitment).
narrative_ontology:cs_axiom_status(state_managed_formalization_schedule_is_credible_commitment, holdable).
narrative_ontology:cs_axiom_grounding('ee4b47e9-4f75-4238-8897-e85495ce0ef7', state_managed_formalization_schedule_is_credible_commitment, conventional).
narrative_ontology:cs_reference_frame('ee4b47e9-4f75-4238-8897-e85495ce0ef7', flexible_employment_as_development_stage_two).
narrative_ontology:cs_drift_state('ee4b47e9-4f75-4238-8897-e85495ce0ef7', contemporary_plan_midpoint_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ee4b47e9-4f75-4238-8897-e85495ce0ef7', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, formalization_pathway_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, state_capacity_builders).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, structured_labor_market).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, organized_labor).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, platform_operators).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, sequential_development_doctrine).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, state_managed_transition_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers in flexible arrangements who are treated (under this reading) as participants in a managed transition toward formal employment. The constraint frames their current precarity as temporary — a necessary intermediate step while state capacity, social security infrastructure, and employer compliance mechanisms are built. They gain gradually improving protections, wage floors, and contractual clarity according to the 12-point plan timeline. Their exit from flexible work is the intended endpoint of the constraint's operation.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, formalization_pathway_workers, beneficiary,
    moderate, generational, constrained, national).

% State agencies (labor ministry, tax authority, social insurance administrator) that set the formalization schedule, enforce compliance milestones, build digital infrastructure for worker registration and benefit accrual, and coordinate private-sector participation. They set and administer the constraint via the 12-point plan and credibly commit to the 2027 standardization target. Their legitimacy under this reading depends on delivering on the transition schedule.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, state_capacity_builders, agenda_setter,
    institutional, generational, analytical, national).

% Digital labor platforms and gig operators that currently deploy flexible workers at reduced compliance cost. Under this reading, they are transitional participants required to incrementally formaliza their workforce according to the state's schedule. They bear the rising cost of benefits, compliance infrastructure, and wage floors as the plan unfolds. Their option to exit is constrained by regulatory mandate; their option to freeze at the current flexible model is foreclosed by the constraint's enforcement.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_operators, payer,
    institutional, biographical, constrained, global).

% Unions and labor federations that support the developmental-state reading as an alternative to both market-efficiency narratives (which would lock flexible work in permanently) and pure precarity extraction (which they oppose). They participate in plan design and milestone monitoring. They benefit from formalization as it expands union membership and collective bargaining scope. They retain the option to withdraw support if the state fails milestones.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, organized_labor, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, organized_labor, observer).

% The vindicated institutional outcome: a labor market where formal employment is the norm, flexible work is limited to genuinely temporary contexts, and worker protections apply universally. This is the constraint's terminal state (not an actor, but the structural goal the reading commits to).
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, structured_labor_market, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(flexible_employment_legitimacy__developmental_state_reading, structured_labor_market).

% Regulatory bodies monitoring whether the formalization schedule disrupts labor market functionality or concentrates bargaining power. They track whether platform operators coordinate to resist compliance, whether the state's infrastructure investments are adequate, and whether wage growth tracks the plan or lags.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% Workers who cannot meet formalization eligibility criteria (undocumented, debt-trapped, without collateral for microfinance pathways, or in jurisdictions where the plan is not yet deployed). They are structurally excluded from the constraint's protections and transition pathway. Under this reading, they are treated as a priority for acceleration, but the schedule-driven constraint produces a cohort left behind during the transition.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, flexible_workers_locked_out, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, diffuse).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__developmental_state_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages an economically necessary but temporally bounded transition from informal/flexible work to formal employment by coordinating state infrastructure build-out, employer compliance escalation, and worker transitions. Solves the sequencing problem: workers cannot be formalized at scale without social security systems in place; employers cannot absorb compliance costs all at once; the state cannot build infrastructure instantly. The constraint staggers these.
% TRANSFER_FUNCTION: Moves compliance cost and social insurance burden from individual workers (currently borne via precarity and uninsured risk) progressively onto employers and the state according to a published schedule. Also moves state resources toward labor administration infrastructure, worker registration systems, and benefit accrual mechanisms. The constraint structures WHEN the costs move, not WHETHER they move.
% ABSENT_VOICES: Flexible workers whose exit from formal pathways pre-dates the plan, workers in shadow economies, workers in jurisdictions not covered by the state's infrastructure plan, and platform operators' investors who would prefer the flexible model remain indefinitely. Their absence from the planning table means formalization pathways may not match their actual constraints (debt, documentation, skill gaps, care responsibilities).
% DISAPPEARANCE_RATIONALE: If the constraint and its state-managed transition vanished, platforms would have no credible pressure to formalize; workers would remain in flexible arrangements indefinitely (this reading's counterfactual); the state would lose its coherence mechanism for infrastructure investment; wage growth would freeze at current precarity levels. The developmental-state reading's entire justification IS that without the constraint, the transition does not happen of its own accord.
% FOUNDING_PROBLEM: Flexible employment systems generate economic efficiency (labor supply-demand matching, platform scaling) but leave workers uninsured and unprotected; direct formalization at scale is economically infeasible (employer compliance burden is too sudden, worker absorption is too fast for skill/credential verification, state administration cannot handle volume all at once). A managed, staged transition is required to achieve both efficiency-enabled growth AND worker protections.
% FOUNDING_PROBLEM_CORROBORATION: Stated by developmental-state policymakers, endorsed by labor unions and ILO technical advisors. CONTESTED by market-efficiency analysts (who argue direct formalization is unnecessary and slows growth) and precarity researchers (who argue the constraint merely delays extraction rather than preventing it). External corroboration from the state side: infrastructure commissioning reports, employment rate data showing stalling formalization during flexible-work phases, social insurance coverage gaps in flexible sectors.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).
:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is DECLINING over the interval (0.52 → 0.38) because the constraint's design is to reduce extraction by raising floors and formalizing protections. Suppression is STABLE-LOW (0.42-0.48) because the constraint operates through published targets and regulatory schedule, not through coercive mechanisms hiding from workers. Theater ratio is MODERATE (0.35 declining to 0.28) because part of the enforcement activity is genuine infrastructure building (real cost), but some is performative milestone-hitting (political theater of plan adherence). The developmental-state reading predicts extractiveness declines as formalization advances; a flat or rising extractiveness would falsify the reading and suggest the constraint is actually a snare masquerading as transition. Measurements are OBSERVED through year 5 (plans already published, early compliance data available), then PROJECTED based on the 2027 target (assuming plan delivers). If real extractiveness stays flat or rises by 2027, the reading fails empirically.
 *
 * PERSPECTIVAL GAP:
 *   The platform operator seat and the worker beneficiary seat compute radically differently. From the operator's position: rising cost mandate with no market rationale — classic extraction. From the worker's position: credible pathway to protections that flexible market competition never delivers — genuine coordination. The engine should compute operator-seat as snare-ward and worker-seat as rope-ward from the same structural data, because power and exit options diverge sharply. This divergence IS the finding the framework exists to capture — the developmental-state reading is only plausible if state POWER is strong enough to override market-efficiency exit options, creating coordination where markets would deadlock.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (formalization pathway workers, organized labor, state capacity builders as legitimacy-vindicators) derive d from: beneficiary role + receiving formal protections + gaining state support for formalization. Derivation path is d_beneficiary. Payers (platform operators constrained by regulatory mandate) derive d from: payer role + forced compliance escalation + constrained exit. Derivation path is d_target. Excluded workers derive d from: powerless + trapped exit + no access to state-managed pathway — they are targets of the constraint's indifference, a secondary extraction mechanism. Workers locked out get higher d than formalization-pathway workers despite both being 'workers' because their exit options are MORE constrained and power is LOWER.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is straightforward: transition flexible to formalized employment by 2027. The TEST is simple: does extractiveness decline as predicted and formalization increase? If yes, the constraint is functioning (scaffold as designed) or has succeeded (sunsets). If extractiveness stays flat or rises while formalization stalls, the founding problem (sequencing bottleneck) was not real, or was not solved by this constraint — the reading collapses into precarity-extraction (the same flexible arrangement defended by the state instead of market forces). Mandatrophy_resolved = FALSE currently (interval is 0-15, target is 2027, roughly year 10 of the series). At measurement point 15 (projected 2027), if extractiveness is still 0.38 and formalization rate meets targets, mandate is LIVE and fulfilled. If extractiveness rises past 0.5 before 2027, mandate is DEAD (the transition is not happening; the reading falsified itself).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_capacity_credibility,
    'Will the state actually deliver the 12-point plan and 2027 formalization target, or will infrastructure investment stall and the timeline slip indefinitely?',
    'Milestone tracking: infrastructure commissioning dates, worker registration rates, employer compliance audit results, wage growth tracking against plan, formalization rate trajectory through 2026-2027.',
    'If state delivery falters, the constraint collapses from scaffold (temporary + credible endpoint) into either piton (theatrical compliance theater without real transition) or snare (permanent flexibility masked by transition narrative). Extractiveness would stabilize high (0.5+) and theater ratio would rise above 0.5, signaling performative maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_credibility, empirical, 'Whether the developmental-state reading''s mandate can be executed.').

omega_variable(
    platform_operator_flight_risk,
    'Will platform operators attempt regulatory arbitrage (exit to jurisdictions without the plan, or vertically integrate into full-employment to avoid compliance) rather than incremental formalization?',
    'Platform exit/redomiciling data, vertical integration attempts, black-market labor subcontracting growth, geographic shifting of operations.',
    'Flight risk is the hidden SUPPRESSION mechanism: if platforms can exit to arbitrage zones, the state''s mandated transition applies only to immobile operators and workers, creating a two-tier labor market. Suppression would rise (0.6+) as the state escalates enforcement to plug exits. The reading would degrade toward a snare (extraction masquerading as transition).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_operator_flight_risk, empirical, 'Whether the constraint''s coercive force is strong enough to prevent regulatory evasion.').

omega_variable(
    formalization_wage_dynamics,
    'As workers transition from flexible to formal employment, do real wages rise, stay flat, or fall (employers reduce hourly rates to offset benefit costs)?',
    'Wage data for cohorting workers through the transition; total compensation (wages + benefits) tracking; labor supply response to total-compensation changes.',
    'If total compensation rises, the constraint delivers real worker benefit and is genuine coordination. If total compensation stays flat or falls (wage rate drops to offset benefits), the constraint is zero-sum transfer: workers trade flexibility for protections but no income gain. A flat-compensation transition would classify the constraint as tangled-rope (coordination + extraction, not pure coordination or pure extraction). This measurement drives the mandatrophy question: does the ''development'' in ''developmental state'' mean actual welfare gain for workers, or institutional reorganization with no net benefit?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalization_wage_dynamics, empirical, 'Whether formalization produces real income gains or compensating-differential wage cuts.').

omega_variable(
    excluded_worker_amplification,
    'Does the formalization plan systematically amplify precarity for workers OUTSIDE its eligibility criteria (undocumented, debt-trapped, skill-unverified), as the state''s infrastructure investments concentrate benefits on the in-pathway cohort?',
    'Cohort-level wage and precarity tracking for excluded vs. pathway-eligible workers; comparative precarity drift before/after plan implementation; shadow-economy growth rates.',
    'If excluded workers experience RISING precarity (wages falling, hours cut, supervision tightening as formalized sectors contract and platforms consolidate) while pathway workers experience improvements, the constraint is not a general transition but a TIERED extraction mechanism. The victims of the constraint are not the workers transitioning (beneficiaries), but the workers left behind. This would reframe the reading as a snare targeting powerless, excluded cohorts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_worker_amplification, empirical, 'Whether the transition pathway systematically excludes and harms its non-eligible cohorts.').

omega_variable(
    reading_boundary_framework_containment,
    'Can the developmental-state reading (transition temporary, formalize by 2027) and the market-efficiency reading (flexibility permanent, market-optimal) coexist within a single policy framework, or do they require incompatible institutional commitments?',
    'Policy discourse analysis: do state officials simultaneously endorse the 2027 target AND defend a permanent role for flexible work? Do legislative texts authorize parallel paths (formalization pathway + protected flexible framework)? Do regulatory agencies enforce contradictory definitions of legitimate flexibility?',
    'If the readings CAN coexist (state commits to both timelines and both frameworks), the constraint is CONCEPTUALLY HYBRID and the reading boundary is not a real foreclosure. The developmental-state reading would not actually rule out market-efficiency; instead they would form a hybrid ''regulated flexibility'' regime. This would reframe the cs_structure relation from ''forecloses'' to ''coexists_with'' and reclassify the constraint as tangled-rope (coordination + market-mechanism hybridization). If the readings CANNOT coexist without contradiction, a genuine foreclosure exists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_framework_containment, conceptual, 'Whether the developmental-state and market-efficiency readings are logically incompatible or can be hybridized into a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(flex_tr_t0, observed).
narrative_ontology:measurement(flex_tr_t2, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2, 0.32).
narrative_ontology:measurement_basis(flex_tr_t2, observed).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(flex_tr_t5, observed).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(flex_tr_t8, projected).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(flex_tr_t12, projected).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(flex_tr_t15, projected).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(flex_be_t0, observed).
narrative_ontology:measurement(flex_be_t2, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement_basis(flex_be_t2, observed).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(flex_be_t5, observed).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement_basis(flex_be_t8, projected).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement_basis(flex_be_t12, projected).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(flex_be_t15, projected).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(flex_su_t0, observed).
narrative_ontology:measurement(flex_su_t2, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement_basis(flex_su_t2, observed).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement_basis(flex_su_t5, observed).
narrative_ontology:measurement(flex_su_t8, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(flex_su_t8, projected).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(flex_su_t12, projected).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(flex_su_t15, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__developmental_state_reading, 0.18).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).

% DUAL FORMULATION NOTE:
% The flexible_employment_legitimacy kernel decomposes into three structurally distinct constraints differing on ε, endpoint, and natural vs. constructed status. This story (developmental_state_reading) treats flexible employment as temporary institutional form with state-managed endpoint. Market_efficiency_reading treats it as permanent equilibrium (ε lower, reads as coordination). Precarity_extraction_reading treats it as permanent extraction mechanism (ε higher, reads as snare). Each reading has independent ε-stability: changing the reading does not change ε; each reading's ε is fixed over the same referent (standing flexible-employment arrangement, assessed by that reading's lights). The three readings are linked by affects_constraints edges representing influence (upstream developmental-state reading creates pressure on downstream market-efficiency and precarity-extraction readings by staking out a state-commitments framing that competitors must respond to).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__developmental_state_reading, powerless, 0.85).
constraint_indexing:directionality_override(flexible_employment_legitimacy__developmental_state_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
