% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Flexible Employment as State-Managed Transition Toward Formalization
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the developmental-state reading of the
 *   flexible-employment-legitimacy kernel: platform work is not a stable
 *   category but a managed transitional form, actively steered by the state
 *   via a 12-point plan toward a 2027 standardization deadline that converts
 *   flexible classification into formal employment protections. Under this
 *   reading, the coordination function is real — the ministry, platforms, and
 *   workers share a scheduled endpoint — but the transition has been extended
 *   and re-milestoned enough that the scaffold's sunset is increasingly
 *   nominal, which is why theater_ratio and suppression_requirement both
 *   drift upward across the interval even as the reading's own framing
 *   insists the endpoint is approaching, not receding. The sibling readings
 *   (market_efficiency_reading, precarity_extraction_reading) describe the
 *   same underlying platform-labor arrangement but attribute different
 *   structural functions to it and are authored as separate constraints per
 *   the ε-invariance principle; the standardization target, the 12-point
 *   plan, and wage growth are read here as kernel-stabilizing devices —
 *   instruments of authority reassertion and managed convergence — not as
 *   evidence of market clearing or as cover for extraction. Those alternative
 *   readings assign different ε and different beneficiary/victim structures
 *   to the same nominal 'flexible employment' concept.
 *
 * KEY AGENTS:
 *   - labor_ministry_planners: agenda_setter (institutional/analytical) — designs and administers the transition timeline
 *   - platform_operators: beneficiary (powerful/mobile) — operates under lighter obligations during the declared transition
 *   - gradually_formalizing_workers: beneficiary/payer (moderate/constrained) — receives incremental entitlements but bears schedule uncertainty
 *   - gig_workers_in_extended_transition: payer (powerless/trapped) — bears the cost of a transition that keeps extending
 *   - informal_sector_incumbents: payer (powerless/trapped) — deprioritized while state capacity serves the platform track
 *   - platform_worker_unions: excluded (organized/constrained) — consulted but without milestone-setting authority
 *   - independent_labor_economists: observer (analytical) — the external check on whether convergence is actually occurring
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.52).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.44).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, scaffold).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as State-Managed Transition Toward Formalization").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:has_sunset_clause(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, 'dbaf5a94-43f2-469e-b023-c28fc54c0536').
narrative_ontology:cs_kernel_codification('dbaf5a94-43f2-469e-b023-c28fc54c0536', formalized).
narrative_ontology:cs_authority_grounding('dbaf5a94-43f2-469e-b023-c28fc54c0536', extraction).
narrative_ontology:cs_interpretation_layer_present('dbaf5a94-43f2-469e-b023-c28fc54c0536').
narrative_ontology:cs_reading_relation('dbaf5a94-43f2-469e-b023-c28fc54c0536', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbaf5a94-43f2-469e-b023-c28fc54c0536', flexible_employment_legitimacy__precarity_extraction_reading, influences).
narrative_ontology:cs_axiom('dbaf5a94-43f2-469e-b023-c28fc54c0536', foundational, state_retains_authority_to_terminate_flexible_classification).
narrative_ontology:cs_axiom_status(state_retains_authority_to_terminate_flexible_classification, holdable).
narrative_ontology:cs_axiom_grounding('dbaf5a94-43f2-469e-b023-c28fc54c0536', state_retains_authority_to_terminate_flexible_classification, conventional).
narrative_ontology:cs_axiom('dbaf5a94-43f2-469e-b023-c28fc54c0536', secondary, wage_growth_is_engineered_convergence_not_spontaneous_clearing).
narrative_ontology:cs_axiom_status(wage_growth_is_engineered_convergence_not_spontaneous_clearing, holdable).
narrative_ontology:cs_axiom_grounding('dbaf5a94-43f2-469e-b023-c28fc54c0536', wage_growth_is_engineered_convergence_not_spontaneous_clearing, empirically_contingent).
narrative_ontology:cs_reference_frame('dbaf5a94-43f2-469e-b023-c28fc54c0536', state_managed_developmental_transition).
narrative_ontology:cs_drift_state('dbaf5a94-43f2-469e-b023-c28fc54c0536', post_first_missed_milestone, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dbaf5a94-43f2-469e-b023-c28fc54c0536', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, labor_ministry_planners).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, gradually_formalizing_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, gig_workers_in_extended_transition).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, informal_sector_incumbents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, gradually_formalizing_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the 12-point plan and the 2027 standardization target, framing platform work as a managed on-ramp into formal employment categories (social insurance registration, minimum benefit floors, portable entitlements). They set milestones, publish compliance timelines, and can revise the glide path, but their authority rests on the transition actually terminating in formalization rather than persisting indefinitely as flexible status.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, labor_ministry_planners, agenda_setter,
    institutional, generational, analytical, national).

% Operate under the flexible-employment classification while the state manages the transition, avoiding full employer obligations (payroll tax, severance, collective bargaining exposure) during the declared interim period. They lobby for extended timelines and phased compliance schedules, benefiting from every year the transition has not yet concluded.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_operators, beneficiary,
    powerful, biographical, mobile, national).

% Are enrolled in pilot formalization tracks — provisional social insurance, portable benefit accounts — that the plan credits as evidence the glide path works. They gain real entitlements incrementally but their formalization date depends on ministry capacity and platform compliance, neither of which they control.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, gradually_formalizing_workers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, gradually_formalizing_workers, payer).

% Work under flexible classification for years past the originally announced timeline, without the wage floors, benefits, or bargaining rights the plan promises at the endpoint. Their situation is justified at every review cycle by reference to the still-pending 2027 target rather than resolved by it. They cannot exit the classification without leaving platform work altogether.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, gig_workers_in_extended_transition, payer,
    powerless, biographical, trapped, national).

% Operate entirely outside both the platform economy and the formalization pipeline — street vendors, day laborers, small unregistered operators. The state's attention and enforcement capacity are absorbed by the flexible-employment standardization project, leaving their own formalization needs perpetually deferred to a later phase of the plan.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, informal_sector_incumbents, payer,
    powerless, biographical, trapped, regional).

% Argue the transition timeline is a moving target that has already slipped once and will slip again, and seek a binding legal floor now rather than a managed glide path. They are consulted in stakeholder forums but hold no seat in setting the 12-point plan's milestones or triggers.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_worker_unions, excluded,
    organized, biographical, constrained, national).

% Track whether wage growth and benefit coverage among platform workers are converging toward formal-sector parity on the schedule the plan projects, or diverging while the transition narrative persists. Their published trend data is the primary independent check on whether the developmental-state reading's timeline is being met.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, independent_labor_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__developmental_state_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a state-sanctioned glide path that lets platform work exist under lighter obligations now in exchange for a scheduled, monitored convergence toward standard employment protections — coordinating platform operators, ministries, and workers around a shared endpoint rather than an indefinite carve-out.
% TRANSFER_FUNCTION: Moves the cost of labor-market flexibility from platform operators (who defer full employer obligations during the transition) onto gig workers in extended transition (who defer receiving wage floors, benefits, and bargaining rights) and onto informal-sector incumbents (whose own formalization is deferred while ministry capacity is spent on the platform track).
% ABSENT_VOICES: Platform worker unions are consulted but do not set milestones or triggers; informal-sector incumbents outside the platform economy are not party to the standardization plan at all despite competing for the same limited state formalization capacity.
% DISAPPEARANCE_RATIONALE: The ministry and platform operators would say the transition framework's disappearance collapses the only credible path to formalization, reverting workers to unmanaged flexible status. Unions and independent economists would say the transition framework's disappearance would simply remove cover for an arrangement that has already missed one deadline, forcing a direct legislative floor that the glide path has substituted for.
% FOUNDING_PROBLEM: Platform work expanded faster than existing employment law could classify it, leaving workers without any applicable social insurance category and platforms without clear compliance obligations; the transition framework was built to convert an unclassified mass of work into a scheduled formalization pipeline.
% FOUNDING_PROBLEM_CORROBORATION: Labor ministry planners attest the founding problem remains live and the 2027 target is the operative solution. Independent labor economists tracking wage and benefit convergence data attest that the pipeline's actual throughput lags the announced schedule, and platform worker unions outside the ministry's own review process attest the transition has functioned more as a durable exemption than a countdown.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, contested).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the developmental-state reading credits the arrangement with a genuine coordination function — the transition pipeline does produce incremental formalization for some workers — but the rising trajectory documents that the promised endpoint keeps slipping, converting scheduled transition into something closer to durable exemption for platform operators. Suppression is moderate (0.44) reflecting active enforcement of the interim classification against workers who would prefer immediate formal status. Theater ratio climbs to 0.38 as milestone reporting and plan revisions increasingly substitute for measurable convergence — a Goodhart signal internal to this reading's own account of its progress.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators sit near the beneficiary end: mobile exit, powerful position, and every year of 'transition' status defers costs that formalization would impose. Gig workers in extended transition sit near the target end: powerless, trapped in platform work if they want income at all, and bearing the cost of a schedule they cannot enforce. Gradually formalizing workers are genuinely dual-positioned — real incremental gains, but their formalization date is hostage to ministry throughput and platform compliance, which is why they carry both beneficiary and payer roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is doing real work here: this reading claims the arrangement was built as a transition (has_sunset_clause: true, the 2027 target) rather than a steady-state extraction mechanism. The mandatrophy risk is that the sunset clause becomes purely rhetorical if milestones keep resetting without consequence — the rising theater_ratio is the leading indicator the story tracks. If the 2027 target passes without the promised convergence and the plan is re-issued with a new target rather than enforced, the developmental-state reading's own internal logic would require reclassifying the arrangement — that is the test this reading sets for itself, not an external imposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_terminus_credibility,
    'Is the 2027 standardization target a genuine, enforceable stabilization point for the kernel, or a rolling deadline that will be re-issued again when it arrives, as has already happened at least once?',
    'Track whether the 2027 date passes with measurable convergence in wage floors and benefit coverage for platform workers (per independent_labor_economists'' data), or whether the ministry issues a revised plan with a new target date and no binding consequence for missing the prior one.',
    'If the target holds and convergence is measured, the developmental-state reading is vindicated and the arrangement should reclassify toward rope or resolve entirely. If the target slips again without consequence, the theater_ratio trend suggests this reading is describing a snare or tangled_rope wearing scaffold language — the sibling precarity_extraction_reading would gain evidential support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_terminus_credibility, empirical, 'Whether the 2027 target is a real kernel-stabilizing commitment or a renewable deferral mechanism.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three readings of flexible_employment_legitimacy disagree — is it about facts (whether wages are rising, whether milestones are met) or about the normative status of an admittedly-real transition (whether a state-managed multi-year transition with deferred protections is itself legitimate regardless of whether it eventually completes)?',
    'Separate the readings'' empirical claims (convergence trend data, milestone compliance) from their normative claims (whether managed deferral is legitimate coordination or extraction) and test each independently; the developmental_state_reading and market_efficiency_reading may agree on facts while disagreeing on normative framing, while precarity_extraction_reading disputes both.',
    'If the disagreement is purely normative, no amount of convergence data resolves the kernel contest and all three readings persist as coexisting framings (per cs_structure.reading_relations). If the disagreement is substantially empirical (does convergence actually happen), one reading''s factual predictions will fail and that reading loses standing relative to the others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel''s sibling readings disagree on facts, norms, or both.').

omega_variable(
    ministry_capacity_vs_capture,
    'Is the extended transition timeline explained by genuine state administrative capacity constraints (a developmental-state-favorable account), or by regulatory capture in which platform lobbying shapes the milestone schedule to serve platform interests (a precarity-extraction-favorable account)?',
    'Compare ministry staffing/budget trends for the formalization pipeline against platform lobbying disclosures and milestone-revision history; a capacity-constrained ministry should show resource shortfalls independent of platform influence, while a captured process should show milestone revisions correlating with platform lobbying activity.',
    'A capacity-constrained finding supports treating the delay as a scaffold under strain rather than a disguised snare. A capture finding would support the precarity_extraction_reading''s account of the same delay pattern.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ministry_capacity_vs_capture, empirical, 'Whether transition delays reflect state capacity limits or platform regulatory capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(flex_tr_t4, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(flex_tr_t16, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(flex_tr_t24, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(flex_be_t4, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 4, 0.43).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(flex_be_t16, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(flex_be_t24, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(flex_su_t4, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(flex_su_t8, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(flex_su_t16, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(flex_su_t24, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__developmental_state_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the flexible_employment_legitimacy kernel. market_efficiency_reading treats the same platform-labor arrangement as a legitimate market-clearing mechanism (low ε, near-rope); precarity_extraction_reading treats it as structural extraction with platforms as concentrated beneficiaries and gig workers as victims (high ε, snare or tangled_rope). This developmental_state_reading occupies the middle position: a genuine but increasingly strained transition scaffold (moderate ε, scaffold with mandatrophy risk). All three share the underlying phenomenon but are authored as separate constraints with independent extractiveness, suppression, and stakeholder structures, per the ε-invariance principle — do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
