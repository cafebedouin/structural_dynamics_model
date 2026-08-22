% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_floor_reading of the
 *   contested kernel income_support_conditionality. The standing arrangement
 *   is a policy of unconditional income support (e.g., universal basic income
 *   or demogrant) administered by the state. Under this reading, the
 *   arrangement functions as a coordination mechanism (rope) that solves the
 *   collective-action problem of individual workers' inability to refuse
 *   coercive employment. Low-wage workers exit the victim set and become
 *   beneficiaries; employers enter the victim set by losing the coercive
 *   power of the reserve army of labor. Sibling readings include the
 *   dependency_trap_reading (which would keep workers as victims) and the
 *   wage_subsidy_reading (which would recast employers as beneficiaries). The
 *   claim/metric independence is maintained: the claimed type is rope, while
 *   metrics honestly describe the moderate resistance from employers and the
 *   low but non-zero extraction implied by taxation and lost employer
 *   surplus.
 *
 * KEY AGENTS:
 *   - low_wage_workers: Primary beneficiary (powerless/mobile with the floor) â gain positive freedom to refuse coercive work.
 *   - employers: Primary payer (powerful/mobile) â lose coercive firing and wage-setting power.
 *   - state_administrator: Agenda setter (institutional/constrained) â administers the unconditional grant.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.08).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.05).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '2321d24c-1823-4cde-9f90-d5c9a21e106e').
narrative_ontology:cs_kernel_codification('2321d24c-1823-4cde-9f90-d5c9a21e106e', formalized).
narrative_ontology:cs_authority_grounding('2321d24c-1823-4cde-9f90-d5c9a21e106e', lineage).
narrative_ontology:cs_interpretation_layer_present('2321d24c-1823-4cde-9f90-d5c9a21e106e').
narrative_ontology:cs_reading_relation('2321d24c-1823-4cde-9f90-d5c9a21e106e', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('2321d24c-1823-4cde-9f90-d5c9a21e106e', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('2321d24c-1823-4cde-9f90-d5c9a21e106e', foundational, positive_freedom_to_refuse_coercive_work).
narrative_ontology:cs_axiom_status(positive_freedom_to_refuse_coercive_work, holdable).
narrative_ontology:cs_axiom_grounding('2321d24c-1823-4cde-9f90-d5c9a21e106e', positive_freedom_to_refuse_coercive_work, deontological).
narrative_ontology:cs_reference_frame('2321d24c-1823-4cde-9f90-d5c9a21e106e', decommodified_labor_market).
narrative_ontology:cs_drift_state('2321d24c-1823-4cde-9f90-d5c9a21e106e', post_welfare_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2321d24c-1823-4cde-9f90-d5c9a21e106e', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income support that secures subsistence regardless of employment status. Can refuse coercive, hazardous, or underpaid work without facing destitution. Their bargaining position in the labor market improves because the reserve army of labor is decommodified.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    powerless, biographical, mobile, national).

% Lose the ability to use the threat of destitution to compel acceptance of low wages and poor conditions. Must offer higher wages or better conditions to attract workers who now have an unconditional outside option. Experience reduced labor discipline and increased turnover costs.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers, payer,
    powerful, biographical, mobile, national).

% Designs and administers the unconditional income program, sets benefit levels, and funds it through taxation. Acts as the coordinating agent that establishes the universal floor and prevents conditionality from reasserting.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, state_administrator, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, low_wage_workers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem where individual workers cannot refuse exploitative wages because each faces destitution alone; unconditional income provides the coordination device enabling a universal exit option from coercive labor.
% TRANSFER_FUNCTION: Moves purchasing power from taxpayers to all residents, and moves bargaining power from employers to workers by decommodifying the reserve army of labor.
% ABSENT_VOICES: Workers who prefer conditional support as a signal of desert or social contribution; also, employers and neoliberal policymakers who view labor market exit as moral hazard are present in discourse but excluded from the design seat where unconditional framing dominates.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared, low-wage workers would lose the outside option that enables refusal of coercive work; labor markets would revert to a work-or-starve dynamic, employer bargaining power would sharply increase, and the positive freedom the arrangement secures would collapse.
% FOUNDING_PROBLEM: Industrial labor markets structurally coerce workers into accepting hazardous, degrading, or underpaid work because the alternative is destitution; individual workers cannot unilaterally refuse without catastrophic personal cost.
% FOUNDING_PROBLEM_CORROBORATION: Labor historians, occupational ethnographers, and behavioral economists outside the direct beneficiary population document the persistence of coercive work-or-starve dynamics; employer associations corroborate the existence of the problem indirectly by opposing the solution on grounds of labor discipline.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16 at interval end) because the unconditional grant is not designed to extract from workers but to expand their choice set; the primary transfer is redistributive, not extractive in the DR sense. Suppression is very low (0.08) because the policy expands alternatives rather than collapsing them. Accessibility collapse is near zero (0.05) since understanding the policy reveals more options, not fewer. Theater ratio is low (0.08) because the benefit is direct and material. Resistance is moderate (0.42) because employer coalitions actively oppose the policy, but this resistance is external to the constraint's operation, not a feature of it.
 *
 * PERSPECTIVAL GAP:
 *   The employer seat experiences the unconditional floor as a direct attack on property rights and managerial prerogative, computing toward extraction or victimization. The worker seat experiences the same policy as liberation from coercion, computing toward subsidy or rope. The agenda-setter seat sees a necessary coordination device. The engine's per-seat classification will diverge accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   Low_wage_workers are declared beneficiaries: the constraint subsidizes their exit options (d near 0.0). Employers are declared payers/victims: the constraint removes their coercive bargaining power and imposes higher labor costs (d near 1.0). The state_administrator sits at moderate d (0.5): it enforces and administers the transfer but does not personally collect or pay. The engine will derive low effective extraction for workers and high effective extraction for employers, producing seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The unconditional nature of the income prevents the constraint from being a scaffold (no sunset needed, not transitional) or a snare (workers are not trapped, they are freed). The risk of mandatrophy would arise if the policy were implemented with hidden conditionality or if the funding mechanism became the primary site of extraction. As authored, the coordination function (the exit option) is the steady-state justification, not a transitional phase. The constraint is not a piton because it is not performatively maintained; the benefit to workers is real and functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_income_support,
    'This constraint is the freedom_floor_reading of the income_support_conditionality kernel. If the dependency_trap_reading were operative, low-wage workers would remain victims and the constraint would read as a scaffold or snare; if the wage_subsidy_reading were operative, employers would read as beneficiaries. Which reading captures the structural truth?',
    'Comparative policy evaluation across pilot programs measuring labor market exit quality versus quantity, employer wage-setting behavior, and worker well-being.',
    'Resolving which reading is structurally dominant determines whether the constraint is classified as coordination (rope) or extraction (snare/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_income_support, conceptual, 'Uncertainty about which kernel reading matches structural reality.').

omega_variable(
    employer_extraction_or_power_loss,
    'Is the cost imposed on employers by unconditional income support a form of extraction (transfer of surplus to workers/state) or merely the removal of an unearned coercive advantage?',
    'Economic analysis of profit margins and wage shares before and after implementation; if employer surplus falls to competitive levels without worker surplus rising equivalently, it is power loss rather than extraction.',
    'If power loss, the victim classification for employers is weakened and the rope reading strengthens; if extraction, the constraint may compute as tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employer_extraction_or_power_loss, conceptual, 'Ambiguity about whether employer costs constitute extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__freedom_floor_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__freedom_floor_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__freedom_floor_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(inco_tr_t32, income_support_conditionality__freedom_floor_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__freedom_floor_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__freedom_floor_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__freedom_floor_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__freedom_floor_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(inco_be_t32, income_support_conditionality__freedom_floor_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__freedom_floor_reading, base_extractiveness, 40, 0.16).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_conditionality__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
