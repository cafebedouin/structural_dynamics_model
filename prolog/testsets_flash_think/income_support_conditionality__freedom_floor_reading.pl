% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   This constraint story instantiates the 'freedom_floor_reading' of the
 *   'income_support_conditionality' kernel. It describes unconditional income
 *   support as a mechanism that decommodifies labor power, thereby creating
 *   positive freedom for individuals to refuse coercive or exploitative work.
 *   The policy is viewed as a coordination mechanism (Rope) that provides a
 *   societal floor, reducing the structural extraction inherent in a labor
 *   market where survival depends solely on wage labor. The metrics reflect
 *   this reading, showing low extractiveness and suppression, as the policy's
 *   function is to empower individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.15).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.2).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, 'b0f2d79c-e5ef-4786-9b69-db4f7983c80f').
narrative_ontology:cs_kernel_codification('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', formalized).
narrative_ontology:cs_authority_grounding('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', lineage).
narrative_ontology:cs_interpretation_layer_present('b0f2d79c-e5ef-4786-9b69-db4f7983c80f').
narrative_ontology:cs_reading_relation('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', foundational, labor_power_decommodification_is_freedom).
narrative_ontology:cs_axiom_status(labor_power_decommodification_is_freedom, holdable).
narrative_ontology:cs_axiom_grounding('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', labor_power_decommodification_is_freedom, deontological).
narrative_ontology:cs_axiom('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', foundational, basic_needs_are_human_rights).
narrative_ontology:cs_axiom_status(basic_needs_are_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', basic_needs_are_human_rights, deontological).
narrative_ontology:cs_reference_frame('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', social_contract_freedom_floor).
narrative_ontology:cs_drift_state('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', contemporary_policy_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b0f2d79c-e5ef-4786-9b69-db4f7983c80f', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, unemployed_individuals).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_relying_on_coercive_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive direct income support, which provides a safety net and the ability to refuse exploitative or undesirable work, thereby increasing their bargaining power and positive freedom.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    moderate, biographical, mobile, national).

% Receive unconditional income, allowing them to meet basic needs without coercive job search requirements, fostering well-being and the freedom to pursue education, caregiving, or entrepreneurial ventures.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, unemployed_individuals, beneficiary,
    powerless, biographical, mobile, national).

% Lose their structural power to compel workers into low-wage or poor-condition jobs, as workers now have an alternative to destitution. They face increased pressure to offer better wages and working conditions to attract labor.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_relying_on_coercive_labor, payer,
    powerful, biographical, constrained, national).

% Champion the policy, framing it as a fundamental human right and a necessary step towards a more just and equitable society. They work to influence public opinion and legislative action.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, advocates_for_basic_income, agenda_setter,
    organized, generational, analytical, global).

% Are responsible for designing, funding, and administering the unconditional income support program. They manage the tax collection and distribution mechanisms.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Contribute to the funding of the unconditional income support program through taxes. Their support or opposition is crucial for the policy's political viability and sustainability.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, taxpayers, payer,
    organized, immediate, constrained, national).

% Study the effects of unconditional income support on labor markets, poverty rates, public health, and economic growth, providing empirical data and theoretical frameworks for policy debates.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, economic_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal income floor, ensuring basic needs are met for all citizens and enabling individuals to refuse exploitative labor, thereby fostering a more equitable and less coercive labor market.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base (primarily higher earners and corporations) to individuals, reducing their economic precarity and dependence on wage labor for survival.
% ABSENT_VOICES: Those who benefit from the current coercive labor market, such as employers who rely on a surplus of desperate labor, would object to the erosion of their power. Proponents of 'workfare' or conditional welfare policies would also object, arguing against the principle of unconditionality.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, many individuals would be forced back into precarious, low-wage, or exploitative work to survive. The balance of power in the labor market would shift significantly back towards employers, and poverty rates would likely increase, fundamentally reorganizing social and economic relations.
% FOUNDING_PROBLEM: The problem of poverty, economic precarity, and the inherent coercion within a labor market where individuals must accept any work, regardless of conditions or pay, to meet their basic needs.
% FOUNDING_PROBLEM_CORROBORATION: Social justice advocates, labor economists, and human rights organizations consistently attest to the ongoing problems of poverty, precarious work, and the coercive power dynamics in labor markets, supporting the claim that this policy addresses a live and pressing issue. International bodies like the UN also advocate for social protection floors.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.15) and suppression (0.20) reflect this reading's core claim that unconditional income support *reduces* the coercive power of the labor market, rather than extracting from its recipients. It provides a net benefit and expands options. The high resistance (0.70) acknowledges the significant political and ideological opposition this policy faces, particularly from those who benefit from the existing labor market dynamics. Accessibility collapse is low (0.25) because the policy *expands* alternatives for workers. Theater ratio is low (0.10) as the direct transfer of income is a functional, not performative, act.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of low-wage workers, this constraint is a clear Rope, providing essential coordination and freedom. From the perspective of employers who benefit from a coercive labor market, it is a 'snare' that extracts their power and increases their costs. The engine's per-seat classification will capture this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers and unemployed individuals are clear beneficiaries, gaining income and freedom (low d). Employers who previously relied on the threat of destitution to secure cheap labor are the 'victims' in this reading, as their coercive power is curtailed (high d). Government agencies and advocates are agenda-setters, while taxpayers bear the financial cost, though the overall societal benefit is argued to outweigh this.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_trap_ambiguity,
    'Does unconditional income support, in practice, lead to long-term dependency and skill atrophy, as claimed by the ''dependency_trap_reading''?',
    'Longitudinal empirical studies tracking labor force participation, skill development, and well-being outcomes of recipients over several generations, compared to control groups.',
    'If significant dependency and atrophy are observed, the ''freedom_floor_reading''s claim of positive freedom would be undermined, potentially shifting the constraint''s effective classification towards a Tangled Rope or even Snare from a societal perspective, as the coordination function would be compromised by unintended negative consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_trap_ambiguity, empirical, 'Whether unconditional income support creates dependency or freedom.').

omega_variable(
    wage_subsidy_ambiguity,
    'Does unconditional income support primarily function as an employer subsidy, allowing them to suppress wages while maintaining worker subsistence, as claimed by the ''wage_subsidy_reading''?',
    'Economic modeling and empirical analysis of wage trends in sectors employing low-wage labor following the implementation of unconditional income support, specifically looking for evidence of wage stagnation or decline.',
    'If it is found to primarily function as a wage subsidy, the ''freedom_floor_reading''s claim of decommodification would be weakened. The constraint might be reclassified as a Tangled Rope, as it would coordinate basic needs but extract from the public via employer benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_subsidy_ambiguity, empirical, 'Whether unconditional income support subsidizes employers or empowers workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__freedom_floor_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.08).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__freedom_floor_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.11).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__freedom_floor_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__freedom_floor_reading, suppression_requirement, 20, 0.12).


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
