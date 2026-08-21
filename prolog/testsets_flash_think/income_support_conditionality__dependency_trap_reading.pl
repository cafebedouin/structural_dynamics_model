% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'dependency trap' reading of the
 *   broader 'income_support_conditionality' kernel. From this perspective,
 *   unconditional income support, while ostensibly a social safety net,
 *   functions as a snare. It is argued to undermine work incentives, leading
 *   to long-term dependency and skill atrophy among recipients, while
 *   imposing a burden on taxpayers for non-productive transfers. The
 *   constraint's persistence relies on the perceived inevitability of such a
 *   system once implemented, and the difficulty of reversing it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.85).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.78).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'd93578da-96f7-44fd-9e4b-0c39ec788e8c').
narrative_ontology:cs_kernel_codification('d93578da-96f7-44fd-9e4b-0c39ec788e8c', formalized).
narrative_ontology:cs_authority_grounding('d93578da-96f7-44fd-9e4b-0c39ec788e8c', extraction).
narrative_ontology:cs_interpretation_layer_present('d93578da-96f7-44fd-9e4b-0c39ec788e8c').
narrative_ontology:cs_reading_relation('d93578da-96f7-44fd-9e4b-0c39ec788e8c', income_support_conditionality__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('d93578da-96f7-44fd-9e4b-0c39ec788e8c', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('d93578da-96f7-44fd-9e4b-0c39ec788e8c', foundational, work_ethic_is_foundational).
narrative_ontology:cs_axiom_status(work_ethic_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('d93578da-96f7-44fd-9e4b-0c39ec788e8c', work_ethic_is_foundational, deontological).
narrative_ontology:cs_axiom('d93578da-96f7-44fd-9e4b-0c39ec788e8c', foundational, unconditional_support_erodes_skills).
narrative_ontology:cs_axiom_status(unconditional_support_erodes_skills, holdable).
narrative_ontology:cs_axiom_grounding('d93578da-96f7-44fd-9e4b-0c39ec788e8c', unconditional_support_erodes_skills, empirically_contingent).
narrative_ontology:cs_reference_frame('d93578da-96f7-44fd-9e4b-0c39ec788e8c', conditional_welfare_state).
narrative_ontology:cs_drift_state('d93578da-96f7-44fd-9e4b-0c39ec788e8c', contemporary_policy_debates, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d93578da-96f7-44fd-9e4b-0c39ec788e8c', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, policy_advocates_for_conditionality).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals receiving unconditional income support, who, from this reading's perspective, become trapped in idleness, experience skill atrophy, and lose motivation to seek employment, leading to long-term dependency.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, identity_locked, national).

% Citizens who fund the unconditional income support system through taxes. From this reading's perspective, they bear the cost of non-productive transfers, supporting a system that creates dependency rather than self-sufficiency.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Political actors, think tanks, and economists who argue that income support should be conditional on work or training. They benefit from the policy debate by advocating for their preferred system and gaining influence when dependency is perceived to rise.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, policy_advocates_for_conditionality, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for unconditional basic income who believe it provides a 'freedom floor' and empowers individuals. From the dependency trap reading, their voices are excluded or dismissed as naive about human incentives and the costs of idleness.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, policy_advocates_for_ubi, excluded,
    organized, generational, constrained, national).

% Researchers and economists who study the effects of social welfare policies on labor supply, employment rates, and skill development. They provide empirical data and theoretical frameworks that can be used to support or refute the dependency trap hypothesis.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_market_analysts, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the primary function is not coordination but rather a transfer mechanism that inadvertently coordinates individuals into a state of passive reliance on state support, rather than active participation in the labor market.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base (taxpayers) to individuals (UBI recipients) without requiring any reciprocal labor or activity, leading to a perceived net drain on societal productivity.
% ABSENT_VOICES: Advocates for UBI as a 'freedom floor' or those who emphasize the non-monetary benefits of unconditional income (e.g., improved health, reduced stress, community engagement) are largely absent from this reading's framing, as their arguments directly challenge the premise of dependency.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, individuals currently receiving it would be compelled to seek employment or alternative means of subsistence, leading to a significant reorganization of the labor market, social services, and individual life choices. Public expenditure would decrease, and the perceived 'dependency trap' would be eliminated.
% FOUNDING_PROBLEM: The original problem unconditional income support aims to solve is poverty alleviation and providing a basic safety net for all citizens, regardless of their employment status.
% FOUNDING_PROBLEM_CORROBORATION: While proponents of unconditional income support attest that poverty and economic insecurity remain live problems, critics (e.g., conservative economists, some policy think tanks, and political commentators) argue that the policy itself creates new, more insidious problems of dependency and skill erosion, thus rendering the 'founding problem' either solved by other means or exacerbated by the proposed solution. This contestation is widely documented in policy debates and academic literature outside of UBI advocacy groups.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the perceived societal cost of lost productivity and the burden on taxpayers, coupled with the 'extraction' of agency and skills from recipients. Suppression (0.78) is high because the system, once established, is seen as trapping recipients in a cycle of idleness, with diminishing alternatives for meaningful work. The low theater ratio (0.1) indicates that the constraint is not primarily performative; its negative effects are considered real and functional from this reading's viewpoint. Accessibility collapse (0.7) is high because the 'trap' makes it harder for recipients to access or desire work alternatives. Resistance (0.2) is low among recipients, who are seen as passively accepting the support, though taxpayers may express diffuse resistance.
 *
 * PERSPECTIVAL GAP:
 *   This reading sharply diverges from the 'freedom_floor_reading' which sees unconditional income as empowering, and the 'wage_subsidy_reading' which focuses on its impact on employer wages. The core difference lies in whether the outcome for recipients is seen as beneficial (freedom) or detrimental (dependency), and whether the primary transfer is to individuals or indirectly to employers. This story focuses solely on the detrimental dependency aspect.
 *
 * DIRECTIONALITY LOGIC:
 *   UBI recipients are targets (high d) as they are seen to bear the costs of dependency and skill erosion. Taxpayers are also targets (high d) as they fund the system without perceived productive return. Policy advocates for conditionality are beneficiaries (low d) as the existence of the 'dependency trap' validates their policy positions and increases their influence. The system itself, by perpetuating dependency, 'benefits' from its own existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare prevents mislabeling this as a Rope or Scaffold. While it involves a transfer of resources, this reading argues it does not solve a genuine collective action problem in a beneficial way, nor is it temporary. Instead, it actively creates identifiable victims (dependent recipients, burdened taxpayers) and suppresses alternatives (work incentives), aligning with the Snare type. The 'mandate' of poverty alleviation is seen as having atrophied into a mechanism for dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a true representation of the effects of unconditional income support, or is it one specific reading of a contested policy kernel?',
    'Comparison with alternative readings (e.g., ''freedom_floor_reading'', ''wage_subsidy_reading'') and empirical evidence from diverse UBI experiments, analyzed through different theoretical lenses.',
    'If this is confirmed as one reading, its classification remains valid within its own framework, but its universal applicability is limited. If alternative readings are found to be more empirically robust, the overall policy classification would shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''dependency_trap_reading'' of the ''income_support_conditionality'' kernel.').

omega_variable(
    dependency_measurement_ambiguity,
    'How is ''dependency'' and ''skill atrophy'' objectively measured, and are these effects directly attributable to unconditional income support versus other socioeconomic factors?',
    'Longitudinal studies with robust control groups, using multiple metrics for labor market participation, skill development, and psychological well-being, alongside qualitative data on recipient experiences and motivations.',
    'If dependency and skill atrophy are not robustly measurable or are primarily driven by external factors, the extractiveness and suppression metrics would be significantly lower, potentially reclassifying the constraint away from a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_measurement_ambiguity, empirical, 'Ambiguity in measuring the core negative outcomes of the dependency trap.').

omega_variable(
    causality_of_idleness,
    'Is the observed idleness among recipients a direct causal effect of unconditional income support, or does it reflect pre-existing conditions, health issues, or a rational choice to pursue non-market activities?',
    'Detailed micro-level studies, including randomized controlled trials and qualitative interviews, to disentangle the motivations and circumstances of recipients, comparing those with and without unconditional support.',
    'If idleness is largely due to factors other than the income support itself, the ''snare'' classification would weaken, as the constraint would not be the primary cause of the negative outcome. This would reduce the perceived ''extraction'' from recipients.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_of_idleness, empirical, 'Disentangling the causes of idleness in UBI recipients.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__dependency_trap_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__dependency_trap_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(inco_tr_t50, income_support_conditionality__dependency_trap_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__dependency_trap_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__dependency_trap_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(inco_be_t50, income_support_conditionality__dependency_trap_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__dependency_trap_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__dependency_trap_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(inco_su_t50, income_support_conditionality__dependency_trap_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, labor_market_flexibility).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, social_safety_net_design).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, public_finance_sustainability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
