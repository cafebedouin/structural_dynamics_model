% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency_trap_reading of the
 *   unconditional_income_support kernel. The standing arrangement under
 *   contest is a policy regime (proposed or enacted) that replaces targeted,
 *   means-tested aid with a uniform unconditional cash transfer. From this
 *   reading's perspective, the arrangement functions as a snare: it extracts
 *   from the working poor by dismantling higher-value targeted programs and
 *   from net taxpayers by requiring substantial new revenue, while
 *   transferring the surplus to non-needy households and to the advocacy
 *   networks whose institutional power depends on the universality framing.
 *   The claim/metric independence is maintained: the reading claims snare,
 *   and the metrics are authored to reflect high extraction and significant
 *   suppression of targeted alternatives.
 *
 * KEY AGENTS:
 *   - middle_upper_class_households: Net beneficiary (powerful/mobile) â receives unconditional transfers despite not needing them.
 *   - ubi_advocacy_networks: Net beneficiary (organized/mobile) â collects political capital and institutional resources from the universality project.
 *   - working_poor: Primary target (powerless/trapped) â loses targeted benefits worth more than the grant.
 *   - net_taxpayers: Secondary target (organized/constrained) â funds transfers with limited political exit.
 *   - state_disbursement_apparatus: Agenda-setter (institutional/constrained) â administers the policy and enforces tax collection.
 *   - means_tested_service_providers: Excluded voice (organized/constrained) â displaced by the shift to cash but absent from design debates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.78).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.68).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, 'b9d68649-cba2-45fa-8284-85443513f567').
narrative_ontology:cs_kernel_codification('b9d68649-cba2-45fa-8284-85443513f567', formalized).
narrative_ontology:cs_authority_grounding('b9d68649-cba2-45fa-8284-85443513f567', expertise).
narrative_ontology:cs_interpretation_layer_present('b9d68649-cba2-45fa-8284-85443513f567').
narrative_ontology:cs_reading_relation('b9d68649-cba2-45fa-8284-85443513f567', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9d68649-cba2-45fa-8284-85443513f567', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('b9d68649-cba2-45fa-8284-85443513f567', foundational, universality_subsidizes_non_needy).
narrative_ontology:cs_axiom_status(universality_subsidizes_non_needy, holdable).
narrative_ontology:cs_axiom_grounding('b9d68649-cba2-45fa-8284-85443513f567', universality_subsidizes_non_needy, empirically_contingent).
narrative_ontology:cs_axiom('b9d68649-cba2-45fa-8284-85443513f567', foundational, in_kind_targeting_maximizes_welfare).
narrative_ontology:cs_axiom_status(in_kind_targeting_maximizes_welfare, holdable).
narrative_ontology:cs_axiom_grounding('b9d68649-cba2-45fa-8284-85443513f567', in_kind_targeting_maximizes_welfare, empirically_contingent).
narrative_ontology:cs_reference_frame('b9d68649-cba2-45fa-8284-85443513f567', targeted_need_based_safety_net).
narrative_ontology:cs_drift_state('b9d68649-cba2-45fa-8284-85443513f567', post_universalization_policy_shift, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b9d68649-cba2-45fa-8284-85443513f567', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_households).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_networks).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, net_taxpayers).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, incentive_neutrality_impossibility).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, universality_efficiency_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional cash transfers that augment disposable income despite lacking material need. Under typical progressive-to-flat funding shifts, they are net fiscal winners because their tax increase is smaller than the grant they receive.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_households, beneficiary,
    powerful, biographical, mobile, national).

% Derive institutional funding, academic tenure, and political influence from promoting universality. The policy's existence or credible prospect sustains their organizational budgets and public profile regardless of net distributional outcomes for the poor.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocacy_networks, beneficiary,
    organized, generational, mobile, national).

% Lose targeted in-kind and cash benefits (housing, nutrition, disability supplements) whose value exceeds the unconditional grant. Remain in the labor market but with reduced total resources and weakened bargaining power due to the loss of program-specific fallback options.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, immediate, trapped, national).

% Fund transfers through taxation while receiving little or no offsetting benefit. Once enacted, unconditional entitlements are politically protected, making exit through repeal costly and institutionally constrained.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, net_taxpayers, payer,
    organized, biographical, constrained, national).

% Sets the policy design, tax rates, and disbursement rules. Enforces compliance through the tax system and benefits administration. Justifies the arrangement on grounds of administrative efficiency and poverty reduction, while overseeing the replacement of targeted programs.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, state_disbursement_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Nonprofits and agencies delivering targeted services (housing navigation, disability support, nutritional counseling) are displaced by the shift to cash. Their client base and funding streams are absorbed or eliminated, yet they are rarely consulted in UBI design debates.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, means_tested_service_providers, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates the administrative burden of means-testing and the stigma associated with targeted welfare receipt, replacing a fragmented program landscape with a single uniform payment.
% TRANSFER_FUNCTION: Moves resources from net taxpayers and the working poor (through lost targeted benefits) to middle- and upper-income households and the advocacy organizations that promote the policy, via unconditional cash disbursement.
% ABSENT_VOICES: Means-tested service providers and the deeply poor with complex needs (who require in-kind supports worth more than cash equivalents) are structurally excluded from UBI design debates. Their absence creates the illusion of consensus around universality.
% DISAPPEARANCE_RATIONALE: If the unconditional income support arrangement vanished overnight, the fiscal flow to non-needy households would cease, targeted aid programs would need to be rebuilt from scratch, and the political coalition sustaining the advocacy networks would fracture. The working poor would regain access to higher-value targeted benefits, and the tax burden would shift downward.
% FOUNDING_PROBLEM: The fragmentation, administrative complexity, and perceived stigma of means-tested welfare systems, which were seen as creating poverty traps, bureaucratic waste, and political vulnerability for social safety nets.
% FOUNDING_PROBLEM_CORROBORATION: UBI advocacy networks and some left-libertarian economists attest the problem is live. Center-right fiscal analysts and targeted-program social workers attest the problem was manageable and the universal replacement destroys more value than it creates; their corroboration comes from outside the benefiting parties.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the arrangement moves resources upward and replaces higher-marginal-value in-kind benefits with lower-marginal-value cash for the poorest. Suppression (0.68) is substantial because the political and bureaucratic machinery of the universal program actively crowds out targeted alternatives: once enacted, the fiscal and cognitive space for means-testing collapses. Theater_ratio (0.40) reflects a moderate performative component â the policy is defended with anti-poverty rhetoric that is increasingly decoupled from its net distributional effect. Accessibility_collapse (0.72) is high because targeted aid infrastructure is dismantled and politically difficult to rebuild once replaced. Resistance (0.55) is moderate: fiscal conservatives and some social-service constituencies resist, but the universal beneficiary base and advocacy networks offset them.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (state apparatus, middle-class households, advocacy networks) experience the constraint as legitimate redistribution or efficient modernization. The payer seats (working poor losing targeted aid, taxpayers funding upward transfers) experience it as extraction. The engine will compute divergent per-seat classifications: low effective extraction for beneficiaries, high effective extraction for trapped working poor.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to middle_upper_class_households and ubi_advocacy_networks, who structurally collect from the arrangement. Victim declarations map to working_poor and net_taxpayers, who structurally pay. The working poor are assigned the highest directionality because they are powerless and trapped: they cannot exit the labor market and cannot reclaim lost targeted benefits. Net_taxpayers are also targets but have slightly lower derived d due to greater organizational power. State_disbursement_apparatus sits near the middle as enforcement agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement prevents mislabeling by treating the founding problem (fragmented welfare state) as contested rather than live. The R5 genealogy shows the problem is disputed: outside corroboration from fiscal analysts and targeted-service providers suggests the targeted safety net was functional and that the universal replacement is a zombie justification masking extraction. This distinguishes the snare from a scaffold: a scaffold would carry a sunset clause and a live founding problem; this arrangement has no sunset and its founding problem is contested at best.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    net_distributional_impact,
    'Does unconditional income support produce net upward or downward redistribution once all offsetting tax and benefit changes are accounted for?',
    'Comprehensive microsimulation across all federal and state transfers and taxes, comparing pre- and post-UBI Gini coefficients and net benefit incidence by decile.',
    'If upward, supports snare classification; if downward, shifts toward scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_distributional_impact, empirical, 'Whether the fiscal incidence of universality is progressive or regressive.').

omega_variable(
    employment_response_magnitude,
    'What is the causal effect of permanent unconditional income on labor supply, distinguishing income effects from substitution effects and selection bias?',
    'Large-scale natural experiments and structural modeling that isolate the treatment effect of guaranteed income on hours worked and earnings.',
    'Large negative effects support high extractiveness (harm to working poor via detachment); small effects weaken the dependency trap claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_response_magnitude, empirical, 'Magnitude of labor-supply response to unconditional transfers.').

omega_variable(
    reading_incommensurability,
    'Can the dependency trap reading and the freedom floor reading be adjudicated by the same empirical evidence, or do they rest on incommensurable normative premises about the value of labor-market participation?',
    'Meta-analysis of whether pilot evidence shifts adherents between readings; survey of whether empirical results alter policy preferences across ideological camps.',
    'If incommensurable, the kernel is a pure values dispute; if adjudicable, one reading may be structurally false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether sibling readings share an empirical adjudication basis.').

omega_variable(
    mandatrophy_of_targeted_aid,
    'Was the targeted welfare state already suffering from political mandatrophy before UBI replaced it, or was the replacement itself the extraction mechanism?',
    'Historical analysis of targeted program enrollment, adequacy trends, and political support in the two decades preceding universalization.',
    'If targeted aid was already a piton, UBI may be a scaffold; if targeted aid was functional, UBI-as-snare is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_of_targeted_aid, conceptual, 'Whether UBI replaced a dead institution or killed a live one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__dependency_trap_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__dependency_trap_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__dependency_trap_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(unco_tr_t25, unconditional_income_support__dependency_trap_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__dependency_trap_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(unco_tr_t40, unconditional_income_support__dependency_trap_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__dependency_trap_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__dependency_trap_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__dependency_trap_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(unco_be_t25, unconditional_income_support__dependency_trap_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__dependency_trap_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(unco_be_t40, unconditional_income_support__dependency_trap_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__dependency_trap_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__dependency_trap_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__dependency_trap_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(unco_su_t25, unconditional_income_support__dependency_trap_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(unco_su_t30, unconditional_income_support__dependency_trap_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(unco_su_t40, unconditional_income_support__dependency_trap_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is the dependency_trap_reading of the unconditional_income_support kernel, decomposed from the freedom_floor_reading and universality_paradox_reading per the Îµ-invariance principle. Each reading carries a distinct Îµ, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
