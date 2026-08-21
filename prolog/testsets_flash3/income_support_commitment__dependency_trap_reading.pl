% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story analyzes unconditional income support from the
 *   'dependency trap' reading, where it is seen as a mechanism that
 *   disincentivizes work, atrophies skills, and increases state dependence.
 *   It is framed as a Tangled Rope because it provides a coordination
 *   function (basic income floor) but also involves asymmetric extraction
 *   (from working taxpayers to non-working recipients, with long-term costs
 *   to individuals whose skills atrophy). The metrics reflect a system that,
 *   while not overtly coercive, creates a subtle but persistent pull towards
 *   non-participation and dependency.
 *
 * KEY AGENTS:
 *   - income_support_recipients_exiting_labor: Primary beneficiary (powerless/identity_locked) — benefits from income, but becomes dependent.
 *   - working_taxpayers: Primary payer (organized/constrained) — funds the system, bears the cost of non-participation.
 *   - individuals_with_atrophied_skills: Long-term victim (powerless/trapped) — bears the cost of skill degradation.
 *   - social_policy_administrators: Agenda setter (institutional/constrained) — manages the system.
 *   - economic_productivity_advocates: Analytical observer (powerful/analytical) — critiques the system's impact on work.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.65).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.4).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '13123322-dfdc-492b-99f4-31c2f5602e86').
narrative_ontology:cs_kernel_codification('13123322-dfdc-492b-99f4-31c2f5602e86', formalized).
narrative_ontology:cs_authority_grounding('13123322-dfdc-492b-99f4-31c2f5602e86', lineage).
narrative_ontology:cs_interpretation_layer_present('13123322-dfdc-492b-99f4-31c2f5602e86').
narrative_ontology:cs_reading_relation('13123322-dfdc-492b-99f4-31c2f5602e86', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('13123322-dfdc-492b-99f4-31c2f5602e86', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('13123322-dfdc-492b-99f4-31c2f5602e86', foundational, labor_market_participation_is_a_social_good).
narrative_ontology:cs_axiom_status(labor_market_participation_is_a_social_good, holdable).
narrative_ontology:cs_axiom_grounding('13123322-dfdc-492b-99f4-31c2f5602e86', labor_market_participation_is_a_social_good, deontological).
narrative_ontology:cs_axiom('13123322-dfdc-492b-99f4-31c2f5602e86', foundational, unconditional_transfers_create_moral_hazard).
narrative_ontology:cs_axiom_status(unconditional_transfers_create_moral_hazard, holdable).
narrative_ontology:cs_axiom_grounding('13123322-dfdc-492b-99f4-31c2f5602e86', unconditional_transfers_create_moral_hazard, empirically_contingent).
narrative_ontology:cs_reference_frame('13123322-dfdc-492b-99f4-31c2f5602e86', work_ethic_and_self_reliance_framework).
narrative_ontology:cs_drift_state('13123322-dfdc-492b-99f4-31c2f5602e86', contemporary_policy_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('13123322-dfdc-492b-99f4-31c2f5602e86', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, income_support_recipients_exiting_labor).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, individuals_with_atrophied_skills).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income, allowing them to reduce or cease labor market participation. From this reading's perspective, they become dependent on state support, leading to skill atrophy and reduced self-sufficiency over time.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, income_support_recipients_exiting_labor, beneficiary,
    powerless, biographical, identity_locked, national).

% Fund the unconditional income support through their taxes. They bear the cost of supporting non-working individuals, which this reading frames as an unfair transfer from productive to non-productive members of society.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    organized, biographical, constrained, national).

% Are the long-term victims of the system, as their skills degrade due to lack of use, making re-entry into the labor market increasingly difficult. This leads to a deeper, self-reinforcing dependency on state support.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, individuals_with_atrophied_skills, payer,
    powerless, generational, trapped, national).

% Implement and manage the unconditional income support programs. They are responsible for the distribution of funds and the oversight of the system, often balancing competing political mandates.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, social_policy_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Analyze the impact of unconditional income support on labor force participation, skill development, and overall economic output. They often advocate for policies that incentivize work and self-reliance.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, economic_productivity_advocates, observer,
    powerful, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a basic safety net, ensuring a minimum standard of living for all citizens, which can reduce poverty and administrative overhead compared to means-tested programs.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base (primarily working taxpayers) to all citizens, including those who choose to reduce or cease labor market participation.
% ABSENT_VOICES: Future generations who will inherit a potentially less productive economy and a larger state dependency burden are absent from the current policy debate. Their interests are often represented by long-term economic forecasters and fiscal conservatives.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, there would be immediate and severe social disruption, a sharp increase in poverty, and a forced re-entry into the labor market for many, regardless of skill or opportunity. The social safety net would collapse, requiring a complete reorganization of welfare provision.
% FOUNDING_PROBLEM: The founding problem was to address poverty, reduce administrative complexity of welfare programs, and provide a basic income floor in an era of increasing automation and precarious work.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of unconditional income support (e.g., social justice advocates, some economists) argue the founding problem of poverty and precarity remains live. Critics (e.g., fiscal conservatives, some labor economists) argue that while poverty is a problem, unconditional income support exacerbates other problems like work disincentives and dependency, suggesting the original solution has created new, more severe problems. Corroboration comes from academic studies on labor market participation and skill development, as well as government budget analyses.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the transfer of resources from productive labor to non-participation, and the long-term societal cost of reduced human capital. Suppression (0.40) is moderate; it's not overt coercion but a 'soft' suppression through the removal of work incentives and the creation of a comfortable, albeit dependent, alternative. Theater ratio is low (0.10) as the system is genuinely functional in its stated goal of providing income, but its negative externalities are downplayed. Accessibility collapse (0.50) is moderate; alternatives (work) exist but are disincentivized. Resistance (0.30) is present from working taxpayers and economic advocates, but not strong enough to dismantle the system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'dependency trap' reading, the constraint is extractive and creates victims. However, from the 'freedom floor' reading, the same constraint would be seen as a net benefit, enhancing autonomy and dignity. The engine's per-seat classification will capture this divergence, showing different types for beneficiaries (who experience it as a 'rope' or 'scaffold') versus payers/victims (who experience it as a 'snare' or 'tangled_rope').
 *
 * DIRECTIONALITY LOGIC:
 *   Income support recipients who exit the labor market are beneficiaries (d near 0.0) as they receive direct financial support. Working taxpayers are payers (d near 1.0) as they fund the system. Individuals with atrophied skills are also payers/victims (d near 1.0) due to the long-term personal cost. Social policy administrators are agenda setters, balancing the system. Economic productivity advocates are observers, analyzing the system's effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'Rope' (as proponents might claim) by highlighting the asymmetric extraction and the creation of long-term dependency. It also avoids mislabeling it as a pure 'Snare' by acknowledging the genuine coordination function of providing a basic income floor. The 'Tangled Rope' classification captures the hybrid nature where a coordination mechanism is intertwined with extractive outcomes and dependency creation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    work_disincentive_magnitude,
    'What is the actual magnitude of the work disincentive effect of unconditional income support, and how does it vary across demographics and economic conditions?',
    'Longitudinal studies and randomized control trials (RCTs) on unconditional income programs in diverse economic contexts, measuring labor force participation, hours worked, and skill development.',
    'If the work disincentive is empirically found to be negligible, this reading''s extractiveness and suppression metrics would be significantly lower, potentially reclassifying it towards a ''Rope''. If the disincentive is substantial, it reinforces the ''Tangled Rope'' or even ''Snare'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(work_disincentive_magnitude, empirical, 'Empirical evidence on the extent to which unconditional income support reduces labor market participation.').

omega_variable(
    dependency_vs_autonomy_framing,
    'Is the outcome of reduced labor market participation primarily ''dependency'' (as framed by this reading) or ''autonomy'' (as framed by the freedom_floor_reading)?',
    'Qualitative sociological studies exploring recipients'' subjective experiences, choices, and perceived well-being, alongside objective measures of civic engagement and non-market contributions.',
    'If recipients overwhelmingly report increased autonomy and meaningful non-market activities, the ''dependency trap'' framing weakens, shifting the conceptual basis of extraction. If dependency is the dominant subjective experience, this reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_vs_autonomy_framing, conceptual, 'The conceptual framing of reduced labor market participation as either dependency or autonomy.').

omega_variable(
    skill_atrophy_evidence,
    'What is the empirical evidence for skill atrophy among long-term recipients of unconditional income support, and what are its long-term economic consequences?',
    'Longitudinal studies tracking skill levels, educational attainment, and re-employment rates of unconditional income recipients compared to control groups over decades.',
    'Strong evidence of significant and irreversible skill atrophy would increase the ''victim'' status of individuals and reinforce the ''Tangled Rope'' or ''Snare'' classification. Lack of such evidence would reduce the perceived long-term harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_atrophy_evidence, empirical, 'Empirical evidence for skill degradation and its economic impact due to long-term income support.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__dependency_trap_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__dependency_trap_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__dependency_trap_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__dependency_trap_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__dependency_trap_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__dependency_trap_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_commitment' kernel, focusing on the dependency trap aspect. It is structurally distinct from the 'freedom_floor_reading' and 'targeting_efficiency_reading' of the same kernel, which emphasize different aspects and outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
