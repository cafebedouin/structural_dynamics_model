% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Unconditional Income Support as Incentive-Distorting Subsidy (Dependency Trap Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency_trap_reading of the
 *   contested kernel 'unconditional_income_support'. The reading frames UBI
 *   as an incentive-distorting subsidy that rewards idleness, crowds out
 *   targeted aid, and redistributes upward to the non-needy. The structural
 *   delta: beneficiaries are the middle/upper class (who receive transfers
 *   they do not need) and UBI advocates (who capture political capital from
 *   universality); victims are the working poor (who lose targeted programs
 *   worth more than the flat UBI) and taxpayers (who bear a $1.4T net cost
 *   after offsets). Extraction is high (ε=0.78) based on AEI meta-analysis
 *   showing -3.2% employment in large pilots. The constraint operates as a
 *   snare: the universality cover story masks extraction from the poor via
 *   program replacement, enforced through legislative majorities and fiscal
 *   consolidation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.78).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.72).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Incentive-Distorting Subsidy (Dependency Trap Reading)").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '02c71e36-78c5-4a93-b1aa-62c4cccf2bad').
narrative_ontology:cs_kernel_codification('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', distributed).
narrative_ontology:cs_authority_grounding('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', extraction).
narrative_ontology:cs_interpretation_layer_present('02c71e36-78c5-4a93-b1aa-62c4cccf2bad').
narrative_ontology:cs_reading_relation('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', foundational, universal_transfer_crowds_out_targeted_aid).
narrative_ontology:cs_axiom_status(universal_transfer_crowds_out_targeted_aid, holdable).
narrative_ontology:cs_axiom_grounding('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', universal_transfer_crowds_out_targeted_aid, empirically_contingent).
narrative_ontology:cs_axiom('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', foundational, unconditionality_reduces_labor_supply).
narrative_ontology:cs_axiom_status(unconditionality_reduces_labor_supply, holdable).
narrative_ontology:cs_axiom_grounding('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', unconditionality_reduces_labor_supply, empirically_contingent).
narrative_ontology:cs_reference_frame('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', targeted_welfare_state).
narrative_ontology:cs_drift_state('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', ubi_pilot_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('02c71e36-78c5-4a93-b1aa-62c4cccf2bad', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, work_incentive_preservation_doctrine).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, targeted_efficiency_principle).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, fiscal_responsibility_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives universal transfer despite not needing it; the payment is a marginal addition to disposable income. Can exit the constraint's fiscal impact through tax planning, capital mobility, or political influence. The universality feature ensures they collect without stigma or means-testing.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class, beneficiary,
    powerful, biographical, mobile, national).

% Gain political capital, funding, and institutional positions from promoting universality. Their career and organizational viability depend on the constraint's expansion. Exit would mean abandoning the core policy commitment that defines their movement.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocates, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, ubi_advocates, agenda_setter).

% Lose targeted programs (housing vouchers, childcare subsidies, disability supplements, EITC) worth more than the flat UBI amount. Face reduced labor supply incentives (AEI meta-analysis: -3.2% employment in large pilots). Cannot easily exit because they depend on the residual safety net and lack capital mobility.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    moderate, biographical, constrained, national).

% Bear net fiscal cost of $1.4 trillion after offsets (per dynamic scoring). The universal design forces high earners to fund transfers to non-needy households. Exit options limited to tax avoidance, emigration, or political reversal — all costly and uncertain.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers, payer,
    organized, generational, constrained, national).

% Design and legislate the UBI structure; choose the flat amount, financing mix, and which targeted programs to eliminate. Their re-election incentives align with visible universal benefits rather than invisible opportunity costs. Can modify or repeal the constraint but face coalition pressures from both beneficiary groups.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, government_policymakers, agenda_setter,
    institutional, generational, analytical, national).

% Evaluate employment effects, fiscal incidence, and distributional outcomes across pilots (Finland, Canada, Kenya, US negative income tax experiments). Their findings feed into the contest between readings but do not determine policy adoption.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, policy_analysts, observer,
    analytical, immediate, analytical, global).

% Depend on means-tested benefits (SSI, Section 8, TANF, Medicaid waivers) that exceed UBI in value and are tailored to specific needs. Universality eliminates their categorical eligibility. They are not consulted in the universalist framing that treats all citizens as identical.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, current_targeted_program_recipients, excluded,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement claims to solve the coordination problem of welfare administration and poverty alleviation by providing a universal unconditional transfer, but in practice it replaces targeted programs with a flat payment that does not account for varying needs.
% TRANSFER_FUNCTION: Moves funds from taxpayers (via general taxation) and from the working poor (via elimination of targeted benefits worth more than the UBI amount) to the middle/upper class (who receive the UBI despite not needing it) and to UBI advocates (who gain political capital from universality).
% ABSENT_VOICES: Current recipients of targeted welfare programs (e.g., disability, housing, childcare subsidies) who would lose more in specific benefits than they gain in UBI, and low-wage workers who face reduced employment incentives. They are excluded from the universalist framing that treats all citizens as identical.
% DISAPPEARANCE_RATIONALE: If the universal basic income were eliminated overnight, the working poor would regain targeted benefits tailored to their needs, taxpayers would see a reduction in net fiscal burden of $1.4 trillion, and labor supply would increase by an estimated 3.2% (per AEI meta-analysis). The welfare system would revert to means-tested programs.
% FOUNDING_PROBLEM: The arrangement was built to solve the perceived problem of welfare stigma, administrative complexity, and poverty traps created by means-tested phase-outs. Proponents argued a universal floor would simplify bureaucracy and empower recipients.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by UBI proponents (e.g., Basic Income Earth Network) and some economists (e.g., Milton Friedman's negative income tax). Critics from the left (e.g., labor unions) and right (e.g., AEI) attest the problem is misdiagnosed: stigma and complexity are solvable without universality, and the poverty trap is worsened by replacing targeted aid with a flat payment. No independent corroboration outside the benefiting parties (UBI advocates) exists for the claim that universality is necessary to solve these problems.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.78) reflects the net transfer from poor to non-needy: the flat UBI replaces means-tested benefits that are more valuable to low-income households, while the financing falls on taxpayers broadly. Suppression (0.72) captures the active enforcement required to dismantle existing targeted programs and resist restoration attempts — legislative lock-in, budget reconciliation rules, and narrative framing that labels opposition as 'against the poor'. Theater ratio (0.41) acknowledges the genuine administrative simplification (reduced means-testing bureaucracy) but notes that a growing share of political energy defends the universal design against evidence of regressive incidence. Accessibility collapse (0.68) is moderate: alternatives (negative income tax, expanded EITC, targeted child allowances) exist but are politically marginalized by the universalist coalition. Resistance (0.55) reflects pushback from labor unions, anti-poverty advocates, and fiscal conservatives — significant but fragmented across ideological lines.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (working_poor, taxpayers) and beneficiary seats (middle_upper_class, ubi_advocates) compute different effective types. From the working_poor seat, the constraint is a snare: they lose tailored support and face work disincentives. From the middle_upper_class seat, it appears as a rope (a modest universal dividend with low administrative friction). The agenda_setter (government_policymakers) sees a tangled_rope: they must coordinate the coalition that passes UBI while extracting from the fiscal commons. The engine computes this divergence from the structural data — the authored claim (snare) reflects the reading's structural assessment, not a consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: middle_upper_class (collects transfer without need, d≈0.1), ubi_advocates (collects political capital, d≈0.15). Victims declared: working_poor (bear net loss of targeted benefits + work disincentive, d≈0.9), taxpayers (bear $1.4T net cost, d≈0.85). Exit modulation: working_poor are constrained (cannot replace lost targeted aid), taxpayers are constrained (cannot avoid financing), middle_upper_class are mobile (can exit via tax planning), ubi_advocates are arbitrage-grade (their role exists only within the constraint). The engine derives directionality from these declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stigma, complexity, poverty traps) is contested: the dependency_trap_reading argues UBI worsens the poverty trap by replacing high-value targeted aid with a low flat payment and introducing work disincentives. The freedom_floor_reading claims UBI solves these problems. The universality_paradox_reading argues both are captured by a cross-ideological coalition that converges on fiscal expansion. Mandatrophy is unresolved: the original mandate (simplify welfare, empower recipients) has been overridden by the extraction dynamic (upward redistribution, program replacement). The constraint persists because the beneficiary coalition (middle class + advocates) blocks re-targeting, not because the founding problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement,
    'Do the three readings of ''unconditional_income_support'' describe the same constraint viewed from different angles, or three structurally distinct constraints?',
    'Decompose the kernel into separate constraint stories per the ε-invariance principle. If ε differs materially across readings (this reading: 0.78; freedom_floor: likely <0.2; universality_paradox: intermediate), they are distinct constraints. The engine will classify each independently; network.affects_constraints links them.',
    'If they are distinct constraints, the corpus must contain three stories linked by network.affects_constraints. If they are one constraint, the ε-invariance principle is violated and the classification becomes observer-relative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement, conceptual, 'Whether the kernel label conflates multiple constraints with different ε values.').

omega_variable(
    employment_effect_magnitude,
    'Is the -3.2% employment effect from the AEI meta-analysis robust across pilot designs, or does it reflect specific implementation choices (e.g., phase-out rates, benefit levels)?',
    'Meta-analysis of all large-scale UBI/NIT pilots with comparable methodology; decomposition by benefit level, phase-out structure, and labor market context.',
    'If the effect is robust, the snare classification strengthens (extraction via labor supply reduction is structural). If it is implementation-dependent, the constraint may be a tangled_rope where coordination benefits can be preserved with design changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_effect_magnitude, empirical, 'Robustness of the labor supply disincentive estimate.').

omega_variable(
    suppression_mechanism_policy_lockin,
    'Is the suppression of targeted-program alternatives structural (legislative entrenchment, budget rules) or internalized (advocacy groups abandon means-tested reform because universality is the only politically viable frame)?',
    'Track legislative history: count bills proposing targeted expansions vs. universal proposals; survey advocacy organization position changes over time.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the political imagination has been captured by the universalist frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_policy_lockin, empirical, 'Structural vs. internalized suppression of alternative welfare designs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uis_dtr_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uis_dtr_tr_t4, unconditional_income_support__dependency_trap_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(uis_dtr_tr_t8, unconditional_income_support__dependency_trap_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(uis_dtr_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(uis_dtr_tr_t16, unconditional_income_support__dependency_trap_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(uis_dtr_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(uis_dtr_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uis_dtr_be_t4, unconditional_income_support__dependency_trap_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(uis_dtr_be_t8, unconditional_income_support__dependency_trap_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(uis_dtr_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(uis_dtr_be_t16, unconditional_income_support__dependency_trap_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(uis_dtr_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(uis_dtr_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(uis_dtr_su_t4, unconditional_income_support__dependency_trap_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(uis_dtr_su_t8, unconditional_income_support__dependency_trap_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(uis_dtr_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(uis_dtr_su_t16, unconditional_income_support__dependency_trap_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(uis_dtr_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, targeted_welfare_programs).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, progressive_tax_system).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, labor_market_activation_policies).

% DUAL FORMULATION NOTE:
% This story is one of three in the unconditional_income_support constraint family. The dependency_trap_reading (this story) instantiates a snare with high ε. The freedom_floor_reading instantiates a rope with low ε. The universality_paradox_reading instantiates a tangled_rope with intermediate ε. All three share the kernel label but differ in ε, beneficiary/victim structure, and classification. They are linked via network.affects_constraints in each story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__dependency_trap_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
