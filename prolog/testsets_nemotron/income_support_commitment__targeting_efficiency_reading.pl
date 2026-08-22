% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeted Income Support — Efficiency Reading (UBI Cannibalization)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story captures the 'targeting efficiency' reading of the
 *   income support commitment kernel. The reading holds that scarce fiscal
 *   resources should be concentrated on those with demonstrated need via
 *   means-tested programs, and that universal basic income financed by
 *   eliminating those programs is a legitimate reform path. The structural
 *   delta is stark: the primary beneficiaries of the current targeted system
 *   (low-income households receiving stacked benefits) become the primary
 *   victims under the proposed UBI replacement, losing an average of $19,100
 *   in net support (Queens parent example: $31,100 targeted stack vs.
 *   ~$12,000 UBI). The constraint operates as a snare — it presents as
 *   coordination (fair allocation of scarce resources) but functions as
 *   extraction from the poor to fund a universal floor that does not
 *   replicate their current support depth. The coordination story is cover;
 *   the constraint's persistence depends on suppressing the alternative of
 *   additive UBI (funded by new revenue, not cannibalization) and on the
 *   administrative machinery that makes targeting appear technically
 *   necessary rather than politically chosen.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.78).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.65).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeted Income Support — Efficiency Reading (UBI Cannibalization)").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, '06e63137-b26b-4ada-b84c-ba7334690368').
narrative_ontology:cs_kernel_codification('06e63137-b26b-4ada-b84c-ba7334690368', formalized).
narrative_ontology:cs_authority_grounding('06e63137-b26b-4ada-b84c-ba7334690368', extraction).
narrative_ontology:cs_interpretation_layer_present('06e63137-b26b-4ada-b84c-ba7334690368').
narrative_ontology:cs_reading_relation('06e63137-b26b-4ada-b84c-ba7334690368', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('06e63137-b26b-4ada-b84c-ba7334690368', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('06e63137-b26b-4ada-b84c-ba7334690368', foundational, targeted_allocation_maximizes_poverty_reduction_per_dollar).
narrative_ontology:cs_axiom_status(targeted_allocation_maximizes_poverty_reduction_per_dollar, holdable).
narrative_ontology:cs_axiom_grounding('06e63137-b26b-4ada-b84c-ba7334690368', targeted_allocation_maximizes_poverty_reduction_per_dollar, empirically_contingent).
narrative_ontology:cs_axiom('06e63137-b26b-4ada-b84c-ba7334690368', foundational, ubi_must_be_financed_by_existing_program_elimination).
narrative_ontology:cs_axiom_status(ubi_must_be_financed_by_existing_program_elimination, holdable).
narrative_ontology:cs_axiom_grounding('06e63137-b26b-4ada-b84c-ba7334690368', ubi_must_be_financed_by_existing_program_elimination, conventional).
narrative_ontology:cs_reference_frame('06e63137-b26b-4ada-b84c-ba7334690368', postwar_targeted_welfare_settlement).
narrative_ontology:cs_drift_state('06e63137-b26b-4ada-b84c-ba7334690368', ubi_cannibalization_proposal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('06e63137-b26b-4ada-b84c-ba7334690368', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, program_administrators).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, means_testing_bureaucracy).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, universal_basic_income_proponents_funded_by_reallocation).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, low_income_households_with_high_benefit_cliffs).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, caregivers_losing_targeted_supplements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce means-testing regimes, eligibility thresholds, and benefit formulas. Their institutional budgets, staffing, and rule-making authority expand with program complexity. They control the administrative machinery that determines who receives what, and they capture career and budgetary rents from the system's operational demands.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, program_administrators, beneficiary).

% Frontline caseworkers, eligibility technicians, fraud investigators, and appeals adjudicators whose employment and professional identity depend on the targeted system's procedural demands. They process applications, verify income, monitor compliance, and administer sanctions. Their institutional interests align with procedural complexity, not beneficiary outcomes.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, means_testing_bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% Advocates and policymakers who propose financing UBI by eliminating or cannibalizing existing targeted programs. They gain a universal floor but structurally require the extraction of targeted benefits from current recipients to fund it. Their policy model treats the poor's existing support as a fungible revenue pool.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, universal_basic_income_proponents_funded_by_reallocation, beneficiary,
    moderate, generational, mobile, national).

% Households receiving means-tested benefits (e.g., a Queens parent with $31,100 in combined benefits: housing, nutrition, childcare, Medicaid). Under UBI replacement funded by program elimination, they lose $19,100 net because the universal payment does not replicate the targeted stack's depth for high-need configurations. They have no exit from the policy choice — they cannot opt out of the reform, cannot access alternative safety nets, and bear the full cost of the transition.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients, payer).

% Families whose targeted benefits phase out steeply with small income increases. The targeted system already extracts via high effective marginal tax rates; UBI replacement flattens the benefit but at a level far below their current total support. They lose the specific supplements (childcare, disability, housing) that the universal payment does not differentiate for.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, low_income_households_with_high_benefit_cliffs, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, low_income_households_with_high_benefit_cliffs, payer).

% Parents of disabled children, elderly caregivers, and others receiving targeted supplements for care-related costs. A flat UBI does not cover the marginal cost of care that targeted programs address. They are structurally unable to exit the care obligation, making them captive to the benefit reduction.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, caregivers_losing_targeted_supplements, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, caregivers_losing_targeted_supplements, payer).

% Legislators and budget officials who use targeting rhetoric to justify spending caps. They benefit from the constraint's framing because it legitimizes austerity: 'we help those who truly need it' becomes the rationale for not expanding the pie. They do not administer programs but set the fiscal envelope that makes targeting a zero-sum allocation rule.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, fiscal_conservatives_targeting_spending, agenda_setter,
    institutional, generational, arbitrage, national).

% Analyze distributional impacts, labor supply effects, and administrative costs of targeted vs. universal designs. They see the full structure: the administrative burden on recipients, the fiscal tradeoffs, the political coalition dynamics. Their seat is analytical — they neither collect nor pay, but their evidence shapes the legitimacy of each reading.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, social_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of a finite fiscal pool to households with demonstrated material need, using means-testing as the allocation mechanism. Solves the problem: given limited resources, how to direct them to the most deprived without universal outlay.
% TRANSFER_FUNCTION: Moves fiscal resources from the general tax base to targeted beneficiaries via a bureaucratic filtering apparatus. The apparatus itself consumes a share (administrative costs, compliance costs imposed on applicants). Under UBI replacement, the transfer reverses: resources move FROM current targeted recipients (who lose net support) TO the universal pool that funds UBI for all, including non-poor households.
% ABSENT_VOICES: Future cohorts who would face a permanently lowered targeted-support ceiling if UBI cannibalization becomes the financing norm. Also absent: recipients in jurisdictions without targeted programs who are told the 'universal' model is the only feasible path — they never had the targeted stack to lose, so their absence is structural, not incidental.
% DISAPPEARANCE_RATIONALE: If the targeting-efficiency constraint vanished overnight, the fiscal politics of income support would reorganize: either universal programs expand without cannibalizing targeted ones (new revenue), or targeted programs expand without the 'efficiency' discipline (deficit spending), or a hybrid emerges. The current zero-sum framing — UBI *requires* cutting targeted aid — is the constraint's active suppression of alternatives. Remove it, and the budget constraint becomes a political choice, not a structural necessity.
% FOUNDING_PROBLEM: Post-war welfare states faced fiscal limits and political resistance to broad taxation. Targeting emerged as the compromise: concentrate limited funds on the 'truly needy' to maximize poverty reduction per dollar, buying political viability for redistribution. The administrative state built the means-testing machinery to operationalize this compromise.
% FOUNDING_PROBLEM_CORROBORATION: Historical institutionalists (e.g., Skocpol, Pierson) document the political coalition that produced targeting as a fiscal-political compromise, not a technical optimum. Feminist economists (e.g., Folbre, England) show the founding problem excluded care work from 'need' calculations. The 'efficiency' claim is corroborated as a political settlement by scholars outside the benefiting administrative coalition; the technical optimality claim is not corroborated by independent evidence — it is the administrators' self-justification.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint's operational logic — UBI funded by targeted-program elimination — transfers resources from the most vulnerable to a universal pool that includes the non-poor. Suppression (0.65) is substantial: the 'fiscal necessity' framing actively suppresses additive UBI proposals and treats the targeted stack as a fungible revenue source rather than a floor. Theater ratio (0.22) is moderate-low: the administrative apparatus performs genuine filtering work, but a growing share of its complexity serves to defend the zero-sum framing rather than improve targeting accuracy. Accessibility collapse (0.48) is moderate: alternatives exist (additive UBI, negative income tax, universal child allowance) but are rendered 'unrealistic' by the constraint's fiscal discipline discourse. Resistance (0.55) is significant: recipients, advocates, and some policymakers resist benefit cuts, but the power asymmetry is extreme.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator seat, the constraint is genuine coordination: limited funds, fair allocation, bureaucratic rationality. From the targeted recipient seat, the same structure is a snare: the 'efficiency' rationale extracts their support to fund a universal benefit that leaves them worse off, and they have no exit. The engine computes this divergence from the declared power/exit/role structure — the claimed type (snare) reflects the analytical seat's reading, not the administrator's self-justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Program administrators and means-testing bureaucracy are structural beneficiaries (d near 0.1-0.2): they capture budgetary and career rents from the system's complexity. UBI-proponents-funded-by-reallocation are also beneficiaries (d ~0.3): they gain a universal program at the cost of current recipients. Targeted recipients, low-income households with cliffs, and caregivers are structural targets (d near 0.9-1.0): they bear the full extraction with trapped or constrained exit. Fiscal conservatives are agenda-setters with arbitrage exit (d ~0.2): they use the constraint to cap spending. Social policy researchers are analytical observers (d=0.5): they see the structure but do not collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fiscal limits + political resistance to broad taxation) is contested: fiscal limits are softer in a sovereign currency context; political resistance is endogenous to the targeting discourse itself. The targeting machinery has atrophied into a self-justifying apparatus — its complexity is now the rationale for its persistence. The mandatrophy is unresolved: the constraint persists because the administrative coalition and the fiscal-conservative coalition jointly benefit from the zero-sum framing, even as the original compromise conditions have shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    additive_ubi_feasibility,
    'Is UBI funded by new revenue (taxation, sovereign money, resource dividends) politically achievable, or is cannibalization the only feasible path?',
    'Legislative history of UBI proposals: track whether any major proposal funds UBI additively vs. by program elimination. Political economy analysis of revenue-side coalitions.',
    'If additive UBI is feasible, the targeting-efficiency constraint''s suppression of alternatives is exposed as political choice, not structural necessity — the snare classification strengthens. If cannibalization is the only path, the constraint''s coordination function gains empirical grounding (though extraction from poor remains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(additive_ubi_feasibility, empirical, 'Whether the zero-sum framing reflects political reality or ideological construction.').

omega_variable(
    administrative_cost_vs_extraction,
    'How much of the targeted system''s administrative cost is genuine coordination overhead vs. extraction-enabling complexity (e.g., benefit cliffs, sanctions, fraud investigation that exceeds fraud losses)?',
    'Comparative administrative cost studies: targeted vs. universal delivery. Audit of sanction regimes vs. measured fraud rates. Micro-simulation of benefit cliff effective marginal tax rates.',
    'If administrative costs are predominantly extraction-enabling, the theater ratio is understated and the snare classification deepens. If costs are genuine coordination overhead, the rope component of the constraint is larger than measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administrative_cost_vs_extraction, empirical, 'Decomposition of administrative burden into functional vs. extractive components.').

omega_variable(
    targeting_efficiency_naturalness,
    'Is the ''targeting efficiency'' axiom a genuine technical principle or a constructed doctrine that serves the administrative and fiscal-conservative coalitions?',
    'Genealogy of the targeting efficiency concept: trace its emergence in welfare economics (Mirrlees, Besley-Coate) vs. its deployment in policy discourse. Test whether the technical literature''s conditions (perfect information, no transaction costs, no behavioral responses) hold in practice.',
    'If the axiom is a constructed doctrine deployed to legitimize extraction, the false_summit_mountain signature would apply to any mountain-like framing of ''technical necessity'' — but this reading already claims snare, so the omega documents the epistemic status of its own coordinating myth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_efficiency_naturalness, conceptual, 'Epistemic status of the constraint''s coordinating justification.').

omega_variable(
    committer_frame_disagreement_location,
    'Where exactly do the three kernel readings disagree structurally — on the beneficiary set, the extraction mechanism, the coordination function, or the founding problem?',
    'Map each reading''s beneficiary/victim declarations, transfer functions, and founding problem status. The disagreement is located in the transfer function (cannibalization vs. additive funding) and the victim set (current recipients as losers vs. non-recipients as gainers).',
    'Clarifies that the kernel contest is not about ''UBI good/bad'' but about the structural relationship between universal and targeted layers. This reading''s victim set IS the freedom_floor_reading''s beneficiary set — the same households appear on opposite sides of the extraction ledger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Structural locus of disagreement across the income_support_commitment kernel''s readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 1965, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isc_ter_tr_t1965, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(isc_ter_tr_t1975, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(isc_ter_tr_t1985, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(isc_ter_tr_t1995, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(isc_ter_tr_t2005, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2005, 0.21).
narrative_ontology:measurement(isc_ter_tr_t2015, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(isc_ter_tr_t2025, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(isc_ter_be_t1965, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(isc_ter_be_t1975, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1975, 0.42).
narrative_ontology:measurement(isc_ter_be_t1985, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(isc_ter_be_t1995, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(isc_ter_be_t2005, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(isc_ter_be_t2015, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2015, 0.73).
narrative_ontology:measurement(isc_ter_be_t2025, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(isc_ter_su_t1965, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(isc_ter_su_t1975, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(isc_ter_su_t1985, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(isc_ter_su_t1995, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(isc_ter_su_t2005, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(isc_ter_su_t2015, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(isc_ter_su_t2025, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__targeting_efficiency_reading, 0.15).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the income_support_commitment kernel. The freedom_floor_reading and dependency_trap_reading are sibling constraints with different beneficiary/victim structures, transfer functions, and founding problem statuses. All three share the kernel_id but instantiate distinct constraints with independent ε values. The targeting_efficiency_reading's high ε (0.78) contrasts with the freedom_floor_reading's lower ε (additive funding) and the dependency_trap_reading's different victim set (future dependents, not current recipients).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, moderate, 0.3).
constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
