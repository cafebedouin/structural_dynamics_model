% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   This constraint story captures the dependency-trap reading of
 *   unconditional income support: a universal basic income framed as
 *   simplification and dignity, but structurally operating as an
 *   incentive-distorting subsidy that rewards idleness at the margin, crowds
 *   out higher-value targeted aid for the working poor, and redistributes
 *   fiscal resources upward to households that do not need income support.
 *   The AEI meta-analysis of large pilots (Finland, SEED, Ontario) showing
 *   -3.2% employment effects grounds the high extractiveness. The constraint
 *   is a snare: extraction is real (working poor lose more than they gain;
 *   taxpayers fund transfers to non-needy), suppression is active (program
 *   replacement requires legislative force, administrative dismantling, and
 *   political coalition maintenance), and the coordination story (simplicity,
 *   stigma reduction) is cover for the transfer. This is ONE READING of the
 *   contested kernel 'unconditional_income_support'; sibling readings are
 *   'freedom_floor_reading' and 'universality_paradox_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.82).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.76).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Incentive-Distorting Subsidy (Dependency Trap Reading)").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, 'ca93049e-2e82-40ec-a271-511a47dcb41e').
narrative_ontology:cs_kernel_codification('ca93049e-2e82-40ec-a271-511a47dcb41e', distributed).
narrative_ontology:cs_authority_grounding('ca93049e-2e82-40ec-a271-511a47dcb41e', extraction).
narrative_ontology:cs_reading_relation('ca93049e-2e82-40ec-a271-511a47dcb41e', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca93049e-2e82-40ec-a271-511a47dcb41e', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('ca93049e-2e82-40ec-a271-511a47dcb41e', foundational, universality_creates_upward_redistribution).
narrative_ontology:cs_axiom_status(universality_creates_upward_redistribution, holdable).
narrative_ontology:cs_axiom_grounding('ca93049e-2e82-40ec-a271-511a47dcb41e', universality_creates_upward_redistribution, empirically_contingent).
narrative_ontology:cs_axiom('ca93049e-2e82-40ec-a271-511a47dcb41e', foundational, targeted_programs_dominate_universal_transfers_for_poverty_reduction).
narrative_ontology:cs_axiom_status(targeted_programs_dominate_universal_transfers_for_poverty_reduction, holdable).
narrative_ontology:cs_axiom_grounding('ca93049e-2e82-40ec-a271-511a47dcb41e', targeted_programs_dominate_universal_transfers_for_poverty_reduction, empirically_contingent).
narrative_ontology:cs_reference_frame('ca93049e-2e82-40ec-a271-511a47dcb41e', pre_uibi_welfare_architecture).
narrative_ontology:cs_drift_state('ca93049e-2e82-40ec-a271-511a47dcb41e', post_pilot_meta_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ca93049e-2e82-40ec-a271-511a47dcb41e', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalition).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, work_incentive_preservation_doctrine).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, targeted_efficiency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional transfers despite having sufficient market income; the transfer functions as a tax-free supplement rather than subsistence support. Their participation costs are near zero and they face no stigma or conditionality. Exit is trivial — they can ignore the payment without material consequence.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients, beneficiary,
    organized, biographical, arbitrage, national).

% Gain political capital and organizational momentum from universality framing; the policy's broad constituency legitimizes their broader reform agenda. They do not directly administer the program but shape its legislative design and public narrative. Exit means shifting to alternative reform vehicles (e.g., negative income tax, expanded EITC) — organizationally costly but feasible.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalition, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalition, agenda_setter).

% Lose targeted programs (housing vouchers, SNAP, Medicaid expansions, childcare subsidies) whose combined value exceeds the UBI amount. The unconditional payment does not replace conditionally-tailored support for high-fixed-cost needs (rent, medical, childcare). No realistic exit — they cannot individually opt out of program replacement, and labor market alternatives are constrained by the very incentive distortion the policy creates.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, biographical, trapped, national).

% Bear net fiscal cost of ~$1.4T/year after offsetting eliminated programs. The transfer is universal, so a large share flows to non-needy households, increasing deadweight loss relative to targeted transfers. Exit is constrained — they can emigrate or reduce taxable labor, but both carry high personal cost; political exit (voting against) is diluted by the program's broad constituency.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Career civil servants and nonprofit operators of means-tested programs whose institutional rationale and funding streams are eliminated by UBI replacement. Their professional identity is fused to the casework model; they cannot easily pivot to universal administration. They would object to replacement but are not consulted in the universality framing.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, targeted_program_administrators, excluded,
    institutional, biographical, identity_locked, national).

% Analyze employment effects, fiscal incidence, and substitution patterns. The AEI meta-analysis and similar studies inform the structural assessment but do not determine policy. They have no stake in the arrangement's persistence.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, labor_market_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — the arrangement does not solve a coordination problem. It replaces a coordination mechanism (targeted need-matching) with a non-coordinating universal transfer.
% TRANSFER_FUNCTION: Moves ~$1.4T/year net from taxpayers and working poor (via eliminated targeted programs) to middle/upper class recipients and the political capital of the advocacy coalition. The working poor pay via loss of in-kind and means-tested benefits worth more per household than the UBI amount.
% ABSENT_VOICES: Targeted program administrators, caseworkers, and current beneficiaries of means-tested programs who would lose more than they gain — they are structurally excluded from the universality coalition that designs the reform. Also excluded: future cohorts who inherit the fiscal structure without having consented to the program replacement.
% DISAPPEARANCE_RATIONALE: If the UBI and its enabling legislation vanished overnight, targeted programs would need to be rebuilt or re-funded; the working poor would immediately face benefit cliffs; the advocacy coalition would lose its primary legislative vehicle; fiscal space would reopen for means-tested expansion. The welfare state architecture would reorganize around the pre-UBI targeted baseline.
% FOUNDING_PROBLEM: The perceived complexity, stigma, and administrative burden of means-tested welfare; the political difficulty of sustaining targeted programs against middle-class resistance; the desire for a simpler, universal floor that avoids 'welfare traps' at the margin.
% FOUNDING_PROBLEM_CORROBORATION: The advocacy coalition attests the founding problem is live (stigma, cliffs, administrative cost persist). Critics from outside the benefiting parties — including former welfare administrators, labor economists studying employment effects, and targeted-program beneficiaries — attest the problem is either solvable within targeted frameworks (e.g., unified eligibility systems, benefit smoothing) or that UBI creates larger distortions than it solves. No neutral arbiter has validated the founding problem as requiring universality as the only solution.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.82) because the net transfer flows from poor to non-poor via program replacement — the working poor lose targeted benefits worth more per household than the UBI amount, while middle/upper class households receive a pure windfall. The $1.4T net fiscal cost after offsets represents deadweight loss relative to a targeted baseline. Suppression (0.76) reflects the active enforcement needed to dismantle means-tested programs and sustain universality against fiscal pressure — the constraint cannot persist without legislative force. Theater ratio (0.41) captures the gap between the 'simplification/dignity' framing and the actual operation: a growing share of political energy defends the universal transfer to non-needy recipients rather than improving outcomes for the poor. Accessibility collapse (0.68) is moderate-high: once UBI replaces targeted programs, the alternative (means-tested system) is politically and administratively difficult to reconstruct. Resistance (0.59) is significant but fragmented — the working poor lack organized voice; taxpayers are diffuse; the advocacy coalition holds narrative initiative.
 *
 * PERSPECTIVAL GAP:
 *   The working poor seat and the middle/upper class recipient seat should compute radically different types: from the working poor's position, the constraint is a snare that extracts via program replacement; from the middle/upper class recipient's position, it is a rope-like coordination benefit (universal floor, no stigma) with negligible personal cost. The advocacy coalition seat reads it as a scaffold (transitional to broader reform) but the metrics show extraction accumulating. The engine computes this divergence from the structural data — the authored claim (snare) reflects the working-poor/target perspective as structurally dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Middle/upper class recipients are full beneficiaries (d ~0.1): they collect a transfer with no conditionality, no stigma, and no material need. UBI advocacy coalition is a beneficiary via political capital (d ~0.15): universality framing expands their constituency and legitimizes broader reform. Working poor are full targets (d ~0.95): they lose higher-value targeted benefits, face incentive distortion, and have trapped exit. Taxpayers are strong targets (d ~0.85): they bear the deadweight cost with constrained exit. Targeted program administrators are excluded (not in the constraint's beneficiary/victim derivation) but identity-locked — their professional existence is fused to the means-tested model. Labor market economists are analytical observers (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (welfare complexity, stigma, cliffs) is real but the mandated solution (universality) has outlived its coordination function — the constraint now primarily redistributes upward. The mandatrophy is resolved: the arrangement persists not because it solves the founding problem better than alternatives, but because the universality coalition captures the political gains. The working poor's worsening position is the extraction signature; the advocacy coalition's political capital is the beneficiary signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_effect_magnitude,
    'Is the -3.2% employment effect from the AEI meta-analysis structurally robust across implementation designs (phasing, clawback rates, interaction with existing tax-benefit systems), or does it reflect specific pilot features that a permanent national UBI would not replicate?',
    'Replication with varied phase-out structures in permanent policy environments; natural experiments from existing universal child allowances (Canada, Poland) and negative income tax trials (US 1970s) with modern causal methods.',
    'If the employment effect shrinks toward zero under permanent, well-designed phasing, the extractiveness drops and the constraint may reclassify toward tangled_rope (coordination with side effects). If it holds, snare classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_effect_magnitude, empirical, 'Whether the labor supply response is an invariant feature of unconditionality or a design-dependent parameter.').

omega_variable(
    targeted_replacement_necessity,
    'Does UBI structurally REQUIRE the elimination of targeted programs, or is the program-replacement coupling a political choice by the advocacy coalition to fund universality?',
    'Comparative analysis of UBI proposals: some (e.g., Yang 2020) explicitly stack UBI atop existing benefits; others (e.g., Murray 2006) replace the entire welfare state. Track which variants gain legislative traction.',
    'If replacement is politically necessary (fiscal constraint), the snare structure is locked in. If stacking is viable, the constraint decomposes: a rope-like universal floor PLUS a snare-like replacement decision. This would trigger ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeted_replacement_necessity, conceptual, 'Whether the extraction vector (program replacement) is intrinsic to UBI or a separable political choice.').

omega_variable(
    committer_frame_ambiguity,
    'This reading is one of three declared readings of the kernel ''unconditional_income_support''. Does the dependency_trap_reading''s core premise (UBI as incentive-distorting subsidy that crowds out targeted aid) logically foreclose the freedom_floor_reading''s core premise (UBI as autonomy-enabling floor that removes labor coercion), or do they coexist as competing frameworks held by different parties?',
    'Analyze whether a single legislative framework could simultaneously instantiate both readings — e.g., a UBI with a high phase-out rate that preserves work incentives while providing a floor. If yes, they coexist; if the incentive structure of one logically negates the other''s autonomy claim, they foreclose.',
    'If forecloses, the kernel has mutually exclusive readings — the engine should flag structural impossibility of joint instantiation. If coexists_with, both readings are live positions in the same policy space. If influences, this reading''s fiscal pressure constrains the freedom_floor_reading''s viability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Structural relationship between this reading and the freedom_floor_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uis_dtr_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(uis_dtr_tr_t4, unconditional_income_support__dependency_trap_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(uis_dtr_tr_t8, unconditional_income_support__dependency_trap_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(uis_dtr_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(uis_dtr_tr_t16, unconditional_income_support__dependency_trap_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(uis_dtr_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(uis_dtr_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(uis_dtr_be_t4, unconditional_income_support__dependency_trap_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(uis_dtr_be_t8, unconditional_income_support__dependency_trap_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(uis_dtr_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(uis_dtr_be_t16, unconditional_income_support__dependency_trap_reading, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(uis_dtr_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(uis_dtr_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(uis_dtr_su_t4, unconditional_income_support__dependency_trap_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(uis_dtr_su_t8, unconditional_income_support__dependency_trap_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(uis_dtr_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(uis_dtr_su_t16, unconditional_income_support__dependency_trap_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(uis_dtr_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This is the dependency_trap_reading of the unconditional_income_support kernel. It shares the kernel with freedom_floor_reading and universality_paradox_reading. The epsilon values differ substantially: this reading authors high extraction (0.82) from program replacement; freedom_floor_reading would author low extraction (coordination benefit); universality_paradox_reading would author moderate extraction with high ambiguity. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__dependency_trap_reading, organized, 0.15).
constraint_indexing:directionality_override(unconditional_income_support__dependency_trap_reading, powerless, 0.95).
constraint_indexing:directionality_override(unconditional_income_support__dependency_trap_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
