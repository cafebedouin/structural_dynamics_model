% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This story instantiates the dependency-trap reading of the
 *   income-support-conditionality kernel: an unconditional transfer program,
 *   defended by its administrators and political sponsors as a compassionate
 *   floor, is read here as a structure whose absence of work conditionality
 *   produces long-duration labor-market non-participation, skill atrophy
 *   among recipients, and an accumulating fiscal burden on taxpayers with no
 *   corresponding progress toward the program's own stated aim of eventual
 *   self-sufficiency. On this reading the coordination story (a simple,
 *   dignity-preserving floor) is largely cover for what functions as
 *   extraction from two directions at once: recipients whose long-run earning
 *   capacity degrades the longer they remain outside employment, and
 *   taxpayers who fund a transfer that, on this reading, is not achieving its
 *   stated transitional purpose. The claimed type (snare) and the metrics
 *   (rising extractiveness, moderate-to-high suppression via accumulated
 *   skill loss and benefit-cliff-adjacent effects, non-trivial theater in
 *   program messaging) are authored independently and happen to track
 *   together here — that is a finding, not a target.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.71).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.58).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, '402dffba-08a2-44c8-9c64-fa9ab25263ed').
narrative_ontology:cs_kernel_codification('402dffba-08a2-44c8-9c64-fa9ab25263ed', distributed).
narrative_ontology:cs_authority_grounding('402dffba-08a2-44c8-9c64-fa9ab25263ed', distributed).
narrative_ontology:cs_reading_relation('402dffba-08a2-44c8-9c64-fa9ab25263ed', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('402dffba-08a2-44c8-9c64-fa9ab25263ed', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('402dffba-08a2-44c8-9c64-fa9ab25263ed', foundational, work_conditionality_necessary_for_incentive_preservation).
narrative_ontology:cs_axiom_status(work_conditionality_necessary_for_incentive_preservation, holdable).
narrative_ontology:cs_axiom_grounding('402dffba-08a2-44c8-9c64-fa9ab25263ed', work_conditionality_necessary_for_incentive_preservation, empirically_contingent).
narrative_ontology:cs_axiom('402dffba-08a2-44c8-9c64-fa9ab25263ed', secondary, unconditional_transfer_causes_labor_supply_reduction).
narrative_ontology:cs_axiom_status(unconditional_transfer_causes_labor_supply_reduction, holdable).
narrative_ontology:cs_axiom_grounding('402dffba-08a2-44c8-9c64-fa9ab25263ed', unconditional_transfer_causes_labor_supply_reduction, empirically_contingent).
narrative_ontology:cs_reference_frame('402dffba-08a2-44c8-9c64-fa9ab25263ed', conditional_means_tested_welfare_baseline).
narrative_ontology:cs_drift_state('402dffba-08a2-44c8-9c64-fa9ab25263ed', post_pilot_program_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('402dffba-08a2-44c8-9c64-fa9ab25263ed', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, welfare_administration_bureaucracy).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, political_incumbents_claiming_compassion).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpaying_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive an unconditional transfer sufficient for subsistence but structured so that returning to work yields little net gain once benefit withdrawal, lost routine, and skill atrophy are accounted for. Years pass without labor-market attachment; the longer the gap, the harder re-entry becomes, and the payment itself becomes the only stable feature of their situation. Formally free to work, but the accumulated skill and network erosion closes the exit even though no rule bars them from leaving.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, trapped, national).

% Fund the transfer through payroll and income taxation while continuing to work under ordinary labor discipline. They bear the fiscal cost of a program whose recipients, on this reading, are not moving toward self-sufficiency, and they have no mechanism to redirect their contribution or exit the tax obligation short of relocating jurisdictions.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpaying_workers, payer,
    moderate, biographical, constrained, national).

% Designs, administers, and defends the unconditional transfer program. Its budget, headcount, and institutional mandate grow with caseload rather than shrinking as recipients achieve independence, since the program has no work-conditionality or exit requirement to satisfy. Insulated from the consequences of low labor-market re-entry rates because its funding is appropriated independent of outcomes.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, welfare_administration_bureaucracy, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, welfare_administration_bureaucracy, agenda_setter).

% Campaign on having established a compassionate, unconditional safety net, collecting electoral credit for the program's existence. Bear little accountability for its long-run labor-market effects, which unfold over years beyond any single electoral cycle, and can reframe any negative outcome as insufficient funding rather than a design flaw.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, political_incumbents_claiming_compassion, beneficiary,
    powerful, biographical, mobile, national).

% Study labor-force participation, re-entry wages, and skill depreciation among long-term recipients. Their findings are contested by advocates of alternative readings of the same program, and their access to longitudinal recipient data is often limited by the administering bureaucracy's disclosure practices.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_economists_documenting_atrophy, observer,
    analytical, generational, analytical, national).

% Would testify that the applicant pool for entry-level and manual positions has thinned as unconditional support removed the urgency to accept those jobs, but their complaints are typically dismissed in policy debate as self-interested wage-suppression talk rather than evidence of a labor-supply effect. Not consulted in program design.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, employers_seeking_entry_level_labor, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, welfare_administration_bureaucracy).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The transfer program was built to coordinate a floor below which no citizen falls, eliminating the administrative overhead of means-testing and work-verification bureaucracy for basic subsistence provision.
% TRANSFER_FUNCTION: Moves tax revenue from currently working taxpayers to non-conditioned recipients, without the transfer being contingent on job search, training enrollment, or any return to labor-market participation.
% ABSENT_VOICES: Employers reporting labor-supply shortages in entry-level roles, and former caseworkers who administered conditional programs and can speak to observed re-entry rate differences, are largely outside the design conversation, which is dominated by the administering bureaucracy and its political sponsors.
% DISAPPEARANCE_RATIONALE: Recipients dispute that removal would 'rearrange' anything for the better — many would face immediate hardship. Taxpayers and labor economists on this reading argue the labor market would rearrange substantially: re-entry incentives would strengthen and participation would rise, though at real short-term cost to current recipients who have already experienced skill atrophy and would face a harder transition than newer entrants would have.
% FOUNDING_PROBLEM: Poverty and precarity among people who fell through the cracks of conditional, means-tested, and work-requirement-laden welfare systems, which were seen as bureaucratically costly, stigmatizing, and prone to benefit cliffs that trapped people just as effectively as no support at all.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying re-entry and wage trajectories (outside the administering bureaucracy and outside recipient advocacy groups) attest that the original poverty-alleviation problem was real but argue the unconditional design has itself become a source of a *different* problem — long-duration non-participation — that the program's own metrics do not track because it has no work-outcome reporting requirement. The bureaucracy and its political sponsors do not corroborate this shift; they maintain the founding problem is unchanged and still fully live.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, contested).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.35 to 0.71) reflecting this reading's claim that the longer the unconditional structure persists without work incentives, the deeper the accumulated skill atrophy and dependency become — a compounding rather than static harm. Suppression (ending at 0.58) is authored as moderate-high: this is not suppression by force but by structural erosion of alternatives — the longer someone is out of the labor market, the more the exit (returning to work) genuinely closes, even though no external actor is coercively blocking it. Theater ratio rises to 0.42 reflecting increasing gap between the program's stated transitional/dignity framing and its actual non-transitional operation as caseloads persist rather than resolve. Accessibility collapse (0.62) and resistance (0.55) are authored at moderate-high levels appropriate to a contested social-policy snare rather than a mountain: real alternatives (conditional support, wage subsidies, training mandates) exist and are actively argued for, but the described mechanism is claimed to progressively foreclose the recipient's own practical alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the administering bureaucracy's seat this looks like an intact, functioning compassionate program (a rope, on their own account); from the recipient and taxpayer seats under this reading it operates as extraction with a coordination-shaped cover story. The engine's per-seat computation is expected to diverge along exactly this line — that divergence is the analytical content of the story, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Recipients and taxpayers are both declared victims because this reading holds that the transfer's design generates costs for each without generating the promised transition: recipients lose earning capacity and labor-market attachment (d driven toward the full-target end by trapped exit options), taxpayers fund a persistent rather than transitional cost (d driven toward target by lack of exit from the tax obligation). The administering bureaucracy and political sponsors are beneficiaries because institutional budget and electoral credit accrue to them regardless of labor-market outcomes — their exit options (arbitrage, mobile) put them structurally furthest from the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents conflating a program's founding coordination rationale (eliminating a fractured, stigmatizing, benefit-cliff-laden conditional welfare system) with its current operation on this reading (a mechanism that on this account has itself become an unconditional trap). The founding problem (fragmented conditional welfare) may be genuinely dead as a live problem for many recipients, while the arrangement persists and administrators / political sponsors continue to attest that the original problem remains fully live — that status/verdict tension is exactly what the R5 mismatch consumer is built to flag, independent of whichever reading is 'correct.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_causal_mechanism_vs_selection,
    'Does unconditional support causally produce skill atrophy and reduced work incentive, or does the recipient population simply select for people who would have had weak labor-market attachment regardless of program design (reverse causation / selection effect)?',
    'Randomized or quasi-experimental UBI trials with long panel follow-up comparing labor-force re-entry and wage trajectories against matched control populations under conditional and no-transfer regimes.',
    'If causal, this reading''s snare classification is well-grounded in a genuine harm mechanism; if primarily selection, the extractiveness attributed to program design is overstated and the freedom_floor_reading''s non-extractive account becomes more defensible for the same population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_causal_mechanism_vs_selection, empirical, 'Whether atrophy is caused by unconditional design or reflects pre-existing recipient selection.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the underlying unconditional-transfer arrangement better described as a dependency trap (this reading), a decommodifying freedom floor, or a disguised employer wage subsidy — and can any single empirical measurement adjudicate among the three, or do they reflect genuinely different normative framings of the same observable facts?',
    'No single dataset resolves this: the three readings share observables (transfer amount, labor-force participation, wage trends) but assign different normative weight and different beneficiary/victim structures to the same facts. Resolution would require adjudicating the underlying value question (is reduced labor supply at the margin a harm, a freedom, or a wage-suppression enabler) rather than gathering more data.',
    'If the freedom_floor_reading is judged the more defensible framing, the beneficiary/victim structure inverts almost entirely and no snare classification would survive; if the wage_subsidy_reading is judged more defensible, employers rather than the bureaucracy become the primary beneficiary and the victim set shifts toward workers broadly rather than recipients specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Structural indeterminacy among the three sibling readings of the same kernel; routed here per Rule 2 rather than folded into this story''s own ε.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (accumulated exit-closure from skill atrophy) structural — an objective loss of marketable skill and network capital — or partly internalized, where recipients come to see themselves as unemployable and stop attempting re-entry independent of their actual residual capacity?',
    'Compare re-entry rates and self-reported employability beliefs among long-duration recipients offered intensive re-skilling and job-placement support versus those offered the transfer alone; a gap favoring the intervention group over what residual skill measures alone would predict indicates an internalized component.',
    'If substantially internalized, effective suppression is higher than the structural skill-loss measure alone suggests, and the trapped exit_options classification for long-duration recipients is even more strongly warranted than the skill-atrophy story alone implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether recipient exit-closure is a structural skill-capital loss, an internalized employability belief, or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__dependency_trap_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__dependency_trap_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__dependency_trap_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__dependency_trap_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__dependency_trap_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__dependency_trap_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__dependency_trap_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__dependency_trap_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(inco_su_t4, income_support_conditionality__dependency_trap_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(inco_su_t8, income_support_conditionality__dependency_trap_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__dependency_trap_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(inco_su_t16, income_support_conditionality__dependency_trap_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'unconditional income support.' Per the ε-invariance principle, the three readings (dependency_trap_reading, freedom_floor_reading, wage_subsidy_reading) are authored as separate constraint stories with independent ε, beneficiary/victim sets, and claimed types, linked here rather than merged into one story with a hidden observable parameter. This file authors the dependency_trap_reading only; see sibling files for the freedom_floor and wage_subsidy readings of the same underlying policy kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
