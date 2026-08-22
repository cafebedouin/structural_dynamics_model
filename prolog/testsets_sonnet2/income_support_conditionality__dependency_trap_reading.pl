% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/labor
 *
 * SUMMARY:
 *   An unconditional income support program disburses a subsistence-level
 *   transfer to all eligible residents regardless of work status or job
 *   search activity. From the dependency-trap reading, the absence of any
 *   work requirement or benefit taper removes the pressure to maintain
 *   labor-market attachment; over a multi-year interval, recipients
 *   experience skill atrophy, network decay, and increasingly severe
 *   effective marginal tax rates on any return to work, while the
 *   administering bureaucracy's institutional interests scale with caseload
 *   rather than exit outcomes. The claim (snare) and the metrics (extraction
 *   rising from 0.42 to 0.71, suppression rising from 0.38 to 0.58) are
 *   authored independently — this is what the reading believes is
 *   descriptively and structurally true of the arrangement it is examining,
 *   not a target the numbers were tuned to hit.
 *
 * KEY AGENTS:
 *   - ubi_recipients: primary target (powerless/trapped) — bears skill atrophy and reentry-cliff cost
 *   - general_taxpayers: secondary target (moderate/constrained) — funds non-productive transfer with no return
 *   - benefits_administration_bureaucracy: agenda setter and structural beneficiary (institutional/arbitrage) — caseload-scaled mandate grows with dependency
 *   - low_wage_employers: excluded party (powerful/mobile) — absorbs labor-supply effect with no design voice
 *   - policy_economists: analytical observer (analytical/analytical) — measures the mechanism without administrative power to change it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.71).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.58).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, '1701ae4a-7283-4e53-b027-900dc1153e0a').
narrative_ontology:cs_kernel_codification('1701ae4a-7283-4e53-b027-900dc1153e0a', formalized).
narrative_ontology:cs_authority_grounding('1701ae4a-7283-4e53-b027-900dc1153e0a', expertise).
narrative_ontology:cs_interpretation_layer_present('1701ae4a-7283-4e53-b027-900dc1153e0a').
narrative_ontology:cs_reading_relation('1701ae4a-7283-4e53-b027-900dc1153e0a', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('1701ae4a-7283-4e53-b027-900dc1153e0a', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('1701ae4a-7283-4e53-b027-900dc1153e0a', foundational, continuous_labor_attachment_is_a_protected_good).
narrative_ontology:cs_axiom_status(continuous_labor_attachment_is_a_protected_good, holdable).
narrative_ontology:cs_axiom_grounding('1701ae4a-7283-4e53-b027-900dc1153e0a', continuous_labor_attachment_is_a_protected_good, empirically_contingent).
narrative_ontology:cs_axiom('1701ae4a-7283-4e53-b027-900dc1153e0a', secondary, unconditional_transfer_design_causes_measurable_skill_atrophy).
narrative_ontology:cs_axiom_status(unconditional_transfer_design_causes_measurable_skill_atrophy, holdable).
narrative_ontology:cs_axiom_grounding('1701ae4a-7283-4e53-b027-900dc1153e0a', unconditional_transfer_design_causes_measurable_skill_atrophy, empirically_contingent).
narrative_ontology:cs_reference_frame('1701ae4a-7283-4e53-b027-900dc1153e0a', labor_market_attachment_primacy).
narrative_ontology:cs_drift_state('1701ae4a-7283-4e53-b027-900dc1153e0a', post_multi_year_enrollment_cohorts, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1701ae4a-7283-4e53-b027-900dc1153e0a', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, benefits_administration_bureaucracy).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, general_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive an unconditional transfer sufficient for subsistence but structured such that returning to work triggers effective marginal tax rates near or above 100% once benefit withdrawal, lost eligibility, and foregone administrative goodwill are counted. Over years, skills atrophy, work history gaps widen, and professional networks decay. The transfer itself becomes the only stable feature of their economic life, and exiting the arrangement means voluntarily absorbing an income cliff most cannot bridge without a job already in hand — which the atrophy has made harder to get.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, trapped, national).

% Fund the transfer program through general taxation with no direct return — no labor output, no tax revenue from recipients, no reduction in future dependency. Individually they cannot opt out of the tax obligation; collectively their only lever is electoral, which acts on a multi-year cycle far slower than the compounding fiscal and social cost of expanding non-productive rolls.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, general_taxpayers, payer,
    moderate, generational, constrained, national).

% Designs eligibility rules, administers disbursement, and justifies the unconditional structure as simplicity and dignity. Its budget, staffing, and institutional mandate scale with caseload rather than with exit-to-employment outcomes, so it bears no structural cost when dependency persists or grows — and gains resourcing and relevance the larger the dependent population becomes.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, benefits_administration_bureaucracy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, benefits_administration_bureaucracy, beneficiary).

% Would prefer a labor supply with strong reentry incentives at the wages they offer; under the dependency-trap structure they see fewer entry-level applicants and rising wage floors as the reservation wage created by unconditional support exceeds what many jobs pay. They have no formal voice in benefit design and are not consulted on eligibility rules, despite absorbing the labor-supply effect directly.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, low_wage_employers, excluded,
    powerful, biographical, mobile, national).

% Study labor-supply elasticity, marginal effective tax rates, and longitudinal employment outcomes for recipients. They can identify the dependency mechanism empirically but hold no administrative authority to redesign the program; their findings enter policy debate but do not automatically change enforcement structure.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, policy_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, benefits_administration_bureaucracy).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a basic income floor without means-testing bureaucracy at the point of disbursement, avoiding the humiliation and administrative cost of proving need repeatedly.
% TRANSFER_FUNCTION: Moves general tax revenue to recipients on an unconditional basis, and moves opportunity cost (foregone labor market attachment, skill development, wage growth) from the transfer's designers onto the recipients who receive it long enough to atrophy.
% ABSENT_VOICES: Low-wage employers who bear the labor-supply consequences are not part of eligibility design. Former recipients who successfully exited and would testify to the difficulty of the reentry cliff are also structurally absent from ongoing program review, which draws mainly on current-recipient satisfaction surveys.
% DISAPPEARANCE_RATIONALE: If the unconditional transfer vanished overnight, long-term recipients would face an acute income shock with degraded labor-market readiness; the administering bureaucracy would lose its caseload-based mandate and shrink; taxpayers would see an immediate reduction in transfer-program tax burden; low-wage employers would see labor supply increase and reservation wages fall. All four seats' arrangements are built around the transfer's existence.
% FOUNDING_PROBLEM: Means-tested welfare created high administrative overhead, stigmatizing eligibility checks, and sharp benefit cliffs that also produced work disincentives — unconditional transfer was proposed to remove the bureaucratic gatekeeping and stigma.
% FOUNDING_PROBLEM_CORROBORATION: The administering bureaucracy and current recipients attest the original stigma-reduction problem remains live and justifies the unconditional design. Independent labor economists and legislative auditors outside the benefiting administrative apparatus attest that the original problem has been substantially replaced by a new one — chronic non-employment and skill loss — that the unconditional structure itself now sustains rather than solves.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction rises over the interval (0.42 to 0.71) as the compounding effect of skill atrophy and benefit-cliff entrenchment deepens — early recipients face a milder reentry gap; long-tenure recipients face a much steeper one, and the fiscal burden on taxpayers accumulates correspondingly. Suppression (0.58 at end) reflects the effective marginal tax rate wall on returning to work rather than overt coercion — the trap is structural (benefit withdrawal mathematics) more than administrative. Theater ratio stays comparatively low (0.28) because the disbursement mechanism genuinely functions as designed; the extraction is not performative, it is a byproduct of the incentive structure operating as intended for a different goal (stigma removal) than the one now being evaluated (labor-market attachment).
 *
 * PERSPECTIVAL GAP:
 *   The bureaucracy's seat and the recipient/taxpayer seats compute to different types under this reading: from the agenda-setter's position the arrangement is stable, well-functioning coordination (uniform, low-overhead disbursement); from the recipient and taxpayer seats the same structure computes as extraction with a high and rising cost of exit. This divergence is exactly what distinguishes the dependency-trap reading from the freedom-floor reading of the identical mechanism — the freedom-floor reading would compute the recipient seat as a beneficiary (decommodified from coercive work), while this reading computes the same seat as trapped. The difference is not measurement error; it is a difference in which normative premise about the value of continuous labor-market attachment is doing the work.
 *
 * DIRECTIONALITY LOGIC:
 *   Recipients are declared victims because the program's design captures them in a low-mobility equilibrium: the transfer plus benefit-cliff mathematics makes them structurally worse off in labor-market terms the longer they remain enrolled, even though each individual disbursement is a nominal subsidy. This is why the derivation would misfire without explicit victim declaration — a naive read of 'recipient of a transfer' would code as pure beneficiary (low d), but the dependency-trap reading holds that the long-run trajectory reverses the sign: what looks like subsidy in the disbursement ledger is extraction in the labor-capital ledger. Taxpayers are victims by direct fiscal transfer with no productive return under this reading. The administering bureaucracy is the true structural beneficiary — caseload growth expands its budget and mandate durability — which is why it is coded as agenda_setter plus secondary beneficiary rather than a neutral administrator.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stigmatizing, high-overhead means-testing) is largely solved by the unconditional mechanism, but the mechanism's persistence is no longer justified by that original problem under this reading — it persists because the administering bureaucracy's institutional interest and the recipients' short-horizon incentive both favor continuation, while the long-horizon costs (skill atrophy, fiscal burden) are diffuse and slow to surface. Classifying this as snare rather than rope prevents the error of treating a program that removed one real problem (administrative stigma) as costless simply because its original justification was sound — the founding problem's partial obsolescence combined with the new problem it generates (dependency) is precisely the contested-status genealogy captured in six_questions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is the dependency-trap reading, the freedom-floor reading, or the wage-subsidy reading the structurally correct account of what unconditional income support does to labor-market behavior — or do all three operate simultaneously on different subpopulations?',
    'Longitudinal labor-supply studies segmenting recipients by pre-transfer employment history, sector, and local wage floor; compare actual reentry rates and wage trajectories against each reading''s predicted mechanism.',
    'If dependency-trap effects dominate for a subpopulation while freedom-floor effects dominate for another, the single-reading classification is a coarse aggregate over a genuinely heterogeneous population — this would argue for further decomposition rather than resolution in favor of any one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, empirical, 'Which kernel reading (or mixture) best fits observed recipient behavior.').

omega_variable(
    counterfactual_labor_attachment,
    'Would the recipients now showing skill atrophy have maintained stronger labor-market attachment under a conditional (work-requirement) alternative, or were they already structurally excluded from stable employment before the transfer existed?',
    'Natural experiment comparing regions with unconditional transfers to matched regions with work-conditional transfers, controlling for pre-existing labor market conditions and recipient demographics.',
    'If pre-existing exclusion dominates, the dependency-trap reading over-attributes causation to the transfer design rather than to prior structural unemployment — this would reduce confidence in the snare classification and shift weight toward a rope or scaffold reading of the same mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_labor_attachment, empirical, 'Whether the transfer causes dependency or merely reveals pre-existing labor-market exclusion.').

omega_variable(
    bureaucratic_capture_degree,
    'Does the administering bureaucracy actively resist reforms that would reduce caseload (e.g., work-requirement pilots, time limits), or does it implement whatever policy legislators direct without institutional self-interest shaping outcomes?',
    'Review internal agency communications, budget request patterns relative to caseload projections, and agency positions taken during legislative reform hearings.',
    'Active resistance would corroborate the beneficiary/agenda_setter dual-role coding and strengthen the snare classification; passive implementation would weaken the case for treating the bureaucracy as a structural beneficiary and might shift it toward a pure agenda_setter with no capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bureaucratic_capture_degree, empirical, 'Whether administrative self-interest actively shapes program persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__dependency_trap_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__dependency_trap_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__dependency_trap_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__dependency_trap_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__dependency_trap_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__dependency_trap_reading, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__dependency_trap_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__dependency_trap_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__dependency_trap_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__dependency_trap_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(inco_su_t4, income_support_conditionality__dependency_trap_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(inco_su_t8, income_support_conditionality__dependency_trap_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__dependency_trap_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(inco_su_t16, income_support_conditionality__dependency_trap_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(inco_su_t24, income_support_conditionality__dependency_trap_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__dependency_trap_reading, 0.12).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint-family members reading the same kernel (income_support_conditionality). dependency_trap_reading authors ε=0.71 with recipients and taxpayers as victims and the administering bureaucracy as beneficiary (snare). freedom_floor_reading authors a low ε for the same transfer mechanism with recipients as beneficiaries (decommodification, rope). wage_subsidy_reading authors moderate-high ε with low-wage employers as beneficiaries and recipients/taxpayers as victims of wage suppression (tangled_rope). All three share the identical disbursement text and are linked via affects_constraints rather than merged, per the ε-invariance decomposition rule — each reading's beneficiary/victim structure and classification is authored independently of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
