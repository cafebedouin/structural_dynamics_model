% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling (Constitutional Nullity Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story analyzes the statutory debt ceiling from the perspective of
 *   the 'constitutional nullity' reading, which posits that the 14th
 *   Amendment Section 4 renders the debt ceiling legally inoperative. Under
 *   this reading, the debt ceiling is a statutory constraint that is
 *   superseded by a higher constitutional authority, making its 'enforcement'
 *   largely performative. The Treasury Department is constitutionally
 *   obligated to pay the nation's debts, and the debt ceiling cannot
 *   legitimately prevent this. Any political activity around it is therefore
 *   theatrical, as the underlying constitutional obligation remains
 *   paramount.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.05).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.05).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.9).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, piton).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling (Constitutional Nullity Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, '9c93093e-e88b-412f-b654-9e67d2657a35').
narrative_ontology:cs_kernel_codification('9c93093e-e88b-412f-b654-9e67d2657a35', fixed_text).
narrative_ontology:cs_authority_grounding('9c93093e-e88b-412f-b654-9e67d2657a35', lineage).
narrative_ontology:cs_interpretation_layer_present('9c93093e-e88b-412f-b654-9e67d2657a35').
narrative_ontology:cs_reading_relation('9c93093e-e88b-412f-b654-9e67d2657a35', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('9c93093e-e88b-412f-b654-9e67d2657a35', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('9c93093e-e88b-412f-b654-9e67d2657a35', foundational, public_debt_validity_unquestionable).
narrative_ontology:cs_axiom_status(public_debt_validity_unquestionable, holdable).
narrative_ontology:cs_axiom_grounding('9c93093e-e88b-412f-b654-9e67d2657a35', public_debt_validity_unquestionable, deontological).
narrative_ontology:cs_axiom('9c93093e-e88b-412f-b654-9e67d2657a35', foundational, statutory_subordination_to_constitution).
narrative_ontology:cs_axiom_status(statutory_subordination_to_constitution, holdable).
narrative_ontology:cs_axiom_grounding('9c93093e-e88b-412f-b654-9e67d2657a35', statutory_subordination_to_constitution, deontological).
narrative_ontology:cs_reference_frame('9c93093e-e88b-412f-b654-9e67d2657a35', constitutional_supremacy_framework).
narrative_ontology:cs_drift_state('9c93093e-e88b-412f-b654-9e67d2657a35', contemporary_political_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9c93093e-e88b-412f-b654-9e67d2657a35', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, bond_investors).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, us_citizens).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, congressional_minority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congressional_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the executive branch's fiscal agent, the Treasury is constitutionally obligated to pay the nation's debts. Under this reading, it has the authority to issue debt as required by appropriations, regardless of the statutory debt ceiling, treating the latter as legally inoperative.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, agenda_setter,
    institutional, immediate, analytical, national).

% Benefits from the ability to appropriate funds without the risk of a default crisis, as the Treasury can always pay. Participates in ceremonial votes on the debt ceiling, but its legislative power is not genuinely constrained by it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congressional_majority, beneficiary,
    institutional, biographical, mobile, national).

% Under this reading, the minority loses its primary leverage point for fiscal policy debates, as the threat of default is nullified. Their participation in debt ceiling debates becomes purely performative, with no real power to force spending cuts.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congressional_minority, payer,
    organized, biographical, constrained, national).

% Benefits from the constitutional guarantee that US debt will always be paid, ensuring the stability and reliability of their investments. They largely ignore the political theater around the debt ceiling, trusting in the 14th Amendment.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, bond_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Benefits from the avoidance of economic catastrophe that a default would entail. While subject to the political instability caused by debt ceiling debates, the constitutional nullity reading ensures their financial well-being is not genuinely threatened by it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, us_citizens, beneficiary,
    organized, generational, constrained, national).

% The ultimate arbiter of constitutional law. While not directly involved in the day-to-day operations, its interpretive authority underpins the constitutional nullity reading of the debt ceiling.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures the US government can always meet its financial obligations, preventing a catastrophic default and maintaining the full faith and credit of the United States.
% TRANSFER_FUNCTION: Ensures that funds appropriated by Congress are transferred to creditors as legally required, preventing an involuntary transfer of wealth away from bondholders and other government payees.
% ABSENT_VOICES: Those who believe the statutory debt ceiling is a legitimate and effective check on congressional spending, or those who argue that the 14th Amendment Section 4 is not self-executing in this context, are largely absent from the legal and operational discourse of this reading.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling (even as a nullity) vanished, the political theater surrounding it would cease, potentially leading to a more rational fiscal policy debate. The Treasury's operational clarity would improve, and the 'threat' of default would be entirely removed from political discourse, fundamentally altering the landscape of fiscal governance.
% FOUNDING_PROBLEM: The 14th Amendment Section 4 was enacted to ensure the validity of the public debt, particularly after the Civil War, preventing any future Congress from repudiating debts or assuming Confederate ones.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, constitutional historians, and the consistent practice of the Treasury Department (even when navigating debt ceiling crises) corroborate that the fundamental problem of ensuring the validity of public debt remains live. The text of the 14th Amendment itself serves as primary corroboration.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).
:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness and suppression reflect the core premise of this reading: the debt ceiling, being constitutionally void, cannot genuinely extract resources or suppress Treasury action. Its legal force is negligible. The high theater ratio (0.90) indicates that most activity surrounding the debt ceiling is performative, aimed at political posturing rather than actual fiscal governance. The accessibility collapse and resistance are low because the Treasury, in this reading, has a clear constitutional path to bypass the statutory limit.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the debt ceiling is an inert, performative constraint. However, other readings (e.g., coordination_scaffold_reading, extraction_snare_reading) would assign it significant operational force, leading to vastly different classifications and metric profiles. This divergence highlights the deep conceptual contestation over the debt ceiling's true nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The Treasury Department, bond investors, and US citizens are beneficiaries because the constitutional nullity reading ensures the government can always pay its debts, preventing economic harm. The congressional minority is a 'victim' in the sense that this reading strips them of a key political leverage point, rendering their threats of default moot and their participation in debt ceiling debates largely ceremonial.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_nullity_vs_political_reality,
    'To what extent does the legal nullity of the debt ceiling (per the 14th Amendment) genuinely override its political and economic effects, given that political actors still treat it as a binding constraint?',
    'Empirical analysis of market reactions during debt ceiling crises where the Treasury has signaled intent to invoke the 14th Amendment, compared to crises where it has not. Legal rulings from the Supreme Court on the matter would provide definitive resolution.',
    'If political reality consistently overrides the legal nullity, the constraint''s effective extractiveness and suppression are higher than this reading suggests, potentially reclassifying it towards a Snare or Tangled Rope. If the nullity holds, this Piton classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_nullity_vs_political_reality, empirical, 'Ambiguity between legal status and political perception of the debt ceiling.').

omega_variable(
    self_executing_14th_amendment_scope,
    'Is the 14th Amendment Section 4 truly self-executing in the context of the debt ceiling, or does it require explicit judicial or executive action to be fully operative?',
    'A Supreme Court ruling clarifying the self-executing nature and scope of the 14th Amendment Section 4 regarding the debt ceiling.',
    'If not self-executing, the Treasury''s path to bypass the debt ceiling is less clear, increasing the constraint''s effective suppression and potentially shifting its classification away from Piton towards a more active constraint type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_executing_14th_amendment_scope, conceptual, 'Ambiguity regarding the operational mechanism of the 14th Amendment Section 4.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1917, 0.7).
narrative_ontology:measurement(stat_tr_t1950, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1950, 0.75).
narrative_ontology:measurement(stat_tr_t1980, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1980, 0.8).
narrative_ontology:measurement(stat_tr_t2000, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2000, 0.85).
narrative_ontology:measurement(stat_tr_t2010, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2010, 0.9).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2024, 0.9).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1917, 0.05).
narrative_ontology:measurement(stat_be_t1950, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(stat_be_t1980, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(stat_be_t2000, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(stat_be_t2010, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1917, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1917, 0.05).
narrative_ontology:measurement(stat_su_t1950, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(stat_su_t1980, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(stat_su_t2000, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(stat_su_t2010, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2010, 0.05).
narrative_ontology:measurement(stat_su_t2024, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
