% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'parliamentary sovereignty' reading of
 *   basic law interpretive authority, where the elected legislature holds
 *   final say on constitutional meaning. This reading prioritizes democratic
 *   accountability over judicial independence, leading to a system where
 *   legislative acts can override judicial interpretations. It is one of
 *   three competing readings of the 'basic_law_interpretive_authority'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.7).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'f904b76d-1776-406a-9c89-0d3f55523439').
narrative_ontology:cs_kernel_codification('f904b76d-1776-406a-9c89-0d3f55523439', formalized).
narrative_ontology:cs_authority_grounding('f904b76d-1776-406a-9c89-0d3f55523439', lineage).
narrative_ontology:cs_interpretation_layer_present('f904b76d-1776-406a-9c89-0d3f55523439').
narrative_ontology:cs_reading_relation('f904b76d-1776-406a-9c89-0d3f55523439', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f904b76d-1776-406a-9c89-0d3f55523439', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('f904b76d-1776-406a-9c89-0d3f55523439', foundational, legislative_supremacy_in_interpretation).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('f904b76d-1776-406a-9c89-0d3f55523439', legislative_supremacy_in_interpretation, conventional).
narrative_ontology:cs_axiom('f904b76d-1776-406a-9c89-0d3f55523439', foundational, democratic_accountability_trumps_judicial_review).
narrative_ontology:cs_axiom_status(democratic_accountability_trumps_judicial_review, holdable).
narrative_ontology:cs_axiom_grounding('f904b76d-1776-406a-9c89-0d3f55523439', democratic_accountability_trumps_judicial_review, deontological).
narrative_ontology:cs_reference_frame('f904b76d-1776-406a-9c89-0d3f55523439', unfettered_parliamentary_will).
narrative_ontology:cs_drift_state('f904b76d-1776-406a-9c89-0d3f55523439', contemporary_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f904b76d-1776-406a-9c89-0d3f55523439', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, majority_electorate).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the democratically elected body, the legislature asserts its right to final interpretation of constitutional meaning, overriding judicial review when necessary. It benefits from direct accountability to the majority electorate and the ability to enact policy without judicial obstruction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the direct translation of its will into law through its elected representatives, unhindered by unelected judicial bodies. It perceives the legislature as the most legitimate interpreter of the basic law.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, majority_electorate, beneficiary,
    organized, biographical, mobile, national).

% Its interpretive authority is subordinated to the legislature's, leading to potential overrides of its rulings. This diminishes its institutional independence and the perceived finality of its constitutional judgments, creating gridlock costs when its interpretations are challenged.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_branch, payer,
    institutional, generational, constrained, national).

% Vulnerable to legislative majorities overriding judicial protections for their rights, as their interests may not be adequately represented through majoritarian democratic processes. Their recourse is limited when the legislature holds final interpretive authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, generational, trapped, national).

% Analyze the theoretical and practical implications of parliamentary sovereignty for constitutional stability, rights protection, and democratic legitimacy. They document the institutional dynamics and the impact on different branches of government.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that constitutional interpretation aligns with the contemporary democratic will, providing a clear mechanism for policy implementation and preventing judicial obstruction of legislative programs.
% TRANSFER_FUNCTION: Transfers final interpretive power over constitutional meaning from the judiciary to the legislature, and potentially transfers rights protections from minorities to the discretion of the majority.
% ABSENT_VOICES: Advocates for robust judicial review and international human rights bodies would argue that this reading undermines checks and balances and leaves fundamental rights vulnerable to majoritarianism. They are often excluded from the direct legislative process.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty as final interpretive authority vanished, the judicial branch would immediately assert greater independence, potentially leading to more robust constitutional review and challenges to existing legislation. The balance of power would shift, and the process of lawmaking and constitutional amendment would fundamentally reorganize.
% FOUNDING_PROBLEM: To ensure that the will of the people, expressed through their elected representatives, is supreme in the governance of the nation, preventing unelected bodies from thwarting democratic mandates.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists and historical documents attest to the foundational concern for democratic accountability. Contemporary political discourse, particularly from legislative bodies, continues to emphasize the importance of legislative supremacy in interpreting the basic law, often in tension with judicial rulings.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the legislative process by providing a clear, democratically accountable interpretive authority, but it also involves significant asymmetric extraction from the judicial branch and rights minorities. Extractiveness is high (0.65) due to the power imbalance it creates, and suppression (0.70) is substantial as it actively limits judicial review and minority protections. Theater ratio is low (0.20) because the legislative function is genuinely active, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature and majority electorate, this is a legitimate and necessary coordination mechanism for democratic governance. From the judiciary and rights minorities, it is an extractive mechanism that undermines checks and balances and fundamental rights. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature and the majority electorate are clear beneficiaries, as this reading empowers them directly. The judicial branch and rights minorities are victims, bearing the costs of diminished authority and protection. Constitutional scholars act as observers, analyzing the system's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination). The coordination function of democratic accountability is real, but the asymmetric power transfer and suppression of judicial and minority voices make it a Tangled Rope. The founding problem of ensuring democratic supremacy is still live, but its implementation through this reading creates new forms of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_mandate_scope,
    'Does ''democratic mandate'' legitimately extend to overriding fundamental rights, or is there an irreducible core of rights beyond majoritarian legislative power?',
    'International human rights law adjudication or a constitutional amendment explicitly defining the limits of legislative interpretive authority.',
    'If a core of rights is deemed beyond majoritarian override, the extractiveness from rights minorities would be reclassified as illegitimate, potentially shifting the constraint towards a Snare for that seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_mandate_scope, conceptual, 'The scope of democratic mandate in constitutional interpretation.').

omega_variable(
    judicial_independence_cost,
    'What is the long-term cost to institutional stability and public trust of a judiciary whose constitutional interpretations can be routinely overridden by the legislature?',
    'Comparative institutional analysis across systems with varying degrees of judicial review, tracking public trust in courts and legislative stability over time.',
    'If the cost is severe, the overall efficiency of the coordination function is undermined, and the constraint''s classification might shift towards a Piton if the system becomes dysfunctional but persists due to inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_cost, empirical, 'Long-term institutional costs of subordinated judicial review.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'basic_law_interpretive_authority' kernel. Its structural properties and axioms are distinct from the 'judicial_supremacy_reading' and 'popular_constitutionalism_reading', which represent alternative framings of constitutional interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
