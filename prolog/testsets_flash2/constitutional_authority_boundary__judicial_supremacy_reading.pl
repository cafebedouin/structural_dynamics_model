% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy over Constitutional Interpretation
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'judicial supremacy' reading of
 *   the constitutional authority boundary kernel. It describes the judiciary
 *   as the final, unchallengeable arbiter of constitutional questions, with
 *   the power to invalidate legislative and executive acts. This reading
 *   posits a hierarchical structure of constitutional interpretation, where
 *   judicial pronouncements are binding on other branches. The constraint is
 *   claimed as a Tangled Rope, reflecting its dual function of providing
 *   constitutional stability (coordination) while concentrating interpretive
 *   power and extracting policy space from other branches (asymmetric
 *   extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.75).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy over Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, 'd685f644-20e1-40ad-a7e4-3579ef1f156b').
narrative_ontology:cs_kernel_codification('d685f644-20e1-40ad-a7e4-3579ef1f156b', fixed_text).
narrative_ontology:cs_authority_grounding('d685f644-20e1-40ad-a7e4-3579ef1f156b', lineage).
narrative_ontology:cs_interpretation_layer_present('d685f644-20e1-40ad-a7e4-3579ef1f156b').
narrative_ontology:cs_reading_relation('d685f644-20e1-40ad-a7e4-3579ef1f156b', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('d685f644-20e1-40ad-a7e4-3579ef1f156b', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('d685f644-20e1-40ad-a7e4-3579ef1f156b', foundational, judicial_finality_in_constitutional_interpretation).
narrative_ontology:cs_axiom_status(judicial_finality_in_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('d685f644-20e1-40ad-a7e4-3579ef1f156b', judicial_finality_in_constitutional_interpretation, conventional).
narrative_ontology:cs_axiom('d685f644-20e1-40ad-a7e4-3579ef1f156b', foundational, constitution_as_supreme_law_enforced_by_judiciary).
narrative_ontology:cs_axiom_status(constitution_as_supreme_law_enforced_by_judiciary, holdable).
narrative_ontology:cs_axiom_grounding('d685f644-20e1-40ad-a7e4-3579ef1f156b', constitution_as_supreme_law_enforced_by_judiciary, deontological).
narrative_ontology:cs_reference_frame('d685f644-20e1-40ad-a7e4-3579ef1f156b', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('d685f644-20e1-40ad-a7e4-3579ef1f156b', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d685f644-20e1-40ad-a7e4-3579ef1f156b', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, citizenry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional text and declares the final meaning of constitutional provisions. Benefits from an interpretive monopoly, allowing it to invalidate acts of other branches without direct challenge. Its institutional identity is fused with this role.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Passes laws that can be invalidated by judicial review. Bears the cost of constrained policy space and the inability to directly override judicial interpretations. Its options are to amend the constitution (difficult) or pass new laws that attempt to work around judicial rulings.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Implements policies and executes laws that can be struck down by the judiciary. Bears the cost of having its actions and interpretations subjected to final judicial review, limiting its policy discretion.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Subject to constitutional interpretations that may not align with popular will or legislative preferences. Benefits from a stable, consistent constitutional framework but pays the cost of counter-majoritarian outcomes and limited direct influence over constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, citizenry, payer,
    organized, generational, constrained, national).

% Analyze the historical development and contemporary implications of judicial supremacy. Their work often informs legal arguments and public discourse but does not directly alter the constraint's operation.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, authoritative interpretation of the constitutional text, resolving disputes between branches and ensuring a consistent application of fundamental law across the nation.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over constitutional meaning from the legislative and executive branches to the judiciary, thereby transferring policy space and political power.
% ABSENT_VOICES: Advocates for popular sovereignty or legislative supremacy are often marginalized in debates about constitutional meaning, as the judicial supremacy reading frames their arguments as attacks on the rule of law itself. They would argue for a more democratic process for constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the legislative and executive branches would immediately assert their own constitutional interpretations, leading to inter-branch conflicts, inconsistent application of law, and a rapid reorganization of political power dynamics. The stability of the legal system would be profoundly disrupted.
% FOUNDING_PROBLEM: The problem of ensuring a consistent and authoritative interpretation of a supreme law, preventing legislative overreach, and protecting individual rights from majoritarian impulses.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and many legal scholars attest that the founding problem of constitutional consistency and rights protection remains live. Critics (e.g., some political scientists and public law scholars) acknowledge the problem but contest whether judicial supremacy is the optimal or only solution, arguing it creates new problems of democratic deficit. Corroboration for the problem's existence is widespread, but for the solution's efficacy, it is contested.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high due to the judiciary's counter-majoritarian veto power, which can significantly constrain the policy choices of elected branches. Suppression (0.75) is also high, as there are few effective remedies for other branches to challenge or override judicial constitutional interpretations, short of difficult constitutional amendment processes. The theater ratio (0.15) is low, indicating that the judiciary's interpretive function is largely genuine, though some performative aspects exist in the presentation of its authority. Accessibility collapse (0.70) is high because alternative paths to constitutional interpretation are largely foreclosed by the judiciary's finality claim. Resistance (0.45) is moderate, as other branches and the citizenry often express disagreement but lack direct means to overturn judicial rulings.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this constraint is a necessary Rope ensuring the rule of law and constitutional fidelity. From the perspective of the legislative and executive branches, it often functions as a Snare, limiting their democratic mandate and policy flexibility. The citizenry's view is mixed, appreciating stability but sometimes resenting counter-majoritarian outcomes. The engine's computation of per-seat classifications will reflect these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the primary beneficiary (d near 0.0) as it gains interpretive monopoly and institutional power. The legislature and executive are targets (d near 1.0) as their policy space is constrained and their acts are subject to invalidation. The citizenry is also a target, bearing the costs of limited democratic input on constitutional meaning, despite benefiting from constitutional stability. The high suppression and limited exit options for the other branches amplify their target directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling judicial supremacy as a pure Mountain (natural law) or a pure Rope (simple coordination). While it provides a coordination function (consistent constitutional interpretation), the significant and actively enforced extraction of policy space from other branches, coupled with identifiable beneficiaries and victims, points to its hybrid nature. The rising extractiveness and suppression over time suggest an accumulation of power beyond its initial coordination mandate, indicating a potential for mandatrophy if the coordination function becomes secondary to the extraction of interpretive rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_monopoly_legitimacy,
    'Is the judiciary''s interpretive monopoly a legitimate and necessary feature of constitutional governance, or an overreach of institutional power?',
    'Comparative constitutional analysis across systems with different models of constitutional review (e.g., parliamentary supremacy, Kelsenian courts, coordinate construction).',
    'If deemed an overreach, the constraint''s legitimacy would be undermined, potentially leading to increased resistance and calls for reform. If deemed necessary, its stability would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_legitimacy, conceptual, 'The legitimacy of the judiciary''s final interpretive authority.').

omega_variable(
    counter_majoritarian_difficulty_impact,
    'To what extent does judicial supremacy genuinely protect minority rights and fundamental liberties, versus merely imposing the preferences of an unelected body?',
    'Empirical studies on the outcomes of judicial review, analyzing the beneficiaries and victims of landmark constitutional rulings over time.',
    'If protection of rights is consistently demonstrated, the coordination function is strengthened. If judicial preferences dominate, the extractive nature is amplified, potentially increasing calls for democratic accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_difficulty_impact, empirical, 'The actual impact of judicial supremacy on democratic outcomes and rights protection.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of legislative/executive constitutional interpretation structural (lack of formal override mechanisms) or internalized (deference to judicial authority)?',
    'Analysis of legislative and executive branch behavior in response to judicial rulings: does resistance manifest as attempts to circumvent or as explicit challenges to the interpretive hierarchy?',
    'If largely internalized, the constraint''s effective suppression is higher than structural measures suggest, as deference persists even where formal barriers might be overcome. If purely structural, removing formal barriers would immediately increase interpretive contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for constitutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1789, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(cons_tr_t1803, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1803, 0.08).
narrative_ontology:measurement(cons_tr_t1865, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1865, 0.1).
narrative_ontology:measurement(cons_tr_t1937, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1937, 0.12).
narrative_ontology:measurement(cons_tr_t1970, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(cons_tr_t2024, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t1789, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(cons_be_t1803, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1803, 0.5).
narrative_ontology:measurement(cons_be_t1865, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1865, 0.55).
narrative_ontology:measurement(cons_be_t1937, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1937, 0.6).
narrative_ontology:measurement(cons_be_t1970, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(cons_be_t2024, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1789, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(cons_su_t1803, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1803, 0.4).
narrative_ontology:measurement(cons_su_t1865, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1865, 0.55).
narrative_ontology:measurement(cons_su_t1937, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1937, 0.65).
narrative_ontology:measurement(cons_su_t1970, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(cons_su_t2024, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__judicial_supremacy_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional authority boundary' kernel. It is linked to sibling readings that offer alternative framings of constitutional interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
