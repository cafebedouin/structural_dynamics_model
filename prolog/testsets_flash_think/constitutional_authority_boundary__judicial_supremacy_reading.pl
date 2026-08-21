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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy over Constitutional Interpretation
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of the
 *   constitutional authority boundary, where courts are the final,
 *   unchallengeable arbiters of constitutional questions. This reading grants
 *   the judiciary a powerful counter-majoritarian veto over legislative and
 *   executive acts. The constraint is claimed as a Tangled Rope, reflecting
 *   its function in coordinating legal interpretation while simultaneously
 *   extracting significant power from other branches.
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
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy over Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '6989250d-c2a0-46f0-ad63-9c989c611b61').
narrative_ontology:cs_kernel_codification('6989250d-c2a0-46f0-ad63-9c989c611b61', fixed_text).
narrative_ontology:cs_authority_grounding('6989250d-c2a0-46f0-ad63-9c989c611b61', lineage).
narrative_ontology:cs_interpretation_layer_present('6989250d-c2a0-46f0-ad63-9c989c611b61').
narrative_ontology:cs_reading_relation('6989250d-c2a0-46f0-ad63-9c989c611b61', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('6989250d-c2a0-46f0-ad63-9c989c611b61', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('6989250d-c2a0-46f0-ad63-9c989c611b61', foundational, judicial_finality_in_constitutional_interpretation).
narrative_ontology:cs_axiom_status(judicial_finality_in_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('6989250d-c2a0-46f0-ad63-9c989c611b61', judicial_finality_in_constitutional_interpretation, deontological).
narrative_ontology:cs_axiom('6989250d-c2a0-46f0-ad63-9c989c611b61', secondary, constitutional_supremacy_over_ordinary_law).
narrative_ontology:cs_axiom_status(constitutional_supremacy_over_ordinary_law, holdable).
narrative_ontology:cs_axiom_grounding('6989250d-c2a0-46f0-ad63-9c989c611b61', constitutional_supremacy_over_ordinary_law, deontological).
narrative_ontology:cs_reference_frame('6989250d-c2a0-46f0-ad63-9c989c611b61', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('6989250d-c2a0-46f0-ad63-9c989c611b61', contemporary_political_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6989250d-c2a0-46f0-ad63-9c989c611b61', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, citizens).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, rule_of_law_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the final arbiter of constitutional questions, the judiciary holds ultimate interpretive authority, invalidating acts of other branches. This grants it significant influence over policy and institutional design, securing its interpretive monopoly.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% The legislative branch's acts are subject to judicial review and potential invalidation. This constrains its policy-making space and forces it to anticipate judicial interpretations, bearing the cost of limited sovereignty.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Executive actions and regulations can be struck down by the judiciary, limiting the executive's ability to implement its agenda. It must comply with judicial mandates, even when it disagrees with the interpretation.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive, payer,
    institutional, biographical, constrained, national).

% Citizens benefit from a stable, consistent constitutional framework and protection of rights, but also bear the costs of policies invalidated by unelected judges, potentially limiting democratic self-governance. Their ability to influence constitutional meaning is indirect and limited.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, citizens, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, citizens, beneficiary).

% Academically analyze and critique judicial interpretations, influencing legal discourse and public opinion. They do not directly participate in the enforcement or suffer its direct costs, but their work shapes the intellectual environment in which the constraint operates.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% Advocate for a system where all three branches have co-equal authority to interpret the Constitution within their own spheres. Their view is structurally excluded by the judicial supremacy reading, which asserts judicial finality.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, proponents_of_coordinate_construction, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, final, and authoritative interpretation of the constitutional text, resolving inter-branch disputes and ensuring legal consistency and stability across the political system.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority and the power to invalidate legislative and executive acts from the elected branches to the unelected judiciary, thereby limiting the policy space of the former.
% ABSENT_VOICES: Proponents of coordinate construction or legislative supremacy are structurally excluded from the final interpretive process; they would argue for a more distributed or democratically accountable system of constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, constitutional disputes would likely lead to inter-branch stalemates, conflicting interpretations, and a potential breakdown of legal order, requiring a fundamental reorganization of governmental authority.
% FOUNDING_PROBLEM: Preventing legislative tyranny and ensuring the supremacy of the written Constitution over transient political majorities, as articulated in foundational legal precedents.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and many legal scholars attest that the founding problem of constitutional stability and protection of rights remains live. Critics (some legislators, political scientists, and legal scholars) argue that while the original problem may have been addressed, judicial supremacy has created new problems of democratic deficit and judicial overreach; legislative hearings and academic critiques provide corroborating evidence.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.68) due to the judiciary's power to invalidate acts of elected branches, effectively dictating policy within constitutional bounds. Suppression is also high (0.75) because this reading actively forecloses alternative interpretive mechanisms and forces other branches to conform. Theater ratio is low (0.15) as this power is actively and frequently exercised, not merely performed. Accessibility collapse is high (0.85) because this reading severely limits alternative avenues for constitutional interpretation and amendment, making judicial interpretation the primary, often sole, path. Resistance is moderate (0.55) reflecting ongoing political and academic debate about the legitimacy and scope of judicial power.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this constraint is a necessary mechanism for upholding the rule of law and constitutional order. From the perspective of the legislature and executive, it represents a significant limitation on their democratic mandate and policy autonomy. Citizens experience both the benefits of rights protection and the costs of policies blocked by unelected judges.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the primary beneficiary (low d) due to its interpretive monopoly and veto power. The legislature and executive are targets (high d) as their actions are constrained and subject to invalidation. Citizens are complex, experiencing both benefits (constitutional stability, rights protection) and costs (limited democratic self-governance), placing them closer to the target end due to the counter-majoritarian aspect.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling judicial supremacy as a pure Rope (ignoring extraction) or a Snare (ignoring coordination). It acknowledges the genuine coordination function of providing a stable constitutional interpretation, while simultaneously highlighting the asymmetric extraction of power and policy space from other branches. The 'contested' status of the founding problem further suggests a potential for mandatrophy, where the original justification may no longer fully align with its current operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretive_ambiguity,
    'Is the constitutional text truly unambiguous in establishing judicial supremacy, or is this an interpretation that emerged from historical practice and judicial assertion?',
    'Historical and textual analysis of the constitutional founding, including debates and early interpretations, to assess the original intent regarding interpretive authority.',
    'If the text is ambiguous, the ''judicial supremacy'' reading is a constructed constraint rather than a natural law, strengthening arguments for alternative interpretive models and potentially reclassifying it as more extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretive_ambiguity, conceptual, 'Ambiguity in the constitutional text regarding the final arbiter of constitutional questions.').

omega_variable(
    counter_majoritarian_legitimacy,
    'To what extent is judicial supremacy legitimate in a democratic system, given its counter-majoritarian nature?',
    'Empirical studies on public trust in institutions, comparative constitutional analysis of systems with different interpretive models, and philosophical analysis of democratic theory.',
    'If legitimacy is found to be low or eroding, the constraint''s effective suppression and extraction are amplified, as it operates against the democratic will, potentially pushing it closer to a Snare from the citizen''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_legitimacy, preference, 'The tension between judicial finality and democratic accountability.').

omega_variable(
    judicial_overreach_boundary,
    'At what point does judicial interpretation cross the line into judicial legislation, and is this boundary consistently maintained?',
    'Analysis of judicial decisions over time, comparing them against established legal principles and legislative intent, and assessing the degree of policy-making involved.',
    'If judicial legislation is frequent and unconstrained, the extraction from the legislature and executive is higher than measured, as the judiciary is not merely interpreting but actively shaping policy without democratic accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_overreach_boundary, empirical, 'The distinction between judicial interpretation and judicial policy-making.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_authority_boundary' kernel. Its structural claims about judicial finality directly contrast with coordinate construction and parliamentary primacy readings, which posit different distributions of interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
