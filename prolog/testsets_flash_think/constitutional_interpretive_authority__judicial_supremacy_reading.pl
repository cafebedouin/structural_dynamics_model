% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'judicial supremacy' reading of
 *   constitutional interpretive authority, where courts hold final power to
 *   interpret the constitution and nullify legislative acts. It is one
 *   reading of the broader 'constitutional_interpretive_authority' kernel.
 *   The constraint is claimed as a Tangled Rope, reflecting its dual function
 *   of coordinating constitutional adherence (a genuine benefit) while
 *   simultaneously extracting power from the legislative branch (an
 *   asymmetric cost). The metrics reflect a high degree of extraction and
 *   suppression, as judicial decisions actively override the will of elected
 *   representatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.8).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.75).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, 'c7f4c0b8-8faf-4a5b-937c-5103726ffef4').
narrative_ontology:cs_kernel_codification('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', fixed_text).
narrative_ontology:cs_authority_grounding('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', lineage).
narrative_ontology:cs_interpretation_layer_present('c7f4c0b8-8faf-4a5b-937c-5103726ffef4').
narrative_ontology:cs_reading_relation('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', foundational, judicial_review_is_final).
narrative_ontology:cs_axiom_status(judicial_review_is_final, holdable).
narrative_ontology:cs_axiom_grounding('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', judicial_review_is_final, deontological).
narrative_ontology:cs_axiom('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', foundational, constitution_is_supreme_law).
narrative_ontology:cs_axiom_status(constitution_is_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', constitution_is_supreme_law, conventional).
narrative_ontology:cs_reference_frame('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', marbury_v_madison_doctrine).
narrative_ontology:cs_drift_state('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', contemporary_era_of_judicial_activism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7f4c0b8-8faf-4a5b-937c-5103726ffef4', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocates).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, individual_citizens).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, majority_will).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, individual_citizens).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, rule_of_law_principle).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, individual_rights_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to interpret the constitution and nullify legislative acts, framing this power as essential for rights protection and constitutional adherence. Benefits from enhanced institutional power, legitimacy, and the ability to shape public policy through legal interpretation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Subject to judicial review and nullification of its acts. Bears the cost of diminished legislative sovereignty, the potential frustration of democratic will, and the need to draft laws with judicial interpretation in mind. Exit options are limited to constitutional amendment or political contestation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Responsible for enforcing laws, including those potentially nullified by the judiciary. Bears the cost of policy disruption and challenges to its implementation agenda. Must defer to judicial rulings, which can constrain executive action.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Benefit from the judiciary's role in protecting fundamental rights, often using judicial review as a mechanism to advance their causes when legislative avenues are blocked. Their influence is amplified by judicial supremacy, providing a powerful avenue for legal and social change.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocates, beneficiary,
    organized, biographical, mobile, national).

% Benefit from the protection of their constitutional rights against potential legislative or executive overreach. However, they also bear the cost of potentially undemocratic outcomes when judicial decisions override popular will, and may feel disempowered by unelected judges making policy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, individual_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, individual_citizens, payer).

% Represents the collective preferences expressed through democratic processes. Bears the cost of having its legislative enactments nullified by judicial review, leading to potential frustration of popular mandates and questions about democratic legitimacy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, majority_will, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_non_agent(constitutional_interpretive_authority__judicial_supremacy_reading, majority_will).

% Advocate for the ultimate authority of the elected legislature and reject judicial nullification of parliamentary acts. They are structurally excluded from the current framework of judicial supremacy, their arguments often dismissed as undermining constitutional order or the rule of law.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, parliamentary_supremacy_proponents, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures constitutional adherence and protects fundamental rights by providing a final arbiter for legal interpretation, preventing legislative or executive overreach and maintaining a stable legal framework.
% TRANSFER_FUNCTION: Transfers final authority over legal interpretation from elected representatives to unelected judges; transfers the power to nullify laws from the legislative process to the judicial branch, thereby shifting policy-making influence.
% ABSENT_VOICES: Proponents of parliamentary supremacy or popular sovereignty are structurally excluded from the final interpretive process. They would argue for the primacy of elected bodies and the democratic deficit of judicial nullification, but their views are marginalized within the established framework.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the legislative and executive branches would immediately gain unchecked power to interpret the constitution, potentially leading to rapid shifts in policy, erosion of minority rights, and a breakdown of constitutional limits as each branch asserts its own interpretation. The entire constitutional order would reorganize.
% FOUNDING_PROBLEM: The constraint was built to prevent tyranny of the majority, protect minority rights, and ensure that government acts within constitutional limits, particularly after historical experiences with legislative abuses or executive overreach.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and historical constitutional documents corroborate the problem of potential legislative overreach and the need for rights protection. However, proponents of legislative supremacy contest the *method* of judicial nullification as the appropriate solution, citing democratic deficits and arguing for alternative mechanisms of constitutional enforcement.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because the power of judicial nullification directly diminishes legislative sovereignty and can impose significant costs on policy implementation. Suppression is also high (0.75) as legislative alternatives to judicial rulings are severely constrained, requiring difficult processes like constitutional amendment or sustained political pressure. The theater ratio is moderate (0.2), indicating that while the judiciary's function is real, there can be performative aspects in framing policy decisions as purely legal interpretations. The increasing trend in extractiveness and suppression over the interval reflects a historical pattern of expanding judicial power and activism.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this arrangement is a necessary Rope or even a Mountain, ensuring the rule of law and protecting fundamental rights. From the legislature's perspective, it often functions as a Snare or Tangled Rope, where its democratic mandate is overridden by an unelected body. The engine's classification will capture this divergence based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a primary beneficiary (low d) as it gains institutional power and legitimacy from its final interpretive authority. Rights advocates also benefit (low d) as judicial review provides a powerful tool for their causes. The legislature and executive branch are targets (high d) as their acts are subject to nullification and their policy agendas can be disrupted. Individual citizens are complex: they benefit from rights protection (low d) but may also bear the cost of undemocratic outcomes (higher d). Majority will is a clear target (high d) as its expression through legislation is directly overridden.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_activism_vs_restraint,
    'Is the exercise of judicial supremacy a necessary safeguard of rights and constitutional principles, or an undemocratic usurpation of legislative power and policy-making?',
    'Empirical studies on the long-term impact of judicial review on democratic accountability, legislative quality, and public trust in institutions, alongside comparative constitutional analysis.',
    'If primarily an undemocratic usurpation, the effective extraction from the legislative seat is higher than currently measured, and the constraint leans more towards a Snare. If primarily a safeguard, the coordination function is stronger, supporting the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_restraint, conceptual, 'Ambiguity between judicial activism and legitimate constitutional guardianship.').

omega_variable(
    empirical_impact_on_legislative_quality,
    'Does the threat of judicial nullification lead to more carefully considered and rights-compliant legislation, or does it lead to legislative paralysis and a transfer of policy-making to the judiciary?',
    'Longitudinal studies comparing legislative processes and outcomes in jurisdictions with strong judicial review versus those with parliamentary supremacy, controlling for other political factors.',
    'If it improves legislative quality, the coordination function is stronger. If it leads to paralysis or judicial policy-making, the extractive component is more pronounced, and the suppression of legislative alternatives is more severe.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_impact_on_legislative_quality, empirical, 'The actual effect of judicial review on legislative behavior and quality.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''judicial_supremacy_reading'' of the ''constitutional_interpretive_authority'' kernel?',
    'Analysis of legal texts, historical jurisprudence, and political science literature to confirm the distinct structural claims of this reading versus its siblings (parliamentary_supremacy_reading, coordinate_construction_reading).',
    'If misidentified, the entire classification and network relationships would be invalid, requiring re-authoring under the correct kernel or as an independent constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific kernel reading being instantiated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(cons_su_t50, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_process_constraint).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, executive_policy_implementation_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_interpretive_authority' kernel. Its sibling readings, 'parliamentary_supremacy_reading' and 'coordinate_construction_reading', represent alternative structural arrangements for constitutional interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
