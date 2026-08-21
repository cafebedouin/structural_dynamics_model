% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'parliamentary supremacy' reading of
 *   constitutional interpretive authority, where the elected legislature
 *   holds final interpretive power and judicial review of parliamentary acts
 *   is absent. It is one reading of the broader
 *   'constitutional_interpretive_authority' kernel, distinct from
 *   'judicial_supremacy_reading' and 'coordinate_construction_reading'. The
 *   legislature benefits from interpretive discretion, while the judiciary
 *   and minority groups bear the costs of limited checks on legislative
 *   power. The constraint is claimed as a Rope by its proponents, but its
 *   metrics reflect a Tangled Rope due to significant extraction and
 *   suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.6).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '90d9457f-dba1-44b9-9f69-1019d31b4757').
narrative_ontology:cs_kernel_codification('90d9457f-dba1-44b9-9f69-1019d31b4757', formalized).
narrative_ontology:cs_authority_grounding('90d9457f-dba1-44b9-9f69-1019d31b4757', lineage).
narrative_ontology:cs_interpretation_layer_present('90d9457f-dba1-44b9-9f69-1019d31b4757').
narrative_ontology:cs_reading_relation('90d9457f-dba1-44b9-9f69-1019d31b4757', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('90d9457f-dba1-44b9-9f69-1019d31b4757', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('90d9457f-dba1-44b9-9f69-1019d31b4757', foundational, electoral_mandate_supremacy).
narrative_ontology:cs_axiom_status(electoral_mandate_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('90d9457f-dba1-44b9-9f69-1019d31b4757', electoral_mandate_supremacy, conventional).
narrative_ontology:cs_axiom('90d9457f-dba1-44b9-9f69-1019d31b4757', foundational, judicial_deference_to_legislature).
narrative_ontology:cs_axiom_status(judicial_deference_to_legislature, holdable).
narrative_ontology:cs_axiom_grounding('90d9457f-dba1-44b9-9f69-1019d31b4757', judicial_deference_to_legislature, conventional).
narrative_ontology:cs_reference_frame('90d9457f-dba1-44b9-9f69-1019d31b4757', unfettered_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('90d9457f-dba1-44b9-9f69-1019d31b4757', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('90d9457f-dba1-44b9-9f69-1019d31b4757', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_party).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, minority_groups).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, individual_citizens).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_mandate_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the final word on constitutional meaning, allowing it to pass and enforce laws without judicial veto. Benefits from broad interpretive discretion, enabling policy implementation aligned with its electoral mandate. Exit options are limited by the constitutional framework itself.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefits from the legislature's interpretive power, as it can enact its agenda without fear of judicial nullification. Its power is tied to maintaining a legislative majority. Exit from this system would mean losing direct control over constitutional interpretation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_party, beneficiary,
    organized, biographical, constrained, national).

% Is denied the power of constitutional review over parliamentary acts, limiting its role to applying laws as passed by the legislature. Its institutional identity is tied to upholding the rule of law, even when it cannot challenge the constitutionality of statutes. Exit from this role would mean a fundamental redefinition of its function.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_branch, payer,
    institutional, generational, identity_locked, national).

% Are vulnerable to legislative majorities, as their rights and interests cannot be protected by judicial review against parliamentary acts. Their only recourse is political advocacy, which is often insufficient against a determined majority. They are trapped within the legislative framework.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, minority_groups, payer,
    powerless, generational, trapped, national).

% Bear the consequences of laws passed under parliamentary supremacy, without the safeguard of an independent judiciary to strike down unconstitutional legislation. Their ability to challenge state action is limited to statutory interpretation, not constitutional principle. Exit options are limited to political participation or emigration.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, individual_citizens, payer,
    moderate, biographical, constrained, national).

% Analyze the implications of parliamentary supremacy for democratic theory, human rights, and the rule of law. They provide critical commentary and comparative analysis, but do not directly participate in the exercise of interpretive authority.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the will of the democratically elected representatives, reflecting the popular mandate, is paramount in shaping the nation's laws and constitutional interpretation, avoiding gridlock or judicial overreach.
% TRANSFER_FUNCTION: Transfers ultimate interpretive discretion and policy-making power from the judicial branch to the legislative branch, and from minority protections to majority rule, legitimating coercion via electoral mandate.
% ABSENT_VOICES: Advocates for strong judicial review and fundamental rights protection are present in public discourse but lack institutional power to challenge parliamentary acts. They would argue for a more robust role for courts in safeguarding constitutional principles.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy in constitutional interpretation vanished overnight, the judicial branch would immediately assert powers of constitutional review, leading to challenges to existing legislation, a rebalancing of power among branches, and a fundamental shift in the constitutional order.
% FOUNDING_PROBLEM: To establish a clear locus of sovereign power in the elected representatives, ensuring democratic accountability and preventing unelected bodies from thwarting the popular will.
% FOUNDING_PROBLEM_CORROBORATION: The elected legislature and governing party consistently attest that the founding problem of democratic accountability and popular sovereignty remains live. Constitutional scholars and opposition parties, while acknowledging the historical context, often contest whether this specific arrangement remains the optimal solution for democratic governance and rights protection.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the power asymmetry where the legislature can interpret the constitution to its advantage, potentially at the expense of minority rights or individual liberties. Suppression (0.7) is high because the judicial branch is structurally prevented from voiding parliamentary acts, and there are few institutional avenues for challenging legislative constitutional interpretations. Theater ratio is low (0.1) as the system is functional in its stated aim of legislative dominance, with little performative maintenance. The increasing extractiveness and suppression over time reflect a gradual hardening of legislative power and a diminishing space for alternative interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the elected legislature, this is a legitimate Rope ensuring democratic accountability. From the perspective of the judicial branch or minority groups, it operates as a Snare or Tangled Rope, enabling majoritarian extraction and suppressing alternative constitutional interpretations. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected_legislature and governing_party are clear beneficiaries, gaining interpretive discretion and policy freedom (low d). The judicial_branch, minority_groups, and individual_citizens are targets, experiencing limitations on their ability to challenge legislative action (high d). The structural delta for this reading is that the legislature gains interpretive discretion, the judiciary is excluded from ultimate authority, and coercion is legitimated by electoral mandate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (democratic accountability) is still live, but its operation has accumulated extraction. The classification as Tangled Rope (despite being claimed as Rope) prevents mislabeling the arrangement as purely coordinative when it clearly involves asymmetric extraction and active enforcement to maintain legislative dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_rights_protection,
    'Is the democratic legitimacy conferred by parliamentary supremacy sufficient to justify the potential for majoritarian overreach and limited judicial protection of fundamental rights?',
    'Comparative constitutional analysis across systems with different interpretive authority models, assessing long-term outcomes for democratic stability, rights protection, and public trust. This is a preference-based question, but empirical data on outcomes can inform the preference.',
    'If democratic legitimacy is deemed paramount, the current arrangement is seen as a legitimate Rope. If rights protection is prioritized, the arrangement is reclassified as a Snare or Tangled Rope due to its extractive potential.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_rights_protection, preference, 'The fundamental trade-off between democratic majoritarianism and minority rights protection in constitutional interpretation.').

omega_variable(
    judicial_role_identity_lock,
    'To what extent is the judicial branch''s ''identity_locked'' exit option a result of genuine institutional commitment to parliamentary supremacy, versus a strategic adaptation to a structurally imposed limitation?',
    'Historical analysis of judicial pronouncements and internal debates, particularly during periods of constitutional reform or political crisis, to discern the underlying institutional self-conception. Counterfactual analysis of how the judiciary might behave if the structural constraint were removed.',
    'If it''s a genuine commitment, the ''identity_locked'' status is stable. If it''s strategic adaptation, the judiciary''s directionality might be closer to a ''constrained'' target, implying higher latent resistance and a greater potential for reassertion of power if the constraint weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_role_identity_lock, empirical, 'Distinguishing genuine institutional commitment from strategic adaptation in the judiciary''s acceptance of limited interpretive authority.').

omega_variable(
    parliamentary_sovereignty_naturalness,
    'Is parliamentary sovereignty a ''natural'' or inevitable feature of a democratic system, or a constructed legal and political choice?',
    'Comparative historical and legal analysis of different democratic constitutional models, particularly those that have evolved away from or never adopted strict parliamentary sovereignty. Examination of the philosophical underpinnings of sovereignty in different traditions.',
    'If ''natural'', the constraint leans towards a Mountain. If ''constructed'', it reinforces its classification as a human-made constraint (Tangled Rope), highlighting the agency involved in its maintenance and the possibility of alternative arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_sovereignty_naturalness, conceptual, 'Ambiguity regarding the ''naturalness'' of parliamentary sovereignty as a constitutional principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(cons_su_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_interpretive_authority' kernel. It represents the 'parliamentary_supremacy_reading', which emphasizes legislative finality in constitutional interpretation. It is linked to sibling readings that offer alternative allocations of interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
