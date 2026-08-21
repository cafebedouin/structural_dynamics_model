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
 *   This constraint describes the 'judicial supremacy' reading of
 *   constitutional interpretive authority, where courts hold the final say on
 *   constitutional meaning, including the power to nullify legislative acts.
 *   This reading is distinct from 'parliamentary supremacy' (legislature is
 *   final arbiter) and 'coordinate construction' (inter-branch dialogue). The
 *   constraint is claimed as a Tangled Rope because it provides a
 *   coordination function (finality in constitutional disputes) but also
 *   involves asymmetric extraction (legislature and electorate are
 *   subordinated to judicial interpretation). The metrics reflect a system
 *   where judicial power has expanded over time, leading to increased
 *   extraction and suppression of alternative interpretive claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '74dac401-2243-4e67-a024-8cc75f826f1c').
narrative_ontology:cs_kernel_codification('74dac401-2243-4e67-a024-8cc75f826f1c', fixed_text).
narrative_ontology:cs_authority_grounding('74dac401-2243-4e67-a024-8cc75f826f1c', lineage).
narrative_ontology:cs_interpretation_layer_present('74dac401-2243-4e67-a024-8cc75f826f1c').
narrative_ontology:cs_reading_relation('74dac401-2243-4e67-a024-8cc75f826f1c', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('74dac401-2243-4e67-a024-8cc75f826f1c', constitutional_interpretive_authority__coordinate_construction_reading, influences).
narrative_ontology:cs_axiom('74dac401-2243-4e67-a024-8cc75f826f1c', foundational, judicial_review_is_inherent_to_constitutionalism).
narrative_ontology:cs_axiom_status(judicial_review_is_inherent_to_constitutionalism, holdable).
narrative_ontology:cs_axiom_grounding('74dac401-2243-4e67-a024-8cc75f826f1c', judicial_review_is_inherent_to_constitutionalism, deontological).
narrative_ontology:cs_axiom('74dac401-2243-4e67-a024-8cc75f826f1c', foundational, courts_are_best_suited_to_protect_minority_rights).
narrative_ontology:cs_axiom_status(courts_are_best_suited_to_protect_minority_rights, holdable).
narrative_ontology:cs_axiom_grounding('74dac401-2243-4e67-a024-8cc75f826f1c', courts_are_best_suited_to_protect_minority_rights, instrumental).
narrative_ontology:cs_reference_frame('74dac401-2243-4e67-a024-8cc75f826f1c', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('74dac401-2243-4e67-a024-8cc75f826f1c', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('74dac401-2243-4e67-a024-8cc75f826f1c', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_groups).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, electorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, rule_of_law_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and exercises final authority in interpreting the constitution, including the power to nullify legislative acts. Benefits from enhanced institutional prestige and control over legal outcomes. Its identity is fused with this interpretive role.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Submits its acts to judicial review and faces potential nullification. Bears the cost of having its democratic will overridden by unelected judges. Its options are to amend legislation, pursue constitutional amendment, or engage in political contestation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Experiences the democratic deficit when laws passed by its elected representatives are struck down by the judiciary. Benefits from the protection of fundamental rights, but pays the cost of reduced self-governance. Exit options are limited to electoral change or constitutional reform movements.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, electorate, payer,
    powerless, biographical, constrained, national).

% Benefit from the judiciary's role as a guardian of fundamental rights, often finding courts more receptive to their claims than legislatures. They leverage judicial review to advance their agendas and protect minority rights. Their influence is amplified by judicial supremacy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Must enforce laws as interpreted by the judiciary, even if it disagrees with the interpretation or if it conflicts with its policy agenda. Bears the cost of constrained executive action and potential political friction with the judiciary.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a final, authoritative arbiter for constitutional disputes, ensuring a consistent interpretation of fundamental law across different branches and levels of government, thereby stabilizing the legal framework.
% TRANSFER_FUNCTION: Transfers ultimate interpretive power over the constitution from the democratically elected legislature to the unelected judiciary, along with the associated political capital and policy influence.
% ABSENT_VOICES: Proponents of parliamentary supremacy or popular constitutionalism are structurally marginalized; they would argue for the primacy of legislative will or direct popular constitutional amendment, but their frameworks are subordinated by judicial supremacy.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the legislature would immediately assert its own interpretive authority, leading to a period of intense inter-branch conflict over constitutional meaning. The legal system would become highly unstable until a new interpretive hierarchy or a system of coordinate construction emerged.
% FOUNDING_PROBLEM: To prevent legislative overreach and protect fundamental rights from majoritarian tyranny, ensuring that the constitution remains the supreme law of the land.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and rights advocacy groups attest that the problem of rights protection remains live. The legislature and some political theorists argue that while rights protection is important, the current arrangement has led to judicial overreach and a democratic deficit, suggesting the founding problem is either solved or has been superseded by new problems of judicial power.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial because the judiciary, an unelected body, effectively overrides the democratic will of the legislature. Suppression (0.70) is high due to the institutional and legal barriers to challenging judicial interpretations, making alternatives difficult to pursue. The theater ratio (0.20) is moderate; while judicial review involves genuine legal reasoning, there's an element of performance in maintaining the 'neutral arbiter' facade when decisions have significant political impact. Accessibility collapse (0.60) is moderate, as while judicial decisions are binding, political and constitutional avenues for resistance (e.g., constitutional amendment, court packing debates) still exist, albeit constrained. Resistance (0.45) is also moderate, reflecting ongoing political and academic debates about the legitimacy and scope of judicial power.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary Rope for upholding the rule of law and protecting rights. From the legislature's and electorate's perspective, it can feel like a Snare, where their democratic choices are nullified by an unaccountable body. Rights advocacy groups see it as a Scaffold for justice. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a clear beneficiary (d=0.0-0.1) as it gains institutional power and prestige. Rights advocacy groups are also beneficiaries (d=0.1-0.2) as their goals are often advanced through judicial action. The legislature and electorate are targets (d=0.8-0.9) as their democratic will is subordinated. The executive branch is also a target (d=0.7-0.8) as its policy implementation is bound by judicial interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination function of legal finality). The rising extractiveness and suppression over time, coupled with the 'contested' status of the founding problem, suggest a drift towards a more extractive arrangement, even if the original mandate for rights protection remains partially valid. The system's persistence is due to the judiciary's institutional power and the perceived need for a final constitutional arbiter, despite the costs to democratic self-governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_rights_protection,
    'Is the democratic deficit created by judicial supremacy a necessary cost for robust rights protection, or does it undermine the very legitimacy of the constitutional order?',
    'Comparative constitutional studies examining systems with different interpretive authority models (e.g., parliamentary supremacy, strong constitutional councils) and their respective outcomes for rights protection and democratic accountability.',
    'If the deficit is deemed an unnecessary cost, it would strengthen arguments for rebalancing interpretive authority towards the legislature, potentially reclassifying the constraint as more extractive. If necessary, it reinforces the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_rights_protection, conceptual, 'The fundamental tension between democratic self-governance and judicial review.').

omega_variable(
    judicial_activism_vs_restraint,
    'To what extent do judicial interpretations reflect a neutral application of constitutional text versus the policy preferences of judges?',
    'Empirical analysis of judicial voting patterns, legal scholarship on interpretive methodologies, and historical case studies of landmark decisions.',
    'If judicial decisions are consistently found to align with judges'' policy preferences, it would increase the perceived extractiveness and theater ratio, suggesting the ''rule of law'' justification is partly a cover for political power. If genuinely neutral, it would reinforce the Rope aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_restraint, empirical, 'The degree to which judicial interpretation is objective or subjective.').

omega_variable(
    alternative_interpretive_models_viability,
    'Are viable alternative models of constitutional interpretation (e.g., coordinate construction, popular constitutionalism) genuinely suppressed, or are they simply less effective at achieving constitutional stability and rights protection?',
    'Analysis of historical attempts to implement alternative models, and theoretical work on their institutional design and practical challenges.',
    'If alternatives are genuinely suppressed and viable, it would increase the ''suppression'' metric and highlight the coercive aspects of judicial supremacy. If they are genuinely less effective, it would strengthen the ''coordination function'' aspect of the current model.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_interpretive_models_viability, conceptual, 'The viability and suppression of alternative constitutional interpretive models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1950, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(cons_tr_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cons_tr_t1990, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(cons_tr_t2010, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(cons_tr_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(cons_be_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(cons_be_t1990, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(cons_be_t2010, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(cons_be_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(cons_su_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(cons_su_t1990, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(cons_su_t2010, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(cons_su_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_process_constraint).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, fundamental_rights_protection_constraint).

% DUAL FORMULATION NOTE:
% This is one reading of the 'constitutional_interpretive_authority' kernel. Its structural properties and metrics differ significantly from the 'parliamentary_supremacy_reading' and 'coordinate_construction_reading' siblings, which are modeled as separate constraints due to their distinct ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
