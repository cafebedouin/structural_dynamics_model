% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Textual Authority
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of constitutional
 *   authority, where the meaning of the Constitution is fixed at the time of
 *   its ratification and derives from the historical public understanding of
 *   its text. This framework aims to constrain judicial discretion by
 *   requiring adherence to historical evidence, making it difficult to
 *   recognize unenumerated rights or adapt to post-ratification social
 *   changes without formal amendment. The constraint is claimed as a
 *   'tangled_rope' because it offers a coordination function (interpretive
 *   stability) but also involves significant extraction by limiting the
 *   interpretive flexibility of other actors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.65).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.7).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading of Constitutional Textual Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, '1a8f1717-5db6-4422-9348-0381f2be98fc').
narrative_ontology:cs_kernel_codification('1a8f1717-5db6-4422-9348-0381f2be98fc', fixed_text).
narrative_ontology:cs_authority_grounding('1a8f1717-5db6-4422-9348-0381f2be98fc', lineage).
narrative_ontology:cs_interpretation_layer_present('1a8f1717-5db6-4422-9348-0381f2be98fc').
narrative_ontology:cs_reading_relation('1a8f1717-5db6-4422-9348-0381f2be98fc', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a8f1717-5db6-4422-9348-0381f2be98fc', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('1a8f1717-5db6-4422-9348-0381f2be98fc', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('1a8f1717-5db6-4422-9348-0381f2be98fc', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('1a8f1717-5db6-4422-9348-0381f2be98fc', foundational, judicial_discretion_constrained_by_history).
narrative_ontology:cs_axiom_status(judicial_discretion_constrained_by_history, holdable).
narrative_ontology:cs_axiom_grounding('1a8f1717-5db6-4422-9348-0381f2be98fc', judicial_discretion_constrained_by_history, instrumental).
narrative_ontology:cs_reference_frame('1a8f1717-5db6-4422-9348-0381f2be98fc', original_public_meaning_framework).
narrative_ontology:cs_drift_state('1a8f1717-5db6-4422-9348-0381f2be98fc', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1a8f1717-5db6-4422-9348-0381f2be98fc', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_judicial_activists).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, advocates_for_unenumerated_rights).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, legislators_seeking_flexible_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively research and promote the originalist methodology, influencing judicial appointments and legal education. Their careers and professional identities are deeply tied to the persistence and dominance of this interpretive framework.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_legal_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from the originalist framework as it provides a seemingly objective basis for judicial decisions that align with their political and social preferences, limiting the scope for judicial innovation they oppose.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_judicial_activists, beneficiary,
    institutional, biographical, constrained, national).

% Bear the costs of a rigid interpretive framework that makes it difficult to recognize and protect rights not explicitly enumerated or understood at the time of ratification, requiring constant political struggle for legislative remedies.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, advocates_for_unenumerated_rights, payer,
    organized, generational, constrained, national).

% Find their legislative efforts constrained by a fixed constitutional meaning, requiring them to navigate complex and often politically unfeasible amendment processes to adapt the constitution to modern challenges.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, legislators_seeking_flexible_interpretation, payer,
    powerful, biographical, constrained, national).

% Are marginalized in legal discourse and judicial appointments when originalism dominates, despite offering alternative, coherent interpretive frameworks. Their arguments are often dismissed as 'judicial activism' by originalist proponents.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_scholars, excluded,
    institutional, generational, identity_locked, national).

% Experiences the outcomes of constitutional interpretation through policy and rights, often without a deep understanding of the underlying interpretive debates. Their ability to influence is primarily through elections and advocacy, which are mediated by the dominant legal frameworks.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, general_public, observer,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded method for interpreting the Constitution, aiming to limit judicial discretion and ensure fidelity to the original intent or public meaning of the text, thereby coordinating legal expectations.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judicial and legislative bodies to historical sources and original understandings, effectively limiting the scope of rights and governmental powers to those recognized at ratification. This transfers power from present-day majorities to past ones.
% ABSENT_VOICES: Living constitutionalist scholars and advocates for evolving rights are often excluded from the dominant discourse, their arguments framed as illegitimate departures from constitutional fidelity. They would argue for an interpretation that adapts to contemporary societal values and needs.
% DISAPPEARANCE_RATIONALE: If originalism as a dominant interpretive method vanished overnight, judicial decisions would immediately shift towards more flexible interpretations, potentially recognizing new rights or reinterpreting existing powers. The legal landscape would reorganize around a more dynamic understanding of constitutional meaning.
% FOUNDING_PROBLEM: The problem of judicial overreach and arbitrary interpretation, where judges might impose their own policy preferences rather than faithfully applying the law.
% FOUNDING_PROBLEM_CORROBORATION: Originalist proponents, conservative legal organizations, and some political commentators attest that the problem of judicial activism remains live. Critics, including living constitutionalists and some legal historians, argue that originalism itself can be a form of judicial activism, selectively applying history to achieve desired outcomes, making the status 'contested'.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading imposes significant costs on those seeking to adapt constitutional meaning to contemporary needs, effectively transferring power from present-day majorities to past ones. Suppression (0.70) is also high due to the active enforcement of historical methodologies and the marginalization of alternative interpretive approaches within legal institutions. The theater ratio (0.20) is relatively low, as the commitment to historical inquiry is genuine, though critics argue its application can be selective. The increasing extractiveness and suppression over time reflect the growing dominance and institutionalization of originalism in judicial appointments and legal discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist scholars and conservative judicial activists, this framework is a legitimate and necessary 'rope' for maintaining constitutional fidelity and preventing judicial activism. From the perspective of advocates for unenumerated rights and legislators seeking flexibility, it operates as a 'snare' or 'tangled_rope', extracting interpretive flexibility and imposing significant costs on social progress. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist legal scholars and conservative judicial activists are beneficiaries, as the framework legitimizes their interpretive approach and policy outcomes. Advocates for unenumerated rights and legislators seeking flexible interpretation are payers, bearing the costs of a rigid system. Living constitutionalist scholars are excluded, as their interpretive framework is actively suppressed within the dominant discourse. The general public is an observer, experiencing the effects without direct control over the interpretive method.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling originalism as pure coordination (a 'rope') by acknowledging its significant extractive and suppressive elements. It also avoids mislabeling it as pure extraction (a 'snare') by recognizing its genuine, albeit contested, coordination function in providing interpretive stability. The 'contested' status of the founding problem highlights the ongoing debate about whether originalism still serves its original mandate or has become a tool for other ends.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_public_meaning,
    'Does originalism derive constitutional meaning from the specific intentions of the framers (original intent) or the broader public understanding of the text at the time of ratification (original public meaning)?',
    'Analysis of judicial opinions and scholarly arguments to determine which methodology predominates in practice and which yields more consistent results.',
    'If original intent, the constraint is more rigid and potentially more extractive, as historical evidence is harder to ascertain and more easily manipulated. If original public meaning, it might allow for slightly more flexibility but still heavily relies on historical linguistic analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_public_meaning, conceptual, 'Ambiguity in the specific historical referent for originalist interpretation.').

omega_variable(
    originalism_as_judicial_activism,
    'Is the application of originalism a neutral, constraining methodology, or can it be selectively applied to achieve politically desired outcomes, thus functioning as a form of judicial activism?',
    'Empirical analysis of originalist judicial decisions, comparing historical evidence used with outcomes, and assessing consistency across cases with differing political implications.',
    'If originalism is found to be selectively applied, its ''suppression'' and ''extractiveness'' metrics would be re-evaluated upwards, and its ''theater_ratio'' might increase, as its stated purpose (neutrality) would diverge from its actual function (political outcome generation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_as_judicial_activism, empirical, 'Whether originalism is a neutral method or a tool for political ends.').

omega_variable(
    suppression_of_alternative_interpretations,
    'To what extent is the suppression of living constitutionalism and other interpretive methods a structural outcome of originalism''s dominance, versus a consequence of their own perceived weaknesses?',
    'Comparative analysis of legal education curricula, judicial appointment criteria, and funding for legal scholarship across different interpretive schools. If structural barriers are high, suppression is more a feature of originalism''s enforcement.',
    'If suppression is primarily structural, the ''suppression'' metric for originalism would be confirmed as high and actively maintained. If it''s due to perceived weaknesses, the ''resistance'' metric for alternative views might be lower, indicating less active contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_interpretations, empirical, 'Mechanism of suppression for alternative constitutional interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1970, constitutional_text_authority__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cons_tr_t1985, constitutional_text_authority__originalist_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text_authority__originalist_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cons_tr_t2010, constitutional_text_authority__originalist_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text_authority__originalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1970, constitutional_text_authority__originalist_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(cons_be_t1985, constitutional_text_authority__originalist_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(cons_be_t2000, constitutional_text_authority__originalist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(cons_be_t2010, constitutional_text_authority__originalist_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(cons_be_t2024, constitutional_text_authority__originalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1970, constitutional_text_authority__originalist_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(cons_su_t1985, constitutional_text_authority__originalist_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(cons_su_t2000, constitutional_text_authority__originalist_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(cons_su_t2010, constitutional_text_authority__originalist_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(cons_su_t2024, constitutional_text_authority__originalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This is one of three linked constraints forming the 'constitutional_text_authority' kernel. This originalist reading directly influences the legitimacy and operational space of the living constitutionalist and positivist readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
