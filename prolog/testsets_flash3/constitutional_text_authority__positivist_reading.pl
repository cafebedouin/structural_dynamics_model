% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Text Authority (Positivist Reading)
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the positivist reading of constitutional
 *   authority, where validity is derived from formal enactment procedures and
 *   institutional sources, strictly separating law from morality. It is one
 *   reading of the broader 'constitutional_text_authority' kernel, alongside
 *   originalist and living constitutionalist interpretations. This reading
 *   emphasizes the procedural constraint on constitutional change and views
 *   moral arguments as irrelevant to legal validity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.15).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.25).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Text Authority (Positivist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '2bc0063c-5fea-4342-ae1b-df58746435d7').
narrative_ontology:cs_kernel_codification('2bc0063c-5fea-4342-ae1b-df58746435d7', fixed_text).
narrative_ontology:cs_authority_grounding('2bc0063c-5fea-4342-ae1b-df58746435d7', lineage).
narrative_ontology:cs_interpretation_layer_present('2bc0063c-5fea-4342-ae1b-df58746435d7').
narrative_ontology:cs_reading_relation('2bc0063c-5fea-4342-ae1b-df58746435d7', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bc0063c-5fea-4342-ae1b-df58746435d7', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_axiom('2bc0063c-5fea-4342-ae1b-df58746435d7', foundational, legal_validity_from_source_not_content).
narrative_ontology:cs_axiom_status(legal_validity_from_source_not_content, holdable).
narrative_ontology:cs_axiom_grounding('2bc0063c-5fea-4342-ae1b-df58746435d7', legal_validity_from_source_not_content, conventional).
narrative_ontology:cs_axiom('2bc0063c-5fea-4342-ae1b-df58746435d7', foundational, separation_of_law_and_morality).
narrative_ontology:cs_axiom_status(separation_of_law_and_morality, holdable).
narrative_ontology:cs_axiom_grounding('2bc0063c-5fea-4342-ae1b-df58746435d7', separation_of_law_and_morality, deontological).
narrative_ontology:cs_reference_frame('2bc0063c-5fea-4342-ae1b-df58746435d7', kelsenian_pure_theory_of_law).
narrative_ontology:cs_drift_state('2bc0063c-5fea-4342-ae1b-df58746435d7', contemporary_legal_realism_influence, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('2bc0063c-5fea-4342-ae1b-df58746435d7', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_profession).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, judicial_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, citizens).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, legislature).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, rule_of_law).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, separation_of_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the clarity and predictability of law derived from formal procedures, allowing for specialized interpretation and application. Relies on a stable, text-based legal framework for its practice.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_profession, beneficiary,
    institutional, generational, constrained, national).

% Applies and interprets the Constitution based on its formal enactment and institutional sources, maintaining a clear distinction between legal validity and moral content. Its authority is grounded in adherence to established procedures.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Operates within the procedural constraints of the Constitution, understanding that the validity of its enactments depends on formal processes rather than inherent moral 'goodness.' Must adhere to the text and established interpretive methods.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Benefit from the stability, predictability, and impartiality of a legal system where constitutional validity is determined by clear, formal criteria rather than subjective moral judgments. This provides a stable framework for rights and duties.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, citizens, beneficiary,
    organized, biographical, constrained, national).

% Their arguments about the moral content or justice of laws are explicitly excluded from determining the *legal validity* of constitutional provisions under this reading. They can critique, but not adjudicate, legal status.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_philosophers, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for legal interpretation and governance by grounding constitutional validity in formal procedures and institutional sources, separating law from subjective moral debates.
% TRANSFER_FUNCTION: Transfers interpretive authority from moral or philosophical arguments to formal legal processes and institutional actors, ensuring legal certainty and limiting judicial discretion to textual and procedural bounds.
% ABSENT_VOICES: Advocates for natural law or moral readings of the Constitution are structurally excluded from the determination of legal validity, as their arguments are deemed irrelevant to the formal criteria of positivism. They would argue for a more morally responsive legal system.
% DISAPPEARANCE_RATIONALE: If this positivist reading vanished, the basis for legal validity would become highly contested, potentially leading to a collapse of the law/morality distinction in constitutional adjudication. Legal certainty would erode, and the judicial system's role would fundamentally shift, leading to widespread legal and political instability.
% FOUNDING_PROBLEM: To establish a clear, objective basis for legal validity, independent of fluctuating moral or political opinions, ensuring stability and predictability in constitutional governance.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and practitioners outside of the immediate judicial system corroborate that the problem of maintaining legal certainty and the law/morality distinction remains live, especially in an era of increasing politicization of judicial appointments and constitutional interpretation.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The positivist reading is characterized by low extractiveness (0.15) and suppression (0.25) because its primary function is to provide a clear, stable framework for legal interpretation, which benefits the legal system and citizens through predictability. The 'suppression' here refers to the exclusion of non-formal arguments from legal validity, which is inherent to the positivist project, not coercive in a traditional sense. Accessibility collapse is high (0.8) because once the positivist framework is adopted, alternative modes of determining legal validity (e.g., moral arguments) are largely collapsed within the legal domain. Resistance is low (0.1) as this reading is a foundational approach within legal theory, though it faces conceptual challenges from other interpretive schools.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legal system, this reading provides essential clarity and stability. From the perspective of those advocating for morally-driven legal reform, it can appear rigid and unresponsive. The engine's classification will reflect the structural benefits of clarity and the 'cost' of excluding moral arguments from legal validity.
 *
 * DIRECTIONALITY LOGIC:
 *   The legal profession and judicial system are beneficiaries, as this reading provides a clear, manageable framework for their work. Citizens also benefit from the predictability. The legislature is a 'payer' in the sense that it must adhere to the formal procedural constraints. Moral philosophers are 'excluded' as their arguments are deemed outside the scope of legal validity, not because they are actively suppressed in a physical sense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    law_morality_distinction_stability,
    'How stable is the strict law/morality distinction in practice, given ongoing societal pressures for morally responsive law?',
    'Empirical analysis of judicial decisions and legislative debates over time, observing instances where moral arguments implicitly or explicitly influence legal outcomes despite formal positivist commitments.',
    'If the distinction proves unstable, the effective extractiveness of this reading (in terms of excluding moral considerations) might be lower than stated, or its theater ratio higher, as formal adherence masks practical compromises. This would push it towards a more ''tangled'' classification, reflecting the hybrid nature of actual legal practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(law_morality_distinction_stability, empirical, 'The practical persistence of the law/morality distinction.').

omega_variable(
    textualism_positivism_convergence,
    'To what extent is strict textualism, as practiced by some originalists, functionally indistinguishable from this positivist reading, and what are the implications for classification?',
    'Conceptual analysis comparing the interpretive methodologies and outcomes of strict textualist originalism with this positivist reading, focusing on their treatment of non-textual sources and moral arguments.',
    'If they are functionally indistinguishable, the ''originalist_reading'' (when strictly textualist) might converge in classification with this ''positivist_reading'', suggesting a shared underlying structural constraint despite different stated philosophical justifications. This would highlight the importance of actual interpretive practice over declared philosophical lineage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textualism_positivism_convergence, conceptual, 'Functional overlap between textualist originalism and legal positivism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__positivist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__positivist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__positivist_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__positivist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__positivist_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__positivist_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__positivist_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__positivist_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__positivist_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__positivist_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__positivist_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__positivist_reading, suppression_requirement, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
