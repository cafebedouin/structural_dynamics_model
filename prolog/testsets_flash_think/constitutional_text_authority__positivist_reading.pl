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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Validity from Formal Enactment (Positivist Reading)
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the positivist reading of constitutional
 *   validity, asserting that law derives its authority from formal enactment
 *   procedures and institutional sources, strictly separate from moral
 *   content. It is one reading of the 'constitutional_text_authority' kernel,
 *   contrasting with originalist and living constitutionalist
 *   interpretations. The positivist framework aims to provide a stable and
 *   predictable legal order, but in doing so, it can exclude moral arguments
 *   from the determination of legal validity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.45).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.78).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Validity from Formal Enactment (Positivist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '8bee4764-66ea-48be-bde5-bb0380b72214').
narrative_ontology:cs_kernel_codification('8bee4764-66ea-48be-bde5-bb0380b72214', fixed_text).
narrative_ontology:cs_authority_grounding('8bee4764-66ea-48be-bde5-bb0380b72214', lineage).
narrative_ontology:cs_interpretation_layer_present('8bee4764-66ea-48be-bde5-bb0380b72214').
narrative_ontology:cs_reading_relation('8bee4764-66ea-48be-bde5-bb0380b72214', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8bee4764-66ea-48be-bde5-bb0380b72214', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('8bee4764-66ea-48be-bde5-bb0380b72214', foundational, law_is_separate_from_morality).
narrative_ontology:cs_axiom_status(law_is_separate_from_morality, holdable).
narrative_ontology:cs_axiom_grounding('8bee4764-66ea-48be-bde5-bb0380b72214', law_is_separate_from_morality, deontological).
narrative_ontology:cs_axiom('8bee4764-66ea-48be-bde5-bb0380b72214', foundational, validity_from_procedure_not_content).
narrative_ontology:cs_axiom_status(validity_from_procedure_not_content, holdable).
narrative_ontology:cs_axiom_grounding('8bee4764-66ea-48be-bde5-bb0380b72214', validity_from_procedure_not_content, conventional).
narrative_ontology:cs_reference_frame('8bee4764-66ea-48be-bde5-bb0380b72214', hartian_rule_of_recognition).
narrative_ontology:cs_drift_state('8bee4764-66ea-48be-bde5-bb0380b72214', contemporary_legal_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8bee4764-66ea-48be-bde5-bb0380b72214', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_profession).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, state_institutions).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, citizens_seeking_moral_justice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets, applies, and benefits from the stability and predictability of a legal system grounded in formal procedures rather than subjective moral content. Their expertise is in the formal system.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_profession, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, legal_profession, beneficiary).

% Enforces laws and benefits from a clear, non-moralistic basis for legal validity, which enhances governmental authority and reduces challenges based on evolving moral claims. The state's power is tied to the formal system.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, state_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, state_institutions, beneficiary).

% Applies and interprets the constitutional text based on formal legal sources, avoiding moral judgments in determining validity. Their legitimacy rests on adherence to procedural correctness.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Enacts laws within the framework of the formally valid constitution, relying on clear procedural rules for constitutional amendment and statutory creation, rather than moral consensus.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislature, agenda_setter,
    institutional, biographical, constrained, national).

% Must obey laws whose validity is determined by formal procedures, even if those laws conflict with their deeply held moral convictions. Their moral arguments are deemed irrelevant to legal validity within this framework.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, citizens_seeking_moral_justice, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, citizens_seeking_moral_justice, excluded).

% Analyze the structure and function of the legal system from a positivist perspective, focusing on its internal coherence and procedural integrity, without necessarily endorsing its moral outcomes.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, analytical_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable, and non-arbitrary legal framework by grounding constitutional validity in formal enactment procedures and institutional sources, thereby separating law from subjective moral judgments.
% TRANSFER_FUNCTION: Transfers the ultimate authority for legal validity from diffuse, contested moral claims to concrete, verifiable institutional processes and textual sources, from citizens to state institutions.
% ABSENT_VOICES: Natural law theorists, moral philosophers, and citizens whose moral intuitions conflict with formally valid laws are structurally excluded from the determination of legal validity; they would argue for the integration of moral content into constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If the positivist understanding of constitutional validity vanished, the legal system would lose its stable foundation, potentially collapsing into moral anarchy or endless disputes over the 'true' moral content of law, leading to a fundamental reorganization of governance and legal practice.
% FOUNDING_PROBLEM: The need for a stable, predictable, and non-arbitrary legal order that is distinct from subjective or evolving moral judgments, to prevent legal chaos and ensure governmental authority.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political scientists attest to the historical and ongoing challenges of maintaining legal stability in the face of moral disagreement. International legal bodies often emphasize formal validity for state sovereignty. These sources, external to the direct beneficiaries, corroborate the persistence of the problem.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) is moderate, reflecting the costs imposed by a formal legal system that prioritizes procedural correctness over individual moral claims. Suppression (0.78) is high, as the state maintains a monopoly on legitimate force to enforce laws derived from this framework. Theater ratio (0.12) is low, indicating that the system is genuinely functional in its stated purpose of providing a formal legal order. Accessibility collapse (0.88) is high because there is no practical alternative to operating within the established legal system. Resistance (0.15) is low, as the positivist framework is largely accepted as the basis for legal practice, even if its philosophical underpinnings are debated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of legal positivists, this framework is a neutral and essential coordination mechanism for a functioning society, ensuring legal certainty. However, from the perspective of those seeking moral justice or advocating for natural law, the same structure can be seen as an extractive system that systematically marginalizes fundamental moral concerns in favor of institutional power and proceduralism. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The legal profession and state institutions are beneficiaries, gaining stability, predictability, and authority from the formal, non-moralistic grounding of law. Citizens seeking moral justice are targets, as their moral claims are systematically excluded from the determination of legal validity, forcing them to accept laws based purely on procedural correctness. The judiciary and legislature, as key institutional actors, also benefit from the clarity and defined scope of their roles within this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_reading_of_constitutional_text_authority,
    'Is this constraint a valid and complete representation of the positivist reading of constitutional text authority?',
    'Comparative analysis with canonical texts of legal positivism (e.g., Hart, Kelsen) and contemporary positivist scholarship.',
    'If the representation is incomplete or inaccurate, the classification of this reading and its relations to other readings may be skewed, affecting the overall kernel analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positivist_reading_of_constitutional_text_authority, conceptual, 'Verifies the fidelity of this story to the positivist legal theory.').

omega_variable(
    originalist_vs_positivist_grounding,
    'To what extent does the originalist reading, despite its textual focus, implicitly or explicitly rely on moral or natural law principles for its ultimate grounding, thereby diverging from the strict law/morality separation of positivism?',
    'Detailed textual analysis of originalist jurisprudence and scholarship, specifically examining arguments for the moral content or natural law foundations of the founding era''s public meaning.',
    'If originalism is found to rely on such principles, the ''coexists_with'' relation might shift towards ''influences'' or even ''forecloses'' in specific contexts, as the underlying philosophical commitments would be more divergent than a shared textualism suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_vs_positivist_grounding, conceptual, 'Examines the philosophical divergence between originalism and positivism despite shared textual focus.').

omega_variable(
    living_constitutionalist_vs_positivist_validity,
    'Is the positivist rejection of moral content in legal validity truly absolute, or do practical applications of law inevitably incorporate some moral reasoning, blurring the strict distinction with living constitutionalism?',
    'Empirical study of judicial decision-making in hard cases, and philosophical analysis of the ''open texture'' of law, to determine if judges, even those claiming positivist adherence, implicitly or explicitly engage in moral reasoning.',
    'If moral reasoning is found to be an unavoidable component of legal application, the ''forecloses'' relation with living constitutionalism might soften to ''coexists_with'' or ''influences'', suggesting a more complex, less absolute distinction in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(living_constitutionalist_vs_positivist_validity, empirical, 'Assesses the practical boundary between positivism and moral reasoning in legal application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1900, constitutional_text_authority__positivist_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(cons_tr_t1930, constitutional_text_authority__positivist_reading, theater_ratio, 1930, 0.11).
narrative_ontology:measurement(cons_tr_t1960, constitutional_text_authority__positivist_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(cons_tr_t1990, constitutional_text_authority__positivist_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text_authority__positivist_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(cons_be_t1900, constitutional_text_authority__positivist_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(cons_be_t1930, constitutional_text_authority__positivist_reading, base_extractiveness, 1930, 0.42).
narrative_ontology:measurement(cons_be_t1960, constitutional_text_authority__positivist_reading, base_extractiveness, 1960, 0.43).
narrative_ontology:measurement(cons_be_t1990, constitutional_text_authority__positivist_reading, base_extractiveness, 1990, 0.44).
narrative_ontology:measurement(cons_be_t2024, constitutional_text_authority__positivist_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1900, constitutional_text_authority__positivist_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(cons_su_t1930, constitutional_text_authority__positivist_reading, suppression_requirement, 1930, 0.76).
narrative_ontology:measurement(cons_su_t1960, constitutional_text_authority__positivist_reading, suppression_requirement, 1960, 0.77).
narrative_ontology:measurement(cons_su_t1990, constitutional_text_authority__positivist_reading, suppression_requirement, 1990, 0.77).
narrative_ontology:measurement(cons_su_t2024, constitutional_text_authority__positivist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, rule_of_law).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, judicial_review).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_amendment_process).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
