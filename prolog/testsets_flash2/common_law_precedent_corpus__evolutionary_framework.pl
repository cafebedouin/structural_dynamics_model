% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent (Evolutionary Framework Reading)
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the common law system of precedent as an
 *   'evolutionary framework,' where judicial decisions adapt to contemporary
 *   normative evolution, permitting reinterpretation and occasional
 *   overruling of past decisions. This reading emphasizes the judiciary's
 *   role in updating the law to maintain its relevance and justice. It is one
 *   reading of the broader 'common_law_precedent_corpus' kernel, distinct
 *   from 'strict_stare_decisis' and 'pluralist_balancing' readings.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda-setter (institutional/constrained) — interprets and adapts law.
 *   - litigants_challenging_outdated_norms: Primary beneficiary (moderate/mobile) — gains pathways for legal change.
 *   - litigants_relying_on_stable_precedent: Primary payer (moderate/constrained) — bears costs of legal uncertainty.
 *   - legal_scholars: Observer (analytical/analytical) — analyzes and critiques legal evolution.
 *   - legislature: Excluded (institutional/constrained) — influenced by, but not directly part of, judicial reinterpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.35).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.2).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.35).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent (Evolutionary Framework Reading)").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, '1423713c-3732-4bda-b671-7d4466e6aa1b').
narrative_ontology:cs_kernel_codification('1423713c-3732-4bda-b671-7d4466e6aa1b', formalized).
narrative_ontology:cs_authority_grounding('1423713c-3732-4bda-b671-7d4466e6aa1b', lineage).
narrative_ontology:cs_interpretation_layer_present('1423713c-3732-4bda-b671-7d4466e6aa1b').
narrative_ontology:cs_reading_relation('1423713c-3732-4bda-b671-7d4466e6aa1b', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('1423713c-3732-4bda-b671-7d4466e6aa1b', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('1423713c-3732-4bda-b671-7d4466e6aa1b', foundational, law_as_adaptive_system).
narrative_ontology:cs_axiom_status(law_as_adaptive_system, holdable).
narrative_ontology:cs_axiom_grounding('1423713c-3732-4bda-b671-7d4466e6aa1b', law_as_adaptive_system, deontological).
narrative_ontology:cs_axiom('1423713c-3732-4bda-b671-7d4466e6aa1b', foundational, judicial_role_as_normative_updater).
narrative_ontology:cs_axiom_status(judicial_role_as_normative_updater, holdable).
narrative_ontology:cs_axiom_grounding('1423713c-3732-4bda-b671-7d4466e6aa1b', judicial_role_as_normative_updater, conventional).
narrative_ontology:cs_reference_frame('1423713c-3732-4bda-b671-7d4466e6aa1b', dynamic_common_law_tradition).
narrative_ontology:cs_drift_state('1423713c-3732-4bda-b671-7d4466e6aa1b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1423713c-3732-4bda-b671-7d4466e6aa1b', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, litigants_challenging_outdated_norms).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, litigants_relying_on_stable_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies precedent, with a recognized power to reinterpret or overrule past decisions to align with evolving societal norms. Benefits from the flexibility to adapt law, but bears the cost of maintaining legitimacy amidst change.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the framework's openness to re-examining and overturning precedents that no longer serve contemporary justice or societal values. They have a pathway to challenge established legal principles.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_challenging_outdated_norms, beneficiary,
    moderate, biographical, mobile, local).

% Bear the cost of legal uncertainty when established precedents are subject to reinterpretation or overruling. Their reliance on settled law for planning and dispute resolution is diminished.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_relying_on_stable_precedent, payer,
    moderate, biographical, constrained, local).

% Analyze the evolution of precedent, critique judicial reasoning, and propose frameworks for legal change. They benefit from the dynamic nature of the law as a subject of study and influence.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_scholars, observer,
    analytical, generational, analytical, global).

% While capable of enacting new laws, the legislature is often reactive to judicial interpretations and may find its role in shaping law partially preempted or influenced by the judiciary's adaptive use of precedent. They are excluded from the direct interpretive process.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legislature, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for legal decision-making that balances continuity with the capacity for adaptation, allowing the law to evolve in response to changing societal values and circumstances while maintaining a degree of predictability.
% TRANSFER_FUNCTION: Transfers the authority to update legal norms from a purely legislative process to a shared judicial-interpretive process, shifting the burden of legal change and its associated costs and benefits among different actors.
% ABSENT_VOICES: Advocates for strict adherence to original intent or historical precedent, who would argue that judicial reinterpretation usurps legislative authority and undermines legal stability, are often marginalized in this framework.
% DISAPPEARANCE_RATIONALE: If this evolutionary framework for precedent vanished, the legal system would either ossify under strict stare decisis or descend into unpredictable ad hoc decision-making, fundamentally altering how law is made, applied, and challenged.
% FOUNDING_PROBLEM: The problem of maintaining legal stability and predictability while ensuring the law remains relevant and just in the face of evolving societal norms and unforeseen circumstances.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and contemporary jurisprudential scholars widely corroborate that this tension is an inherent and ongoing challenge in common law systems, requiring continuous adaptation. The judiciary's own opinions frequently articulate this balance.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because while the system provides flexibility, it imposes costs of uncertainty on those relying on settled law. Suppression is low (0.20) as the system is designed to allow for challenges and reinterpretation, not to rigidly enforce past decisions. Theater ratio is low (0.10) as the adaptive function is genuine, not merely performative. The historical measurements show a slight increase in extractiveness and suppression over time, reflecting the growing complexity and contestation around legal evolution.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and litigants challenging norms experience this framework as a beneficial, adaptive mechanism. Litigants relying on stable precedent, however, experience it as a source of uncertainty and potential cost. The engine will compute these divergent classifications based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary, as the primary interpreter and adapter, is a beneficiary (low d). Litigants challenging outdated norms are also beneficiaries, as the system provides them a mechanism for change. Litigants relying on stable precedent are payers (higher d) due to the uncertainty introduced. The legislature is excluded from the direct interpretive process, making it a target of the judiciary's expanded role.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging the genuine coordination function of legal adaptation while also recognizing the costs of uncertainty. It avoids framing all change as 'extraction' by highlighting the beneficiaries of legal evolution, distinguishing it from a pure Snare. The 'live' status of the founding problem (balancing stability and adaptation) confirms the mandate is still relevant, preventing a Piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_judicial_reinterpretation,
    'What are the ultimate limits of judicial reinterpretation before it is perceived as judicial overreach or usurpation of legislative power?',
    'Empirical studies of public and legislative reactions to landmark overruling decisions, and analysis of the frequency and scope of legislative responses to judicial reinterpretation.',
    'If reinterpretation is frequently perceived as overreach, the constraint''s legitimacy (and thus its effective suppression) could erode, potentially leading to legislative pushback or a shift towards a ''strict_stare_decisis'' or ''pluralist_balancing'' reading by other actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_reinterpretation, conceptual, 'The boundary between legitimate judicial evolution and perceived overreach.').

omega_variable(
    predictability_vs_adaptability_tradeoff,
    'At what point does the benefit of legal adaptability outweigh the cost of reduced predictability for citizens and businesses?',
    'Economic analysis of transaction costs and investment impacts under varying degrees of legal stability, combined with sociological studies of public trust in the legal system.',
    'If the costs of unpredictability become too high, the system may face pressure to shift towards a more ''strict_stare_decisis'' or ''pluralist_balancing'' approach, or face increased resistance from economic actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predictability_vs_adaptability_tradeoff, empirical, 'The optimal balance point between legal predictability and adaptability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1800, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(comm_tr_t1850, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(comm_tr_t1900, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(comm_tr_t1950, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(comm_tr_t2000, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(comm_tr_t2024, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1800, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(comm_be_t1850, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1850, 0.25).
narrative_ontology:measurement(comm_be_t1900, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(comm_be_t1950, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1950, 0.33).
narrative_ontology:measurement(comm_be_t2000, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(comm_be_t2024, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1800, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement(comm_su_t1850, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1850, 0.17).
narrative_ontology:measurement(comm_su_t1900, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(comm_su_t1950, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1950, 0.19).
narrative_ontology:measurement(comm_su_t2000, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(comm_su_t2024, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, constitutional_interpretation_doctrine).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, legislative_process_efficiency).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_law_precedent_corpus' kernel. The other readings are 'strict_stare_decisis' and 'pluralist_balancing', each representing a distinct structural claim about the nature of precedent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
