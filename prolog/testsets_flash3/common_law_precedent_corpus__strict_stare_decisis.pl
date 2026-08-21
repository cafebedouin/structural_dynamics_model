% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis (Common Law Precedent)
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'strict stare decisis' reading of common
 *   law precedent, where past judicial decisions are considered highly
 *   binding and require extraordinary justification for departure. This
 *   reading emphasizes stability and predictability over adaptability. It is
 *   one of several competing interpretations of how precedent should function
 *   within a common law system. The metrics reflect the high cost borne by
 *   those seeking to challenge established legal norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.65).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.75).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis (Common Law Precedent)").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc').
narrative_ontology:cs_kernel_codification('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', formalized).
narrative_ontology:cs_authority_grounding('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', lineage).
narrative_ontology:cs_interpretation_layer_present('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc').
narrative_ontology:cs_reading_relation('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', common_law_precedent_corpus__evolutionary_framework, influences).
narrative_ontology:cs_reading_relation('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', foundational, judicial_predictability_supremacy).
narrative_ontology:cs_axiom_status(judicial_predictability_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', judicial_predictability_supremacy, deontological).
narrative_ontology:cs_axiom('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', foundational, judicial_restraint_principle).
narrative_ontology:cs_axiom_status(judicial_restraint_principle, holdable).
narrative_ontology:cs_axiom_grounding('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', judicial_restraint_principle, conventional).
narrative_ontology:cs_reference_frame('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', classical_common_law_stability).
narrative_ontology:cs_drift_state('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', contemporary_legal_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('aa26a4a5-6a4c-4459-a1ea-986c6bd5e4dc', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, judicial_conservatives).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_system_stability).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_challenging_precedent).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, social_reform_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and legal scholars who uphold strict adherence to precedent, viewing it as essential for judicial legitimacy, predictability, and limiting judicial activism. They benefit from the stability and the power to shape future law through past decisions.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, judicial_conservatives, agenda_setter,
    institutional, generational, constrained, national).

% Parties in legal disputes whose cases are directly harmed by existing precedent. They face high barriers to overturning established law, requiring extraordinary justification and often significant financial and temporal costs.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_challenging_precedent, payer,
    powerless, immediate, trapped, local).

% Groups and individuals seeking legal changes to align with evolving social norms or scientific understanding. They find their efforts constrained by rigid adherence to past rulings, forcing them into legislative or constitutional amendment pathways.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, social_reform_advocates, payer,
    organized, generational, constrained, national).

% The abstract principle of legal predictability and consistency, which is enhanced by strict adherence to precedent. While not an agent, it represents a value that benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_system_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(common_law_precedent_corpus__strict_stare_decisis, legal_system_stability).

% Judges and legal scholars who advocate for a more flexible interpretation of precedent, allowing for adaptation to contemporary societal needs and values. Their views are often marginalized or require significant political capital to implement under a strict stare decisis regime.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, judicial_progressives, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures predictability and consistency in legal rulings, allowing individuals and institutions to plan their actions with reasonable assurance of legal outcomes. It coordinates judicial decision-making across time and different courts.
% TRANSFER_FUNCTION: Transfers the burden of legal innovation and adaptation from the judiciary to legislative bodies or constitutional amendment processes. It also transfers the cost of challenging established norms to individual litigants and advocacy groups.
% ABSENT_VOICES: Judicial progressives and those advocating for a more adaptive legal system are often marginalized in strict stare decisis frameworks. They would argue for greater judicial flexibility to address evolving societal needs, but their arguments are often dismissed as 'judicial activism'.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished overnight, legal predictability would collapse, leading to widespread uncertainty in contracts, property rights, and criminal law. Every case could become a re-litigation of foundational principles, forcing a rapid re-establishment of some form of binding precedent.
% FOUNDING_PROBLEM: The need for stability, predictability, and fairness in legal systems, preventing arbitrary judicial decisions and ensuring that like cases are treated alike.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars across the ideological spectrum, as well as practicing lawyers and businesses, corroborate the ongoing need for legal stability. While the degree of rigidity is contested, the underlying problem of arbitrary justice remains live.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the strict application of precedent often forces litigants to bear significant costs to challenge or adapt existing law, even when social or economic conditions have changed. Suppression is also high (0.75) as the legal system actively resists departures from precedent, making it difficult for new interpretations to gain traction. Theater ratio is low (0.20) because the commitment to stability is genuine, though it may serve to entrench certain interests. The increasing trend in extractiveness and suppression over time reflects a hardening of this interpretation in response to pressures for legal reform.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of judicial conservatives, strict stare decisis is a necessary 'rope' for maintaining legal order and legitimacy. From the perspective of litigants and advocates for change, it operates as a 'snare' or 'tangled rope,' extracting costs and suppressing adaptation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial conservatives and the abstract concept of legal system stability are beneficiaries, as strict stare decisis aligns with their goals of predictability and limited judicial activism. Litigants challenging precedent and social reform advocates are victims, bearing the direct costs and facing significant barriers to legal change. Judicial progressives are excluded, as their arguments for flexibility are often dismissed within this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_justification_ambiguity,
    'What constitutes ''extraordinary justification'' for departing from precedent, and is this standard applied consistently or selectively?',
    'Empirical analysis of judicial opinions over time, coding for the types of justifications accepted for overruling precedent and identifying patterns of application across different legal domains or political contexts.',
    'If the standard is inconsistently applied or serves to protect specific interests, the constraint''s effective extractiveness and suppression are higher than measured, indicating a more ''snare-like'' operation. If consistently applied, it supports the ''tangled rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_justification_ambiguity, empirical, 'Ambiguity in the standard for departing from precedent.').

omega_variable(
    judicial_legitimacy_tradeoff,
    'Is strict adherence to precedent genuinely necessary for maintaining judicial legitimacy, or does excessive rigidity erode public trust by failing to adapt to societal change?',
    'Sociological studies of public perception of judicial decisions, comparing jurisdictions with different approaches to stare decisis, and analyzing the long-term impact on public confidence in the judiciary.',
    'If rigidity erodes legitimacy, the ''beneficiary'' status of legal system stability is undermined, and the constraint''s overall coordination function is weaker than claimed, pushing it towards a ''snare'' classification. If it upholds legitimacy, the ''tangled rope'' classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_legitimacy_tradeoff, conceptual, 'Tradeoff between judicial rigidity and public legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1900, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(comm_tr_t1930, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(comm_tr_t1960, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(comm_tr_t1990, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(comm_tr_t2010, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(comm_tr_t2024, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1900, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(comm_be_t1930, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1930, 0.55).
narrative_ontology:measurement(comm_be_t1960, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(comm_be_t1990, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(comm_be_t2010, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(comm_be_t2024, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1900, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(comm_su_t1930, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1930, 0.65).
narrative_ontology:measurement(comm_su_t1960, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(comm_su_t1990, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(comm_su_t2010, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(comm_su_t2024, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_law_precedent_corpus kernel. Its strict adherence to precedent creates structural pressure on more flexible readings by limiting their scope and legitimacy within the legal system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
