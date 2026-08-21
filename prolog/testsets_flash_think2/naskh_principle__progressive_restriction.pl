% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Naskh Principle: Progressive Restriction Reading
 *   domain: islamic_jurisprudence/quranic_hermeneutics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'progressive restriction' reading of the
 *   Naskh principle in Islamic jurisprudence. It posits that Quranic
 *   revelation moved from more permissive to more restrictive rulings,
 *   representing a divine pedagogical process rather than outright abrogation
 *   (invalidation) of earlier verses. This reading provides a framework for
 *   legal interpretation, but in doing so, it restricts the interpretive
 *   options of those who might prefer to emphasize earlier, more permissive
 *   texts, leading to identifiable beneficiaries and victims. The constraint
 *   is claimed as a Rope by its proponents (a necessary coordination for
 *   legal coherence) but operates with significant extraction and
 *   suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.7).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.8).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.7).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Naskh Principle: Progressive Restriction Reading").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "islamic_jurisprudence/quranic_hermeneutics/legal_theory").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, 'ae26f5f1-dd6c-41a2-896a-fc16867e31b8').
narrative_ontology:cs_kernel_codification('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', fixed_text).
narrative_ontology:cs_authority_grounding('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', lineage).
narrative_ontology:cs_interpretation_layer_present('ae26f5f1-dd6c-41a2-896a-fc16867e31b8').
narrative_ontology:cs_reading_relation('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_axiom('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', foundational, divine_pedagogy_evolution).
narrative_ontology:cs_axiom_status(divine_pedagogy_evolution, holdable).
narrative_ontology:cs_axiom_grounding('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', divine_pedagogy_evolution, theological).
narrative_ontology:cs_axiom('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', foundational, later_revelation_supersedes_earlier_permissiveness).
narrative_ontology:cs_axiom_status(later_revelation_supersedes_earlier_permissiveness, holdable).
narrative_ontology:cs_axiom_grounding('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', later_revelation_supersedes_earlier_permissiveness, conventional).
narrative_ontology:cs_reference_frame('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', early_islamic_legal_development).
narrative_ontology:cs_drift_state('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', contemporary_islamic_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ae26f5f1-dd6c-41a2-896a-fc16867e31b8', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, scholarly_establishment).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, conservative_legal_schools).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, liberal_reformers).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, adherents_of_earlier_texts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The body of recognized Islamic scholars and institutions that formulate, teach, and enforce interpretive methodologies. They benefit from the coherence and authority this reading provides to legal rulings, maintaining their interpretive leadership.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, scholarly_establishment, agenda_setter,
    institutional, generational, constrained, global).

% Legal schools and movements whose interpretations align with or are strengthened by the progressive restriction reading. This framework validates their more restrictive legal positions and provides a clear methodology for navigating textual evolution.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, conservative_legal_schools, beneficiary,
    organized, generational, constrained, global).

% Scholars and activists who seek to derive more permissive or context-sensitive rulings from the Quran. This reading restricts their interpretive options, often invalidating their arguments by prioritizing later, more restrictive verses as the final divine intent.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, liberal_reformers, payer,
    organized, biographical, constrained, global).

% Individuals or groups who, based on their understanding, prefer to adhere to the more permissive rulings found in earlier Quranic verses. They find their practices or beliefs restricted by the dominant interpretive framework, facing social or legal pressure to conform.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, adherents_of_earlier_texts, payer,
    powerless, biographical, identity_locked, global).

% Scholars who adhere to the classical abrogation theory, where later verses explicitly invalidate earlier ones. While also restrictive, their methodology is distinct, and they are often excluded from the specific discourse of 'progressive restriction' as a separate interpretive school.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogation_scholars, excluded,
    institutional, generational, constrained, global).

% Scholars who argue that all Quranic verses remain valid within their specific revelatory contexts, resolving apparent contradictions through contextual specification. Their approach is fundamentally opposed to the progressive restriction reading, leading to their exclusion from its core interpretive framework.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_scholars, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and authoritative framework for understanding the evolution of divine law within the Quran, guiding believers and legal practitioners through a perceived pedagogical process of revelation.
% TRANSFER_FUNCTION: Transfers interpretive authority and legal validity from earlier, more permissive Quranic verses to later, more restrictive ones, effectively shifting legal weight from those who might prefer earlier texts to those who uphold the progressive restriction as final divine intent.
% ABSENT_VOICES: Scholars and practitioners advocating for the 'classical abrogation' or 'contextual harmonization' readings are structurally excluded from the core discourse of this specific interpretive framework, as their methodologies are either superseded or fundamentally opposed. They would argue for alternative ways to reconcile textual evolution.
% DISAPPEARANCE_RATIONALE: If the progressive restriction principle vanished overnight, Islamic legal theory would lose a major interpretive tool for reconciling textual evolution. This would necessitate entirely new hermeneutical approaches to apparent contradictions or shifts in Quranic verses, leading to significant re-evaluation of many established rulings and potentially a more permissive or context-dependent legal landscape.
% FOUNDING_PROBLEM: To reconcile apparent contradictions or shifts in legal rulings within the Quran, and to establish a clear, authoritative methodology for deriving law from a text revealed progressively over time, ensuring doctrinal coherence and practical guidance.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Islamic legal institutions and historical scholarly consensus attest to the ongoing need for a principle to manage textual evolution. While the specific *method* (progressive restriction vs. abrogation vs. harmonization) is contested, the underlying problem of reconciling Quranic verses revealed over time remains central to Islamic jurisprudence, as evidenced by centuries of scholarly debate and legal practice.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because this reading systematically prioritizes later, more restrictive verses, thereby limiting the scope of earlier, more permissive ones and imposing a specific legal outcome. Suppression (0.8) is also high, as this interpretive framework actively suppresses alternative readings (like contextual harmonization) that would yield different legal conclusions. The theater ratio (0.4) reflects a genuine intellectual effort to construct a coherent hermeneutic, but also a performative aspect in maintaining doctrinal unity and the authority of specific legal schools. The metrics show a slight increase over time, reflecting the hardening of interpretive stances and the increasing enforcement of this specific reading within certain legal traditions.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading (scholarly establishment, conservative legal schools) perceive it as a necessary and divinely guided coordination mechanism for legal coherence and moral development. From their seat, it is a Rope or Tangled Rope with a strong coordination function. However, from the perspective of liberal reformers and adherents of earlier texts, the same structure operates as a Snare or highly extractive Tangled Rope, systematically suppressing alternative interpretations and imposing restrictive legal outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   The scholarly establishment and conservative legal schools are clear beneficiaries (d near 0.0) as this reading validates their interpretive authority and legal positions. Liberal reformers and adherents of earlier texts are targets (d near 1.0) as their interpretive options are restricted, and their preferred readings are often invalidated. The constraint subsidizes the authority of the former by extracting interpretive freedom from the latter.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_pedagogy_vs_interpretive_choice,
    'Is the ''progressive restriction'' truly a reflection of divine pedagogy and intent, or is it an interpretive choice made by specific scholarly traditions to achieve certain legal or social outcomes?',
    'Comparative theological and historical analysis of early Islamic legal development, examining the socio-political contexts in which this principle gained prominence versus alternative readings.',
    'If primarily an interpretive choice, the constraint''s extractiveness and suppression would be re-evaluated as human-constructed rather than divinely mandated, potentially shifting its classification closer to a Snare. If genuinely divine pedagogy, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_pedagogy_vs_interpretive_choice, conceptual, 'Ambiguity between divine intent and human interpretation in the progressive restriction principle.').

omega_variable(
    impact_on_adherents_practical_lives,
    'What is the measurable impact of this progressive restriction reading on the practical lives and freedoms of adherents, particularly those who might otherwise follow more permissive interpretations?',
    'Sociological and anthropological studies examining the lived experiences of Muslim communities under legal systems influenced by this reading, comparing outcomes with communities following alternative hermeneutics.',
    'Quantifiable evidence of significant restrictions on personal autonomy or social practices would amplify the effective extraction (χ) for affected individuals, reinforcing a Snare-like classification for their seat. Lack of significant impact would dampen χ.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_adherents_practical_lives, empirical, 'Empirical impact of progressive restriction on individual freedoms and practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.3).
narrative_ontology:measurement(nask_tr_t350, naskh_principle__progressive_restriction, theater_ratio, 350, 0.35).
narrative_ontology:measurement(nask_tr_t700, naskh_principle__progressive_restriction, theater_ratio, 700, 0.38).
narrative_ontology:measurement(nask_tr_t1050, naskh_principle__progressive_restriction, theater_ratio, 1050, 0.39).
narrative_ontology:measurement(nask_tr_t1400, naskh_principle__progressive_restriction, theater_ratio, 1400, 0.4).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(nask_be_t350, naskh_principle__progressive_restriction, base_extractiveness, 350, 0.65).
narrative_ontology:measurement(nask_be_t700, naskh_principle__progressive_restriction, base_extractiveness, 700, 0.68).
narrative_ontology:measurement(nask_be_t1050, naskh_principle__progressive_restriction, base_extractiveness, 1050, 0.69).
narrative_ontology:measurement(nask_be_t1400, naskh_principle__progressive_restriction, base_extractiveness, 1400, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(nask_su_t350, naskh_principle__progressive_restriction, suppression_requirement, 350, 0.75).
narrative_ontology:measurement(nask_su_t700, naskh_principle__progressive_restriction, suppression_requirement, 700, 0.78).
narrative_ontology:measurement(nask_su_t1050, naskh_principle__progressive_restriction, suppression_requirement, 1050, 0.79).
narrative_ontology:measurement(nask_su_t1400, naskh_principle__progressive_restriction, suppression_requirement, 1400, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, islamic_legal_rulings).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, fatwa_issuance).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).

% DUAL FORMULATION NOTE:
% This constraint is the 'progressive_restriction' reading of the 'naskh_principle' kernel, which also includes 'classical_abrogation' and 'contextual_harmonization' as sibling readings. Each reading offers a distinct hermeneutical approach to reconciling the evolution of Quranic revelation, leading to different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
