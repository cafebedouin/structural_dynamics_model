% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Abrogation (Naskh) Principle in Quranic Hermeneutics
 *   domain: islamic_jurisprudence/theology/legal_theory
 *
 * SUMMARY:
 *   This constraint describes the classical principle of Naskh (abrogation)
 *   in Quranic hermeneutics, where later revealed verses are understood to
 *   supersede and nullify the legal force of earlier verses on the same
 *   topic. This reading provides legal certainty and a clear interpretive
 *   hierarchy, benefiting classical jurists and madhhab scholars. However, it
 *   comes at the cost of interpretive flexibility and can create theological
 *   tensions for those who seek to harmonize all verses. The constraint is
 *   claimed as a Rope by its proponents (a necessary coordination mechanism
 *   for legal coherence) but operates with significant extraction and
 *   suppression, making it a Tangled Rope from an analytical perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.65).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.75).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.65).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Abrogation (Naskh) Principle in Quranic Hermeneutics").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "islamic_jurisprudence/theology/legal_theory").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '98c110b7-3f4f-4b56-a577-8377292efbbf').
narrative_ontology:cs_kernel_codification('98c110b7-3f4f-4b56-a577-8377292efbbf', formalized).
narrative_ontology:cs_authority_grounding('98c110b7-3f4f-4b56-a577-8377292efbbf', lineage).
narrative_ontology:cs_interpretation_layer_present('98c110b7-3f4f-4b56-a577-8377292efbbf').
narrative_ontology:cs_reading_relation('98c110b7-3f4f-4b56-a577-8377292efbbf', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_reading_relation('98c110b7-3f4f-4b56-a577-8377292efbbf', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('98c110b7-3f4f-4b56-a577-8377292efbbf', foundational, chronological_supersession_is_divine_intent).
narrative_ontology:cs_axiom_status(chronological_supersession_is_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('98c110b7-3f4f-4b56-a577-8377292efbbf', chronological_supersession_is_divine_intent, theological).
narrative_ontology:cs_axiom('98c110b7-3f4f-4b56-a577-8377292efbbf', foundational, legal_clarity_requires_definitive_resolution_of_contradictions).
narrative_ontology:cs_axiom_status(legal_clarity_requires_definitive_resolution_of_contradictions, holdable).
narrative_ontology:cs_axiom_grounding('98c110b7-3f4f-4b56-a577-8377292efbbf', legal_clarity_requires_definitive_resolution_of_contradictions, conventional).
narrative_ontology:cs_reference_frame('98c110b7-3f4f-4b56-a577-8377292efbbf', classical_islamic_legal_theory).
narrative_ontology:cs_drift_state('98c110b7-3f4f-4b56-a577-8377292efbbf', contemporary_islamic_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('98c110b7-3f4f-4b56-a577-8377292efbbf', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, madhhab_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, lay_muslims_seeking_legal_clarity).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, modernist_reformers).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextual_interpreters).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, lay_muslims_seeking_theological_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars established and codified the principle of abrogation, using it to resolve apparent contradictions in the Quran and derive clear legal rulings. Their authority is partly constituted by their mastery of this complex interpretive method. They benefit from the legal certainty and interpretive hierarchy it provides.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars within established schools of Islamic law (madhhabs) rely on the classical abrogation principle to maintain consistency and coherence within their legal traditions. It provides a clear methodology for legal derivation and minimizes internal contradictions, reinforcing their institutional authority.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, madhhab_scholars, beneficiary,
    organized, generational, constrained, global).

% Many ordinary Muslims benefit from the clear, unambiguous legal rulings derived through the abrogation principle, which simplifies adherence to Islamic law. They rely on scholarly consensus and established interpretations for guidance.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_muslims_seeking_legal_clarity, beneficiary,
    moderate, biographical, constrained, global).

% These scholars challenge the classical abrogation principle, arguing it undermines the Quran's eternal relevance and leads to selective readings. They bear the cost of being marginalized or labeled as heterodox within mainstream Islamic discourse for rejecting a foundational hermeneutical tool.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, modernist_reformers, payer,
    organized, generational, constrained, global).

% Scholars who emphasize the historical and situational context of revelation find the abrogation principle overly rigid, as it strips earlier verses of their legal force. They struggle to reconcile their contextual approach with the established hierarchy of abrogated and abrogating verses, often facing resistance from traditional institutions.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contextual_interpreters, payer,
    moderate, biographical, constrained, global).

% Some Muslims find the idea of God abrogating His own words problematic from a theological perspective, as it can imply divine changeability or imperfection. They struggle with the theological implications of abrogation, but are often bound by community norms and traditional interpretations.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_muslims_seeking_theological_coherence, payer,
    powerless, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic methodology for resolving apparent contradictions within the Quran, ensuring legal consistency and clarity across diverse verses and historical contexts.
% TRANSFER_FUNCTION: Transfers interpretive authority and legal precedence from earlier Quranic verses to later ones on the same topic, effectively nullifying the legal force of the abrogated verses while preserving the authority of the abrogating ones. This transfers interpretive power to those who master the chronological and contextual details of revelation.
% ABSENT_VOICES: Early Islamic sects and individual scholars who rejected the principle of abrogation entirely, or proposed alternative methods of harmonization, were largely marginalized or their views suppressed in the formation of classical Islamic legal theory. Their arguments for the eternal validity of all verses are absent from the dominant discourse.
% DISAPPEARANCE_RATIONALE: If the classical abrogation principle vanished overnight, the entire edifice of Islamic jurisprudence would be thrown into disarray. Legal rulings on numerous topics (e.g., alcohol consumption, inheritance, warfare) would become ambiguous, leading to widespread interpretive chaos and a fundamental re-evaluation of the Quran's internal consistency. The authority of classical jurists and madhhabs would be severely undermined.
% FOUNDING_PROBLEM: The early Muslim community and subsequent jurists faced the challenge of reconciling seemingly contradictory verses in the Quran, particularly those revealed at different stages of the Prophet Muhammad's mission, to derive a coherent and consistent body of Islamic law.
% FOUNDING_PROBLEM_CORROBORATION: Classical and traditional scholars universally attest to the founding problem's live status, arguing that without abrogation, legal chaos would ensue. Modernist and contextual interpreters, while challenging the solution, generally acknowledge the existence of apparent contradictions that require hermeneutical resolution, thus corroborating the problem itself, if not the classical solution.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the interpretive power concentrated in the hands of those who determine the chronological order and scope of abrogation, effectively nullifying certain verses for legal application. Suppression (0.75) is high because alternative hermeneutical approaches (like contextual harmonization) are actively marginalized or rejected within traditional institutions. The theater ratio is low (0.1) as the principle is genuinely applied and forms a core part of legal reasoning, not merely a performance. Accessibility collapse is high (0.8) because once the principle is accepted, alternative interpretations of 'abrogated' verses become largely inaccessible for legal derivation. Resistance (0.4) is moderate, coming primarily from modernist and reformist movements rather than widespread popular rejection.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of classical jurists, the abrogation principle is a necessary and divinely sanctioned tool for maintaining the integrity and applicability of Islamic law (a Rope). From the perspective of modernist reformers, it is an extractive mechanism that stifles interpretive innovation and undermines the Quran's holistic message (a Snare). The engine's classification as Tangled Rope reflects the hybrid nature: a genuine coordination function (legal clarity) coupled with asymmetric extraction and active suppression of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical jurists and madhhab scholars are clear beneficiaries and agenda-setters, as the principle solidifies their interpretive authority and provides a framework for their legal systems. Lay Muslims seeking legal clarity also benefit from the unambiguous rulings. Modernist reformers and contextual interpreters are payers, as their alternative approaches are suppressed, and they bear the cost of challenging established norms. Lay Muslims seeking theological coherence are also payers, as they grapple with the theological implications of abrogation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_human_interpretation,
    'Is the principle of abrogation a direct reflection of divine will and wisdom, or primarily a human interpretive construct developed to manage textual complexity?',
    'Theological consensus shift over centuries, or a new, universally accepted hermeneutical framework that renders abrogation unnecessary for legal coherence.',
    'If primarily a human construct, its ''naturalness'' claim weakens, increasing its perceived extractiveness and suppression. If divine, its legitimacy is reinforced, and its coordination function is emphasized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_will_vs_human_interpretation, conceptual, 'Ambiguity regarding the divine vs. human origin of the abrogation principle.').

omega_variable(
    scope_of_abrogation_ambiguity,
    'What is the precise scope of abrogation: does it apply only to legal rulings, or can it extend to theological tenets and ethical principles?',
    'Further scholarly consensus or authoritative pronouncements clarifying the boundaries of abrogation''s application.',
    'A broader scope increases the principle''s power to nullify verses, potentially increasing extraction and suppression of alternative interpretations. A narrower scope limits its impact, reducing perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_abrogation_ambiguity, conceptual, 'Uncertainty about the extent of abrogation''s application within the Quran.').

omega_variable(
    theological_coherence_cost,
    'Is the legal clarity provided by abrogation worth the potential theological cost of implying divine changeability or imperfection?',
    'A shift in the dominant theological paradigm within Islam that re-prioritizes divine immutability over legal consistency derived through abrogation.',
    'If the theological cost is deemed too high, resistance to the principle would increase, and its legitimacy would be challenged, potentially leading to a reclassification towards Snare. If the legal clarity is paramount, its Rope-like qualities are emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_coherence_cost, preference, 'Trade-off between legal clarity and theological coherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nask_tr_t200, naskh_principle__classical_abrogation, theater_ratio, 200, 0.08).
narrative_ontology:measurement(nask_tr_t400, naskh_principle__classical_abrogation, theater_ratio, 400, 0.1).
narrative_ontology:measurement(nask_tr_t800, naskh_principle__classical_abrogation, theater_ratio, 800, 0.1).
narrative_ontology:measurement(nask_tr_t1200, naskh_principle__classical_abrogation, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(nask_tr_t1400, naskh_principle__classical_abrogation, theater_ratio, 1400, 0.1).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nask_be_t200, naskh_principle__classical_abrogation, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(nask_be_t400, naskh_principle__classical_abrogation, base_extractiveness, 400, 0.65).
narrative_ontology:measurement(nask_be_t800, naskh_principle__classical_abrogation, base_extractiveness, 800, 0.68).
narrative_ontology:measurement(nask_be_t1200, naskh_principle__classical_abrogation, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(nask_be_t1400, naskh_principle__classical_abrogation, base_extractiveness, 1400, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(nask_su_t200, naskh_principle__classical_abrogation, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(nask_su_t400, naskh_principle__classical_abrogation, suppression_requirement, 400, 0.75).
narrative_ontology:measurement(nask_su_t800, naskh_principle__classical_abrogation, suppression_requirement, 800, 0.78).
narrative_ontology:measurement(nask_su_t1200, naskh_principle__classical_abrogation, suppression_requirement, 1200, 0.75).
narrative_ontology:measurement(nask_su_t1400, naskh_principle__classical_abrogation, suppression_requirement, 1400, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(naskh_principle__classical_abrogation, 0.1).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, islamic_legal_rulings).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, quranic_exegesis_methodology).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'naskh_principle' kernel. Its ε value reflects the specific structural consequences of applying classical abrogation, distinct from other readings that emphasize harmonization or progressive restriction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
