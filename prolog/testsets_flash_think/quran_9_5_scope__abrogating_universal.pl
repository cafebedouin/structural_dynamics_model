% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Quran 9:5 Abrogating Universal Jihad Doctrine
 *   domain: Islamic Jurisprudence / Hermeneutics / Political Theology
 *
 * SUMMARY:
 *   This constraint represents a specific, highly contested reading of Quran
 *   9:5, often referred to as the 'Verse of the Sword' (Ayat al-Sayf), which
 *   asserts that it abrogates all prior peaceful verses and establishes
 *   universal offensive jihad as a perpetual legal obligation until
 *   polytheists submit or convert. This reading is foundational for certain
 *   expansionist and jihadist movements. It is presented as a Snare due to
 *   its high extractiveness and suppression, targeting non-Muslims and
 *   suppressing dissenting interpretations within Islam. The claimed type
 *   'snare' reflects the structural reality of this interpretation's
 *   application, not a judgment on the Quran itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.9).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.95).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Quran 9:5 Abrogating Universal Jihad Doctrine").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "Islamic Jurisprudence / Hermeneutics / Political Theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, 'eda7029d-9fc8-4204-a684-a3ff874e14c4').
narrative_ontology:cs_kernel_codification('eda7029d-9fc8-4204-a684-a3ff874e14c4', fixed_text).
narrative_ontology:cs_authority_grounding('eda7029d-9fc8-4204-a684-a3ff874e14c4', lineage).
narrative_ontology:cs_interpretation_layer_present('eda7029d-9fc8-4204-a684-a3ff874e14c4').
narrative_ontology:cs_reading_relation('eda7029d-9fc8-4204-a684-a3ff874e14c4', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('eda7029d-9fc8-4204-a684-a3ff874e14c4', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('eda7029d-9fc8-4204-a684-a3ff874e14c4', foundational, verse_9_5_abrogates_peaceful_verses).
narrative_ontology:cs_axiom_status(verse_9_5_abrogates_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('eda7029d-9fc8-4204-a684-a3ff874e14c4', verse_9_5_abrogates_peaceful_verses, conventional).
narrative_ontology:cs_axiom('eda7029d-9fc8-4204-a684-a3ff874e14c4', foundational, universal_offensive_jihad_is_standing_obligation).
narrative_ontology:cs_axiom_status(universal_offensive_jihad_is_standing_obligation, holdable).
narrative_ontology:cs_axiom_grounding('eda7029d-9fc8-4204-a684-a3ff874e14c4', universal_offensive_jihad_is_standing_obligation, deontological).
narrative_ontology:cs_reference_frame('eda7029d-9fc8-4204-a684-a3ff874e14c4', early_islamic_legal_tradition).
narrative_ontology:cs_drift_state('eda7029d-9fc8-4204-a684-a3ff874e14c4', contemporary_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('eda7029d-9fc8-4204-a684-a3ff874e14c4', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, adherents_of_this_reading).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslims_polytheists).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, dissenting_muslim_scholars).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, coexistence_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These movements interpret Quran 9:5 as a divine mandate for universal offensive jihad, justifying military expansion and the subjugation or conversion of non-Muslims. They actively enforce this doctrine and benefit from its political and material gains, seeing their identity as fused with this mission.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements, beneficiary).

% Individuals and groups who believe in and propagate this interpretation. They benefit from a clear, divinely sanctioned mission, a sense of religious superiority, and potential material gains from expansion. Their identity is deeply tied to this theological framework.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, adherents_of_this_reading, beneficiary,
    organized, biographical, identity_locked, global).

% The primary targets of this doctrine, facing demands for submission, conversion, or subjugation. They bear the direct costs of violence, loss of sovereignty, and forced religious change. Their voices are entirely excluded from the interpretive framework.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslims_polytheists, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, non_muslims_polytheists, excluded).

% Scholars who offer alternative, contextual, or defensive interpretations of Quran 9:5. They face intellectual suppression, accusations of heresy, and social ostracization from adherents of the abrogating universal reading. Their careers and safety can be at risk.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, dissenting_muslim_scholars, payer,
    moderate, biographical, constrained, global).

% Individuals and groups, both Muslim and non-Muslim, who promote interfaith dialogue, peaceful coexistence, and pluralism. This reading actively suppresses their narratives and delegitimizes their efforts, often labeling them as apostates or enemies of the faith.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, coexistence_advocates, excluded,
    powerless, generational, constrained, global).

% Academics, historians, and political scientists who study Islamic hermeneutics and political theology. They analyze the historical development and contemporary impact of this doctrine without being subject to its direct enforcement or benefits.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies adherents under a clear, divinely sanctioned, expansionist mission, providing a coherent legal and theological framework for military action and political subjugation of non-Muslims.
% TRANSFER_FUNCTION: Transfers sovereignty, resources, and religious allegiance from non-Muslim populations and territories to the expansionist movements and adherents of this reading, justified as a divine right.
% ABSENT_VOICES: Non-Muslims, polytheists, and moderate Muslim scholars advocating for contextual or peaceful interpretations are actively suppressed or excluded from the interpretive discourse. Their perspectives are deemed irrelevant or heretical.
% DISAPPEARANCE_RATIONALE: If this doctrine and its enforcement vanished, the theological justification for offensive jihad would collapse, leading to a profound re-evaluation of Islamic political theology, potentially fostering new interfaith relations and altering the geopolitical landscape where such movements operate.
% FOUNDING_PROBLEM: The perceived need for a clear, unambiguous divine mandate to establish Islamic supremacy and address the perceived threat or infidelity of non-Muslim populations, particularly after early conflicts in Islamic history.
% FOUNDING_PROBLEM_CORROBORATION: Adherents of this reading, particularly expansionist movements, attest that the founding problem (the need for Islamic supremacy and dealing with non-believers) is still live and eternal. External corroboration is absent; dissenting scholars and analytical observers contest this, arguing the problem was time-bound or is a misinterpretation.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.9) is severe, as it demands submission or conversion from non-Muslims, backed by force. Suppression (0.95) is extremely high, as it actively delegitimizes and often persecutes alternative interpretations and advocates for coexistence. The theater ratio (0.1) is low because this doctrine is typically applied directly and aggressively, with little performative cover for its core function. Accessibility collapse (0.9) is high as it aims to eliminate all alternatives to submission. Resistance (0.8) is high, coming from both non-Muslim targets and internal Muslim dissenters. The temporal measurements reflect periods of waxing and waning influence of this interpretation throughout history, with a recent resurgence.
 *
 * PERSPECTIVAL GAP:
 *   Adherents of this reading perceive it as a divinely ordained, just, and necessary command for establishing God's law on Earth, thus a 'mountain' or 'rope' for them. However, from the perspective of non-Muslims, dissenting scholars, and analytical observers, it functions as a 'snare' due to its coercive, extractive, and suppressive nature. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist jihadist movements and their adherents are clear beneficiaries and agenda-setters, gaining power, resources, and a sense of divine mandate. Non-Muslims and polytheists are the primary targets and victims, facing existential threats. Dissenting Muslim scholars and coexistence advocates are also victims, facing severe intellectual and physical suppression for challenging the doctrine. Analytical observers maintain an external, non-participatory stance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_hermeneutics_validity,
    'Is the hermeneutical principle of abrogation (naskh) correctly applied to Quran 9:5, and does it genuinely supersede all prior peaceful verses?',
    'Consensus among a broad, independent body of Islamic jurists and hermeneutic scholars, or a re-evaluation of classical tafsir (exegesis) methodologies.',
    'If abrogation is deemed invalid or misapplied, the foundational premise of this reading collapses, reclassifying it from a ''snare'' to a ''piton'' (if maintained by inertia) or dissolving it entirely. If validated, its theological grounding is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_hermeneutics_validity, conceptual, 'The validity of the abrogation claim for Quran 9:5.').

omega_variable(
    divine_command_vs_interpretation,
    'Is the universal offensive jihad doctrine a direct, unambiguous divine command, or a specific human interpretation of a complex text?',
    'Comparative theological analysis across diverse Islamic schools of thought, examining the historical and linguistic context of the verse, and its relationship to the broader Quranic corpus.',
    'If it''s primarily a human interpretation, its ''mountain-like'' claim of divine inevitability weakens, making it more susceptible to reclassification as a ''snare'' or ''tangled_rope'' based on its human-constructed elements. If a direct command, its adherents'' identity-lock is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_vs_interpretation, conceptual, 'Distinguishing divine command from human interpretation in this doctrine.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of dissenting Muslim scholars structural (e.g., state censorship, fatwas) or internalized (e.g., self-censorship due to fear of reprisal or social pressure)?',
    'Post-exit suppression trajectory: if scholars continue to self-censor or face social ostracization even after formal legal barriers are removed, reclassify as partially internalized. Empirical studies on academic freedom in relevant contexts.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the snare more insidious. If purely structural, removing external barriers would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting scholars.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 622, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t622, quran_9_5_scope__abrogating_universal, theater_ratio, 622, 0.1).
narrative_ontology:measurement(qura_tr_t750, quran_9_5_scope__abrogating_universal, theater_ratio, 750, 0.05).
narrative_ontology:measurement(qura_tr_t1200, quran_9_5_scope__abrogating_universal, theater_ratio, 1200, 0.15).
narrative_ontology:measurement(qura_tr_t1600, quran_9_5_scope__abrogating_universal, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(qura_tr_t1900, quran_9_5_scope__abrogating_universal, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(qura_tr_t2024, quran_9_5_scope__abrogating_universal, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t622, quran_9_5_scope__abrogating_universal, base_extractiveness, 622, 0.7).
narrative_ontology:measurement(qura_be_t750, quran_9_5_scope__abrogating_universal, base_extractiveness, 750, 0.85).
narrative_ontology:measurement(qura_be_t1200, quran_9_5_scope__abrogating_universal, base_extractiveness, 1200, 0.75).
narrative_ontology:measurement(qura_be_t1600, quran_9_5_scope__abrogating_universal, base_extractiveness, 1600, 0.6).
narrative_ontology:measurement(qura_be_t1900, quran_9_5_scope__abrogating_universal, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(qura_be_t2024, quran_9_5_scope__abrogating_universal, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t622, quran_9_5_scope__abrogating_universal, suppression_requirement, 622, 0.75).
narrative_ontology:measurement(qura_su_t750, quran_9_5_scope__abrogating_universal, suppression_requirement, 750, 0.9).
narrative_ontology:measurement(qura_su_t1200, quran_9_5_scope__abrogating_universal, suppression_requirement, 1200, 0.8).
narrative_ontology:measurement(qura_su_t1600, quran_9_5_scope__abrogating_universal, suppression_requirement, 1600, 0.65).
narrative_ontology:measurement(qura_su_t1900, quran_9_5_scope__abrogating_universal, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(qura_su_t2024, quran_9_5_scope__abrogating_universal, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_law_of_war_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_9_5_scope' kernel. It directly contests and seeks to delegitimize the 'contextual_defensive' and 'progressive_synthesis' readings, which offer alternative interpretations of Quran 9:5.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
