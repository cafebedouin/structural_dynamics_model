% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Qur'an 9:5 Abrogating Universal Offensive Jihad Mandate
 *   domain: political_theology/religious_jurisprudence
 *
 * SUMMARY:
 *   The abrogating_universal reading of Qur'an 9:5 interprets the verse as a
 *   permanent, superseding command that establishes universal offensive
 *   obligation against non-Muslims until they submit or convert. This reading
 *   competes with two other theological readings of the same kernel:
 *   contextual_defensive (argues 9:5 addresses specific 7th-century treaty
 *   violations and does not abrogate defensive-only jurisprudence) and
 *   progressive_synthesis (argues 9:5 is time-bound political directive,
 *   superseded by Qur'an's overarching ethical trajectory). The
 *   abrogating_universal reading is the one instantiated here. It
 *   structurally benefits expansionist jihadist movements and political
 *   authorities claiming divine mandate; it victimizes non-Muslim
 *   populations, pacifist Muslim traditions, and scholars advocating
 *   coexistence frameworks. The constraint exhibits dramatic lifecycle
 *   dynamics: suppression and extractiveness were substantially lower
 *   (0.32–0.48) during the early Islamic period and classical-medieval
 *   jurisprudence eras when contextual readings dominated institutional
 *   authority, but rose sharply (0.68–0.92) in the 20th century as jihadist
 *   movements gained organizational capacity and used the reading to
 *   legitimate recruitment and military action.
 *
 * KEY AGENTS:
 *   - Expansionist jihadist movements (organized, agenda-setter): define and enforce the abrogating interpretation, benefit from theological justification for territorial and religious expansion.
 *   - Non-Muslim populations (powerless, victim): targeted as valid military objectives absent submission, facing forced conversion, subjugation, or violence.
 *   - Muslim pacifist traditions (moderate, victim-identity_locked): theological position rendered obsolete by the abrogating claim, forced to choose between assimilation, public recantation, or marginalization.
 *   - Coexistence-advocating scholars (powerful, victim-constrained): interpretive authority suppressed, professional standing threatened, constrained from exit by commitment to Islamic scholarship.
 *   - Political authorities (institutional, beneficiary-mobile): gain theological legitimation for military policy and governance claims; can exit the reading if political circumstances shift.
 *   - Traditional jurisprudence schools (institutional, excluded): historically-established schools marginalized by the claim that verse 9:5 supersedes their accumulated jurisprudence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.88).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.92).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.88).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Qur'an 9:5 Abrogating Universal Offensive Jihad Mandate").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "political_theology/religious_jurisprudence").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, 'f66550d8-d9d2-4267-a781-74beda365cf6').
narrative_ontology:cs_kernel_codification('f66550d8-d9d2-4267-a781-74beda365cf6', fixed_text).
narrative_ontology:cs_authority_grounding('f66550d8-d9d2-4267-a781-74beda365cf6', extraction).
narrative_ontology:cs_interpretation_layer_present('f66550d8-d9d2-4267-a781-74beda365cf6').
narrative_ontology:cs_reading_relation('f66550d8-d9d2-4267-a781-74beda365cf6', quran_9_5_scope__contextual_defensive, coexists_with).
narrative_ontology:cs_reading_relation('f66550d8-d9d2-4267-a781-74beda365cf6', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('f66550d8-d9d2-4267-a781-74beda365cf6', foundational, verse_9_5_permanent_abrogation).
narrative_ontology:cs_axiom_status(verse_9_5_permanent_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('f66550d8-d9d2-4267-a781-74beda365cf6', verse_9_5_permanent_abrogation, deontological).
narrative_ontology:cs_axiom('f66550d8-d9d2-4267-a781-74beda365cf6', foundational, non_muslim_submission_prerequisite).
narrative_ontology:cs_axiom_status(non_muslim_submission_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('f66550d8-d9d2-4267-a781-74beda365cf6', non_muslim_submission_prerequisite, deontological).
narrative_ontology:cs_reference_frame('f66550d8-d9d2-4267-a781-74beda365cf6', permanent_universal_islamic_obligation).
narrative_ontology:cs_drift_state('f66550d8-d9d2-4267-a781-74beda365cf6', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f66550d8-d9d2-4267-a781-74beda365cf6', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, political_authorities_claiming_divine_mandate).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, muslim_pacifist_traditions).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, coexistence_advocating_scholars).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).

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
 *   Extractiveness is high (0.88) because the reading authorizes uncompensated subjugation of entire populations and transfer of interpretive authority to movements that weaponize it. Suppression is higher still (0.92) because the reading depends on active enforcement of a singular interpretation against competing scholarly traditions—coexistence frameworks must be suppressed or delegitimized for the reading to persist. Theater is low (0.12) because the constraint does minimal performative work; its enforcement is direct and its extractive function is transparent to participants. The measurement series document a critical lifecycle pattern: extractiveness and suppression DECLINED significantly from 632–1800 (medieval/early-modern era when contextual jurisprudence dominated) to 0.32–0.48, then ROSE sharply from 1950 onward (0.68–0.92 by 2026) as jihadist organizational movements scaled globally and used the reading to legitimate campaigns. This reversal indicates the reading is not naturally-persisting; it required organizational investment and active suppression of competing readings to be reinstated as dominant.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (jihadist movements) perceives the constraint as simple textual truth—verse 9:5 objectively abrogates all prior peaceful passages. From the payer seats (non-Muslim populations, pacifist Muslims, coexistence scholars), the same constraint is a constructed reading whose persistence depends on organizational power and suppression of alternatives. The engine should compute this gap: jihadist seats perceive near-mountain classification (unchangeable textual fact), while victim and scholar seats compute high-extraction snare. The divergence is not a measurement error; it is precisely the gap the abrogating reading creates—it manufactures perceived inevitability to suppress recognition of its extractive character.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist movements have low directionality (d near 0.0–0.2) as beneficiaries who collect theological mandate and organizational legitimacy. Non-Muslim populations have high directionality (d near 0.9–1.0) as trapped targets who bear the constraint's direct physical and political costs. Pacifist Muslim traditions sit at d ~0.7–0.85: they are victims of interpretive suppression (high d) but could theoretically exit through recantation (slightly lower than trapped populations). Coexistence scholars sit at d ~0.6–0.75: powerful enough to resist and theoretically mobile, but identity-locked to Islamic scholarship means recanting their position damages professional survival. Political authorities sit at d ~0.3–0.5: they benefit from the reading's theological cover but can shift interpretation if political winds change, giving them genuine arbitrage options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits no mandatrophy: its founding problem (theological resolution of seemingly-conflicting verses about warfare) remains contested, not obsolete. The reading's mandate persists because movements continue to believe it is valid and enforce it. However, the lifecycle measurements reveal something more subtle: the reading's mandate is NOT naturally-persisting (as a mountain would be if truly textual-fact). The sharp rise in extractiveness and suppression from 1950–2026, starting from much lower baseline in medieval eras, indicates the abrogating reading is a constructed, actively-defended interpretation that required 20th-century organizational movements to scale it globally. The medieval decline (when contextual readings dominated) shows it is not textually-inevitable. This is the signature of a snare that manufactures perceived inevitability: the reading claims to be mere textual truth (mountain-like) but its lifecycle shows organizational construction (snare). An omega variable flags this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_inevitability_vs_constructed_reading,
    'Is the abrogating_universal reading a straightforward, textually-inevitable interpretation of Qur''an 9:5, or a constructed reading whose dominance depends on organizational power and suppression of alternatives?',
    'Historical analysis of medieval/classical jurisprudence documentation showing contextual readings were dominant institutional positions (resolved: yes, they were). Counterfactual: if jihadist organizational movements had not scaled globally from 1950–2026, would this reading remain marginal? Corroboration: major contemporary Islamic institutions (Al-Azhar, traditional madhabs) reject the abrogating_universal reading—indicating it is contested, not textually-mandatory.',
    'If the reading is textually-inevitable, it is a Mountain constraint. If it is organizationally-constructed, it is a Snare (extractive constraint depending on suppression). The lifecycle measurements strongly suggest the latter—the sharp rise in suppression and extractiveness after 1900 indicates construction, not discovery of textual meaning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_inevitability_vs_constructed_reading, empirical, 'Whether the abrogating reading is textual fact or constructed reading.').

omega_variable(
    beneficiary_consciousness_authenticity,
    'Do expansionist jihadist movements genuinely believe the abrogating_universal reading is accurate textual interpretation, or do they strategically deploy it knowing it is contested?',
    'Internal documentary evidence (recruitment materials, theological treatises, leadership statements) combined with external analysis of strategic communications. High consciousness = deliberate choice to suppress alternatives; low consciousness = genuine belief in textual truth. The distinction affects whether suppression is intentional extraction or sincere theological enforcement.',
    'High consciousness (strategic deployment) classifies the constraint as intentional snare with malice aforethought. Low consciousness (sincere belief) reclassifies as snare-via-zealotry where beneficiaries do not recognize the constraint as extractive. The suppression magnitude stays the same; the agent''s intentionality is the variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_consciousness_authenticity, empirical, 'Whether jihadist beneficiaries are strategically conscious of the reading''s contested status.').

omega_variable(
    identity_lock_persistence_after_exit,
    'For Muslim pacifists and coexistence scholars with identity_locked exit: if they left the religious framework entirely, would suppression persist? Or is suppression entirely internalized within the religious identity itself?',
    'Post-exit trajectories: apostates, secular ex-Muslims, reformed scholars who publicly abandoned Islamic authority claims. If suppression persists (continued harassment, family isolation, psychological distress), it is partially structural/internalized. If suppression ceases upon exit, it was entirely identity-contingent.',
    'If suppression is identity-contingent, it can be escaped by abandoning Islamic identity—exit is theoretically possible but costs religious identity. If suppression persists after exit, it is structural (community violence, state sanctions) and trapped exit is confirmed. The identity-locked classification itself depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_after_exit, empirical, 'Whether suppression of pacifist voices is identity-contingent or structurally independent.').

omega_variable(
    nasikh_doctrine_interpretive_closure,
    'Does the nasikh (abrogation) doctrine itself permit flexible interpretation, or does it claim to provide definitive textual closure? Can the doctrine itself be read contextually?',
    'Theological analysis of nasikh doctrine across Islamic schools: does it permit conditions (abrogate under X circumstance), temporal limits (valid for N years), or only parties? Or does it declare permanent overriding?',
    'If nasikh permits conditions, then even accepting the doctrine does not entail the abrogating_universal reading—conditions could limit verse 9:5 to specific contexts. If nasikh claims permanence, the reading is more logically binding. This affects whether the constraint''s claim is defensible within orthodox Islamic logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nasikh_doctrine_interpretive_closure, conceptual, 'Whether the nasikh doctrine permits the abrogating_universal interpretation or alternative readings.').

omega_variable(
    non_muslim_population_agency_erasure,
    'In classifying non-Muslims as victims with trapped exit, are we accurately reflecting their political agency, or are we absorbing the constraint''s own erasure of their capacity to resist and negotiate?',
    'Historical analysis of non-Muslim responses: treaties, counter-movements, successful defensive coalitions, institutional accommodations that limited the reading''s enforcement. Do these examples show genuine exit/negotiation options, or are they marginal to the dominant enforcement pattern?',
    'If non-Muslims had genuine negotiating capacity in most historical contexts, exit_options should be ''constrained'' rather than ''trapped''—they faced difficult choices, not elimination. If the reading''s enforcement was so overwhelming that negotiation was illusory, ''trapped'' is accurate. The distinction affects directionality valuation and the snare classification''s certainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_muslim_population_agency_erasure, empirical, 'Whether non-Muslim populations had constrained or trapped exit options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 632, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t632, quran_9_5_scope__abrogating_universal, theater_ratio, 632, 0.08).
narrative_ontology:measurement(qura_tr_t900, quran_9_5_scope__abrogating_universal, theater_ratio, 900, 0.06).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__abrogating_universal, theater_ratio, 1400, 0.04).
narrative_ontology:measurement(qura_tr_t1800, quran_9_5_scope__abrogating_universal, theater_ratio, 1800, 0.03).
narrative_ontology:measurement(qura_tr_t1950, quran_9_5_scope__abrogating_universal, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(qura_tr_t2000, quran_9_5_scope__abrogating_universal, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(qura_tr_t2026, quran_9_5_scope__abrogating_universal, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(qura_be_t632, quran_9_5_scope__abrogating_universal, base_extractiveness, 632, 0.65).
narrative_ontology:measurement(qura_be_t900, quran_9_5_scope__abrogating_universal, base_extractiveness, 900, 0.58).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__abrogating_universal, base_extractiveness, 1400, 0.42).
narrative_ontology:measurement(qura_be_t1800, quran_9_5_scope__abrogating_universal, base_extractiveness, 1800, 0.38).
narrative_ontology:measurement(qura_be_t1950, quran_9_5_scope__abrogating_universal, base_extractiveness, 1950, 0.72).
narrative_ontology:measurement(qura_be_t2000, quran_9_5_scope__abrogating_universal, base_extractiveness, 2000, 0.81).
narrative_ontology:measurement(qura_be_t2026, quran_9_5_scope__abrogating_universal, base_extractiveness, 2026, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t632, quran_9_5_scope__abrogating_universal, suppression_requirement, 632, 0.55).
narrative_ontology:measurement(qura_su_t900, quran_9_5_scope__abrogating_universal, suppression_requirement, 900, 0.48).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__abrogating_universal, suppression_requirement, 1400, 0.35).
narrative_ontology:measurement(qura_su_t1800, quran_9_5_scope__abrogating_universal, suppression_requirement, 1800, 0.32).
narrative_ontology:measurement(qura_su_t1950, quran_9_5_scope__abrogating_universal, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(qura_su_t2000, quran_9_5_scope__abrogating_universal, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(qura_su_t2026, quran_9_5_scope__abrogating_universal, suppression_requirement, 2026, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__abrogating_universal, 0.18).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_statecraft__caliphate_legitimacy).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, muslim_minority_coexistence_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel quran_9_5_scope. The kernel is Qur'anic verse 9:5 and its textual/theological implications for warfare and non-Muslim status. Each reading instantiates a structurally distinct constraint with different ε, different beneficiary/victim sets, and different suppression mechanisms. The abrogating_universal reading (this file) has the highest extractiveness (0.88) and suppression (0.92); it authorizes first-strike violence against non-Muslims absent submission. The contextual_defensive reading (sibling constraint) interprets the verse as addressing specific 7th-century treaty violations and preserves defensive-only jurisprudence—lower extractiveness (~0.45). The progressive_synthesis reading treats verse 9:5 as time-bound political directive, superseded by Qur'an's ethical trajectory—intermediate extractiveness (~0.55). The three readings are NOT different measurements of one constraint; they are three constraints with three different logical structures and three different ε values. They are linked via network.affects_constraints because the abrogating reading's dominance depends on suppressing the interpretive authority of the other two readings—institutional competition between readings is the defining structural feature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
