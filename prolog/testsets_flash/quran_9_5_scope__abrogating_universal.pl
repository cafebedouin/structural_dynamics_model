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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Quran 9:5 Abrogating Universal Jihad Obligation
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'abrogating_universal' reading of Quran
 *   9:5, which interprets the verse as abrogating all prior peaceful verses
 *   and establishing universal offensive jihad against polytheists until
 *   their submission or conversion. This reading transforms what might
 *   otherwise be a contextual directive into a standing legal obligation for
 *   expansionist movements. It places all non-Muslims (and even moderate
 *   Muslims who reject this interpretation) into a victim set, legitimizing
 *   first-strike violence and suppressing any framework for peaceful
 *   coexistence.
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
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Quran 9:5 Abrogating Universal Jihad Obligation").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '0c310d6e-47fe-409b-87e1-a2ab56cd8400').
narrative_ontology:cs_kernel_codification('0c310d6e-47fe-409b-87e1-a2ab56cd8400', fixed_text).
narrative_ontology:cs_authority_grounding('0c310d6e-47fe-409b-87e1-a2ab56cd8400', lineage).
narrative_ontology:cs_interpretation_layer_present('0c310d6e-47fe-409b-87e1-a2ab56cd8400').
narrative_ontology:cs_reading_relation('0c310d6e-47fe-409b-87e1-a2ab56cd8400', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('0c310d6e-47fe-409b-87e1-a2ab56cd8400', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('0c310d6e-47fe-409b-87e1-a2ab56cd8400', foundational, abrogation_of_peaceful_verses).
narrative_ontology:cs_axiom_status(abrogation_of_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('0c310d6e-47fe-409b-87e1-a2ab56cd8400', abrogation_of_peaceful_verses, conventional).
narrative_ontology:cs_axiom('0c310d6e-47fe-409b-87e1-a2ab56cd8400', foundational, universal_offensive_jihad_obligation).
narrative_ontology:cs_axiom_status(universal_offensive_jihad_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0c310d6e-47fe-409b-87e1-a2ab56cd8400', universal_offensive_jihad_obligation, theological).
narrative_ontology:cs_reference_frame('0c310d6e-47fe-409b-87e1-a2ab56cd8400', classical_abrogating_jurisprudence).
narrative_ontology:cs_drift_state('0c310d6e-47fe-409b-87e1-a2ab56cd8400', contemporary_global_islamic_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0c310d6e-47fe-409b-87e1-a2ab56cd8400', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadi_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, radical_clerics).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslims).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, moderate_muslims).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, coexistence_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions and beliefs of adherents towards a unified goal of establishing Islamic supremacy through military expansion and conversion, providing a clear legal and theological justification for such actions.
% TRANSFER_FUNCTION: Transfers autonomy, resources, and religious freedom from non-Muslim populations to the Islamic state or movement, and transfers theological authority from diverse interpretations to a singular, militant one.
% ABSENT_VOICES: The voices of non-Muslims, who are directly targeted, are absent from the interpretive process. Also absent are the voices of early Islamic scholars who held more nuanced or contextual interpretations of jihad, and contemporary Muslim scholars advocating for peaceful coexistence or defensive-only jihad. Their perspectives are actively suppressed by this reading's assertion of abrogation.
% DISAPPEARANCE_RATIONALE: If this specific reading of Quran 9:5 vanished overnight, it would fundamentally alter the theological and legal basis for many expansionist jihadi movements, likely leading to their collapse or radical reorientation. The global landscape of interfaith relations and conflict would significantly rearrange, as a major justification for religiously motivated violence would be removed. Moderate Islamic interpretations would gain prominence, and the victim sets would largely dissolve.
% FOUNDING_PROBLEM: The problem this reading was built to solve was the perceived theological inconsistency between earlier peaceful verses and later, more militant ones, particularly in the context of the early Muslim community's expansion and conflicts with surrounding tribes. It sought to provide a clear, unified legal framework for military action and the treatment of non-Muslims.
% FOUNDING_PROBLEM_CORROBORATION: Radical clerics and jihadi ideologues attest that the problem of theological inconsistency and the need for a clear mandate for offensive jihad remain live, citing ongoing conflicts and the perceived need to establish global Islamic rule. However, a vast majority of mainstream Islamic scholars and international legal bodies, from outside the benefiting parties, dispute this, arguing that the founding problem was either specific to the 7th century or is now superseded by principles of international law and interfaith dialogue. They corroborate that the 'problem' is now primarily a justification for political power, not a genuine theological dilemma.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is very high (0.9) because this reading demands submission or conversion, extracting autonomy, resources, and even life from non-adherents. Suppression is also very high (0.95) as it actively delegitimizes and seeks to eliminate alternative interpretations and coexistence frameworks, requiring constant enforcement against dissenters and external 'targets'. The theater ratio is low (0.1) because the stated goal (universal submission/conversion) is directly pursued, with little performative cover for other functions. Resistance is high (0.8) due to the extreme demands placed on non-Muslims and the rejection by moderate Muslims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of expansionist jihadi movements, this constraint is a divine command, a Mountain or a Rope for righteous action. From the perspective of non-Muslims and moderate Muslims, it is a clear Snare, an existential threat that demands active resistance. The engine's classification will reflect the latter, as the structural data points to extreme extraction and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist jihadi movements and radical clerics are the primary beneficiaries (d=0.0-0.1) as this reading provides them with a divine mandate for their agenda, legitimizing their actions and attracting followers. Non-Muslims are the primary targets (d=1.0) as they face the direct demands of submission or conversion. Moderate Muslims and coexistence advocates are also targets (d=0.8-0.9) as their interpretations and efforts are suppressed and delegitimized by this reading. The constraint subsidizes the expansionist agenda while extracting from all others.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_doctrine_validity,
    'Is the doctrine of abrogation (naskh) itself a valid hermeneutical principle for Quranic interpretation, or is it a later jurisprudential construct?',
    'Scholarly consensus on the historical development of naskh theory and its textual basis; analysis of early Islamic legal methodology.',
    'If naskh is invalid, the entire basis for 9:5 abrogating peaceful verses collapses, reclassifying this constraint as a constructed snare with no textual grounding. If valid, the debate shifts to the scope and application of 9:5.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_doctrine_validity, conceptual, 'Validity of the abrogation doctrine as a hermeneutical tool.').

omega_variable(
    kernel_reading_abrogating_universal,
    'This constraint is the ''abrogating_universal'' reading of the ''quran_9_5_scope'' kernel. What would change if the ''contextual_defensive'' or ''progressive_synthesis'' readings were adopted?',
    'Shift in dominant jurisprudential interpretation within influential Islamic legal schools or political movements.',
    'If ''contextual_defensive'' were adopted, the victim set would shrink to only treaty-breakers, first-strike violence would be unauthorized, and the constraint would reclassify as a Rope or Mountain (defensive war). If ''progressive_synthesis'' were adopted, the constraint would become a historical artifact, reclassifying as a Piton or even disappearing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_abrogating_universal, conceptual, 'Impact of alternative readings of Quran 9:5 on the constraint''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t100, quran_9_5_scope__abrogating_universal, theater_ratio, 100, 0.15).
narrative_ontology:measurement(qura_tr_t500, quran_9_5_scope__abrogating_universal, theater_ratio, 500, 0.12).
narrative_ontology:measurement(qura_tr_t1000, quran_9_5_scope__abrogating_universal, theater_ratio, 1000, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(qura_be_t100, quran_9_5_scope__abrogating_universal, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(qura_be_t500, quran_9_5_scope__abrogating_universal, base_extractiveness, 500, 0.85).
narrative_ontology:measurement(qura_be_t1000, quran_9_5_scope__abrogating_universal, base_extractiveness, 1000, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(qura_su_t100, quran_9_5_scope__abrogating_universal, suppression_requirement, 100, 0.85).
narrative_ontology:measurement(qura_su_t500, quran_9_5_scope__abrogating_universal, suppression_requirement, 500, 0.9).
narrative_ontology:measurement(qura_su_t1000, quran_9_5_scope__abrogating_universal, suppression_requirement, 1000, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_law_of_war).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, dhimmi_status_application).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_9_5_scope' kernel. Its structural properties (high extraction, universal victim set) are distinct from the 'contextual_defensive' and 'progressive_synthesis' readings, which would yield significantly lower extractiveness and different victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
