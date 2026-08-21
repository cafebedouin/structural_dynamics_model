% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Quran 9:5 as Contextual Defensive Warfare
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint represents a specific reading of Quranic verse 9:5,
 *   interpreting it as a directive for defensive warfare strictly within its
 *   7th-century Medinan context, specifically against treaty-breaking
 *   polytheist tribes. It explicitly rejects the notion that 9:5 abrogates
 *   (supersedes) earlier peaceful verses, instead emphasizing the Quran's
 *   overall commitment to treaty obligations and defensive action. This
 *   reading is crucial for integrationist Muslim-majority states and minority
 *   communities seeking peaceful coexistence and alignment with international
 *   law. The constraint's metrics reflect its low extractiveness and
 *   suppression, consistent with a coordination mechanism for peace and
 *   order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.2).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.15).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.2).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Quran 9:5 as Contextual Defensive Warfare").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "islamic_jurisprudence/hermeneutics/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'b48b1099-13ab-434a-a559-36a9d09e3f6b').
narrative_ontology:cs_kernel_codification('b48b1099-13ab-434a-a559-36a9d09e3f6b', fixed_text).
narrative_ontology:cs_authority_grounding('b48b1099-13ab-434a-a559-36a9d09e3f6b', lineage).
narrative_ontology:cs_interpretation_layer_present('b48b1099-13ab-434a-a559-36a9d09e3f6b').
narrative_ontology:cs_reading_relation('b48b1099-13ab-434a-a559-36a9d09e3f6b', quran_9_5_scope__abrogating_universal, coexists_with).
narrative_ontology:cs_reading_relation('b48b1099-13ab-434a-a559-36a9d09e3f6b', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('b48b1099-13ab-434a-a559-36a9d09e3f6b', foundational, quranic_verses_are_contextual).
narrative_ontology:cs_axiom_status(quranic_verses_are_contextual, holdable).
narrative_ontology:cs_axiom_grounding('b48b1099-13ab-434a-a559-36a9d09e3f6b', quranic_verses_are_contextual, conventional).
narrative_ontology:cs_axiom('b48b1099-13ab-434a-a559-36a9d09e3f6b', foundational, treaty_obligations_are_paramount).
narrative_ontology:cs_axiom_status(treaty_obligations_are_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b48b1099-13ab-434a-a559-36a9d09e3f6b', treaty_obligations_are_paramount, deontological).
narrative_ontology:cs_reference_frame('b48b1099-13ab-434a-a559-36a9d09e3f6b', medinan_defensive_context).
narrative_ontology:cs_drift_state('b48b1099-13ab-434a-a559-36a9d09e3f6b', contemporary_international_relations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b48b1099-13ab-434a-a559-36a9d09e3f6b', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, muslim_minority_communities_in_pluralistic_societies).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, international_law_frameworks).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_breaking_polytheist_tribes_7th_century).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, islamic_law_of_war_is_defensive).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, treaty_obligations_are_paramount_in_islam).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, quranic_verses_are_contextual).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from a reading that supports peaceful coexistence, treaty adherence, and defensive warfare, aligning their foreign policy with international norms and fostering internal stability. This interpretation provides a theological basis for their integrationist policies.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, mobile, national).

% These communities benefit from an interpretation that promotes peace and interfaith harmony, reducing external prejudice and internal conflict. It allows them to reconcile their faith with the demands of living in non-Muslim majority states without theological contradiction.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, muslim_minority_communities_in_pluralistic_societies, beneficiary,
    moderate, biographical, constrained, local).

% The framework of international law, particularly regarding just war theory and treaty obligations, is implicitly vindicated by this reading, as it aligns Islamic jurisprudence with global legal principles. It benefits from the reduction of theological justifications for offensive warfare.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, international_law_frameworks, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(quran_9_5_scope__contextual_defensive, international_law_frameworks).

% In the 7th-century Medinan context, these tribes faced military action due to their repeated treaty violations and aggression against the early Muslim community. This reading confines the verse's application to such specific historical circumstances, making them the 'victims' of a context-specific defensive response.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_breaking_polytheist_tribes_7th_century, payer,
    powerless, immediate, trapped, local).

% These groups are structurally excluded from this interpretive framework, as their ideology relies on an abrogating, universalist reading of 9:5 to justify offensive jihad. They would vehemently reject this contextual-defensive interpretation, as it undermines their theological basis for action.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, jihadist_groups, excluded,
    organized, biographical, identity_locked, regional).

% Scholars from the classical abrogating school, who hold that 9:5 supersedes earlier peaceful verses, are excluded from this reading's core premises. Their interpretive methodology would be challenged by the emphasis on context and non-abrogation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, classical_jurists_abrogating_school, excluded,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a theological framework for Muslim communities and states to engage peacefully with non-Muslims, uphold treaties, and restrict military action to defensive necessity, thereby coordinating inter-communal and international relations on a basis of mutual respect and legal obligation.
% TRANSFER_FUNCTION: Transfers theological legitimacy from offensive, universalist interpretations of jihad to defensive, contextualized interpretations, thereby shifting the moral and legal burden of initiating conflict onto those who violate treaties or initiate aggression.
% ABSENT_VOICES: Jihadist groups and proponents of the abrogating-universalist school of thought are absent from this reading's internal discourse; they would argue that this interpretation dilutes the Quran's universal commands and undermines the obligation of offensive jihad, but their premises are rejected by this reading's methodology.
% DISAPPEARANCE_RATIONALE: If this contextual-defensive reading vanished, the theological justification for peaceful coexistence and defensive warfare would weaken, potentially leading to increased internal and external conflict for Muslim states and communities, as more aggressive interpretations might gain prominence. International relations involving Muslim-majority nations would become more volatile.
% FOUNDING_PROBLEM: The early Muslim community in Medina faced existential threats from hostile, treaty-breaking tribes, necessitating a clear directive for defensive military action while upholding the sanctity of treaties.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historians and scholars of Islamic law, from outside the immediate beneficiaries of this reading, corroborate that the 7th-century Medinan context was indeed one of specific tribal conflicts and treaty violations. They attest that the original problem is no longer live in the same form, but the interpretive debate persists.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) because this reading primarily coordinates peaceful relations and defensive action, imposing costs only on those who violate established norms or treaties. Suppression is also low (0.15) as its persistence relies on theological coherence and alignment with broader ethical principles rather than coercion. The theater ratio is minimal (0.05) because its function is genuinely about establishing a framework for just conduct, not performative justification for extraction. Accessibility collapse is moderate (0.7) as alternative, more aggressive readings exist but are actively countered by this interpretive tradition. Resistance is low (0.1) from within its own interpretive community, though it faces strong resistance from opposing schools of thought.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between this reading and the abrogating-universalist reading. From the perspective of this contextual-defensive reading, the abrogating-universalist view is a misinterpretation that distorts the Quran's message. Conversely, proponents of the abrogating view would see this reading as weakening Islamic legal obligations. The engine's classification will highlight how different interpretive methodologies lead to vastly different structural outcomes from the same kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist Muslim states and minority communities are beneficiaries, as this reading provides a theological basis for their peaceful and pluralistic engagement. International law frameworks also benefit from this alignment. The 7th-century treaty-breaking tribes are identified as the historical 'victims' of the verse's specific application. Jihadist groups and classical abrogating jurists are excluded, as their positions are fundamentally incompatible with this reading's premises.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_specificity,
    'To what extent can the specific 7th-century Medinan context be definitively isolated from broader, timeless Quranic principles, and how does this isolation impact the verse''s applicability today?',
    'Further historical-critical scholarship on early Islamic history and comparative analysis of Quranic hermeneutics across different eras.',
    'If the context is less isolable, the ''contextual_defensive'' reading''s claim to limit the verse''s scope is weakened, potentially allowing for broader application. If more isolable, the reading''s specificity is strengthened, further limiting the verse to its original historical moment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_specificity, empirical, 'Ambiguity regarding the precise boundaries of historical context in Quranic interpretation.').

omega_variable(
    abrogation_methodology_validity,
    'Is the classical methodology of abrogation (naskh) a valid and universally accepted interpretive principle within Islamic jurisprudence, or is its application contested?',
    'Analysis of the historical development of naskh theory, its internal inconsistencies, and the arguments of scholars who reject or limit its application.',
    'If naskh is widely rejected or severely limited, the ''abrogating_universal'' reading loses its primary theological tool, strengthening the ''contextual_defensive'' reading. If naskh is affirmed, the ''contextual_defensive'' reading faces a stronger challenge from the abrogating school.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_methodology_validity, conceptual, 'Contestation over the interpretive principle of abrogation (naskh) in Islamic law.').

omega_variable(
    coexistence_norms_structural_protection,
    'How robustly do ''coexistence norms'' (as a structural protection) actually prevent the re-emergence of more aggressive interpretations in times of political or social stress?',
    'Empirical study of how Muslim-majority states and communities apply this reading during periods of conflict or political instability, observing whether the defensive interpretation holds or shifts towards more aggressive stances.',
    'If coexistence norms prove fragile under stress, the ''contextual_defensive'' reading''s practical efficacy as a ''rope'' is reduced, and its classification might drift towards a ''tangled_rope'' or even ''snare'' if it fails to prevent extraction by aggressive actors. If robust, its ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_norms_structural_protection, empirical, 'The resilience of coexistence norms against political and social pressures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__contextual_defensive, theater_ratio, 10, 0.05).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__contextual_defensive, theater_ratio, 20, 0.05).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__contextual_defensive, theater_ratio, 30, 0.05).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__contextual_defensive, theater_ratio, 40, 0.05).
narrative_ontology:measurement(qura_tr_t50, quran_9_5_scope__contextual_defensive, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__contextual_defensive, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__contextual_defensive, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__contextual_defensive, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__contextual_defensive, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(qura_be_t50, quran_9_5_scope__contextual_defensive, base_extractiveness, 50, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__contextual_defensive, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__contextual_defensive, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(qura_su_t30, quran_9_5_scope__contextual_defensive, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__contextual_defensive, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(qura_su_t50, quran_9_5_scope__contextual_defensive, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, islamic_law_of_war_doctrine).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, treaty_adherence_in_islamic_states).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_9_5_scope' kernel. The other readings are 'abrogating_universal' and 'progressive_synthesis'. Each represents a distinct structural claim about the verse's meaning and application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
