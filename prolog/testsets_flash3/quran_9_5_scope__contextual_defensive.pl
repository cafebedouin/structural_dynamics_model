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
 *   This constraint represents the 'contextual-defensive' reading of Quran
 *   9:5, which interprets the verse as a specific directive for defensive
 *   warfare against treaty-breaking polytheist tribes in 7th-century Medina,
 *   rather than a universal command for offensive jihad. This reading
 *   emphasizes the historical context, the sanctity of treaties, and the
 *   overall defensive nature of Islamic warfare. It explicitly rejects the
 *   abrogation of peaceful verses and promotes peaceful pluralism. This is
 *   one reading of the 'quran_9_5_scope' kernel.
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
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'ed79f02b-2201-4042-8e37-7e7bc5a9071c').
narrative_ontology:cs_kernel_codification('ed79f02b-2201-4042-8e37-7e7bc5a9071c', fixed_text).
narrative_ontology:cs_authority_grounding('ed79f02b-2201-4042-8e37-7e7bc5a9071c', lineage).
narrative_ontology:cs_interpretation_layer_present('ed79f02b-2201-4042-8e37-7e7bc5a9071c').
narrative_ontology:cs_reading_relation('ed79f02b-2201-4042-8e37-7e7bc5a9071c', quran_9_5_scope__abrogating_universal, coexists_with).
narrative_ontology:cs_reading_relation('ed79f02b-2201-4042-8e37-7e7bc5a9071c', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('ed79f02b-2201-4042-8e37-7e7bc5a9071c', foundational, quranic_verses_are_contextual).
narrative_ontology:cs_axiom_status(quranic_verses_are_contextual, holdable).
narrative_ontology:cs_axiom_grounding('ed79f02b-2201-4042-8e37-7e7bc5a9071c', quranic_verses_are_contextual, conventional).
narrative_ontology:cs_axiom('ed79f02b-2201-4042-8e37-7e7bc5a9071c', foundational, islamic_law_of_war_is_defensive).
narrative_ontology:cs_axiom_status(islamic_law_of_war_is_defensive, holdable).
narrative_ontology:cs_axiom_grounding('ed79f02b-2201-4042-8e37-7e7bc5a9071c', islamic_law_of_war_is_defensive, deontological).
narrative_ontology:cs_reference_frame('ed79f02b-2201-4042-8e37-7e7bc5a9071c', classical_defensive_jihad_framework).
narrative_ontology:cs_drift_state('ed79f02b-2201-4042-8e37-7e7bc5a9071c', contemporary_international_law_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ed79f02b-2201-4042-8e37-7e7bc5a9071c', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, muslim_minority_communities_in_pluralistic_societies).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_breaking_polytheist_tribes_7th_century_medina).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, islamic_law_of_war_is_defensive).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, treaty_obligations_are_paramount_in_islam).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, quranic_verses_are_contextual).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a jurisprudential reading that supports peaceful coexistence, treaty adherence, and defensive military postures, aligning with modern international law and promoting stability within diverse populations. This reading provides a theological basis for integration and pluralism.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, mobile, national).

% Benefits from a reading that counters extremist narratives and promotes a peaceful image of Islam, facilitating their integration and reducing prejudice. It provides a theological framework for loyalty to non-Muslim states and interfaith harmony.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, muslim_minority_communities_in_pluralistic_societies, beneficiary,
    organized, biographical, constrained, local).

% Historically, these tribes faced military action due to their repeated violations of treaties and aggression against the early Muslim community. This reading confines the verse's application to their specific historical context and actions, not a universal command.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_breaking_polytheist_tribes_7th_century_medina, payer,
    powerless, immediate, trapped, local).

% Are structurally excluded from this reading's interpretive framework, as it directly contradicts their universalist, offensive jihad ideology. They would vehemently reject this contextual interpretation, as it undermines their theological justification for violence against non-combatants and non-Muslim states.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, jihadist_extremist_groups, excluded,
    organized, generational, identity_locked, global).

% These scholars actively promote and defend this contextual reading, emphasizing historical specificity, linguistic nuance, and the overall Quranic ethical framework. They shape the discourse and provide the intellectual foundation for this interpretation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, islamic_scholars_of_contextual_hermeneutics, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the understanding of Quranic verses related to warfare, ensuring they are interpreted within their historical context and in alignment with broader Islamic ethical principles of peace and justice, thereby fostering peaceful relations between Muslim and non-Muslim communities.
% TRANSFER_FUNCTION: Transfers interpretive authority from literalist, universalist readings to contextual, defensive ones, shifting the theological justification for violence from offensive to defensive, and prioritizing treaty obligations over unprovoked aggression.
% ABSENT_VOICES: Jihadist extremist groups and literalist interpreters are excluded from the mainstream discourse that promotes this reading; they would argue for a universal, abrogating interpretation of 9:5, but their views are marginalized by the scholarly consensus supporting contextualism.
% DISAPPEARANCE_RATIONALE: If this contextual-defensive reading vanished, the vacuum would likely be filled by more literalist or abrogating interpretations, leading to increased theological justification for offensive warfare, undermining treaty obligations, and destabilizing relations between Muslim and non-Muslim states and communities.
% FOUNDING_PROBLEM: The problem of reconciling seemingly aggressive Quranic verses with the broader message of peace and justice in Islam, and preventing their misuse to justify unprovoked aggression or abrogation of treaties.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Islamic scholarly institutions, international relations experts, and interfaith dialogue organizations corroborate that this problem remains live, as extremist groups continue to exploit literalist readings of such verses to justify violence. Their analyses from outside the direct beneficiaries support the ongoing need for contextual hermeneutics.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.2) because this reading primarily coordinates peaceful coexistence and defensive action, with minimal extraction from its beneficiaries. Suppression is also low (0.15) as it relies on scholarly consensus and ethical persuasion rather than coercion. Accessibility collapse is moderate (0.7) because while this reading is widely accepted in mainstream scholarship, literalist interpretations still exist and are accessible to some. Resistance is low (0.1) as this reading is generally welcomed by those seeking peaceful interpretations of Islam.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between those who adhere to this contextual-defensive reading and those who advocate for an abrogating-universalist reading. For the former, the constraint is a rope coordinating peace; for the latter, it is a snare suppressing what they believe is a divine command. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist Muslim-majority states and Muslim minority communities are beneficiaries (d near 0.0) as this reading provides a theological basis for their peaceful existence and integration. The 7th-century treaty-breaking tribes were historical targets (d near 1.0), but this is a historical application. Jihadist extremist groups are excluded, as this reading directly undermines their ideology.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_accuracy,
    'How definitively can the specific historical context of Quran 9:5 be established to limit its application to 7th-century Medinan treaty-breakers?',
    'Further historical and archaeological research into early Islamic period, combined with rigorous philological analysis of classical Arabic texts and early tafsir (exegesis).',
    'Stronger historical evidence would solidify this reading''s claim to contextual specificity, making it harder for universalist interpretations to gain traction. Weaker evidence would open the door to broader applications, increasing extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_accuracy, empirical, 'The empirical certainty of the historical context limiting the verse''s scope.').

omega_variable(
    abrogation_doctrine_validity,
    'Is the doctrine of abrogation (naskh) itself a valid hermeneutical principle in Islamic jurisprudence, and if so, what are its precise rules and limitations?',
    'Intensive theological and jurisprudential debate among leading Islamic scholars, leading to a widely accepted consensus on the conditions and scope of naskh, or its rejection.',
    'If naskh is rejected or severely limited, the ''abrogating_universal'' reading would lose its primary theological tool, strengthening the ''contextual_defensive'' reading. If naskh is affirmed broadly, this reading would face greater challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_doctrine_validity, conceptual, 'The conceptual validity and scope of the doctrine of abrogation in Quranic interpretation.').

omega_variable(
    coexistence_norm_priority,
    'To what extent do the Quran''s general verses promoting peace and coexistence (e.g., 2:256, 5:32) establish an overarching ethical framework that limits the interpretation of verses related to warfare?',
    'Development of a comprehensive, widely accepted ''maqasid al-shariah'' (higher objectives of Islamic law) framework that explicitly prioritizes peace, justice, and human dignity as foundational principles for all legal interpretations.',
    'A strong prioritization of coexistence norms would further entrench the ''contextual_defensive'' reading, making it difficult for any verse to be interpreted as promoting unprovoked aggression. Weak prioritization would allow more literalist readings to gain ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_norm_priority, preference, 'The normative priority given to general peace verses over specific warfare verses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__contextual_defensive, theater_ratio, 10, 0.04).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__contextual_defensive, theater_ratio, 20, 0.03).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__contextual_defensive, theater_ratio, 30, 0.04).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__contextual_defensive, theater_ratio, 40, 0.05).
narrative_ontology:measurement(qura_tr_t50, quran_9_5_scope__contextual_defensive, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__contextual_defensive, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__contextual_defensive, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__contextual_defensive, base_extractiveness, 30, 0.19).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__contextual_defensive, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(qura_be_t50, quran_9_5_scope__contextual_defensive, base_extractiveness, 50, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__contextual_defensive, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__contextual_defensive, suppression_requirement, 20, 0.13).
narrative_ontology:measurement(qura_su_t30, quran_9_5_scope__contextual_defensive, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__contextual_defensive, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(qura_su_t50, quran_9_5_scope__contextual_defensive, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is the 'contextual_defensive' reading of the 'quran_9_5_scope' kernel. It is linked to the 'abrogating_universal' and 'progressive_synthesis' readings, which represent alternative interpretations of the same Quranic verse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
