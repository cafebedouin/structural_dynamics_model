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
 *   human_readable: Quran 9:5 Abrogating Universal Jihad Obligation
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the 'abrogating_universal' reading of Quran
 *   9:5, which posits that this verse abrogates all prior peaceful verses,
 *   establishing universal offensive jihad as a standing legal obligation
 *   until polytheists submit or convert. This reading is a core tenet for
 *   certain traditional and expansionist Islamic movements, providing a
 *   theological justification for aggressive military and political action
 *   against non-Muslims. The high extractiveness and suppression reflect the
 *   consequences for those targeted by this doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.92).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.88).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.92).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Quran 9:5 Abrogating Universal Jihad Obligation").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '90d40dfc-65a9-43f6-8ed1-8668055ce176').
narrative_ontology:cs_kernel_codification('90d40dfc-65a9-43f6-8ed1-8668055ce176', fixed_text).
narrative_ontology:cs_authority_grounding('90d40dfc-65a9-43f6-8ed1-8668055ce176', lineage).
narrative_ontology:cs_interpretation_layer_present('90d40dfc-65a9-43f6-8ed1-8668055ce176').
narrative_ontology:cs_reading_relation('90d40dfc-65a9-43f6-8ed1-8668055ce176', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('90d40dfc-65a9-43f6-8ed1-8668055ce176', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('90d40dfc-65a9-43f6-8ed1-8668055ce176', foundational, abrogation_doctrine_valid).
narrative_ontology:cs_axiom_status(abrogation_doctrine_valid, holdable).
narrative_ontology:cs_axiom_grounding('90d40dfc-65a9-43f6-8ed1-8668055ce176', abrogation_doctrine_valid, conventional).
narrative_ontology:cs_axiom('90d40dfc-65a9-43f6-8ed1-8668055ce176', foundational, universal_offensive_jihad_obligation).
narrative_ontology:cs_axiom_status(universal_offensive_jihad_obligation, holdable).
narrative_ontology:cs_axiom_grounding('90d40dfc-65a9-43f6-8ed1-8668055ce176', universal_offensive_jihad_obligation, deontological).
narrative_ontology:cs_reference_frame('90d40dfc-65a9-43f6-8ed1-8668055ce176', classical_abrogation_jurisprudence).
narrative_ontology:cs_drift_state('90d40dfc-65a9-43f6-8ed1-8668055ce176', contemporary_global_jihadist_movements, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('90d40dfc-65a9-43f6-8ed1-8668055ce176', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_political_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, traditional_jurists_and_scholars).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, polytheists_and_non_muslims).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, advocates_of_coexistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These movements interpret Quran 9:5 as a divine mandate for universal offensive jihad, justifying military expansion and the subjugation or conversion of non-Muslims. They gain power, resources, and legitimacy by enforcing this interpretation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_political_movements, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars and jurists who uphold the abrogation doctrine and the universal application of offensive jihad. Their authority and intellectual lineage are reinforced by this interpretation, which they codify and transmit through religious institutions.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, traditional_jurists_and_scholars, beneficiary,
    organized, generational, constrained, global).

% As direct targets of offensive jihad, they face demands for submission, conversion, or subjugation. Their lands, resources, and autonomy are subject to seizure. Their only 'exit' is to abandon their identity or resist violently.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, polytheists_and_non_muslims, payer,
    powerless, immediate, trapped, local).

% Muslim and non-Muslim individuals and groups who promote interfaith dialogue, peaceful coexistence, and contextual interpretations of religious texts. They face suppression, marginalization, and accusations of heresy from proponents of the abrogating_universal reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, advocates_of_coexistence, payer,
    moderate, biographical, constrained, regional).

% Scholars who argue for contextual or progressive interpretations of Quranic verses, rejecting the abrogation of peaceful verses. They are often excluded from mainstream religious discourse and institutions dominated by traditional interpretations.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, liberal_muslim_scholars, excluded,
    moderate, biographical, identity_locked, global).

% Academics and policy experts who study the historical and political impact of this interpretation, analyzing its role in conflicts and state-building without endorsing its theological claims.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, secular_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the Muslim community under a clear, expansionist legal framework for interacting with non-Muslims, providing a coherent basis for military action and political organization.
% TRANSFER_FUNCTION: Transfers sovereignty, resources, and religious allegiance from non-Muslim communities to the Muslim state or movement; transfers authority and legitimacy to those who enforce this doctrine.
% ABSENT_VOICES: Liberal Muslim scholars and interfaith dialogue advocates are structurally excluded from the interpretive process that validates this reading; they would argue for alternative, peaceful interpretations but are marginalized by the dominant discourse.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, it would fundamentally alter Islamic political theology, inter-religious relations, and the justification for many historical and contemporary conflicts. The global landscape of religious and political power would reorganize around new interpretive frameworks.
% FOUNDING_PROBLEM: The early Muslim community faced existential threats and needed clear legal guidance for defense, expansion, and the establishment of a new social and political order in a hostile environment.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within traditional Islamic jurisprudence attest that the problem of establishing Islamic supremacy and dealing with non-believers remains live. Critics (including liberal Muslim scholars and secular historians) argue that the original context is long gone, and the interpretation persists due to political utility rather than ongoing necessity; this critical view is supported by independent historical and sociological analyses from outside the benefiting parties.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.92) because this reading mandates the subjugation or conversion of non-Muslims, leading to significant transfers of sovereignty, wealth, and autonomy. Suppression is also very high (0.88) as it actively suppresses alternative interpretations, peaceful coexistence, and the very existence of non-Muslim political entities. The theater ratio is low (0.15) because this reading is typically presented as a direct, unambiguous legal command, with little performative or symbolic maintenance; its function is direct and coercive. Accessibility collapse is high for non-Muslims, as their options are severely limited to submission or resistance. Resistance is high from those targeted by this doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of expansionist movements, this is a divinely mandated, legitimate framework for establishing justice and order. From the perspective of non-Muslims and advocates of coexistence, it is a highly extractive and suppressive snare. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist political movements and traditional jurists are clear beneficiaries, gaining power, legitimacy, and resources. Polytheists and non-Muslims are direct victims, facing existential threats and forced choices. Advocates of coexistence are also victims, as their efforts are suppressed. Liberal Muslim scholars are excluded, their voices marginalized by this dominant interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_doctrine_validity,
    'Is the doctrine of abrogation (nasikh wa mansukh) itself a valid hermeneutical principle, and does it apply to Quran 9:5 in a way that nullifies all prior peaceful verses?',
    'Deep textual analysis across the entire Quranic corpus, historical-critical scholarship on early Islamic jurisprudence, and comparative theological studies of abrogation''s role in other religious traditions.',
    'If the abrogation doctrine is found to be invalid or misapplied to 9:5, the structural basis for universal offensive jihad collapses, significantly reducing extractiveness and suppression, potentially reclassifying the constraint as a Piton or even dissolving it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_doctrine_validity, conceptual, 'The foundational hermeneutical principle underlying this reading.').

omega_variable(
    contextual_vs_universal_application,
    'Is Quran 9:5 a universal, standing legal obligation, or is its application strictly limited to the specific 7th-century Medinan context of treaty-breaking polytheist tribes?',
    'Historical and philological analysis of the verse''s immediate context, comparison with other Quranic verses on warfare and peace, and examination of early Islamic legal practice beyond the interpretive claims of later jurists.',
    'If the verse is found to be strictly contextual, the victim set shrinks dramatically (from all non-Muslims to specific historical actors), and the authorization for first-strike violence is removed, transforming the constraint''s extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contextual_vs_universal_application, empirical, 'The scope of application for the verse''s command.').

omega_variable(
    political_utility_vs_theological_necessity,
    'To what extent is the persistence and prominence of this reading driven by its political utility for expansionist movements, rather than its inherent theological necessity or textual clarity?',
    'Sociological and political analysis of groups that promote this reading, examining their power structures, resource acquisition, and strategic objectives, alongside their theological arguments.',
    'If political utility is the primary driver, the constraint''s ''naturalness'' (as a divine command) is undermined, suggesting a higher degree of constructedness and instrumentalization, potentially shifting its classification towards a Snare maintained for political gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_utility_vs_theological_necessity, empirical, 'The underlying drivers of the reading''s propagation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__abrogating_universal, theater_ratio, 20, 0.15).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__abrogating_universal, theater_ratio, 40, 0.15).
narrative_ontology:measurement(qura_tr_t60, quran_9_5_scope__abrogating_universal, theater_ratio, 60, 0.15).
narrative_ontology:measurement(qura_tr_t80, quran_9_5_scope__abrogating_universal, theater_ratio, 80, 0.15).
narrative_ontology:measurement(qura_tr_t100, quran_9_5_scope__abrogating_universal, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__abrogating_universal, base_extractiveness, 20, 0.88).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__abrogating_universal, base_extractiveness, 40, 0.9).
narrative_ontology:measurement(qura_be_t60, quran_9_5_scope__abrogating_universal, base_extractiveness, 60, 0.91).
narrative_ontology:measurement(qura_be_t80, quran_9_5_scope__abrogating_universal, base_extractiveness, 80, 0.92).
narrative_ontology:measurement(qura_be_t100, quran_9_5_scope__abrogating_universal, base_extractiveness, 100, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__abrogating_universal, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__abrogating_universal, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(qura_su_t60, quran_9_5_scope__abrogating_universal, suppression_requirement, 60, 0.87).
narrative_ontology:measurement(qura_su_t80, quran_9_5_scope__abrogating_universal, suppression_requirement, 80, 0.88).
narrative_ontology:measurement(qura_su_t100, quran_9_5_scope__abrogating_universal, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_state_legitimacy).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, inter_religious_relations_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_9_5_scope' kernel. This 'abrogating_universal' reading directly contradicts the 'contextual_defensive' and 'progressive_synthesis' readings, leading to distinct structural outcomes for beneficiaries and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
