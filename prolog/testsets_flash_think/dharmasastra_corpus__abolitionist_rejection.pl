% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus (Abolitionist Rejection Reading)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint story represents the 'abolitionist rejection' reading of
 *   the Dharmasastra corpus. From this perspective, the Dharmasastra texts
 *   and the social system they justify (the caste system) are fundamentally
 *   oppressive and lack any legitimate authority. The reading asserts that
 *   the entire framework must be wholly abandoned, as it serves primarily as
 *   a mechanism for extraction and suppression, particularly against lower
 *   castes, Dalits, and women. The metrics reflect this view, showing
 *   extremely high extractiveness and suppression, with minimal performative
 *   'theater' masking its true function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.92).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.95).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.92).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus (Abolitionist Rejection Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, 'eec0d090-7aa6-44f0-bd8f-ef103c4412c4').
narrative_ontology:cs_kernel_codification('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', fixed_text).
narrative_ontology:cs_authority_grounding('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', extraction).
narrative_ontology:cs_interpretation_layer_present('eec0d090-7aa6-44f0-bd8f-ef103c4412c4').
narrative_ontology:cs_reading_relation('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', foundational, caste_system_inherently_unjust).
narrative_ontology:cs_axiom_status(caste_system_inherently_unjust, holdable).
narrative_ontology:cs_axiom_grounding('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', caste_system_inherently_unjust, deontological).
narrative_ontology:cs_axiom('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', foundational, dharmasastra_texts_lack_legitimate_authority).
narrative_ontology:cs_axiom_status(dharmasastra_texts_lack_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', dharmasastra_texts_lack_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', egalitarian_justice_framework).
narrative_ontology:cs_drift_state('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', contemporary_abolitionist_movement, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('eec0d090-7aa6-44f0-bd8f-ef103c4412c4', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, upper_castes).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, dharmasastra_interpreters).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, lower_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically and presently benefit from the social, economic, and ritual hierarchy established and justified by Dharmasastra. They actively maintain the system through social norms, religious practice, and sometimes political influence. Exit from this privileged position is possible but entails loss of status and power.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, upper_castes, agenda_setter,
    institutional, generational, constrained, global).

% The traditional custodians and interpreters of the Dharmasastra texts. Their authority and social standing are directly derived from the legitimacy of the corpus. Abandoning the framework would mean the dissolution of their professional and social identity.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dharmasastra_interpreters, agenda_setter,
    institutional, generational, identity_locked, global).

% Bear the social, economic, and ritual burdens imposed by the caste hierarchy. Their access to resources, education, and social mobility is severely restricted by the system. Exit is extremely difficult, often involving migration or conversion, but the social stigma and structural disadvantages often persist.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, lower_castes, payer,
    powerless, generational, trapped, global).

% Experience the most extreme forms of discrimination and violence under the caste system, being considered 'outside' the varna hierarchy. Their lives are fundamentally shaped by the oppressive structures justified by Dharmasastra. Exit is virtually impossible within the traditional social framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalits, payer,
    powerless, generational, trapped, global).

% Across all castes, women face specific restrictions and subordinate roles prescribed by Dharmasastra, impacting their autonomy, property rights, and social participation. While not 'trapped' in the same way as Dalits, their options are severely constrained by patriarchal norms.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women, payer,
    powerless, generational, constrained, global).

% Actively campaign for the complete dismantling of the caste system and the rejection of Dharmasastra's authority. They bear the costs of resistance and social ostracization but are not directly bound by the system's benefits or strictures in the same way as other stakeholders.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, abolitionist_activists, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__abolitionist_rejection, upper_castes).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading, the constraint coordinates a rigid social hierarchy, assigning roles and duties based on birth, thereby maintaining social order and stability for the benefit of upper castes.
% TRANSFER_FUNCTION: Transfers social status, economic resources, ritual purity, and political power from lower castes and women to upper castes and male members, enforced through religious injunctions and social norms.
% ABSENT_VOICES: The voices of those historically and systematically silenced by the caste system, particularly Dalits, lower castes, and women, whose perspectives were excluded from the formation and interpretation of Dharmasastra. Their objections would fundamentally challenge the legitimacy of the entire framework.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra corpus and its associated authority vanished overnight, the social, economic, and political structures of societies influenced by it would undergo a profound and rapid reorganization. The caste system, lacking its primary textual and normative justification, would face immense pressure to collapse, leading to a reordering of power, status, and resource distribution.
% FOUNDING_PROBLEM: The problem of establishing and maintaining a stable, hierarchical social order that assigns specific roles and duties based on birth, ensuring social control and the perpetuation of power structures.
% FOUNDING_PROBLEM_CORROBORATION: No legitimate corroboration exists from outside the benefiting parties. The claim that this system solved a 'problem' of social disorder is a self-serving justification by those who benefited from the hierarchy; independent historical and sociological analyses consistently highlight its oppressive nature rather than its problem-solving function for society as a whole.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.92) and suppression (0.95) reflect the abolitionist view that the Dharmasastra-justified caste system is a deeply entrenched, coercive structure that systematically extracts resources, labor, and dignity from marginalized groups. The low theater ratio (0.10) indicates that the oppressive function is direct and real, not merely performative or symbolic. Accessibility collapse is high (0.90) because the system is pervasive and offers few genuine alternatives or exits for those born into its lower strata. Resistance is high (0.70) due to centuries of struggle against the caste system, yet the system's persistence demonstrates its formidable suppressive power.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from both orthodox and reformist interpretations. While orthodox literalists would see the system as divinely ordained and beneficial for social order, and reformists might seek to extract an 'ethical core,' the abolitionist perspective sees no redeemable coordination function or legitimate authority, only pure extraction and oppression. The engine's classification will highlight this stark divergence from other readings of the same kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Upper castes and Dharmasastra interpreters are clear beneficiaries and agenda-setters, deriving immense social, economic, and ritual power from the system. Lower castes, Dalits, and women are the primary targets and victims, bearing the full weight of its extractive and suppressive mechanisms. Abolitionist activists, while actively resisting, are positioned as observers who bear the costs of challenging the system rather than benefiting from or being directly trapped within its core structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_founding_problem,
    'Was the ''founding problem'' (establishing social order through rigid hierarchy) ever a legitimate problem to solve, or was it always a justification for power consolidation?',
    'Historical and sociological analysis of pre-Dharmasastra social structures and the emergence of caste, focusing on power dynamics rather than functionalist explanations.',
    'If the founding problem is deemed illegitimate, it further strengthens the Snare classification by removing any pretense of genuine coordination, reinforcing the view that the system was always purely extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_founding_problem, conceptual, 'Whether the system''s stated purpose was ever legitimate.').

omega_variable(
    internalized_suppression_extent,
    'To what extent is the observed suppression internalized by individuals within the caste system, rather than purely structural?',
    'Longitudinal studies of individuals who have exited or resisted the system, examining the persistence of self-limiting beliefs or social conditioning after structural barriers are reduced.',
    'If internalized suppression is significant, the effective suppression is higher than structural measures suggest, making the constraint more resilient to external challenges and requiring deeper, cultural interventions for dismantling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_extent, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    cost_of_dismantling_vs_reform,
    'What are the true social and economic costs of a complete ''abolitionist'' dismantling of the caste system and Dharmasastra''s authority, compared to a ''reformist'' approach?',
    'Comparative analysis of societies that have undergone radical social restructuring versus those pursuing gradual reform, assessing long-term stability, equity, and human development outcomes.',
    'If dismantling proves less costly or more effective in achieving equity, it strengthens the abolitionist argument; if reform is shown to be more feasible with comparable outcomes, it challenges the ''wholly abandoned'' stance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_of_dismantling_vs_reform, preference, 'Feasibility and impact of abolition vs. reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1800, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(dhar_tr_t1850, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1850, 0.06).
narrative_ontology:measurement(dhar_tr_t1900, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1900, 0.07).
narrative_ontology:measurement(dhar_tr_t1950, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(dhar_tr_t2000, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(dhar_tr_t2024, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1800, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1800, 0.95).
narrative_ontology:measurement(dhar_be_t1850, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1850, 0.94).
narrative_ontology:measurement(dhar_be_t1900, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1900, 0.93).
narrative_ontology:measurement(dhar_be_t1950, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1950, 0.92).
narrative_ontology:measurement(dhar_be_t2000, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2000, 0.92).
narrative_ontology:measurement(dhar_be_t2024, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1800, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1800, 0.98).
narrative_ontology:measurement(dhar_su_t1850, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1850, 0.97).
narrative_ontology:measurement(dhar_su_t1900, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1900, 0.96).
narrative_ontology:measurement(dhar_su_t1950, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1950, 0.95).
narrative_ontology:measurement(dhar_su_t2000, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(dhar_su_t2024, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dharmasastra_corpus' kernel, alongside 'orthodox_literalist' and 'reformist_contextual'. Each reading instantiates a distinct constraint with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
