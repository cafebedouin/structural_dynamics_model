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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus: Abolitionist Rejection Reading
 *   domain: religious_law/social_hierarchy
 *
 * SUMMARY:
 *   This constraint represents the 'abolitionist rejection' reading of the
 *   Dharmasastra corpus, which asserts that the texts and the caste system
 *   they underpin are fundamentally oppressive and must be abandoned. It is
 *   one of three readings of the Dharmasastra corpus kernel, with sibling
 *   readings being the 'orthodox literalist' (eternal, revealed truth) and
 *   'reformist contextual' (ethical core separable from caste)
 *   interpretations. This reading focuses on the inherent injustice and calls
 *   for a complete dismantling of the framework.
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
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus: Abolitionist Rejection Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/social_hierarchy").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '4391e9c6-55ca-4155-a64d-d0acda5f3619').
narrative_ontology:cs_kernel_codification('4391e9c6-55ca-4155-a64d-d0acda5f3619', fixed_text).
narrative_ontology:cs_authority_grounding('4391e9c6-55ca-4155-a64d-d0acda5f3619', extraction).
narrative_ontology:cs_interpretation_layer_present('4391e9c6-55ca-4155-a64d-d0acda5f3619').
narrative_ontology:cs_reading_relation('4391e9c6-55ca-4155-a64d-d0acda5f3619', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('4391e9c6-55ca-4155-a64d-d0acda5f3619', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('4391e9c6-55ca-4155-a64d-d0acda5f3619', foundational, inherent_oppression_of_caste_system).
narrative_ontology:cs_axiom_status(inherent_oppression_of_caste_system, holdable).
narrative_ontology:cs_axiom_grounding('4391e9c6-55ca-4155-a64d-d0acda5f3619', inherent_oppression_of_caste_system, deontological).
narrative_ontology:cs_axiom('4391e9c6-55ca-4155-a64d-d0acda5f3619', foundational, textual_authority_illegitimate).
narrative_ontology:cs_axiom_status(textual_authority_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('4391e9c6-55ca-4155-a64d-d0acda5f3619', textual_authority_illegitimate, deontological).
narrative_ontology:cs_reference_frame('4391e9c6-55ca-4155-a64d-d0acda5f3619', post_colonial_emancipatory_justice).
narrative_ontology:cs_drift_state('4391e9c6-55ca-4155-a64d-d0acda5f3619', contemporary_global_human_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4391e9c6-55ca-4155-a64d-d0acda5f3619', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, dominant_caste_groups).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, religious_authorities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, subordinate_caste_groups).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, abolitionist_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups historically and currently benefit from the social, economic, and ritual hierarchy established by Dharmasastra. They actively uphold and enforce its tenets, often viewing their position as divinely ordained. Their identity is deeply intertwined with the system.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dominant_caste_groups, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, dominant_caste_groups, beneficiary).

% Custodians and interpreters of Dharmasastra, these authorities derive their power and legitimacy from the textual tradition. They actively resist any reinterpretation or abandonment that would undermine their authority or the hierarchical social order.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, religious_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, religious_authorities, beneficiary).

% These groups are subjected to social, economic, and ritual discrimination and exploitation based on their birth within the caste system. Their access to resources, education, and social mobility is severely constrained by Dharmasastra's prescriptions.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, subordinate_caste_groups, payer,
    powerless, generational, trapped, global).

% Historically considered 'untouchable,' Dalits face the most extreme forms of discrimination, violence, and exclusion under the caste system. Their lives are profoundly shaped by the oppressive structures derived from Dharmasastra, with virtually no exit options within the traditional framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalits, payer,
    powerless, generational, trapped, global).

% Across all caste groups, women are subjected to patriarchal norms and restrictions on their autonomy, property rights, and social roles as prescribed by Dharmasastra. Their status is often defined in relation to male family members, limiting their individual agency.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women, payer,
    powerless, generational, trapped, global).

% These activists actively challenge the legitimacy and authority of Dharmasastra and the caste system. They bear the costs of resistance, including social ostracization, legal battles, and personal risk, in their efforts to dismantle the oppressive structures.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, abolitionist_activists, observer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, abolitionist_activists, payer).

% In many modern states, secular legal systems formally outlaw caste discrimination and uphold principles of equality. However, their ability to fully displace the social and religious authority of Dharmasastra is often constrained by cultural inertia and political will.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, secular_legal_systems, observer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, secular_legal_systems, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The system claims to establish a divinely ordained social order, maintain ritual purity, and ensure cosmic balance by assigning hierarchical roles and duties (dharma) to individuals based on birth.
% TRANSFER_FUNCTION: Transfers status, ritual purity, economic resources, and labor from subordinate caste groups, Dalits, and women to dominant caste groups and religious authorities, enforced through social ostracization, religious sanctions, and economic dependency.
% ABSENT_VOICES: Historically, the voices of subordinate caste groups, Dalits, and women were systematically excluded from interpreting or challenging Dharmasastra. Even today, their lived experiences and critiques are often marginalized or dismissed by traditional authorities, who claim to speak for the entire tradition.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra's authority and the caste system it underpins vanished overnight, the entire social, economic, and religious fabric of societies where it holds sway would be fundamentally reordered. Power structures would collapse, new forms of social organization based on equality would emerge, and the lives of millions would be transformed.
% FOUNDING_PROBLEM: To establish a stable, divinely sanctioned social order, prevent social chaos, and ensure the spiritual well-being of society by defining clear, hierarchical roles and duties for all individuals.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist scholars, human rights organizations, and social justice movements attest that the 'founding problem' was a pretext for establishing and maintaining a deeply oppressive hierarchy. They argue that the system's 'solutions' are now the primary source of immense suffering and injustice, rendering the original mandate illegitimate. Traditional religious authorities, however, maintain that the founding problem of social and cosmic order is still live and requires the system's preservation.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.92) and suppression (0.95) are extremely high, reflecting the abolitionist view that the caste system is a deeply entrenched, coercive mechanism for exploitation. The theater ratio is low (0.10) because the oppression is direct and functional, not merely performative; the system actively enforces its hierarchy. Accessibility collapse is high (0.90) due to the pervasive nature of caste, making genuine alternatives almost impossible for those trapped within it. Resistance is high (0.75) due to centuries of anti-caste movements, but the system's inertia and power prevent easy dismantling.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dominant caste groups and religious authorities, the Dharmasastra provides a legitimate, divinely ordained social order. From the abolitionist perspective, and that of the victims, it is a pure snare, a mechanism of profound injustice and exploitation. The engine's computation of per-seat classifications will highlight this stark divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant caste groups and religious authorities are clear beneficiaries and agenda-setters, deriving immense power and resources from the system. Subordinate caste groups, Dalits, and women are the primary victims, bearing the full weight of its extraction and suppression. Abolitionist activists, while observers, also bear significant costs in their struggle against the system. Secular legal systems are external observers with some capacity to act, but their influence is often limited by the deep-seated nature of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_oppression_vs_reinterpretation,
    'Is the Dharmasastra corpus inherently and irredeemably oppressive, or can its ethical core be separated from its historical, hierarchical prescriptions through reinterpretation?',
    'Analysis of successful, widespread reform movements that demonstrably dismantle caste-based discrimination while retaining textual authority, or sustained failure of such movements.',
    'If inherently oppressive, the abolitionist rejection is structurally necessary. If reinterpretation is viable, the constraint might be reclassified as a Tangled Rope or Piton, with the possibility of reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inherent_oppression_vs_reinterpretation, conceptual, 'Whether the texts permit non-oppressive readings or are fundamentally flawed.').

omega_variable(
    contemporary_influence_vs_secular_law,
    'What is the true extent of Dharmasastra''s contemporary influence on social practice and individual lives, particularly in regions with secular anti-discrimination laws?',
    'Empirical sociological studies, legal enforcement data, and ethnographic research on the persistence of caste practices despite legal prohibitions.',
    'If influence remains high despite secular law, the constraint''s effective suppression and extractiveness are amplified. If influence is negligible, the constraint may be a Piton, maintained by inertia rather than active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_influence_vs_secular_law, empirical, 'The gap between formal legal status and lived social reality.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (economic dependency, social ostracization) or internalized (belief in one''s caste dharma, fatalism)?',
    'Post-exit suppression trajectory: if individuals continue to self-regulate according to caste norms after structural barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit, making true liberation harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the caste system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 20, 0.11).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 40, 0.1).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 60, 0.1).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 80, 0.1).
narrative_ontology:measurement(dhar_tr_t100, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 20, 0.91).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 40, 0.92).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 60, 0.92).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 80, 0.92).
narrative_ontology:measurement(dhar_be_t100, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 100, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.93).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 20, 0.94).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 40, 0.95).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 60, 0.95).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 80, 0.95).
narrative_ontology:measurement(dhar_su_t100, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, caste_based_discrimination_laws).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, religious_freedom_interpretations).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Dharmasastra corpus kernel, each representing a distinct structural claim about its nature and legitimacy. This reading directly challenges the premises of the orthodox literalist and reformist contextual readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
