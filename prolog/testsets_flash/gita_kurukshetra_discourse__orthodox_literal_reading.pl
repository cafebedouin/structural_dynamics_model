% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Bhagavad Gita Kurukshetra Discourse: Orthodox Literal Reading
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.85).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.9).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, snare).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Bhagavad Gita Kurukshetra Discourse: Orthodox Literal Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, 'e39ef46d-f353-4a27-903b-abe1c5850c54').
narrative_ontology:cs_kernel_codification('e39ef46d-f353-4a27-903b-abe1c5850c54', fixed_text).
narrative_ontology:cs_authority_grounding('e39ef46d-f353-4a27-903b-abe1c5850c54', lineage).
narrative_ontology:cs_interpretation_layer_present('e39ef46d-f353-4a27-903b-abe1c5850c54').
narrative_ontology:cs_reading_relation('e39ef46d-f353-4a27-903b-abe1c5850c54', gita_kurukshetra_discourse__gandhian_allegorical_reading, forecloses).
narrative_ontology:cs_reading_relation('e39ef46d-f353-4a27-903b-abe1c5850c54', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('e39ef46d-f353-4a27-903b-abe1c5850c54', foundational, caste_dharma_is_divine_mandate).
narrative_ontology:cs_axiom_status(caste_dharma_is_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e39ef46d-f353-4a27-903b-abe1c5850c54', caste_dharma_is_divine_mandate, theological).
narrative_ontology:cs_axiom('e39ef46d-f353-4a27-903b-abe1c5850c54', foundational, righteous_violence_is_permissible_for_kshatriya).
narrative_ontology:cs_axiom_status(righteous_violence_is_permissible_for_kshatriya, holdable).
narrative_ontology:cs_axiom_grounding('e39ef46d-f353-4a27-903b-abe1c5850c54', righteous_violence_is_permissible_for_kshatriya, deontological).
narrative_ontology:cs_reference_frame('e39ef46d-f353-4a27-903b-abe1c5850c54', vedic_social_order_and_dharma).
narrative_ontology:cs_drift_state('e39ef46d-f353-4a27-903b-abe1c5850c54', contemporary_globalized_ethics, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e39ef46d-f353-4a27-903b-abe1c5850c54', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, divinely_ordained_social_order).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, those_killed_in_dharmic_war).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, dissenting_interpretations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As traditional interpreters of sacred texts, they define and propagate the orthodox literal reading, maintaining their interpretive monopoly and the social order it justifies. They benefit from the stability and authority derived from this reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priesthood, agenda_setter,
    institutional, generational, identity_locked, regional).

% Benefits from the legitimation of their duty to engage in 'righteous' violence and warfare, reinforcing their social status and power. Their dharma is explicitly tied to this interpretation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, biographical, constrained, regional).

% The abstract concept of a caste-based social hierarchy, which is presented as divinely sanctioned and immutable by this reading. Its persistence is a core outcome of the constraint.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, divinely_ordained_social_order, beneficiary,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_non_agent(gita_kurukshetra_discourse__orthodox_literal_reading, divinely_ordained_social_order).

% Are victims of the rigid caste hierarchy, bound by prescribed duties and denied social mobility, with their suffering justified as part of their dharma. Exit options are severely limited by social and religious enforcement.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes, payer,
    powerless, generational, trapped, local).

% Bear the ultimate cost of 'righteous' violence, their lives sacrificed in conflicts legitimized by this textual interpretation. They have no agency in the decision to wage war.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, those_killed_in_dharmic_war, payer,
    powerless, immediate, trapped, local).

% Alternative readings (e.g., allegorical, universalist) are actively suppressed or marginalized by the orthodox literal reading, which claims exclusive interpretive authority. Their voices are not given equal standing in the discourse.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, dissenting_interpretations, excluded,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_non_agent(gita_kurukshetra_discourse__orthodox_literal_reading, dissenting_interpretations).

% Analyze the ethical implications of this reading, often critiquing its justification of violence and social inequality from a universalist human rights perspective. They do not participate in the internal religious discourse but comment on its societal impact.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, secular_ethicists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, divinely sanctioned framework for social order and individual duty (dharma) within a hierarchical caste system, ensuring social stability and the performance of ritual and martial roles.
% TRANSFER_FUNCTION: Transfers social authority, interpretive power, and material benefits to the Brahmin and Kshatriya classes, while extracting obedience, labor, and lives from lower castes and those targeted by 'righteous' violence.
% ABSENT_VOICES: The voices of lower castes, those who suffer from caste-based discrimination, and proponents of non-violent or egalitarian interpretations are systematically marginalized or silenced within the orthodox discourse. They would challenge the divine sanction of hierarchy and violence.
% DISAPPEARANCE_RATIONALE: If this orthodox literal reading vanished, the foundational justification for caste-based duties and 'righteous' violence would collapse. This would lead to significant social upheaval, challenges to traditional authority structures, and a re-evaluation of ethical norms within dharmic traditions, fundamentally reorganizing social and religious life.
% FOUNDING_PROBLEM: The problem of maintaining social order, individual duty (dharma), and the legitimacy of warfare in ancient Indian society, particularly in the context of a warrior's moral dilemma regarding violence against kin.
% FOUNDING_PROBLEM_CORROBORATION: The Brahmin priesthood and traditional scholars attest that the problems of maintaining dharma and social order are still live, citing contemporary challenges to traditional values. However, secular ethicists and social reformers outside the benefiting parties contest the legitimacy of the 'founding problem' as framed, arguing it perpetuates inequality rather than solving a universal human dilemma.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the caste-based duty and righteous violence truly a divine mandate, or a social construct maintained by powerful groups for their benefit?',
    'Comparative theological analysis across diverse dharmic traditions, historical-critical textual scholarship, and sociological studies of power dynamics in religious institutions.',
    'If a social construct, the constraint''s extractiveness and suppression are purely human-imposed, strengthening its classification as a Snare. If genuinely divine, it would lean towards a Mountain, but the presence of beneficiaries would still trigger False Summit detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, conceptual, 'Ambiguity between divine command and human-authored social control.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social/religious enforcement) or internalized (belief in one''s dharma/karma)?',
    'Post-exit suppression trajectory: if individuals continue to adhere to caste duties even after external enforcement is removed, it suggests internalized suppression. Sociological studies on agency and belief systems.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would make exit even more difficult and amplify the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in caste adherence.').

omega_variable(
    interpretive_monopoly_legitimacy,
    'Is the Brahmin priesthood''s interpretive monopoly a legitimate function of preserving tradition, or a mechanism for controlling narrative and power?',
    'Historical analysis of interpretive shifts, examination of challenges to authority, and comparison with other religious traditions'' interpretive structures.',
    'If primarily a power mechanism, it reinforces the Snare classification by highlighting the active suppression of alternative readings. If a legitimate preservation function, it might slightly reduce the perceived extractiveness of the ''agenda_setter'' role, but not the overall constraint type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_legitimacy, conceptual, 'Legitimacy of interpretive authority in maintaining the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gita_tr_t25, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(gita_tr_t50, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(gita_tr_t75, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(gita_be_t25, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement(gita_be_t50, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(gita_be_t75, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 75, 0.84).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(gita_su_t25, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement(gita_su_t50, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 50, 0.9).
narrative_ontology:measurement(gita_su_t75, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 75, 0.89).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
