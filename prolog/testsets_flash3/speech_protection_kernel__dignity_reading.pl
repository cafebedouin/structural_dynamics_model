% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Dignity (Dignity Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'dignity reading' of speech protection,
 *   where the right to free expression is conditional on not contributing to
 *   the structural subordination of target groups. It recognizes group harm
 *   as distinct from individual harm and seeks to ensure equal dignity. This
 *   reading is a specific interpretation of the broader
 *   'speech_protection_kernel' and stands in tension with more absolutist or
 *   harm-threshold-focused interpretations. The metrics reflect the active
 *   enforcement required to uphold this conditional protection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.65).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.7).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Dignity (Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '4a698d4c-d773-45d3-92f0-f89cc6e2933f').
narrative_ontology:cs_kernel_codification('4a698d4c-d773-45d3-92f0-f89cc6e2933f', formalized).
narrative_ontology:cs_authority_grounding('4a698d4c-d773-45d3-92f0-f89cc6e2933f', lineage).
narrative_ontology:cs_interpretation_layer_present('4a698d4c-d773-45d3-92f0-f89cc6e2933f').
narrative_ontology:cs_reading_relation('4a698d4c-d773-45d3-92f0-f89cc6e2933f', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a698d4c-d773-45d3-92f0-f89cc6e2933f', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a698d4c-d773-45d3-92f0-f89cc6e2933f', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a698d4c-d773-45d3-92f0-f89cc6e2933f', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('4a698d4c-d773-45d3-92f0-f89cc6e2933f', foundational, equal_dignity_as_precondition_for_speech).
narrative_ontology:cs_axiom_status(equal_dignity_as_precondition_for_speech, holdable).
narrative_ontology:cs_axiom_grounding('4a698d4c-d773-45d3-92f0-f89cc6e2933f', equal_dignity_as_precondition_for_speech, deontological).
narrative_ontology:cs_axiom('4a698d4c-d773-45d3-92f0-f89cc6e2933f', foundational, group_based_structural_harm_is_distinct_from_individual_offense).
narrative_ontology:cs_axiom_status(group_based_structural_harm_is_distinct_from_individual_offense, holdable).
narrative_ontology:cs_axiom_grounding('4a698d4c-d773-45d3-92f0-f89cc6e2933f', group_based_structural_harm_is_distinct_from_individual_offense, empirically_contingent).
narrative_ontology:cs_reference_frame('4a698d4c-d773-45d3-92f0-f89cc6e2933f', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('4a698d4c-d773-45d3-92f0-f89cc6e2933f', contemporary_digital_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4a698d4c-d773-45d3-92f0-f89cc6e2933f', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, marginalized_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, advocates_for_equality).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speakers_of_hate_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, groups_engaging_in_subordinating_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal frameworks that protect their equal dignity by restricting speech that structurally subordinates them. Their ability to participate fully in society is enhanced by the reduction of hate speech and group libel. Exit from their identity is not an option.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, marginalized_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Actively champion and enforce the dignity-based interpretation of speech rights, seeking to expand protections against subordinating speech. They work through legal and political channels to shape jurisprudence and public understanding.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, advocates_for_equality, agenda_setter,
    organized, generational, constrained, national).

% Bear the cost of having their speech restricted or unprotected when it is deemed to contribute to structural subordination. They may face legal penalties or social censure. Their 'exit' is to modify their speech or face consequences.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_of_hate_speech, payer,
    moderate, immediate, constrained, local).

% Represent organized efforts to use speech that subordinates target groups. They experience the constraint as a limitation on their expressive freedom and may actively resist its enforcement, often claiming 'free speech' absolutism.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, groups_engaging_in_subordinating_speech, payer,
    organized, biographical, constrained, national).

% Are tasked with interpreting and applying speech laws in a manner consistent with the dignity reading. They adjudicate cases, balance competing rights, and develop legal precedents that define the boundaries of protected and unprotected speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Are largely excluded from the framing of speech rights within the dignity reading, as their core premise (near-categorical protection) is incompatible. They would argue that any content-based restriction, even for dignity, is a dangerous precedent.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolutist_free_speech_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interaction and public discourse around a norm of equal dignity, ensuring that speech does not actively undermine the status and participation of marginalized groups. It aims to create a more inclusive public sphere.
% TRANSFER_FUNCTION: Transfers expressive freedom from those whose speech subordinates others to marginalized groups, who gain greater protection from harm and enhanced capacity for participation. It also transfers enforcement costs to the state and legal system.
% ABSENT_VOICES: Those who hold an absolutist view of free speech are structurally marginalized in this framework; they would argue that the dignity reading creates a 'heckler's veto' or chills legitimate expression, but their arguments are often dismissed as undermining the very dignity the reading seeks to protect.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, speech that structurally subordinates marginalized groups would proliferate unchecked, leading to increased social fragmentation, reduced participation by target groups, and a more hostile public sphere. Legal systems would lose a key tool for addressing group-based harm.
% FOUNDING_PROBLEM: The historical and ongoing problem of speech being used to perpetuate structural inequality and deny equal dignity to marginalized groups, leading to real-world harm and exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil rights organizations, and international human rights bodies consistently corroborate the ongoing nature of this problem, citing empirical evidence of the impact of hate speech and subordinating discourse on target communities. This corroboration comes from outside the immediate beneficiaries of the constraint.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-high, as it restricts certain forms of speech, extracting expressive freedom from some to protect the dignity of others. Suppression (0.70) is also high, reflecting the active legal and social enforcement mechanisms needed to counter subordinating speech. The theater ratio (0.20) is relatively low, as the efforts to protect dignity are generally genuine, though some performative aspects may exist in public discourse. Accessibility collapse (0.40) is moderate, as alternative forms of expression exist, but the specific avenue of subordinating speech is curtailed. Resistance (0.55) is significant, as this reading is actively contested by those who prioritize broader speech protections.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries experience this as a necessary coordination mechanism for a just society, while payers experience it as an extractive suppression of their rights. The engine's per-seat classification will reflect this divergence, showing a positive classification for beneficiaries and a negative one for payers, even though the constraint's overall claimed type is 'tangled_rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups and advocates for equality are beneficiaries, as the constraint directly serves their interests in equal dignity and participation. Speakers of hate speech and groups engaging in subordinating speech are payers, as their expressive freedom is curtailed. Courts and regulators act as agenda-setters, interpreting and enforcing the constraint. Absolutist free speech advocates are excluded, as their core tenets are incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_subordination_definition,
    'How is ''structural subordination'' precisely defined and measured in a way that avoids subjective application or chilling legitimate, critical speech?',
    'Development of clear, judicially reviewable criteria for identifying speech that contributes to structural subordination, potentially drawing on social science evidence of group-based harm and historical context.',
    'A clear definition would reduce the perceived extractiveness and suppression for speakers, making the constraint more ''rope-like'' by increasing predictability. An ambiguous definition would increase perceived suppression and resistance, pushing it towards ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_subordination_definition, conceptual, 'Ambiguity in defining the core concept of ''structural subordination''.').

omega_variable(
    balancing_dignity_vs_expression,
    'What is the optimal balance point between protecting equal dignity and ensuring robust, even offensive, expression in a pluralistic society?',
    'Ongoing jurisprudential development, public deliberation, and empirical studies on the societal impacts of different speech regimes. This is likely a perpetual negotiation rather than a definitive resolution.',
    'A societal consensus on this balance would stabilize the constraint''s operation and reduce resistance. Persistent disagreement would maintain high resistance and contestation, potentially leading to cyclical enforcement and challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balancing_dignity_vs_expression, preference, 'The inherent tension between dignity protection and expressive freedom.').

omega_variable(
    empirical_impact_of_subordinating_speech,
    'What is the demonstrable empirical link between specific categories of subordinating speech and actual, measurable harm to marginalized groups'' dignity and participation?',
    'Longitudinal social science research, psychological studies, and ethnographic analysis to establish causal pathways and quantify the impact of different forms of speech.',
    'Strong empirical evidence would bolster the legitimacy of the dignity reading, reducing resistance and potentially lowering the perceived ''suppression'' as the justification becomes clearer. Weak or contested evidence would fuel claims of arbitrary censorship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_impact_of_subordinating_speech, empirical, 'The empirical basis for claims of harm from subordinating speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__dignity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__dignity_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__dignity_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__dignity_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__dignity_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__dignity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__dignity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__dignity_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__dignity_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__dignity_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__dignity_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__dignity_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__dignity_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__dignity_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__dignity_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
