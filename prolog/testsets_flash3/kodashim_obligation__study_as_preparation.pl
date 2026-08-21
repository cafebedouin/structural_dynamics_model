% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Obligation: Study as Preparation for Messianic Restoration
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint describes the reading of the Kodashim (sacrificial) laws
 *   within Jewish tradition that emphasizes their study as a preparatory act
 *   for a future messianic era when the Temple will be rebuilt and sacrifices
 *   resumed. The laws themselves are currently unperformable, but the
 *   obligation to study them remains binding. This reading sees the study as
 *   instrumental, preserving technical knowledge for a deferred, but certain,
 *   future. It is one reading of the broader 'kodashim_obligation' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.25).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.1).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.25).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Obligation: Study as Preparation for Messianic Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious_studies/jewish_law/textual_preservation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '60e4245a-aedd-4238-909d-f735eb85ca73').
narrative_ontology:cs_kernel_codification('60e4245a-aedd-4238-909d-f735eb85ca73', fixed_text).
narrative_ontology:cs_authority_grounding('60e4245a-aedd-4238-909d-f735eb85ca73', lineage).
narrative_ontology:cs_interpretation_layer_present('60e4245a-aedd-4238-909d-f735eb85ca73').
narrative_ontology:cs_reading_relation('60e4245a-aedd-4238-909d-f735eb85ca73', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('60e4245a-aedd-4238-909d-f735eb85ca73', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('60e4245a-aedd-4238-909d-f735eb85ca73', foundational, sacrificial_law_remains_binding).
narrative_ontology:cs_axiom_status(sacrificial_law_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('60e4245a-aedd-4238-909d-f735eb85ca73', sacrificial_law_remains_binding, theological).
narrative_ontology:cs_axiom('60e4245a-aedd-4238-909d-f735eb85ca73', foundational, messianic_restoration_is_future_event).
narrative_ontology:cs_axiom_status(messianic_restoration_is_future_event, holdable).
narrative_ontology:cs_axiom_grounding('60e4245a-aedd-4238-909d-f735eb85ca73', messianic_restoration_is_future_event, theological).
narrative_ontology:cs_reference_frame('60e4245a-aedd-4238-909d-f735eb85ca73', post_temple_exile_halakha).
narrative_ontology:cs_drift_state('60e4245a-aedd-4238-909d-f735eb85ca73', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('60e4245a-aedd-4238-909d-f735eb85ca73', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, future_priesthood).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_of_jews).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, divine_command_theory).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, messianic_redemption_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the intellectual and spiritual burden of studying complex, unperformable laws, with the understanding that the benefit is deferred to a future generation. Their identity is deeply tied to this ongoing commitment.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_of_jews, payer,
    moderate, biographical, identity_locked, global).

% The ultimate recipient of the preserved knowledge, enabling the restoration of sacrificial practice. This is a conceptual beneficiary, representing the eschatological goal.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, messianic_future).

% Will be able to perform the sacrifices correctly due to the technical knowledge preserved by current study. They are a future, institutional beneficiary.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, future_priesthood, beneficiary,
    institutional, generational, analytical, global).

% Interpret and transmit the obligation to study Kodashim, emphasizing its preparatory role. They maintain the curriculum and the theological justification for this practice.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the historical development and social function of Kodashim study, often viewing it through a lens of cultural preservation or identity formation rather than divine command or messianic preparation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, secular_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transmission of complex, technical religious law, ensuring that knowledge required for future ritual performance is not lost, despite current inability to perform.
% TRANSFER_FUNCTION: Transfers the burden of study and the deferred hope of restoration from the current generation to the messianic future, while preserving technical knowledge for a future priesthood.
% ABSENT_VOICES: Those who view Kodashim study as purely academic or historical, or those who believe the laws are permanently abrogated, are often marginalized in traditional discourse, as their perspectives undermine the preparatory imperative.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim as preparation vanished, a core pillar of traditional Jewish religious life would collapse. The intergenerational project of preserving technical knowledge for the Temple's restoration would cease, fundamentally altering the community's relationship to its past and future.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered sacrificial law unperformable, creating a crisis of religious practice and the risk of losing vital technical knowledge for a future restoration.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and traditional Jewish communities universally attest that the problem of unperformable sacrificial law and the need for its preservation remain live. This is corroborated by the continued centrality of Kodashim study in yeshivot and religious curricula globally, reflecting a widespread communal commitment beyond any single benefiting party.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the primary 'cost' is intellectual and spiritual effort, not material transfer, and the 'victim' (current generation) is also a beneficiary of the spiritual continuity. Suppression is low as adherence is largely voluntary and identity-driven, not coercively enforced. Theater ratio is low because the study is genuinely functional for its stated purpose (preservation of knowledge). The claimed type is 'rope' because it's a coordination mechanism for intergenerational knowledge transfer, with net benefits for the collective over time, despite the deferred nature of the primary benefit.
 *
 * PERSPECTIVAL GAP:
 *   While the 'current_generation_of_jews' bears the immediate cost of study, their deep identity-lock and belief in the messianic future means they perceive the constraint as a net benefit, a 'rope' binding them to a sacred future. An external observer might see the deferred benefit as a form of extraction, but from the internal perspective, it is a necessary and meaningful commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'current_generation_of_jews' are payers, bearing the burden of study, but their identity-locked exit means their directionality is not fully target-like. The 'messianic_future' and 'future_priesthood' are beneficiaries, receiving the preserved knowledge. 'Rabbinic_authorities' are agenda-setters, guiding the practice. The low extractiveness and high accessibility collapse reflect the deeply internalized nature of this obligation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by explicitly linking current unperformable practice to a future, live mandate (messianic restoration). The 'founding_problem' (loss of sacrificial practice) is deemed 'live' because the messianic era has not yet arrived. The study, therefore, retains its function as preparation, preventing it from becoming a 'piton' of inert ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''study_as_preparation'' reading of the Kodashim obligation, or is it better understood as a ''study_as_performance'' or ''study_as_archive'' reading?',
    'Analysis of primary rabbinic texts and communal practice: does the emphasis fall on instrumental knowledge preservation, on the inherent spiritual efficacy of study, or on historical/cultural documentation?',
    'If reclassified as ''study_as_performance'', extractiveness might be lower (study is its own reward); if ''study_as_archive'', extractiveness might be higher (effort without direct religious benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing this reading from sibling interpretations of the Kodashim obligation.').

omega_variable(
    messianic_future_actuality,
    'To what extent is the ''messianic_future'' a concrete, actionable beneficiary versus a conceptual, aspirational one?',
    'Theological and philosophical analysis of messianic belief within the tradition, and its impact on contemporary halakhic (legal) decision-making.',
    'If the messianic future is purely conceptual, the ''beneficiary'' status is weaker, potentially increasing the effective extractiveness on the current generation. If it''s a concrete expectation, the deferred benefit is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_future_actuality, conceptual, 'The nature of the messianic future as a beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_preparation, theater_ratio, 500, 0.05).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_preparation, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(koda_tr_t1950, kodashim_obligation__study_as_preparation, theater_ratio, 1950, 0.05).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_preparation, base_extractiveness, 500, 0.22).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_preparation, base_extractiveness, 1000, 0.23).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.24).
narrative_ontology:measurement(koda_be_t1950, kodashim_obligation__study_as_preparation, base_extractiveness, 1950, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_preparation, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_preparation, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_preparation, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(koda_su_t1950, kodashim_obligation__study_as_preparation, suppression_requirement, 1950, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'kodashim_obligation' kernel. Other readings include 'study_as_performance' (study itself is the ritual) and 'study_as_archive' (study is historical preservation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
