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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Obligation: Study as Preparation for Messianic Restoration
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint describes the obligation within Jewish law to study the
 *   Kodashim (sacrificial laws) not for immediate performance, but as a
 *   preparatory act for a future messianic era when the Temple will be
 *   rebuilt and sacrifices restored. The study is seen as a vital,
 *   instrumental act of preservation and readiness. This is one reading of
 *   the broader 'Kodashim obligation' kernel, emphasizing the deferred,
 *   instrumental nature of the study.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.15).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.05).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.15).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Obligation: Study as Preparation for Messianic Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious_studies/jewish_law/textual_preservation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6').
narrative_ontology:cs_kernel_codification('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', fixed_text).
narrative_ontology:cs_authority_grounding('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', lineage).
narrative_ontology:cs_interpretation_layer_present('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6').
narrative_ontology:cs_reading_relation('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', kodashim_obligation__study_as_archive, influences).
narrative_ontology:cs_axiom('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', foundational, sacrificial_law_binding_but_unperformable).
narrative_ontology:cs_axiom_status(sacrificial_law_binding_but_unperformable, holdable).
narrative_ontology:cs_axiom_grounding('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', sacrificial_law_binding_but_unperformable, deontological).
narrative_ontology:cs_axiom('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', foundational, study_as_instrumental_preparation).
narrative_ontology:cs_axiom_status(study_as_instrumental_preparation, holdable).
narrative_ontology:cs_axiom_grounding('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', study_as_instrumental_preparation, instrumental).
narrative_ontology:cs_reference_frame('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3d3bdb5c-6a48-4033-a642-b5c08d3aa0a6', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, future_priesthood).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_of_jews).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, divine_covenant_permanence).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, messianic_redemption_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the intellectual and spiritual burden of studying complex, currently unperformable sacrificial laws, with the understanding that this effort is a deferred investment for a future they may not personally witness. Their 'payment' is the time and effort of study, and the deferral of full religious practice.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_of_jews, payer,
    moderate, biographical, identity_locked, global).

% Receives the preserved knowledge and the readiness for the restoration of the Temple service. This 'beneficiary' is a future state, not a current actor, representing the ultimate purpose of the preparatory study.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future, beneficiary,
    analytical, civilizational, analytical, universal).

% Will inherit the technical knowledge and practical understanding of sacrificial rites, enabling them to perform the service immediately upon the Temple's rebuilding. They are the direct recipients of the preserved expertise.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, future_priesthood, beneficiary,
    analytical, generational, analytical, global).

% Promulgate and reinforce the obligation to study Kodashim, guiding the community in its understanding and application. They interpret the tradition and maintain the continuity of the legal system, ensuring the knowledge is passed down.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transmission of complex, technical religious law (Kodashim) to ensure its availability and correct understanding for a future, currently unperformable, messianic restoration of the Temple service.
% TRANSFER_FUNCTION: Transfers intellectual effort and spiritual commitment from the current generation of Jews to the messianic future and future priesthood, in the form of preserved and understood sacrificial law.
% ABSENT_VOICES: Those who believe the sacrificial laws are entirely obsolete or purely historical would argue against the binding nature of preparatory study, but their voices are largely outside the mainstream discourse of this reading, which emphasizes continuity and future redemption.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim as preparation vanished, a core tenet of messianic expectation and the continuity of Jewish law would be undermined. The community's relationship to its past and future would fundamentally shift, potentially leading to a loss of technical knowledge vital for any future Temple service.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the sacrificial laws (Kodashim) unperformable, creating a dilemma for a legal system predicated on their performance, while maintaining the belief in their divine origin and future restoration.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities universally attest that the problem is live, as the Temple remains unbuilt. This is corroborated by the ongoing daily prayers for the Temple's rebuilding and the continued study of Kodashim in yeshivas worldwide, demonstrating a communal commitment that transcends individual benefiting parties.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the 'cost' of study is primarily intellectual and spiritual effort, which is seen as a positive religious act rather than a burden, and the benefits are deferred but significant (ensuring future performance). Suppression is low (0.05) as adherence is largely voluntary and identity-driven, not coercively enforced. Theater ratio is zero because the study is genuinely functional for its stated purpose of preservation and preparation; there is no performative maintenance of a defunct system. Accessibility collapse is high (0.9) because for adherents of this reading, there are no viable alternatives to studying these laws if one accepts the premise of future restoration. Resistance is low (0.02) because the obligation is deeply embedded in the religious identity and communal practice.
 *
 * PERSPECTIVAL GAP:
 *   The current generation experiences the constraint as a demanding but meaningful obligation, while the messianic future 'benefits' from it without direct participation. The gap is primarily temporal and existential, rather than one of direct extraction from a present actor. The engine's classification will reflect the low extraction from the current generation, consistent with a 'rope' classification, as the 'payment' is a voluntary investment in a shared future.
 *
 * DIRECTIONALITY LOGIC:
 *   The current generation of Jews are the 'payers' (d near target end) as they bear the immediate cost of study. The 'messianic future' and 'future priesthood' are the beneficiaries (d near beneficiary end) as they receive the preserved knowledge and the capacity for restoration. Rabbinic authorities are the agenda-setters, guiding and reinforcing the obligation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timing_uncertainty,
    'When will the messianic era arrive, and how does the indefinite deferral of the ''benefit'' (Temple restoration) impact the perceived ''cost'' of current study?',
    'Theological consensus on messianic signs, or the actual arrival of the messianic era.',
    'If the messianic era is perceived as indefinitely distant or unlikely, the extractiveness of current study might be perceived as higher by the current generation, potentially shifting the classification towards a ''tangled_rope'' if the ''benefit'' becomes too abstract.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_timing_uncertainty, conceptual, 'Uncertainty regarding the timing of the messianic era and its impact on the perceived value of preparatory study.').

omega_variable(
    technical_knowledge_sufficiency,
    'Is the current mode of study truly sufficient to preserve the technical knowledge required for actual Temple service, or is there a gap that only direct practice could fill?',
    'Expert assessment by scholars of ancient Temple practices, or the actual attempt to restore the service.',
    'If the knowledge is found to be insufficient, the ''preparation'' function of the study would be undermined, potentially increasing the ''theater_ratio'' and shifting the classification towards a ''piton'' if the study becomes purely symbolic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_knowledge_sufficiency, empirical, 'Whether current study adequately preserves technical knowledge for future Temple service.').

omega_variable(
    intergenerational_commitment_fragility,
    'How robust is the intergenerational commitment to this long-deferred project, and what factors could lead to a decline in adherence to the study obligation?',
    'Sociological studies of religious communities over time, or shifts in educational priorities within the community.',
    'A decline in commitment would reduce the ''suppression'' and ''accessibility_collapse'' for the current generation, potentially leading to a ''piton'' classification if the constraint persists primarily through inertia rather than active adherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_commitment_fragility, empirical, 'Fragility of intergenerational commitment to deferred religious obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.0).
narrative_ontology:measurement(koda_tr_t25, kodashim_obligation__study_as_preparation, theater_ratio, 25, 0.0).
narrative_ontology:measurement(koda_tr_t50, kodashim_obligation__study_as_preparation, theater_ratio, 50, 0.0).
narrative_ontology:measurement(koda_tr_t75, kodashim_obligation__study_as_preparation, theater_ratio, 75, 0.0).
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_preparation, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(koda_be_t25, kodashim_obligation__study_as_preparation, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(koda_be_t50, kodashim_obligation__study_as_preparation, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(koda_be_t75, kodashim_obligation__study_as_preparation, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_preparation, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_preparation, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(koda_su_t25, kodashim_obligation__study_as_preparation, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(koda_su_t50, kodashim_obligation__study_as_preparation, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(koda_su_t75, kodashim_obligation__study_as_preparation, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(koda_su_t100, kodashim_obligation__study_as_preparation, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'study_as_preparation' reading of the 'kodashim_obligation' kernel. It is structurally distinct from 'study_as_performance' (where study is the performance itself) and 'study_as_archive' (where study is historical preservation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
