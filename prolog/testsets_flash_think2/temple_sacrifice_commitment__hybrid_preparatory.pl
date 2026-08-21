% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Hybrid Preparatory Commitment to Temple Sacrifice
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid preparatory' reading of the Temple
 *   sacrifice commitment within Halakhic tradition. This reading asserts that
 *   the study of sacrifice laws maintains the divine command in a suspended
 *   state, neither fully active nor merely archived. It functions as an
 *   active preparatory exercise for a future messianic restoration. This
 *   reading is distinct from those that view study as the performance itself,
 *   or as purely archival, or as having undergone symbolic transformation.
 *   The constraint is claimed as a Rope by its proponents, but its operation,
 *   as described by the metrics, reveals it to be a Tangled Rope due to the
 *   extraction of resources for an uncertain future benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.55).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.6).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Hybrid Preparatory Commitment to Temple Sacrifice").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '90bd7220-4748-4db3-9c0f-8ada83c499cf').
narrative_ontology:cs_kernel_codification('90bd7220-4748-4db3-9c0f-8ada83c499cf', fixed_text).
narrative_ontology:cs_authority_grounding('90bd7220-4748-4db3-9c0f-8ada83c499cf', lineage).
narrative_ontology:cs_interpretation_layer_present('90bd7220-4748-4db3-9c0f-8ada83c499cf').
narrative_ontology:cs_reading_relation('90bd7220-4748-4db3-9c0f-8ada83c499cf', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('90bd7220-4748-4db3-9c0f-8ada83c499cf', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('90bd7220-4748-4db3-9c0f-8ada83c499cf', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('90bd7220-4748-4db3-9c0f-8ada83c499cf', foundational, divine_command_is_eternal_and_unfulfilled).
narrative_ontology:cs_axiom_status(divine_command_is_eternal_and_unfulfilled, holdable).
narrative_ontology:cs_axiom_grounding('90bd7220-4748-4db3-9c0f-8ada83c499cf', divine_command_is_eternal_and_unfulfilled, deontological).
narrative_ontology:cs_axiom('90bd7220-4748-4db3-9c0f-8ada83c499cf', foundational, active_preparation_maintains_commitment).
narrative_ontology:cs_axiom_status(active_preparation_maintains_commitment, holdable).
narrative_ontology:cs_axiom_grounding('90bd7220-4748-4db3-9c0f-8ada83c499cf', active_preparation_maintains_commitment, conventional).
narrative_ontology:cs_reference_frame('90bd7220-4748-4db3-9c0f-8ada83c499cf', post_temple_exile_continuity).
narrative_ontology:cs_drift_state('90bd7220-4748-4db3-9c0f-8ada83c499cf', contemporary_diaspora, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('90bd7220-4748-4db3-9c0f-8ada83c499cf', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, messianic_restorationists).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, community_donors).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the complex laws of Temple sacrifice, ensuring their continuity. They benefit from the intellectual and institutional commitment to this study, which validates their expertise and role within the community.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Serve as centers of learning where these laws are studied. They receive funding from donors and attract students, perpetuating their institutional existence and role in maintaining the tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, beneficiary,
    institutional, generational, constrained, global).

% Provide significant financial support to yeshivas and scholars, funding the study of laws for a future event. Their donations are driven by religious commitment but represent a tangible resource transfer for an uncertain, deferred benefit.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, community_donors, payer,
    powerful, biographical, constrained, local).

% Dedicate substantial time, intellectual effort, and often personal financial sacrifice to studying laws that cannot currently be performed. Their commitment is deeply tied to their religious identity and the belief in future restoration.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha, payer,
    moderate, biographical, identity_locked, global).

% Their vision of a future messianic era, including the rebuilding of the Temple and resumption of sacrifices, is kept alive and actively prepared for by this ongoing study. The commitment provides a framework for their aspirations.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, messianic_restorationists, beneficiary,
    organized, generational, identity_locked, global).

% Would question the practical utility or rationality of dedicating extensive resources to studying non-performable laws. They are outside the interpretive community and their critiques are generally not engaged with by the stakeholders.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, secular_critics, excluded,
    powerless, immediate, mobile, local).

% Believe that study without material instantiation is insufficient to fulfill the divine command, viewing it as mere archival preservation. Their more literal interpretation is marginalized by this reading's emphasis on preparatory study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, proponents_of_performance_only, excluded,
    organized, biographical, constrained, global).

% Believe that the intellectual engagement of studying sacrifice law *is* itself the performance of the divine command, making the 'preparatory' aspect of this reading diminish the present spiritual value of study. They are excluded from the core framing of this reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, proponents_of_study_as_exercise, excluded,
    organized, biographical, constrained, global).

% Believe the commitment to sacrifice has undergone an authorized transformation, where prayer and ethical living are the new instantiation, rendering preparatory study for a material restoration obsolete. Their view is not central to this reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, proponents_of_symbolic_transformation, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing intellectual and spiritual commitment to the Temple service, ensuring its knowledge is preserved, transmitted, and actively prepared for future messianic restoration, thereby preventing its archival decay or purely theoretical status.
% TRANSFER_FUNCTION: Transfers cognitive resources (time, intellectual effort, spiritual focus) from students and financial resources from donors to rabbinic scholars and institutions, in exchange for maintaining the commitment to future messianic restoration and the continuity of the Halakhic tradition.
% ABSENT_VOICES: Proponents of other readings (performance_only, study_as_exercise, symbolic_transformation) are structurally excluded from the dominant discourse of this reading, as are secular critics who question the premise entirely. They would argue for different interpretations of the commitment or its irrelevance.
% DISAPPEARANCE_RATIONALE: If this specific commitment to preparatory study vanished overnight, the entire institutional structure of yeshivas and rabbinic scholarship focused on these laws would collapse or radically reorient. The community's sense of future purpose, its continuity with its past, and its collective identity would be profoundly altered, leading to a significant reorganization of religious life and institutions.
% FOUNDING_PROBLEM: The founding problem was how to maintain the divine command regarding Temple sacrifice and its associated laws during a prolonged period of exile and inability to perform them, without allowing the knowledge to atrophy, become purely theoretical, or be forgotten.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition and historical continuity attest to the ongoing challenge of maintaining this commitment across centuries of exile. While some within the community contest the *method* of maintenance, the underlying problem of suspended practice and the need for its preservation is widely acknowledged across different interpretive schools and by historical accounts from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because significant cognitive and financial resources are dedicated to studying laws that cannot be performed, with the benefit deferred to an uncertain future. Suppression (0.60) is present in the form of institutional and social pressure to conform to this interpretive stance, marginalizing alternative readings. The theater ratio (0.40) reflects that while the study is genuinely functional for preservation and preparation, a portion of its maintenance is performative, signaling ongoing commitment despite practical impossibility. Accessibility collapse is moderate (0.40) as other interpretive options exist but are less institutionally supported. Resistance (0.50) comes from proponents of alternative readings and secular critics.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars and yeshiva institutions, this constraint is a vital coordination mechanism for preserving tradition and preparing for redemption. From the perspective of community donors and students, it involves a significant transfer of resources and effort for a benefit that is distant and uncertain, making it feel more extractive. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and yeshiva institutions are beneficiaries, as they receive resources and maintain their institutional roles. Messianic restorationists also benefit, as their vision is actively sustained. Community donors and students of Halakha are payers, as they contribute resources and effort for a deferred benefit. Their exit options are identity-locked, as their commitment is deeply intertwined with their religious identity. Other interpretive schools are excluded, as their views are not central to this reading's framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents the commitment from becoming a Piton (inertial, atrophied) by framing study as a 'preparatory exercise,' thus maintaining a live, forward-looking mandate. However, the moderate extractiveness and suppression, coupled with the deferred and uncertain benefit, prevent it from being a pure Rope. The 'hybrid preparatory' framing is precisely what keeps it from being merely an archive (Piton) or a purely symbolic act (a different kind of Rope), but it does so by extracting resources for its maintenance, making it a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncertainty_of_restoration,
    'Is the messianic restoration a certainty, a hope, or a theological construct, and how does this affect the perceived value of preparatory study?',
    'Theological consensus shifts, or empirical events (e.g., actual restoration efforts).',
    'If the restoration is perceived as less certain, the extractiveness of preparatory study increases for payers, potentially shifting the classification towards Snare. If certainty increases, extractiveness decreases, moving it closer to Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncertainty_of_restoration, conceptual, 'Theological status of messianic restoration and its impact on perceived value.').

omega_variable(
    resource_allocation_efficiency,
    'Is the current allocation of cognitive and financial resources to preparatory study the most efficient or effective way to achieve the stated goal of maintaining commitment and preparing for restoration?',
    'Independent economic and sociological analysis of resource deployment in religious institutions, or comparative studies with alternative models of commitment maintenance.',
    'If the allocation is found to be inefficient, the measured extractiveness is further justified as an overhead, potentially strengthening the Tangled Rope classification or pushing it towards Snare if the inefficiency is severe and unacknowledged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Efficiency of resource allocation for preparatory study.').

omega_variable(
    interpretive_legitimacy_of_hybrid_reading,
    'Is this ''hybrid preparatory'' reading the most legitimate or coherent way to maintain the commitment to Temple sacrifice, compared to sibling readings?',
    'Shifts in rabbinic consensus, emergence of a dominant alternative interpretive framework, or a formal re-adjudication of the commitment''s status.',
    'If this reading''s legitimacy erodes, its suppression requirements might increase to maintain adherence, or its coordination function might weaken, potentially leading to a reclassification towards Piton or Snare if it becomes purely extractive without genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_legitimacy_of_hybrid_reading, conceptual, 'The relative legitimacy of the hybrid preparatory reading among competing interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(temp_tr_t1250, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 1250, 0.3).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 1500, 0.35).
narrative_ontology:measurement(temp_tr_t1750, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 1750, 0.38).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(temp_tr_t2024, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement(temp_be_t1250, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 1250, 0.45).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 1500, 0.5).
narrative_ontology:measurement(temp_be_t1750, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 1750, 0.53).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement(temp_be_t2024, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 1000, 0.45).
narrative_ontology:measurement(temp_su_t1250, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 1250, 0.5).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement(temp_su_t1750, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 1750, 0.58).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 2000, 0.59).
narrative_ontology:measurement(temp_su_t2024, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, halakhic_interpretive_authority).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, messianic_expectations_norm).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'temple_sacrifice_commitment' kernel. Its extractiveness differs significantly from sibling readings like 'study_as_exercise' (lower extractiveness) or 'performance_only' (higher extractiveness if enforced without possibility of performance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
