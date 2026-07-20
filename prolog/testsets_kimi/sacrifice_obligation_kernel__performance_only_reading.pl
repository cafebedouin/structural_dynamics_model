% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation â Performance-Only Reading
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint instantiates the performance_only_reading of the
 *   sacrifice_obligation_kernel. It holds that the biblical command to bring
 *   sacrifices in the Temple remains fully binding and can only be discharged
 *   through physical performance; study of sacrificial law is preparatory but
 *   does not fulfill the mitzvah. Since the destruction of the Second Temple
 *   approximately 1,900 years ago, the commanded performance has been
 *   structurally impossible. The entire Jewish community is thus situated as
 *   the bearer of an unfulfillable divine command. No human agent
 *   concentrates benefit from this arrangement; the halakhic authority
 *   administers the obligation but does not extract material gain. The
 *   constraint persists by institutional and theological inertia, maintaining
 *   1,900 years of unperformed obligation through curriculum, liturgy, and
 *   legal discourse. As a kernel reading, it is structurally contested by
 *   siblings that intellectualize the obligation (study_as_exercise), suspend
 *   it eschatologically (messianic_suspension), or archive it culturally
 *   (symbolic_archive).
 *
 * KEY AGENTS:
 *   - halakhic_authority: Agenda-setter (institutional/constrained) â administers the performance-only interpretation and could theoretically alter it
 *   - jewish_community: Payer (organized/identity_locked) â bears the diffuse costs of an unfulfillable commandment across 1,900 years
 *   - reform_movements: Excluded (organized/mobile) â advocates alternative readings but barred from the halakhic decisorial process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.88).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.3).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, piton).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation â Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '9fb51499-5d9b-44fa-9f66-b7e0f57d84ba').
narrative_ontology:cs_kernel_codification('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', fixed_text).
narrative_ontology:cs_authority_grounding('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', lineage).
narrative_ontology:cs_interpretation_layer_present('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba').
narrative_ontology:cs_reading_relation('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', foundational, physical_performance_sole_fulfillment).
narrative_ontology:cs_axiom_status(physical_performance_sole_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', physical_performance_sole_fulfillment, deontological).
narrative_ontology:cs_axiom('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', foundational, study_preparatory_not_discharge).
narrative_ontology:cs_axiom_status(study_preparatory_not_discharge, holdable).
narrative_ontology:cs_axiom_grounding('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', study_preparatory_not_discharge, deontological).
narrative_ontology:cs_reference_frame('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', temple_cult_operational).
narrative_ontology:cs_drift_state('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9fb51499-5d9b-44fa-9f66-b7e0f57d84ba', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, jewish_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, divine_command_immutability).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, temple_cult_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the interpretation that study of sacrificial law is preparatory and does not fulfill the biblical obligation. Maintains the curriculum, prayer liturgy, and legal rulings that preserve the performance-only stance. Derives no material benefit from the arrangement but maintains institutional authority through continuity with the classical textual tradition. Could theoretically rule the obligation suspended or transformed but has not done so for 1,900 years.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority, agenda_setter,
    institutional, civilizational, constrained, global).

% Constituted by the divine command to bring sacrifices in the Temple. For 1,900 years has been unable to perform the commanded physical acts. Studies the sacrificial laws in detail as preparation, but is taught that this study does not discharge the obligation. Bears the spiritual, educational, and psychological costs of an unfulfillable commandment as a constitutive feature of religious identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, jewish_community, payer,
    organized, generational, identity_locked, global).

% Advance alternative readings of the sacrifice obligation (study-as-fulfillment, symbolic archive, or messianic suspension) that would relieve the community of the unfulfillable command. They are excluded from the normative halakhic decisorial process and their readings are ruled outside the tradition by the agenda-setter.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, reform_movements, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated the covenantal relationship between God and Israel through prescribed Temple ritual. In its current degraded form, the reading maintains that only physical performance can coordinate this relationship; study preserves knowledge but cannot substitute.
% TRANSFER_FUNCTION: Moves cognitive and spiritual labor (study, memorization, liturgical substitution) from the Jewish community into the maintenance of an unfulfillable obligation. No human agent captures the transfer; the gain accrues to the abstract continuity of the halakhic system itself.
% ABSENT_VOICES: Reform and progressive Jewish movements, secular biblical scholars, and supersessionist theologians argue for alternative readings (study-as-fulfillment, messianic suspension, or cultural archiving) but are excluded from the halakhic decisorial framework.
% DISAPPEARANCE_RATIONALE: If the performance-only obligation were declared void or fulfilled, Jewish legal study curricula would eliminate the tractates devoted to sacrifices as active obligation, prayer formulations would shift, and the eschatological orientation toward Temple restoration would lose its halakhic anchor. The religious landscape would rearrange.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the need to preserve the integrity of biblical law without allowing study or prayer to completely replace the commanded physical sacrifices.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and archaeologists attest to the Temple destruction and the historical shift from priestly cult to rabbinic text-based Judaism. No external party corroborates the theological claim that the obligation remains in full performative force; the corroboration of the historical problem does not extend to the halakhic solution's persistence.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88) because the obligation extracts continuous cognitive, spiritual, and educational labor from the community for a performance that cannot occur. Suppression is moderate (0.30) because while there is little active coercion, alternative theological framings are suppressed within normative halakhic discourse. Theater_ratio is high (0.82) because the bulk of current activity (study, liturgical recollection) is performative maintenance of an atrophied function. Accessibility_collapse is high (0.80) because within the committed framework, no alternative discharge of the obligation is recognized. Resistance is low (0.12) because the obligation is largely accepted as constitutive of identity rather than actively resisted. The metrics claim independence from the piton classification: the engine may compute a different per-seat type, which is the intended signal.
 *
 * PERSPECTIVAL GAP:
 *   The halakhic authority experiences the constraint as custodianship of an unbroken legal tradition; the engine will likely compute a symmetric or low-extraction seat for the agenda-setter. The Jewish community experiences it as a binding command they are powerless to fulfill; the engine will compute a high-extraction target seat due to identity_locked exit and victim declaration. This divergence is structural and intended.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared, consistent with the structural observation that no human agent captures the extraction. The Jewish community is declared in victims, driving their directionality toward the full-target end (d near 1.0). The halakhic authority is neither beneficiary nor victim; their directionality reverts to the institutional power-atom fallback, placing them near symmetric. The absence of a beneficiary means effective extraction is not inverted into subsidy for any seat; the constraint's extraction is pure cost borne diffusely.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as piton prevents mislabeling the constraint as a snare: there is no concentrated beneficiary extracting rents, and there is no active enforcement machinery coercing participation. The high extractiveness is inertial, not predatory. The mandatrophy is resolved in the sense that the founding problem (maintaining obligation integrity post-Temple) is dead, but the arrangement persists because the cost of authoritative revision (undermining the entire halakhic system's claim to continuity) exceeds the benefit of declaring the obligation suspended. If a beneficiary were discovered (e.g., a class of scholars whose careers depend on the performance-only framing), the constraint would recompute toward tangled_rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the performance_only_reading of the sacrifice_obligation_kernel. How would the structural classification change if the study_as_exercise_reading, messianic_suspension_reading, or symbolic_archive_reading were adopted instead?',
    'Corpus-level analysis comparing the epsilon, beneficiary/victim structure, and computed type of each reading in the constraint family.',
    'Adopting study_as_exercise would shift the victim set toward the study-exhausted and reduce extractiveness by intellectualizing the obligation; messianic_suspension would transform the constraint into a scaffold or rope with a sunset clause; symbolic_archive would eliminate the victim set entirely by demoting the obligation to cultural memory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Commitment-system framing ambiguity across sibling readings of the sacrifice obligation kernel').

omega_variable(
    divine_command_vs_institutional_inertia,
    'Is the persistence of this obligation a function of immutable divine command (Mountain within the theological frame) or institutional inertia preventing the halakhic authority from declaring the obligation suspended?',
    'Historical analysis of halakhic responsiveness to changed material conditions; whether the authority has ever formally terminated an analogous biblical obligation.',
    'If Mountain, the high extractiveness is an unavoidable feature of the created order; if Piton, the extraction is maintained by human choice and could be alleviated by authoritative ruling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_vs_institutional_inertia, conceptual, 'Ambiguity between divine natural law and inertial institutional persistence').

omega_variable(
    suppression_mechanism_ambiguity,
    'Does the constraint persist because of structural enforcement by halakhic authorities, or because of internalized identity-lock within the Jewish community?',
    'Comparative analysis of communities that have formally rejected the performance-only reading; if rejection correlates with reduced institutional enforcement rather than identity change, suppression is structural.',
    'If internalized, effective suppression and extractiveness are higher than structural measures suggest because the community carries the constraint across institutional boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in religious obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_perf_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sacrifice_perf_tr_t500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 500, 0.4).
narrative_ontology:measurement(sacrifice_perf_tr_t1000, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1000, 0.56).
narrative_ontology:measurement(sacrifice_perf_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.7).
narrative_ontology:measurement(sacrifice_perf_tr_t1900, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1900, 0.82).

% Extraction over time
narrative_ontology:measurement(sacrifice_perf_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(sacrifice_perf_be_t500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 500, 0.68).
narrative_ontology:measurement(sacrifice_perf_be_t1000, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1000, 0.75).
narrative_ontology:measurement(sacrifice_perf_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.82).
narrative_ontology:measurement(sacrifice_perf_be_t1900, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1900, 0.88).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__performance_only_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel conflates the biblical command, its post-Temple halakhic status, and its contemporary cultural function. Decomposed into four structurally distinct readings (performance_only, study_as_exercise, messianic_suspension, symbolic_archive), each with independent epsilon, beneficiary/victim structure, and classification. This file instantiates the performance_only reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
