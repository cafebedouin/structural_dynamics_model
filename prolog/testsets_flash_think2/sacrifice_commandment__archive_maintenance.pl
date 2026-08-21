% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Halakhic Study as Archive Maintenance for Future Temple Restoration
 *   domain: religious/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   This constraint describes the halakhic (Jewish legal) practice of
 *   studying the laws of Temple sacrifices as a means of preserving technical
 *   knowledge for a future, messianic restoration of the Temple. This reading
 *   emphasizes the archival function of study, deferring direct worship or
 *   immediate physical preparation. It is one reading of the broader
 *   'sacrifice_commandment' kernel, distinct from interpretations that view
 *   study as a form of performance or that suspend the commandment entirely.
 *
 * KEY AGENTS:
 *   - halakhic_scholars: Agenda-setter/Beneficiary (institutional/identity_locked)
 *   - future_generations: Beneficiary (powerless/analytical)
 *   - present_worshippers: Payer (moderate/constrained)
 *   - rabbinic_authorities: Agenda-setter (institutional/identity_locked)
 *   - messianic_activists: Excluded (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.45).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.6).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, rope).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Halakhic Study as Archive Maintenance for Future Temple Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic_theory/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '68d8bacf-19a9-474d-9ccd-0764e7baa809').
narrative_ontology:cs_kernel_codification('68d8bacf-19a9-474d-9ccd-0764e7baa809', fixed_text).
narrative_ontology:cs_authority_grounding('68d8bacf-19a9-474d-9ccd-0764e7baa809', lineage).
narrative_ontology:cs_interpretation_layer_present('68d8bacf-19a9-474d-9ccd-0764e7baa809').
narrative_ontology:cs_reading_relation('68d8bacf-19a9-474d-9ccd-0764e7baa809', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('68d8bacf-19a9-474d-9ccd-0764e7baa809', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_axiom('68d8bacf-19a9-474d-9ccd-0764e7baa809', foundational, knowledge_preservation_is_commanded).
narrative_ontology:cs_axiom_status(knowledge_preservation_is_commanded, holdable).
narrative_ontology:cs_axiom_grounding('68d8bacf-19a9-474d-9ccd-0764e7baa809', knowledge_preservation_is_commanded, deontological).
narrative_ontology:cs_axiom('68d8bacf-19a9-474d-9ccd-0764e7baa809', foundational, future_restoration_is_divine_will).
narrative_ontology:cs_axiom_status(future_restoration_is_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('68d8bacf-19a9-474d-9ccd-0764e7baa809', future_restoration_is_divine_will, theological).
narrative_ontology:cs_reference_frame('68d8bacf-19a9-474d-9ccd-0764e7baa809', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('68d8bacf-19a9-474d-9ccd-0764e7baa809', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('68d8bacf-19a9-474d-9ccd-0764e7baa809', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_generations).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, halakhic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_worshippers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devote their lives to the intricate study of Temple laws, ensuring the knowledge is preserved and transmitted. They benefit from the continuity of their intellectual tradition and the communal support for their work, even if direct application is deferred.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, halakhic_scholars, beneficiary).

% The ultimate recipients of the preserved knowledge, who would be able to apply it if the Temple were restored. They bear no present cost but are the primary justification for the current effort.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_generations, beneficiary,
    powerless, civilizational, analytical, universal).

% Support the institutions and scholars dedicated to this study, often through donations or communal participation. While they believe in the future restoration, the direct spiritual fulfillment from this specific form of engagement is indirect or deferred, making their contribution feel like a cost for a distant benefit.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_worshippers, payer,
    moderate, biographical, constrained, local).

% Interpret and uphold the halakhic tradition, guiding the community in its commitment to this form of study. They ensure the continuity of the legal framework that justifies the archive maintenance.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Advocate for more immediate, tangible actions towards Temple reconstruction or a more direct, present fulfillment of the commandment. Their perspective is often sidelined by the archive maintenance reading, which defers such actions.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, messianic_activists, excluded,
    organized, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, future_generations).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation and transmission of complex ritual knowledge across generations, ensuring its availability for a future, currently impossible, religious practice.
% TRANSFER_FUNCTION: Transfers intellectual effort and communal resources from present scholars and worshippers to the maintenance of a knowledge archive for future use.
% ABSENT_VOICES: Messianic activists who believe in more immediate, physical preparation for the Temple's return, or those who seek more direct, present spiritual fulfillment from religious practice, are structurally excluded from the dominant discourse of this reading.
% DISAPPEARANCE_RATIONALE: If the commitment to preserving this knowledge vanished, the continuity of halakhic tradition regarding Temple service would be broken, making future restoration (if it ever became possible) significantly harder or impossible, fundamentally altering the religious landscape.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the inability to perform sacrifices, creating a dilemma for how to fulfill divine commandments related to the Temple in its absence.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts and ongoing halakhic discourse attest to this problem. The continued absence of the Temple and the ongoing relevance of its laws within Jewish tradition corroborate the problem's persistence.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).
:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate because present worshippers and scholars invest significant resources and intellectual effort for a benefit that is deferred to an uncertain future. Suppression is moderate, reflecting the strong communal and rabbinic norms that encourage this study, making alternatives for fulfilling the commandment less accessible within the tradition. Theater ratio is low because the study genuinely serves its stated archival purpose. The claimed type is 'rope' as it coordinates a collective, long-term effort for a shared future good, despite the present-day costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of future generations, this constraint is a pure benefit, providing essential knowledge. For present worshippers, it represents a cost with deferred, indirect spiritual fulfillment. Halakhic scholars and rabbinic authorities, as agenda-setters, perceive it as a necessary and meritorious act of continuity. Messianic activists, however, see it as an insufficient or even delaying action.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars and rabbinic authorities are beneficiaries and agenda-setters, as they maintain the tradition and benefit from its continuity. Future generations are the ultimate beneficiaries, receiving the preserved knowledge. Present worshippers are payers, contributing resources and effort for a deferred benefit. Messianic activists are excluded, as their preferred actions are sidelined by this reading's emphasis on archival study.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a Snare, despite the present-day costs, because it genuinely coordinates a collective action (knowledge preservation) for a future collective good (Temple restoration). The 'rope' classification acknowledges the coordination function while the moderate extractiveness reflects the cost of deferral. The 'live' status of the founding problem (Temple destruction) prevents it from being a Piton, as its function is still relevant, even if its fulfillment is indirect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_utility_of_study,
    'Is the current level and specific focus of halakhic study genuinely necessary for future Temple restoration, or has it become an end in itself, exceeding the practical requirements of archive maintenance?',
    'Expert halakhic and historical analysis comparing the scope of current study to the minimum required for practical restoration, or a hypothetical scenario where the Temple is rebuilt, revealing gaps or redundancies in the preserved knowledge.',
    'If the study exceeds practical necessity, the extractiveness for present worshippers and scholars is higher than currently assessed, as a portion of their effort is for a non-archival purpose. This would push the constraint closer to a Tangled Rope or even Snare if the ''end in itself'' serves specific scholarly interests over communal utility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_utility_of_study, empirical, 'Whether the scope of study aligns with its stated archival purpose.').

omega_variable(
    messianic_timing_ambiguity,
    'When is the ''future'' for which this knowledge is being preserved? Does the indefinite deferral of active preparation or direct worship undermine the spiritual vitality or urgency of the commandment?',
    'Theological and philosophical discourse within the tradition, potentially influenced by external events or shifts in communal priorities, leading to a re-evaluation of messianic timelines or the nature of preparation.',
    'If the deferral is deemed too indefinite or spiritually detrimental, the extractiveness for present worshippers increases, as the ''future utility'' becomes too distant to justify present costs. This could lead to a reclassification towards a Snare if the deferral is perceived as a mechanism to maintain scholarly authority rather than genuine preparation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_timing_ambiguity, conceptual, 'The impact of indefinite deferral on the constraint''s perceived utility and extractiveness.').

omega_variable(
    reading_legitimacy_contest,
    'Is this ''archive_maintenance'' reading a legitimate fulfillment of the sacrifice commandment, or a temporary substitute for a suspended obligation, with implications for the spiritual status of present-day practice?',
    'A shift in dominant halakhic consensus, potentially triggered by a major rabbinic ruling or a significant communal movement, re-evaluating the spiritual efficacy of study in lieu of actual sacrifice.',
    'If reclassified as a mere substitute, the perceived value of the ''coordination function'' diminishes, and the extractiveness for present participants increases, as their efforts are for a less spiritually potent outcome. This could shift the classification towards a Piton if the primary function is seen as atrophied, maintained mostly by inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, conceptual, 'The spiritual legitimacy of study as fulfillment vs. substitute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_commandment__archive_maintenance, theater_ratio, 400, 0.09).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_commandment__archive_maintenance, theater_ratio, 800, 0.09).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_commandment__archive_maintenance, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_commandment__archive_maintenance, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_commandment__archive_maintenance, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sacr_be_t400, sacrifice_commandment__archive_maintenance, base_extractiveness, 400, 0.38).
narrative_ontology:measurement(sacr_be_t800, sacrifice_commandment__archive_maintenance, base_extractiveness, 800, 0.41).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_commandment__archive_maintenance, base_extractiveness, 1200, 0.43).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_commandment__archive_maintenance, base_extractiveness, 1600, 0.44).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_commandment__archive_maintenance, base_extractiveness, 2000, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sacr_su_t400, sacrifice_commandment__archive_maintenance, suppression_requirement, 400, 0.57).
narrative_ontology:measurement(sacr_su_t800, sacrifice_commandment__archive_maintenance, suppression_requirement, 800, 0.58).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_commandment__archive_maintenance, suppression_requirement, 1200, 0.59).
narrative_ontology:measurement(sacr_su_t1600, sacrifice_commandment__archive_maintenance, suppression_requirement, 1600, 0.6).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_commandment__archive_maintenance, suppression_requirement, 2000, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
