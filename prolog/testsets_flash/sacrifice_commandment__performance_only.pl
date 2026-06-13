% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment: Physical Performance Only Reading
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'performance_only' reading of the
 *   sacrifice commandment within Halakhic theory, which asserts that the
 *   commandment requires physical execution in the Temple and is therefore
 *   suspended, not fulfilled, in its absence. This reading directs
 *   significant scholarly attention and institutional resources towards the
 *   theoretical study of unperformable acts, maintaining rabbinic authority
 *   and institutional relevance, but at the cost of diverting intellectual
 *   labor from other areas and potentially fostering spiritual frustration
 *   among lay adherents. The constraint is claimed as a Snare due to its high
 *   extraction of scholarly attention and suppression of alternative
 *   interpretations, despite its framing as a faithful adherence to divine
 *   law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.9).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, snare).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment: Physical Performance Only Reading").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious_studies/halakhic_theory/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '140dad0e-e4f3-466c-95f9-21cb442ff0df').
narrative_ontology:cs_kernel_codification('140dad0e-e4f3-466c-95f9-21cb442ff0df', fixed_text).
narrative_ontology:cs_authority_grounding('140dad0e-e4f3-466c-95f9-21cb442ff0df', lineage).
narrative_ontology:cs_interpretation_layer_present('140dad0e-e4f3-466c-95f9-21cb442ff0df').
narrative_ontology:cs_reading_relation('140dad0e-e4f3-466c-95f9-21cb442ff0df', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('140dad0e-e4f3-466c-95f9-21cb442ff0df', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('140dad0e-e4f3-466c-95f9-21cb442ff0df', foundational, physical_execution_is_sine_qua_non).
narrative_ontology:cs_axiom_status(physical_execution_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('140dad0e-e4f3-466c-95f9-21cb442ff0df', physical_execution_is_sine_qua_non, deontological).
narrative_ontology:cs_axiom('140dad0e-e4f3-466c-95f9-21cb442ff0df', foundational, commandment_suspended_not_fulfilled_without_temple).
narrative_ontology:cs_axiom_status(commandment_suspended_not_fulfilled_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('140dad0e-e4f3-466c-95f9-21cb442ff0df', commandment_suspended_not_fulfilled_without_temple, deontological).
narrative_ontology:cs_reference_frame('140dad0e-e4f3-466c-95f9-21cb442ff0df', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('140dad0e-e4f3-466c-95f9-21cb442ff0df', contemporary_messianic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('140dad0e-e4f3-466c-95f9-21cb442ff0df', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, yeshiva_institutions).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, scholarly_attention).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and transmitters of Halakha. They direct scholarly attention to the intricate laws of sacrifice, even when unperformable, maintaining the intellectual tradition and their authority within it. Their professional identity is deeply intertwined with this interpretive framework.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the continuous need for specialized study and interpretation of complex, often unperformable, laws. This sustains their curriculum, funding, and institutional relevance, even if the practical application is suspended.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% The collective intellectual effort and focus of the community, particularly students and scholars. It is diverted towards theoretical study of unperformable acts, rather than contemporary, actionable Halakha or other fields of knowledge. This represents a significant opportunity cost.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, scholarly_attention, payer,
    powerless, biographical, trapped, global).

% Bear the cost of a religious system that emphasizes a central commandment as unfulfillable in their lifetime, potentially leading to spiritual frustration or a sense of incompleteness. Their identity is tied to the tradition, making exit difficult.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, lay_adherents, payer,
    moderate, biographical, identity_locked, global).

% Advocate for the immediate rebuilding of the Temple and resumption of sacrifices, directly challenging the 'suspended not fulfilled' interpretation. Their calls for action are suppressed by the dominant rabbinic authority that maintains the current interpretive framework.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_movements, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intellectual and spiritual life of the community around a shared understanding of divine commandments, even those currently unperformable, maintaining a continuous tradition of study and anticipation.
% TRANSFER_FUNCTION: Transfers significant intellectual labor and spiritual focus from contemporary religious practice and other fields of knowledge towards the theoretical study of ancient sacrificial rites, from scholars and adherents to the maintenance of rabbinic authority and institutional relevance.
% ABSENT_VOICES: Messianic movements and those advocating for a more 'active' or 'performative' fulfillment of the commandment (e.g., through symbolic acts or immediate Temple rebuilding) are excluded from mainstream discourse, their interpretations deemed premature or heretical.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the entire structure of rabbinic authority and yeshiva curricula would need to fundamentally reorient. The focus of study would shift dramatically, potentially leading to a crisis of identity for institutions and scholars, and a re-evaluation of religious practice for lay adherents.
% FOUNDING_PROBLEM: The destruction of the Second Temple left a central divine commandment (sacrifices) unperformable, creating a crisis of religious practice and continuity for the Jewish people.
% FOUNDING_PROBLEM_CORROBORATION: The problem of unperformable commandments is universally acknowledged within the tradition. The 'suspended not fulfilled' status is attested by centuries of rabbinic literature and is a foundational premise for the current structure of Jewish law, corroborated by the historical absence of the Temple and the ongoing theological discourse.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because 1,900 years of intellectual labor are directed at a commandment that cannot be performed, representing a massive opportunity cost. Suppression (0.9) is also high, as this reading actively suppresses alternative interpretations (e.g., 'study as performance') and challenges to rabbinic authority. The theater ratio (0.1) is low because the study is genuinely rigorous and not merely performative; its 'function' is to maintain the interpretive tradition and the authority structure, even if the original divine command remains unfulfilled. Accessibility collapse is high (0.95) because, within this framework, there are virtually no legitimate alternatives to the current interpretive path for fulfilling the commandment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars, this is a faithful and necessary interpretation that preserves the integrity of the commandment. From the perspective of scholarly attention and lay adherents, it is an extractive structure that diverts resources and creates spiritual longing without fulfillment. The engine's classification as a Snare highlights this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and yeshiva institutions are beneficiaries, as this reading sustains their intellectual and institutional roles. Scholarly attention and lay adherents are victims, bearing the cost of diverted intellectual resources and spiritual incompleteness. Messianic movements are excluded, as their calls for immediate action directly challenge the 'suspended' status.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving the sacrifice commandment) is still 'live' in a theological sense, but its practical function is suspended. The 'performance_only' reading prevents mislabeling this as a Piton, because it actively extracts (scholarly attention) and suppresses alternatives, rather than merely persisting through inertia. It is a Snare because the coordination story (preserving the commandment) is cover for the extraction of intellectual labor and the maintenance of an authority structure that benefits from the unperformability of the core act.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the extensive theoretical study of unperformable sacrifice laws a genuine coordination function (preserving knowledge for a future Temple) or primarily an extractive mechanism (maintaining rabbinic authority and institutional relevance)?',
    'Analysis of resource allocation: if a significant portion of resources is dedicated to speculative, non-practical aspects of sacrifice law, it leans towards extraction. If it focuses on practical, immediately applicable aspects for a future Temple, it leans towards coordination.',
    'If primarily extractive, the constraint''s Snare classification is reinforced. If a stronger coordination function is identified, it might shift towards a Tangled Rope, acknowledging a genuine, albeit costly, coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Distinguishing genuine knowledge preservation from authority maintenance.').

omega_variable(
    alternative_fulfillment_validity,
    'Could alternative forms of ''fulfillment'' (e.g., prayer, study as performance, symbolic acts) be legitimately recognized without undermining the core commandment, or does ''performance_only'' represent an irreducible theological truth?',
    'Theological and jurisprudential analysis of historical precedents and contemporary arguments for alternative modes of fulfillment. Examination of how other commandments are fulfilled when direct performance is impossible.',
    'If alternative fulfillments are deemed valid, the suppression of these options becomes more clearly extractive, reinforcing the Snare classification. If ''performance_only'' is an irreducible truth, the constraint is closer to a Mountain (though still with beneficiaries).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_fulfillment_validity, conceptual, 'Theological status of alternative modes of commandment fulfillment.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''performance_only'' reading of the sacrifice commandment, or is it a strategic interpretation that benefits identifiable agents by maintaining the commandment''s unperformable status?',
    'Historical and textual analysis of the evolution of this reading, examining its emergence in relation to shifts in rabbinic authority and institutional power dynamics. Comparison with other readings and their historical contexts.',
    'If it is a strategic interpretation, the ''Snare'' classification is strongly affirmed, highlighting the constructed nature of the constraint. If it is a genuine, non-strategic reading, the classification might still be a Snare due to its structural effects, but the ''naturalness'' of the interpretation would be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Whether the ''performance_only'' reading is a genuine theological interpretation or a constructed one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_commandment__performance_only, theater_ratio, 300, 0.18).
narrative_ontology:measurement(sacr_tr_t600, sacrifice_commandment__performance_only, theater_ratio, 600, 0.15).
narrative_ontology:measurement(sacr_tr_t900, sacrifice_commandment__performance_only, theater_ratio, 900, 0.12).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_commandment__performance_only, theater_ratio, 1200, 0.11).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_commandment__performance_only, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(sacr_be_t300, sacrifice_commandment__performance_only, base_extractiveness, 300, 0.75).
narrative_ontology:measurement(sacr_be_t600, sacrifice_commandment__performance_only, base_extractiveness, 600, 0.8).
narrative_ontology:measurement(sacr_be_t900, sacrifice_commandment__performance_only, base_extractiveness, 900, 0.82).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_commandment__performance_only, base_extractiveness, 1200, 0.83).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.84).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_commandment__performance_only, base_extractiveness, 1900, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sacr_su_t300, sacrifice_commandment__performance_only, suppression_requirement, 300, 0.75).
narrative_ontology:measurement(sacr_su_t600, sacrifice_commandment__performance_only, suppression_requirement, 600, 0.8).
narrative_ontology:measurement(sacr_su_t900, sacrifice_commandment__performance_only, suppression_requirement, 900, 0.85).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_commandment__performance_only, suppression_requirement, 1200, 0.88).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.89).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_commandment__performance_only, suppression_requirement, 1900, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_commandment' kernel. Its 'performance_only' interpretation directly influences and is influenced by the 'study_as_performance' and 'archive_maintenance' readings, as they all grapple with the unperformability of the commandment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
