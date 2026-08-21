% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice Commandment: Archive Maintenance Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents one reading of the 'sacrifice commandment'
 *   kernel, specifically the 'archive_maintenance' reading. In this view, the
 *   divine commandment to offer sacrifices, currently impossible due to the
 *   absence of the Temple, is fulfilled by diligently studying and preserving
 *   the technical laws of sacrifice. This ensures the knowledge is available
 *   for a future, messianic restoration of the Temple, rather than being a
 *   form of present worship or a suspended obligation. It functions as a
 *   scaffold, providing temporary support for a future state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.45).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.2).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice Commandment: Archive Maintenance Reading").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious_studies/halakhic_theory/commitment_system_analysis").

narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '67d5d13e-4a34-431e-a860-3c6a85553374').
narrative_ontology:cs_kernel_codification('67d5d13e-4a34-431e-a860-3c6a85553374', fixed_text).
narrative_ontology:cs_authority_grounding('67d5d13e-4a34-431e-a860-3c6a85553374', lineage).
narrative_ontology:cs_interpretation_layer_present('67d5d13e-4a34-431e-a860-3c6a85553374').
narrative_ontology:cs_reading_relation('67d5d13e-4a34-431e-a860-3c6a85553374', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('67d5d13e-4a34-431e-a860-3c6a85553374', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_axiom('67d5d13e-4a34-431e-a860-3c6a85553374', foundational, knowledge_preservation_is_divine_service).
narrative_ontology:cs_axiom_status(knowledge_preservation_is_divine_service, holdable).
narrative_ontology:cs_axiom_grounding('67d5d13e-4a34-431e-a860-3c6a85553374', knowledge_preservation_is_divine_service, theological).
narrative_ontology:cs_axiom('67d5d13e-4a34-431e-a860-3c6a85553374', foundational, physical_performance_is_currently_impossible).
narrative_ontology:cs_axiom_status(physical_performance_is_currently_impossible, holdable).
narrative_ontology:cs_axiom_grounding('67d5d13e-4a34-431e-a860-3c6a85553374', physical_performance_is_currently_impossible, empirically_contingent).
narrative_ontology:cs_reference_frame('67d5d13e-4a34-431e-a860-3c6a85553374', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('67d5d13e-4a34-431e-a860-3c6a85553374', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('67d5d13e-4a34-431e-a860-3c6a85553374', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_generations).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, halakhic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, contemporary_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary agents who engage in the study and preservation of sacrificial laws. Their careers and intellectual identities are often tied to this field of study, even without an active Temple. They maintain the archive of knowledge.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% The ultimate beneficiaries of this reading, as the preserved knowledge is intended for their use in a restored Temple. They bear no present cost but receive the future utility.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_generations, beneficiary,
    powerless, generational, analytical, global).

% Supports the institutions and scholars dedicated to this study, often through donations or communal resources. While they may not directly engage in the study, they bear the cost of its maintenance without direct present spiritual benefit from the sacrifice itself.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, contemporary_community, payer,
    moderate, biographical, constrained, local).

% Individuals or groups who believe in the imminent restoration of the Temple and may seek to perform sacrifices now, or interpret study as a direct fulfillment. This reading excludes their present-day performance claims.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, messianic_aspirants, excluded,
    moderate, immediate, identity_locked, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transmission of complex ritual knowledge, ensuring that the technical details of Temple sacrifices are not lost during periods of non-performance, enabling future restoration.
% TRANSFER_FUNCTION: Transfers intellectual effort and communal resources from the contemporary community to the maintenance of a scholarly archive, for the benefit of future generations who may need to implement these laws.
% ABSENT_VOICES: Those who believe that the commandment is either suspended entirely without a Temple (performance_only reading) or that study itself constitutes performance (study_as_performance reading) would object to the 'archive maintenance' framing, arguing it either understates or overstates the present obligation. Messianic aspirants would object to the deferral of performance.
% DISAPPEARANCE_RATIONALE: If the commitment to maintaining this archive vanished, the intricate knowledge of sacrificial laws would likely degrade and be lost over generations, making a future Temple restoration (if it were to occur) significantly more difficult or impossible to implement according to tradition.
% FOUNDING_PROBLEM: The destruction of the Temple created a crisis of continuity for the sacrificial commandments: how to fulfill or preserve the divine obligation when physical performance is impossible.
% FOUNDING_PROBLEM_CORROBORATION: Halakhic authorities across various traditions corroborate the ongoing challenge of maintaining ritual knowledge in the absence of a Temple. Historical texts and rabbinic responsa from outside the immediate scholarly beneficiaries attest to the continuous concern for preserving this knowledge for future use.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is moderate (0.45) because resources are diverted to study without immediate ritual fulfillment, creating a cost for the contemporary community for a future benefit. Suppression is low (0.20) as participation is largely voluntary, driven by religious commitment rather than coercion. Theater ratio is low (0.10) because the study is genuinely aimed at knowledge preservation, not merely performative. The 'scaffold' classification reflects its transitional nature, supporting a future state.
 *
 * PERSPECTIVAL GAP:
 *   Halakhic scholars, deeply invested in the intellectual tradition, experience this as a vital, meaningful activity (low d). The contemporary community, bearing the costs without direct present spiritual benefit, might experience a higher d. Future generations, as pure beneficiaries, would have a very low d. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are the agenda-setters, defining and maintaining the scope of this study. Future generations are the primary beneficiaries, receiving the preserved knowledge. The contemporary community acts as a payer, supporting the scholarly endeavor. Messianic aspirants are excluded, as their desire for immediate performance is not accommodated by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_utility_discount_rate,
    'What is the appropriate discount rate for the future utility of preserved knowledge, given the uncertainty of Temple restoration?',
    'Theological and philosophical discourse on eschatology and intergenerational obligation, combined with sociological studies of knowledge transmission in long-term, high-uncertainty contexts.',
    'A high discount rate would increase the effective extractiveness for the contemporary community, as the present cost outweighs the highly uncertain future benefit. A low discount rate would reduce extractiveness, validating the long-term investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_utility_discount_rate, conceptual, 'Uncertainty in the value of future knowledge preservation.').

omega_variable(
    mandate_scope_ambiguity,
    'Is the divine mandate for ''sacrifice commandment'' solely about physical performance, or does it inherently include the preservation of knowledge for future performance?',
    'Further halakhic and theological interpretation, potentially informed by new textual discoveries or shifts in communal understanding of divine will.',
    'If the mandate is solely physical, this reading''s justification as a ''scaffold'' is weakened, potentially reclassifying it as a ''piton'' (performing a vestigial function). If knowledge preservation is integral, the scaffold classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_scope_ambiguity, conceptual, 'Ambiguity in the scope of the divine commandment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.1).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__archive_maintenance, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__archive_maintenance, theater_ratio, 60, 0.1).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_commandment__archive_maintenance, theater_ratio, 80, 0.1).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__archive_maintenance, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__archive_maintenance, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__archive_maintenance, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(sacr_be_t80, sacrifice_commandment__archive_maintenance, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__archive_maintenance, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sacr_su_t20, sacrifice_commandment__archive_maintenance, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(sacr_su_t40, sacrifice_commandment__archive_maintenance, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(sacr_su_t60, sacrifice_commandment__archive_maintenance, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(sacr_su_t80, sacrifice_commandment__archive_maintenance, suppression_requirement, 80, 0.2).
narrative_ontology:measurement(sacr_su_t100, sacrifice_commandment__archive_maintenance, suppression_requirement, 100, 0.2).


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
