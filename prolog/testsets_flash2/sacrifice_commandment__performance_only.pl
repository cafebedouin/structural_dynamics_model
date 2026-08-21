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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment: Performance-Only Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'performance-only' reading of the Jewish
 *   sacrifice commandment, which holds that the commandment requires physical
 *   execution and is therefore suspended, not fulfilled, in the absence of
 *   the Temple. This reading, while preserving the literal meaning of the
 *   commandment, directs significant scholarly and communal resources towards
 *   an unperformable act, creating a form of extraction from intellectual
 *   labor and community attention. The high extractiveness reflects 1,900
 *   years of study labor directed at unperformable acts, with scholarly
 *   attention as the primary victim. This is one reading of the
 *   'sacrifice_commandment' kernel.
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
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious_studies/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '4a5b1e35-d2e9-4ecf-9b74-9b765079329c').
narrative_ontology:cs_kernel_codification('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', fixed_text).
narrative_ontology:cs_authority_grounding('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', lineage).
narrative_ontology:cs_interpretation_layer_present('4a5b1e35-d2e9-4ecf-9b74-9b765079329c').
narrative_ontology:cs_reading_relation('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', foundational, sacrifice_requires_physical_execution).
narrative_ontology:cs_axiom_status(sacrifice_requires_physical_execution, holdable).
narrative_ontology:cs_axiom_grounding('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', sacrifice_requires_physical_execution, deontological).
narrative_ontology:cs_axiom('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', foundational, commandment_suspended_without_temple).
narrative_ontology:cs_axiom_status(commandment_suspended_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', commandment_suspended_without_temple, conventional).
narrative_ontology:cs_reference_frame('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4a5b1e35-d2e9-4ecf-9b74-9b765079329c', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, yeshiva_institutions).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, scholarly_attention).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, community_resources).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and transmitters of Halakha, who maintain the doctrine that Temple sacrifices require physical performance. Their careers and intellectual identity are deeply intertwined with the study and preservation of these laws, even in their unperformable state. They benefit from the intellectual labor directed at this field.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Educational institutions that dedicate significant curriculum and resources to the study of sacrificial laws. They benefit from the continued intellectual engagement and the prestige associated with preserving this complex body of knowledge, even if its practical application is suspended.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% Represents the collective intellectual effort and focus of the rabbinic community. Under this reading, a substantial portion of this attention is directed towards a commandment that cannot be physically fulfilled, diverting it from other areas of living Halakha or contemporary ethical challenges. This is a victim in the sense of misallocated intellectual capital.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, scholarly_attention, payer,
    powerless, biographical, trapped, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__performance_only, scholarly_attention).

% Financial and human resources within the Jewish community that support yeshivas and scholarly endeavors. A portion of these resources is directed towards maintaining the study of unperformable sacrificial laws, which, under this reading, yields no direct spiritual or practical benefit in the present, representing a cost.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, community_resources, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__performance_only, community_resources).

% Individuals who support religious institutions and adhere to rabbinic authority. They bear the cost of maintaining a scholarly tradition that, in this reading, emphasizes the unfulfillable nature of a central commandment, potentially leading to a sense of spiritual incompleteness or a diversion of their own devotional energy.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, lay_adherents, payer,
    moderate, biographical, constrained, local).

% Groups advocating for the immediate rebuilding of the Temple and resumption of sacrifices. This reading, by emphasizing the physical requirement and current suspension, implicitly pushes back against their efforts, framing them as premature or halakhically unsound. They are excluded from the mainstream discourse that accepts the current suspension.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_activists, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a consistent halakhic understanding of the sacrifice commandment, ensuring that the community does not mistakenly believe study alone fulfills the physical requirement, thus preserving the integrity of the commandment for a future Temple era.
% TRANSFER_FUNCTION: Transfers scholarly attention and community resources towards the rigorous study of unperformable sacrificial laws, from the broader community to rabbinic institutions and scholars.
% ABSENT_VOICES: Those who believe that the study of sacrifice laws *is* a form of spiritual performance, or that the commandment is fulfilled through other means, are marginalized. Their voices would challenge the premise that physical execution is the sole mode of fulfillment, arguing for a more expansive interpretation.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the vast intellectual and institutional infrastructure dedicated to the study of unperformable sacrificial laws would reorient. Scholarly attention and community resources would likely shift to other areas of Halakha or contemporary Jewish life, and the theological understanding of divine obligation would fundamentally change.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, the Jewish community faced the challenge of how to relate to the central commandment of sacrifice, which could no longer be physically performed, without abandoning it entirely.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities universally attest that the problem of relating to the suspended commandment remains live. Historical texts and ongoing scholarly discourse corroborate the continuous engagement with this problem since the Temple's destruction.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the intellectual and communal investment in studying these laws yields no direct, performative spiritual benefit in the present, under this reading. Suppression is high because the rabbinic authority structure actively enforces this interpretation, marginalizing alternative views that might seek to 'fulfill' the commandment through non-physical means. Theater ratio is low because the study is genuinely rigorous and not merely performative; its 'theatricality' is in its lack of immediate practical application, not in its execution.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars, this is a necessary preservation of Halakha's integrity. From the perspective of scholarly attention and community resources, it is a significant diversion of effort and capital towards an unfulfillable task. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and yeshiva institutions are beneficiaries, as their authority and existence are sustained by this intellectual tradition. Scholarly attention and community resources are victims, as they are 'extracted' and directed towards an unperformable commandment. Lay adherents bear a diffuse cost. Messianic activists are excluded, as their alternative approach is suppressed by this reading's dominance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_fulfillment_validity,
    'Are there valid halakhic interpretations that allow for non-physical fulfillment of the sacrifice commandment in the absence of the Temple?',
    'A shift in mainstream rabbinic consensus or the emergence of a widely accepted halakhic ruling that redefines ''fulfillment'' for this commandment.',
    'If alternative fulfillments are validated, the extractiveness of this constraint would decrease significantly, as the ''victim'' (scholarly attention, community resources) would be seen as directed towards a performable, beneficial act. The constraint might reclassify from snare to tangled_rope or even rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fulfillment_validity, conceptual, 'Whether the commandment''s fulfillment is strictly tied to physical performance or can be achieved through other means.').

omega_variable(
    mandatrophy_of_study_focus,
    'Has the intense focus on unperformable sacrificial laws outlived its original mandate of preserving knowledge for a future Temple, becoming an end in itself?',
    'Empirical analysis of scholarly output and institutional mission statements over time, assessing whether the emphasis has shifted from ''preservation for future'' to ''study for its own sake'' without a clear present benefit.',
    'If the mandate has atrophied, the constraint''s classification would lean more strongly towards snare or piton, as the coordination function (preserving knowledge) would be seen as secondary to the extraction of scholarly labor for an inertial tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_study_focus, empirical, 'Whether the study of unperformable laws has become an inertial tradition rather than a functional preparation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of Temple) or internalized (scholarly identity tied to this specific interpretation)?',
    'Post-exit suppression trajectory: if scholars continue to prioritize this study even if alternative interpretations gain traction, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the scholarly community carries the suppression with them after any external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in scholarly adherence to this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__performance_only, theater_ratio, 500, 0.12).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__performance_only, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.09).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_commandment__performance_only, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__performance_only, base_extractiveness, 500, 0.75).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__performance_only, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.83).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_commandment__performance_only, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__performance_only, suppression_requirement, 500, 0.85).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__performance_only, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.89).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_commandment__performance_only, suppression_requirement, 1950, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
