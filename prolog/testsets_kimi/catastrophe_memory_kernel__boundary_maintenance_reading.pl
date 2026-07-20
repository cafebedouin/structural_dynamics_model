% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Ritual Mourning as Group Boundary Enforcement
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the boundary_maintenance_reading of
 *   the catastrophe_memory_kernel. It treats shared mourning-practice not
 *   merely as commemoration but as an active mechanism of group boundary
 *   enforcement, where the ritual coordination of grief produces solidarity
 *   for the in-group at the cost of individual autonomy and out-group
 *   exclusion. The kernel is contested: sibling readings interpret the same
 *   ritual complex as encoding survival competence, preserving symbolic
 *   continuity, or transmitting intergenerational trauma. This reading
 *   isolates the boundary-enforcement function as a structurally distinct
 *   claim with its own Îµ.
 *
 * KEY AGENTS:
 *   - Ritual guardians (agenda_setter / organized / identity_locked) â prescribe and enforce mourning norms
 *   - In-group community (beneficiary / moderate / identity_locked) â receives solidarity and distinct identity
 *   - Grieving individuals (payer / powerless / identity_locked) â bear autonomy costs of conformity
 *   - Out-group members (excluded / powerless / trapped) â bear social severance
 *   - Memory studies scholar (observer / analytical) â external analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.62).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.66).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Ritual Mourning as Group Boundary Enforcement").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'e4fbb325-eb12-4c3d-b169-b3f26febb1fe').
narrative_ontology:cs_kernel_codification('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', distributed).
narrative_ontology:cs_authority_grounding('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', practice).
narrative_ontology:cs_interpretation_layer_present('e4fbb325-eb12-4c3d-b169-b3f26febb1fe').
narrative_ontology:cs_reading_relation('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', foundational, communal_boundary_priority_over_individual_grief).
narrative_ontology:cs_axiom_status(communal_boundary_priority_over_individual_grief, holdable).
narrative_ontology:cs_axiom_grounding('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', communal_boundary_priority_over_individual_grief, conventional).
narrative_ontology:cs_axiom('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', foundational, ritual_exclusion_constitutes_collective_identity).
narrative_ontology:cs_axiom_status(ritual_exclusion_constitutes_collective_identity, holdable).
narrative_ontology:cs_axiom_grounding('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', ritual_exclusion_constitutes_collective_identity, conventional).
narrative_ontology:cs_reference_frame('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', communal_integrity_through_exclusionary_mourning).
narrative_ontology:cs_drift_state('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', contemporary_pluralist_context, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e4fbb325-eb12-4c3d-b169-b3f26febb1fe', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, grieving_individuals).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious or communal leaders who prescribe correct mourning performance, enforce distinction between in-group and out-group grief, and derive authority from maintaining collective boundaries after catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_guardians, agenda_setter,
    organized, generational, identity_locked, regional).

% Members of the community who receive social cohesion and distinct identity through participation in shared mourning; their belonging is confirmed by conforming to ritual norms and by the exclusion of outsiders.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_community, beneficiary,
    moderate, biographical, identity_locked, regional).

% Individuals experiencing loss who must subordinate personal grief expressions to the prescribed ritual form; non-conforming mourning risks social sanction and loss of community standing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, grieving_individuals, payer,
    powerless, immediate, identity_locked, local).

% Non-members who are barred from participating in the mourning solidarity and whose offers of condolence or shared grief are rejected as boundary violations; they bear the cost of social severance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_members, excluded,
    powerless, immediate, trapped, regional).

% Academic observer studying how catastrophe memory is ritually mobilized to sustain group boundaries; neither benefits from nor is harmed by the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, memory_studies_scholar, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_community).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains group boundaries and in-group solidarity by prescribing uniform mourning practices that visibly distinguish members from non-members after collective catastrophe.
% TRANSFER_FUNCTION: Transfers individual autonomy over grief expression and intergroup openness into collective boundary clarity and in-group cohesion; the cost is borne by individuals whose mourning does not fit the form and by out-groups who are symbolically excluded.
% ABSENT_VOICES: Out-group members and individuals experiencing non-normative grief are structurally excluded from defining the ritual; their objections are delegitimized as improper mourning or as threats to group identity.
% DISAPPEARANCE_RATIONALE: If the shared mourning-practice constraint disappeared, the group's primary boundary mechanism would weaken; alternative grief expressions would emerge, intergroup barriers would soften, and the social distinction between in-group and out-group would require new rituals to maintain.
% FOUNDING_PROBLEM: The community needed to maintain cohesion and distinctiveness after catastrophic loss, preventing dispersal or assimilation into surrounding groups.
% FOUNDING_PROBLEM_CORROBORATION: Community historians and religious authorities attest the founding catastrophe and dispersal risk. Independent historians and sociologists outside the benefiting community debate whether the boundary function was the original intent or a later overlay; some corroborate survival necessity, others note the ritual's expansion beyond catastrophe contexts.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate: the ritual genuinely coordinates collective identity but asymmetrically extracts autonomy from individuals and recognition from out-groups. Suppression (0.66) reflects active social sanction against non-conforming grief and the exclusion of outsiders. Theater ratio (0.46) acknowledges that a substantial portion of mourning performance is directed toward visible boundary-marking rather than personal bereavement. Accessibility collapse (0.60) captures that alternatives (personalized grief, intergroup mourning) are socially available but costly. Resistance (0.45) records occasional individual deviation and scholarly critique but not organized opposition. The measurement series track gradual intensification of extraction and theatricality as the ritual becomes reflexive.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (ritual guardians) experiences the constraint as necessary coordination preserving communal existence; the payer seat (grieving individuals) experiences it as a suppression of authentic grief in service of collective performance; the excluded seat (out-group members) experiences it as social severance. The engine computes these divergences from structural data â identical ritual forms produce opposed directionalities depending on whether the agent's position is inside the boundary being maintained or outside it.
 *
 * DIRECTIONALITY LOGIC:
 *   In-group community is the structural beneficiary: participation in the ritual yields solidarity and identity confirmation, producing a low directionality. Grieving individuals and out-group members are the structural victims: they bear the costs of conformity and exclusion, producing high directionality. Ritual guardians sit closer to the beneficiary end because their authority is constituted by the constraint's persistence, though they pay maintenance costs. The scholar is analytical with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing communal dispersal after catastrophe â may be dead or transformed, yet the boundary-maintenance ritual persists. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags the constraint as a potential zombie: the world would rearrange if it vanished, but the arrangement may no longer solve the problem it was built for. This prevents mislabeling the ritual as pure coordination (Rope) by exposing that its persistence outlives its founding justification, while the maintained extraction prevents classification as inertial Piton because in-group community still actively captures solidarity gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-conforming grief structural (enforced by ritual guardians through explicit sanction) or internalized (mourners believe conforming grief is the only legitimate form)?',
    'Post-exit observation: if individuals who leave the community continue to suppress idiosyncratic grief, the mechanism is partially internalized; if suppression drops immediately upon exit, it is structural.',
    'If internalized, effective suppression exceeds the structural measure, deepening the extraction from individual autonomy and potentially shifting the computed type toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in ritual mourning').

omega_variable(
    founding_intent_vs_boundary_function,
    'Was the ritual''s boundary-maintenance function present at its founding, or did it emerge later as an overlay on originally commemorative or survival-oriented practice?',
    'Historical and textual analysis of the ritual''s earliest documented forms, comparing them to contemporary boundary-marking elements.',
    'If boundary maintenance is a later overlay, the constraint exhibits extraction accumulation (Goodhart drift) layered onto an original coordination function, supporting a stronger Tangled Rope classification; if original, the extraction is more intrinsic to the design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_intent_vs_boundary_function, conceptual, 'Whether boundary enforcement was original intent or emergent overlay').

omega_variable(
    kernel_reading_separability,
    'Does the boundary_maintenance reading isolate a structurally distinct constraint, or is it observationally equivalent to the symbol_continuity_reading under a different description?',
    'Comparative analysis: if the symbol continuity reading shows markedly lower extractiveness and no victim set, the readings are structurally distinct; if metrics converge, they are the same constraint under different labels.',
    'If the readings are not separable, this story should merge with symbol_continuity_reading; if separable, the Îµ-invariance principle is upheld.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separability, conceptual, 'Structural separability of boundary maintenance from symbol continuity reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t12, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(cata_tr_t36, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 36, 0.38).
narrative_ontology:measurement(cata_tr_t48, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 48, 0.42).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 60, 0.46).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cata_be_t12, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(cata_be_t36, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 36, 0.58).
narrative_ontology:measurement(cata_be_t48, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 48, 0.6).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t12, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(cata_su_t36, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 36, 0.62).
narrative_ontology:measurement(cata_su_t48, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 48, 0.64).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 60, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
