% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Preservation Ritual (Hybrid Atrophy Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A ritual complex originally developed to preserve survival-competence and
 *   threat-recognition across generations following collective catastrophe.
 *   Under modernity, the operational survival function has atrophied; the
 *   ritual persists as mourning-practice and identity-performance, maintained
 *   by community inertia rather than by demonstrated adaptive value. The
 *   present generation inherits costly obligations without receiving the
 *   historical payoff, while ritual administrators steward a form whose
 *   original purpose they no longer deliver. This constraint is the
 *   hybrid_atrophy_reading of the catastrophe_memory_preservation kernel, one
 *   of three contested readings. Sibling readings include the
 *   survival_competence_reading (ongoing operational coordination) and the
 *   mourning_practice_reading (pure symbolic identity coordination without
 *   victimization).
 *
 * KEY AGENTS:
 *   - ritual_administrators: agenda_setter (organized/identity_locked) â inherited stewardship without rent capture
 *   - in_group_identity_bearers: beneficiary (organized/identity_locked) â diffuse identity benefit, no concentrated extraction
 *   - present_generation_practitioners: payer (moderate/identity_locked) â bear costs without adaptive payoff
 *   - memory_studies_scholars: observer (analytical/analytical) â external attestation of functional atrophy
 *   - secularized_descendants: excluded (moderate/mobile) â exited but absent from negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.4).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.35).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Preservation Ritual (Hybrid Atrophy Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '25ea11a2-5e05-4b0b-b0ed-19b3af466404').
narrative_ontology:cs_kernel_codification('25ea11a2-5e05-4b0b-b0ed-19b3af466404', distributed).
narrative_ontology:cs_authority_grounding('25ea11a2-5e05-4b0b-b0ed-19b3af466404', practice).
narrative_ontology:cs_interpretation_layer_present('25ea11a2-5e05-4b0b-b0ed-19b3af466404').
narrative_ontology:cs_reading_relation('25ea11a2-5e05-4b0b-b0ed-19b3af466404', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('25ea11a2-5e05-4b0b-b0ed-19b3af466404', catastrophe_memory_preservation__mourning_practice_reading, influences).
narrative_ontology:cs_axiom('25ea11a2-5e05-4b0b-b0ed-19b3af466404', foundational, survival_competence_atrophy_thesis).
narrative_ontology:cs_axiom_status(survival_competence_atrophy_thesis, holdable).
narrative_ontology:cs_axiom_grounding('25ea11a2-5e05-4b0b-b0ed-19b3af466404', survival_competence_atrophy_thesis, empirically_contingent).
narrative_ontology:cs_axiom('25ea11a2-5e05-4b0b-b0ed-19b3af466404', foundational, ritual_continuation_without_survival_payoff_is_inertial_extraction).
narrative_ontology:cs_axiom_status(ritual_continuation_without_survival_payoff_is_inertial_extraction, holdable).
narrative_ontology:cs_axiom_grounding('25ea11a2-5e05-4b0b-b0ed-19b3af466404', ritual_continuation_without_survival_payoff_is_inertial_extraction, instrumental).
narrative_ontology:cs_reference_frame('25ea11a2-5e05-4b0b-b0ed-19b3af466404', operational_survival_transmission).
narrative_ontology:cs_drift_state('25ea11a2-5e05-4b0b-b0ed-19b3af466404', modernity_secularization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('25ea11a2-5e05-4b0b-b0ed-19b3af466404', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_bearers).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherited the duty to officiate and transmit ritual form after catastrophe. They do not capture material rents; their authority depends on performing stewardship of a practice whose original survival function they privately acknowledge as lost or transformed. They could in principle discontinue the ritual, but doing so would dissolve their social position and the community's identity anchor.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_administrators, agenda_setter,
    organized, generational, identity_locked, regional).

% Receive diffuse social belonging and boundary-affirmation from the ritual's continued performance. The benefit is collective and non-appropriable: no individual or faction captures the identity value as concentrated rent. Their stake is continuity of group definition, not operational survival competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_bearers, beneficiary,
    organized, generational, identity_locked, regional).

% Bear the material and temporal costs of ritual performance and preparation, inheriting obligations framed as communal survival but receiving no operational survival competence in return. Exit is blocked by identity fusion with the group and the narrative that discontinuity equals cultural death.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Document the historical function of catastrophe rituals and measure the gap between original survival transmission and contemporary symbolic performance. They operate outside the community's identity constraints and can attest to functional atrophy without bearing ritual costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, memory_studies_scholars, observer,
    analytical, civilizational, analytical, national).

% Descendants who have left the community or abandoned ritual practice. They would argue that resources devoted to ritual maintenance should be redirected to present-tense adaptive challenges, but their absence from the ritual sphere removes them from the community's decision-making about practice continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, secularized_descendants, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically coordinated intergenerational transfer of survival-relevant knowledge and threat-recognition competence following collective catastrophe; currently coordinates social cohesion and boundary-maintenance through symbolic mourning-performance.
% TRANSFER_FUNCTION: Moves time, material resources, and cognitive attention from present-generation practitioners to the maintenance of ritual form and collective memory infrastructure, without returning operational survival competence.
% ABSENT_VOICES: Secularized descendants and pragmatic modernizers who would argue for diverting ritual maintenance resources to present-tense adaptive challenges; they are excluded by the identity-frame that treats ritual discontinuity as communal death.
% DISAPPEARANCE_RATIONALE: The ritual form anchors in-group boundary maintenance and intergenerational identity claims; its disappearance would force the community to reconstruct identity coherence through other means, though the original survival function would remain unaffected.
% FOUNDING_PROBLEM: Preservation of survival-competence and threat-recognition capacity across generations following societal catastrophe, when written records are absent and environmental threats are severe.
% FOUNDING_PROBLEM_CORROBORATION: Ritual studies scholars and historical anthropologists attest that the original operational survival function has been superseded by modern infrastructure (literacy, medicine, state protection); these sources sit outside the beneficiary set of in-group identity maintenance. Ritual administrators may claim continuity of function, but independent ethnographic analysis corroborates atrophy.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.4, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.40) and declining over the interval because the original survival-transfer function has decayed, leaving only the diffuse costs of ritual maintenance. Theater_ratio is high (0.72) because most current activity is performative maintenance of an atrophied function rather than operational competence transfer. Suppression is moderate-low (0.35): enforcement is social-normative and inertial, not actively coercive. Accessibility_collapse is moderate (0.50) because physical exit is possible but socially and identity-costly. Resistance is low (0.30): dissatisfaction is diffuse and internalized rather than organized. The claim/metric gap is deliberate: the constraint is claimed as piton (atrophied/inertial) while the metrics describe a still-moderately-extractive structure whose extraction is declining â the engine measures that trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The ritual_administrator seat experiences the constraint as stewardship of a fragile inheritance; the present_generation_practitioner seat experiences it as an unchosen burden whose justification has evaporated. The memory_studies_scholar seat sees theatrical maintenance of a dead function. The engine computes per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   in_group_identity_bearers are declared beneficiaries: they receive diffuse belonging and boundary-affirmation, so their directionality sits near the beneficiary end, dampening effective extraction. present_generation_practitioners are declared victims: they bear material and temporal costs without adaptive payoff, so their directionality sits near the target end, amplifying effective extraction. ritual_administrators are not declared as either beneficiaries or victims; their directionality derives from their identity_locked exit and agenda_setter role, placing them near symmetric but slightly toward target because they are structurally bound to a form they do not capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â intergenerational survival competence after catastrophe â is dead. Modern literacy, medicine, and state protection have superseded the ritual's operational function. However, the constraint persists because no party is hurt enough to fix it and no party captures enough to maintain it actively. Declaring it a piton rather than a snare is essential because there is no concentrated beneficiary extracting rents; the identity benefit is diffuse and non-appropriable. Declaring it a piton rather than a scaffold is essential because there is no sunset clause or transitional justification â the persistence is inertial, not planned. The mandatrophy_resolved flag documents that the mandate has outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the ritual''s current function better understood as atrophied survival competence (this reading), pure mourning practice (mourning_practice_reading), or ongoing operational threat-recognition (survival_competence_reading)?',
    'Comparative ethnographic analysis measuring operational competence transfer versus symbolic identity performance across ritual iterations; historical demography of the community''s actual threat-exposure.',
    'If survival competence is still operationally transferred, the constraint is not a piton but a rope or tangled_rope; if pure mourning without victimization, extraction may be lower than this reading claims and the constraint may be a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which kernel reading correctly characterizes the ritual''s current structural function').

omega_variable(
    extractive_atrophy_trajectory,
    'Does the measured extractiveness genuinely decline over generational time, or does the extraction modality shift from material to cognitive (attention, guilt, identity-obligation)?',
    'Longitudinal cohort analysis tracking resource expenditure and psychological burden across generations, normalized for economic capacity.',
    'If extraction shifts modality rather than declining, the temporal measurement series misrepresents the constraint''s stability; it may be a stable snare rather than a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_atrophy_trajectory, empirical, 'Whether extraction declines or merely transforms in modality').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural or internalized for present-generation practitioners?',
    'Post-exit trajectory study of practitioners who discontinue ritual participation: if social costs are survivable and internal distress is high, suppression is internalized; if social costs are severe, suppression is structural.',
    'Internalized suppression raises effective extraction beyond the structural measure; structural suppression with active enforcers would indicate the constraint is actively enforced and potentially a snare or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for ritual adherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_tr_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_tr_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.65).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_tr_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 50, 0.69).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.72).

% Extraction over time
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_be_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_be_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_be_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 50, 0.43).
narrative_ontology:measurement(catastrophe_memory_preservation__hybrid_atrophy_reading_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__hybrid_atrophy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_preservation kernel, which decomposes into three structurally distinct claims: hybrid_atrophy_reading (atrophied survival function, present piton), survival_competence_reading (ongoing operational coordination), and mourning_practice_reading (pure symbolic identity coordination). Each reading instantiates a different constraint with distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
