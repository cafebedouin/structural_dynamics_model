% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Mourning Practice Ritual Preservation Reading
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This reading instantiates the mourning-practice interpretation of the
 *   catastrophe-memory-preservation kernel. Under this reading, ritual
 *   preserves SYMBOLIC CONTINUITY and collective identity through voluntary
 *   commemorative practice — the ritual's function is to mark the community's
 *   relationship to a defining loss and to transmit that meaning across
 *   generations. The constraint operates as a rope: it solves a genuine
 *   coordination problem (how does a community maintain narrative identity
 *   across generations after catastrophe?) and does so without suppression or
 *   coerced participation. Participants volunteer to enact the ritual;
 *   younger members are present and absorb meaning through structural
 *   participation, not through extraction or identity-locked compulsion. The
 *   beneficiary is the in-group cohesion and narrative continuity itself —
 *   not an external agent, but the community's capacity to remain coherent
 *   around a shared past. Extractiveness is low: ritual specialists may hold
 *   status or deference, but they do not siphon material resources or enforce
 *   compliance through coercion. The theater ratio is moderate-to-high (0.42
 *   at interval end) because the ritual's entire point is symbolic
 *   performance — marking, witnessing, and transmitting meaning through form
 *   and presence. This is not theater in the sense of false function
 *   (inertial maintenance of a degraded constraint); it is theater as the
 *   PRIMARY function itself. The accessibility collapse is moderate (0.35)
 *   because alternatives to the specific ritual form exist (secular
 *   commemoration, written history, family narrative) but the collective
 *   enactment provides a unique coordination mechanism that those
 *   alternatives do not. Resistance is very low (0.12) because participation
 *   is voluntary and the constraint is rarely actively resisted — the tension
 *   arises from generational change and secularization, not from active
 *   opposition to the ritual form itself.
 *
 * KEY AGENTS:
 *   - Ritual practitioners: voluntary participants who maintain the practice and are beneficiaries of the identity continuity it provides
 *   - Younger generation: inherited beneficiaries of the transmitted meaning; their participation is expected but not coerced
 *   - Ritual specialists (clergy, remembrancers, historians): institutional agenda-setters who steward the symbolic form and decide what is commemorated
 *   - Affected catastrophe survivors: observers whose lived experience is the referent; their presence is honored but their stake is distinct from the coordination function
 *   - Secular/external analysts: excluded from the ritual's self-understanding but present as external observers documenting its identity-maintenance function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.08).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Mourning Practice Ritual Preservation Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, '098b0516-7f8a-43b9-9396-7524015322fe').
narrative_ontology:cs_kernel_codification('098b0516-7f8a-43b9-9396-7524015322fe', implicit).
narrative_ontology:cs_authority_grounding('098b0516-7f8a-43b9-9396-7524015322fe', practice).
narrative_ontology:cs_interpretation_layer_present('098b0516-7f8a-43b9-9396-7524015322fe').
narrative_ontology:cs_reading_relation('098b0516-7f8a-43b9-9396-7524015322fe', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('098b0516-7f8a-43b9-9396-7524015322fe', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('098b0516-7f8a-43b9-9396-7524015322fe', foundational, ritual_is_identity_practice_not_operational_training).
narrative_ontology:cs_axiom_status(ritual_is_identity_practice_not_operational_training, holdable).
narrative_ontology:cs_axiom_grounding('098b0516-7f8a-43b9-9396-7524015322fe', ritual_is_identity_practice_not_operational_training, deontological).
narrative_ontology:cs_axiom('098b0516-7f8a-43b9-9396-7524015322fe', secondary, voluntary_participation_constitutes_genuine_coordination).
narrative_ontology:cs_axiom_status(voluntary_participation_constitutes_genuine_coordination, holdable).
narrative_ontology:cs_axiom_grounding('098b0516-7f8a-43b9-9396-7524015322fe', voluntary_participation_constitutes_genuine_coordination, conventional).
narrative_ontology:cs_reference_frame('098b0516-7f8a-43b9-9396-7524015322fe', identity_continuity_through_ritual_enactment).
narrative_ontology:cs_drift_state('098b0516-7f8a-43b9-9396-7524015322fe', contemporary_secular_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('098b0516-7f8a-43b9-9396-7524015322fe', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, in_group_cohesion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, ritual_practitioners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, younger_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in mourning rituals — commemorations, memorial practices, collective remembrance ceremonies — that reaffirm shared identity and symbolic belonging. They gather, enact the prescribed forms, and transmit the meaning to younger members. Participation is largely voluntary; exit from the ritual is feasible (individuals can decline or reduce participation) but carries social friction within the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_practitioners, beneficiary,
    organized, generational, mobile, local).

% Inherit the ritual form through transmission during commemorations. They learn who and what the community grieves, what events shaped the group's identity, and how symbolic continuity is maintained. Their participation is expected but not formally coerced; they absorb meaning through presence and repetition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, younger_generation, beneficiary,
    moderate, biographical, constrained, local).

% Steward the ritual form: remembrancers, historians, clergy, elder arbiters who decide what is commemorated, how, and when. They maintain the symbolic script and adjudicate which events count as 'catastrophe worthy of remembrance.' Their role is continuity management, not extraction; they carry professional or spiritual identity fused to the role.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_specialists, agenda_setter,
    institutional, generational, identity_locked, local).

% Those who directly experienced the catastrophe the ritual commemorates. Their lived experience is the referent the ritual encodes; they are present during the practice but their stake is distinct — the ritual memorializes their loss and suffering, not operational competence to prevent recurrence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, affected_catastrophe_survivors, observer,
    powerless, biographical, trapped, local).

% Scholars, historians, anthropologists who study the ritual from outside the community. They would argue that the ritual's primary function is symbolic identity-maintenance, not transmission of survival-relevant threat-recognition. They are not participants and their analytical framing is not integrated into the ritual's self-understanding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, secular_or_external_analysts, excluded,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assembles the community at regular intervals around a shared catastrophe memory; establishes a collective narrative of loss, resilience, and identity rooted in that event; reaffirms who 'we' are through ritual enactment rather than through semantic exposition or operational instruction.
% TRANSFER_FUNCTION: Moves symbolic continuity and group identity across generations through performance and presence. What is transferred is the meaning of the past event and the community's definition of itself in relation to that past, not operational procedures or threat-recognition mechanisms.
% ABSENT_VOICES: Secular members of the community who question the ritual's contemporary utility would argue that repeating the form is theater rather than coordination; external analysts would frame it as identity-maintenance rather than survival-relevant practice. They are excluded from deciding what counts as 'catastrophe worthy' or how the commemoration proceeds.
% DISAPPEARANCE_RATIONALE: If the mourning ritual disappeared, the community's internal narrative continuity would degrade; younger members would lack the structured opportunity to inherit the symbolic meaning; the group's collective identity would weaken or require explicit re-narration outside the ritual form. The community would reorganize around alternative identity markers or lose coherence.
% FOUNDING_PROBLEM: A catastrophic event occurred (war, genocide, famine, natural disaster) that shaped the community's defining loss. How is that loss remembered and transmitted so the community's identity, forged in relation to that event, persists across generations?
% FOUNDING_PROBLEM_CORROBORATION: Ritual practitioners and specialists attest that intergenerational transmission of the catastrophe's meaning is ongoing and necessary. Younger members report that their understanding of the group's identity is rooted in ritual participation. External historians and anthropologists confirm that such rituals are cross-culturally documented as identity-maintenance mechanisms and that the founding problem (identity continuity after catastrophe) remains live wherever the group persists.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because the ritual operates on voluntary participation and produces no material transfer from participants to specialists. The specialists hold institutional status (power to decide what is commemorated) but extract no rents or compliance taxes. The theater ratio is high (0.42) because the ritual IS primarily performance — its function is to mark and transmit meaning through form, not to accomplish an operational task. The ratio does not indicate degradation (inertial maintenance of a dead function) but rather the ritual's intended design: symbolic continuity is enacted through repeated ceremonial form. Suppression is minimal (0.08) because exit is available and low-cost — an individual can decline participation without catastrophic personal consequence, though participation is normatively expected. The measurement series show extractiveness and theater ratio stable over 100 time units, with suppression slightly increasing as modernity creates greater distance from the catastrophe itself, making the younger generation's participation require modest social pressure to maintain. Accessibility collapse (0.35) reflects that alternatives to the specific ritual form exist and are accessible (individuals can remember privately, communities can use secular commemoration) but the collective enactment is irreplaceable in its coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The ritual specialist seat and the younger-generation seat should compute differently from the engine's structural derivation. From the specialist's position, the ritual is a vital institutional function — they hold decision-making power over what is commemorated and how, and they carry strong professional identity tied to the role. From the younger generation's position, participation is expected (constrained exit) and the benefit is passive (inheriting meaning rather than actively choosing it). The engine's directionality derivation should reflect this: specialists sit near beneficiary-end (they control the constraint), younger members sit more toward symmetric (they benefit but are not free agents in participation). The ritual practitioners themselves are mobile and voluntary, so their directionality should be near the beneficiary end (low d).
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual practitioners (organized power, mobile exit) are structural beneficiaries: they choose to participate because it affirms their identity and reinforces group belonging. Their directionality (d) should be near 0.0 (full beneficiary) — the constraint subsidizes their sense of community. Younger generation (moderate power, constrained exit) sit at intermediate directionality (~0.4–0.5): they inherit the meaning and benefit from group identity, but their participation is normatively expected and their choice to exit is constrained by social friction. Ritual specialists (institutional power, identity-locked exit) are agenda-setters: they control the form and benefit from institutional status, but their exit is fused with identity — they cannot leave without losing professional self-definition. Their directionality should be low (~0.2), reflecting that they hold decision power but do not extract material rents. Affected survivors (powerless, trapped) are observers: the ritual memorializes their loss, so they are honored presences but not structured as beneficiaries or targets — the constraint is about the living community's identity, not the survivors' operational needs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy — there is no signal that the founding problem is dead while the ritual persists as theater or inertia. The founding problem (maintaining identity continuity after catastrophe) remains live: the community continues to face the task of transmitting its defining past to new members who lack lived memory of the catastrophe. The ritual is not a degraded form of something that once had operational force; it is the intended form for its function (symbolic continuity and identity transmission). The moderate theater ratio (0.42) is not a warning sign but a design feature: the ritual is theatrical because that is how meaning is marked and transmitted. The measurement series show no degradation trajectory — extractiveness and theater ratio are stable, suppression stays minimal. The constraint should compute as a genuine rope (coordination without extraction or coercion), not as a piton or a shadow of a lost function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the mourning-practice reading the stable self-understanding of this community, or is it an analytical reframing imposed by external observers?',
    'Ask the ritual practitioners and specialists how they describe the ritual''s purpose. If they emphasize identity and symbolic meaning, the reading is internally held. If they emphasize threat-recognition or survival competence, the reading is an external analytical overlay.',
    'If the community itself understands the ritual as mourning practice (identity continuity), the reading is authentically kernel-grounded and represents a live position within the community''s self-understanding. If the community claims survival-competence function and analysts reframe it as identity-maintenance, the reading is externally imposed and the constraint''s classification is observer-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, empirical, 'Whether the mourning-practice reading reflects the community''s own understanding or is an external analytical lens.').

omega_variable(
    theater_as_function_vs_dysfunction,
    'Is the high theater ratio (0.42) evidence that the ritual''s function is healthy (symbolism IS the point), or evidence that the ritual is degrading (performance replacing practical function)?',
    'Track whether the ritual''s meaning-transmission is effective: do younger members report that their understanding of group identity and the catastrophe''s significance depends on the ritual? If yes, theater is healthy function. If younger members derive meaning from other sources and attend the ritual out of obligation, theater is degradation.',
    'If theater is healthy function, the constraint classifies as rope (coordination through symbolic practice). If theater is degradation masking a dead function, the constraint would reclassify toward piton (inertial maintenance). The measurement series treatment changes accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_as_function_vs_dysfunction, empirical, 'Whether the ritual''s symbolic performance constitutes its primary function or masks functional atrophy.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the measured suppression (0.08) capturing the actual coercive force to participate, or is it understating the internalized identity-lock mechanism that makes younger members feel obligated to attend despite formal exit options?',
    'Compare exit behavior across cohorts: do younger members who grow up secular/non-practicing eventually return to the ritual, or do they stay exited? Post-exit trajectory: if participants who leave the ritual later feel regret or identity-loss, suppression is higher than the structural measure suggests (internalized). If exit is stable and guilt-free, suppression is accurately measured.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than authored, and the rope classification shifts toward tangled_rope (coordination WITH identity-lock coercion). If internalized suppression is minimal, the rope classification holds (genuine voluntary coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Whether low measured suppression masks high internalized identity obligation.').

omega_variable(
    sibling_reading_coexistence,
    'Can the mourning-practice reading and the survival-competence reading coexist within the same community, or must one reading foreclose the other?',
    'Examine communities that maintain the same ritual form: do some interpret it as mourning practice (identity) while others interpret it as threat-recognition training (survival)? Or does adopting one reading logically exclude the other?',
    'If coexistence is possible (different community members hold different readings), the kernel exhibits genuine contestation and both readings are live. If the readings foreclose each other, only one reading can be true for any given community, and the kernel exhibits a fundamental dispute about what the ritual IS, not a diversity of valid readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the mourning-practice and survival-competence readings are logically compatible within one community''s self-understanding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 60, 0.43).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 60, 0.19).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 20, 0.06).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 40, 0.07).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 60, 0.08).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 80, 0.08).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__mourning_practice_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% The catastrophe-memory-preservation kernel admits three structurally distinct readings. This story instantiates the mourning-practice reading: ritual preserves symbolic continuity and collective identity. The survival-competence reading frames the same ritual as operational threat-recognition training. The hybrid-atrophy reading claims the ritual has degraded from survival-competence to mourning practice. Each reading instantiates a different constraint with different epsilon values, beneficiary structures, and type classifications. The three stories are linked as a constraint family; each story's network.affects_constraints array includes its siblings. The upstream survival-competence reading influences both the mourning-practice and hybrid-atrophy readings (the latter explicitly claims the former has atrophied). The mourning-practice reading forecloses (logically rules out) the survival-competence reading within a single community's self-understanding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
