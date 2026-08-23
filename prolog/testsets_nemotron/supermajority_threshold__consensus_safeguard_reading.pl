% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__consensus_safeguard_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Supermajority Threshold as Consensus Safeguard
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint story models the supermajority threshold as a consensus
 *   safeguard — a high barrier to constitutional amendment that filters out
 *   transient majoritarian passions and ensures changes reflect deep,
 *   persistent democratic consensus. The reading treats the threshold as a
 *   genuine coordination mechanism: it solves the problem of constitutional
 *   instability by requiring broad agreement for fundamental change,
 *   benefiting constitutional continuity, future citizens, and democratic
 *   institutions diffusely. No specific victim set exists unless the
 *   threshold actively blocks a majority will, at which point the blocked
 *   majority becomes a contingent victim. The claimed type is rope (pure
 *   coordination with minimal coercive overhead), while the authored metrics
 *   describe low but non-zero extraction, moderate suppression (the barrier
 *   itself), and rising theater over time — the engine measures the
 *   divergence. This is one reading of the supermajority_threshold kernel;
 *   the minoritarian_veto_reading and adaptive_gradient_reading instantiate
 *   different constraints from the same kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.18).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.35).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, rope).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Supermajority Threshold as Consensus Safeguard").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "constitutional_theory/political_economy/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, '8af7f4de-db0f-46bf-84bc-a5999132d556').
narrative_ontology:cs_kernel_codification('8af7f4de-db0f-46bf-84bc-a5999132d556', formalized).
narrative_ontology:cs_authority_grounding('8af7f4de-db0f-46bf-84bc-a5999132d556', lineage).
narrative_ontology:cs_interpretation_layer_present('8af7f4de-db0f-46bf-84bc-a5999132d556').
narrative_ontology:cs_reading_relation('8af7f4de-db0f-46bf-84bc-a5999132d556', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('8af7f4de-db0f-46bf-84bc-a5999132d556', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('8af7f4de-db0f-46bf-84bc-a5999132d556', foundational, deep_consensus_as_legitimacy_condition).
narrative_ontology:cs_axiom_status(deep_consensus_as_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('8af7f4de-db0f-46bf-84bc-a5999132d556', deep_consensus_as_legitimacy_condition, deontological).
narrative_ontology:cs_axiom('8af7f4de-db0f-46bf-84bc-a5999132d556', secondary, constitutional_stability_as_intergenerational_obligation).
narrative_ontology:cs_axiom_status(constitutional_stability_as_intergenerational_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8af7f4de-db0f-46bf-84bc-a5999132d556', constitutional_stability_as_intergenerational_obligation, deontological).
narrative_ontology:cs_reference_frame('8af7f4de-db0f-46bf-84bc-a5999132d556', constitutional_continuity_framework).
narrative_ontology:cs_drift_state('8af7f4de-db0f-46bf-84bc-a5999132d556', contemporary_polarization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8af7f4de-db0f-46bf-84bc-a5999132d556', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_continuity).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, future_citizens).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, democratic_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, blocked_majorities).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, constitutional_stability_doctrine).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, deep_consensus_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designed the amendment threshold as part of the constitutional architecture. They bear the cost of a higher barrier when they seek amendments but benefit from the stability it provides to the system they created. Their exit is analytical — they observe the constraint's operation from the design perspective.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_designers, agenda_setter,
    institutional, generational, analytical, national).

% The systemic good of constitutional persistence and stability. Not an actor but the diffuse beneficiary the constraint coordinates around. Collects no rents; its 'benefit' is the prevention of constitutional churn.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_continuity, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__consensus_safeguard_reading, constitutional_continuity).

% Citizens not yet born who inherit the constitutional order. They benefit from stability but cannot consent to or exit the constraint. Their 'exit' is trapped — they are born into the threshold's regime.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, future_citizens, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__consensus_safeguard_reading, future_citizens).

% Legislatures, courts, and executive branches that operate within a stable constitutional framework. They benefit from predictable rules of the game but also face the constraint when seeking structural reforms.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, democratic_institutions, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__consensus_safeguard_reading, democratic_institutions).

% Contingent victims who emerge only when the threshold blocks a majority-supported amendment. They bear the cost of delayed or prevented change. Their exit is constrained — they can mobilize for a supermajority, wait for political realignment, or seek judicial reinterpretation, but all paths are costly and uncertain.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, blocked_majorities, payer,
    organized, biographical, constrained, national).

% Analyze the threshold's empirical operation: amendment success rates, blocking patterns, distributional effects. They see the full structure across readings but do not collect from or pay into the constraint.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__consensus_safeguard_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__consensus_safeguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents constitutional instability by requiring broad, persistent agreement for fundamental changes — solves the problem of transient majorities enacting changes that lack deep democratic legitimacy and may be reversed by the next majority.
% TRANSFER_FUNCTION: Moves amendment authority from simple majorities to supermajorities; the 'transfer' is the opportunity cost of amendments that fail to meet the threshold. No direct resource transfer; the constraint reallocates political power across time (present majority vs. future stability).
% ABSENT_VOICES: Citizens in polities without supermajority requirements (who experience constitutional change more fluidly); historical majorities whose amendments were blocked and who left no institutional record of their objection; future citizens who cannot yet speak to whether the stability served them.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished overnight, constitutional amendments would proceed by simple majority (or whatever lower rule applies), likely increasing amendment frequency and decreasing constitutional stability. The political system would reorganize around lower-threshold change, altering the strategic calculus of constitutional politics.
% FOUNDING_PROBLEM: Constitutional instability under simple majority amendment: frequent fundamental changes driven by transient majorities, undermining the constitution's function as a stable framework for democratic governance and intergenerational commitment.
% FOUNDING_PROBLEM_CORROBORATION: The consensus_safeguard_reading's proponents (constitutional designers, some political theorists) attest the problem is live — polarization makes stability more necessary. Critics (minoritarian_veto_reading proponents, adaptive_gradient_reading proponents) attest the problem is dead or misdiagnosed — the threshold now blocks necessary adaptation, or was never about stability but entrenchment. No neutral corroborator outside the beneficiary set is universally accepted; the founding problem's status is itself a site of contestation.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__consensus_safeguard_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__consensus_safeguard_reading_tests).
:- end_tests(supermajority_threshold__consensus_safeguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the threshold's primary operation is filtering rather than transferring value; it does not systematically extract from a defined group. Suppression is moderate (0.35) because the barrier itself prevents action — but this is the coordination function, not coercive enforcement against alternatives. Theater ratio rises from 0.15 to 0.22 over the interval, reflecting increasing performative invocation of 'deep consensus' to justify blocking changes that may have genuine majority support. Accessibility collapse is high (0.72) because once the supermajority requirement is understood as legitimate, alternatives (simple majority amendment, legislative override) collapse conceptually — the constraint redefines what counts as legitimate constitutional change. Resistance is low (0.28) because the constraint is widely accepted as a constitutional design feature, though resistance rises when blocking occurs.
 *
 * PERSPECTIVAL GAP:
 *   From the consensus_safeguard_reading's seat, the constraint is a rope: genuine coordination with diffuse benefits and minimal extraction. From the minoritarian_veto_reading's seat, the same structure is a snare: identifiable minorities capture veto power to entrench privilege. From the adaptive_gradient_reading's seat, it is a tangled_rope or scaffold: coordination function exists but requires calibration, and miscalibration creates extraction. The engine computes these per-seat classifications from the structural data; this reading authors only its own structural view.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are diffuse and structural: constitutional continuity (the system's persistence), future citizens (protected from transient majorities), and democratic institutions (stabilized against volatility). These are not actors who collect rents — they are the systemic goods the constraint coordinates. No standing victim set exists in this reading; victims appear only contingently when a majority is blocked. The agenda-setter seat (constitutional designers, amendment authorities) sits near symmetric: they bear the cost of the higher threshold (harder to pass desired amendments) but benefit from the stability it provides. The analytical observer sees the full structure: a coordination mechanism whose legitimacy depends on the 'deep consensus' claim remaining empirically grounded.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing constitutional instability from transient passions) remains contested — some argue it is live (polarization makes it more necessary), others argue it is dead (the threshold now blocks necessary adaptation), others that it was never the real problem (the threshold was always about entrenchment). This reading treats the problem as live but acknowledges the contestation. Mandatrophy is not resolved; the constraint's function is actively disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_identity,
    'This constraint is one reading (consensus_safeguard_reading) of the supermajority_threshold kernel. What structural elements would change if the sibling readings (minoritarian_veto_reading, adaptive_gradient_reading) were instantiated instead?',
    'Compare the three readings'' beneficiary/victim structures, extractiveness referents, and claimed types. The consensus_safeguard_reading has diffuse beneficiaries and no victim set unless blocking occurs; minoritarian_veto_reading has identifiable victims (majoritarian will blocked); adaptive_gradient_reading has calibration-dependent extraction.',
    'Confirms ε-invariance: each reading instantiates a different constraint with its own ε, beneficiary structure, and type. The kernel is the contested commitment; the readings are the constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_identity, conceptual, 'Kernel-reading decomposition: this reading''s structural identity vs. siblings').

omega_variable(
    diffuse_beneficiary_legitimacy,
    'Are the diffuse beneficiaries (constitutional continuity, future citizens, democratic institutions) genuine beneficiaries of a coordination function, or is this a false summit where identifiable actors (incumbent power holders, judicial elites, entrenched interests) capture the stability rent?',
    'Empirical analysis of who actually benefits when amendments are blocked: trace blocked amendment history to identify systematic patterns of which groups'' preferences are protected. Cross-reference with judicial review outcomes and institutional self-preservation cases.',
    'If identifiable capture is demonstrated, the constraint reclassifies from rope toward tangled_rope or snare via FSM logic. If diffuse beneficiaries are genuine, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_beneficiary_legitimacy, empirical, 'Whether diffuse beneficiaries are structurally real or cover for concentrated capture').

omega_variable(
    blocking_as_extraction_trigger,
    'When does the constraint shift from passive stability filter to active extraction? The consensus_safeguard_reading declares victims only ''unless blocking occurs'' — at what frequency and pattern of blocking does the victim set become structurally real rather than contingent?',
    'Longitudinal study of supermajority threshold invocations: measure blocking frequency, the demographic/political profile of blocked majorities, and whether blocked proposals systematically disadvantage particular groups. Threshold: if blocking systematically protects the same interests, victim set is structural.',
    'If blocking is rare and distributionally neutral, the reading''s ''no victims unless blocking'' claim holds. If blocking is systematic and distributionally biased, victims become a standing structural feature, shifting classification toward tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(blocking_as_extraction_trigger, empirical, 'Threshold at which contingent blocking becomes structural victimization').

omega_variable(
    calibration_uncertainty,
    'The adaptive_gradient_reading argues the threshold requires evidence-based calibration. Is the current threshold (e.g., 2/3, 3/4, constitutional convention requirement) calibrated to actual consensus formation rates, or is it a historical artifact that the consensus_safeguard_reading treats as natural law?',
    'Comparative constitutional analysis: examine amendment success rates across different thresholds and political contexts. Test whether the threshold filters ''transient passion'' or simply filters all change. Historical analysis of the threshold''s origin: was it set by deliberation or by power?',
    'If uncalibrated, the consensus_safeguard_reading''s ''deep consensus'' claim is a post-hoc rationalization; extraction may be higher than authored. If calibrated, the reading''s coordination function is empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_uncertainty, empirical, 'Whether the threshold is functionally calibrated or a historical artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t25, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(supe_tr_t25, observed).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(supe_tr_t50, observed).
narrative_ontology:measurement(supe_tr_t75, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 75, 0.21).
narrative_ontology:measurement_basis(supe_tr_t75, observed).
narrative_ontology:measurement(supe_tr_t100, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(supe_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t25, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement_basis(supe_be_t25, observed).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 50, 0.16).
narrative_ontology:measurement_basis(supe_be_t50, observed).
narrative_ontology:measurement(supe_be_t75, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 75, 0.17).
narrative_ontology:measurement_basis(supe_be_t75, observed).
narrative_ontology:measurement(supe_be_t100, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement_basis(supe_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t25, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(supe_su_t25, observed).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 50, 0.3).
narrative_ontology:measurement_basis(supe_su_t50, observed).
narrative_ontology:measurement(supe_su_t75, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 75, 0.33).
narrative_ontology:measurement_basis(supe_su_t75, observed).
narrative_ontology:measurement(supe_su_t100, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 100, 0.35).
narrative_ontology:measurement_basis(supe_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__consensus_safeguard_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__consensus_safeguard_reading, 0.08).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the supermajority_threshold kernel. This reading (consensus_safeguard) claims the threshold is a stability coordination mechanism (rope). The minoritarian_veto_reading claims it is a minority capture mechanism (snare). The adaptive_gradient_reading claims it is a calibration-dependent tool (tangled_rope or scaffold). The three readings have different ε values, different beneficiary/victim structures, and different claimed types — they are different constraints linked by the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
