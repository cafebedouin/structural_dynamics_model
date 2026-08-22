% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Ritual as Survival Competence Transmission (Operational Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the operational_competence_reading of
 *   the catastrophe_memory_transmission kernel. The reading holds that
 *   ritual's primary structural function is the transmission of survival
 *   competence — pattern recognition for threat identification, resource
 *   coordination protocols, and threat assessment rehearsal — through
 *   repeated enactment. Ritual elements are evaluated by their operational
 *   yield: Passover as rapid-departure readiness drill, Tisha B'Av as
 *   resource-scarcity training, Yom Kippur as collective accountability
 *   rehearsal. The beneficiary is future survival capacity; the constraint
 *   type is rope (a coordination mechanism for intergenerational knowledge
 *   transmission) rather than mountain, because the transmission requires
 *   active communal maintenance — it does not persist without practitioners.
 *   The victim class is those who mistake symbol for substance
 *   (symbolic_interpretation_purists), whose interpretive frame is treated as
 *   a category error that degrades transmission fidelity.
 *
 * KEY AGENTS:
 *   - future_surviving_communities: Primary beneficiary (organized/generational/arbitrage) — receives transmitted survival competence
 *   - ritual_practitioners: Primary beneficiary (organized/biographical/mobile) — active transmitters who internalize competence through enactment
 *   - symbolic_interpretation_purists: Victim (moderate/biographical/constrained) — their reading is marginalized as confusing vehicle for payload
 *   - anthropological_observers: Observer (analytical/civilizational/analytical) — evaluates operational yield across communities and history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.08).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Ritual as Survival Competence Transmission (Operational Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '14a5e543-f0d6-44eb-83be-90924b58776b').
narrative_ontology:cs_kernel_codification('14a5e543-f0d6-44eb-83be-90924b58776b', distributed).
narrative_ontology:cs_authority_grounding('14a5e543-f0d6-44eb-83be-90924b58776b', practice).
narrative_ontology:cs_interpretation_layer_present('14a5e543-f0d6-44eb-83be-90924b58776b').
narrative_ontology:cs_reading_relation('14a5e543-f0d6-44eb-83be-90924b58776b', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('14a5e543-f0d6-44eb-83be-90924b58776b', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('14a5e543-f0d6-44eb-83be-90924b58776b', foundational, ritual_form_separable_from_operational_payload).
narrative_ontology:cs_axiom_status(ritual_form_separable_from_operational_payload, holdable).
narrative_ontology:cs_axiom_grounding('14a5e543-f0d6-44eb-83be-90924b58776b', ritual_form_separable_from_operational_payload, empirically_contingent).
narrative_ontology:cs_axiom('14a5e543-f0d6-44eb-83be-90924b58776b', foundational, survival_competence_primary_ritual_function).
narrative_ontology:cs_axiom_status(survival_competence_primary_ritual_function, holdable).
narrative_ontology:cs_axiom_grounding('14a5e543-f0d6-44eb-83be-90924b58776b', survival_competence_primary_ritual_function, instrumental).
narrative_ontology:cs_reference_frame('14a5e543-f0d6-44eb-83be-90924b58776b', ancestral_catastrophe_survival_competence).
narrative_ontology:cs_drift_state('14a5e543-f0d6-44eb-83be-90924b58776b', contemporary_anthropological_evaluation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('14a5e543-f0d6-44eb-83be-90924b58776b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_surviving_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, ritual_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, symbolic_interpretation_purists).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, survival_competence_thesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, ritual_as_operational_training).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive transmitted survival competence through ritual practice — pattern recognition for threat identification, resource coordination protocols, and rehearsed departure readiness. The competence is the inheritance; the ritual is the transmission mechanism. They benefit by being operationally prepared when catastrophe recurs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_surviving_communities, beneficiary,
    organized, generational, arbitrage, global).

% Active participants who enact the ritual cycles. They gain operational fluency through repetition — knowing the rapid-departure sequence, the resource-rationing logic, the threat-assessment heuristics. Their participation maintains the transmission channel; they can leave the practice but the competence they've internalized remains.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_practitioners, beneficiary,
    organized, biographical, mobile, global).

% Hold that ritual's primary value is symbolic continuity, identity preservation, and mourning-practice as intrinsic communal goods. Under the operational competence reading, their interpretive frame is treated as a potentially dangerous confusion — mistaking the transmission vehicle for the payload. They pay the cost of having their reading marginalized as 'missing the point.'
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbolic_interpretation_purists, payer,
    moderate, biographical, constrained, global).

% Study the ritual system as a cultural technology for survival-competence transmission. They evaluate operational yield across communities and historical episodes, tracking which ritual elements correlate with measurable survival outcomes. They neither collect rents nor bear extraction.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, anthropological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational transmission problem for survival competence: how to preserve and transmit threat-assessment heuristics, resource-coordination protocols, and rapid-departure readiness across generations without loss of operational fidelity, when the catastrophes themselves are rare and the knowledge must survive the peaceful intervals.
% TRANSFER_FUNCTION: Moves survival competence (pattern recognition, coordination protocols, threat rehearsal) from ancestral catastrophe survivors to future community members through ritual enactment. The transfer vehicle is the ritual cycle; the payload is operational competence.
% ABSENT_VOICES: Communities whose rituals were interrupted by conquest, displacement, or forced assimilation — they would object to the claim that ritual competence is 'transmitted' when the transmission chain was broken by violence. Also absent: the dead ancestors whose survival the rituals encode; their testimony on whether the rituals worked as training is unavailable.
% DISAPPEARANCE_RATIONALE: If the operational-competence reading vanished overnight, communities would lose the explicit framework that treats ritual as survival training. The rituals might persist as symbolic practice (per the symbol_continuity_reading), but the operational calibration — the knowledge that Passover rehearse rapid departure, that Tisha B'Av trains resource scarcity — would degrade. Survival competence would become implicit rather than explicit, increasing the risk of transmission failure during the next catastrophe.
% FOUNDING_PROBLEM: How to ensure that the survival lessons of catastrophe — when to flee, how to ration, how to recognize the warning patterns — survive the peaceful generations when no one remembers the catastrophe directly, and transmit with enough operational fidelity to be useful when the next catastrophe arrives.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by historical anthropologists (e.g., Scott on highland Southeast Asian oral traditions, Diamond on societal collapse patterns) who document ritual-as-survival-manual across cultures, and by contemporary disaster researchers who find that communities with rehearsed evacuation rituals have measurably better survival outcomes. The operational competence reading does not rest solely on the beneficiary community's self-assertion.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.08) because the constraint extracts little from participants — the 'cost' is participation time and the discipline of enactment, but the return is survival competence that benefits the practitioners and their descendants. Suppression is low (0.12) because the constraint does not coerce participation; communities that abandon the rituals simply lose the transmission channel (exit is mobile for practitioners). Theater ratio is low (0.15) because the ritual enactments are the actual training mechanism, not a performance substitute. Accessibility collapse is high (0.88) because alternative transmission mechanisms for this specific competence (intergenerational oral instruction without ritual scaffold) have historically failed at scale — the ritual structure is the load-bearing transmission technology. Resistance is near-zero (0.05) because the constraint is not enforced against unwilling participants; it persists because it works.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seat, the constraint is a rope — genuine coordination solving a real transmission problem with minimal coercion. From the symbolic purist seat, the same structure reads as a potential snare — their cherished symbolic framework is displaced by a reductive operationalist reading that treats meaning as instrumental. The engine computes this divergence from the structural data: practitioners have mobile exit and generational benefit; purists have constrained exit (their interpretive community is bound to the symbolic reading) and bear the cost of displacement.
 *
 * DIRECTIONALITY LOGIC:
 *   Future surviving communities and ritual practitioners are structural beneficiaries (d near 0.0) — the constraint subsidizes them with survival competence. Symbolic interpretation purists are payers (d elevated) because their interpretive frame is structurally displaced by the operational reading; they bear the cost of having their reading treated as a transmission-degrading error. Anthropological observers sit at d=0.5 (symmetric analytical seat). The directionality derives from who receives the competence payload versus who bears the interpretive displacement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intergenerational survival competence transmission) remains live — catastrophes still occur, and communities with rehearsed protocols still survive better. The constraint has not outlived its function. Mandatrophy is resolved: the arrangement persists because its founding problem persists, not from institutional inertia. The slight rise in extractiveness and suppression over the 3000-year interval reflects the gradual accretion of symbolic layers that the operational reading must continuously distinguish from the operational core — a maintenance cost, not a function shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vehicle_payload_separability,
    'Are the ritual''s symbolic form and its operational competence payload structurally separable, or does the symbolic form constitutively generate the competence (as the hybrid_embedded_reading claims)?',
    'Natural experiment: communities that retain the operational protocols but lose the symbolic form (e.g., secularized evacuation drills derived from ritual prototypes) — do they maintain equivalent survival outcomes? If yes, separable; if no, the symbolic form is constitutive.',
    'If separable, the operational reading is structurally sound and the symbolic layer is optional theater (increasing theater_ratio over time). If inseparable, the operational reading commits a category error — the ''symbolic'' is not a vehicle but the generative substrate, and the constraint''s extractiveness is underestimated because it counts symbolic maintenance as optional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vehicle_payload_separability, conceptual, 'Whether symbolic form and operational payload are separable transmission components.').

omega_variable(
    universal_law_vs_coordination_mechanism,
    'Is the competence-transmission function a universal law of cultural evolution (mountain) or a contingent coordination mechanism (rope) that some communities solve differently?',
    'Cross-cultural survey: do ALL long-surviving communities use ritual for competence transmission, or do some use non-ritual mechanisms (written manuals, institutional training, oral epics without ritual enactment)? If universal, mountain; if contingent, rope.',
    'If mountain, extractiveness and suppression are measurement errors (the constraint is a structural feature of cultural survival). If rope, the low but non-zero metrics are accurate — the mechanism requires maintenance and can fail.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_law_vs_coordination_mechanism, empirical, 'Whether ritual-as-survival-transmission is a universal cultural law or a contingent coordination solution.').

omega_variable(
    kernel_reading_location,
    'Is this constraint a reading of the catastrophe_memory_transmission kernel (operational_competence_reading), and what would the sibling readings (symbol_continuity_reading, hybrid_embedded_reading) change structurally?',
    'Structural comparison: the operational reading names future survival capacity as beneficiary and symbolic purists as victims; the symbol_continuity_reading names identity continuity as beneficiary and operationalists as victims; the hybrid reading names the inseparable unity as beneficiary and reductionists of either stripe as victims. The disagreement is located in the beneficiary/victim structure and the claimed_type (rope vs mountain vs tangled_rope).',
    'Each reading instantiates a different constraint with different ε, different stakeholders, different classification. They are not perspectives on one constraint — they are distinct constraints linked by kernel_id.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'This constraint is one reading of a contested kernel; sibling readings would change beneficiary/victim structure and claimed type.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmto_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cmto_tr_t750, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 750, 0.11).
narrative_ontology:measurement(cmto_tr_t1500, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement(cmto_tr_t2250, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 2250, 0.14).
narrative_ontology:measurement(cmto_tr_t3000, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 3000, 0.15).

% Extraction over time
narrative_ontology:measurement(cmto_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(cmto_be_t750, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 750, 0.06).
narrative_ontology:measurement(cmto_be_t1500, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 1500, 0.07).
narrative_ontology:measurement(cmto_be_t2250, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 2250, 0.07).
narrative_ontology:measurement(cmto_be_t3000, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 3000, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(cmto_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(cmto_su_t750, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 750, 0.07).
narrative_ontology:measurement(cmto_su_t1500, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(cmto_su_t2250, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 2250, 0.11).
narrative_ontology:measurement(cmto_su_t3000, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 3000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__operational_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint (operational_competence_reading) and its two siblings (symbol_continuity_reading, hybrid_embedded_reading) form the catastrophe_memory_transmission constraint family. Each reading instantiates a different constraint with distinct ε, beneficiary/victim structure, and claimed_type. The operational reading claims rope with low extractiveness; the symbol_continuity_reading likely claims mountain (identity continuity as intrinsic good); the hybrid reading likely claims tangled_rope (inseparable unity with both coordination and extraction). The kernel_id links them as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
