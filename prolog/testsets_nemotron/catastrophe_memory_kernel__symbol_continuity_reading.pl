% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual as Symbolic Continuity and Collective Identity Preservation
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story represents the symbol_continuity_reading of the
 *   catastrophe_memory_kernel — the reading that ritual preserves symbolic
 *   continuity and collective identity across time through mourning-practice
 *   as identity-marker. The beneficiary is tradition-continuity itself
 *   (vindicated as symbolic_continuity_doctrine,
 *   collective_identity_through_mourning, tradition_as_living_memory) and the
 *   actors who bear it (tradition_bearers, community_members,
 *   transmission_custodians). The victim is adaptive modification: ritual
 *   rigidity imposes costs on those who would innovate or modify the
 *   practice. Extractiveness is low (0.12) because the constraint operates
 *   through symbolic transmission without operational survival yield — it
 *   coordinates identity and memory, not material survival. This reading
 *   coexists with three sibling readings of the same kernel:
 *   survival_competence_reading (ritual encodes adaptive capacity for
 *   persecution-survival), trauma_encoding_reading (ritual encodes
 *   intergenerational trauma as warning system), and
 *   boundary_maintenance_reading (ritual enforces group boundaries through
 *   shared mourning-practice). Each sibling would produce a different
 *   constraint with different ε, different beneficiaries/victims, and
 *   different type classification.
 *
 * KEY AGENTS:
 *   - tradition_bearers: Primary beneficiaries (organized/identity_locked) — receive identity continuity and communal standing through ritual participation
 *   - community_members: Beneficiaries (organized/identity_locked) — receive collective identity anchoring through shared symbolic practice
 *   - transmission_custodians: Beneficiaries/agenda_setters (institutional/identity_locked) — receive authority and status as guardians of the tradition
 *   - adaptive_modification: Victim (conceptual/constrained) — the capacity for ritual innovation and contextual adaptation bears the cost of symbolic rigidity
 *   - ritual_innovators: Victims (moderate/constrained) — practitioners who would modify the ritual face identity-exclusion costs
 *   - analytical_observer: Observer (analytical/analytical) — sees full structural field across all four readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.12).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.18).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual as Symbolic Continuity and Collective Identity Preservation").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'e48b0ba2-223a-465a-8ebf-247629ac9b14').
narrative_ontology:cs_kernel_codification('e48b0ba2-223a-465a-8ebf-247629ac9b14', distributed).
narrative_ontology:cs_authority_grounding('e48b0ba2-223a-465a-8ebf-247629ac9b14', practice).
narrative_ontology:cs_interpretation_layer_present('e48b0ba2-223a-465a-8ebf-247629ac9b14').
narrative_ontology:cs_reading_relation('e48b0ba2-223a-465a-8ebf-247629ac9b14', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e48b0ba2-223a-465a-8ebf-247629ac9b14', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('e48b0ba2-223a-465a-8ebf-247629ac9b14', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('e48b0ba2-223a-465a-8ebf-247629ac9b14', foundational, symbolic_continuity_as_identity_foundation).
narrative_ontology:cs_axiom_status(symbolic_continuity_as_identity_foundation, holdable).
narrative_ontology:cs_axiom_grounding('e48b0ba2-223a-465a-8ebf-247629ac9b14', symbolic_continuity_as_identity_foundation, deontological).
narrative_ontology:cs_axiom('e48b0ba2-223a-465a-8ebf-247629ac9b14', secondary, ritual_fidelity_over_functional_adaptation).
narrative_ontology:cs_axiom_status(ritual_fidelity_over_functional_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('e48b0ba2-223a-465a-8ebf-247629ac9b14', ritual_fidelity_over_functional_adaptation, conventional).
narrative_ontology:cs_reference_frame('e48b0ba2-223a-465a-8ebf-247629ac9b14', catastrophe_memory_as_living_symbol).
narrative_ontology:cs_drift_state('e48b0ba2-223a-465a-8ebf-247629ac9b14', contemporary_modernity_contact, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e48b0ba2-223a-465a-8ebf-247629ac9b14', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_bearers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, transmission_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, ritual_innovators).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, symbolic_continuity_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_through_mourning).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, tradition_as_living_memory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carry and enact the ritual across generations. Their communal standing and self-understanding are constituted through faithful transmission. Exit means identity rupture — loss of the symbolic framework that makes their collective history intelligible. They receive identity continuity and belonging; they pay with fidelity to form and time-commitment.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, tradition_bearers, beneficiary,
    organized, generational, identity_locked, regional).

% Participate in the ritual as the primary anchor of collective identity. The mourning-practice makes the catastrophe narratable and the community continuous. Exit is possible (secular alternatives exist) but costly: leaving the ritual means leaving the shared symbolic world. They receive identity anchoring; they pay with participation and conformity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, community_members, beneficiary,
    organized, biographical, identity_locked, local).

% Institutional guardians (clergy, elders, ritual specialists) who authorize the ritual form, train successors, and adjudicate modifications. Their authority derives from the tradition they guard. They receive status, authority, and material support; they pay with the burden of maintaining fidelity. Their exit is structurally blocked — the role exists only within the tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, transmission_custodians, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, transmission_custodians, beneficiary).

% The conceptual capacity for ritual innovation and contextual adaptation. Bears the cost of symbolic rigidity: the ritual's fidelity requirements foreclose modifications that might better serve present circumstances (new catastrophe forms, changed community demographics, psychological needs). This is not an actor but a structural victim — the flexibility the system lacks.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification).

% Practitioners (younger generation, reform-minded members, trauma-survivors with different needs) who would modify the ritual form or create complementary practices. They face identity-exclusion costs: proposing changes risks being cast as betraying the tradition. They can exit to secular or alternative communities but lose the identity-continuity the ritual provides. They pay rigidity costs; they receive no offsetting benefit from the constraint itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_innovators, payer,
    moderate, biographical, constrained, local).

% Scholar of religious studies / collective memory / ritual practice who compares all four readings of the catastrophe_memory_kernel. Sees the full structural field: how each reading extracts different ε, names different beneficiaries/victims, and classifies differently. Neither collects nor pays; provides the analytical seat the engine uses for cross-reading comparison.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__symbol_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__symbol_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and symbolic continuity across generations after catastrophe: provides a shared, repeatable practice that makes the catastrophe narratable, the community continuous, and identity transmissible without requiring each generation to re-invent meaning.
% TRANSFER_FUNCTION: Moves attention, time, and emotional labor from participants (tradition_bearers, community_members, ritual_innovators) into the ritual form itself — the symbolic system accumulates the investment. No material transfer; the 'gain' is the maintained symbolic structure, which is diffuse (no single seat captures it).
% ABSENT_VOICES: Descendants who would inherit the tradition but are not yet born (cannot consent to the identity-lock); trauma-survivors for whom the ritual re-enacts rather than processes; secular community members who share the history but not the ritual form. These voices are structurally excluded by the generational time-horizon and the identity-boundary of the practice.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community would lose its primary symbolic anchor for collective identity. Alternative practices (secular commemoration, storytelling, digital archives) would emerge but would not carry the same identity-constitutive force. The community would reorganize around new or hybrid memory-practices — the world rearranges because arrangements depend on this constraint.
% FOUNDING_PROBLEM: After catastrophe, how does a community preserve its collective identity and symbolic continuity across generations when the catastrophe itself threatens to rupture the chain of transmission? The ritual was built to solve: identity rupture risk, meaning-loss across generational turnover, and the need for a portable, repeatable practice that survives dispersal.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of post-catastrophe communities (e.g., post-Holocaust Jewish communities, post-genocide Armenian communities, post-colonial indigenous communities) document that ritualized mourning-practice is a cross-cultural response to identity rupture risk — corroboration from outside the benefiting tradition-custodians. The problem recurs with each new catastrophe and each generational turnover.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12) reflects the genuine cost of symbolic transmission: the ritual requires participation, repetition, and fidelity to form, which consumes time and attention but yields no material extraction. Suppression (0.18) is low because the constraint does not actively prevent alternatives — other memory practices can coexist — but it creates identity-lock exit costs: leaving the ritual means leaving the identity it constitutes. Theater ratio (0.25) is modest: some performative elaboration accumulates over time (ornamentation, extended liturgy) but the core practice remains functional for its coordination purpose. Accessibility collapse (0.35) is moderate: the symbolic system is learnable and alternatives exist (secular commemoration, other ritual forms), but the identity-lock dynamic makes exit costly. Resistance (0.15) is low because the constraint operates through voluntary participation and identity-affirmation rather than coercion. The gradual rise in all metrics over 50 time units reflects institutional elaboration: as the tradition ages, custodians add layers (theater), boundary-maintenance intensifies slightly (suppression), and the symbolic system becomes more self-referential (extractiveness drift).
 *
 * PERSPECTIVAL GAP:
 *   From the tradition_bearer seat, the constraint is a Rope: genuine coordination of collective memory with minimal extraction. From the ritual_innovator seat, it approaches a Snare: the identity-lock exit costs function as suppression of modification. From the transmission_custodian seat, it may drift toward Piton: elaboration accumulates without functional gain. The engine computes these per-seat divergences from the structural data; the authored claim (Rope) represents the dominant coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (tradition_bearers, community_members, transmission_custodians) receive identity continuity and status — directionality near 0.0 (beneficiary end). Victims (adaptive_modification as conceptual victim, ritual_innovators as actor-victims) bear rigidity costs and identity-exclusion — directionality near 1.0 (target end). The 'tradition-continuity itself' beneficiary is encoded as vindicated_propositions, not actor-beneficiaries, because it collects no rents. The constraint coordinates identity (identity_coordination type) with low inherent extraction; any extraction above the Boltzmann floor (0.08) would signal identity-lock capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (symbolic continuity of collective identity after catastrophe) remains live — catastrophes recur, identity needs anchoring. The constraint is not mandatrophic: it still solves the problem it was built for. However, the gradual metric drift (rising theater, suppression, extractiveness) signals risk of future mandatrophy if elaboration outpaces functional need. The founding_problem_status is 'live' with external corroboration from anthropological studies of post-catastrophe communities (not just tradition-internal attestation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the symbol_continuity_reading of the catastrophe_memory_kernel. What would the sibling readings (survival_competence_reading, trauma_encoding_reading, boundary_maintenance_reading) change structurally?',
    'Compare constraint stories for each sibling reading: survival_competence_reading would elevate extractiveness and shift beneficiaries to survival-competence custodians; trauma_encoding_reading would increase suppression and name trauma-bearers as victims; boundary_maintenance_reading would raise enforcement and name boundary-crossers as victims. The structural delta between readings is the kernel''s contest surface.',
    'If sibling readings produce substantially different ε, suppression, or beneficiary/victim structures, the kernel is structurally fragmented — the label ''catastrophe memory ritual'' covers multiple constraints. If they converge, the kernel may be a single constraint with interpretive variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between sibling readings of the catastrophe memory kernel').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Is ''tradition-continuity itself'' a genuine beneficiary (an actor that collects) or a vindicated proposition (a state of affairs the constraint serves)? The base_properties lists both beneficiaries (actors) and vindicated_propositions — are they correctly separated?',
    'Check whether the named beneficiary groups (tradition_bearers, community_members, transmission_custodians) actually receive material or status flows from the constraint, versus whether ''tradition continuity'' is the abstract end the constraint serves. If no actor collects, reclassify the beneficiary declarations as vindicated propositions.',
    'Misplacing a proposition as a beneficiary inflates the coordination signal and distorts directionality derivation. Correct placement keeps the engine''s coordination/extraction gate clean.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, conceptual, 'Whether tradition-continuity is an actor-collector or a proposition-vindicated').

omega_variable(
    extraction_floor_for_symbolic_transmission,
    'Is the low extractiveness (0.12) the genuine cost of symbolic transmission, or does it mask extraction that operates through identity-locking (participants cannot exit the ritual without identity rupture)?',
    'Measure exit costs for participants who attempt to modify or abstain from the ritual: if exit triggers identity rupture (exclusion, loss of communal standing, psychological dislocation), the effective extraction is higher than the symbolic-transmission floor. Compare with identity_coordination floor (0.08) and attachment_coordination floor (0.08).',
    'If identity-locking operates, the constraint may be a Tangled Rope (coordination + asymmetric extraction via identity costs) rather than a Rope. The Boltzmann floor test would flag excess extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_floor_for_symbolic_transmission, empirical, 'Whether low base extractiveness masks identity-lock extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_tr_t10, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_tr_t30, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_be_t10, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_be_t30, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 30, 0.11).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_su_t10, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_su_t30, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 30, 0.16).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.17).
narrative_ontology:measurement(catastrophe_memory_kernel__symbol_continuity_reading_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__symbol_continuity_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_kernel. The kernel decomposes into four structurally distinct constraints: symbol_continuity (this story, Rope, ε≈0.12), survival_competence (higher ε, coordination with survival yield), trauma_encoding (higher suppression, warning-system function), boundary_maintenance (active enforcement, exclusionary function). They are linked by shared ritual substrate but differ in ε, beneficiary/victim structure, and type. The ε-invariance principle requires separate stories because measuring 'the ritual' through survival lens vs. symbol lens vs. trauma lens yields different extraction values — different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__symbol_continuity_reading, organized, 0.15).
constraint_indexing:directionality_override(catastrophe_memory_kernel__symbol_continuity_reading, institutional, 0.1).
constraint_indexing:directionality_override(catastrophe_memory_kernel__symbol_continuity_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
