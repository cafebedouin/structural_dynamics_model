% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual as Symbol Continuity Keeper (Catastrophe Memory Kernel)
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the
 *   catastrophe_memory_kernel: ritual as a mechanism for preserving symbolic
 *   continuity and collective identity across time. The kernel itself is
 *   contested—different communities and scholarly traditions read the same
 *   ritual practices as encoding trauma (trauma_encoding_reading),
 *   transmitting survival competence (survival_competence_reading), enforcing
 *   group boundaries (boundary_maintenance_reading), or preserving symbolic
 *   continuity (this reading). This story focuses exclusively on the
 *   symbol-continuity reading: ritual as the primary mechanism by which a
 *   post-catastrophe community anchors its collective identity in a shared
 *   historical narrative, independent of whether the ritual's original
 *   survival functions persist. Extractiveness is low (0.28) because the
 *   constraint operates primarily through meaning-making, not through
 *   coercive exclusion or resource transfer. Theater rises over the interval
 *   (0.42 to 0.58) because as immediate survival pressures ease, ritual
 *   maintenance becomes increasingly performance-of-continuity rather than
 *   transmission-of-urgent-knowledge. The constraint is classified as rope
 *   because it solves a genuine coordination problem (how do we stay 'us'
 *   across generations?) with minimal suppression and genuine participant
 *   benefit—though the benefit is identity-continuity, not material survival,
 *   and that distinction is the reading's core claim.
 *
 * KEY AGENTS:
 *   - tradition_carriers: custodians of ritual form; authority structure for transmission; identity-locked to the role
 *   - ritual_participants: community members; receive identity-continuity benefit; bear cost of time spent on ritual vs. adaptive response
 *   - adaptive_modification_advocates: excluded; would argue for flexibility; constrained by tradition-carriers' authority
 *   - externally_imposed_pressure_agents: excluded; view ritual as anachronistic; seek to discourage or ban
 *   - analytical_observer: can examine whether symbol-continuity is the dominant function or whether other readings better explain persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual as Symbol Continuity Keeper (Catastrophe Memory Kernel)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'f8ebcd97-708f-43e3-b70a-ee88f0a15b5d').
narrative_ontology:cs_kernel_codification('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', distributed).
narrative_ontology:cs_authority_grounding('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', practice).
narrative_ontology:cs_interpretation_layer_present('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d').
narrative_ontology:cs_reading_relation('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', foundational, symbolic_identity_continuity_primary_function).
narrative_ontology:cs_axiom_status(symbolic_identity_continuity_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', symbolic_identity_continuity_primary_function, instrumental).
narrative_ontology:cs_axiom('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', secondary, tradition_form_binds_meaning_across_time).
narrative_ontology:cs_axiom_status(tradition_form_binds_meaning_across_time, holdable).
narrative_ontology:cs_axiom_grounding('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', tradition_form_binds_meaning_across_time, deontological).
narrative_ontology:cs_reference_frame('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', tradition_form_as_identity_vessel).
narrative_ontology:cs_drift_state('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', contemporary_assimilationist_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f8ebcd97-708f-43e3-b70a-ee88f0a15b5d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, ritual_participants).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, ritual_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians of the ritual form—elders, clergy, community leaders—who maintain the prescribed actions, words, and symbolic objects across generations. They understand themselves as transmitters of a heritage that constitutes the group's identity. Their 'agenda' is preserving the ritual exactly as received, resistant to modification even when contexts change.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, tradition_carriers, agenda_setter,
    organized, civilizational, identity_locked, regional).

% Community members who perform or attend the ritual. They gain a sense of continuity with ancestors, reinforcement of group membership, and integration into a larger historical narrative. They also bear the cost of performing actions whose immediate survival utility has atrophied—time spent on ritual is time not spent on adaptive response or economic gain.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_participants, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, ritual_participants, payer).

% Community members (often younger, or those focused on immediate survival) who recognize that the ritual's original function has changed or that modified practice would better serve current needs. They would advocate for flexibility in form, but their voices are structurally marginalized by the tradition-carriers' custodial authority.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_advocates, excluded,
    moderate, biographical, constrained, regional).

% State authorities, dominant-culture institutions, or assimilationist pressures that view the ritual as anachronistic, wasteful, or subversive and seek to discourage or ban it. From their perspective, the ritual's symbolic continuity-function is irrelevant; what matters is whether it impedes integration or state control.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, externally_imposed_pressure_agents, excluded,
    powerful, biographical, arbitrage, national).

% Scholar or analyst examining whether the ritual's primary function is symbolic continuity (identity preservation across time) or whether other functions—trauma encoding, boundary maintenance, survival skill transmission—better explain why the constraint persists.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and group identity across generations by anchoring shared meaning in repeated, prescribed symbolic action. Creates a time-bridge: participants perform actions their ancestors performed, embedding themselves in a historical continuity that constitutes 'who we are.'
% TRANSFER_FUNCTION: Transfers symbolic authority and group-identity claims from one generation to the next through embodied practice. Participants give time and attention to ritual; they receive in return a narrative position—membership in a lineage, a sense of historical rootedness—that would be available to them through no other channel.
% ABSENT_VOICES: Adaptive modification advocates and assimilationist authorities are structurally excluded. The modification advocates would argue that the ritual's form has become decoupled from its function and that flexibility would strengthen actual group survival; external pressure agents would argue the entire symbolic-continuity function is illusory or dysfunctional. Neither group is invited to the table where ritual transmission happens.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared, the group's sense of historical continuity with ancestors would suffer immediate loss—participants would lose a primary mechanism for experiencing themselves as inheritors of a lineage. What would rearrange is identity and collective memory, not material survival (the ritual's operational survival yield is negligible). Some community members would argue the group would actually strengthen by adapting; external observers would argue assimilation would accelerate; tradition-carriers would say the group's soul would be lost.
% FOUNDING_PROBLEM: After catastrophe (persecution, displacement, attempted genocide), the community must preserve its collective identity and historical memory across generations despite disruption of normal institutional life and the temptation to abandon distinguishing practices for safety or assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Tradition-carriers attest the founding problem persists: the ritual is the primary means by which identity continuity is maintained despite ongoing pressure. Independent historians and anthropologists studying post-catastrophe communities attest that symbolic continuity functions as a resilience mechanism—groups that maintain shared ritual practice show stronger intergenerational identity transmission than those that abandon it, even when the ritual's original survival functions have been superseded.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low because the constraint does not concentrate material benefits on an identifiable agent—the beneficiary is 'tradition-continuity itself,' an abstract condition, not a person or institution collecting rents. Suppression is very low (0.12) because the constraint's persistence depends on voluntary participation rooted in identity-fusion, not on external coercion or barrier-creation. Theater rises over time (0.42 → 0.58) because the measurement interval spans catastrophe recovery: in the immediate post-catastrophe phase, ritual carries urgent information (trauma encoding, survival skills, boundary-marking); as the community stabilizes and external survival pressure eases, the ritual persists but increasingly functions as performance-of-continuity rather than transmission of survival-critical knowledge. This is not a failing of the ritual—it is how symbolic continuity works—but it does mean that what was once a multi-function constraint gradually concentrates into a single (symbolic) function. Theater_ratio rising from 0.42 to 0.58 reflects this functional concentration, not constraint decay. Accessibility_collapse is moderate (0.42) because alternatives to ritual (historical study, cultural narrative, institutional memory) exist and are accessible, yet the ritual's embodied, generational form is psychologically and socially difficult to replace once identity has fused with it. Resistance is low (0.35) because there is no coercive force driving participation—resistance comes from adaptation advocates and external pressures, both structurally excluded, not from the majority of tradition-carriers and participants who embrace the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The tradition-carriers and ritual-participants read this constraint as the solution to the founding problem—how to preserve collective identity across generations despite catastrophic disruption. From their seated position, the ritual is self-evidently necessary and beneficial; they resist modification because modification threatens the very continuity the ritual exists to preserve. The adaptive-modification advocates and external pressure agents read the same practice differently: as anachronistic, rigid, or functionally decoupled from present survival needs. They would see the rising theater_ratio as evidence that the ritual has become hollow performance, whereas the tradition-carriers see it as evidence of successful transformation into enduring symbol. The engine computes these perspectival differences from the power/exit_options/role configuration: tradition-carriers hold identity-locked exit and agenda-setter role, so they see rope (genuine coordination); modification advocates hold constrained exit and excluded role, so they see snare (constraints imposed on them without voice); external pressure agents hold arbitrage exit and excluded role, so they see the entire constraint as dysfunctional theater. No one seat's reading is 'correct'—the classification gap is the structural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition-carriers (agenda_setter, organized power, identity-locked exit) experience d ≈ 0.15: they benefit from the constraint (it preserves the tradition they steward and gives them custodial authority) and have arbitrage-grade exit at the institutional level—they could modify the tradition if they chose. Ritual-participants (beneficiary + payer, moderate power, identity-locked exit) experience d ≈ 0.5: they gain identity-continuity (genuine benefit) and bear a real cost (time, effort, opportunity cost of not adapting). The cost does not feel extractive because it is bundled with the benefit—identity-fusion makes the boundary between cost and benefit psychologically transparent. Modification-advocates (excluded, constrained exit) would compute d higher: they bear the cost of constraint-persistence (group rigidity, foreclosed adaptive options) but are not counted in the beneficiary set and cannot shape the tradition. External pressure agents experience the constraint as purely extractive (d ≈ 1.0) because it opposes their assimilationist agenda and they have no seat at the transmission table. The schema captures this by declining to declare modification-advocates or external agents as beneficiaries—they are excluded, and the beneficiary set (tradition_continuity) is abstract. The directionality divergence is the structural fact the engine detects.
 *
 * MANDATROPHY ANALYSIS:
 *   The foundational question: is the ritual's founding problem (preserving identity across catastrophic disruption) still live, or has the constraint's function atrophied while its form persists? The founding_problem_status is 'live' because empirical evidence (scholars studying post-catastrophe communities, testimony from tradition-carriers) confirms that symbolic continuity remains critical to intergenerational identity transmission. However, the rising theater_ratio (0.42 → 0.58) and the low extractiveness suggest that the constraint has undergone functional transformation: it solves the founding problem, but it does so increasingly through performance of continuity rather than transmission of survival-critical knowledge. This is NOT mandatrophy in the classical sense (function dead, form persists by inertia)—the function is live. But it is also not the rope-type coordination that the constraint is classified as under this reading. Under the trauma_encoding_reading or survival_competence_reading (sibling readings), the same ritual would be classified differently because the founding problem would be framed differently. The symbol_continuity_reading isolates the identity-preservation function as primary; other readings would argue that function is secondary to trauma-encoding or survival-skill transmission. The mandatrophy analysis, therefore, routes through the reading_relations: this reading coexists_with the trauma_encoding and survival_competence readings; each reading isolates a different function as primary; if any of the other functions is dead while symbol-continuity is live, those readings would show different classifications (piton, snare, or mountain depending on the function). The measurement data does not resolve which reading is structurally primary—that is precisely what the omega variables capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    function_precedence_under_catastrophe,
    'Is symbolic identity-continuity the PRIMARY function the ritual serves, or is it a byproduct of other functions (trauma-encoding, survival-skill transmission, boundary-maintenance) that remain functionally critical?',
    'Ethnographic analysis comparing post-catastrophe communities where ritual practice varies systematically: communities that maintain rituals with strong symbol-continuity framing but weak survival-content against communities with strong survival-content but weak identity-framing. Measure intergenerational identity persistence and adaptive capacity in each group. If identity-persistence correlates with symbol-continuity practices independent of survival-content, this reading''s functional claim is supported. If identity-persistence correlates primarily with survival-content regardless of symbol-continuity framing, the trauma-encoding or survival-competence reading better explains the constraint.',
    'If symbol-continuity is primary: the constraint remains rope (genuine coordination for identity preservation) and theater-rise is not pathological but functional transformation. If symbol-continuity is secondary: the constraint may be better classified under a sibling reading (trauma-encoding or survival-competence), changing the type and extraction assessment. The measurement data (rising theater, low extractiveness) would be reinterpreted: theater-rise might indicate functional atrophy rather than functional transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(function_precedence_under_catastrophe, empirical, 'Whether symbol-continuity is the ritual''s primary function or a secondary effect of other functions.').

omega_variable(
    reading_foreclosure_by_catastrophe_outcome,
    'Do the sibling readings (trauma_encoding, survival_competence, boundary_maintenance) foreclose each other, or do they coexist as simultaneously live framings that different community members adopt?',
    'Qualitative analysis of community discourse: document whether tradition-carriers, ritual-participants, and modification-advocates argue that the ritual''s PRIMARY function is one of the four (enforcing boundaries, preserving identity, encoding survival skills, encoding trauma) or whether they invoke multiple functions simultaneously depending on context. If each stakeholder group adopts a different primary reading (e.g., elders emphasize identity-continuity while younger participants emphasize survival-skills), the readings coexist. If a single authority (e.g., a canonical text or founding leader) specifies one function and other functions are treated as secondary implications, one reading may have institutional precedence but not logical foreclosure.',
    'If readings foreclose: at most one of the four readings can be structurally true; the others are misreadings of the same constraint. If readings coexist: the catastrophe_memory_kernel instantiates four distinct constraints, each with its own epsilon, each with its own type. This omega determines whether the network.affects_constraints linkage is a family (four related constraints) or a family tree (one primary constraint with three derived or disputed readings).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_by_catastrophe_outcome, conceptual, 'Whether the four readings of the catastrophe-memory kernel are logically compatible or mutually exclusive.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the low suppression (0.12) because the ritual is genuinely non-coercive (participants choose freely, exit is merely costly), or because suppression has become internalized (participants believe they want to perform the ritual, cannot imagine exit, and would resist modification even if exit were costless)?',
    'Post-disruption ethnography: if the constraint were suddenly removed by external force (e.g., state ban, diaspora relocation, institutional collapse), would participants quickly reconstitute the ritual, or would suppression dissipate because external enforcement was all that held participation? If reconstitution is immediate and widespread despite no enforcement, suppression is primarily internalized and should be scored higher. If participation drops sharply or does not recover, suppression was structural (identity-fusion is itself the structure that binds).',
    'If suppression is primarily internalized: the constraint is more binding than the 0.12 score suggests; effective extraction may be higher because exit is not merely costly but psychologically foreclosed. If suppression is primarily structural (institutional rules preventing modification): the low score is accurate; participants bear low suppression but participate willingly. The measurement trajectory (suppression_requirement rising modestly from 0.08 to 0.12) suggests that as immediate external pressure eases, suppression is increasingly maintained through identity-fusion rather than enforcement machinery—a sign of internalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the ritual''s low suppression score reflects genuine voluntary participation or internalized constraint.').

omega_variable(
    reading_instantiation_ambiguity,
    'This constraint is authored as ONE READING of the catastrophe_memory_kernel. But which reading is this community ACTUALLY instantiating? If tradition-carriers frame the ritual as trauma-encoding and boundary-maintenance, while scholars frame it as symbol-continuity, which framing determines the constraint''s structural identity?',
    'Authority analysis: whose framing of the ritual''s function is authoritative in the community itself? If tradition-carriers'' own narrative emphasizes trauma and boundaries, the constraint is better instantiated under trauma_encoding_reading or boundary_maintenance_reading. If the community''s authority structure is silent on function and outside scholars impose the symbol-continuity frame, the frame is analytic (belongs in the observer seat), not constitutive. This is a CONCEPTUAL question, not empirical: it asks which reading-authority is operative, not whether the functions actually occur.',
    'If the tradition-carriers'' own framing prioritizes symbol-continuity: this reading is structurally grounded and the authored classification stands. If the tradition-carriers prioritize trauma or boundary, the constraint should be re-authored under the sibling reading that matches their authority-grounded framing. If no reading is privileged in the tradition itself, the four readings are genuinely coequal and the choice of which to author is an analytical decision, properly documented in the network structure (affects_constraints bidirectionally) and in this omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_instantiation_ambiguity, conceptual, 'Which reading''s authority structure is operative: the community''s own framing of ritual function, or the analytical framing imposed by scholarship?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.56).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 75, 0.58).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 32, 0.26).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 75, 0.29).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 16, 0.09).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 32, 0.1).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 75, 0.13).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__symbol_continuity_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the catastrophe_memory_kernel. The kernel is a contested commitment: what function does ritual practice serve in post-catastrophe communities? This reading (symbol_continuity_reading) isolates symbolic identity-preservation as the primary function; sibling readings isolate trauma-encoding, survival-competence transmission, and boundary-maintenance. Each reading instantiates a distinct constraint with different epsilon values, different beneficiary structures, and potentially different types. The readings coexist across different community members' frames; no single reading is foreclosed by the others' logical structure. Relationship to siblings: symbol_continuity reading INFLUENCES the other three (shapes how their functions are evaluated within identity-preservation frame) but does not FORECLOSE them (all four readings remain live in community discourse). See omega 'reading_foreclosure_by_catastrophe_outcome' for exploration of whether these readings are genuinely coexistent or logically mutually exclusive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
