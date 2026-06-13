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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Mourning Ritual: Symbolic Continuity and Collective Identity Preservation
 *   domain: religious/social/memorial
 *
 * SUMMARY:
 *   This constraint describes mourning ritual as a mechanism for preserving
 *   symbolic continuity and collective identity in the aftermath of
 *   catastrophe. The reading instantiated here holds that ritual's primary
 *   function is identity-coordination through synchronized re-enactment of
 *   the catastrophe narrative—not the preservation of operational
 *   survival-competence (as the survival_competence_reading claims) nor an
 *   atrophied remnant of such function (as the hybrid_atrophy_reading
 *   asserts). Ritual coordinates collective memory by making participation in
 *   the performance of the event's meaning a requirement of group membership.
 *   The constraint is CLAIMED as rope—voluntary coordination around shared
 *   identity—and the authored metrics support that: low to moderate
 *   extractiveness (the emotional labor is distributed and roughly reciprocal
 *   across participants), minimal suppression (participation is opt-in,
 *   enforced by social reinforcement rather than coercion), and low theater
 *   ratio (the symbolic work is genuine, not performative cover for hidden
 *   function). Measurement series span 100 time units to model
 *   multi-generational dynamics; the slight uptick in extractiveness reflects
 *   the accumulation of emotional labor required as the direct witness
 *   generation ages and the burden of testimony-transmission intensifies,
 *   with theater ratio rising as later generations enact the ritual with less
 *   direct experiential grounding.
 *
 * KEY AGENTS:
 *   - in_group_mourners: voluntary participants in ritual practice; benefit from identity continuity and emotional coherence
 *   - ritual_transmitting_communities: maintain ritual practice, transmit narrative; benefit from group persistence as an organized entity
 *   - secular_descendants: structurally excluded because they reject the ritual's symbolic framing while remaining group members
 *   - historical_survivors: bear the cost of re-exposure to trauma and emotional labor of narrative integration
 *   - future_generations: receive the catastrophe as pre-interpreted through ritual frame; benefit from coherence but lose individual interpretation autonomy
 *   - analytical_observer: measures constraint structure without stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Mourning Ritual: Symbolic Continuity and Collective Identity Preservation").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious/social/memorial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'f191d2bc-8d54-46bc-a09e-e96bd841d771').
narrative_ontology:cs_kernel_codification('f191d2bc-8d54-46bc-a09e-e96bd841d771', distributed).
narrative_ontology:cs_authority_grounding('f191d2bc-8d54-46bc-a09e-e96bd841d771', practice).
narrative_ontology:cs_interpretation_layer_present('f191d2bc-8d54-46bc-a09e-e96bd841d771').
narrative_ontology:cs_reading_relation('f191d2bc-8d54-46bc-a09e-e96bd841d771', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('f191d2bc-8d54-46bc-a09e-e96bd841d771', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('f191d2bc-8d54-46bc-a09e-e96bd841d771', foundational, ritual_function_primary_is_symbolic_identity).
narrative_ontology:cs_axiom_status(ritual_function_primary_is_symbolic_identity, holdable).
narrative_ontology:cs_axiom_grounding('f191d2bc-8d54-46bc-a09e-e96bd841d771', ritual_function_primary_is_symbolic_identity, deontological).
narrative_ontology:cs_axiom('f191d2bc-8d54-46bc-a09e-e96bd841d771', foundational, collective_memory_constitutes_shared_group_being).
narrative_ontology:cs_axiom_status(collective_memory_constitutes_shared_group_being, holdable).
narrative_ontology:cs_axiom_grounding('f191d2bc-8d54-46bc-a09e-e96bd841d771', collective_memory_constitutes_shared_group_being, conventional).
narrative_ontology:cs_reference_frame('f191d2bc-8d54-46bc-a09e-e96bd841d771', post_catastrophe_ritual_continuity).
narrative_ontology:cs_drift_state('f191d2bc-8d54-46bc-a09e-e96bd841d771', contemporary_secular_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f191d2bc-8d54-46bc-a09e-e96bd841d771', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, in_group_mourners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, ritual_transmitting_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__mourning_practice_reading, historical_survivors).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, collective_identity_constituted_through_shared_memory).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, symbolic_continuity_independent_of_operational_function).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in mourning rituals that mark shared catastrophe memory and affirm collective identity. They benefit from the emotional coherence the ritual provides, the sense of continuity with ancestors who performed the same practice, and the public marker of group membership. Participation is voluntary; they can choose to perform the ritual, modify it, or absent themselves.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, in_group_mourners, beneficiary,
    moderate, generational, mobile, local).

% Maintain the ritual script, teach younger members the procedures and narrative context, and enforce continuity through social reinforcement (not legal coercion). They collect the benefit of group persistence and continuity—the ritual sustains the community's internal coherence and external identity marker. Agenda-setting is distributed: no single authority imposes the ritual; it emerges from community consensus about what practices matter for 'us.'
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_transmitting_communities, beneficiary,
    organized, civilizational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, ritual_transmitting_communities, agenda_setter).

% Members of the in-group whose secular worldviews or skepticism about the ritual's meaning place them outside its natural constituency. They are not barred from participation, but they experience a cognitive dissonance—the ritual asks them to affirm symbolic meanings they no longer endorse. Their objection (that the ritual is 'just' theater) is heard but has not overturned the community's commitment to perform it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, secular_descendants, excluded,
    moderate, biographical, mobile, local).

% First-generation bearers of the catastrophe memory. They experience the ritual as an obligation—to testify, to frame the disaster within a narrative structure, to make it meaningful through the community's interpretive lens. The cost they bear is emotional: they must re-expose themselves to traumatic memory in the ritual context and work to connect their lived experience to the community's symbolic interpretation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, historical_survivors, payer,
    powerless, biographical, identity_locked, local).

% Receive the catastrophe memory and collective identity already-framed through ritual language and practice. They inherit a pre-interpreted past—the ritual shapes what the catastrophe means to them before they can form their own interpretation. They benefit from the coherence and continuity this provides but are constrained by the inherited frame.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, future_generations, beneficiary,
    powerless, civilizational, identity_locked, local).

% Examines how ritual organizes memory, identity, and community continuity. Observes the constraint without stakes in its operation and records the structural dynamics for analysis.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes the collective interpretation of catastrophe into a shared symbolic frame; synchronizes the community's memory-performance so that all members participate in affirming that 'this happened to us and it means this.' Solves the coordination problem: without the ritual, the catastrophe would fragment into individual trauma narratives with no unified group identity emerging from it.
% TRANSFER_FUNCTION: Moves emotional labor (testimony, re-exposure to trauma, embodied performance) from future generations backward to first-generation survivors and current ritual participants. The flow is not material—it is the burden of bearing witness and maintaining the narrative under oath. Future generations receive the catastrophe as already-integrated into group meaning; survivors pay the cost of integration itself.
% ABSENT_VOICES: Alternative memory-keepers (secular historians, therapeutic frameworks, legal restitution systems) would argue for event-focused documentation without the identity-fusion that ritual performs. They would advocate for de-coupling catastrophe memory from group identity and allowing individuals to construct their own meaning. These voices are structurally excluded because the ritual's premise is precisely that meaning emerges from collective re-enactment, not from individual interpretation.
% DISAPPEARANCE_RATIONALE: If the mourning ritual vanished overnight, the catastrophe would cease to be a unifying event for the in-group. Collective identity would either fragment (individuals maintain separate traumatic memories) or reorient around new practices. The community would lose the synchronized, periodic re-affirmation of 'we are the people to whom this happened.' Future generations would receive the catastrophe as historical fact, not as living group memory, changing how the community understands itself.
% FOUNDING_PROBLEM: After catastrophe, how does a community maintain internal coherence and transmit that catastrophe's meaning to generations who did not experience it? How does 'we survived this' become a binding identity marker rather than a dividing trauma?
% FOUNDING_PROBLEM_CORROBORATION: Survivors, community ritual leaders, and anthropologists studying post-catastrophe communities all attest that without repeated, synchronized re-enactment of the catastrophe narrative, the community's internal coherence erodes and the meaning of the event becomes subject to re-interpretation by each generation separately. Secular critics contest that the founding problem remains relevant (they argue the event is documented and its trauma should be metabolized individually), but ritual-transmitting communities affirm the founding problem is perpetually live.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).

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
 *   Extractiveness is low-to-moderate (0.28 at interval end) because the constraint solves a genuine coordination problem (how does a community maintain identity across generations?) without creating identifiable victims or concentrated benefits. The emotional labor moved by the constraint is distributed—all participants bear some cost (re-exposure to trauma, time, embodied performance) and all receive the benefit (group identity, continuity, coherence). Suppression is minimal (0.12) because participation is genuinely voluntary—members can decline to participate without legal or severe economic consequence, though they may lose some access to group belonging. Theater ratio rises slightly (from 0.08 to 0.18) over the interval as later generations encounter the ritual with less lived connection to the original catastrophe and more of the practice becomes enactment of inherited symbolic meaning rather than processing of lived experience. This rising ratio is not evidence of extraction; it reflects Goodhart drift where the ritual's observable symbolic performance increases as its emotional integration function (which was higher for survivors) becomes less accessible. The accessibility_collapse value (0.65) is moderate: alternatives to ritual mourning exist (secular commemoration, therapeutic processing, historical documentation), but they do not provide the synchronized identity-coordination that ritual provides, so the ritual's structural advantage persists. Resistance is low-to-moderate (0.35) because the constraint does meet real objection (secular descendants, modernization pressure, therapeutic critique), but these objections have not overturned the community's commitment to the practice; they coexist uneasily within the group.
 *
 * PERSPECTIVAL GAP:
 *   From the in-group mourners' and ritual-transmitting communities' perspective, this is authentic coordination—genuine collective identity emerging from synchronized re-enactment. From the analytical observer's perspective, the same structure organizes memory and constrains individual interpretation. From secular descendants' perspective, it is theatrical enforcement of pre-modern identity framing masquerading as voluntary practice. From historical survivors' perspective, it is an ongoing obligation to make their trauma meaningful within the community's symbolic order. The engine computes these divergent readings from the structural data: all seats see the same constraint (ritual performance) but experience different directionalities because they have different relationships to the identity it coordinates. Survivors (identity_locked exit) and future generations (identity_locked, powerless) sit higher in directionality than voluntary participants (mobile exit). The agenda-setter seat (ritual-transmitting communities) and beneficiary seats (in-group mourners, future generations) sit lower in directionality than excluded seats (secular descendants face the coordination demand without full voice in its definition).
 *
 * DIRECTIONALITY LOGIC:
 *   In-group mourners have mobile exit (they can choose not to participate) and are beneficiaries (they receive identity continuity); directionality d is low, near the beneficiary end (~0.20). Ritual-transmitting communities have organized power, maintain the practice through consensus, and benefit from group persistence; they are dual-positioned (beneficiary and agenda-setter) with distributed authority rather than concentrated control; d is near the symmetric point (~0.45, slightly beneficiary-leaning). Secular descendants are excluded—they experience the coordination demand (participation is expected) but lack full voice in how the ritual is defined; they are trapped between identity membership (they are in-group) and cognitive dissonance (they reject the symbolic frame); d is higher (~0.55, approaching target) despite mobile exit, because the cost of exit is identity loss. Historical survivors are powerless, identity_locked, and bear the emotional labor cost; d is high (~0.75, near target), making them the most structurally vulnerable seat. Future generations are powerless and identity_locked but benefit from coherence; d is moderate (~0.50), as they receive benefit (pre-integrated meaning) without having chosen it. No directionality overrides are needed; the structural derivation from beneficiary/victim declarations plus exit modulation produces coherent assignments.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (how does a post-catastrophe community maintain identity across generations?) remains live. The ritual continues to function as identity coordination; the symbolic work is not atrophied into theater. Mandatrophy is not present on this reading. However, an omega variable addresses the structural ambiguity: does the ritual preserve genuine survival-competence (making it a mountain or hybrid rope-mountain), or does it preserve only symbolic continuity without operational function (making it pure identity coordination)? This reading answers the question as stated in its framing—the constraint preserves symbolic continuity, not survival-competence—which forecloses the survival_competence_reading's core premise while coexisting with the hybrid_atrophy_reading's recognition that modernization has changed the ritual's function. The mandatrophy analysis supports the rope classification: the constraint solves a coordination problem (group identity) without concealing extraction (the cost to participants is transparent and reciprocal) and is genuinely voluntary (no suppression apparatus beyond social reinforcement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_symbolic_function,
    'Does this constraint preserve genuine survival-competence (threat recognition, adaptive response patterns) or only symbolic continuity divorced from operational function?',
    'Ethnographic and historical comparison: do communities that practice this ritual show superior threat-recognition or adaptive capacity compared to communities without it? Do descendants who learn the ritual through enactment show measurable competence gain vs. those who learn it as historical narrative only?',
    'If operational competence is preserved, the constraint is hybrid (rope + mountain component). If only symbolic continuity is preserved, it is pure rope. This reading asserts the symbolic-only answer; the survival_competence_reading asserts the opposite. The mismatch indicates that the kernel permits two coherent readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_symbolic_function, empirical, 'Whether ritual preserves operational survival-competence or symbolic continuity only.').

omega_variable(
    voluntary_participation_boundary,
    'Is the measured suppression (0.12) accurate, or is there hidden suppression in the form of social stigma, economic disadvantage, or identity loss for those who exit?',
    'Longitudinal tracking of participants and non-participants: do non-participants face measurable disadvantage in marriage prospects, employment, property rights, or ritual participation in other ceremonies? Do they report internal pressure (shame, alienation) that persists after their choice not to participate?',
    'If hidden suppression is substantial, the constraint reclassifies toward snare (voluntary appearance with coercive backup). If it remains minimal, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_boundary, empirical, 'Whether exit from ritual participation is truly voluntary or carries hidden costs.').

omega_variable(
    modernization_atrophy_trajectory,
    'Does the rising theater_ratio (from 0.08 to 0.18) indicate genuine Goodhart drift (emotional integration giving way to symbolic performance) or a measurement artifact of increasing participation by generations with less lived trauma?',
    'Qualitative comparison of ritual intensity and emotional coherence reported by participants across generations; ethnographic assessment of whether the ritual''s emotional work is diminishing or shifting in character.',
    'If genuine atrophy, the constraint is drifting toward the hybrid_atrophy_reading (rituals once functioned operationally but now serve only identity). If it is measurement artifact, the rope classification remains stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernization_atrophy_trajectory, conceptual, 'Whether rising theater ratio indicates functional atrophy or measurement artifact from generational distance.').

omega_variable(
    kernel_reading_contest_structure,
    'This constraint is one reading of a three-way contested kernel. The other readings (survival_competence, hybrid_atrophy) would assign different ε values and type classifications to the same ritual practice. Is the structural divergence because the readings measure different observables, or because they instantiate genuinely distinct constraints?',
    'Recursive application of ε-invariance principle: if changing the observable (operational competence vs. symbolic meaning) would change ε, then this is not one constraint viewed from three angles—it is three distinct constraints riding the same ritual practice.',
    'If three distinct constraints, each reading is a separate constraint story and they are linked by network.affects_constraints. If one constraint with reading-dependent measurement, the corpus must represent the contest through omega variables and methodological annotations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the kernel contest indicates three structurally distinct constraints or one constraint with committer-determined reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t33, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 33, 0.14).
narrative_ontology:measurement_basis(cata_tr_t33, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.17).
narrative_ontology:measurement_basis(cata_tr_t50, observed).
narrative_ontology:measurement(cata_tr_t66, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 66, 0.19).
narrative_ontology:measurement_basis(cata_tr_t66, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement_basis(cata_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t33, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 33, 0.27).
narrative_ontology:measurement_basis(cata_be_t33, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.29).
narrative_ontology:measurement_basis(cata_be_t50, observed).
narrative_ontology:measurement(cata_be_t66, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 66, 0.28).
narrative_ontology:measurement_basis(cata_be_t66, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(cata_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__mourning_practice_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_preservation kernel. The mourning_practice_reading asserts that ritual preserves symbolic continuity without operational function and is pure rope coordination. The survival_competence_reading asserts ritual preserves threat-recognition competence and may be mountain or hybrid. The hybrid_atrophy_reading asserts ritual once preserved competence but has atrophied to pure symbolic function under modernity. The three readings are committer-axis competitors: they differ in their assessment of the ritual's primary function and whether modernization has changed that function. They share the same observable (ritual performance in post-catastrophe communities) but assign different ε values and type classifications depending on which function (operational vs. symbolic) is treated as primary. Each reading is a separate constraint story linked by this network edge. The kernel itself (catastrophe_memory_preservation as a practice across cultures) is contested; the contest cannot be resolved within a single constraint story because ε is invariant only within a single reading. Decomposition into three stories following DP-001 (ε-invariance principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
