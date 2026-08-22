% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_boundary_maintenance, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Mourning-Practice Boundary Enforcement
 *   domain: religious/social/collective_memory
 *
 * SUMMARY:
 *   A group bound by historical catastrophe (persecution, genocide, diaspora)
 *   maintains cohesion through shared mourning-practice. The ritual
 *   prescribes forms of lamentation, temporal cycles of commemoration,
 *   participation requirements, and boundary markers that distinguish
 *   group-members from outsiders. This reading frames the constraint as
 *   boundary-maintenance: the ritual's primary function is enforcing who
 *   belongs to the group and who does not, by making the mourning-practice
 *   the gateway to inclusion. Individual mourners bear the cost of conformity
 *   (suppressed idiosyncratic grief, time commitment, identity-lock);
 *   out-group members bear the cost of exclusion (systematic inability to
 *   participate, confirmation of outsider status). The group derives cohesion
 *   from the synchronized practice. The reading is one of four competing
 *   interpretations of the same contested kernel (catastrophe_memory_kernel);
 *   the others frame it as survival-competence encoding, symbol-continuity
 *   transmission, or intergenerational trauma encoding. This reading ONLY
 *   instantiates the boundary-maintenance interpretation.
 *
 * KEY AGENTS:
 *   - ritual_authority: administers and enforces the mourning-practice rules; derives authority from lineage and boundary-maintenance function
 *   - individual_mourners: powerless, identity-locked; bear conformity costs and suppression of idiosyncratic grief; benefit from group inclusion
 *   - group_boundary_maintainers: moderate power; enforce adherence and mark deviations; benefit from boundary-maintenance function
 *   - out_group_members: structurally excluded; their exclusion confirms the boundary
 *   - in_group_cohesion: abstract good vindicated by the practice; the primary coordination benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Mourning-Practice Boundary Enforcement").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious/social/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'a118d717-4110-4b5c-9cb1-cc0a3b783672').
narrative_ontology:cs_kernel_codification('a118d717-4110-4b5c-9cb1-cc0a3b783672', distributed).
narrative_ontology:cs_authority_grounding('a118d717-4110-4b5c-9cb1-cc0a3b783672', lineage).
narrative_ontology:cs_interpretation_layer_present('a118d717-4110-4b5c-9cb1-cc0a3b783672').
narrative_ontology:cs_reading_relation('a118d717-4110-4b5c-9cb1-cc0a3b783672', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a118d717-4110-4b5c-9cb1-cc0a3b783672', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a118d717-4110-4b5c-9cb1-cc0a3b783672', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('a118d717-4110-4b5c-9cb1-cc0a3b783672', foundational, mourning_practice_is_boundary_enforcement_mechanism).
narrative_ontology:cs_axiom_status(mourning_practice_is_boundary_enforcement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('a118d717-4110-4b5c-9cb1-cc0a3b783672', mourning_practice_is_boundary_enforcement_mechanism, deontological).
narrative_ontology:cs_axiom('a118d717-4110-4b5c-9cb1-cc0a3b783672', secondary, group_cohesion_worth_individual_autonomy_cost).
narrative_ontology:cs_axiom_status(group_cohesion_worth_individual_autonomy_cost, holdable).
narrative_ontology:cs_axiom_grounding('a118d717-4110-4b5c-9cb1-cc0a3b783672', group_cohesion_worth_individual_autonomy_cost, deontological).
narrative_ontology:cs_reference_frame('a118d717-4110-4b5c-9cb1-cc0a3b783672', boundary_maintenance_primary_function).
narrative_ontology:cs_drift_state('a118d717-4110-4b5c-9cb1-cc0a3b783672', contemporary_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a118d717-4110-4b5c-9cb1-cc0a3b783672', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_mourners).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, individual_mourners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, group_boundary_maintainers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers, defines, and enforces the mourning-practice requirements. Determines what forms of grief are acceptable, what participation is mandatory or optional, what deviations from the prescribed ritual constitute boundary violation. Derives authority from lineage (the ritual's standing in the group's tradition) and from the boundary-maintenance function itself (the group continues because of these practices).
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_authority, agenda_setter,
    institutional, generational, analytical, regional).

% Participate in prescribed mourning practices (prescribed forms of lamentation, ritual meals, temporal cycles of commemoration, dress codes, speech restrictions). Bear costs: time, emotional labor, conformity pressure, suppression of idiosyncratic grief expression. Also benefit from group inclusion and the cognitive structure the ritual provides for processing catastrophe. Cannot exit without losing group membership and the identity constituted through the group.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, individual_mourners, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, individual_mourners, beneficiary).

% Enforce adherence to the mourning-practice rules: mark deviations, enforce correction through social pressure, exclude persistent violators. Benefit from the boundary-maintenance function itself — the ritual's enforcement coheres the group against assimilation and diffusion. Are themselves subject to the ritual, but derive status from their enforcer role.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, group_boundary_maintainers, beneficiary,
    moderate, generational, constrained, regional).

% Structurally excluded from the mourning practice (cannot participate, cannot perform the rituals). If they attempt to participate, they violate boundary — their outsider status is confirmed and corrected. The exclusion is the mechanism: the boundary is maintained by demonstrating who cannot/may not mourn according to these rules.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_members, payer,
    moderate, biographical, constrained, regional).

% The abstract good the ritual enforces — the group's continuity, internal synchronization, collective identity. Not an actor; a vindicated proposition. Listed here for completeness because the constraint's beneficiary structure names it as the primary coordination good being produced.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion, beneficiary,
    analytical, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion).

% Documents the ritual system from outside, noting the boundary-maintenance function and the extractive costs it imposes on individual mourners. Neither participates nor enforces; records the structure and its effects.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, observing_anthropologist, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes grief expression across the group, transforming individual loss into collective memorial practice. Establishes shared symbols, temporality, and language for processing catastrophe. Coordinates the group's identity-boundary against external assimilation and internal fragmentation.
% TRANSFER_FUNCTION: Moves individual autonomy over grief expression (what to feel, how to express it, when to move past it) to the ritual authority and the group boundary-maintainers. Moves individual out-group relations (freedom to associate with outsiders, to adopt their mourning practices, to blur the boundary) to boundary-maintenance requirements. The transfer is paid by individual mourners and out-group members in conformity costs and exclusion.
% ABSENT_VOICES: Individuals whose grief does not fit the prescribed form (unusual loss, complicated emotion, different cultural background, neurodivergence in ritual performance). Out-group members who would object to their systematic exclusion from the mourning practice but are not seated in the conversation. Defectors and apostates who have left the group specifically because of ritual conformity pressure.
% DISAPPEARANCE_RATIONALE: If the mourning-practice enforcement vanished overnight, individual grief expression would diversify, some mourners would adopt practices from other groups or invent idiosyncratic ones, the group's boundary would become permeable (out-group members could participate and integrate), and group cohesion would attenuate — the group would still exist but would reorganize around different boundary-maintenance mechanisms or accept greater internal heterogeneity.
% FOUNDING_PROBLEM: After catastrophe (persecution, genocide, diaspora, displacement), the group faced fragmentation: mourners isolated, grief expressions diverging, boundary against the hostile outside world collapsing, younger members assimilating into surrounding cultures. A synchronized mourning practice solved the fragmentation problem by establishing ritual as the primary mechanism for coordinating identity and exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Historians of diaspora communities and persecution survival attest the founding problem was live and acute in the periods when the mourning practices were codified. Anthropologists and ritual scholars document that the practices continue to serve a boundary-maintenance function in contemporary groups. However, sociologists and critics of ritual-based conformity argue the founding problem (immediate survival threat, risk of cultural dissolution) has substantially diminished for many groups in contemporary settings, yet the ritual enforcement persists—suggesting the practice is now maintained for in-group cohesion rather than survival necessity. No unanimous consensus; substantial scholarly disagreement on whether the founding problem remains live or has been superseded.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.58 at interval end) because the constraint enforces conformity and exclusion rather than material extraction, but the costs to individual autonomy and out-group relations are real and sustained. Suppression is high (0.62) because the boundary-maintenance function depends on active enforcement — deviations are marked, corrected, and violations are punished by exclusion. Theater is low-moderate (0.28) because while some ritual performance is genuinely generative (transforms grief into shared meaning), a growing fraction is performative conformity — mourners performing the practice to avoid exclusion rather than to express genuine grief. The temporal series shows extractiveness and suppression rising in early phases (as the group consolidates and externalizes the boundary against threats) and plateauing in later phases (as the external threat diminishes and the ritual persists more by institutional inertia and identity-lock than by functional necessity). This pattern is consistent with a constraint that was originally functional (high coordination value relative to extraction cost) and has degraded toward piton-like behavior (extraction cost persists after the functional justification has weakened). The individual-mourner seat experiences high identity-lock (cannot exit without losing group membership, which is constitutive of identity), making their effective extraction χ higher than the base ε because of the trapped-option amplification.
 *
 * PERSPECTIVAL GAP:
 *   Ritual authority and group boundary-maintainers experience the constraint as genuine coordination (the group continues, the boundary holds, internal cohesion is maintained through shared practice). Individual mourners, especially those whose grief does not fit the prescribed form, experience it as extraction (conformity pressure, suppression of authentic emotion, identity-lock preventing exit). Out-group members experience it as exclusion (systematic inability to participate, confirmation of outsider status regardless of their other relationships to the group). The engine will compute different types per seat from the structural data: the agenda-setter seat will derive lower d (nearer beneficiary), the powerless individual-mourner seat will derive higher d (nearer target), and the out-group-member seat will derive very high d (pure exclusion, no benefit). This divergence is not a defect; it is the point of per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual authority (institutional, analytical exit): d near 0.2 (beneficiary — sets rules, derives authority from the function, benefits from continuation). Individual mourners (powerless, identity-locked exit): d near 0.75 (target — bear conformity costs, cannot exit without losing constitutive identity). Group boundary-maintainers (moderate, constrained exit): d near 0.45 (symmetric — enforce the boundary and derive status, but are also subject to conformity). Out-group members (moderate, constrained exit): d near 0.85 (target — systematically excluded, no benefits from participation, excluded by the boundary-enforcement mechanism itself). The identity-locked exit status of individual mourners is the critical structural feature: it amplifies their effective extraction because they cannot arbitrage or defect. The identity-lock is not imposed externally; it is the result of the group's constitutive practice — the mourning-ritual IS how the group members recognize themselves as members. Leaving the ritual is not merely defecting from a rule; it is ceasing to be part of the group that understands itself through that practice.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not yet mandatrophic. The founding problem (catastrophe-driven group fragmentation) remains live in the diaspora context: younger members still face assimilation pressure, external threats to group boundary persist, and the ritual remains functional in coordinating identity and exclusion. However, the temporal series shows the theater_ratio rising (from 0.08 toward 0.28), suggesting that a growing fraction of the ritual's performance is conformity-driven rather than grief-driven — a signal that the functional justification may be weakening even as the enforcement persists. If theater_ratio continues to rise and the founding problem status shifts from 'live' to 'substantially mitigated,' the constraint would approach mandatrophy: the practice persists because it is institutionally embedded and identity-fused, not because it solves the problem it was built to solve. The current classification as tangled_rope (genuine coordination + asymmetric enforcement) is defensible; piton (degraded, inertial) would be the end-state classification if the founding problem dies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression structural (external enforcement, exclusion mechanisms, social pressure from outside the individual''s decision-making) or internalized (the individual has incorporated the ritual''s authority into their self-concept, experiences the conformity as autonomous choice)?',
    'Post-exit trajectory: track individuals who leave the group and measure whether suppression persists after exit. If suppression remains (individuals continue to experience the ritual as authoritative even after leaving the group), significant portion is internalized; if suppression dissipates, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the measured structural force suggests — the target carries the suppression internally. If primarily structural, remedies (opening participation, weakening enforcement) would effectively reduce suppression. Identity-lock status depends partly on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression in mourning-practice conformity is externally imposed or internally maintained.').

omega_variable(
    boundary_maintenance_vs_trauma_transmission,
    'Is the ritual primarily maintained because it enforces group boundaries, or because it transmits intergenerational trauma-adaptation? These readings coexist; the question is which is primary.',
    'Compare groups where the boundary remains salient (active external threat, ongoing diaspora) vs. groups where the boundary has become less salient (safe, integrated, low assimilation pressure). If extractiveness/suppression remains high in low-threat environments, boundary-maintenance is primary; if it declines sharply, trauma-transmission/survival-competence is primary.',
    'If boundary-maintenance is primary, fixing the constraint requires addressing group cohesion alternatives; if trauma-transmission is primary, fixing it requires addressing intergenerational psychological processes. This affects the mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_maintenance_vs_trauma_transmission, conceptual, 'Which reading of the contested kernel is structurally primary — boundary maintenance or trauma transmission.').

omega_variable(
    identity_lock_constitutive_vs_imposed,
    'Is the individual mourner''s identity-lock constitutive (the group is understood through the mourning-practice, ceasing the practice means ceasing to be part of the group) or imposed (the group would continue to claim the individual as a member even if they declined to participate)?',
    'Interview data and ethnographic observation: do individuals who decline mourning-practice participation report that they feel outside the group, or do the group''s representatives say they are still members? Are there models of ''cultural maintenance'' without ritual participation?',
    'If constitutive, the identity-lock amplifies effective extraction χ because the individual cannot arbitrage (exit means losing identity, not just group membership). If imposed, exit is more viable and χ is lower. Affects directionality derivation for individual-mourner seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_constitutive_vs_imposed, conceptual, 'Whether identity-lock in mourning-practice participation is constitutive of group membership or normatively imposed.').

omega_variable(
    committer_ambiguity_reading_plurality,
    'Is this boundary-maintenance reading the endorsed reading of the catastrophe_memory_kernel, or one reading among live alternatives held by different members of the group?',
    'Documentary and interview evidence: do all group representatives frame the ritual primarily as boundary-maintenance, or do different representatives emphasize different functions (survival-competence, trauma-encoding, symbol-continuity)? Is there a dominant authority that enforces one reading, or plural authorities allowing coexisting readings?',
    'If one reading is dominant, classification is clear: boundary-maintenance constraint with CS structure rooted in the authority grounding. If readings coexist, the constraint is more accurately modeled as a contested kernel with four coexisting constraint interpretations (four separate JSON files linked by network.affects_constraints), not one story trying to capture all readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_ambiguity_reading_plurality, conceptual, 'Whether the boundary-maintenance reading is the primary or one of several coexisting readings of the catastrophe-memory kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t14, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement(cata_tr_t28, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 28, 0.18).
narrative_ontology:measurement(cata_tr_t42, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 42, 0.24).
narrative_ontology:measurement(cata_tr_t56, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 56, 0.27).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t14, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 14, 0.48).
narrative_ontology:measurement(cata_be_t28, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 28, 0.52).
narrative_ontology:measurement(cata_be_t42, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 42, 0.56).
narrative_ontology:measurement(cata_be_t56, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 56, 0.57).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 70, 0.58).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cata_su_t14, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 14, 0.51).
narrative_ontology:measurement(cata_su_t28, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 28, 0.55).
narrative_ontology:measurement(cata_su_t42, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 42, 0.6).
narrative_ontology:measurement(cata_su_t56, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 56, 0.62).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 70, 0.62).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__boundary_maintenance_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested catastrophe_memory_kernel. The kernel is a persisting mourning-practice in a catastrophe-marked group; different readings of the kernel instantiate different constraints. Boundary-maintenance_reading frames the primary function as group-boundary enforcement. Symbol-continuity_reading frames it as preservation of collective identity across time. Survival-competence_reading frames it as transmission of persecution-survival adaptive capacity. Trauma-encoding_reading frames it as encoding intergenerational trauma as warning. Each reading has distinct ε (extractiveness), distinct beneficiary/victim structures, and distinct classification. The kernel itself is not a constraint; it is the shared text/practice that different parties read differently. The four readings are interdependent: if one reading's axioms are foreclosed or overridden, it affects the validity landscape for others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
