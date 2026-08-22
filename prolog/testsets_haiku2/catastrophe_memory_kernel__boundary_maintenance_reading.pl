% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Mourning-Practice Boundary Enforcement (Boundary-Maintenance Reading)
 *   domain: religious/social/collective_memory
 *
 * SUMMARY:
 *   This constraint is the BOUNDARY-MAINTENANCE READING of the
 *   catastrophe-memory kernel — one of four coexisting readings of how ritual
 *   enforces collective memory following catastrophic loss. This reading
 *   focuses on the boundary-enforcement function: mourning practices
 *   standardize grief-expression, police group membership, exclude out-group
 *   commemorators, and lock individuals into identity-fused participation.
 *   The kernel itself (the standing arrangement of ritual mourning practice)
 *   is NOT contested; what IS contested is what function it primarily serves
 *   and what consequences that function carries. This reading emphasizes
 *   extraction and conformity cost; sibling readings emphasize survival
 *   competence, symbolic continuity, and trauma-encoding. The constraint is
 *   CLAIMED as tangled_rope because it unambiguously coordinates in-group
 *   identity (coordination function) WHILE ALSO extracting individual
 *   autonomy and excluding out-group participants (asymmetric extraction).
 *   This claim is independent of the metrics — the engine measures whether
 *   the structural data supports the claim.
 *
 * KEY AGENTS:
 *   - ritual_authorities: maintain the canonical practice and enforce conformity through gatekeeping and delegitimization
 *   - ritual_community_members: participate, transmit, but bear conformity costs and exit costs (identity-locked)
 *   - individual_grief_expressors: encounter suppression when their grief style diverges from the canonical form
 *   - out_group_members: excluded from commemorative participation by boundary rules
 *   - intergenerational_transmitters: perpetuate the practice but enforce conformity on the next generation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.62).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.71).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Mourning-Practice Boundary Enforcement (Boundary-Maintenance Reading)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious/social/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'b84ad6e3-e1fb-4d21-9a66-6b76436b2c63').
narrative_ontology:cs_kernel_codification('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', distributed).
narrative_ontology:cs_authority_grounding('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', lineage).
narrative_ontology:cs_interpretation_layer_present('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63').
narrative_ontology:cs_reading_relation('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', foundational, boundary_identity_requires_conformity).
narrative_ontology:cs_axiom_status(boundary_identity_requires_conformity, holdable).
narrative_ontology:cs_axiom_grounding('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', boundary_identity_requires_conformity, deontological).
narrative_ontology:cs_axiom('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', foundational, out_group_exclusion_necessary_for_in_group_coherence).
narrative_ontology:cs_axiom_status(out_group_exclusion_necessary_for_in_group_coherence, holdable).
narrative_ontology:cs_axiom_grounding('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', out_group_exclusion_necessary_for_in_group_coherence, deontological).
narrative_ontology:cs_reference_frame('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', collective_mourning_through_canonical_ritual_authority).
narrative_ontology:cs_drift_state('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', contemporary_diaspora_pluralism_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b84ad6e3-e1fb-4d21-9a66-6b76436b2c63', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, intergenerational_transmitters).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_community_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_grief_expressors).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, intergenerational_transmitters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in shared mourning practices that mark catastrophe anniversaries, commemoration dates, and collective memory. They gain belonging, intergenerational continuity, and reinforced group identity through participation. They also bear conformity costs: prescribed expressions of grief, temporal commitment to ritual observance, exclusion from participation in rival or different memorial frameworks, and pressure to transmit the practice to children. Exit from the practice means losing the identity bundle it sustains.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_community_members, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_community_members, payer).

% Religious leaders, community elders, or institutional custodians who codify which mourning practices are canonical, define the correct expressions of grief, certify who may lead rituals, and adjudicate departures from the prescribed form. They justify the standardization as spiritual fidelity or historical accuracy. They maintain authority by controlling the ritual form and by selective inclusion/exclusion of community members.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_authorities, agenda_setter,
    institutional, generational, mobile, global).

% Individuals who experience the catastrophe's aftermath or transmission through family trauma and seek to express grief in non-canonical forms (heterodox memorial, secular grieving, personal narrative outside the prescribed script). They encounter institutional pressure to conform to the canonical mourning practice, are told their grief-work is inauthentic or divisive if it deviates, and face social exclusion or delegitimization within the community if they do not participate in the standardized ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, individual_grief_expressors, payer,
    powerless, biographical, constrained, local).

% Persons who share the historical catastrophe (or its aftermath) but do not belong to the in-group's ethnic, religious, or kinship community. They may wish to participate in commemoration or relate to shared trauma but are barred by boundary rules (only this group's members may lead, speak, or ritually center the commemorative narrative). Their exclusion is maintained by ritual authority's control of access and by the in-group's assertion that the practice is identity-specific and non-transferable.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_members, excluded,
    moderate, biographical, trapped, global).

% Parents and educators responsible for teaching children the canonical mourning practices and attaching them to the group identity. They benefit from continuity of the group (their identity is secured through the transmission line) but also bear the cost of enforcing conformity, managing children's resistance or grief styles that don't fit the script, and perpetuating the identity fusion that makes exit costly for the next generation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, intergenerational_transmitters, payer,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, intergenerational_transmitters, beneficiary).

% Academic historians, truth commissions, or external researchers who study the catastrophe and its commemoration. They observe the mourning practice from outside and can measure whether the practice serves boundary maintenance, symbolic continuity, trauma encoding, or survival competence — different readings of the same kernel. They have no direct stake in the ritual's outcome.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, historical_commissions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_authorities).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mourning practice solves the collective problem of maintaining group identity and solidarity across generations following catastrophic loss. A shared, canonical mourning framework prevents grief from fragmenting into individual or competing narratives and binds community members through synchronized affective expression.
% TRANSFER_FUNCTION: The practice moves individual autonomy (the right to grieve in one's own way, to adopt rituals from other traditions or create personal memorial frameworks) from members to the community's ritual authorities. In exchange, members receive bounded inclusion, intergenerational continuity, and protection of group identity. Out-group members lose access to commemorative participation — the practice transfers their commemorative voice to the in-group alone.
% ABSENT_VOICES: Individual griever styles that don't conform to the canonical expression; out-group members who experienced the same catastrophe but are structurally excluded from the ritual space; diaspora members whose memorial practices diverge from the home community's approved form; younger generations who inherit the practice but experience it as inherited trauma rather than chosen solidarity. These voices would argue for memorial pluralism and opt-out pathways, but are kept silent by the boundary rules that define 'authentic' mourning.
% DISAPPEARANCE_RATIONALE: If the canonical mourning practice and its enforcement vanished, the in-group's intergenerational identity structure would weaken; individuals would develop heterodox grief expressions; out-group members would claim commemorative legitimacy; and the ritual authorities would lose the mechanism through which they maintain community cohesion and gatekeep group membership. The constraint's disappearance would trigger rapid organizational realignment of how the community relates to its catastrophe and who is entitled to speak about it.
% FOUNDING_PROBLEM: Following catastrophic loss (genocide, displacement, communal death), the group faced the problem of maintaining collective identity and preventing dissolution into individual grief, diaspora fragmentation, or historical forgetting. A shared mourning practice provided a durable structure for intergenerational transmission of the catastrophe's meaning and kept the group socially bounded despite spatial dispersion or temporal distance from the events.
% FOUNDING_PROBLEM_CORROBORATION: Ritual authorities and community elders attest that canonical mourning practice is necessary to prevent forgetting and maintain group cohesion across generations. Independent historians and anthropologists studying diaspora communities attest that mourning practices DO persist identity across time and do prevent fragmentation — but also document that the same social binding function could be achieved through more pluralistic memorial frameworks. Interviews with second- and third-generation members reveal they experience the practice as both identity-sustaining AND constraining on grief-expression autonomy; the founding problem (preventing dissolution) is still live, but the practice's boundary-enforcement side effects are now disputed.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness sits at 0.62 because the practice genuinely coordinates in-group identity (substantive coordination benefit) but also extracts individual autonomy and excludes out-group participation (substantive extraction). The temporal trajectory shows a rise from 0.48 (early post-catastrophe, when the practice was more voluntary) to 0.62 (contemporary, when the practice has hardened into an identity-constituting requirement). Suppression rises from 0.55 to 0.71 over the same period, indicating that the enforcement machinery (social sanctioning, delegitimization, gatekeeping) has intensified to maintain the practice. Theater rises from 0.12 to 0.28, suggesting that a growing fraction of ritual activity now functions to police boundaries and maintain authority over commemoration rather than to process grief itself. The measurements are taken at regular intervals over 75 years (modeling a three-generation span from early post-catastrophe through contemporary diaspora). The stabilization after year 50 suggests the practice has reached a plateau — it no longer needs to intensify further enforcement because the identity-lock mechanism has fully ossified (members now inherit the practice as identity-constituting rather than choosing it for coordination benefits).
 *
 * PERSPECTIVAL GAP:
 *   Ritual authorities experience this constraint primarily as beneficial coordination (in-group unity, intergenerational transmission, prevention of forgetting) — from their seat, the enforcement is justified and the boundaries are necessary. Individual griever-expressors and out-group members experience it as exclusion and suppression — they perceive the same enforcement machinery as constraining their voice and their belonging. The engine computes seat-divergent classifications from the structural data: from the in-group-cohesion beneficiary seat, the constraint may compute closer to rope; from the individual-autonomy and out-group-exclusion victim seats, it should compute closer to snare. The authored claim (tangled_rope) reflects the fact that both the coordination and the extraction are real and structurally inseparable in this reading — you cannot have in-group boundary maintenance without excluding others and constraining individual expression.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual authorities (institutional power, mobile exit) face low directionality (d near 0.2-0.3) because they benefit from the constraint's operation and can exit it at lower cost (they can reorganize ritual forms without losing institutional identity). In-group community members (organized power, identity-locked exit) sit near d=0.4-0.5 (symmetric) because they gain intergenerational continuity but also bear conformity costs and cannot exit without identity dissolution. Individual grief-expressors (powerless, constrained exit) face high directionality (d near 0.7-0.8) because they bear suppression and delegitimization without commensurate coordination benefit. Out-group members (moderate power, trapped exit) face near-maximum directionality (d near 0.9) because they are excluded from the very benefit the constraint coordinates and cannot exit the exclusion except by leaving the community geography entirely. The metrics (extraction 0.62, suppression 0.71) are authored as the aggregate across all these directional positions; individual seats would experience different effective extraction depending on their power atom and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing identity dissolution following catastrophe) is still LIVE — ritual authorities and community members attest to this, and empirical observation confirms that diaspora communities with canonical mourning practices DO maintain intergenerational continuity better than those without. However, the practice's boundary-enforcement function has become increasingly extractive relative to its coordination function, as shown by the rising theater_ratio and rising suppression_requirement. The core question for mandatrophy resolution is whether the constraint's persistence is justified by the founding problem (coordination-necessary) or whether the founding problem could be solved through less extractive mechanisms (memorial pluralism, opt-in intensity, out-group inclusion with boundary-marking rather than boundary-exclusion). This reading instantiates the claim that mandatrophy is NOT resolved — the constraint continues to extract individual autonomy and exclude out-group voices long after the founding problem (preventing dissolution) has been stabilized. The measurement trajectory (rising extraction and suppression, plateauing by year 50) suggests the constraint is moving from functional necessity toward institutional inertia — it persists because the community has identity-fused with it, not because the founding problem requires its current extractive form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_maintenance_vs_trauma_processing,
    'Is the suppression measured in this constraint (conformity pressure, delegitimization of non-canonical grief) a necessary byproduct of boundary maintenance, or does it exceed what boundary-enforcement structurally requires?',
    'Cross-community comparison: study diaspora communities with canonical mourning practices (high boundary enforcement) and observe whether those with more inclusive, pluralistic memorial frameworks show either equivalent identity-persistence (suggesting suppression is extractive overhead) or identity-dissolution (suggesting suppression is boundary-necessary).',
    'If suppression is shown to be extractive overhead, the constraint should reclassify toward snare. If suppression is shown to be boundary-necessary, the tangled_rope classification is confirmed. If the evidence is ambiguous, a conceptual omega on the boundary-integrity axiom is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_maintenance_vs_trauma_processing, empirical, 'Whether measured suppression is structurally necessary for boundary maintenance or extractive excess.').

omega_variable(
    out_group_inclusion_impossibility,
    'Could the canonical mourning practice include out-group participants (others who experienced the catastrophe) while maintaining in-group boundary identity? Or is boundary identity structurally dependent on exclusion?',
    'Natural experiment from communities that have experimentally widened commemorative participation to out-group members: measure whether in-group identity fractured, persisted, or reconfigured; measure whether the practice''s coordination function remained stable.',
    'If out-group inclusion is compatible with in-group identity and coordination, the exclusion is shown to be extractive rather than boundary-necessary, and the constraint should trend toward snare. If out-group inclusion dissolves in-group identity, the exclusion is boundary-necessary and the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(out_group_inclusion_impossibility, conceptual, 'Whether the constraint''s boundary-enforcement function is structurally dependent on out-group exclusion or whether exclusion is chosen identity-politics.').

omega_variable(
    identity_lock_mechanism_reversibility,
    'Is the identity-lock exit option genuinely irreversible, or are members identity-locked because the constraint''s framing makes exit costly rather than because the identity is structurally inseparable from the practice?',
    'Ethnographic study of members who attempt to exit or renegotiate their participation: measure the post-exit psychological/social trajectory (does identity dissolve, or does it persist in new form?) and the community''s response (is the member cut off, or do relationships persist?).',
    'If identity genuinely dissolves post-exit, the exit option is correctly classified as identity_locked. If identity persists or reconfigures, exit is constrained rather than identity_locked, reducing measured suppression and potentially reclassifying the constraint toward rope. This is a classification-critical ambiguity because identity_lock amplifies the effective extraction by removing the exit option entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_reversibility, empirical, 'Whether the measured suppression reflects genuinely irreversible identity-lock or socially-costly but reversible exit.').

omega_variable(
    reading_incommensurability,
    'Is the boundary-maintenance reading incommensurable with the survival-competence reading, or do they describe complementary (non-foreclosed) functions of the same practice?',
    'Historical and ethnographic evidence from communities that have articulated or lived multiple readings simultaneously: do members describe the practice as BOTH boundary-enforcing AND survival-encoding, or are these experienced as contradictory frames?',
    'If incommensurable (members experience only one reading as legitimate), the reading_relations should be declared as forecloses. If complementary (members hold both readings), the relation is coexists_with. This affects how the corpus treats the kernel''s interpretive space — are the readings in logical conflict or in pragmatic coexistence?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether the boundary-maintenance and survival-competence readings logically foreclose each other or can coexist in a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t12, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(cata_tr_t37, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 37, 0.26).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(cata_tr_t62, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 62, 0.28).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 75, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cata_be_t12, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(cata_be_t37, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 37, 0.62).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(cata_be_t62, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 62, 0.62).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t12, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement(cata_su_t37, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 37, 0.7).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(cata_su_t62, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 62, 0.71).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 75, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__boundary_maintenance_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe-memory kernel. The kernel itself (the standing practice of ritual mourning) is not contested; what IS contested is the primary function the ritual serves. The boundary-maintenance reading frames ritual as enforcing group boundaries through conformity pressure and exclusion. Sibling readings (in separate constraint stories) frame the same practice as encoding survival competence, preserving symbolic continuity, and encoding intergenerational trauma. Each reading instantiates different ε values, different beneficiary/victim structures, and different axioms. All four readings coexist as live interpretive frameworks held by different community constituencies; no single reading logically forecloses the others, though they create resource and legitimacy pressures that influence each other. The kernel family is linked via network.affects_constraints to enable contamination analysis across the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, institutional, 0.25).
constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
