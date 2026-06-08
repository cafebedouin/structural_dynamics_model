% ============================================================================
% CONSTRAINT STORY: boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boundary_maintenance_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: boundary_maintenance_reading
 *   human_readable: Ritual Boundary Maintenance Through Shared Mourning Practice
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates the boundary-maintenance reading of the
 *   catastrophe_memory_kernel — the claim that shared mourning practices
 *   function primarily to enforce and renew group boundaries through enforced
 *   conformity to prescribed grief expressions. The reading is one of four
 *   structurally distinct interpretations of how mourning rituals operate:
 *   boundary-maintenance (this reading), symbol-continuity (sibling),
 *   survival-competence transmission (sibling), and trauma-encoding
 *   (sibling). Each reading produces a different constraint with different
 *   beneficiaries, victims, and extractiveness profiles. The
 *   boundary-maintenance reading identifies in-group cohesion and ritual
 *   authority as beneficiaries, and individual autonomy, out-group relations,
 *   and grief-expression diversity as victims. The constraint exhibits
 *   tangled_rope structure: genuine coordination function (collective grief
 *   processing, intergenerational memory transmission) coupled with
 *   asymmetric extraction (conformity enforcement, identity threat, exclusion
 *   of alternative expressions). The measurement trajectory shows
 *   extractiveness rising from 0.35 to 0.48 over the first 20 time units
 *   (increasing boundary strictness as pluralism threatens group cohesion),
 *   then declining to 0.45 as secular alternatives mature. Suppression
 *   requirement declines from 0.60 to 0.42 as the constraint's enforcement
 *   mechanisms weaken in pluralistic contexts. Theater ratio rises from 0.25
 *   to 0.38, indicating increasing performative content as the
 *   boundary-maintenance function atrophies in secularized institutions.
 *
 * KEY AGENTS:
 *   - Grieving Individual: Primary victim (powerless/identity_locked) — identity fused with group; cannot express grief outside prescribed forms without risking social death
 *   - Peripheral Community Member: Secondary victim (moderate/constrained) — benefits from group solidarity but constrained by conformity expectations; can exit at cost of social standing
 *   - Ritual Authority Holder: Primary beneficiary (institutional/arbitrage) — priest, elder, or keeper of mourning practice; experiences constraint as coordination; has exit options and cultural legitimacy
 *   - In-Group Cohesion: Beneficiary (abstract collective good) — the group's boundary and identity are renewed through enforced mourning participation
 *   - Secular Memorialization Movement: Organized agents (organized/mobile) — building alternative mourning structures that decouple grief processing from boundary enforcement
 *   - Secularized Religious Institution: Institutional actor (institutional/constrained) — maintains mourning rituals through inertia; boundary-maintenance function has atrophied
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable features of human group dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boundary_maintenance_reading, 0.45).
domain_priors:suppression_score(boundary_maintenance_reading, 0.5).
domain_priors:theater_ratio(boundary_maintenance_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boundary_maintenance_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(boundary_maintenance_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(boundary_maintenance_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(boundary_maintenance_reading, "Ritual Boundary Maintenance Through Shared Mourning Practice").
narrative_ontology:topic_domain(boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(boundary_maintenance_reading, '3a9c7f1f-124e-447d-b4f1-19232064ed79').
narrative_ontology:cs_kernel_codification('3a9c7f1f-124e-447d-b4f1-19232064ed79', distributed).
narrative_ontology:cs_authority_grounding('3a9c7f1f-124e-447d-b4f1-19232064ed79', lineage).
narrative_ontology:cs_interpretation_layer_present('3a9c7f1f-124e-447d-b4f1-19232064ed79').
narrative_ontology:cs_reading_relation('3a9c7f1f-124e-447d-b4f1-19232064ed79', boundary_maintenance_reading__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a9c7f1f-124e-447d-b4f1-19232064ed79', boundary_maintenance_reading__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a9c7f1f-124e-447d-b4f1-19232064ed79', boundary_maintenance_reading__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('3a9c7f1f-124e-447d-b4f1-19232064ed79', foundational, boundary_maintenance_primary_function).
narrative_ontology:cs_axiom_status(boundary_maintenance_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('3a9c7f1f-124e-447d-b4f1-19232064ed79', boundary_maintenance_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('3a9c7f1f-124e-447d-b4f1-19232064ed79', foundational, enforced_conformity_necessary_for_cohesion).
narrative_ontology:cs_axiom_status(enforced_conformity_necessary_for_cohesion, holdable).
narrative_ontology:cs_axiom_grounding('3a9c7f1f-124e-447d-b4f1-19232064ed79', enforced_conformity_necessary_for_cohesion, empirically_contingent).
narrative_ontology:cs_reference_frame('3a9c7f1f-124e-447d-b4f1-19232064ed79', mourning_practice_as_boundary_enforcement).
narrative_ontology:cs_drift_state('3a9c7f1f-124e-447d-b4f1-19232064ed79', contemporary_pluralistic_context, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3a9c7f1f-124e-447d-b4f1-19232064ed79', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, in_group_cohesion).
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, ritual_authority_holders).
narrative_ontology:constraint_victim(boundary_maintenance_reading, individual_autonomy).
narrative_ontology:constraint_victim(boundary_maintenance_reading, out_group_relations).
narrative_ontology:constraint_victim(boundary_maintenance_reading, grief_expression_diversity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, peripheral_community_member).
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, ritual_authority_holder).
narrative_ontology:constraint_victim(boundary_maintenance_reading, grieving_individual).
narrative_ontology:constraint_victim(boundary_maintenance_reading, peripheral_community_member).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experiences loss and grief; must express mourning through prescribed ritual forms or face social death. Identity is fused with group membership; cannot imagine themselves outside the group. Conformity is enforced through identity threat rather than material coercion. Bears the cost of conformity and identity subordination.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, grieving_individual, payer,
    powerless, biographical, identity_locked, local).

% Participates in mourning ritual and benefits from group solidarity, collective memory transmission, and access to community support networks. Also constrained by conformity expectations and boundary-policing pressure. Can exit at cost of social standing and community access. Mixed experience of coordination and extraction.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, peripheral_community_member, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(boundary_maintenance_reading, peripheral_community_member, beneficiary).

% Priest, elder, or designated keeper of mourning practice. Administers and enforces the ritual; benefits from authority position and cultural legitimacy. Experiences the constraint as solving the genuine problem of collective grief processing and group identity renewal. Has exit options (can reinterpret or modify practice) and experiences low extraction.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, ritual_authority_holder, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(boundary_maintenance_reading, ritual_authority_holder, beneficiary).

% Abstract collective good: the group's boundary and identity are renewed through enforced mourning participation. The constraint's operation directly produces group boundary maintenance. Not an agent but a beneficiary of the constraint's operation.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, in_group_cohesion, beneficiary,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(boundary_maintenance_reading, in_group_cohesion).

% Organized agents (secular grief counselors, interfaith dialogue groups, secular memorial practices) building alternative mourning structures that decouple grief processing from group boundary enforcement. Have mobile exit options and are actively constructing parallel pathways. See the traditional ritual's boundary-maintenance function as transitional and replaceable.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, secular_memorialization_movement, agenda_setter,
    organized, civilizational, mobile, global).

% Religious institution in pluralistic society maintaining mourning rituals largely through institutional inertia and cultural nostalgia. The boundary-maintenance function has atrophied; many members participate for social reasons rather than identity fusion. Constrained by pluralism and declining enforcement capacity. Maintains the ritual because institutional identity is constituted through the practice.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, secularized_religious_institution, agenda_setter,
    institutional, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Processing collective grief and transmitting intergenerational memory in response to catastrophic loss. The mourning practice solves the genuine problem of how groups collectively acknowledge loss and maintain continuity with the deceased and the past.
% TRANSFER_FUNCTION: Conformity and identity subordination flow from individuals to the group and ritual authorities. Individuals transfer their grief expression to prescribed ritual forms; ritual authorities transfer authority and cultural legitimacy to themselves through control of mourning practice. The group transfers boundary maintenance to individuals through enforced participation.
% ABSENT_VOICES: Individuals who have left the group or rejected the mourning practice are absent from the conversation. Their voices would testify to whether exit is possible and what it costs. Secular grief counselors and alternative memorialization practitioners are increasingly present but were historically absent from the conversation about mourning practice.
% DISAPPEARANCE_RATIONALE: If the boundary-maintenance reading of mourning practice disappeared, the group would need to find alternative mechanisms for boundary maintenance and identity renewal. Secular alternatives (therapy, secular ceremonies, individualized remembrance) would become the primary mourning structures. The group's cohesion would depend on mechanisms other than enforced ritual conformity. The constraint's disappearance would require significant institutional reorganization.
% FOUNDING_PROBLEM: How do groups collectively acknowledge catastrophic loss and maintain continuity with the deceased and the past? How do groups renew their boundaries and identity in the face of death? The mourning practice was built to solve these problems through enforced collective ritual participation.
% FOUNDING_PROBLEM_CORROBORATION: Ritual authorities and community elders attest that the founding problem is live — groups still need to process grief and maintain identity. Secular memorialization practitioners attest that the founding problem can be solved through alternative mechanisms that do not require enforced ritual conformity. Grieving individuals report that the founding problem is real but that the prescribed ritual forms do not always serve their grief processing needs. The status is contested because different stakeholders have different answers to whether the traditional mourning practice is still the best solution.
narrative_ontology:disappearance_verdict(boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(boundary_maintenance_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRIEVING INDIVIDUAL (SNARE) — Identity fused with group membership; cannot express grief outside prescribed forms without risking social death. Mourning practice is mandatory; deviation triggers exclusion. The individual's grief is subordinated to group boundary maintenance. Maximum extraction from this position: conformity enforced through identity threat, not material coercion.
constraint_indexing:constraint_classification(boundary_maintenance_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: PERIPHERAL COMMUNITY MEMBER (TANGLED ROPE) — Participates in mourning ritual and benefits from group solidarity and collective memory transmission, but constrained by conformity expectations. Can exit at cost of social standing and access to community support networks. Mixed experience: genuine coordination function (shared grief processing) alongside extraction (conformity pressure, boundary policing).
constraint_indexing:constraint_classification(boundary_maintenance_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RITUAL AUTHORITY HOLDER (ROPE) — Priest, elder, or designated keeper of mourning practice. Experiences the constraint as coordination: the ritual solves the genuine problem of collective grief processing and group identity renewal. Benefits from authority position and cultural legitimacy. Has exit options (can reinterpret or modify practice) and experiences low extraction. The ritual is their primary institutional function.
constraint_indexing:constraint_classification(boundary_maintenance_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SECULAR MEMORIALIZATION MOVEMENT (SCAFFOLD) — Organized agents (secular grief counselors, interfaith dialogue groups, secular memorial practices) see the boundary-maintenance function as transitional. Alternative mourning structures (therapy, secular ceremonies, individualized remembrance) are building parallel pathways that decouple grief processing from group boundary enforcement. The traditional ritual's extraction mechanism weakens as alternatives mature. Sunset logic: as secular memorialization norms spread, the mandatory character of religious mourning practice erodes.
constraint_indexing:constraint_classification(boundary_maintenance_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SECULARIZED RELIGIOUS INSTITUTION (PITON) — In pluralistic societies, religious institutions maintain mourning rituals largely through institutional inertia and cultural nostalgia. The boundary-maintenance function has atrophied (many members participate for social reasons, not identity fusion). The ritual persists as performance — maintained because alternatives haven't fully replaced it and because the institution's identity is constituted through the practice, not because the practice functionally maintains boundaries anymore. Theater ratio reflects this degradation.
constraint_indexing:constraint_classification(boundary_maintenance_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, shared mourning practice is an immutable feature of human collective life: all groups process grief collectively, and all collective grief processing creates boundaries. This perspective sees the constraint as a natural law of group dynamics — boundary maintenance through ritual is inherent to human social organization. However, the structural data reveals this as a false summit: the specific extractive mechanisms (conformity enforcement, identity threat, exclusion of alternative grief expressions) are contingent institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(boundary_maintenance_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boundary_maintenance_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boundary_maintenance_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boundary_maintenance_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(boundary_maintenance_reading, TR),
    TR >= 0.70.

:- end_tests(boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate. The constraint extracts conformity and identity subordination from individuals, but the extraction is coupled with genuine coordination benefits (collective grief processing, intergenerational memory transmission, group identity renewal). The beneficiary (in-group cohesion) is real and valued by participants. The extraction is not pure predation but rather asymmetric distribution of costs and benefits: individuals bear conformity costs; the group and ritual authorities benefit from boundary maintenance. The measurement trajectory shows extractiveness rising to 0.48 as pluralism threatens group cohesion (stricter boundary enforcement), then declining as secular alternatives mature (reduced enforcement capacity). Suppression (0.50): Moderate. The primary suppression mechanism is identity threat rather than material coercion — individuals cannot exit without experiencing identity dissolution. This is internalized suppression, not structural. Secondary suppression includes social cost (loss of community support, status reduction) and informational isolation (limited exposure to alternative grief expressions). The measurement trajectory shows suppression declining from 0.60 to 0.42 as pluralistic contexts reduce the effectiveness of identity threat and as secular alternatives become visible. Theater ratio (0.35): Low-moderate. The mourning practice has genuine functional content (grief processing, memory transmission) and is not primarily performative. However, the theater ratio rises from 0.25 to 0.38 as the constraint's enforcement mechanisms weaken in secularized institutions — the ritual persists increasingly through institutional inertia rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across power and exit dimensions. The grieving individual (powerless/identity_locked) experiences pure extraction (snare) — their identity is constituted through the group and they cannot imagine exit. The peripheral member (moderate/constrained) experiences mixed coordination and extraction (tangled_rope) — they benefit from group solidarity but are constrained by conformity expectations. The ritual authority (institutional/arbitrage) experiences pure coordination (rope) — the ritual solves their institutional function and they have exit options. The secular movement (organized/mobile) experiences a temporary problem with a sunset (scaffold) — alternative mourning structures are building pathways that decouple grief from boundary enforcement. The secularized institution (institutional/constrained) experiences a degraded ritual (piton) — the boundary-maintenance function has atrophied but the ritual persists through institutional identity. The analytical observer risks seeing an immutable natural law (mountain) — boundary maintenance through ritual is inherent to human groups — but the structural data reveals this as a false summit: the specific extractive mechanisms are contingent institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the extraction flow. Grieving individuals (powerless/identity_locked) occupy the full-target position (d ≈ 0.95): they bear conformity costs and identity threat with minimal exit options. Peripheral members (moderate/constrained) occupy a moderate-target position (d ≈ 0.65): they bear conformity costs but also benefit from group solidarity and have constrained exit options. Ritual authorities (institutional/arbitrage) occupy a beneficiary position (d ≈ 0.15): they benefit from authority and cultural legitimacy and have arbitrage-grade exit options. In-group cohesion (abstract beneficiary) occupies a pure-beneficiary position (d ≈ 0.05): the constraint's operation directly produces group boundary renewal. The secular movement (organized/mobile) occupies a low-target position (d ≈ 0.35): they are constrained by the existing constraint but have mobile exit options and are actively building alternatives. The secularized institution (institutional/constrained) occupies a moderate-beneficiary position (d ≈ 0.25): they benefit from institutional identity but are constrained by pluralism and declining enforcement capacity. The analytical observer (analytical/analytical) occupies a neutral position (d ≈ 0.50): they observe the constraint from outside its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The boundary-maintenance reading does not resolve mandatrophy but rather instantiates it. The constraint's mandate is to maintain group boundaries through enforced mourning conformity. The function is to process collective grief and transmit intergenerational memory. These are distinct: boundary maintenance is a side effect of grief processing, not its primary purpose. The measurement trajectory shows the mandate (boundary enforcement) persisting even as the function (grief processing) is increasingly served by secular alternatives. The theater ratio rising from 0.25 to 0.38 indicates that the ritual is increasingly maintained for boundary purposes (performative) rather than grief-processing purposes (functional). This is classic mandatrophy: the original function (grief processing) is being displaced by alternative mechanisms, but the constraint persists because it serves a secondary function (boundary maintenance) that the institution has become dependent on. The constraint will resolve mandatrophy when either (a) the boundary-maintenance function is no longer needed (group cohesion is maintained through other mechanisms), or (b) the constraint is explicitly reframed as a boundary-maintenance mechanism rather than a grief-processing mechanism, and the institution accepts that this function can be served by alternative structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_function_necessity,
    'Is boundary maintenance through mourning practice a necessary function of collective grief processing, or a contingent institutional choice?',
    'Comparative analysis of grief processing in groups with and without boundary-enforcing rituals; examination of whether grief processing efficacy correlates with boundary strictness; cross-cultural comparison of mourning practices with varying boundary functions.',
    'If necessary: the constraint is closer to mountain (natural law of group dynamics). If contingent: the constraint is clearly tangled_rope (coordination function + extraction mechanism). This determines whether the boundary-maintenance reading is a genuine structural feature or a naturalization of institutional choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_function_necessity, empirical, 'Whether boundary maintenance is necessary to grief processing or contingent institutional choice').

omega_variable(
    identity_lock_mechanism,
    'Is the individual''s conformity to mourning practice driven by identity fusion (they cannot imagine themselves outside the group) or by material/social costs (they can imagine exit but face high penalties)?',
    'Ethnographic analysis of individuals who have left the group: do they report identity dissolution or material loss? Comparison of exit narratives across groups with varying boundary strictness. Analysis of whether individuals who leave report identity reconstruction or identity persistence.',
    'If identity fusion: the constraint''s suppression is internalized and persists after exit (higher effective suppression). If material costs: suppression is structural and decays after exit. This affects the piton classification — a piton with internalized suppression is more stable than one with structural suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether conformity is driven by identity fusion or material/social costs').

omega_variable(
    reading_kernel_ambiguity,
    'Is the catastrophe_memory_kernel best read as boundary-maintenance (this reading), symbol-continuity (sibling), survival-competence (sibling), or trauma-encoding (sibling)? Do these readings coexist or foreclose each other?',
    'Historical analysis of how mourning practices have been justified and defended within the tradition: do authorities appeal to boundary maintenance, symbolic continuity, survival skills transmission, or trauma processing? Do these justifications coexist or compete? Analysis of whether a single mourning practice can serve all four functions simultaneously or whether they require different ritual structures.',
    'If readings coexist: the catastrophe_memory_kernel is genuinely multi-functional and all four readings are live. If readings foreclose: the kernel is contested and only one reading can be held within a single framework. This determines the reading_relations structure in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether boundary-maintenance reading coexists with or forecloses sibling readings of the catastrophe_memory_kernel').

omega_variable(
    secular_alternative_sufficiency,
    'Do secular memorialization practices (therapy, secular ceremonies, individualized remembrance) actually provide equivalent grief processing and collective memory transmission, or do they fail to serve functions that religious mourning rituals serve?',
    'Longitudinal comparison of grief outcomes and collective memory retention in groups using secular vs. religious mourning practices. Analysis of whether secular alternatives maintain group cohesion and intergenerational memory transmission at equivalent rates. Examination of whether individuals who switch to secular practices report equivalent sense of collective participation.',
    'If sufficient: the scaffold perspective is confirmed — secular alternatives can replace religious mourning and the boundary-maintenance function can be decoupled from grief processing. If insufficient: the scaffold is aspirational and the boundary-maintenance function remains structurally necessary, making the constraint more stable (less likely to sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_alternative_sufficiency, empirical, 'Whether secular memorialization practices provide equivalent grief processing and memory transmission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boundary_maintenance_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bound_tr_t0, boundary_maintenance_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bound_tr_t10, boundary_maintenance_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(bound_tr_t20, boundary_maintenance_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(bound_tr_t30, boundary_maintenance_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(bound_be_t0, boundary_maintenance_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bound_be_t10, boundary_maintenance_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(bound_be_t20, boundary_maintenance_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(bound_be_t30, boundary_maintenance_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bound_su_t0, boundary_maintenance_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(bound_su_t10, boundary_maintenance_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(bound_su_t20, boundary_maintenance_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(bound_su_t30, boundary_maintenance_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boundary_maintenance_reading, attachment_coordination).
narrative_ontology:affects_constraint(boundary_maintenance_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(boundary_maintenance_reading, survival_competence_reading).
narrative_ontology:affects_constraint(boundary_maintenance_reading, trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% The boundary-maintenance reading is one of four structurally distinct readings of the catastrophe_memory_kernel. Each reading produces a different constraint with different ε values and beneficiary/victim structures. The readings are linked through the kernel: they are alternative interpretations of the same mourning practice, not separate constraints. However, they have different extractiveness profiles and different classification types. The boundary-maintenance reading (this constraint) has moderate extractiveness (0.45) and tangled_rope structure. The sibling readings are expected to have different ε values reflecting their different functional claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
