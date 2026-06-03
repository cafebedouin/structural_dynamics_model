% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Catastrophe Memory as Mourning Practice and Boundary Maintenance (D1/D4 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint models Tisha B'Av and analogous catastrophe
 *   commemorations through the MOURNING-PRACTICE READING of a contested
 *   kernel about how ritual preserves memory after catastrophe. The kernel
 *   (catastrophe_memory_function) admits three structurally distinct
 *   readings: (1) mourning-practice reading (D1/D4 only — ritual maintains
 *   group boundary and collective grief); (2) survival-competence reading (D5
 *   only — ritual transmits adaptive institutional knowledge); (3)
 *   hybrid-transformation reading (D1/D4+D5 — ritual both mourns and trains).
 *   This JSON instantiates reading 1 only. The mourning-practice reading
 *   frames Tisha B'Av as fundamentally about preserving the group's identity
 *   and boundary through collective commemoration of loss. The ritual is
 *   D1/D4: it marks the community's continued existence despite catastrophe
 *   (D1), and it maintains the boundary between those who observe and those
 *   outside the mourning tradition (D4). Notably absent from this reading's
 *   functional scope is D5 (survival-competence transmission) — this reading
 *   does not treat the ritual as encoding adaptive mechanisms, institutional
 *   resilience structures, or knowledge for institutional continuity. The
 *   reading claims that the ritual's PRIMARY function is boundary-maintenance
 *   through enforced collective grief, which creates an extraction mechanism:
 *   individual grief expression is subordinated to collective ritual form;
 *   alternative interpretations of catastrophe are suppressed; participation
 *   becomes obligatory for community membership.
 *
 * KEY AGENTS:
 *   - Mourning Individual: Primary victim (powerless/identity_locked) — bears memorial obligation; identity fused with collective grief performance; cannot exit without severing group belonging
 *   - Transmitted Community: Secondary actor (moderate/constrained) — experiences ritual as both coordination (strengthens group identity) and extraction (mandatory forms override individual grief processing)
 *   - Religious Authority Structure: Primary beneficiary (institutional/arbitrage) — net beneficiary of constraint; legitimacy derives from guardian role over collective memory and ritual enforcement
 *   - Ritual Form Itself: Institutional persistence mechanism (institutional/constrained) — performative structure maintained by inertia; the form persists even when subjective states it indexes (grief, repentance) have attenuated
 *   - Secular/Revisionist Memory Movements: Organized challenger (organized/mobile) — seeks to preserve catastrophe memory while decoupling from religious authority enforcement; shows extraction mechanism is separable from coordination function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (authority control, suppression of interpretations) as inherent to how communities process loss
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Catastrophe Memory as Mourning Practice and Boundary Maintenance (D1/D4 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'acaeb533-ee51-4aa9-b0cd-4a0d65df5a46').
narrative_ontology:cs_kernel_codification('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', fixed_text).
narrative_ontology:cs_authority_grounding('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', lineage).
narrative_ontology:cs_interpretation_layer_present('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46').
narrative_ontology:cs_reading_relation('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', foundational, collective_grief_obligation_constitutes_identity).
narrative_ontology:cs_axiom_status(collective_grief_obligation_constitutes_identity, holdable).
narrative_ontology:cs_axiom_grounding('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', collective_grief_obligation_constitutes_identity, deontological).
narrative_ontology:cs_axiom('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', secondary, prescribed_forms_channel_authentic_grief).
narrative_ontology:cs_axiom_status(prescribed_forms_channel_authentic_grief, overridden).
narrative_ontology:cs_axiom_grounding('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', prescribed_forms_channel_authentic_grief, empirically_contingent).
narrative_ontology:cs_reference_frame('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', sacred_commemoration_lineage).
narrative_ontology:cs_drift_state('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', contemporary_pluralist_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('acaeb533-ee51-4aa9-b0cd-4a0d65df5a46', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, community_identity_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, commemorative_authority).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, individual_grief_expression).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, adaptive_capacity_transmission).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOURNING INDIVIDUAL (SNARE) — Identity fused with community grief obligation. Cannot exit the commemorative cycle without severing group belonging. Individual emotional processing is subordinated to collective ritual script. Maximum extraction: the person bears the memorial duty; the community extracts conformity to prescribed mourning forms.
constraint_indexing:constraint_classification(catastrophe_memory_function__mourning_practice_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: TRANSMITTED COMMUNITY (TANGLED ROPE) — Experiences ritual as genuine coordination of collective memory (shared mourning strengthens group boundary), but also experiences extraction through mandatory participation, prescribed emotional forms, and suppression of alternative grief expressions. The ritual coordinates but constrains.
constraint_indexing:constraint_classification(catastrophe_memory_function__mourning_practice_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS AUTHORITY STRUCTURE (ROPE) — Net beneficiary of the constraint. The ritual preserves and legitimizes the authority's role as guardian of collective memory and boundary maintenance. Benefits from enforcing commemorative practice; experiences the constraint as pure coordination: ritual transmits the tradition itself, which the authority administers.
constraint_indexing:constraint_classification(catastrophe_memory_function__mourning_practice_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE RITUAL FORM (PITON) — The specified mourning practices (fasting, specific prayers, restricted activities on Tisha B'Av) have become largely performative. Many participants observe the form without the subjective states the form originally indexed (grief, repentance, solidarity). Theater ratio high (0.68) because the ritual persists through institutional requirement, not through functional grief transmission. The form is maintained by inertia and legitimacy inheritance, not because it optimally processes loss.
constraint_indexing:constraint_classification(catastrophe_memory_function__mourning_practice_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, collective memory requires ritual forms; mourning is inherent to human groups; boundary maintenance through commemoration is immutable. This perspective naturalizes the constraint as fundamental to how communities preserve identity across catastrophe. The engine will detect this as a false summit: the 'inherent' framing masks the contingent institutional arrangements (who controls the ritual, which grief expressions are permitted, which community interpretations are canonical).
constraint_indexing:constraint_classification(catastrophe_memory_function__mourning_practice_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SECULAR/REVISIONIST MEMORY MOVEMENTS (TANGLED ROPE) — Organized groups seeking to preserve catastrophe memory while uncoupling it from religious authority. They benefit from the constraint's coordination function (collective memory strengthens) but reject the extraction mechanism (mandatory religious forms, authority control, suppression of alternative interpretations). This perspective shows the constraint can coordinate WITHOUT the religious enforcement — the beneficiary and extraction mechanisms are separable.
constraint_indexing:constraint_classification(catastrophe_memory_function__mourning_practice_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_function__mourning_practice_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_function__mourning_practice_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The mourning-practice reading claims the ritual extracts conformity (mandatory participation, prescribed grief forms override authentic expression, alternative interpretations suppressed). But the extraction is not total — community members derive genuine identity and connection benefits, the ritual does coordinate collective grief, and there are partial exit narratives (some observants reinterpret the forms, some participate minimally). The ε estimate reflects that extraction is real but embedded in a functioning coordination mechanism. Suppression (0.62): Moderate-high. Multiple barriers prevent exit: social ostracism for non-participation, identity fusion (mourning identity is core self), spiritual condemnation, exclusion from community decision-making and marriage prospects. Individual grief expressions that diverge from prescribed forms are actively discouraged. Alternative catastrophe interpretations (secular, historical, psychological) are suppressed within the tradition's canonical frame. However, suppression is not absolute — secular commemorations exist, revisionist interpretations are published, some individuals successfully negotiate alternative observance. Theater ratio (0.68): High and increasing over the interval (0.42→0.68). Prescribed forms (fasting, garment tearing, specific prayers) are increasingly observed as cultural performance rather than as indexing the subjective states they originally marked. Many participants report going through motions; emotional authenticity is not the measure; conformity to form is the requirement. The increase reflects progressive disconnection between ritual form and the grief states it was designed to channel — more performance, less catharsis.
 *
 * PERSPECTIVAL GAP:
 *   The mourning-practice reading generates a dramatic perspectival gap across power levels. The religious authority sees pure coordination (Rope) — the ritual transmits the tradition. The mourning individual sees pure extraction (Snare) — the ritual enforces identity fusion and suppresses authentic grief. The moderate community sees both (Tangled Rope) — boundary strengthening and conformity extraction coexist. The ritual form itself is performative (Piton) — it persists through institutional inertia, not functional grief processing. The secular movement sees separable coordination from enforcement (Tangled Rope with lower extraction) — memory can be preserved without religious authority control. The analytical observer risks seeing natural law (Mountain) — ritual mourning is inherent to human groups — which the structural data reveals as false summit (naturalized institutional arrangements mask contingent authority control and suppression mechanisms).
 *
 * DIRECTIONALITY LOGIC:
 *   The powerless/identity_locked agent (mourning individual) experiences maximum extraction: the ritual's suppression mechanisms operate primarily on those with fewest exit options. Their d-value is high (approaching 1.0), producing high f(d) and high experienced extractiveness. The institutional beneficiary (religious authority) has low d (approximately 0.10), experiencing the constraint as pure coordination with negative effective extraction (they are subsidized by the system). The moderate/constrained agent (transmitted community) has intermediate d (~0.55), experiencing both coordination benefits and extraction costs. The secular movement with mobile exit options has lower d than the identity-locked individual despite being organized, because they retain arbitrage capacity (can leave without identity death). The ritual form itself as an institutional actor has constrained rather than arbitrage exit — it cannot abandon the forms without losing authority legitimacy — producing d closer to 0.45.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by rejecting the survival-competence claim entirely. The reading asserts that Tisha B'Av's PRIMARY function is D1/D4 (boundary maintenance through commemoration), not D5 (adaptive capacity transmission). By foreclosing or coexisting-with rather than integrating the D5 reading, the mourning-practice reading avoids the temptation to justify extraction as necessary for resilience-transmission — the extraction is JUST FOR boundary maintenance. This makes the ethical stakes clearer: if you accept the mourning-practice reading, you cannot defend the ritual's suppression mechanisms by appealing to adaptive necessity. The suppression is either justified by identity/boundary value alone (deontological), or it is unjustified extraction. The reading's deontological axiom (group identity constituted through shared mourning) becomes transparent as a normative claim, not a functional necessity. The hybrid-transformation reading, by contrast, can defend suppression as necessary for adaptive transmission (empirically contingent axiom). The sibling readings have different ethical architectures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_survival_competence,
    'Does the mourning practice reading foreclose the survival competence reading (Tisha B''Av as D1/D4 only vs D1/D4+D5)?',
    'Historical and ethnographic analysis: do commemorative practices in practice transmit adaptive institutional knowledge (decentralized governance structures, economic resilience mechanisms, conflict resolution traditions)? Or is adaptive knowledge transmission incidental, with the ritual designed and enforced primarily for boundary maintenance?',
    'If mourning practice forecloses survival competence: this reading stands alone; the hybrid reading coexists but the pure-D5 reading is incompatible. If mourning practice coexists with D5 transmission: the readings overlap and all three are live. If mourning practice ignores D5 entirely: the survival competence reading is present but unthematized in this reading''s framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_survival_competence, empirical, 'Whether mourning practice reading logically forecloses survival competence reading or coexists with it').

omega_variable(
    boundary_maintenance_vs_identity_constitution,
    'Is ritual boundary maintenance a functional outcome (the ritual coordinates group identity as a separate effect) or a PERFORMANCE of identity (the ritual IS identity-making, with no separable coordination function)?',
    'Ethnographic analysis of individuals who practice the ritual: do they report feeling more connected to the group (functional boundary maintenance) or do they report BECOMING members through ritual participation (constitutive identity-making)? Do secular commemorations without religious form achieve boundary maintenance? Are boundary effects observable without the ritual?',
    'If functional: the constraint is coordination-dominant (lower extraction estimate). If constitutive: the constraint is extraction-dominant (higher extraction estimate, closer to snare). If both: tangled rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_maintenance_vs_identity_constitution, empirical, 'Whether boundary maintenance is functional outcome or constitutive identity-making').

omega_variable(
    emotional_suppression_mechanism,
    'Are prescribed mourning forms (prescribed wailing, fasting, garment tearing) channels for grief expression (liberation function) or suppression mechanisms that override authentic emotional responses (constraint function)?',
    'Psychological ethnography: do participants report the forms as enabling grief or constraining it? Do post-ritual interviews show emotional catharsis or frustration? Do individuals who reject the forms report different grief trajectories? Historical analysis: have the forms'' functions changed (originally liberatory, now constraining)?',
    'If liberatory: suppression estimate (0.62) is too high; constraint is lower-extraction rope. If suppressing: suppression estimate is accurate; tangled rope or snare confirmed. If temporally variable: the constraint''s function has shifted over the interval; measure drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_suppression_mechanism, empirical, 'Whether prescribed mourning forms channel or suppress emotional expression').

omega_variable(
    mandatory_participation_coercion,
    'How binding is the commemorative obligation? Can community members opt out of ritual participation without identity loss?',
    'Structural analysis: what are the actual consequences of non-participation? Social ostracism, exclusion from community decision-making, loss of marriage prospects, spiritual condemnation? Are exit costs material (structural barriers) or internalized (identity fusion)? Are there exit narratives within the tradition itself (alternatives recognized as legitimate)?',
    'If true exit: exit_options should be ''constrained'' or ''mobile'' rather than ''identity_locked''; extraction estimate lowers. If high identity fusion: ''identity_locked'' is accurate; extraction remains high. If socially variable (some groups enforce, others permit exit): decompose into separate stories for rigid vs flexible communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatory_participation_coercion, empirical, 'Whether commemorative obligation is structurally binding or identity-locked').

omega_variable(
    sibling_reading_mutual_exclusivity,
    'Are the three readings (mourning_practice, survival_competence, hybrid_transformation) mutually exclusive or can they coexist as different framings of the same ritual?',
    'Conceptual analysis: does the mourning-practice reading (D1/D4 only) logically foreclose the survival-competence reading (D5 transmission)? Or is D5 transmission orthogonal to D1/D4, such that a ritual can do both simultaneously without contradiction? Can a single community hold all three readings at different interpretive levels?',
    'If mutually exclusive: mourning-practice foreclose(s) or is_foreclosed_by the other readings; declare ''forecloses'' or ''coexists_with'' accordingly. If orthogonal: all three coexist; all three readings are live positions. Affects constraint decomposition strategy: one reading (this one) vs constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_mutual_exclusivity, conceptual, 'Logical relationship between mourning-practice, survival-competence, and hybrid readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmf_mourn_theater_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cmf_mourn_theater_t50, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(cmf_mourn_theater_t100, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(cmf_mourn_extract_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cmf_mourn_extract_t50, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(cmf_mourn_extract_t100, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cmf_mourn_suppress_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cmf_mourn_suppress_t50, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 50, 0.54).
narrative_ontology:measurement(cmf_mourn_suppress_t100, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function kernel admits three structurally distinct readings with different ε values and different primary functions. This story instantiates the mourning-practice reading (D1/D4, ε=0.38, Tangled Rope from the community perspective). The survival-competence reading (D5, ε≈0.25, likely Rope or Scaffold from most perspectives) and hybrid-transformation reading (D1/D4+D5, ε≈0.55, likely Tangled Rope from all perspectives) are separate constraint stories linked by network.affects_constraints. The three readings are NOT the same constraint viewed from different angles — they are different functional claims about what the ritual does, and each claim has its own empirical signature. The ε-invariance principle requires separate files because the measurement basis changes: mourning-practice reading measures extraction relative to boundary-maintenance functionality; survival-competence reading measures extraction relative to adaptive knowledge transmission; hybrid reading measures both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
