% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status: Messianic Deferral Reading
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   The halakhic commandment to study sacrifice laws (kodashim) instantiates
 *   a structurally complex constraint in post-70 CE Jewish law. The Temple's
 *   destruction removed the material conditions for executing most
 *   sacrificial commandments, yet the halakhic tradition did not formally
 *   supersede them. Instead, the tradition developed a doctrine of deferral:
 *   commandments tied to Temple/altar are suspended-but-not-obsolete,
 *   maintained in study and memory, awaiting restoration in the messianic
 *   future. This story generates ONE reading of the contested kernel — the
 *   messianic deferral reading — which holds that the commandment's present
 *   status is temporary suspension justified by genuine future contingency
 *   (messianic restoration). This reading creates a tangled-rope structure:
 *   the study obligation provides genuine coordination function (preserving
 *   knowledge, maintaining tradition, enabling community cohesion) while
 *   extracting significant opportunity cost from present-generation
 *   practitioners who bear the study burden but cannot execute the
 *   commandment. The extractiveness has accumulated over 1000+ years as the
 *   deferral has extended indefinitely and the theater ratio has risen
 *   (detailed exegesis of increasingly arcane procedures for sacrifices that
 *   will never be executed). The constraint exhibits all six DR types from
 *   different perspectives, creating a diagnostic exemplar for how contested
 *   kernels generate perspectival variation in classification.
 *
 * KEY AGENTS:
 *   - Present Generation (individuals bound by study obligation): Powerless/identity-locked (cannot exit without identity rupture) — bears full extractive cost of study time without present fulfillment
 *   - Committed Halakhic Community (moderate/constrained): Mixed coordination and extraction — benefits from shared study but costs from opportunity forgone
 *   - Halakhic Authority Structure (institutional/arbitrage): Benefits from maintaining deferral doctrine and interpretive prerogative — perceives constraint as pure coordination
 *   - Textual Apparatus (institutional/arbitrage, piton perspective): Maintains elaborate procedures through institutional inertia; theater high; function deferred indefinitely
 *   - Messianic Contingency Agents (organized/constrained, scaffold perspective): See deferral as genuinely temporary with sunset at restoration
 *   - Analytical Observer (analytical/analytical): Risks naturalizing deferral as immutable logical law; oracle gap visible in how framework privileges certain readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.38).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.52).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.38).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status: Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious_studies/halakhic_theory/commitment_system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '3623d21e-03b5-4ebd-92a7-351c3a1ecbb7').
narrative_ontology:cs_kernel_codification('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', fixed_text).
narrative_ontology:cs_authority_grounding('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', lineage).
narrative_ontology:cs_interpretation_layer_present('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7').
narrative_ontology:cs_reading_relation('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_axiom('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', foundational, deferral_contingency_is_genuine).
narrative_ontology:cs_axiom_status(deferral_contingency_is_genuine, holdable).
narrative_ontology:cs_axiom_grounding('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', deferral_contingency_is_genuine, theological).
narrative_ontology:cs_axiom('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', foundational, study_obligation_remains_binding).
narrative_ontology:cs_axiom_status(study_obligation_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', study_obligation_remains_binding, deontological).
narrative_ontology:cs_reference_frame('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', suspended_commandment_with_contingent_restoration).
narrative_ontology:cs_drift_state('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', post_1000_years_extended_deferral, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3623d21e-03b5-4ebd-92a7-351c3a1ecbb7', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_preparation_apparatus).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, halakhic_authority_structure).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_ritual_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, immediate_commandment_fulfillment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESENT GENERATION (SNARE) — Individuals bound by the commandment to study sacrifice laws cannot fulfill the commandment through present action (no altar, no Temple). They are trapped in a study obligation justified by a future contingency they will not live to see. Exit would require abandoning Jewish identity constituted through halakhic obligation. The extraction is severe: time and cognitive resources devoted to mastering laws they cannot implement, with deferred benefit structure (messianic world-to-come) providing no present compensation.
constraint_indexing:constraint_classification(kodashim_commandment_status__messianic_deferral, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMITTED HALAKHIC COMMUNITY (TANGLED ROPE) — Community members benefit from the coordination function (shared study, transmitted knowledge, collective memory of sacrificial law) while bearing the opportunity cost of study time not devoted to present-day needs. Some exit is possible (leaving the halakhic tradition) but costs are high (community exclusion, identity rupture, relational bonds). The reading provides genuine coordination (maintaining textual tradition, enabling debate) alongside asymmetric extraction (time commitment justified by messianic contingency, not present function).
constraint_indexing:constraint_classification(kodashim_commandment_status__messianic_deferral, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HALAKHIC AUTHORITY STRUCTURE (ROPE) — The institutional apparatus benefits from maintaining the commandment's status as suspended-but-not-obsolete. This reading preserves the authority's interpretive prerogative: authority decides which laws remain 'in abeyance' pending restoration, controls the deferral narrative, and maintains institutional continuity across pre- and post-Temple eras. Authority experiences the constraint as pure coordination: organizing knowledge, sustaining transmission, enabling future restoration. No meaningful exit cost for authority — the institutional apparatus reproduces itself through the study obligation.
constraint_indexing:constraint_classification(kodashim_commandment_status__messianic_deferral, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TEXTUAL APPARATUS (PITON) — From the civilization-scale view, the detailed study of sacrifice laws in a Temple-absent world is substantially performative. The apparatus (Mishna, Talmud, commentarial tradition) maintains elaborate procedures and qualifications for sacrifices that cannot be executed. The theater persists through institutional inertia: the texts were created under functional requirements (pre-70 CE); the apparatus continues because the tradition does not formally supersede them, not because they serve present function. Theater ratio high: extensive detailed exegesis of laws whose implementation is perpetually deferred.
constraint_indexing:constraint_classification(kodashim_commandment_status__messianic_deferral, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MESSIANIC CONTINGENCY (SCAFFOLD) — The deferral structure itself is temporary: the reading holds that restoration is a future historical event (messianic redemption) at which point the commandment becomes fully operative again. The present constraint (suspended but not obsolete) has a built-in sunset: when the Temple is restored, the study obligation transforms into operational fulfillment. Low effective extraction because the deferral is explicitly temporary, not permanent. Organized agents (messianic movements, restoration-focused communities) see the present as preparatory and the constraint as a coordinate toward future implementation.
constraint_indexing:constraint_classification(kodashim_commandment_status__messianic_deferral, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational view, the deferral reading instantiates a structurally irreducible feature of post-70-CE Jewish law: commandments tied to Temple/altar cannot be fulfilled in Temple-absent conditions. This appears as a natural law of halakhic logic — the impossibility of violating a law you structurally cannot execute. However, this classification is perspectival and subject to false-summit detection: the 'irreducible' deferral logic masks the constructed nature of the messianic contingency framing.
constraint_indexing:constraint_classification(kodashim_commandment_status__messianic_deferral, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kodashim_commandment_status__messianic_deferral, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kodashim_commandment_status__messianic_deferral, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, TR),
    TR >= 0.70.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, rising over 1000-year interval from 0.22 to 0.42. The constraint begins with moderate extractiveness because the messianic deferral reading frames study as preparation for genuine future fulfillment. But as centuries pass without restoration and the study obligation persists, the extractiveness increases — the contingency becomes increasingly distant, and the opportunity cost to present practitioners becomes increasingly uncompensated. The trajectory shows accumulating extraction as deferral-indefinitely replaces genuine contingency. Suppression (0.52): Moderate-high. Significant barriers include: (1) identity-locked binding of study obligation to Jewish identity (exit requires identity rupture); (2) institutional enforcement (halakhic authority maintains the obligation); (3) epistemological barriers (no way to verify the messianic contingency or timeline, making exit decision difficult); (4) relational costs (community exclusion). Suppression increases over time as authority structure becomes more invested in deferral maintenance. Theater ratio (0.65): Moderate-high and rising. The study of sacrifice laws in a Temple-absent world is substantially performative — elaborate exegesis of procedures that cannot be executed. Theater increases over time as the gap between study and implementation widens, and as commentary becomes increasingly abstract and removed from lived practice. Claimed type (Tangled Rope): The constraint coordinates genuine function (tradition preservation, knowledge transmission, community bonding) while extracting significant cost (opportunity forgone, study burden, deferred fulfillment). Both coordination and extraction are substantial and real.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence among institutional and individual positions. The authority sees rope (coordination of tradition). The present generation sees snare (trapped in study without present fulfillment). The community sees tangled rope (mixed coordination and extraction). The messianic contingency agents see scaffold (temporary deferral). The textual apparatus shows piton (performative maintenance). The analytical observer risks mountain (naturalizing deferral as immutable logic). The gaps reveal that the 'same constraint' is experienced as fundamentally different structural phenomena depending on whether you are the beneficiary maintaining the deferral doctrine (rope) or the individual bearing the study burden with no present fulfillment (snare). The identity_locked exit option for the present generation is crucial: individuals are structurally mobile (could physically leave, could adopt alternative readings) but cannot exercise mobility because their Jewish identity is constituted through halakhic obligation. This creates the classical identity-lock signal: trap-level subjective immutability arising from identity fusion, not structural immobility.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation creates distinct d values for each perspective. (1) Present generation: identity-locked exit at biographical time. This derives d ≈ 0.89 (high target, victim status + identity_locked at biographical = rope perception of immutability, but from analytical distance shows actual mobility is present). (2) Committed community: constrained exit (community exclusion cost is high but surmountable). Derives d ≈ 0.68 (moderate-high target). (3) Halakhic authority: arbitrage exit (can shift interpretation, redefine deferral, adopt alternative readings). Derives d ≈ 0.15 (low target, beneficiary status). The spread in d values (0.15 to 0.89) across the same structural constraint reflects how authority benefits while present generation bears costs. The suppression metric (0.52) is NOT scaled by these d values — suppression is the raw structural property of how alternatives are blocked (identity lock, institutional enforcement, epistemological barriers). The effective extractiveness chi experienced by each agent is χ = ε × f(d) × σ(S); the present generation with d=0.89 experiences high chi despite moderate ε, while the authority with d=0.15 experiences low chi. This directionality structure is NOT overridden — it derives naturally from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING EXEMPLAR: The mandatrophy is resolved by showing that each reading generates a different classification from the same kernel, and that the differences are not measurement noise but genuine differences in which structural features each reading emphasizes. The messianic_deferral reading emphasizes contingency and deferral, producing tangled_rope as the analytical compromise (genuine coordination + asymmetric extraction + genuine contingency for future restoration). The performance_only reading emphasizes suspension and husking, producing snare (extraction of study obligation without present fulfillment or contingent benefit). The study_as_performance reading emphasizes performative fulfillment, producing rope (study as genuine coordination mechanism with no extraction asymmetry). These are three coherent readings of the same halakhic doctrine, not three measurements of one truth. The constraint resolves by acknowledging that the kernel is genuinely contested — authority decides which reading dominates institutional interpretation, and perspectival variation (present generation vs. authority vs. analytical observer) propagates from the contested kernel into perspectival variation in type. The false-summit detection is relevant: the mountain perspective (analytical observer naturalizing deferral as immutable logic) is a false summit when beneficiaries are declared — the 'logic' of deferral is maintained by institutional actors who benefit from it. Removing that institutional benefit (or acknowledging it) dissolves the appearance of inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_restoration_timeline,
    'Is the messianic restoration contingency a genuine structural feature of the halakhic system, or a deferred-indefinitely rationalization that functionally treats suspended commandments as permanently obsolete?',
    'Historical analysis of halakhic authority statements about restoration likelihood; correlation between explicit messianic timeline claims and actual deferral maintenance duration; comparative analysis with similar contingent-on-future-event structures in other legal traditions',
    'If restoration contingency is genuine: deferral is scaffold (temporary, with sunset). If functionally indefinite: deferral is piton (performative maintenance of obsolete law). If systematically deferred to avoid present implementation: deferral is snare (extraction through obligation without fulfillment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_restoration_timeline, conceptual, 'Whether messianic restoration is genuine structural contingency or functionally permanent deferral').

omega_variable(
    study_obligation_present_value,
    'Does the study obligation for suspended commandments provide genuine present-day value (coordination of tradition, community cohesion, intellectual training, spiritual preparation), or is its value entirely future-contingent?',
    'Ethnographic analysis of study practice and stated rationales; measurement of community bonding and intellectual engagement; comparison with alternative ways to achieve same coordination outcomes (if suspension were lifted or acknowledged as permanent)',
    'If present value substantial: constraint is rope or tangled_rope (genuine coordination benefit alongside deferral cost). If value purely future-contingent: constraint is snare (extraction through obligation justified only by deferred benefit). If present and future value mixed: constraint is tangled_rope (confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_obligation_present_value, empirical, 'Whether study obligation provides genuine present-day coordination value').

omega_variable(
    reading_contestation_structure,
    'Is this deferral reading (suspended but not obsolete) institutionally endorsed as the binding interpretation, or one contested reading among live alternatives (performance_only, study_as_performance)?',
    'Analysis of halakhic authority consensus statements; documentation of dissenting readings and their institutional reach; analysis of which reading dominates in actual community practice',
    'If binding consensus: reading appears as mountain (natural law of halakhic interpretation). If contested live reading: reading appears as coexisting alternative (affects classification of sibling readings via reading_relations field in cs_structure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contestation_structure, empirical, 'Whether messianic deferral reading is institutional consensus or contested alternative').

omega_variable(
    kernel_contestation_cognitive_capture,
    'Are the three sibling readings (study_as_performance, performance_only, messianic_deferral) structurally stable alternative readings of one contested kernel, or does the framework of analysis itself presuppose one reading as obviously correct?',
    'Reflexive analysis of which reading the analytical observer''s own framework privileges; documentation of asymmetries in how each reading''s ''natural law'' appearance is justified; identification of identity-locked positions within the observer''s analysis',
    'If framework is neutral across readings: cs_structure classifications use coexists_with, influences symmetrically. If framework privileges one reading: analytical observer is identity_locked (oracle gap instantiated); reclassify observer perspective to include identity_locked exit option.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contestation_cognitive_capture, conceptual, 'Whether analytical framework itself is biased toward one kernel reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_md_theater_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.48).
narrative_ontology:measurement(kodashim_md_theater_t500, kodashim_commandment_status__messianic_deferral, theater_ratio, 500, 0.6).
narrative_ontology:measurement(kodashim_md_theater_t1000, kodashim_commandment_status__messianic_deferral, theater_ratio, 1000, 0.65).

% Extraction over time
narrative_ontology:measurement(kodashim_md_extractiveness_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(kodashim_md_extractiveness_t500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 500, 0.38).
narrative_ontology:measurement(kodashim_md_extractiveness_t1000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1000, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_md_suppression_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(kodashim_md_suppression_t500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 500, 0.48).
narrative_ontology:measurement(kodashim_md_suppression_t1000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1000, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.12).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, post_temple_jewish_obligation_framework).

% DUAL FORMULATION NOTE:
% The kodashim_commandment_status kernel decomposes into three constraint stories representing three contested readings. Each reading has its own ε value and perspectival classification profile. The messianic_deferral reading (this story, ε=0.38) emphasizes contingency and generates tangled_rope as the central type. Sibling readings performance_only (ε=0.68, snare-dominant) and study_as_performance (ε=0.15, rope-dominant) represent alternative framings of the same kernel with structurally different classifications. The three stories are linked via network.affects_constraints to represent the kernel contest structure. Do NOT collapse the three readings into one story with measurement-dependent ε — each reading is a distinct structural interpretation of the halakhic doctrine, and the ε-invariance principle requires separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
