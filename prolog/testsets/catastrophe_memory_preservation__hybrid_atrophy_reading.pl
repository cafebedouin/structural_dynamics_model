% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Ritual: Atrophied Survival Practice as Mourning Theater
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint traces the structural trajectory of a ritual practice
 *   that once preserved collective survival-competence (threat recognition,
 *   organizational response patterns, traumatic lesson transmission) but has
 *   atrophied under modernity to become primarily mourning theater. The
 *   hybrid_atrophy_reading positions this constraint at a specific moment in
 *   this trajectory: the ritual's original adaptive function (operational
 *   threat-recognition) has been lost or rendered inert in modern contexts
 *   where threats are abstract, distant, or institutional rather than
 *   immediate and communal; simultaneously, the ritual has acquired a
 *   secondary function (identity maintenance, intergenerational continuity
 *   narrative) that keeps it active despite the loss of its original purpose.
 *   This creates a piton: the ritual persists not because it functions as
 *   originally designed, but because institutional and identity-based inertia
 *   sustains the practice, and because the secondary mourning/identity
 *   function is now valuable enough to maintain. The constraint exhibits high
 *   theater (0.75) because much of the ritual's contemporary practice is
 *   ceremonial form without operational content—practitioners execute the
 *   ritual because it preserves symbolic continuity and group identity, not
 *   because it operationally preserves threat-recognition capacity.
 *   Present-generation practitioners bear extraction (time, emotional labor,
 *   maintenance of knowledge) without the adaptive survival payoff that would
 *   justify the burden in the original framework. The extractiveness metric
 *   (0.28) is moderate and declining because the ritual's burden has
 *   decreased as institutional and communal enforcement have relaxed, but
 *   suppression remains high (0.62) because identity fusion and communal
 *   expectation maintain the practice despite reduced functional payoff.
 *
 * KEY AGENTS:
 *   - Present-generation practitioners (powerless/identity_locked): bear costs of ritual maintenance without adaptive survival payoff; identity fused with practice
 *   - Religious/cultural leadership (organized/constrained): benefit from coordination function (group cohesion); bear enforcement costs; aware of atrophy but maintain ritual for identity continuity
 *   - Institutional religious framework (institutional/arbitrage): benefits from ritualized authority; persists through inertia and doctrinal justification; theater ratio 75%
 *   - Analytical observer (analytical/analytical): can see the coordination function and identity maintenance without collapsing into the institutional or identity-locked perspectives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Ritual: Atrophied Survival Practice as Mourning Theater").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'a088cdb6-5c5f-43da-8150-b8686c8b3da9').
narrative_ontology:cs_kernel_codification('a088cdb6-5c5f-43da-8150-b8686c8b3da9', distributed).
narrative_ontology:cs_authority_grounding('a088cdb6-5c5f-43da-8150-b8686c8b3da9', practice).
narrative_ontology:cs_interpretation_layer_present('a088cdb6-5c5f-43da-8150-b8686c8b3da9').
narrative_ontology:cs_reading_relation('a088cdb6-5c5f-43da-8150-b8686c8b3da9', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('a088cdb6-5c5f-43da-8150-b8686c8b3da9', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_axiom('a088cdb6-5c5f-43da-8150-b8686c8b3da9', foundational, ritual_function_historically_adaptive).
narrative_ontology:cs_axiom_status(ritual_function_historically_adaptive, holdable).
narrative_ontology:cs_axiom_grounding('a088cdb6-5c5f-43da-8150-b8686c8b3da9', ritual_function_historically_adaptive, empirically_contingent).
narrative_ontology:cs_axiom('a088cdb6-5c5f-43da-8150-b8686c8b3da9', foundational, modern_practice_atrophied_to_ceremony).
narrative_ontology:cs_axiom_status(modern_practice_atrophied_to_ceremony, holdable).
narrative_ontology:cs_axiom_grounding('a088cdb6-5c5f-43da-8150-b8686c8b3da9', modern_practice_atrophied_to_ceremony, empirically_contingent).
narrative_ontology:cs_reference_frame('a088cdb6-5c5f-43da-8150-b8686c8b3da9', continuous_threat_recognition_practice).
narrative_ontology:cs_drift_state('a088cdb6-5c5f-43da-8150-b8686c8b3da9', contemporary_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a088cdb6-5c5f-43da-8150-b8686c8b3da9', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintenance).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, intergenerational_continuity_narrative).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESENT-GENERATION PRACTITIONER (SNARE) — Identity fused with inherited ritual practice. Structurally mobile (could stop performing the ritual) but identity-locked into it by religious belonging, familial obligation, and communal expectation. Bears substantial costs (time, emotional labor, maintenance of knowledge) without adaptive survival payoff. The ritual no longer preserves threat-recognition capacity; it preserves identity only. Experiences maximum extraction because exit would require becoming a different person within the community.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__hybrid_atrophy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RELIGIOUS/CULTURAL LEADERSHIP (TANGLED ROPE) — Benefits from the ritual's coordination function (maintains group cohesion, intergenerational transmission, collective identity) while also bearing enforcement costs (teaching, correction, adaptation to modern contexts). Constrained exit: abandoning the ritual would disrupt their authority and the community's self-understanding. Both coordination (real identity maintenance) and extraction (asymmetric enforcement burden) present. Leadership is aware the ritual no longer preserves operational survival competence but maintains it because identity continuity has become the primary function.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__hybrid_atrophy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL RELIGIOUS FRAMEWORK (PITON) — The ritual persists through institutional inertia and theological justification (canonical texts, doctrinal claims about continuity with ancestors). Performative content is high (75% theater — ceremonial form, symbolic language, commemorative gestures without operational content). The institution experiences arbitrage: the ritual no longer requires genuine operational maintenance but persists because changing it would disrupt the narrative of continuous authority and tradition. Degraded piton: the function (survival-competence preservation) has atrophied, but the ritual persists because the institution has 'become' the ritual's preservation.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__hybrid_atrophy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / COORDINATION VIEW (ROPE) — From a sufficiently abstract perspective, the ritual solves a genuine coordination problem: how does a community transmit catastrophe lessons and maintain collective identity across generational discontinuity? The ritual's actual mechanism (identity fusion, ceremonial practice, narrative continuity) succeeds at this coordination regardless of whether it preserves operational threat-recognition. This perspective sees low extractiveness (0.15-0.20) because the coordination function is real and broadly distributed. However, this perspective obscures the atrophy — it treats the current state as a deliberate and sufficient adaptation.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__hybrid_atrophy_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_preservation__hybrid_atrophy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_preservation__hybrid_atrophy_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28, declining from 0.42): Moderate and declining. The ritual's original extractiveness was high because maintaining survival-competence transmission required substantial ongoing effort and bore real adaptive payoff—the burden was justified by the outcome. As modernity has made the original adaptive function obsolete or inert (threats are now institutional/geopolitical rather than communal/immediate), the extractiveness has declined because the burden no longer maps onto survival function. However, it has not declined to zero because present-generation practitioners still bear substantial costs (time, emotional labor, knowledge maintenance) for identity and continuity functions that are secondary and less universal. Theater ratio (0.75, rising from 0.55): High and rising. As the ritual's functional content has atrophied, its performative/ceremonial content has come to dominate. Modern practitioners maintain the form (ceremonial language, commemorative gestures, symbolic enactments) without the operational content (threat-recognition training, real organizational practice). The theater has increased as practitioners have become more conscious that the ritual preserves identity rather than survival-competence, and they have adapted the practice to this secondary function. Suppression (0.62, stable): High and stable. The ritual is maintained through strong communal and identity-based suppression of exit. Practitioners are identity-locked (exit would require identity dissolution), and communal expectation enforces participation. Institutional suppression (doctrinal justification, theological claims about continuity) is stable across the interval. The suppression does not decline because identity lock and communal enforcement remain even as the functional payoff has disappeared.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound. Present-generation practitioners experience the constraint as snare-like—they are trapped by identity fusion and bear costs without adaptive payoff. Leadership experiences it as tangled_rope—they coordinate group function while bearing enforcement costs. The institution experiences it as piton—a degraded ritual maintained through inertia. The analytical observer experiences it as rope—a genuine coordination mechanism for transmitting identity and continuity. None of these perspectives are wrong; they describe genuinely different structural positions. However, the gap reveals the constraint's instability: if practitioners increasingly recognize the atrophy (shift toward the analytical perspective), the identity lock may weaken, and the constraint may transition toward a rope or decompose entirely. If leadership loses the will to enforce, suppression drops and the constraint may become an optional practice rather than a maintained ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   The hybrid_atrophy_reading positions present-generation practitioners as victims experiencing identity-locked exit (they are structurally mobile—could stop performing the ritual—but cannot exercise this mobility without identity dissolution). The power-atom for this perspective is 'powerless' because they have no genuine agency within the constraint structure; the constraint is maintained through their identity fusion, not through their voluntary participation. The religious/cultural leadership occupies a beneficiary position (they benefit from the ritual's coordination function and their authority is sustained by maintaining it) but with constrained exit (abandoning it would disrupt their role). The institutional framework is a pure beneficiary with arbitrage exit (it can reinterpret the ritual's meaning while maintaining it, adjusting the performance as needed without fundamental transformation). The analytical observer sees low extractiveness because the coordination function (identity maintenance, intergenerational transmission) is real and represents a legitimate social need, even though it is disconnected from the ritual's original purpose.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the classical sense because its classification (piton) is stable across perspectives—no two perspectives contradict on the fundamental type. However, there is a temporal mandatrophy built into the story: as the ritual transitions from functional (survival-competence preservation) to atrophied (identity/mourning theater), the classification changes from tangled_rope (when the ritual bore real coordination and real extraction with adaptive justification) to piton (when the functional content has been lost and only theatrical performance remains). This temporal shift is visible in the measurements: extractiveness declines (the burden is less justified as function disappears), theater ratio rises (the ceremonial form comes to dominate), and suppression remains stable (identity lock and communal enforcement persist even as function atrophies). The constraint's movement from tangled_rope toward piton is the atrophy story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_vs_adaptation_boundary,
    'Is the shift from survival-competence to mourning-practice atrophy (loss of original function) or legitimate adaptation (ritual respecified for new social function)?',
    'Historical analysis of ritual practice change: comparison of pre-industrial (when threat recognition operated) vs modern performance (when threats are abstract or geopolitically distant); documentation of whether practitioners recognize the loss of operational function or frame it as intentional respecification.',
    'If atrophy: piton classification stands; extractiveness and suppression should remain high (burden without payoff). If adaptation: reclassify as rope or tangled_rope; extractiveness represents legitimate coordination cost, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_adaptation_boundary, empirical, 'Whether ritual shift is atrophy or adaptive respecification').

omega_variable(
    survival_competence_preservation_mechanism,
    'Does the inherited ritual practice actually preserve any operational threat-recognition capacity that practitioners would lack without it (implicit knowledge of historical threats, organizational response patterns, group cohesion under stress)?',
    'Comparative ethnography: measure threat-recognition accuracy and collective response capacity in communities with continuous ritual practice vs those without; assess whether specific ritual elements correlate with improved survival outcomes in actual catastrophe scenarios.',
    'If operative: survival_competence_reading is correct; reclassify to rope or tangled_rope with genuine coordination function. If inert: hybrid_atrophy_reading confirmed; piton classification holds, extractiveness reflects burden without adaptive payoff.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survival_competence_preservation_mechanism, empirical, 'Whether ritual practice preserves functional threat-recognition capacity').

omega_variable(
    kernel_reading_boundary,
    'Is this constraint the hybrid_atrophy_reading (ritual has two phases: formerly adaptive, now atrophied to mourning theater) or is it actually the mourning_practice_reading (ritual is best understood as always-already a mourning practice, with ''survival competence'' as a retrospective or mythological attribution)?',
    'Historical documentation: direct accounts of ritual function in pre-modern catastrophe contexts; analysis of whether the operational survival-competence interpretation is grounded in documented practice or is a later theological reading imposed on purely ceremonial activity.',
    'If hybrid_atrophy: this story is correct; extractiveness reflects the temporal trajectory from function to theater. If mourning_practice: reclassify to a different reading entirely; extractiveness and beneficiary/victim structure shift fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, empirical, 'Whether ritual exhibits temporal atrophy or was always mourning practice').

omega_variable(
    identity_lock_irreversibility,
    'For practitioners, is identity_locked exit modulation describing a genuinely identity-fused bind, or is it better modeled as constrained exit (high cost to exit, but not identity collapse)?',
    'Ethnographic analysis of practitioners who have exited: do they describe the exit as identity dissolution (identity_locked) or as high-cost relocation (constrained)? Do non-practitioners who were raised in the tradition but left describe themselves as fundamentally changed persons or as having paid high social costs for a manageable boundary shift?',
    'If identity_locked: present-generation snare classification holds; suppression is high (cognitive + material barriers). If constrained: reclassify present generation to moderate power with higher agency; suppression drops to 0.40-0.50.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_irreversibility, empirical, 'Whether practitioner exit is identity dissolution or high-cost relocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmp_atrophy_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cmp_atrophy_tr_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 50, 0.68).
narrative_ontology:measurement(cmp_atrophy_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.75).

% Extraction over time
narrative_ontology:measurement(cmp_atrophy_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cmp_atrophy_be_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(cmp_atrophy_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cmp_atrophy_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cmp_atrophy_su_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(cmp_atrophy_su_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_preservation kernel generates three structurally distinct constraint stories. The hybrid_atrophy_reading (this file) describes the constraint as a temporal trajectory from functional coordination to atrophied theater. The mourning_practice_reading describes the constraint as a stable identity/mourning function (survives_competence attribution is secondary). The survival_competence_reading describes the constraint as a persistent operational transmission mechanism. These are not three perspectives on one constraint—they are three distinct constraints whose difference turns on how the ritual's history is interpreted and whether its contemporary function is understood as atrophy or as adaptation. All three stories share the same base practices but differ in their ε values and beneficiary/victim structures depending on whether the analyst treats the ritual as having lost function (hybrid_atrophy: extractiveness declining), as having preserved function (survival_competence: extractiveness stable or slowly declining as modernity increases information access), or as having always been primarily identity-based (mourning_practice: extractiveness stable, suppression justified by genuine coordination need). The network links indicate that resolving the kernel contest empirically would change which story is authoritative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
