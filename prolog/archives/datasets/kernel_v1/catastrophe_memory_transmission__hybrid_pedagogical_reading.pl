% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_pedagogical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_pedagogical_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__hybrid_pedagogical_reading
 *   human_readable: Catastrophe-Memory Transmission as Hybrid Pedagogy: Mourning-as-Vigilance
 *   domain: religious_studies/cultural_anthropology/memory_studies
 *
 * SUMMARY:
 *   Catastrophe-memory transmission through mourning-as-vigilance represents
 *   a hybrid pedagogical system in which grief-processing simultaneously
 *   encodes early-warning competence and enforces identity continuity. This
 *   constraint is one reading of a contested kernel about how communities
 *   preserve collective memory across generations following major trauma. The
 *   hybrid pedagogical reading holds that the primary function is DUAL:
 *   mourning ritual teaches emotional processing (boundary-maintenance,
 *   witness-bearing, intergenerational acknowledgment) AND transmits
 *   actionable survival knowledge (threat-recognition patterns, adaptive
 *   strategies, early-warning protocols embedded in collective memory). The
 *   constraint's extractiveness depends critically on whether this embedded
 *   competence remains functionally available to future generations or
 *   becomes increasingly symbolic and performative. As communities distance
 *   from the original catastrophe, extractiveness tends to rise because the
 *   vigilance burden persists while the competence foundation attenuates. The
 *   measurement trajectory shows this degradation: extractiveness rises from
 *   0.28 (when living memory of the catastrophe was direct) to 0.52 (when
 *   transmission has become increasingly ritualized); theater ratio rises
 *   correspondingly from 0.35 to 0.64 (indicating more performative, less
 *   competence-rich mourning); suppression (the burden on bereaved
 *   individuals and successor generations) rises from 0.42 to 0.58. This
 *   constraint exemplifies how coordination mechanisms can degrade into
 *   extraction as the functional purpose becomes obscured by ritual
 *   perpetuation.
 *
 * KEY AGENTS:
 *   - Bereaved Individuals: Primary victims (powerless/identity_locked) — grief-processing is mandatory identity work; cannot exit without abandoning social position and warning-role. Bear the emotional labor of vigilance.
 *   - Survivor Collective: Organized actors (organized/constrained) — coordinate ritual transmission and threat-recognition; benefit from collective resilience but constrained by resource commitment to ritual maintenance.
 *   - Community Leadership / Institutional Memory: Primary beneficiaries (institutional/arbitrage) — benefit from social cohesion, authority to interpret what threats warrant vigilance, and intergenerational continuity. Can arbitrage the system through interpretive control.
 *   - Future Generations (at Catastrophe Risk): Secondary victims (powerless/trapped) — inherit intergenerational debt; face heightened risk if warning system has atrophied to purely symbolic form.
 *   - Ritual Performance Layer: Institutional actor (institutional/constrained) — maintains ceremonial form; increasingly performative as embedded competence is lost. Theater drives classification toward Piton.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as genuinely hybrid; classification depends on empirical status of embedded competence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_pedagogical_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_pedagogical_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_pedagogical_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_pedagogical_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_pedagogical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_pedagogical_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_pedagogical_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_pedagogical_reading, "Catastrophe-Memory Transmission as Hybrid Pedagogy: Mourning-as-Vigilance").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_pedagogical_reading, "religious_studies/cultural_anthropology/memory_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__hybrid_pedagogical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_pedagogical_reading, 'ec79ac7e-9854-4265-8049-7ee780779b44').
narrative_ontology:cs_kernel_codification('ec79ac7e-9854-4265-8049-7ee780779b44', implicit).
narrative_ontology:cs_authority_grounding('ec79ac7e-9854-4265-8049-7ee780779b44', lineage).
narrative_ontology:cs_interpretation_layer_present('ec79ac7e-9854-4265-8049-7ee780779b44').
narrative_ontology:cs_reading_relation('ec79ac7e-9854-4265-8049-7ee780779b44', catastrophe_memory_transmission__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec79ac7e-9854-4265-8049-7ee780779b44', catastrophe_memory_transmission__survival_competence_reading, influences).
narrative_ontology:cs_axiom('ec79ac7e-9854-4265-8049-7ee780779b44', foundational, mourning_encodes_actionable_competence).
narrative_ontology:cs_axiom_status(mourning_encodes_actionable_competence, holdable).
narrative_ontology:cs_axiom_grounding('ec79ac7e-9854-4265-8049-7ee780779b44', mourning_encodes_actionable_competence, empirically_contingent).
narrative_ontology:cs_axiom('ec79ac7e-9854-4265-8049-7ee780779b44', foundational, early_warning_transmission_enables_adaptive_capacity).
narrative_ontology:cs_axiom_status(early_warning_transmission_enables_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('ec79ac7e-9854-4265-8049-7ee780779b44', early_warning_transmission_enables_adaptive_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('ec79ac7e-9854-4265-8049-7ee780779b44', living_memory_of_catastrophe_actively_shapes_ritual).
narrative_ontology:cs_drift_state('ec79ac7e-9854-4265-8049-7ee780779b44', post_living_memory_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('ec79ac7e-9854-4265-8049-7ee780779b44', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_pedagogical_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_pedagogical_reading, community_leadership).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_pedagogical_reading, intergenerational_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_pedagogical_reading, bereaved_individuals).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_pedagogical_reading, future_generations_if_warning_atrophies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BEREAVED INDIVIDUAL (SNARE) — Identity is constituted through the mourning role; grief-processing is mandatory identity work. Cannot exit without abandoning their place in the community's warning system. The constraint extracts emotional labor and vigilance while framing these as identity expression rather than work. Individual experiences maximal extraction because their identity is locked into the role of witness-who-remembers.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_pedagogical_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: SURVIVOR COLLECTIVE (TANGLED ROPE) — Organized agents who coordinate mourning rituals and transmit survival knowledge. Experience both genuine coordination (collective threat-recognition and adaptive capacity) and asymmetric extraction (burden of maintaining vigilance, resource commitment to ritual maintenance). Can exit but at high cost to group cohesion and early-warning capacity.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_pedagogical_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMMUNITY LEADERSHIP (ROPE) — Institutional actors who benefit from the constraint's coordination function. Leadership experiences the mourning system as pure coordination: ritual transmits survival knowledge, maintains group identity, and enables collective threat-response. Net beneficiary through social cohesion and proven adaptive capacity. Can arbitrage the system by controlling interpretive authority over what threats warrant vigilance.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_pedagogical_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FUTURE GENERATIONS AT CATASTROPHE RISK (SNARE) — If the mourning-vigilance system degrades, future generations face heightened catastrophe risk with attenuated early-warning capacity. Trapped in intergenerational debt they did not incur; the constraint's extraction (through atrophied warning systems) manifests as vulnerability. They bear the cost of intergenerational transmission failure.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_pedagogical_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: RITUAL PERFORMANCE LAYER (PITON) — The formal ceremonies and commemorative practices increasingly perform mourning without transmitting embedded survival competence. Theater has risen as communities lose living connection to original catastrophe. Rituals persist through institutional inertia and identity commitment rather than functional early-warning value. Theater ratio high because the warning content has become implicit or lost.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_pedagogical_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits genuine coordination (cultural transmission of survival knowledge through grief-processing) alongside asymmetric extraction (burden of vigilance concentrated on the bereaved, institutional benefit accruing to leadership). The constraint is neither pure natural law nor pure extraction—it is a hybrid that depends critically on whether embedded survival competence remains actionable or becomes purely symbolic.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_pedagogical_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_pedagogical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_pedagogical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_pedagogical_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_pedagogical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_pedagogical_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_transmission__hybrid_pedagogical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts emotional labor from bereaved individuals and ongoing vigilance from the collective, framed as identity-expression and community duty rather than work. The extraction is not maximal because the constraint's coordination function is real—communities demonstrably improve threat-recognition and collective adaptive capacity through ritual transmission. But extractiveness is substantial because living memory (which made competence clear) has faded; the ritual persists through institutional inertia, not active competence. Suppression (0.58): Moderate-high. Bereaved individuals face strong barriers to exiting the mourning role—social ostracism, identity dissolution, loss of community standing. The constraint is socially enforced through shame and belonging pressure. Exit is theoretically possible but psychologically costly for identity-locked individuals. Theater ratio (0.64): High. The ritual performance component dominates in communities distant from the original catastrophe. Formal ceremonies persist, but the embedded survival knowledge they encode is increasingly opaque to participants. The constraint is maintained through institutional inertia and identity commitment rather than transparent early-warning function. Measurement trajectory reveals degradation over time: as direct catastrophe memory fades, theater rises and competence actionability falls, pushing extractiveness upward.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid pedagogical reading produces a distinct perspectival architecture. The bereaved individual at biographical time-horizon with identity_locked exit sees the constraint as binding (Snare-toward Rope boundary depending on whether competence remains actionable). The survivor collective sees coordination with asymmetric burden (Tangled Rope). The institutional leadership sees pure coordination (Rope). Future generations see the constraint's degraded form as pure risk (Snare). The ritual performance layer sees its own increasing theater (Piton). The analytical observer sees the full hybrid: genuine coordination function alongside real extraction, with extractiveness trending upward as competence attenuates. This perspectival range is the diagnostic signature of the hybrid reading—other readings of the same kernel would produce different gap structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Bereaved individuals experience high extraction (d ≈ 0.88) because they bear the emotional labor of vigilance while lacking exit capacity (identity_locked means structural mobility is obscured by identity fusion). Community leadership experiences low extraction (d ≈ 0.08) because they benefit from the coordination function and retain arbitrage options. The survivor collective experiences moderate extraction (d ≈ 0.50) because they participate in both coordination and burden-bearing. The analytical observer derives d from their structural position relative to the constraint (neither full beneficiary nor victim, but positioned to see both), yielding d ≈ 0.72. The constraint's effective extraction (χ) is scaled by the sigmoid f(d) and spatial scope σ(S=local/regional ranges from 0.8–0.9). No directionality overrides are necessary—the structural data (identity_locked exit + victim status for bereaved individuals, arbitrage exit + beneficiary status for leadership) drives d values that produce the observed perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid pedagogical reading resolves the mandatrophy by distinguishing genuine coordination (transmission of survival competence) from extraction (emotional labor burden). The classification as Tangled Rope is correct CONDITIONAL on the embedded competence remaining actionable. If competence atrophies to symbolic form, the constraint should be reclassified as higher-extractiveness Snare from most perspectives. The measurement trajectory (rising extractiveness and theater) suggests this atrophy is occurring in real time. The constraint's stability depends on whether communities can maintain the dual function (emotional + competence) as direct catastrophe memory recedes. This is not a mandatrophy that requires reclassification but a **lifecycle drift**: the constraint is moving from genuine Tangled Rope (high coordination benefit justifying the burden) toward degraded Snare (burden without competence benefit) as the system's functional purpose obscures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_actionability_threshold,
    'At what point does embedded survival knowledge in mourning ritual become purely symbolic rather than actionable early-warning competence?',
    'Longitudinal ethnographic tracking: correlation between ritual participation and measurable threat-recognition capacity (response times to warning signs, accuracy of threat identification, transmission quality across generations)',
    'If knowledge remains actionable: constraint is genuinely mixed (Tangled Rope with real coordination function). If knowledge becomes symbolic: constraint shifts toward Snare (extraction of emotional labor and vigilance with diminished coordination benefit). Extractiveness should track with competence loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_actionability_threshold, empirical, 'Whether embedded survival knowledge remains actionable or becomes symbolic').

omega_variable(
    identity_lock_vs_structural_trapping,
    'Is the bereaved individual''s immobility primarily due to identity-fusion with the mourning role, or due to structural barriers (social exclusion, economic dependency, lack of alternative community)?',
    'Ethnographic observation of exit attempts; analysis of agents who leave the community vs. those who remain; measurement of identity salience (how central is the mourning role to self-concept) vs. material barriers',
    'If identity-locked: the classification from biographical time-horizon is Rope (agent perceives mutability in principle), not Mountain. If structurally trapped: Snare persists across timeframes. This differentiates cognitive from material binding mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trapping, empirical, 'Whether immobility is cognitive (identity-locked) or structural (trapped)').

omega_variable(
    leadership_extraction_intentionality,
    'Does community leadership consciously extract benefit from the mourning-vigilance system, or is the asymmetric benefit an unintended consequence of institutional perpetuation?',
    'Analysis of leadership statements, control patterns, and adaptive responses to competence loss; observation of whether leadership actively maintains the system when competence degrades or allows it to become purely performative',
    'If intentional extraction: constraint moves toward Snare classification from leadership perspective. If unintended: remains Rope (coordination with incidental asymmetry). Does not change the base properties but influences interpretation of whether the system is being actively enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leadership_extraction_intentionality, conceptual, 'Whether leadership extraction is intentional or structural byproduct').

omega_variable(
    reading_contest_empirical_resolution,
    'Which of the three sibling readings (hybrid_pedagogical, mourning_practice, survival_competence) does the empirical data actually support as the primary function of catastrophe-memory transmission?',
    'Comparative ethnographic analysis: measure outcome variation across communities with different emphasis patterns (grief-processing vs. boundary-maintenance vs. competence transmission); track which emphasis correlates with measurable early-warning capacity and identity continuity',
    'If hybrid_pedagogical dominates: extractiveness is moderate (~0.52) because real coordination function partially justifies the burden. If mourning_practice dominates: hybrid reading is aspirational; actual extractiveness may be higher (~0.65+) with less real competence. If survival_competence dominates: hybrid reading accurately captures the constraint. This omega routes the committer frame''s reading contest into empirical territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_empirical_resolution, empirical, 'Which reading of the kernel is empirically dominant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_pedagogical_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_hybrid_tr_t0, catastrophe_memory_transmission__hybrid_pedagogical_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catmem_hybrid_tr_t3, catastrophe_memory_transmission__hybrid_pedagogical_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(catmem_hybrid_tr_t6, catastrophe_memory_transmission__hybrid_pedagogical_reading, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(catmem_hybrid_be_t0, catastrophe_memory_transmission__hybrid_pedagogical_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(catmem_hybrid_be_t3, catastrophe_memory_transmission__hybrid_pedagogical_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(catmem_hybrid_be_t6, catastrophe_memory_transmission__hybrid_pedagogical_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(catmem_hybrid_su_t0, catastrophe_memory_transmission__hybrid_pedagogical_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(catmem_hybrid_su_t3, catastrophe_memory_transmission__hybrid_pedagogical_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(catmem_hybrid_su_t6, catastrophe_memory_transmission__hybrid_pedagogical_reading, suppression_requirement, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_pedagogical_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__hybrid_pedagogical_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_pedagogical_reading, catastrophe_memory_transmission__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_pedagogical_reading, catastrophe_memory_transmission__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_pedagogical_reading, intergenerational_identity_transmission).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_pedagogical_reading, ritual_theater_and_competence_atrophy).

% DUAL FORMULATION NOTE:
% This constraint is part of the catastrophe_memory_transmission kernel family. The three readings (hybrid_pedagogical, mourning_practice, survival_competence) are structurally distinct constraints with overlapping domains but different ε values and victim sets. The hybrid reading (this file) claims ε ≈ 0.52 with extractiveness trending upward as competence attenuates. The mourning_practice reading would show lower coordination function and higher theater (ε ≈ 0.58). The survival_competence reading would show higher coordination benefit if competence remains robust (ε ≈ 0.35). Each reading is a separate constraint story linked via network.affects_constraints. The empirical resolution of which reading dominates in a given community determines which constraint story best models that community's actual dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
