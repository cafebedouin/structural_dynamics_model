% ============================================================================
% CONSTRAINT STORY: suffering_ontology_mountain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suffering_ontology_mountain, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: suffering_ontology_mountain
 *   human_readable: Suffering as Ontological Feature of Embodied Existence
 *   domain: philosophy/existential_psychology/moral_philosophy
 *
 * SUMMARY:
 *   The ontological status of suffering represents a foundational question in
 *   philosophy, existential psychology, and moral philosophy: Is suffering a
 *   contingent feature of specific social arrangements (and thus potentially
 *   eliminable through institutional change), or is it an irreducible feature
 *   of embodied existence? This constraint story models suffering-as-such —
 *   the presence of suffering in human experience, not any particular
 *   instance or source of suffering — as a mountain constraint. The
 *   classification is based on three converging lines of evidence: (1)
 *   Cross-cultural invariance: suffering is present across all known human
 *   societies despite radically different social, economic, and political
 *   arrangements. Hunter-gatherer, agricultural, industrial, and
 *   post-industrial societies; egalitarian and hierarchical structures;
 *   collectivist and individualist cultures — all contain suffering. (2)
 *   Physiological substrate: pain mechanisms, stress responses, aging,
 *   disease, and mortality have biological bases that are not contingent on
 *   social arrangements. (3) Philosophical convergence: diverse and
 *   independent philosophical and religious traditions (Buddhism, Stoicism,
 *   Existentialism, Christianity, Daoism, Epicureanism) all treat suffering
 *   as a fundamental feature of existence requiring response, not a
 *   contingent problem admitting of elimination. The constraint's very low
 *   extractiveness (ε = 0.08) reflects that suffering is not imposed by any
 *   agent or institution — it is not extraction. The minimal extractiveness
 *   represents the unavoidable 'cost' of embodied existence: biological
 *   organisms with nervous systems capable of pain, consciousness capable of
 *   loss, and finite lifespans necessarily encounter suffering. This is not a
 *   false summit naturalizing contingent social arrangements; it is
 *   recognition of structural limits inherent to the kind of beings we are.
 *
 * KEY AGENTS:
 *   - Suffering Individual (immediate experience): powerless/trapped — cannot exit embodiment or physiological pain mechanisms; experiences suffering as immediate and unchangeable
 *   - Biographical Subject (life-span view): moderate/constrained — can reduce specific sufferings through choices and circumstances but cannot eliminate suffering across the lifespan
 *   - Medical/Therapeutic Institution: institutional/arbitrage — can ameliorate specific instances of suffering but exists precisely because suffering is a structural constant
 *   - Philosophical/Religious Tradition: organized/mobile — can adopt different frameworks for responding to suffering but all frameworks address the same underlying phenomenon
 *   - Analytical Observer (cross-cultural invariance): analytical/analytical — observes suffering's presence across all human contexts and identifies it as a natural law constraint
 *   - Privileged Individual (resource-rich position): powerful/arbitrage — can insulate from many contingent sufferings but still faces aging, loss, and mortality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suffering_ontology_mountain, 0.08).
domain_priors:suppression_score(suffering_ontology_mountain, 0.02).
domain_priors:theater_ratio(suffering_ontology_mountain, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suffering_ontology_mountain, extractiveness, 0.08).
narrative_ontology:constraint_metric(suffering_ontology_mountain, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(suffering_ontology_mountain, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(suffering_ontology_mountain, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(suffering_ontology_mountain, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suffering_ontology_mountain, mountain).
narrative_ontology:human_readable(suffering_ontology_mountain, "Suffering as Ontological Feature of Embodied Existence").
narrative_ontology:topic_domain(suffering_ontology_mountain, "philosophy/existential_psychology/moral_philosophy").

domain_priors:emerges_naturally(suffering_ontology_mountain).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUFFERING INDIVIDUAL / IMMEDIATE EXPERIENCE (MOUNTAIN) — From the immediate phenomenological position, suffering presents as an unchangeable feature of embodied existence. Physical pain, illness, aging, and mortality are not negotiable. The individual cannot exit their embodiment or the physiological mechanisms that produce suffering. This is not extraction — no agent benefits from the constraint — but an irreducible structural feature of biological existence.
constraint_indexing:constraint_classification(suffering_ontology_mountain, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: BIOGRAPHICAL SUBJECT / LIFE-SPAN VIEW (MOUNTAIN) — Across a human lifespan, suffering remains structurally invariant despite changes in social position, wealth, or circumstance. The biographical arc includes unavoidable loss, decline, and death. While specific sources of suffering may be contingent (poverty, oppression, illness), the presence of suffering itself is not. The moderate power and constrained exit reflect that biographical agents have some capacity to reduce specific sufferings but cannot eliminate suffering as such.
constraint_indexing:constraint_classification(suffering_ontology_mountain, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MEDICAL/THERAPEUTIC INSTITUTION (MOUNTAIN) — Medical and therapeutic institutions exist precisely because suffering is a structural constant. They can ameliorate specific instances (treat disease, manage pain, provide palliative care) but cannot eliminate the underlying ontological feature. The institutional perspective has arbitrage exit (can choose which suffering to address, can allocate resources) but still classifies the constraint as mountain because the phenomenon itself is not contingent on institutional arrangements. Medicine does not create suffering; it responds to it.
constraint_indexing:constraint_classification(suffering_ontology_mountain, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHILOSOPHICAL/RELIGIOUS TRADITION (MOUNTAIN) — Organized philosophical and religious traditions across cultures (Buddhism, Stoicism, Existentialism, Christianity, Daoism) converge on suffering as a fundamental feature of existence requiring response rather than elimination. The diversity of proposed responses (acceptance, transcendence, meaning-making, compassion) itself indicates that the constraint is not contingent on any particular cultural framework. Traditions have mobile exit (can adopt different frameworks) but all frameworks address the same underlying phenomenon.
constraint_indexing:constraint_classification(suffering_ontology_mountain, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CROSS-CULTURAL INVARIANCE (MOUNTAIN) — From a civilizational/universal analytical position, suffering exhibits the signature of a natural law constraint: (1) Cross-cultural presence despite radically different social systems — hunter-gatherer, agricultural, industrial, and post-industrial societies all contain suffering. (2) Physiological substrate — pain mechanisms, stress responses, aging, and mortality are biological universals. (3) Zero degrees of freedom — no social arrangement, technological intervention, or philosophical framework has eliminated suffering as such. (4) Accessibility collapse ≥ 0.95 — all agents across all contexts encounter suffering. (5) Resistance ≤ 0.08 — attempts to eliminate suffering (rather than specific sufferings) have universally failed. This is not a false summit naturalizing contingent social arrangements; it is recognition of an irreducible feature of embodied existence.
constraint_indexing:constraint_classification(suffering_ontology_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: PRIVILEGED INDIVIDUAL / RESOURCE-RICH POSITION (MOUNTAIN) — Even from a position of maximum social power and resource access, suffering remains structurally present. Wealth, status, and power can reduce specific sufferings (poverty, oppression, lack of medical care) but cannot eliminate aging, illness, loss, or mortality. The arbitrage exit reflects that powerful agents can choose which sufferings to address and can insulate themselves from many contingent sufferings, but the constraint itself remains mountain — the privileged still age, still lose loved ones, still face their own mortality. This perspective is critical for distinguishing ontological suffering (mountain) from socially-contingent suffering (which may be snare, tangled_rope, or other extractive types).
constraint_indexing:constraint_classification(suffering_ontology_mountain, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suffering_ontology_mountain_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(suffering_ontology_mountain, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suffering_ontology_mountain, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(suffering_ontology_mountain, ExtMetricName, E),
    domain_priors:suppression_score(suffering_ontology_mountain, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(suffering_ontology_mountain),
    narrative_ontology:constraint_metric(suffering_ontology_mountain, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(suffering_ontology_mountain, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(suffering_ontology_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. Suffering-as-such is not imposed by any agent or institution. No beneficiary extracts from the constraint. The minimal extractiveness represents the unavoidable 'cost' of embodied existence: biological organisms with nervous systems, consciousness, and finite lifespans necessarily encounter suffering. This is not zero because existence itself has costs (energy expenditure, vulnerability, finitude), but it is not extraction in the sense of asymmetric transfer from victim to beneficiary. Suppression (0.02): Minimal. Suffering does not suppress alternatives because it is not maintained by any enforcement mechanism. Agents are free to respond to suffering in diverse ways (acceptance, transcendence, amelioration, meaning-making). The minimal suppression reflects only that embodied existence itself is not optional — one cannot choose not to be embodied while alive — but within that constraint, degrees of freedom are maximal. Theater ratio (0.15): Very low. Responses to suffering (medical treatment, therapy, spiritual practice, philosophical reflection) are largely functional rather than performative. Some theater exists (e.g., cultural rituals around death that serve social cohesion more than the dying individual), but the vast majority of suffering-response activity has genuine function. Accessibility collapse (0.95): Near-total. All embodied agents encounter suffering. Resistance (0.08): Very low. No social arrangement, technological intervention, or philosophical framework has eliminated suffering as such, though many have reduced specific sufferings. Emerges naturally: True. Suffering arises from the structure of embodied existence, not from institutional design.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all six perspectives classify as mountain. This is the signature of a genuine natural law constraint: invariance across observation positions. The powerless individual in immediate pain, the biographical subject across a lifespan, the institutional medical system, the organized philosophical tradition, the analytical cross-cultural observer, and the privileged individual with maximum resources all encounter the same structural reality: suffering is present and cannot be eliminated, only responded to. The uniformity of classification is itself diagnostic. If any perspective classified the constraint as rope, tangled_rope, or snare, it would indicate that the 'suffering' being measured was actually a contingent social arrangement (e.g., poverty, oppression, lack of medical access) rather than ontological suffering. The fact that even the powerful/arbitrage perspective (the privileged individual) classifies as mountain confirms that we are measuring a feature of embodied existence, not a feature of social hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims in the extractive sense. Suffering is not imposed by one agent on another; it is a structural feature of embodied existence. All perspectives therefore derive directionality from their power and exit options relative to an abstract 'constraint of embodiment' rather than from a beneficiary/victim relationship. The powerless/trapped individual experiences maximum constraint (d ≈ 0.95) but this is not extraction — it is the irreducible cost of being embodied. The institutional/arbitrage medical system experiences low constraint (d ≈ 0.15) because it can choose which sufferings to address, but it is not a beneficiary extracting from victims — it is a response mechanism to a natural law constraint. The analytical observer's d ≈ 0.72 reflects the observer's recognition that suffering is a universal constraint, not a position of extraction or benefit. All perspectives classify as mountain because the constraint exhibits zero degrees of freedom: no agent, at any power level, with any exit options, can eliminate suffering as such.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all constraints are extractive. The mandatrophy asks: 'Is this coordination or extraction?' For suffering-as-such, the answer is 'neither.' Suffering is not a coordination mechanism (it does not solve a collective action problem) and it is not extraction (no agent benefits from imposing it on others). It is a third category: a natural law constraint — an irreducible feature of the territory that all agents must navigate. The very low extractiveness (ε = 0.08) and minimal suppression (σ = 0.02) place it firmly in mountain territory, and the cross-cultural invariance, physiological substrate, and philosophical convergence confirm that this is not a false summit. The constraint's classification as mountain from all perspectives, including the analytical observer, indicates that this is a genuine structural limit, not a contingent institutional arrangement being naturalized. The mandatrophy is resolved by recognizing that the framework's primary purpose — distinguishing coordination from extraction — does not exhaust the space of constraints. Some constraints are neither, and the mountain category captures this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suffering_boundary_definition,
    'Where is the boundary between ontological suffering (mountain) and socially-contingent suffering (potentially extractive)?',
    'Cross-cultural anthropological analysis: which forms of suffering persist across all known social arrangements vs which correlate with specific institutional structures. Physiological substrate analysis: which suffering has irreducible biological basis vs which is mediated by social interpretation.',
    'If boundary is narrow (only physical pain, mortality): many forms of psychological and social suffering are potentially addressable through institutional change, and claims that they are ''inherent to existence'' are false summits. If boundary is broad (includes existential anxiety, meaning-crisis, relational loss): more suffering is ontological, and therapeutic/political projects aimed at elimination are structurally impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_boundary_definition, conceptual, 'Boundary between ontological and socially-contingent suffering').

omega_variable(
    transhumanist_exit_possibility,
    'Do radical life-extension or mind-uploading technologies constitute genuine exit from embodied suffering, or do they merely shift the constraint''s form?',
    'Empirical: if such technologies become available, longitudinal analysis of whether suffering persists in new substrate. Conceptual: whether suffering is tied to embodiment as such or to specific features of biological embodiment (aging, pain receptors, mortality).',
    'If genuine exit possible: suffering is not a true mountain but a scaffold with a very long sunset (civilizational-scale technological transition). If suffering persists in new forms: mountain classification confirmed — suffering is tied to finitude and limitation as such, not to biological substrate specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transhumanist_exit_possibility, empirical, 'Whether transhumanist technologies provide exit from suffering').

omega_variable(
    hedonic_adaptation_mechanism,
    'Is hedonic adaptation (the tendency to return to baseline happiness despite positive or negative events) itself a source of ontological suffering, or a neutral feature of consciousness?',
    'Neuroscientific analysis of hedonic set-point mechanisms; philosophical analysis of whether the gap between desired and actual hedonic state constitutes suffering or is merely the condition for motivation and goal-directed behavior.',
    'If hedonic adaptation is itself suffering-generative: the constraint''s extractiveness may be higher than measured (ε = 0.08 may underestimate). If neutral: current measurement stands. If hedonic adaptation is protective (prevents prolonged suffering): the constraint''s extractiveness may be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hedonic_adaptation_mechanism, conceptual, 'Whether hedonic adaptation generates or mitigates suffering').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suffering_ontology_mountain, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_ancient, suffering_ontology_mountain, theater_ratio, 0, 0.15).
narrative_ontology:measurement(theater_medieval, suffering_ontology_mountain, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(theater_modern, suffering_ontology_mountain, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(extract_ancient, suffering_ontology_mountain, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(extract_medieval, suffering_ontology_mountain, base_extractiveness, 1000, 0.08).
narrative_ontology:measurement(extract_modern, suffering_ontology_mountain, base_extractiveness, 2000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint models suffering-as-such (the presence of suffering in embodied existence) as distinct from specific sources of suffering (poverty, oppression, illness, loss), which may be contingent and thus classify as rope, tangled_rope, snare, or other types depending on their institutional structure. Specific suffering-source constraints would be downstream of this ontological constraint and would need separate constraint stories with their own epsilon values. For example: 'poverty_induced_suffering' (likely snare or tangled_rope, ε ≥ 0.40), 'medical_access_barriers' (likely tangled_rope, ε ≈ 0.35), 'grief_from_loss' (likely mountain, ε ≈ 0.10). The ontological constraint is the invariant substrate; the specific sources are the variable institutional layers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
