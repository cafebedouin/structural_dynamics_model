% ============================================================================
% CONSTRAINT STORY: boiling_frog_syndrome
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boiling_frog_syndrome, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: boiling_frog_syndrome
 *   human_readable: Boiling Frog Syndrome: Incremental Extraction Masked by Gradualism
 *   domain: social/institutional/cognitive
 *
 * SUMMARY:
 *   Boiling frog syndrome describes a structural extraction mechanism where
 *   cumulative costs are imposed in increments small enough that each
 *   increment avoids triggering collective resistance, yet large enough over
 *   time to achieve substantial aggregate extraction. The constraint exhibits
 *   a characteristic pattern: extracted value increases monotonically;
 *   collective awareness lags extraction by multiple increments; intermediate
 *   institutions become complicit by normalizing each new baseline;
 *   measurement systems degrade through baseline drift; and exit options
 *   atrophy as targets become progressively more dependent on the system.
 *   This is the signature of tangled rope: genuine coordination functions
 *   exist (actors adapt to incrementally changing conditions), but extraction
 *   mechanisms are active (systematic asymmetric imposition of costs). The
 *   constraint operates across domains — labor regulation, environmental
 *   standards, healthcare benefits, housing cost burdens, tax obligations —
 *   wherever institutional actors can control the pace and framing of
 *   changes.
 *
 * KEY AGENTS:
 *   - Target Population: Primary victim (powerless/trapped) — bears cumulative extraction but cannot organize effective resistance because each increment appears tolerable in isolation; baseline normalization prevents perception of aggregate change
 *   - Extraction Beneficiary: Primary beneficiary (institutional/arbitrage) — controls increment timing, framing, and justification; experiences the constraint as workable coordination mechanism; has arbitrage options to alternative extraction methods
 *   - Intermediate Institutions: Complicit actor (moderate/constrained) — mediates between beneficiary and target; initially perceives coordination function; gradually becomes aware of extraction mechanism but faces institutional and resource costs to resistance
 *   - Reform Movements: Organized resistance (organized/mobile) — perceive the pattern explicitly; build threshold-based triggers and accountability mechanisms; see the constraint as temporary and reversible
 *   - Legacy Measurement Systems: Institutional degradation (institutional/arbitrage) — perform measurement and notification functions while normalizing the extracted state into the baseline; theater increases as systems sustain the illusion of stasis
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks misclassifying the constraint as a cognitive/natural limit rather than an enforceable institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boiling_frog_syndrome, 0.58).
domain_priors:suppression_score(boiling_frog_syndrome, 0.65).
domain_priors:theater_ratio(boiling_frog_syndrome, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boiling_frog_syndrome, extractiveness, 0.58).
narrative_ontology:constraint_metric(boiling_frog_syndrome, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(boiling_frog_syndrome, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boiling_frog_syndrome, tangled_rope).
narrative_ontology:human_readable(boiling_frog_syndrome, "Boiling Frog Syndrome: Incremental Extraction Masked by Gradualism").
narrative_ontology:topic_domain(boiling_frog_syndrome, "social/institutional/cognitive").

domain_priors:requires_active_enforcement(boiling_frog_syndrome).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boiling_frog_syndrome, extraction_beneficiary).
narrative_ontology:constraint_beneficiary(boiling_frog_syndrome, institutional_inertia_maintainer).
narrative_ontology:constraint_victim(boiling_frog_syndrome, target_population).
narrative_ontology:constraint_victim(boiling_frog_syndrome, institutional_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET POPULATION (SNARE) — The frog experiences cumulative extraction without perceiving crisis because each increment is tolerable in isolation. Suppression is internalized through normalization of the new baseline after each increment. The agent cannot organize effective resistance because the problem is distributed across time rather than concentrated in a single moment. Exit becomes progressively harder as alternatives atrophy.
constraint_indexing:constraint_classification(boiling_frog_syndrome, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INTERMEDIATE INSTITUTIONS (TANGLED ROPE) — Organizations intermediate between beneficiary and target experience the constraint as containing both coordination functions (adaptation to incrementally changing environment) and extraction (enforcement of successive 'new normals'). These institutions are constrained by resource requirements and institutional norms but retain some capacity to organize collective response. They perceive the mechanism but face costs to exit or resistance.
constraint_indexing:constraint_classification(boiling_frog_syndrome, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXTRACTION BENEFICIARY (ROPE) — The institutional actor or power center coordinating the incremental extractions experiences the constraint as pure coordination: systematically raising expectations and extraction amounts while maintaining plausible deniability creates a workable equilibrium. Benefits from the constraint flow directly to this agent. Arbitrage options allow exit or substitution to alternative extraction mechanisms at will.
constraint_indexing:constraint_classification(boiling_frog_syndrome, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM MOVEMENTS (SCAFFOLD) — Organized resistance actors (unions, advocacy groups, social movements) perceive the boiling frog mechanism and build explicit sunset structures: accountability mechanisms, threshold-based triggers, reversibility mandates. Their classification reflects that boiling frog syndrome is a temporary coordination failure with a recognizable path to termination. The coalition has agency and exit paths.
constraint_indexing:constraint_classification(boiling_frog_syndrome, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY MEASUREMENT SYSTEMS (PITON) — Historical baseline-setting mechanisms and notification thresholds degrade over time as they normalize the new extracted state into the baseline. The measurement apparatus itself becomes part of the extraction mechanism — it sustains the illusion of stasis by reporting changes relative to the immediately preceding state rather than absolute historical decline. Theater ratio remains high as the system performs measurement functions it no longer serves.
constraint_indexing:constraint_classification(boiling_frog_syndrome, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, boiling frog syndrome appears as an immutable cognitive/perceptual constraint: human perception is built for detecting discontinuous changes, not gradual drifts. The constraint emerges from how minds process information. This perspective risks naturalizing a contingent institutional arrangement (systematic incremental extraction) as an inherent cognitive limit. The engine's false summit detector will flag this as misclassification.
constraint_indexing:constraint_classification(boiling_frog_syndrome, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boiling_frog_syndrome_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boiling_frog_syndrome, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boiling_frog_syndrome, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boiling_frog_syndrome, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(boiling_frog_syndrome, TR),
    TR >= 0.70.

:- end_tests(boiling_frog_syndrome_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting substantial aggregate extraction achieved through incremental imposition. The value is elevated from the initial state (0.15) because cumulative extraction approaches the threshold of perceptibility. The mechanism is less severe than a snare would be (≥0.66) because intermediate institutions retain some capacity to organize resistance and some extractions serve legitimate coordination functions. Suppression (0.65): High. Suppression operates through normalization of baselines, control over measurement and notification systems, framing each increment as isolated and inevitable, and degradation of exit options. Suppression is not total (targets retain some perception of change and some alternative options exist) but is substantial enough to prevent organized collective action until the aggregate extraction becomes undeniable. Theater ratio (0.68): High and increasing. Measurement systems perform the function of transparency while actually obscuring historical change through baseline drift. Communication about incremental changes frames each as necessary and proportionate. Institutions perform the appearance of deliberation and justification while systematically denying the aggregate pattern.
 *
 * PERSPECTIVAL GAP:
 *   Boiling frog syndrome creates a maximal perspectival gap between the beneficiary (who sees coordination and optimization) and the target population (who experience extraction but cannot perceive it collectively). Intermediate institutions are caught in the gap: they recognize both functions but face costs to resistance. The analytical observer's risk is misclassifying the entire constraint as a cognitive/natural law because human perception does have genuine limits to drift detection. However, the structural data contradicts this: (a) organized reform movements recognize the pattern explicitly, (b) boiling frog requires active suppression (control over baselines, notification systems, framing) to function, and (c) the pattern is disrupted when institutional actors withdraw cooperation or when explicit trigger mechanisms are imposed. These factors identify the constraint as institutional, not cognitive.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (institutional/arbitrage) derives low d ≈ 0.15-0.20: they benefit from the constraint and can exit or substitute alternative extraction methods at will. Effective extraction experienced by the beneficiary is therefore damped. Intermediate institutions (moderate/constrained) derive medium d ≈ 0.55-0.65: they face resource costs and institutional constraints in resisting but retain some exit options. Reform movements (organized/mobile) derive lower d ≈ 0.40-0.50: they are organized agents with clear exit paths (movement success, institutional change). The target population (powerless/trapped) derives high d ≈ 0.90-0.95: they bear extraction with minimal exit options. The directionality chain shows how the same constraint produces radically different experienced extractiveness: beneficiaries see rope, targets see snare, organized groups see scaffold, intermediate institutions see tangled rope. No agent sees the constraint the way the analytical observer initially describes it (as a cognitive mountain), because the structural asymmetries are plainly visible from every position except the false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   Boiling frog syndrome resolves the mandatrophy by clarifying the relationship between incremental coordination and systematic extraction. The constraint is genuinely tangled: it coordinates adaptive responses to changing conditions (rope function), but the coordination is asymmetrically imposed (extraction function). The mandatrophy question is 'how much coordination overhead versus how much systematic rent extraction?' The measurements show that as time proceeds, the theater ratio increases (measurement systems degrade) and extractiveness increases (more extraction is achieved). This trajectory is diagnostic of mandatrophy: as the system persists, the extraction component grows while the coordination component either stays constant or becomes a cover story. The snare classification from the target's perspective is the true structural state: pure extraction mechanism with suppressed alternatives. The rope and scaffold perspectives are partial truths — genuine coordination functions exist, but they are subordinated to extraction. The piton perspective reveals institutional degradation: measurement systems that once served transparency now serve concealment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_awareness_gap,
    'What increment size and frequency combine with baseline normalization to prevent collective awareness of cumulative extraction?',
    'Empirical measurement across domains: labor conditions, environmental regulations, contract terms, social benefits. Identify increment thresholds below which individual notification occurs but collective action does not; above which both occur.',
    'If threshold is cognitive (universal): boiling frog is a mountain and countermeasures require artificial external trigger mechanisms. If threshold is institutional (artifact of control over measurement baselines): boiling frog is a snare and countermeasures can disable the normalization mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_awareness_gap, empirical, 'Threshold increment size preventing collective awareness').

omega_variable(
    baseline_normalization_mechanism,
    'Is baseline drift (updating reference points to current state) a cognitive inevitability or an enforced institutional practice?',
    'Cross-cultural and historical analysis: societies with explicit absolute baselines vs relative baselines; intervention studies where baselines are held constant or disclosed across time.',
    'If inevitability: boiling frog is a mountain-like constraint on human perception. If practice: it is an enforceable institutional norm and its discontinuation is structurally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseline_normalization_mechanism, conceptual, 'Whether baseline normalization is cognitive or institutional').

omega_variable(
    exit_option_decay,
    'Do incremental extractions structurally decay target agents'' exit options (financial depletion, skill atrophy, identity lock), or do agents retain mobile options but fail to exercise them?',
    'Comparative analysis of exit outcomes: agents who recognized the pattern early and departed vs those remaining; cost assessment for departure at different points in the extraction timeline; identity-lock interviews vs material barrier analysis.',
    'If decay is structural: targets transition from constrained to trapped over the interval, making late intervention increasingly costly. If psychological: targets retain exit options despite believing otherwise; cost of departure is lower than subjectively perceived.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_decay, empirical, 'Whether exit option decay is structural or psychological').

omega_variable(
    coordinated_beneficiary_behavior,
    'Is the systematic increment pattern (timing, magnitude, framing) evidence of coordinated extraction strategy, or does it emerge from independent institutional pressures that happen to aggregate into a boiling pattern?',
    'Institutional genealogy: trace decision-making records, communications, and intent statements from beneficiary actors. Test whether pattern persists if explicit coordination is disrupted.',
    'If coordinated: snare classification is strengthened (intentional extraction mechanism). If emergent: tangled rope classification is more accurate (structural asymmetry without explicit control).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordinated_beneficiary_behavior, empirical, 'Whether increment pattern reflects coordinated extraction strategy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boiling_frog_syndrome, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bfs_tr_t0, boiling_frog_syndrome, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bfs_tr_t3, boiling_frog_syndrome, theater_ratio, 3, 0.48).
narrative_ontology:measurement(bfs_tr_t6, boiling_frog_syndrome, theater_ratio, 6, 0.62).
narrative_ontology:measurement(bfs_tr_t10, boiling_frog_syndrome, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(bfs_be_t0, boiling_frog_syndrome, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bfs_be_t3, boiling_frog_syndrome, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(bfs_be_t6, boiling_frog_syndrome, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(bfs_be_t10, boiling_frog_syndrome, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boiling_frog_syndrome, resource_allocation).
narrative_ontology:affects_constraint(boiling_frog_syndrome, baseline_normalization_institutional_drift).
narrative_ontology:affects_constraint(boiling_frog_syndrome, incremental_entrapment_mechanism).
narrative_ontology:affects_constraint(boiling_frog_syndrome, collective_action_suppression_through_gradualism).

% DUAL FORMULATION NOTE:
% Boiling frog syndrome is often presented as a cognitive constraint (humans cannot perceive gradual change). This story models it as an institutional constraint enforced through control of baselines, measurement systems, and increment timing. Related constraint family: the cognitive limits story, if written separately, would have lower extractiveness (ε ≈ 0.20, mountain-like) and would focus on perceptual thresholds. This story focuses on the institutional enforcement mechanisms that exploit cognitive limits — the extractiveness is higher because suppression is active, not passive. Both stories affect each other: institutional exploiters benefit from cognitive limits; cognitive limits are amplified when institutions control the measurement baseline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
