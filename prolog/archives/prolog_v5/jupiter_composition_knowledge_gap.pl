% ============================================================================
% CONSTRAINT STORY: jupiter_composition_knowledge_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jupiter_composition_knowledge_gap, []).

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
 *   constraint_id: jupiter_composition_knowledge_gap
 *   human_readable: Knowledge Gap in Jupiter's Composition Affecting Planetary Formation Models
 *   domain: planetary_science/technological_constraint
 *
 * SUMMARY:
 *   The precise composition of Jupiter's core — particularly the abundance of
 *   water, rocks, metals, and other heavy elements — remains ambiguous
 *   despite decades of observation. This knowledge gap constrains the
 *   development of planetary formation models because Jupiter serves as the
 *   reference standard for understanding how gas giants form: its
 *   mass-metallicity relationship, core size, and heavy-element distribution
 *   encode the formation history. The constraint exhibits hybrid behavior: it
 *   functions partly as pure coordination (scientists must work within shared
 *   observational limits) and partly as extraction (space agencies benefit
 *   from justified mission proposals while theorists bear the cost of model
 *   uncertainty). The theater ratio (0.55) reflects that much Jupiter science
 *   discussion involves reiterative modeling with unchanged observational
 *   constraints — theoretical work loops over parameter space without closing
 *   the knowledge gap. Over the measurement interval, extractiveness has
 *   increased modestly (0.28 to 0.38) as the number of exoplanet discoveries
 *   has risen, increasing pressure on the reference standard's precision.
 *   Theater has risen slightly as new model variants incorporate Jupiter data
 *   without reducing the underlying ambiguity.
 *
 * KEY AGENTS:
 *   - Planetary Formation Theory: Primary victim (powerless/trapped) — cannot exit constraint; theoretical models operate within bounds of composition uncertainty
 *   - Exoplanet Discovery Programs: Secondary victim (moderate/trapped) — need Jupiter as reference standard to classify super-Jupiters; trapped by uncertain baseline
 *   - Space Agencies (NASA, ESA, JAXA): Primary beneficiary (institutional/arbitrage) — knowledge gap justifies mission proposals and drives funding; benefit from discovery opportunities
 *   - Observational Astronomers: Secondary beneficiary (institutional/arbitrage) — Jupiter composition ambiguity creates publication opportunities and career advancement incentives
 *   - Instrument Development Consortium: Mixed actor (organized/constrained) — benefits from mission funding but constrained by technical barriers to deep-atmosphere measurement
 *   - Classical Formation Models (Nice, Grand Tack): Degraded actor (institutional/arbitrage) — maintained through inertia despite Jupiter composition assumptions being questioned
 *   - Analytical Observer: Theoretical vantage (analytical/analytical) — risks naturalizing institutional measurement choices as inherent physical limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jupiter_composition_knowledge_gap, 0.38).
domain_priors:suppression_score(jupiter_composition_knowledge_gap, 0.48).
domain_priors:theater_ratio(jupiter_composition_knowledge_gap, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, extractiveness, 0.38).
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jupiter_composition_knowledge_gap, tangled_rope).
narrative_ontology:human_readable(jupiter_composition_knowledge_gap, "Knowledge Gap in Jupiter's Composition Affecting Planetary Formation Models").
narrative_ontology:topic_domain(jupiter_composition_knowledge_gap, "planetary_science/technological_constraint").

domain_priors:requires_active_enforcement(jupiter_composition_knowledge_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jupiter_composition_knowledge_gap, space_agencies).
narrative_ontology:constraint_beneficiary(jupiter_composition_knowledge_gap, observational_astronomers).
narrative_ontology:constraint_victim(jupiter_composition_knowledge_gap, planetary_formation_theory).
narrative_ontology:constraint_victim(jupiter_composition_knowledge_gap, exoplanet_discovery_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLANETARY FORMATION THEORY (SNARE) — Cannot exit the constraint; fundamental models must operate within the bounds of Jupiter composition uncertainty. The theoretical framework is trapped by instrumental limitations that neither theorists nor modelers can circumvent. Each new exoplanet discovery adds pressure to resolve core composition ambiguity, but the epistemic commons bears the cost of degraded predictive power.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXOPLANET DISCOVERY PROGRAMS (SNARE) — Constrained by Jupiter's ambiguous composition when attempting to classify super-Jupiters and gas giants in other systems. Programs cannot build robust classification schemas without knowing Jupiter's core mass, metallicity profile, and internal structure. Trapped by the reference standard being uncertain.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SPACE AGENCIES AND OBSERVATIONAL ASTRONOMERS (ROPE) — Benefit from the knowledge gap: justifies mission proposals (JUICE, future deep-atmosphere probes), drives funding allocation, and creates opportunities for high-impact discoveries. The uncertainty itself generates scientific opportunity and resources. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTRUMENT DEVELOPMENT CONSORTIUM (TANGLED ROPE) — Benefits from mission funding and high-stakes measurement objectives (drives innovation in seismic detection, gravitational field mapping). But constrained by technical barriers: Jupiter's extreme pressure environment, radiation belts, and atmospheric opacity limit probe survivability and measurement precision. Mixed coordination and extraction — the problem enables technology development but the constraints are genuinely hard.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CLASSICAL PLANETARY SCIENCE MODELS (PITON) — Pre-existing formation models (Nice model, Grand Tack hypothesis) persist despite Jupiter composition uncertainty because alternatives haven't fully replaced them. The models are maintained through intellectual inertia — they are cited, refined, and incorporated into reviews even though their core assumptions about Jupiter's interior are increasingly questioned. Theater ratio reflects that model updates are often procedural extensions rather than fundamental rethinking.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LIMITS VIEW (MOUNTAIN) — From a universal perspective, Jupiter's internal composition is constrained by physics: equations of state, gravitational equilibrium, interior dynamics. Some aspects are inherent limits (you cannot perfectly know the core without direct measurement). However, the base extractiveness (0.38) contradicts a mountain classification — the constraint is substantially institutional (mission design choices, funding allocation) rather than purely physical. This perspective is a false summit.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jupiter_composition_knowledge_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(jupiter_composition_knowledge_gap, TR),
    TR >= 0.70.

:- end_tests(jupiter_composition_knowledge_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The core composition gap creates real constraints on model development (extraction cost to theorists) but also real scientific opportunity (benefit to agencies). The value reflects that space agencies benefit disproportionately during the uncertainty window, but the benefit is not purely extractive — new missions genuinely advance understanding. Suppression (0.48): Moderate. Technical barriers to deep-atmosphere measurement are real (extreme pressure, radiation, corrosive chemistry) but not absolute. Alternative constraints (gravity mapping, thermal evolution, comparative planetology) provide partial information, reducing suppression below total. Theater ratio (0.55): Moderate-high. Much Jupiter composition discussion involves model refinements with static observational inputs; papers often present parameter-space variations rather than new constraints. But some genuine observational work (Juno gravity field mapping, atmospheric spectroscopy) reduces theater below dominant levels. The modest increase over time (0.42 to 0.55) reflects growing model elaboration without proportional empirical progress.
 *
 * PERSPECTIVAL GAP:
 *   Planetary formation theory sees a pure extraction (Snare) — the knowledge gap is an absolute constraint on model quality. Space agencies see pure coordination (Rope) — the gap is a legitimate scientific problem justifying missions. Exoplanet programs see the gap as a reference-standard ambiguity (Snare) — their classification schemas are trapped by the baseline uncertainty. The instrument consortium sees mixed benefits and barriers (Tangled Rope) — technology development is enabled by mission stakes but constrained by physics. Classical models see their own institutional persistence (Piton) — the models continue through citation patterns and textbook inertia despite compositional assumptions being increasingly questioned. The analytical observer risks seeing an immutable physical limit (Mountain) — knowing Jupiter's interior perfectly is impossible without direct measurement — but the base extractiveness contradicts this. The constraint is substantially institutional (mission prioritization, funding allocation) rather than purely physical.
 *
 * DIRECTIONALITY LOGIC:
 *   Structurally, space agencies experience low directionality (d ≈ 0.15) because they are institutional beneficiaries with arbitrage options — they can fund alternative missions or redirect resources. Planetary formation theorists experience high directionality (d ≈ 0.90) because they are trapped victims with no exit option — their models must incorporate Jupiter data regardless. Exoplanet programs occupy a constrained middle position (d ≈ 0.65) — they need the reference standard but have some flexibility in applying uncertainty bounds to exoplanet inferences. The instrument consortium has moderate directionality (d ≈ 0.55) because the technical barriers are real but solvable with sufficient resources. The engine derives these automatically from beneficiary/victim declarations and exit options; no manual override is required.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by recognizing that the knowledge gap is simultaneously a legitimate scientific problem (Rope coordination function) and a platform for institutional benefit-extraction (agencies benefit from justified missions). The Tangled Rope classification captures both: there is genuine coordination work (theorists must share observational constraints, modelers must align with data), and there is genuine asymmetric extraction (space agencies accrue disproportionate benefit from the uncertainty window). The piton perspective reveals that classical models persist partly through inertia, indicating partial degradation of their original function. The false summit at the analytical level shows that naturalizing measurement uncertainty as inherent physics masks institutional choices about mission priority and funding. No single type is correct; the constraint's type varies by perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seismic_detection_feasibility,
    'Can acoustic oscillation modes in Jupiter''s interior be reliably detected and inverted to determine core composition, or are the modes damped below instrumental sensitivity?',
    'Direct measurement by atmospheric probes (Galileo-scale or better sensitivity); theoretical modeling of wave propagation in Jupiter''s heterogeneous interior; correlation with Saturn seismic data if available',
    'If detectable: knowledge gap closes within 10-15 years via dedicated seismic mission. If below sensitivity: alternative constraints (gravity, magnetism, thermal evolution) must bear the full inference burden, extending uncertainty indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seismic_detection_feasibility, empirical, 'Feasibility of detecting Jupiter seismic modes at instrumental sensitivity').

omega_variable(
    heavy_element_migration_pathway,
    'Did Jupiter''s heavy elements settle into a coherent core or distribute throughout the interior during formation? If distributed, how much was retained vs lost?',
    'High-precision interior density profile from gravity field mapping; metallicity measurements via atmospheric spectroscopy at depth; comparison with formation simulations across parameter space',
    'If coherent core: composition ambiguity is primarily about core size and density. If distributed: theoretical framework must account for turbulent mixing, and composition ambiguity extends throughout the interior. Changes the informational structure of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heavy_element_migration_pathway, empirical, 'Whether Jupiter''s heavy elements form coherent core or distributed interior').

omega_variable(
    exoplanet_formation_pathways_independence,
    'Are exoplanet formation pathways fundamentally different from Jupiter''s (requiring different models), or do they share core physics that demands Jupiter as reference standard?',
    'Comparative analysis of exoplanet population statistics (core mass distributions, migration signatures); chemical abundance patterns in young exoplanet systems; formation simulation results across metallicity regimes',
    'If pathways are independent: exoplanet programs can develop local models without Jupiter reference, reducing extraction. If coupled: Jupiter composition remains necessary benchmark, maintaining constraint structure indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exoplanet_formation_pathways_independence, conceptual, 'Whether exoplanet formation requires Jupiter as reference standard').

omega_variable(
    mission_cost_benefit_threshold,
    'What information gain threshold justifies a dedicated Jupiter composition mission (estimated 2-5 billion USD)? How much does the current knowledge gap reduce predictive power?',
    'Quantitative sensitivity analysis: how much does Jupiter composition uncertainty propagate into exoplanet classification error rates; cost-benefit comparison of dedicated mission vs incremental improvement from indirect constraints',
    'If high threshold: current gap may persist indefinitely (missions deprioritized). If low threshold: gap closes via near-term mission (JUICE gravity data, future probes). Determines whether constraint is structural or institutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_cost_benefit_threshold, preference, 'Cost-benefit justification for dedicated Jupiter composition measurement mission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jupiter_composition_knowledge_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jup_comp_tr_t0, jupiter_composition_knowledge_gap, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jup_comp_tr_t5, jupiter_composition_knowledge_gap, theater_ratio, 5, 0.48).
narrative_ontology:measurement(jup_comp_tr_t10, jupiter_composition_knowledge_gap, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(jup_comp_be_t0, jupiter_composition_knowledge_gap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jup_comp_be_t5, jupiter_composition_knowledge_gap, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(jup_comp_be_t10, jupiter_composition_knowledge_gap, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jupiter_composition_knowledge_gap, information_standard).
narrative_ontology:affects_constraint(jupiter_composition_knowledge_gap, exoplanet_mass_metallicity_relation).
narrative_ontology:affects_constraint(jupiter_composition_knowledge_gap, gas_giant_formation_core_accretion).
narrative_ontology:affects_constraint(jupiter_composition_knowledge_gap, planetary_migration_model_coupling).

% DUAL FORMULATION NOTE:
% Jupiter's composition ambiguity is upstream of exoplanet classification constraints. Formation models require Jupiter as calibration reference; without precise composition, downstream exoplanet inferences inherit uncertainty. The three downstream constraints share the same knowledge gap as a structural dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
