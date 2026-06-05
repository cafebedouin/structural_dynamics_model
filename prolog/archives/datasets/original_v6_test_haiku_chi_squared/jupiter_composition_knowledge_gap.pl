% ============================================================================
% CONSTRAINT STORY: jupiter_composition_knowledge_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: planetary_science/exoplanet_discovery
 *
 * SUMMARY:
 *   The precise composition of Jupiter's core — particularly the abundance of
 *   heavy elements (metals, ices, silicates) relative to the hydrogen-helium
 *   envelope — remains uncertain to within ±1-2 Earth masses despite two
 *   decades of spacecraft observations and gravitational moment analysis.
 *   This knowledge gap creates a structural constraint on planetary formation
 *   theory: all models of how giant planets assemble must be calibrated
 *   against Jupiter, yet that calibration point is fuzzy. The constraint
 *   manifests as a hybrid of coordination (Jupiter-focused missions
 *   legitimately require funding) and extraction (researchers dependent on
 *   formation models are forced to work with imprecise boundary conditions).
 *   The theater_ratio reflects increasing reliance on model refinement and
 *   parameter tuning rather than new observational constraints. As
 *   alternative formation pathways (pebble accretion, streaming instability)
 *   mature, the constraint's enforcement power weakens — models that need
 *   less Jupiter-specific input grow competitive with classical core
 *   accretion approaches.
 *
 * KEY AGENTS:
 *   - Jupiter Observation Community: Primary beneficiary (institutional/arbitrage) — Juno mission and proposed future probes maintain funding and institutional priority through knowledge gap
 *   - Exoplanet Survey Community: Primary victim (powerless/trapped) — Cannot validate formation models without Jupiter composition constraints; all discoveries filtered through uncertain calibration
 *   - Comparative Planetology Groups: Secondary victim (moderate/constrained) — Bear costs of model uncertainty while partially benefiting from coordinated observation effort
 *   - Machine Learning Researchers: Organized responders (organized/mobile) — Building uncertainty quantification frameworks that create workaround pathways
 *   - Classical Formation Theory Community: Institutional maintainers (institutional/arbitrage) — Textbook models persist despite acknowledged limitations; maintains research momentum
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — Risks treating measurement uncertainty as natural law rather than contingent institutional constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jupiter_composition_knowledge_gap, 0.38).
domain_priors:suppression_score(jupiter_composition_knowledge_gap, 0.62).
domain_priors:theater_ratio(jupiter_composition_knowledge_gap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, extractiveness, 0.38).
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jupiter_composition_knowledge_gap, tangled_rope).
narrative_ontology:human_readable(jupiter_composition_knowledge_gap, "Knowledge Gap in Jupiter's Composition Affecting Planetary Formation Models").
narrative_ontology:topic_domain(jupiter_composition_knowledge_gap, "planetary_science/exoplanet_discovery").

domain_priors:requires_active_enforcement(jupiter_composition_knowledge_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jupiter_composition_knowledge_gap, jupiter_observation_community).
narrative_ontology:constraint_beneficiary(jupiter_composition_knowledge_gap, formation_model_developers).
narrative_ontology:constraint_victim(jupiter_composition_knowledge_gap, exoplanet_discovery_accuracy).
narrative_ontology:constraint_victim(jupiter_composition_knowledge_gap, comparative_planetology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXOPLANET SURVEY COMMUNITY (SNARE) — Cannot exit Jupiter composition uncertainty without abandoning planet formation model validation. Trapped by dependence on the knowledge gap. All exoplanet discoveries are filtered through imperfect formation models calibrated on Jupiter's unknown properties. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPARATIVE PLANETOLOGY GROUPS (TANGLED ROPE) — Constrained by Jupiter composition data limitations but benefit from observational effort focused on Jupiter. The knowledge gap both enables (novel research directions) and constrains (model calibration) their work. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUPITER OBSERVATION PROGRAMS (ROPE) — Beneficiary from the knowledge gap; maintains funding and research priority for Jupiter-focused missions (Juno, future missions). The gap creates legitimate scientific demand for observation coordination. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary through first-mover advantage in Jupiter science.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MACHINE LEARNING ENSEMBLE STRATEGIES (SCAFFOLD) — Organized response building uncertainty quantification frameworks and ensemble model approaches that work around composition knowledge gaps through probabilistic inference. These methods create a temporary coordination mechanism with decreasing reliance on precise Jupiter parameters. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.15.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLASSICAL PIECEWISE FORMATION THEORY (PITON) — Traditional core accretion + migration models persist despite acknowledged Jupiter composition uncertainty. The models are maintained through institutional inertia: textbooks teach them, funding follows, journal acceptance follows. theater_ratio≈0.58 reflects that much Jupiter formation literature is refined re-derivation of known models rather than novel constraint application. The theoretical framework sees itself as degraded but continues because alternatives haven't achieved parity.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, the composition knowledge gap might appear as an inherent measurement limitation — Jupiter's deep interior is inaccessible, so uncertainty is a natural law of our observational capacity. However, structural data (ε=0.38, suppression=0.62, theater=0.58) contradicts mountain classification. The constraint is contingent on institutional practices (which missions get funded, which models get refined) rather than immutable.
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
    constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.38): Moderate. The knowledge gap extracts constraints on exoplanet model validation, but the extraction is not maximal because: (1) alternative formation theories (pebble accretion) reduce dependence on Jupiter-specific inputs; (2) observational uncertainty is genuine, not manufactured; (3) Juno and future missions show incremental progress. The value increased from 0.22 (10 years ago) as the field's reliance on Jupiter calibration deepened and alternative pathways were slower to mature than expected. Suppression (0.62): Moderate-high. Jupiter's interior is fundamentally difficult to observe; deep interior pressure-temperature regimes cannot be directly probed. Publication bias favors papers that claim progress even when composition remains uncertain. Budget competition makes alternative approaches harder to fund. But suppression is not total — distributed spectroscopy, magnetic field analysis, and gravity moments have all advanced. Theater ratio (0.58): Moderate-high. Significant portion of formation model literature is parameter refinement rather than new constraint discovery. Classical theory textbooks are taught despite known composition uncertainty. But theater is not dominant — observational programs (Juno, proposed Galileo mission) conduct genuine scientific work with clear measurement goals.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits four distinct DR types reflecting genuine structural conflicts. Jupiter observation programs (Rope) see coordination — they are legitimately solving the problem of measuring Jupiter properties. The exoplanet community (Snare) sees pure extraction — they are forced to use imprecise boundary conditions they cannot independently verify. Comparative planetology (Tangled Rope) sees both — the observation effort enables their work but the uncertainty constrains it. Machine learning approaches (Scaffold) see a temporary problem with a technical solution — uncertainty quantification and ensemble methods progressively reduce the constraint's bite. Classical theory (Piton) sees its own degradation — the models persist through momentum, not function. The analytical observer (Mountain) risks naturalizing what is contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Jupiter observation programs: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Knowledge gaps justify continued funding. Exoplanet community: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit validation framework. Comparative planetology: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but some workarounds available (alternative observation strategies). Machine learning approaches: Organized + mobile → d≈0.35, f(d)≈0.32. Low extraction; these actors have agency and can develop technical solutions. Classical theory maintainers: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification derives from theater gate (0.58 ≥ piton floor), not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit detector applies.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Jupiter composition knowledge gap is genuinely tangled: it has a real coordination function (legitimate scientific value of observing Jupiter) AND real extraction (researchers downstream forced to work with uncertain inputs). The constraint avoids false classification as pure coordination (Rope) because the costs are asymmetric — those who benefit from Jupiter funding are not the same as those who bear the model validation costs. It avoids false classification as pure extraction (Snare) because Jupiter observation has genuine scientific merit, not manufactured complexity. The four distinct types across perspectives reveal that the extraction is not inherent to physical reality but to institutional structure: alternative formation theories (pebble accretion, streaming instability) progressively reduce extractive force as they mature. The theater increase over the 10-year interval (0.35 → 0.58) indicates that model refinement is increasingly substituting for new observational constraints — a classic sign of institutional inertia maintaining a constraint past its functional peak.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_composition_measurement_feasibility,
    'Can Jupiter''s core composition be measured to within ±0.1 Earth masses precision through gravitational moment analysis alone, or does measurement always require interior structure assumptions?',
    'Future gravity field measurements from Galileo mission; comparison of moment-based estimates with independent seismic constraints if available',
    'If yes: knowledge gap is engineering problem (Scaffold with defined sunset). If no: uncertainty is intrinsic to measurement (Mountain). Current field consensus leans toward no, supporting tangled_rope/snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(core_composition_measurement_feasibility, empirical, 'Whether core composition can be directly measured or requires model assumptions').

omega_variable(
    model_sensitivity_to_composition_variance,
    'How much does exoplanet migration distance prediction change when Jupiter composition uncertainty range is propagated through core accretion models?',
    'Global sensitivity analysis on formation models; Monte Carlo propagation of composition uncertainty; comparison to observational scatter in exoplanet orbits',
    'If sensitivity ≤ 10%: knowledge gap is low-extraction constraint (Rope). If sensitivity ≥ 30%: gap is high-extraction constraint (Snare/Tangled Rope). Current estimates suggest 15-25% sensitivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_sensitivity_to_composition_variance, empirical, 'Model sensitivity to Jupiter composition uncertainty').

omega_variable(
    alternative_formation_pathway_sufficiency,
    'Do pebble accretion and streaming instability models constrain Jupiter''s formation sufficiently without precise core composition knowledge?',
    'Comparative model testing on exoplanet population statistics; test whether alternative formation theory predictions require Jupiter composition input',
    'If yes: knowledge gap becomes local to classical models (constraint decomposes). If no: gap is fundamental to all formation frameworks (confirms tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_formation_pathway_sufficiency, empirical, 'Whether alternative formation theories bypass composition knowledge requirements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jupiter_composition_knowledge_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jckg_tr_t0, jupiter_composition_knowledge_gap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jckg_tr_t5, jupiter_composition_knowledge_gap, theater_ratio, 5, 0.48).
narrative_ontology:measurement(jckg_tr_t10, jupiter_composition_knowledge_gap, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(jckg_be_t0, jupiter_composition_knowledge_gap, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jckg_be_t5, jupiter_composition_knowledge_gap, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(jckg_be_t10, jupiter_composition_knowledge_gap, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jupiter_composition_knowledge_gap, information_standard).
narrative_ontology:affects_constraint(jupiter_composition_knowledge_gap, exoplanet_migration_model_uncertainty).
narrative_ontology:affects_constraint(jupiter_composition_knowledge_gap, giant_planet_formation_parity).

% DUAL FORMULATION NOTE:
% The Jupiter composition knowledge gap decomposes into two structurally distinct constraints: (1) measurement uncertainty in gravitational moment analysis (mountain-like, ε≈0.12), and (2) institutional dependence on Jupiter calibration (tangled_rope, ε≈0.38). The composition gap itself is measured; the constraint here is the downstream extraction caused by model calibration dependence. These are linked: progress on measurement reduces the extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
