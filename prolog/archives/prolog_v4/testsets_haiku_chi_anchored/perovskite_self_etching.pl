% ============================================================================
% CONSTRAINT STORY: perovskite_self_etching
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perovskite_self_etching, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perovskite_self_etching
 *   human_readable: The 2D Perovskite Machinability Constraint
 *   domain: technological/semiconductors
 *
 * SUMMARY:
 *   The 2D perovskite machinability constraint arises from the fundamental
 *   incompatibility between conventional semiconductor lithography techniques
 *   and the soft lead halide crystal lattice. Traditional approaches
 *   (photolithography with photoresist, electron beam patterning, reactive
 *   ion etching, and ion implantation) rely on harsh chemical solvents,
 *   energetic particle bombardment, and high temperatures — all of which
 *   damage or destroy perovskite films. This creates a structural extraction
 *   mechanism: perovskite researchers are locked into expensive,
 *   damage-inducing processing workflows, while vendors of conventional
 *   lithography equipment maintain high margins by capturing the entire
 *   perovskite research community regardless of material suitability. The
 *   constraint exhibits all six DR types depending on perspective: for
 *   researchers it is a Snare (trapped by incompatible processes); for
 *   vendors it is a Rope (pure coordination through market lock-in); for
 *   emerging soft-processing research it is Tangled Rope (mixed coordination
 *   and extraction); for the open-fab movement it is Scaffold (temporary with
 *   sunset); for semiconductor industry standards it is Piton (degraded
 *   ritual maintained by institutional inertia); and from an analytical view
 *   it appears as a false Mountain (naturalizing what is actually a
 *   contingent technological choice). The theater ratio (0.64) reflects that
 *   much of conventional lithography's dominance in the perovskite domain is
 *   performative — the implicit assumption that 'semiconductor processing
 *   means silicon-optimized tools' remains unquestioned despite being
 *   unsuitable for soft materials. Extractiveness (0.58) captures the
 *   sustained cost and performance penalty borne by researchers; suppression
 *   (0.68) reflects the lack of commercially viable alternatives and the
 *   institutional barriers to process switching.
 *
 * KEY AGENTS:
 *   - Perovskite research groups: Primary victims (powerless/trapped) — cannot escape damage-inducing conventional lithography within institutional budgets
 *   - Device yield and efficiency: Secondary victim (moderate/constrained) — suffer 40-60% performance degradation from damage; can invest in mitigation but not escape constraint
 *   - Conventional lithography vendors: Primary beneficiary (institutional/arbitrage) — capture sustained demand for expensive equipment; market lock-in enables high margins despite material unsuitability
 *   - Emerging soft-material processing research: Organized secondary actor (organized/constrained) — developing gentler alternatives (self-etching, selective dissolution, laser ablation); constrained by lack of commercial tooling and publication bias
 *   - Semiconductor industry standards bodies: Institutional secondary actor (institutional/arbitrage) — maintain and propagate silicon-optimized processing as universal best practice; see conventional lithography as inherent rather than contingent
 *   - Open-source soft-fab movement: Organized alternative pathway (organized/mobile) — building low-cost damage-minimal workflows in academic/startup settings with explicit sunset logic (5-10 year maturation timeline)
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks falsely naturalizing material incompatibility as physical law rather than technological design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perovskite_self_etching, 0.58).
domain_priors:suppression_score(perovskite_self_etching, 0.68).
domain_priors:theater_ratio(perovskite_self_etching, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perovskite_self_etching, extractiveness, 0.58).
narrative_ontology:constraint_metric(perovskite_self_etching, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(perovskite_self_etching, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perovskite_self_etching, tangled_rope).
narrative_ontology:human_readable(perovskite_self_etching, "The 2D Perovskite Machinability Constraint").
narrative_ontology:topic_domain(perovskite_self_etching, "technological/semiconductors").

domain_priors:requires_active_enforcement(perovskite_self_etching).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perovskite_self_etching, conventional_lithography_vendors).
narrative_ontology:constraint_beneficiary(perovskite_self_etching, semiconductor_equipment_manufacturers).
narrative_ontology:constraint_victim(perovskite_self_etching, perovskite_researchers).
narrative_ontology:constraint_victim(perovskite_self_etching, device_yield_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEROVSKITE RESEARCH GROUP (SNARE) — Trapped by material incompatibility. Traditional lithography (photoresist, electron beam, ion implantation) damages the soft lead halide lattice, causing defects and drastically reducing device performance. No viable alternative manufacturing pathway exists within institutional budgets. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(perovskite_self_etching, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVICE YIELD AND EFFICIENCY (SNARE) — Constrained by physics and tooling limits. Perovskite devices fabricated with conventional lithography suffer 40-60% performance degradation due to damage-induced defects. Researchers can invest in damage mitigation but cannot escape the constraint without switching materials or developing new processes. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(perovskite_self_etching, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONVENTIONAL LITHOGRAPHY VENDORS (ROPE) — Benefit from sustained demand and high margins on equipment and services. The perovskite market's incompatibility with soft materials reinforces their market lock-in: researchers must purchase expensive photoresist, e-beam tools, and reactive ion etching systems regardless of damage. Vendors experience the constraint as pure coordination: 'This is how semiconductor processing works.' d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(perovskite_self_etching, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMERGING SOFT-MATERIAL PROCESSING RESEARCH (TANGLED ROPE) — Organized research communities (materials chemistry, soft electronics, MEMS) are developing gentle processing alternatives: self-etching nanowires, selective-dissolution patterning, laser-assisted ablation, and ionic-liquid-based lift-off. These methods reduce damage and achieve comparable feature resolution. However, adoption is constrained by lack of commercial tooling, unproven scalability at device-level, and publication bias toward conventional approaches. d≈0.48, f(d)≈0.62, σ=1.1 → χ≈0.39.
constraint_indexing:constraint_classification(perovskite_self_etching, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SEMICONDUCTOR INDUSTRY STANDARDS (PITON) — Conventional lithography dominates through institutional inertia, not optimal function. Photoresist, E-beam, RIE, and ion implantation are well-established, taught in every semiconductor engineering program, and embedded in facility design. Equipment manufacturers have decades of optimization investment. Theater ratio 0.64 reflects that much of the 'best practice' rhetoric around conventional lithography is performative: the assumption that this is the only viable path has become unquestioned. The standard persists because the economic switching costs are high, not because alternatives have been fairly evaluated.
constraint_indexing:constraint_classification(perovskite_self_etching, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN-SOURCE SOFT-FAB MOVEMENT (SCAFFOLD) — Academic and startup ecosystems (Kirchhoff Institute, Cambridge Graphene Center, soft-robotics labs) are building low-cost, damage-minimal alternative processing workflows. These alternatives exhibit coordination function (sharing methods, collaborative troubleshooting) with explicit sunset logic: as soft-material processing matures in the next 5-10 years, researchers will have viable off-the-shelf alternatives to conventional lithography. d≈0.38, f(d)≈0.38, σ=1.1 → χ≈0.27.
constraint_indexing:constraint_classification(perovskite_self_etching, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — From a civilizational view, material incompatibility (hard processes + soft materials = damage) appears immutable: 'Lithography requires harsh chemicals and energies; perovskites are soft; therefore incompatibility is inherent.' This naturalizes what is actually a contingent historical fact: conventional lithography was optimized for rigid silicon, not soft materials. The structural data (ε=0.58, suppression=0.68, theater=0.64) reveals this as a false natural law. The constraint is technological (process architecture) not fundamental (physics).
constraint_indexing:constraint_classification(perovskite_self_etching, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perovskite_self_etching_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(perovskite_self_etching, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perovskite_self_etching, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(perovskite_self_etching, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(perovskite_self_etching, TR),
    TR >= 0.70.

:- end_tests(perovskite_self_etching_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting sustained material damage and economic costs. Perovskite researchers incur significant expenses for conventional equipment unsuitable for their material platform, and they lose 40-60% device performance to damage. However, extractiveness is not maximal (0.70+) because some workarounds exist (damage mitigation strategies, gentler techniques at smaller scales), and the constraint is recognized as a problem rather than accepted as natural. The trajectory from 0.35 to 0.58 over 15 years reflects increasing severity as perovskite research matured and the damage problem became more apparent. Suppression (0.68): Moderate-high. Barriers to alternative processing include lack of commercial tooling, high switching costs, publication bias favoring conventional approaches, and institutional inertia in semiconductor curricula and facility design. However, suppression is not total because academic labs can develop custom gentle-processing workflows, and early-stage alternatives have demonstrated proof-of-concept. Theater ratio (0.64): Moderate-high. Much of conventional lithography's dominance in perovskite domain is performative: the rhetoric of 'best practice' and 'industry standard' is sustained even though the techniques cause material-specific damage. The 'we must use conventional lithography because it is semiconductor best practice' narrative has become unquestioned. Theater has increased over time as the performance degradation became more obvious yet the industry response remained to 'engineer around' the damage rather than question the process choice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival disagreement on classification type and severity. Perovskite researchers experience pure extraction (Snare): they are forced to use inappropriate tools at high cost with severe performance penalties and no exit option within institutional constraints. Conventional lithography vendors experience pure coordination (Rope): they view themselves as providing standard industry solutions, with no awareness of material-specific unsuitability. Emerging soft-processing research sees mixed coordination and extraction (Tangled Rope): there is genuine value in developing gentler methods and the research community is organized, but commercial barriers and publication bias constrain adoption. The open-fab movement sees a temporary problem with a real sunset (Scaffold): they anticipate that in 5-10 years, mature alternatives will make conventional lithography obsolete for soft materials. Industry standards bodies see institutional best practice (Piton): the constraint is maintained through unquestioned assumption and switching costs, not because it works. The analytical observer sees false universality (false Mountain): naturalizing material incompatibility as physical law rather than a contingent design choice. The perspectival gap reveals that 'conventional lithography' is not a solution — it is a tool optimized for a different problem (rigid silicon patterning) being forced onto a problem it was never designed to solve.
 *
 * DIRECTIONALITY LOGIC:
 *   Perovskite research groups: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction. Researchers cannot exit the constraint without abandoning perovskites entirely. Device yield and efficiency: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction; some mitigation is possible but the fundamental incompatibility remains. Conventional lithography vendors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Negative effective extraction (net beneficiary) — vendors can freely switch to new market opportunities and face no constraint. Emerging soft-material processing research: Mixed (organized/constrained) → d≈0.48, f(d)≈0.62. Moderate extraction due to institutional barriers and publication bias, but organized capacity and real alternatives reduce severity. Open-source soft-fab movement: Mixed (organized/mobile) → d≈0.38, f(d)≈0.38. Lower extraction because the movement has agency, developing pathways themselves with visible sunset. Semiconductor industry standards: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (≥0.70), not from high χ. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False Mountain requires the engine's false summit detector to identify naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is TECHNOLOGICAL, not NATURAL. The apparent contradiction — 'Is perovskite machinability inherent to the material or a contingent design choice?' — is dissolved by understanding that conventional lithography is optimized for silicon rigidity, not perovskite softness. The natural-law framing ('material incompatibility is inherent') falsely universalizes what is actually a narrow design choice. The correct classification (Tangled Rope with Snare from researcher perspective, Scaffold from alternatives perspective) reveals that the constraint is institutional, not material-physical. If we measured perovskite compatibility with soft-processing techniques (self-etching, selective dissolution, laser ablation) rather than conventional lithography, the extractiveness would drop significantly and the constraint would shift toward Rope or Scaffold. This is the critical insight: the constraint is not about perovskite properties, but about tool-material mismatch. The mandatrophy prevention rule is satisfied by explicitly declaring beneficiaries (lithography vendors) and victims (perovskite researchers), which reveals the extraction mechanism (market lock-in) rather than naturalizing it as physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_etching_selectivity,
    'Can self-etching mechanisms achieve sub-100nm feature resolution and aspect ratios comparable to conventional lithography while preserving perovskite lattice integrity?',
    'Experimental demonstration of patterned perovskite devices (solar cells, LEDs, photodetectors) with self-etching or gentle processing; comparison of feature precision and device performance against conventional lithography baseline',
    'If yes with <5% performance loss: Scaffold perspective confirmed, sunset timeline is 5-10 years. If no or >20% loss: Scaffold is aspirational, constraint persists as Snare/Tangled Rope for foreseeable future.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_etching_selectivity, empirical, 'Whether self-etching achieves resolution and selectivity parity with conventional lithography').

omega_variable(
    commercial_tooling_availability,
    'Will equipment manufacturers (Applied Materials, ASML, Coventor) commercialize soft-material processing tools, or will adoption remain confined to academic/startup workflows?',
    'Market surveys; equipment vendor R&D announcements and product roadmaps; adoption rates in device manufacturing facilities over next 3-5 years',
    'If commercialized: constraint shifts from Snare to Tangled Rope/Scaffold (institutional support for alternatives). If not commercialized: researchers remain trapped (Snare persists), theater ratio continues to rise as conventional tools are used despite their unsuitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_tooling_availability, empirical, 'Whether commercial semiconductor equipment vendors will develop soft-material processing tools').

omega_variable(
    material_platform_lock_in,
    'Are perovskites the optimal semiconductor platform for soft electronics, or are they over-invested due to historical preference for halide materials?',
    'Comparative device performance analysis: perovskites via gentle processing vs alternative soft semiconductors (organic conjugated polymers, 2D TMDs, natural biopolymers) using conventional processing',
    'If perovskites remain superior: research must solve machinability constraint. If alternatives are competitive: constraint becomes a technology-switching problem, not an inherent limitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_platform_lock_in, empirical, 'Whether perovskites remain the optimal soft-semiconductor platform').

omega_variable(
    performance_damage_thresholds,
    'What is the true damage threshold for conventional lithography on perovskites — is 40-60% performance loss unavoidable, or can damage mitigation strategies reduce this to <10% with proper protocols?',
    'Systematic study of photoresist type, energy density, etch chemistry, and post-process annealing on perovskite device performance; statistical analysis of damage mechanisms',
    'If mitigation reduces loss to <10%: constraint becomes lower-severity Tangled Rope (manageable with engineering). If threshold is inherent: constraint remains high-severity Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_damage_thresholds, empirical, 'Whether performance damage from conventional lithography is avoidable through damage mitigation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perovskite_self_etching, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pero_tr_t0, perovskite_self_etching, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pero_tr_t7, perovskite_self_etching, theater_ratio, 7, 0.56).
narrative_ontology:measurement(pero_tr_t15, perovskite_self_etching, theater_ratio, 15, 0.64).

% Extraction over time
narrative_ontology:measurement(pero_be_t0, perovskite_self_etching, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pero_be_t7, perovskite_self_etching, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(pero_be_t15, perovskite_self_etching, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perovskite_self_etching, enforcement_mechanism).
narrative_ontology:affects_constraint(perovskite_self_etching, perovskite_stability_window).
narrative_ontology:affects_constraint(perovskite_self_etching, soft_semiconductor_capital_access).
narrative_ontology:affects_constraint(perovskite_self_etching, lead_halide_toxicity_regulation).

% DUAL FORMULATION NOTE:
% The machinability constraint is downstream of broader soft-semiconductor material platform choices (perovskite vs organics vs 2D materials) but represents a distinct structural constraint: process incompatibility. The upstream stability and capital-access constraints set the perovskite adoption landscape; the machinability constraint determines whether perovskite devices can be manufactured profitably.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(perovskite_self_etching, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
