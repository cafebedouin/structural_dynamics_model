% ============================================================================
% CONSTRAINT STORY: noncentrosymmetric_asoc_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_noncentrosymmetric_asoc_coupling, []).

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
 *   constraint_id: noncentrosymmetric_asoc_coupling
 *   human_readable: Noncentrosymmetric Spin-Orbit Coupling and Asymmetric Band Structure Claims
 *   domain: condensed_matter_physics/materials_science
 *
 * SUMMARY:
 *   Noncentrosymmetric spin-orbit coupling (ASOC) represents a rich
 *   coordination-extraction hybrid in condensed matter physics. The
 *   phenomenon — asymmetric band splitting in materials lacking inversion
 *   symmetry — is theoretically sound and fundamentally important. Yet the
 *   empirical verification landscape exhibits classic tangled-rope dynamics:
 *   genuine scientific coordination (investigating ASOC materials is
 *   genuinely valuable) combined with asymmetric extraction (ASOC claims
 *   generate proliferating measurement requirements while the primary
 *   beneficiaries capture citation priority). The constraint emerges from the
 *   gap between theoretical tractability and experimental verification
 *   difficulty. ASOC calculations are elegant and numerically clean; ASOC
 *   measurements are sensitive to sample quality, measurement protocol, and
 *   calibration. This mismatch drives the extraction mechanism: theorists and
 *   early experimentalists benefit from the theory's clarity and rigor, while
 *   verification communities bear the burden of measurement proliferation and
 *   inconsistent results. The theater_ratio (0.65) reflects both genuine
 *   scientific content and performative element — much published discussion
 *   celebrates the theoretical structure and conceptual beauty of ASOC
 *   without robust experimental grounding, and measurement choices are often
 *   influenced by publication incentives rather than scientific necessity.
 *
 * KEY AGENTS:
 *   - Noncentrosymmetric materials research groups: Primary beneficiaries (institutional/arbitrage) — capture priority through novel ASOC claims and measurements; experience constraint as pure coordination
 *   - Experimental verification community: Primary victim (powerless/trapped) — unable to exit the measurement consensus problem; bear accumulating replication burdens
 *   - Competing experimental groups: Secondary victim (moderate/constrained) — face resource barriers and publication competition; also benefit from ASOC research ecosystem
 *   - Theoretical prediction community: Mixed role (institutional/constrained) — benefits from ASOC theory driving experiments but bears costs of explaining anomalies and failed predictions
 *   - Materials database and standardization initiatives: Organized actors (organized/mobile) — arXiv, Materials Project, NOMAD building alternative pathways with declining theater
 *   - Traditional materials theory pedagogy: Institutional actor (institutional/arbitrage) — maintains ASOC in textbooks through inertia; sees own pedagogical function as degraded (Piton perspective)
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing measurement consensus problem as intrinsic to physics rather than coordination failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(noncentrosymmetric_asoc_coupling, 0.38).
domain_priors:suppression_score(noncentrosymmetric_asoc_coupling, 0.48).
domain_priors:theater_ratio(noncentrosymmetric_asoc_coupling, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, extractiveness, 0.38).
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(noncentrosymmetric_asoc_coupling, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(noncentrosymmetric_asoc_coupling, tangled_rope).
narrative_ontology:human_readable(noncentrosymmetric_asoc_coupling, "Noncentrosymmetric Spin-Orbit Coupling and Asymmetric Band Structure Claims").
narrative_ontology:topic_domain(noncentrosymmetric_asoc_coupling, "condensed_matter_physics/materials_science").

domain_priors:requires_active_enforcement(noncentrosymmetric_asoc_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(noncentrosymmetric_asoc_coupling, noncentrosymmetric_materials_research_groups).
narrative_ontology:constraint_beneficiary(noncentrosymmetric_asoc_coupling, theoretical_prediction_community).
narrative_ontology:constraint_victim(noncentrosymmetric_asoc_coupling, experimental_verification_resources).
narrative_ontology:constraint_victim(noncentrosymmetric_asoc_coupling, field_measurement_consensus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPERIMENTAL VERIFICATION COMMUNITY (SNARE) — Trapped in the measurement consensus problem. Cannot exit without abandoning the measurement protocols themselves. Asymmetric coupling claims generate proliferation of required measurements (ARPES, STM, X-ray diffraction variants) with inconsistent results across experimental groups. No mechanism exists to coordinate on which measurements are dispositive.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING EXPERIMENTAL GROUPS (TANGLED ROPE) — Constrained by equipment costs and publication competition. Genuine coordination function exists: ASOC materials are interesting and investigating them does advance the field. But asymmetric extraction occurs through measurement proliferation — groups that publish novel ASOC claims impose heavy replication burdens on competitors while capturing priority. Some groups benefit from early claims; others incur replication costs.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NONCENTROSYMMETRIC MATERIALS RESEARCH GROUPS (ROPE) — Primary beneficiaries with arbitrage options. ASOC predictions and measurements generate publications, citations, and funding. They experience the constraint as pure coordination: designing noncentrosymmetric materials and measuring their properties is the research itself. The measurement proliferation they drive is experienced as methodological thoroughness, not extraction.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THEORETICAL PREDICTION COMMUNITY (TANGLED ROPE) — Institutionally constrained but benefits from the coordination logic of materials discovery. ASOC band structure predictions drive experimental programs. But theorists also bear extraction costs: unpredicted experimental signatures require theoretical elaboration, and failed predictions can damage reputational standing. The relationship is genuinely mixed — coordination and asymmetric extraction coexist.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MATERIALS DATABASE AND STANDARDIZATION INITIATIVES (SCAFFOLD) — Organized actors (ICSD, Materials Project, NOMAD) have agency and see a sunset: standardized measurement protocols, open crystal structure databases, and machine-learning prediction pipelines are building alternative pathways for ASOC discovery that bypass proliferating single-group measurements. Theater is declining as data standardization increases verification transparency.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL MATERIALS THEORY PEDAGOGY (PITON) — Noncentrosymmetric spin-orbit coupling is taught as a fundamental phenomenon, but its empirical verification remains contested. Textbooks and courses persist in presenting ASOC as settled because it makes pedagogical sense and because alternatives haven't fully replaced it, not because the underlying claims are robustly verified. Theater ratio reflects that much ASOC discussion in literature is about the theoretical beauty and conceptual clarity of the phenomenon, not about its empirical reality in specific materials.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scope, spin-orbit coupling is a fundamental relativistic effect in solid-state physics — it must emerge whenever heavy elements with strong nuclear charge couple orbital and spin angular momentum. ASOC is not optional; it is inherent to the physics. The question is not whether it exists, but how strongly it manifests in specific materials. However, this mountain classification risks naturalizing the measurement consensus problem as intrinsic to the physics, when it is actually a coordination failure.
constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(noncentrosymmetric_asoc_coupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(noncentrosymmetric_asoc_coupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(noncentrosymmetric_asoc_coupling, TR),
    TR >= 0.70.

:- end_tests(noncentrosymmetric_asoc_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The ASOC claim involves genuine scientific content — spin-orbit coupling is real and noncentrosymmetric materials are interesting. But the extractiveness reflects that early ASOC claims, while scientifically justified, impose measurement verification burdens on the field that exceed proportional benefit. The constraint started lower (0.28) and has increased to 0.38 as measurement proliferation has accumulated. This is below the 0.46 threshold for high-extraction constraints, reflecting genuine coordination function alongside extraction. Suppression (0.48): Moderate. Barriers to independent ASOC verification include specialized equipment (ARPES, STM, neutron/X-ray beamlines), sample preparation tacit knowledge, interpretation ambiguity (many mechanisms can produce band splitting), and publication bias. But barriers are surmountable — multiple groups have independent ASOC measurement capability. Suppression is structural but not total. Theater ratio (0.65): Moderate-high. Much ASOC discussion celebrates theoretical elegance and conceptual clarity without robust experimental grounding. Measurement choices are often driven by publication incentives (novel signatures get cited more than confirmatory measurements). The ritual of ASOC claims followed by replication attempts and measurement proliferation has performative elements. Theater has increased from 0.52 to 0.65 as the field has become more specialized and early enthusiasm has outpaced empirical verification.
 *
 * PERSPECTIVAL GAP:
 *   The widest gap is between noncentrosymmetric research groups (Rope) and experimental verification communities (Snare). The primary beneficiaries experience the constraint as pure coordination — designing and measuring noncentrosymmetric materials is the research itself, and the measurement requirements they generate feel like natural scientific thoroughness. The verification communities trapped in measurement consensus see the same activities as extraction — ASOC claims impose cumulative measurement burdens while primary beneficiaries capture priority. This gap reflects real differences in structural position: beneficiaries have exit options (can switch research topics); victims have trapped exit (cannot abandon materials characterization). The gap is not resolvable by persuasion — it reflects genuine structural asymmetry in the constraint's extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status, power level, and exit options. Noncentrosymmetric research groups are primary beneficiaries (institutional power, arbitrage exit) — they capture priority and funding benefit from ASOC claims. The theoretical prediction community benefits institutionally but faces constrained exit through career dependence on prediction accuracy. Competing experimental groups are moderate victims with constrained exit — they face resource costs of replication without proportional benefit. The experimental verification community is a powerless victim with trapped exit — they cannot leave without abandoning the field itself. Materials database initiatives have organized power and mobile options — they can build alternative verification pathways. The constraint's directionality flow is clearly toward the primary beneficiaries and away from trapped verification communities.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint shows why mandatrophy matters. The ASOC claim is simultaneously genuine scientific coordination (investigating spin-orbit coupling advances materials science) and asymmetric extraction (measurement proliferation imposes unequal burdens). The mandatrophy would ask: 'Is ASOC primarily a coordination mechanism that happens to impose costs, or is it primarily an extraction mechanism dressed in coordination language?' The perspectival evidence resolves this: beneficiaries genuinely experience coordination (they describe measurement proliferation as scientific thoroughness); victims genuinely experience extraction (they are trapped in verification cycles). Both perspectives are empirically true because they reflect real structural differences. The tangled rope classification captures this hybrid by requiring BOTH a genuine coordination function (ASOC materials are scientifically interesting and worth studying) AND asymmetric extraction (primary beneficiaries capture disproportionate benefit). The mandatrophy is resolved by showing that the constraint genuinely coordinates while genuinely extracting — not a natural law (Mountain) and not pure extraction (Snare), but a hybrid that requires active enforcement (measurement standards, verification protocols, funding mechanisms) to maintain its equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_protocol_convergence,
    'Will experimental groups converge on a standard set of ASOC measurements, or will the proliferation continue indefinitely?',
    'Historical tracking of published measurement protocols for canonical ASOC materials (WeO₂, CaW₂O₆, Y₂Mo₂O₇); comparison of measurement count and diversity over 5-year intervals',
    'If convergence occurs: suppression drops, the constraint shifts toward Rope. If proliferation continues: suppression remains high, extraction mechanism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_protocol_convergence, empirical, 'Whether measurement protocols for ASOC characterization converge or proliferate').

omega_variable(
    spin_orbit_coupling_signature_uniqueness,
    'Is there a set of experimental signatures uniquely diagnostic of ASOC, or do multiple mechanisms produce similar observables?',
    'Systematic comparison of ARPES line shapes, spin-resolved photoemission, scanning tunneling spectroscopy, and transport measurements across confirmed ASOC and non-ASOC materials; machine learning classification of experimental signatures to assess separability',
    'If unique signature exists: measurement proliferation is coordination failure (suppression addressable). If signatures degenerate: measurement proliferation reflects genuine physical ambiguity (suppression structural).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spin_orbit_coupling_signature_uniqueness, empirical, 'Whether ASOC has a unique experimental signature or degenerate with other mechanisms').

omega_variable(
    sample_dependent_variability,
    'How much of the ASOC signal variation across groups derives from genuine sample differences versus experimental artifact and protocol variance?',
    'Round-robin measurements of identical samples across independent experimental groups; correlation analysis of measured ASOC strength against known structural parameters and impurity content; sensitivity analysis of experimental signatures to measurement temperature, surface condition, and detector calibration',
    'If mostly sample-dependent: measurement proliferation is necessary (Rope classification rises). If mostly artifact and protocol variance: standardization would resolve suppression (Scaffold sunset is real).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sample_dependent_variability, empirical, 'Proportion of ASOC variability from samples vs experimental protocol').

omega_variable(
    theoretical_prediction_accuracy_asymmetry,
    'Are theoretical ASOC predictions better at explaining existing data than predicting new phenomena?',
    'Historical comparison of published ASOC predictions (pre-2020) with subsequent experimental outcomes; classification of outcomes as ''explained existing data'' vs ''predicted new discovery''; statistical test for asymmetry in predictive success rates',
    'If predictions perform well prospectively: theory-experiment coordination is genuine (Rope). If predictions only explain retrospectively: extraction via prediction-then-measurement sequence is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theoretical_prediction_accuracy_asymmetry, empirical, 'Whether ASOC theories predict new phenomena or explain existing data retrospectively').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(noncentrosymmetric_asoc_coupling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ncsoc_tr_t0, noncentrosymmetric_asoc_coupling, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ncsoc_tr_t5, noncentrosymmetric_asoc_coupling, theater_ratio, 5, 0.61).
narrative_ontology:measurement(ncsoc_tr_t10, noncentrosymmetric_asoc_coupling, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(ncsoc_be_t0, noncentrosymmetric_asoc_coupling, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ncsoc_be_t5, noncentrosymmetric_asoc_coupling, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(ncsoc_be_t10, noncentrosymmetric_asoc_coupling, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(noncentrosymmetric_asoc_coupling, information_standard).
narrative_ontology:affects_constraint(noncentrosymmetric_asoc_coupling, verification_bottleneck).
narrative_ontology:affects_constraint(noncentrosymmetric_asoc_coupling, inverse_spin_valve_signature).

% DUAL FORMULATION NOTE:
% Noncentrosymmetric ASOC coupling is downstream of both the general verification bottleneck in quantum materials discovery (shares measurement coordination challenges) and peer-to-peer claims about specific spin-orbit phenomena. This story models the ASOC constraint's own extractiveness (0.38); upstream constraints model general verification problems (0.40) and specific competing ASOC claims (0.35-0.45 range).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
