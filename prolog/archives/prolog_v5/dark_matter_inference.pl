% ============================================================================
% CONSTRAINT STORY: dark_matter_inference
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dark_matter_inference, []).

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
 *   constraint_id: dark_matter_inference
 *   human_readable: Dark Matter Inference as Institutional Constraint
 *   domain: cosmology/astrophysics/institutional_epistemology
 *
 * SUMMARY:
 *   Dark matter inference operates as a structural constraint on cosmological
 *   and astrophysical hypothesis space. The existence of gravitational
 *   phenomena inconsistent with visible matter (galactic rotation curves,
 *   cluster dynamics, CMB acoustic peaks, large-scale structure growth)
 *   requires explanation. The dark matter framework provides a unified
 *   account but has generated 50+ years of null experimental results and
 *   persistent observational anomalies. The constraint exhibits tangled rope
 *   characteristics: genuine coordination function (explaining multiple
 *   phenomena with one hypothesis) combined with asymmetric extraction
 *   (resource allocation favoring dark matter searches, publication bias
 *   against alternative gravity frameworks, suppression of alternative
 *   hypothesis space). The theater ratio indicates increasing performativity:
 *   particle detection experiments continue with refined sensitivity goals
 *   despite decades of null results; funding structures default to dark
 *   matter without proportional resources for alternatives. Alternative
 *   gravity researchers face career barriers and funding gatekeeping. Yet
 *   genuine coordination exists — dark matter provides a parsimonious
 *   framework across CMB, lensing, structure formation, and dynamics. The
 *   constraint is not pure extraction (snare) because the coordination
 *   function is real; nor is it pure coordination (rope) because the resource
 *   allocation is asymmetric and gatekeeping is structural.
 *
 * KEY AGENTS:
 *   - Dark Matter Research Establishment: Primary beneficiary (institutional/arbitrage) — controls funding allocation, publication prestige, first interpretation of anomalies; can pivot to alternatives if evidence warrants (arbitrage exit option)
 *   - Alternative Gravity Researchers: Primary victim (powerless/trapped) — face funding gatekeeping, publication bias, career risk; career paths constrained to peripheral institutions or topic switching
 *   - Observational Astronomers: Secondary victim (moderate/constrained) — benefit from dark matter framework for interpreting data but face friction in publishing alternative hypotheses or anomaly investigations; can work within framework with career cost for deviation
 *   - Precision Cosmology Coalition: Organized actors (organized/mobile) — collaborations pursuing next-generation surveys (JWST, Vera Rubin, CMB missions); empirical precision improvements will eventually enable distinguishing dark matter from modified gravity; sunset embedded in observational precision gain
 *   - Particle Dark Matter Detection: Institutional program (institutional/arbitrage) — experiments continue with goal-post displacement despite null results; maintains inertial theater rather than functional discovery
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the inferential gap as a law of nature rather than recognizing contingent institutional gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dark_matter_inference, 0.38).
domain_priors:suppression_score(dark_matter_inference, 0.52).
domain_priors:theater_ratio(dark_matter_inference, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dark_matter_inference, extractiveness, 0.38).
narrative_ontology:constraint_metric(dark_matter_inference, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(dark_matter_inference, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dark_matter_inference, tangled_rope).
narrative_ontology:human_readable(dark_matter_inference, "Dark Matter Inference as Institutional Constraint").
narrative_ontology:topic_domain(dark_matter_inference, "cosmology/astrophysics/institutional_epistemology").

domain_priors:requires_active_enforcement(dark_matter_inference).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dark_matter_inference, dark_matter_research_establishment).
narrative_ontology:constraint_beneficiary(dark_matter_inference, large_experimental_collaborations).
narrative_ontology:constraint_victim(dark_matter_inference, alternative_gravity_researchers).
narrative_ontology:constraint_victim(dark_matter_inference, observational_astronomy_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE GRAVITY RESEARCHER (SNARE) — Structurally trapped by funding gatekeeping and publication bias. Funding agencies and journal editors default to dark matter frameworks; MOND, emergent gravity, or modified dispersion relations face systematic skepticism. Career risk for pursuing alternatives is severe. Exit requires abandoning research program or relocating to peripheral institutions. Maximum experienced extraction — no coordination benefit from this agent's perspective.
constraint_indexing:constraint_classification(dark_matter_inference, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OBSERVATIONAL ASTRONOMER (TANGLED ROPE) — Constrained by telescope allocation committees and data interpretation frameworks that presume dark matter. Genuine coordination function exists: dark matter hypothesis enables unified description of rotation curves, CMB, large-scale structure. But asymmetric extraction occurs through resource allocation favoring dark matter searches over anomaly investigations. Some agency (can publish null results, can propose alternative hypotheses) but significant friction and career cost.
constraint_indexing:constraint_classification(dark_matter_inference, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DARK MATTER RESEARCH ESTABLISHMENT (ROPE) — Benefits from funding concentration, publication prestige, and first-interpretation authority over anomalies. Experiences the constraint as coordination: unified theoretical framework for multiple observational puzzles. Can pivot to alternative hypotheses if evidence accumulates (arbitrage option). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(dark_matter_inference, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRECISION COSMOLOGY COALITION (SCAFFOLD) — Organized actors (gravitational lensing surveys, CMB precision experiments, large-scale structure collaborations) see dark matter inference as a temporary coordination mechanism. As observational precision increases and systematic uncertainties shrink, the scope for alternative gravity models expands. The sunset clause is embedded in observational precision improvement: at sufficiently high signal-to-noise, distinguishing between dark matter and modified gravity becomes empirically tractable. Estimated sunset: 15-25 years as JWST, Vera Rubin Observatory, and next-generation surveys mature.
constraint_indexing:constraint_classification(dark_matter_inference, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PARTICLE DARK MATTER DETECTION (PITON) — Decades of null results from WIMPs, axions, and other particle searches have shifted the constraint toward performative theater. Experiments continue with refined sensitivity goals, but the underlying functional requirement (detecting dark matter particles directly) has not been satisfied despite enormous resource investment. The institutional commitment persists through research program inertia, not empirical success. Theater ratio reflects that experiment design, data analysis, and interpretation frameworks are increasingly optimized for continuing the program rather than resolving the central question.
constraint_indexing:constraint_classification(dark_matter_inference, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some inferential gap between observations and mechanism is inherent to physics: we cannot see dark matter directly; we infer its existence from gravitational effects. This gap is structural to the observational method itself. The mountain perspective naturalizes the inference process as an immutable feature of cosmological knowledge. However, the structural data (moderate extractiveness, high suppression, high theater ratio) contradicts the mountain classification — revealing that the 'inherent gap' framing naturalizes what is actually a contingent institutional constraint on hypothesis space.
constraint_indexing:constraint_classification(dark_matter_inference, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dark_matter_inference_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dark_matter_inference, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dark_matter_inference, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(dark_matter_inference, TR),
    TR >= 0.70.

:- end_tests(dark_matter_inference_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, reflecting mixed coordination and extraction. The dark matter framework genuinely solves multiple observational puzzles (CMB acoustic peaks, large-scale structure growth, cluster dynamics), justifying substantial resources. But the asymmetry is real: alternative frameworks receive disproportionately low resources and face publication friction. The 0.38 value reflects that roughly 60% of dark matter research represents legitimate hypothesis evaluation, while 40% represents extraction through resource gatekeeping and opportunity cost. Suppression (0.52): Moderate-high. Barriers include funding gatekeeping, publication bias in journals and preprint servers favoring dark matter interpretation, peer review skew against alternative hypotheses, career risk, and default framing in textbooks and training. But suppression is not absolute — some alternative gravity papers publish, some researchers maintain alternative programs, some funding exists for competing hypotheses. Theater ratio (0.68): High, reflecting performative components. Particle detection experiments have run for 50+ years with null results yet continue with refined sensitivity goals (goal-post displacement). Funding agency language frames dark matter as 'most promising' despite empirical neutrality. CMB constraints on dark matter properties are reinterpreted as supporting evidence rather than boundary conditions. The increase from 0.48 to 0.68 over the interval reflects that as empirical null results accumulate, the constraint relies increasingly on institutional inertia and framing rather than novel discovery.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical position determines experienced extractiveness. The dark matter establishment genuinely sees rope — they experience the constraint as coordination that unifies multiple phenomena, with beneficiary position giving them arbitrage options. Alternative researchers genuinely see snare — they experience the constraint as coercive gatekeeping with no exit without abandoning research identity. Observational astronomers genuinely see tangled rope — they use the dark matter framework productively but face friction when pursuing anomalies or alternatives. The precision cosmology coalition genuinely sees scaffold — they recognize that empirical precision improvements (JWST resolution of lensing, Vera Rubin weak lensing surveys, next-generation CMB missions) will eventually enable distinguishing dark matter from modified gravity, embedding a natural sunset clause. The particle detection program genuinely sees piton — they continue research with goal-post displacement and institutional inertia despite decades of null results. The analytical observer genuinely risks seeing mountain — naturalizing the inferential gap as an immutable feature of observational cosmology — but the structural data reveals this as a false summit. The perspectival gap exposes that the 'inherent gap' framing naturalizes what is actually contingent gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim status and exit options. The dark matter establishment occupies the institutional/arbitrage position: they control interpretation frameworks and can shift resources to alternatives if evidence changes (high exit option). This gives them low directionality (d ≈ 0.15), resulting in low or negative effective extraction from their perspective. Alternative researchers occupy the powerless/trapped position: they face career barriers, funding gatekeeping, and loss of research program if they exit the field entirely (low exit option). This gives them high directionality (d ≈ 0.90), resulting in high experienced extraction. Observational astronomers occupy the moderate/constrained position: they benefit from the dark matter framework for interpreting observations but face friction in pursuing alternatives (moderate exit cost). This gives them medium-high directionality (d ≈ 0.60), resulting in moderate experienced extraction. The analytical observer with analytical exit options has neither benefit nor vulnerability — they see the constraint structure itself (d ≈ 0.72 canonical), producing the characteristic moderate-high analytical extraction value that reveals structural asymmetries rather than experiencing them.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CLASSIFICATION RESOLVES MANDATROPHY: The dark matter inference constraint genuinely contains both coordination and asymmetric extraction. The coordination function is real and substantial: the dark matter hypothesis provides unified explanation for CMB acoustic peaks (sound wave damping by dark matter density), large-scale structure growth (gravitational amplification of dark matter density perturbations), galactic rotation curves (unseen mass), and cluster dynamics. Discarding this coordination would fragment cosmology back into disconnected phenomena. But the extraction is also real and substantial: resource allocation is asymmetric (dark matter receives ~90% of hypothesis-space funding versus ~10% for alternatives), publication bias favors dark matter interpretation, career paths constrain alternative research to peripheral positions, and funding gatekeeping prevents proportional resource allocation to competing hypotheses. Tangled rope classification requires both: genuine coordination function (✓ dark matter provides multi-scale unified framework) AND asymmetric extraction (✓ resource and opportunity cost allocation is structurally biased). The misclassification risk would be calling this 'pure coordination' (rope), which would ignore the real gatekeeping and career suppression affecting alternative researchers. The tangled rope classification holds both truths simultaneously: yes, dark matter is a powerful coordinating hypothesis; AND yes, the institutional arrangement asymmetrically benefits a particular research establishment while suppressing alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dark_matter_vs_modified_gravity_empirical_distinguishability,
    'At what observational precision threshold do dark matter and modified gravity hypotheses become empirically distinguishable rather than merely observationally equivalent?',
    'Analysis of degeneracies in lensing + dynamics + CMB + structure formation: identification of observables that break the degeneracy (e.g., higher-order lensing statistics, primordial tensor modes, galaxy-scale dynamics inconsistent with both CDM and MOD)',
    'If distinguishable within 10 years: dark matter inference constraint becomes scaffold with explicit sunset. If degeneracies remain: constraint may persist as tangled_rope (genuine coordination function alongside asymmetric resource allocation). If modified gravity is definitively ruled out: constraint becomes rope (pure coordination, no extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dark_matter_vs_modified_gravity_empirical_distinguishability, empirical, 'Empirical distinguishability of dark matter vs modified gravity hypotheses').

omega_variable(
    funding_gatekeeping_mechanism_intentionality,
    'Is funding bias against alternative gravity frameworks a deliberate gatekeeping mechanism or an emergent artifact of research consensus?',
    'Analysis of funding agency deliberations, grant reviewer demographics and citation patterns, historical comparison with paradigm shifts in other fields (plasma physics, fluid turbulence) where alternative frameworks gained resources after empirical challenges',
    'If deliberate gatekeeping: supports snare classification for trapped agents (institutional enforcement of hypothesis space). If emergent consensus artifact: supports tangled_rope classification (genuine coordination with unintended extraction effects). Affects assessment of whether suppression is structural or performative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(funding_gatekeeping_mechanism_intentionality, conceptual, 'Whether funding bias is deliberate gatekeeping or emergent consensus').

omega_variable(
    particle_detection_null_results_information_content,
    'Do the null results from direct dark matter detection experiments provide genuine information (ruling out parameter space) or primarily sustain the experimental program through goal-post displacement?',
    'Meta-analysis of published constraints: do successive experiments rule out non-overlapping parameter regions or do they repeatedly target the same regions with improved sensitivity? Historical tracking of predicted discovery timelines vs actual experimental schedules.',
    'If genuine information: particle detection program is rope (legitimate negative result coordination). If goal-post displacement: program is piton (inertial theater). Theater ratio interpretation depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(particle_detection_null_results_information_content, empirical, 'Information content of successive null results in dark matter detection').

omega_variable(
    observational_anomaly_real_or_calibration,
    'Are persistent observational anomalies (some galaxy rotation curves, some cluster dynamics, some lensing statistics) genuine physics anomalies or systematic measurement/calibration artifacts?',
    'Cross-instrument validation of anomalies, comparison with simulations including realistic systematics, high-resolution follow-up studies of anomalous systems',
    'If genuine anomalies: dark matter inference becomes snare for observers forced to accept framework that doesn''t fully explain observations. If systematic artifacts: dark matter inference becomes stronger rope (solves real observational puzzles). Affects directionality for observational astronomers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observational_anomaly_real_or_calibration, empirical, 'Whether observational anomalies are real or systematic artifacts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dark_matter_inference, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dm_tr_t0, dark_matter_inference, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dm_tr_t10, dark_matter_inference, theater_ratio, 10, 0.62).
narrative_ontology:measurement(dm_tr_t20, dark_matter_inference, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(dm_be_t0, dark_matter_inference, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dm_be_t10, dark_matter_inference, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(dm_be_t20, dark_matter_inference, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dark_matter_inference, information_standard).
narrative_ontology:affects_constraint(dark_matter_inference, galaxy_rotation_curve_interpretation).
narrative_ontology:affects_constraint(dark_matter_inference, cmb_acoustic_peak_physics).
narrative_ontology:affects_constraint(dark_matter_inference, large_scale_structure_growth_mechanisms).

% DUAL FORMULATION NOTE:
% Dark matter inference sits upstream of specific observational claims about galaxy clusters, CMB, and structure formation. Each downstream constraint has its own extractiveness reflecting the empirical status of specific phenomena; the inference constraint itself has extractiveness reflecting the institutional gatekeeping and resource allocation mechanisms around hypothesis space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
