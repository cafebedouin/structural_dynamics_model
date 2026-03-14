% ============================================================================
% CONSTRAINT STORY: majorana_fermion_engineering_constraints
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_majorana_fermion_engineering_constraints, []).

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
 *   constraint_id: majorana_fermion_engineering_constraints
 *   human_readable: Majorana Fermion Engineering Constraints
 *   domain: condensed_matter_physics/topological_quantum_computing
 *
 * SUMMARY:
 *   Majorana fermion engineering in topological quantum computing systems
 *   exhibits a complex constraint structure where physical necessity,
 *   institutional capital concentration, and knowledge gatekeeping are
 *   structurally entangled. The constraint operates at the intersection of
 *   fundamental physics exploration and commercialized quantum computing
 *   development. A Majorana zero-mode system requires: nanometer-scale
 *   fabrication precision in hybrid superconductor-nanowire or topological
 *   insulator structures, cryogenic operation typically below 50 mK,
 *   specialized measurement apparatus costing millions of dollars, and years
 *   of accumulated tacit knowledge in fabrication protocols. This genuine
 *   physical complexity creates legitimate barriers to independent
 *   replication, but these barriers are amplified by institutional IP
 *   protection, equipment monopolization by major research institutions, and
 *   funding concentration in established labs. The constraint manifests
 *   differently across the research ecosystem: established institutions
 *   experience it as a coordination mechanism ensuring their technological
 *   leadership; competing research groups experience it as a trap preventing
 *   market entry; the abstract field reproducibility bears the cost of
 *   unverified claims; and open collaboration networks attempt to build
 *   alternative pathways through standardized protocols and shared
 *   fabrication knowledge. The theater ratio (0.65) reflects peer review's
 *   degraded verification capacity for experimental systems with specialized
 *   requirements — reviewers cannot assess fabrication quality, apparatus
 *   calibration, or measurement fidelity from manuscripts alone.
 *
 * KEY AGENTS:
 *   - Major Research Institutions (Institutional/Arbitrage): Primary beneficiaries — capture prestige, funding concentration, graduate recruitment advantages from pioneering position in Majorana field
 *   - Topological Quantum Computing Companies (Institutional/Arbitrage): Secondary beneficiaries — create technology moat and competitive advantage through engineering complexity and IP protection
 *   - Competing Research Groups (Moderate/Trapped): Primary victims — face multi-year development timelines and millions in equipment investment with no guarantee of reproducibility; capital barriers prevent entry
 *   - Field Reproducibility (Powerless/Trapped): Secondary victim — abstract epistemic commons bears cost of unverified zero-mode claims without mechanism for self-correction
 *   - Open Collaboration Networks (Organized/Constrained): Organized agents attempting to reduce barriers through shared fabrication protocols, standardized measurements, and collaborative consortia; constrained by institutional resistance
 *   - Traditional Physics Review System (Institutional/Arbitrage): Maintains performative peer review despite degraded verification function for complex experimental systems
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing institutional arrangement as law of physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(majorana_fermion_engineering_constraints, 0.58).
domain_priors:suppression_score(majorana_fermion_engineering_constraints, 0.68).
domain_priors:theater_ratio(majorana_fermion_engineering_constraints, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(majorana_fermion_engineering_constraints, extractiveness, 0.58).
narrative_ontology:constraint_metric(majorana_fermion_engineering_constraints, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(majorana_fermion_engineering_constraints, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(majorana_fermion_engineering_constraints, tangled_rope).
narrative_ontology:human_readable(majorana_fermion_engineering_constraints, "Majorana Fermion Engineering Constraints").
narrative_ontology:topic_domain(majorana_fermion_engineering_constraints, "condensed_matter_physics/topological_quantum_computing").

domain_priors:requires_active_enforcement(majorana_fermion_engineering_constraints).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(majorana_fermion_engineering_constraints, topological_quantum_computing_companies).
narrative_ontology:constraint_beneficiary(majorana_fermion_engineering_constraints, major_research_institutions).
narrative_ontology:constraint_victim(majorana_fermion_engineering_constraints, field_reproducibility).
narrative_ontology:constraint_victim(majorana_fermion_engineering_constraints, competing_research_groups).
narrative_ontology:constraint_victim(majorana_fermion_engineering_constraints, experimental_resource_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD REPRODUCIBILITY (SNARE) — Cannot exit the engineering bottleneck; bears full cost of irreproducible claims and false positives. Abstract epistemic commons has no advocate, no exit option, and no means to organize. Experiences maximum extraction.
constraint_indexing:constraint_classification(majorana_fermion_engineering_constraints, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING RESEARCH GROUPS (SNARE) — Trapped by capital requirements, specialized equipment scarcity, and necessity of proprietary fabrication knowledge. Cannot replicate without years of development and millions in equipment investment. High suppression and no genuine exit option below career destruction.
constraint_indexing:constraint_classification(majorana_fermion_engineering_constraints, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: QUANTUM COMPUTING INDUSTRY (TANGLED ROPE) — Benefits from technological moat created by engineering complexity (coordination benefit: sustained competitive advantage). Simultaneously extracts through IP capture and knowledge gatekeeping. Mobile exit options (can pivot to alternative topological platforms) but benefits from current constraint. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(majorana_fermion_engineering_constraints, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: MAJOR RESEARCH INSTITUTIONS (ROPE) — Experience constraint as pure coordination mechanism: complex engineering enables prestige and funding concentration. First-mover institutions capture priority and resources. Arbitrage options (can shift to different topological systems) but benefit from established position in Majorana ecosystem. Net beneficiary through institutional prestige and funding advantage.
constraint_indexing:constraint_classification(majorana_fermion_engineering_constraints, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN COLLABORATION NETWORKS (TANGLED ROPE) — Organized agents (open-source topological design repositories, standardized measurement protocols, collaborative consortia) benefit from increased verification accessibility and shared fabrication knowledge (genuine coordination). But constrained by resource scarcity and resistance from institutions protecting competitive advantages. Both coordination and extraction present.
constraint_indexing:constraint_classification(majorana_fermion_engineering_constraints, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL PHYSICS REVIEW SYSTEM (PITON) — Peer review for Majorana engineering claims is substantially performative. Reviewers cannot verify nanofabrication quality, cryogenic apparatus calibration, or measurement protocols from manuscripts alone. Review ritual persists through institutional inertia despite degraded verification function. High theater ratio; low genuine verification function.
constraint_indexing:constraint_classification(majorana_fermion_engineering_constraints, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, engineering complexity for Majorana systems creates inherent verification lag: exotic quantum phenomena always require specialized equipment and deep expertise. Gap between claim and confirmation appears inevitable. However, structural data reveals this as false summit — the bottleneck is contingent institutional arrangement (capital concentration, IP protection), not law of nature.
constraint_indexing:constraint_classification(majorana_fermion_engineering_constraints, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(majorana_fermion_engineering_constraints_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(majorana_fermion_engineering_constraints, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(majorana_fermion_engineering_constraints, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(majorana_fermion_engineering_constraints, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(majorana_fermion_engineering_constraints, TR),
    TR >= 0.70.

:- end_tests(majorana_fermion_engineering_constraints_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Majorana systems involve genuine physical complexity requiring years of development and millions in capital. However, the measured extractiveness reflects not just legitimate physical barriers but also institutional amplification: IP concentration, funding gatekeeping, and deliberate obscuration of fabrication knowledge. The trajectory from 0.38 to 0.58 over 12 time units shows increasing extractiveness as institutional mechanisms strengthen relative to open-science alternatives. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) Physical — specialized equipment scarcity, nanofabrication expertise concentration. (2) Institutional — IP protection restricting access to protocols. (3) Economic — capital barriers exceeding typical research group budgets. (4) Epistemic — tacit knowledge barriers and measurement protocol obscuration. Theater ratio (0.65): Moderate-high and increasing. Peer review for Majorana claims faces structural verification limits: reviewers cannot validate nanofabrication quality, cryogenic apparatus function, or measurement calibration from papers. The ritual persists (journal acceptance/rejection) despite degraded verification. Theater has increased as measurement complexity outpaced reviewer capacity. This is not peer review failure — it is inherent to the domain — but it creates space for unverified claims.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence. Established institutions (Rope/Institutional) see a coordination problem being solved correctly — their expertise and capital enable leading-edge research. Competing research groups (Snare/Moderate) see a trap — years of work, millions of capital, with high probability of reproducing known results or confirming false positives. The field's epistemic health (Snare/Powerless) sees contamination — zero-mode claims accumulate faster than braiding experiments confirm topological protection. Open collaborators (Tangled Rope/Organized) see both a problem and emerging solutions — standardized protocols and shared fabrication knowledge reducing barriers, but constrained by institutional opposition. The review system (Piton/Institutional) sees its own degraded function — theater persisting through inertia. The analytical observer risks seeing natural law (Mountain/Analytical) — quantum computing is just hard — but the structural data reveals contingent institutional choices: selective IP protection, equipment monopolization, and funding concentration amplifying legitimate physical barriers. If the field adopted open fabrication repositories, shared cryogenic facilities, and collaborative measurement protocols, the legitimate physical barriers would remain, but the institutional extraction would dissolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional analysis reveals structural extraction amplifying physical complexity. Beneficiary institutions (major labs, quantum computing companies) derive d ≈ 0.15-0.25 from arbitrage exit options and institutional positioning: they can shift to alternative platforms or commercialize current advantages. Their effective extractiveness (χ) is negative or near-zero — they experience the constraint as enabling their position. Competing groups (moderate power/trapped exit) derive d ≈ 0.85-0.95: they are structurally targeted, face insurmountable capital barriers, and cannot pivot without abandoning the field. Their experienced extractiveness is maximum. The field's epistemic reliability (powerless/trapped) derives d = 1.0: pure target, no exit, full extraction. Organized open collaborators (organized/constrained) derive d ≈ 0.55: they have some agency and some benefits (access to collaborative knowledge) but face resource constraints and institutional opposition. The divergence in d values across perspectives (from -0.10 for institutional beneficiaries to 1.0 for powerless field) produces the perspectival gap: same physical constraint, opposite structural experiences.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all Tangled Rope criteria: (1) Genuine coordination function: legitimate physical complexity in Majorana engineering enables prestige-based leadership and technological advancement for capable institutions. (2) Asymmetric extraction: competing groups and field reproducibility bear costs disproportionate to benefits; extraction runs toward established institutions. (3) Active enforcement: IP protection, equipment monopolization, and knowledge gatekeeping actively maintain the asymmetry — without these mechanisms, open-science alternatives would bypass the constraint. The mandatrophy is resolved by demonstrating that the constraint is NOT pure extraction (Snare) despite its severity — institutional beneficiaries do genuinely coordinate advances in topological quantum computing, and the system does produce research value. However, it is also NOT pure coordination (Rope) — the extraction mechanisms (IP, equipment gatekeeping, tacit knowledge obscuration) are not incidental to the coordination function but essential to maintaining institutional advantage. The Tangled Rope classification correctly captures that both functions are structurally necessary to the constraint's operation: remove the coordination value, and funding dries up; remove the extraction asymmetry, and institutional advantage dissolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majorana_signature_ambiguity,
    'Do zero-energy modes measured in topological superconductor/nanowire systems represent true Majorana fermions or non-topological zero modes with similar experimental signatures?',
    'Braiding experiments demonstrating non-abelian statistics; topological protection verification via disorder robustness; identification of alternative mechanisms producing zero-energy signatures',
    'If true Majoranas: field validation increases, institutional investment justified, engineering constraints become necessity. If non-topological: major engineering complexity provides no quantum advantage, entire constraint structure becomes pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majorana_signature_ambiguity, empirical, 'Ambiguity regarding Majorana vs non-topological zero-mode signatures').

omega_variable(
    engineering_necessity_vs_contingency,
    'Is extreme engineering complexity (cryogenic systems, nanofabrication precision, specialized measurement apparatus) truly necessary for Majorana systems or contingent on current material platform choices?',
    'Development of alternative platforms (higher-temperature superconductors, Floquet engineering, synthetic dimension approaches); comparison of engineering barriers across platforms',
    'If necessary: engineering bottleneck reflects fundamental physics, suppression remains high, structure remains Snare from competing groups. If contingent: engineering bottleneck is institutional (capital/IP concentration), alternative platforms bypass constraints entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engineering_necessity_vs_contingency, empirical, 'Whether extreme engineering is necessary or contingent on material platform').

omega_variable(
    capital_barrier_irreversibility,
    'Does the multi-year, multi-million-dollar investment required for Majorana fabrication capability create irreversible capital lock-in, or can research groups shift to alternative topological platforms without total loss?',
    'Cost analysis for platform transition; survey of research groups that shifted platforms; measurement of asset recovery rates for Majorana-specific equipment',
    'If irreversible: suppression due to sunk costs (true trap), Snare classification from all victim perspectives. If recoverable: suppression is high but not total, exit options upgrade to constrained, some perspectives shift toward Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_barrier_irreversibility, empirical, 'Whether capital investment creates irreversible lock-in or allows platform transition').

omega_variable(
    reproducibility_threshold,
    'What success rate threshold for independent Majorana zero-mode reproduction distinguishes legitimate early-stage physics from extractive claim-staking?',
    'Historical comparison: fields with similar measurement difficulty and reproducibility timeline; meta-analysis of zero-energy mode claims vs successful braiding demonstrations',
    'If threshold < 20% reproduction success: many legitimate exploratory claims misclassified as extraction. If threshold > 50%: extractive claims persist unchallenged in peer review.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reproducibility_threshold, conceptual, 'Reproducibility threshold for legitimate early-stage physics vs extractive claims').

omega_variable(
    institutional_capture_feedback,
    'Does institutional concentration of Majorana research resources (funding, equipment access, graduate students) create self-reinforcing capture where competing groups structurally cannot enter the field, or does it represent fair reward for pioneering work?',
    'Analysis of funding distribution, equipment access rates, and graduate recruitment patterns; comparison to other high-barrier condensed matter fields; measurement of institutional turnover in topological quantum computing',
    'If capture: suppression is artificially elevated, Snare classification confirmed. If fair reward: suppression reflects legitimate barrier, Tangled Rope classification from organized competitors justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_feedback, preference, 'Whether institutional concentration represents capture or fair pioneering reward').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(majorana_fermion_engineering_constraints, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(majferm_tr_t0, majorana_fermion_engineering_constraints, theater_ratio, 0, 0.48).
narrative_ontology:measurement(majferm_tr_t4, majorana_fermion_engineering_constraints, theater_ratio, 4, 0.58).
narrative_ontology:measurement(majferm_tr_t8, majorana_fermion_engineering_constraints, theater_ratio, 8, 0.65).
narrative_ontology:measurement(majferm_tr_t12, majorana_fermion_engineering_constraints, theater_ratio, 12, 0.62).

% Extraction over time
narrative_ontology:measurement(majferm_be_t0, majorana_fermion_engineering_constraints, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(majferm_be_t4, majorana_fermion_engineering_constraints, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(majferm_be_t8, majorana_fermion_engineering_constraints, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(majferm_be_t12, majorana_fermion_engineering_constraints, base_extractiveness, 12, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(majorana_fermion_engineering_constraints, global_infrastructure).
narrative_ontology:affects_constraint(majorana_fermion_engineering_constraints, topological_qubit_fabrication_bottleneck).
narrative_ontology:affects_constraint(majorana_fermion_engineering_constraints, cryogenic_measurement_apparatus_scarcity).
narrative_ontology:affects_constraint(majorana_fermion_engineering_constraints, quantum_computing_capital_concentration).

% DUAL FORMULATION NOTE:
% Majorana engineering constraints decompose into three structurally distinct stories: (1) physics_verification_bottleneck (ε=0.40, high-extractiveness verification bottleneck, downstream of Majorana specifics), (2) majorana_fermion_engineering_constraints (ε=0.58, institutional/capital concentration), (3) alternative_topological_platforms (ε=0.35, lower engineering barrier, competing directly). This story is the institutional middle term; verify relationships via empirical constraint decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(majorana_fermion_engineering_constraints, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
