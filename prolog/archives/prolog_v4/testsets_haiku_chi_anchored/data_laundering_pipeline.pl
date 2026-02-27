% ============================================================================
% CONSTRAINT STORY: data_laundering_pipeline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_laundering_pipeline, []).

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
 *   constraint_id: data_laundering_pipeline
 *   human_readable: The Provenance Eraser: Data Laundering Pipeline
 *   domain: technological/legal/economic
 *
 * SUMMARY:
 *   Data laundering pipelines represent a structural mechanism for stripping
 *   legal protections from personal and copyrighted data through automated
 *   transformations. The constraint operates across three domains:
 *   technological (vectorization, synthetic generation, multi-hop API relay),
 *   legal (jurisdiction arbitrage, anonymization doctrine), and economic
 *   (capital accumulation in AI training companies vs. zero recovery for data
 *   subjects). The core extraction mechanism is jurisdictional and technical
 *   obfuscation: data enters as legally protected material, undergoes
 *   transformations designed to sever traceability to its origin, and emerges
 *   as 'synthetic' or 'anonymized' material free from the original
 *   protections. The snare classification reflects that data subjects and
 *   copyright holders have no exit options once their data is incorporated —
 *   they cannot withdraw it, cannot trace it, and face asymmetric legal
 *   burdens (they must prove harm; the pipeline operator claims protection
 *   under anonymization doctrine). The theater_ratio (0.58) reflects moderate
 *   performativity: the legal fiction of anonymization and the technical
 *   narrative of 'synthetic generation' provide institutional cover, but the
 *   underlying mechanism is essentially identity erasure, not genuine
 *   functional anonymization. The extractiveness trajectory (0.35→0.62 over
 *   the interval) shows the constraint intensifying as: (1) AI training at
 *   scale becomes more economically valuable, (2) legal enforcement remains
 *   fragmented across jurisdictions, and (3) technical sophistication of
 *   laundering methods increases.
 *
 * KEY AGENTS:
 *   - Data Subjects: Powerless/trapped — individuals whose personal data is incorporated into training sets; cannot exit or trace their data; bear zero economic benefit
 *   - Copyright Holders: Powerless/trapped — publishers and authors whose copyrighted works are vectorized and remixed; face asymmetric legal burden to prove infringement post-laundering
 *   - AI Training Companies: Institutional/arbitrage — primary beneficiaries; experience pipeline as practical coordination solution for accessing training data; have full exit options (relocate to permissive jurisdictions)
 *   - Data Aggregators: Institutional/arbitrage — intermediate actors; provide technical services for transformations; benefit from legal opacity and jurisdictional arbitrage
 *   - Regulatory Authorities: Organized/constrained — constrained by jurisdictional limits, slow enforcement cycles, and technical complexity; see pipeline as hybrid mechanism (some coordination value in establishing anonymization standards, some extraction as standards are circumvented)
 *   - Legal Doctrine of Anonymization: Institutional/arbitrage — performative framework; persists despite empirical re-identification vulnerabilities; provides cover for pipeline operators
 *   - Analytical Observer: Civilizational perspective — sees constraint as hybrid extraction-coordination; technical transformations do solve practical coordination problems (integrating diverse data sources) while simultaneously enabling extraction from powerless agents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_laundering_pipeline, 0.62).
domain_priors:suppression_score(data_laundering_pipeline, 0.68).
domain_priors:theater_ratio(data_laundering_pipeline, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_laundering_pipeline, extractiveness, 0.62).
narrative_ontology:constraint_metric(data_laundering_pipeline, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(data_laundering_pipeline, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_laundering_pipeline, snare).
narrative_ontology:human_readable(data_laundering_pipeline, "The Provenance Eraser: Data Laundering Pipeline").
narrative_ontology:topic_domain(data_laundering_pipeline, "technological/legal/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_laundering_pipeline, ai_training_companies).
narrative_ontology:constraint_beneficiary(data_laundering_pipeline, data_aggregators).
narrative_ontology:constraint_victim(data_laundering_pipeline, original_data_subjects).
narrative_ontology:constraint_victim(data_laundering_pipeline, copyright_holders).
narrative_ontology:constraint_victim(data_laundering_pipeline, regulatory_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DATA SUBJECT (SNARE) — Original creators and subjects of personal data (individuals in training datasets, authors of copyrighted text) cannot exit the pipeline once their data enters. No alternative exists; suppression of legal remedy is high. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈1.05.
constraint_indexing:constraint_classification(data_laundering_pipeline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE COPYRIGHT HOLDER (SNARE) — Publishers and authors subject to data laundering lose legal recourse once works are vectorized and remixed. Trapped by jurisdictional arbitrage and technical obfuscation. d≈0.91, f(d)≈1.38, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(data_laundering_pipeline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: THE AI TRAINING COMPANY (ROPE) — Experiences the pipeline as a coordination solution: solving the practical problem of 'how to access training data at scale without legal entanglement.' From their perspective, the transformations are a technical coordination mechanism. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(data_laundering_pipeline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE REGULATORY COALITION (TANGLED ROPE) — Legal and regulatory actors (jurisdictions, privacy advocates, copyright enforcement bodies) are organized but constrained by jurisdictional limits and technological complexity. The pipeline provides coordination (establishing shared norms around 'anonymization' thresholds) but extracts by shifting burden to regulators who must prove harm post-facto. d≈0.58, f(d)≈0.72, σ=1.2 → χ≈0.51.
constraint_indexing:constraint_classification(data_laundering_pipeline, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE LEGAL DOCTRINE (PITON) — Traditional legal frameworks treat 'anonymization' as a bright-line categorical property. Once data is deemed anonymized, protection lapses. But re-identification attacks show this is largely performative: high-dimensional datasets are rarely truly anonymized. The doctrine persists through institutional inertia despite functional degradation. theater_ratio=0.58 reflects moderate theatricality; legal cover remains plausible but weakening. d≈0.10, f(d)≈-0.09, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(data_laundering_pipeline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the pipeline exhibits both genuine technical coordination (vectorization does solve the practical problem of integrating diverse data sources) and structural extraction (it simultaneously strips legal protections from powerless agents). The constraint is not a pure snare or pure coordination, but a hybrid where coordination benefits flow to capital and extraction costs flow to individuals. d≈0.62, f(d)≈0.85, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(data_laundering_pipeline, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_laundering_pipeline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_laundering_pipeline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_laundering_pipeline, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_laundering_pipeline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_laundering_pipeline, TR),
    TR >= 0.70.

:- end_tests(data_laundering_pipeline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The pipeline extracts significant economic and legal value from data subjects and copyright holders. Capital is concentrated in AI companies whose models derive value from incorporated training data; those data sources receive zero compensation and zero legal remedy post-laundering. However, not maximal (0.62 vs 0.80+) because: (1) some laundering is genuinely imperfect — re-identification is possible for motivated attackers, providing a compliance floor; (2) regulatory pressure is increasing, reducing indefinite extraction; (3) some technical transformations do provide utility beyond obfuscation (vectorization genuinely enables scalable training). Suppression (0.68): High. Multiple barriers prevent legal or practical recourse: jurisdictional arbitrage (pipeline can operate from permissive jurisdictions), technical obfuscation (data subjects cannot trace their data through transformations), burden-shifting doctrine (subject must prove harm post-facto rather than operator proving protection pre-facto), and organizational asymmetry (distributed subjects vs. coordinated companies). Theater ratio (0.58): Moderate-high. The 'anonymization' and 'synthetic generation' narratives are substantially theatrical. Bright-line categorical anonymization is not empirically justified; high-dimensional data rarely survives re-identification attacks. Yet the theater is not total (0.58 vs 0.80+) because: (1) vectorization does provide technical separation from source identifiers, even if statistical structure remains; (2) some regulatory scrutiny is real, creating compliance burdens; (3) legal frameworks (GDPR, CCPA) do provide some procedural protection, even if enforcement is weak.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Data subjects experience pure snare (trapped, no exit, no legal recourse). Copyright holders experience snare (asymmetric burden, jurisdictional escape). The AI training company experiences rope/coordination (solving legitimate problem of accessing diverse training data). The regulatory coalition experiences tangled rope (real coordination function in setting anonymization standards; real extraction as standards are circumvented). The legal doctrine experiences piton (performative framework persisting through inertia despite empirical vulnerabilities). The analytical observer experiences tangled rope (sees both genuine technical coordination and structural extraction). The perspectival gap is maximal because the agent with most power (AI company) experiences the constraint as pure coordination while the powerless agent (data subject) experiences it as pure snare. This gap is a diagnostic signature of successful extraction: when the beneficiary sees coordination and the victim sees snare, the constraint is working as designed.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Cannot exit or trace data. Copyright holders: Victim + trapped → d≈0.91, f(d)≈1.38. Maximum extraction; similar to data subjects but with additional legal standing in some jurisdictions. AI training companies: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Can relocate operations to permissive jurisdictions. Data aggregators: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.09. Net beneficiary; provide technical services and profit from opacity. Regulatory coalition: Victim + constrained → d≈0.58, f(d)≈0.72. Moderate extraction; constrained by jurisdictional limits and enforcement complexity; also benefits from some coordination (standards setting). Legal doctrine: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.09. Net beneficiary (provides cover for operators). Analytical observer: Mixed → d≈0.62, f(d)≈0.85. Sees genuine coordination (transformations solve integration problems) alongside extraction (protection-stripping).
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY MARKER: The constraint exhibits a clear extraction-disguised-as-coordination pattern. AI training companies genuinely solve a technical coordination problem (how to integrate heterogeneous data sources at scale) while simultaneously extracting by stripping legal protections. The mandatrophy is resolved by recognizing that the coordination function is real but asymmetric: benefits to capital, costs to labor (data subjects). The snare classification dominates because the extraction mechanism (laundering) is the primary structural feature; the coordination is a secondary effect that enables extraction. If the pipeline were pure coordination (no protection-stripping), it would classify as Rope. If the coordination function were eliminated and only laundering remained, it would remain Snare. The hybrid classification (Tangled Rope from analytical perspective) is correct but subordinate to Snare as the binding classification for powerless agents — the system's first priority is identifying extraction that affects the most vulnerable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reidentification_technical_threshold,
    'At what point do synthetic transformations sufficiently degrade re-identification risk to render data functionally anonymized?',
    'Empirical re-identification studies: measure success rates of inference attacks on progressively transformed datasets; comparison with baseline random-guess success rates',
    'If threshold is empirically low (5-10% re-identification after common transformations): legal doctrine of anonymization is empirically false, and data laundering is pure snare. If threshold is high (>50% resistance): transformations provide genuine protection, and constraint shifts toward rope/scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reidentification_technical_threshold, empirical, 'Technical thresholds for functional anonymization').

omega_variable(
    synthetic_data_independence_assumption,
    'Does synthetic data generated from original data truly break informational dependency on source, or does it retain statistical fingerprints that reveal provenance?',
    'Adversarial model membership inference; analysis of whether synthetic data preserves correlational structures that uniquely identify source dataset composition',
    'If synthetic data retains source fingerprints: laundering is theater, not genuine protection. If truly independent: coordinate generation is a valid technical solution, and classification shifts to scaffold/rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_data_independence_assumption, empirical, 'Whether synthetic generation achieves genuine statistical independence').

omega_variable(
    jurisdictional_arbitrage_sustainability,
    'Can regulatory coalitions achieve coordinated enforcement across jurisdictions fast enough to close data laundering pathways, or is jurisdictional arbitrage a structural feature?',
    'Timeline analysis: measure lag between regulatory action in one jurisdiction and adaptation of pipelines in others; track whether enforcement actions reduce pipeline throughput or merely relocate it',
    'If enforcement can be coordinated: constraint is tangled rope with sunset (regulatory catching up). If arbitrage is structural: constraint remains snare indefinitely, and international coordination frameworks are performative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jurisdictional_arbitrage_sustainability, conceptual, 'Whether coordinated jurisdictional enforcement can close arbitrage gaps').

omega_variable(
    data_subject_collective_action_threshold,
    'Can data subjects or copyright holders achieve collective organization sufficient to initiate lawsuits or regulatory action, or are transaction costs and information asymmetry insurmountable?',
    'Empirical tracking of class action litigation; measurement of filing rates pre/post organizing campaigns; analysis of settlement success rates',
    'If collective action threshold can be crossed: powerless agents can upgrade to organized, classification shifts. If threshold is permanently structural: powerless agents remain trapped, and snare classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_subject_collective_action_threshold, preference, 'Whether data subjects can achieve collective action').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_laundering_pipeline, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dlp_tr_t0, data_laundering_pipeline, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dlp_tr_t5, data_laundering_pipeline, theater_ratio, 5, 0.5).
narrative_ontology:measurement(dlp_tr_t10, data_laundering_pipeline, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(dlp_be_t0, data_laundering_pipeline, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dlp_be_t5, data_laundering_pipeline, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dlp_be_t10, data_laundering_pipeline, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_laundering_pipeline, resource_allocation).
narrative_ontology:affects_constraint(data_laundering_pipeline, copyright_enforcement_asymmetry).
narrative_ontology:affects_constraint(data_laundering_pipeline, privacy_doctrine_reidentification).
narrative_ontology:affects_constraint(data_laundering_pipeline, jurisdictional_regulatory_arbitrage).

% DUAL FORMULATION NOTE:
% The data laundering pipeline decomposes into three structurally distinct constraints: (1) technical data transformation (ε≈0.35, coordination-heavy, Rope from most perspectives); (2) legal anonymization doctrine (ε≈0.42, piton-range, theater_ratio high); (3) jurisdictional enforcement arbitrage (ε≈0.58, snare-range, extraction-heavy). This story (data_laundering_pipeline) integrates all three; the network links show which downstream constraints are affected by the integrated pipeline's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_laundering_pipeline, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
