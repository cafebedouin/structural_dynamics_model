% ============================================================================
% CONSTRAINT STORY: data_laundering_pipeline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The data laundering pipeline represents a structural extraction mechanism
 *   where personal data, copyrighted works, and other legally protected
 *   information are passed through automated transformations to strip away
 *   original legal protections and attribution requirements. The constraint
 *   operates across three domains: personal data (GDPR/CCPA protections),
 *   copyright (creative works used without compensation or attribution), and
 *   epistemic integrity (synthetic data generations that obscure original
 *   provenance). The pipeline creates a structural asymmetry: data subjects
 *   and copyright holders bear the costs of unauthorized use, while AI
 *   training operators and data aggregators capture the benefits of
 *   unrestricted training datasets. The constraint's extractiveness (0.58)
 *   reflects that the transformation process is intentionally designed to
 *   evade legal detection, and victims have minimal practical recourse once
 *   data has been laundered. Suppression (0.68) reflects significant barriers
 *   to tracking transformed data, enforcing rights across jurisdictions, and
 *   identifying origin information once vectorized or synthesized. The
 *   theater ratio (0.61) indicates that privacy-by-design compliance
 *   frameworks create performative protection through impact assessments and
 *   transparency requirements while actual enforcement is weak and evasion is
 *   technically straightforward.
 *
 * KEY AGENTS:
 *   - Data Subject: Primary victim (powerless/trapped) — personal data stripped of privacy protections; no recourse mechanism
 *   - Copyright Holder: Primary victim (powerless/trapped) — creative works used for training without compensation or attribution
 *   - AI Training Operator: Primary beneficiary (institutional/arbitrage) — captures value from unrestricted training datasets; can relocate to arbitrage jurisdictions
 *   - Data Aggregator: Secondary beneficiary (institutional/arbitrage) — monetizes laundered datasets; high exit optionality
 *   - Regulatory Authority: Mixed actor (organized/constrained) — has coordination function but constrained by jurisdictional gaps and resource limits
 *   - Privacy Framework: Institutional actor (institutional/arbitrage) — maintains performative protection theater; degraded function (piton perspective)
 *   - Provenance Coalition: Organized agents (organized/constrained) — emerging technical infrastructure (watermarking, ledgers) building alternative pathways with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent governance failure as information entropy law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_laundering_pipeline, 0.58).
domain_priors:suppression_score(data_laundering_pipeline, 0.68).
domain_priors:theater_ratio(data_laundering_pipeline, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_laundering_pipeline, extractiveness, 0.58).
narrative_ontology:constraint_metric(data_laundering_pipeline, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(data_laundering_pipeline, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_laundering_pipeline, snare).
narrative_ontology:human_readable(data_laundering_pipeline, "The Provenance Eraser: Data Laundering Pipeline").
narrative_ontology:topic_domain(data_laundering_pipeline, "technological/legal/economic").

domain_priors:requires_active_enforcement(data_laundering_pipeline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_laundering_pipeline, ai_training_operators).
narrative_ontology:constraint_beneficiary(data_laundering_pipeline, data_aggregators).
narrative_ontology:constraint_victim(data_laundering_pipeline, original_data_subjects).
narrative_ontology:constraint_victim(data_laundering_pipeline, copyright_holders).
narrative_ontology:constraint_victim(data_laundering_pipeline, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DATA SUBJECT (SNARE) — Individual whose personal data was scraped and has no practical recourse once it enters the pipeline. Cannot exit the constraint. Suppression is maximal: legal frameworks (GDPR, CCPA) do not track transformed data; technical tools to identify origin are absent; costs of litigation exceed recovery. Extraction is severe and irreversible.
constraint_indexing:constraint_classification(data_laundering_pipeline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COPYRIGHT HOLDER (SNARE) — Artist, author, or creator whose copyrighted work was fed into synthetic generation pipelines. Original copyright protections dissolve once work is vectorized and used as training data. Cannot enforce rights against derived synthetic outputs. Trapped by the speed of distribution and cost of litigation.
constraint_indexing:constraint_classification(data_laundering_pipeline, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: AI TRAINING OPERATOR (ROPE) — Entity operating the pipeline experiences it as a coordination mechanism: laundering protections enables efficient model training at scale. High arbitrage options: can relocate operations to regulatory arbitrage jurisdictions, redirect training pipeline, or acquire already-laundered datasets. Net beneficiary with full agency.
constraint_indexing:constraint_classification(data_laundering_pipeline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Government or data protection authority sees the constraint as mixed: has coordination function (preventing total anarchy of data handling) but also extractive (leverages enforcement authority asymmetrically against small operators while large platforms gain regulatory capture exemptions). Constrained by cross-border jurisdictional gaps and resource limitations.
constraint_indexing:constraint_classification(data_laundering_pipeline, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIVACY-BY-DESIGN FRAMEWORK (PITON) — Legal privacy frameworks (GDPR Article 35, CCPA, etc.) maintain theater of provenance protection through impact assessments and transparency requirements. In practice, these frameworks are largely performative: transformations are documented as 'anonymization' despite reversibility, impact assessments rubber-stamped, and enforcement costs prohibitive. The framework persists through institutional inertia and public legitimacy theater, not effective function.
constraint_indexing:constraint_classification(data_laundering_pipeline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DATA PROVENANCE TRACKING COALITION (SCAFFOLD) — Emerging technical and legal infrastructure (cryptographic watermarking, immutable provenance logs, synthetic data labeling mandates) aims to create alternative verification pathways that restore traceability despite transformations. Sees the laundering pipeline as a temporary coordination failure with sunset: distributed audit trails and mandatory provenance metadata are building exit pathways. High suppression but declining theater as technical controls mature.
constraint_indexing:constraint_classification(data_laundering_pipeline, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, information flows always tend toward entropy: once data is released into digital networks, perfect provenance tracking is physically impossible. The compression and transformation of data necessarily degrades origin signal. This perspective frames the constraint as a natural law of information systems. However, this false summit naturalizes what is actually a contingent governance failure: cryptographic watermarking, ledger-based provenance, and mandatory transformation disclosure can restore practical traceability. The engine's false summit detector will identify this.
constraint_indexing:constraint_classification(data_laundering_pipeline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.58): High. The pipeline is specifically engineered to enable extraction: each transformation step (vectorization, synthetic generation, API relay) is chosen to degrade recoverable provenance information while maintaining utility for training. The base extraction value reflects that the mechanism is not coercive in the traditional sense but is systematically designed to evade legal protections. The trajectory from 0.35 → 0.58 reflects that as transformations become more sophisticated, detection and recovery become harder, increasing effective extraction. Suppression (0.68): High. Multiple barriers prevent victims from enforcing rights: (1) jurisdictional gaps between data origin and processing location, (2) technical opacity — transformations obscure origin sufficiently that tracking requires specialized forensic analysis, (3) cost asymmetry — individual litigation costs exceed typical personal data damages, (4) public authority enforcement is resource-constrained and reactive rather than preventive. Theater ratio (0.61): Moderate-high. Privacy frameworks (GDPR, CCPA) maintain substantial theater: impact assessments are often generic templates, anonymization claims are reversible, consent-to-training is buried in terms of service, and enforcement is rare against well-resourced operators. However, the theater is not as extreme as in traditional compliance (0.85+) because technical detection of laundering is advancing and some enforcement actions are visible. Theater has increased from 0.42 → 0.61 as frameworks have evolved, indicating that performative elements are being added faster than substantive enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival gap between victims and beneficiaries. Data subjects and copyright holders see the pipeline as pure extraction (snare): they have no exit, no benefit, maximal costs, and no recourse. AI operators see the pipeline as pure coordination (rope): they solve the legitimate problem of obtaining diverse training data at scale; they have multiple exit options and net benefit. Regulatory authorities see mixed coordination and extraction (tangled rope): the framework serves both to prevent total anarchy and to enable selective enforcement. The privacy-by-design framework sees itself as performatively protecting (piton): maintaining legitimacy theater through impact assessments while actual function degrades as transformations become more sophisticated. The provenance tracking coalition sees a solvable temporary problem (scaffold): emerging technical infrastructure (watermarking, ledgers) can restore traceability with a 5-15 year sunset. The analytical observer risks seeing an immutable law of information (mountain): once data is transformed, perfect provenance recovery is impossible. The perspectival gap is irreducible because the agents have genuinely incompatible structural interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position relative to extraction flow. Data subjects and copyright holders are powerless/trapped victims: they cannot exit the pipeline once data has been released; they bear full costs of unauthorized use; their d values are high (0.90+), producing maximum experienced extractiveness. AI operators and aggregators are institutional/arbitrage beneficiaries: they can relocate operations, acquire datasets from different sources, and have multiple exit options; their d values are low (0.10-0.20), producing negative or minimal experienced extractiveness (they benefit). Regulatory authorities occupy a mixed position: they have power and some exit (can enforce or not) but are constrained by jurisdictional realities; their d is intermediate (0.40-0.55). The piton classification for the privacy framework derives from high theater_ratio (0.61) combined with degraded function: the framework persists through legitimacy theater (compliance visibility) rather than actual enforcement effectiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness 0.58 > 0.46): The classification prevents mislabeling the pipeline as pure coordination (rope) by requiring separate perspectives from victims and beneficiaries. If only the AI operator's perspective were considered (rope), the constraint would appear benign — a useful coordination mechanism for training data acquisition. If only the data subject's perspective were considered (snare), the constraint would appear to have no legitimate function. The mandatrophy is resolved by requiring all six perspectives: the beneficiary's rope (legitimate coordination need), the victim's snare (genuine extraction), the regulatory authority's tangled rope (mixed function with institutional capture), the framework's piton (performative protection), the coalition's scaffold (technical exit path), and the analytical observer's false summit (risked naturalization). The resolution confirms that the constraint is genuinely extractive (snare/tangled rope predominates) but claims a coordination function (rope perspective is valid) that masks the extraction. The beneficiary's experience of efficient training data acquisition is real; the victim's experience of irreversible unauthorized use is equally real. The constraint is a snare with coordination theater, not a rope with equity problems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_of_transformation,
    'Can modern synthetic data generation, vectorization, or multi-hop relay truly irreversibly sever provenance, or do adversarial reverse-engineering techniques allow recovery of original data?',
    'Empirical testing of reversibility against state-of-the-art synthetic data attacks; analysis of watermark robustness; cryptanalysis of claimed anonymization techniques',
    'If truly irreversible: extraction is structural (snare classification confirmed). If reversible: much of the claimed laundering is theater and extraction is moderate (tangled rope from beneficiary perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reversibility_of_transformation, empirical, 'Whether data transformation is truly irreversible or subject to reverse-engineering').

omega_variable(
    regulatory_intent_vs_capture,
    'Do privacy-by-design frameworks represent genuine regulatory intent to protect data subjects, or do they function primarily to provide cover for institutional actors (platforms, AI operators) while appearing to enforce?',
    'Historical analysis of enforcement actions: ratio of enforcement against small operators vs large platforms; analysis of exemptions and safe harbors; cost-benefit analysis of compliance vs fines',
    'If genuine intent: regulatory authority perspective is correct (tangled rope with mixed function). If captured: framework is pure theater (piton perspective dominates) and systemic extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intent_vs_capture, conceptual, 'Whether privacy frameworks are genuine enforcement or regulatory theater').

omega_variable(
    synthetic_data_parity_illusion,
    'Is synthetic data generated from copyrighted originals genuinely non-infringing, or does it constitute derivative work under copyright law regardless of technical transformation?',
    'Legal precedent analysis; analysis of similarity metrics between synthetic and original; court rulings on fair use vs training data (e.g., New York Times v. OpenAI outcomes)',
    'If synthetic is infringing: laundering pipeline is legal violation (extraction is snare). If synthetic is non-infringing: much claimed extraction is prevented by valid legal doctrine and effective extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_data_parity_illusion, conceptual, 'Whether synthetic data is legally derivative work or non-infringing').

omega_variable(
    mandatory_provenance_feasibility,
    'Can cryptographic watermarking, provenance ledgers, and mandatory transformation disclosure actually scale to global data flows without creating a parallel dystopia of surveillance infrastructure?',
    'Technical analysis of watermark robustness at scale; cost analysis of ledger maintenance; analysis of surveillance risks of global provenance tracking infrastructure',
    'If feasible without creating worse harms: scaffold sunset is real and extraction will decline. If surveillance risk is prohibitive: scaffold is aspirational and constraint persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatory_provenance_feasibility, empirical, 'Whether mandatory provenance tracking can scale without creating surveillance dystopia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_laundering_pipeline, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dlp_tr_t0, data_laundering_pipeline, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dlp_tr_t5, data_laundering_pipeline, theater_ratio, 5, 0.54).
narrative_ontology:measurement(dlp_tr_t10, data_laundering_pipeline, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(dlp_be_t0, data_laundering_pipeline, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dlp_be_t5, data_laundering_pipeline, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dlp_be_t10, data_laundering_pipeline, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_laundering_pipeline, resource_allocation).
narrative_ontology:affects_constraint(data_laundering_pipeline, synthetic_data_authenticity).
narrative_ontology:affects_constraint(data_laundering_pipeline, copyright_attribution_erasure).
narrative_ontology:affects_constraint(data_laundering_pipeline, gdpr_enforceability_gap).

% DUAL FORMULATION NOTE:
% The data laundering pipeline decomposes into three structurally distinct constraints: (1) synthetic_data_authenticity (ε ≈ 0.42) — whether synthetic outputs are genuinely non-infringing; (2) copyright_attribution_erasure (ε ≈ 0.65) — whether transformation strips copyright obligations; (3) gdpr_enforceability_gap (ε ≈ 0.51) — whether privacy frameworks can track and enforce across transformations. Each has different ε values reflecting different empirical uncertainty. The present story (data_laundering_pipeline) is the hybrid constraint capturing how these three interact in the pipeline architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_laundering_pipeline, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
