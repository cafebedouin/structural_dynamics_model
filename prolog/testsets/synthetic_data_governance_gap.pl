% ============================================================================
% CONSTRAINT STORY: synthetic_data_governance_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_synthetic_data_governance_gap, []).

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
 *   constraint_id: synthetic_data_governance_gap
 *   human_readable: Synthetic Data Governance Gap
 *   domain: data_governance/ai_regulation/institutional_coordination
 *
 * SUMMARY:
 *   The synthetic data governance gap describes a structural coordination
 *   failure where the technical solution (synthetic data training) creates
 *   genuine value for ML development while simultaneously enabling extractive
 *   secondary use of individual likenesses without consent frameworks. The
 *   constraint exhibits the full DR spectrum: from the data subject's
 *   perspective (snare — no exit, no consent mechanism), to the regulator's
 *   (tangled rope — must balance innovation and protection), to the
 *   producer's (rope — pure coordination value), to the traditional
 *   governance system's (piton — categories are performing enforcement
 *   rituals without functional mapping to synthetic data), to an emerging
 *   alternative governance coalition's (scaffold — distributed training and
 *   privacy-preserving techniques offer exit paths). The theater_ratio of
 *   0.68 reflects that much current synthetic data governance consists of
 *   iterating existing doctrine (consent, purpose limitation, minimization)
 *   against a fundamentally different class of data objects. Extractiveness
 *   increased from 0.32 to 0.58 over the measurement interval as synthetic
 *   data adoption accelerated without corresponding governance evolution.
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary victims (powerless/trapped) — whose characteristics are replicated without meaningful consent or compensation mechanism
 *   - Synthetic Data Producers and Model Developers: Primary beneficiaries (institutional/arbitrage) — capture value from faster development cycles and privacy-preserving training without consent frameworks
 *   - Regulatory Bodies (GDPR, CCPA, DPA): Organized victims (organized/constrained) — must define new enforcement categories while balancing innovation and protection
 *   - Downstream AI Operators: Moderate victims (moderate/constrained) — benefit from higher-quality models but bear risk from non-transparent training data provenance
 *   - Traditional Data Governance System: Institutional piton (institutional/arbitrage) — enforcement categories persist through inertia despite degraded functional mapping
 *   - Emerging Governance Coalition: Organized agents (organized/mobile) — international standards bodies, AI transparency initiatives, federated learning consortia building alternative pathways with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing governance failure as inherent innovation/privacy trade-off
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(synthetic_data_governance_gap, 0.58).
domain_priors:suppression_score(synthetic_data_governance_gap, 0.62).
domain_priors:theater_ratio(synthetic_data_governance_gap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(synthetic_data_governance_gap, extractiveness, 0.58).
narrative_ontology:constraint_metric(synthetic_data_governance_gap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(synthetic_data_governance_gap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(synthetic_data_governance_gap, tangled_rope).
narrative_ontology:human_readable(synthetic_data_governance_gap, "Synthetic Data Governance Gap").
narrative_ontology:topic_domain(synthetic_data_governance_gap, "data_governance/ai_regulation/institutional_coordination").

domain_priors:requires_active_enforcement(synthetic_data_governance_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(synthetic_data_governance_gap, synthetic_data_producers).
narrative_ontology:constraint_beneficiary(synthetic_data_governance_gap, ai_model_developers).
narrative_ontology:constraint_victim(synthetic_data_governance_gap, data_subjects_represented_in_training).
narrative_ontology:constraint_victim(synthetic_data_governance_gap, regulatory_compliance_infrastructure).
narrative_ontology:constraint_victim(synthetic_data_governance_gap, downstream_ai_system_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECTS (SNARE) — Individuals whose characteristics, demographics, and behaviors are encoded in synthetic training data have no exit option and no meaningful consent mechanism. They cannot opt out of synthetic replication without proving their data was used. Maximum experienced extraction: identity used without compensation or control, with no recourse mechanism.
constraint_indexing:constraint_classification(synthetic_data_governance_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATORY BODIES (TANGLED ROPE) — National data protection authorities (GDPR, CCPA enforcement bodies) face a coordination problem: synthetic data is genuinely useful for innovation and privacy-preserving ML training. But the same synthetic generation process creates extractive asymmetries: producers capture value from replicated likenesses without consent frameworks. Regulators are constrained by their own mandate to balance innovation and protection. They benefit from synthetic data's coordination function (enables privacy-preserving research) while bearing costs of defining new enforcement categories with limited precedent.
constraint_indexing:constraint_classification(synthetic_data_governance_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYNTHETIC DATA PRODUCERS (ROPE) — Experience the constraint as pure coordination: generating synthetic data enables faster model development, reduces privacy risk from real data, and creates legitimate value. From their position, the constraint is a solution to the real-data-training bottleneck. They have arbitrage options (shift to synthetic data, licensing frameworks) and capture concentrated benefits. Net beneficiary.
constraint_indexing:constraint_classification(synthetic_data_governance_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOWNSTREAM AI OPERATORS (TANGLED ROPE) — Organizations deploying trained models benefit from higher-quality models trained on synthetic data (coordination). But they also bear extraction costs: models trained on synthetic data can have subtle distribution shifts, demographic representation gaps, or out-of-distribution failure modes that are harder to detect without knowing the synthetic generation process. Constrained by lack of transparency into training data provenance. Mixed benefit and harm.
constraint_indexing:constraint_classification(synthetic_data_governance_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL DATA GOVERNANCE (PITON) — Existing data governance categories (personally identifiable information, consent, purpose limitation, data minimization) were built for concrete data trails. Synthetic data degrades these categories: Is synthetic data 'about' the original data subjects? Does consent to real-data collection cover synthetic replication? Is purpose limitation binding on synthetic derivatives? The governance machinery persists through institutional inertia (GDPR articles are recited) but the functional mapping is broken. Theater ratio high because enforcement actions cite traditional doctrine that no longer clearly applies.
constraint_indexing:constraint_classification(synthetic_data_governance_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING GOVERNANCE COALITION (SCAFFOLD) — International standards bodies (ISO, IEEE), AI transparency initiatives, and federated learning consortia are building alternative governance pathways: synthetic data lineage tracking, quality attestation frameworks, provenance registries, and federated/decentralized training as substitutes. These are sunset mechanisms — as distributed training and differential privacy mature, the centralized synthetic generation bottleneck loses its necessity. Organized agents see a temporary problem being solved. Has exit path and agency.
constraint_indexing:constraint_classification(synthetic_data_governance_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — The civilizational perspective risks naturalizing the governance gap as an inherent trade-off between innovation and privacy: 'Large-scale AI training has always required data, and synthetic data is a neutral technical solution to privacy concerns.' This framing treats the gap as a structural feature of how ML scales rather than a governance failure. The engine's false summit detector will identify this as naturalization of what is actually a contingent institutional coordination gap.
constraint_indexing:constraint_classification(synthetic_data_governance_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(synthetic_data_governance_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(synthetic_data_governance_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(synthetic_data_governance_gap, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(synthetic_data_governance_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(synthetic_data_governance_gap, TR),
    TR >= 0.70.

:- end_tests(synthetic_data_governance_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. Synthetic data producers capture significant value from faster development cycles, reduced privacy liability exposure, and avoidance of consent infrastructure costs. The extraction is not maximal because synthetic training is genuinely useful for downstream innovation and does reduce privacy risks relative to unconstrained real-data training. The increase from 0.32 to 0.58 reflects growing adoption without corresponding governance evolution — as synthetic data becomes standard practice, the 'exceptional research tool' framing disappears and the extraction mechanism becomes structural. Suppression (0.62): High. Data subjects have no meaningful exit or recourse mechanism. No consent requirement exists in most jurisdictions for synthetic replication of likenesses derived from training data. Regulatory enforcement is nascent and uncertain. Career and institutional incentives favor synthetic generation. Theater ratio (0.68): High and increasing. Current governance consists of retrofitting GDPR consent and purpose-limitation doctrine onto synthetic data without clear functional mapping. Enforcement actions cite traditional doctrine; compliance consists of adding consent checkboxes that do not meaningfully govern synthetic replication. The theater increased as synthetic adoption accelerated without governance evolution.
 *
 * PERSPECTIVAL GAP:
 *   Data subjects (Snare) vs. producers (Rope) exhibit maximum perspectival divergence because they occupy opposite structural positions: one has no exit or control, the other has both. Regulators and downstream operators (both Tangled Rope) experience asymmetric mixed benefit/harm: they are constrained by different factors (mandate vs. opacity) but both face real costs alongside benefits. The traditional governance system (Piton) versus the emerging coalition (Scaffold) represents institutional divergence in time horizon: legacy doctrine persists through inertia despite degraded fit; alternatives are being built with generational time horizons. The analytical observer risks collapsing this perspectival structure into a false mountain ('innovation requires privacy friction') — the false summit detector identifies this as naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim position and exit options. Data subjects are victims with trapped exit → high d → high f(d) → maximum experienced extraction chi. Producers are beneficiaries with arbitrage exit → low d → low f(d) → negative or minimal experienced extraction. Regulators are victims (constrained by mandate to protect) but with moderate institutional power and constrained exit (they cannot simply opt out of regulating) → moderate d → moderate f(d) → moderate chi. Downstream operators are both beneficiaries (better models) and victims (opacity risk) with constrained exit (they must train models) → moderate-high d → moderate chi. The analytical observer with analytical exit experiences moderate d reflecting neither pure victim nor beneficiary status. The scaffold coalition with mobile exit and organized power experiences low chi — they have agency and exit pathways.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC RESOLUTION: The synthetic data governance gap resolves the mandatrophy by showing that the coordination benefit (faster, privacy-preserving training) is real AND the extraction mechanism (replicated likenesses without consent) is real — they are not competing interpretations of a single underlying phenomenon, but genuinely distinct structural elements that happen to be co-located in the same technical object (synthetic data). Tangled Rope classification correctly identifies both. The mandate is not 'Is synthetic data good or bad?' but 'How do we decouple the coordination function (privacy-preserving training) from the extraction mechanism (unconsented likeness replication)?' This is precisely what scaffold/alternative governance mechanisms (federated training, differential privacy, provenance registries) attempt to do: preserve the coordination benefit while blocking the extraction pathway. The mandatrophy is resolved not by choosing a single type but by recognizing that Tangled Rope is the correct classification and that the strategic problem is transforming it toward either pure Rope (if extraction can be eliminated via governance) or pure Scaffold (if alternatives can replace centralized synthetic generation entirely).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthetic_data_derivativeness_threshold,
    'At what degree of transformation does synthetic data cease to be a derivative of original subjects and become genuinely independent?',
    'Empirical analysis of membership inference attacks, attribute inference attacks, and identity leakage from synthetic datasets. Comparison with privacy-preserving training baseline (differential privacy epsilon bounds).',
    'If transformation is minimal (membership inference high): synthetic data is legally and ethically a derivative work requiring consent. If transformation is complete (membership inference near random): synthetic data is independent data, extraction mechanism disappears.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(synthetic_data_derivativeness_threshold, empirical, 'Threshold for synthetic data independence from original subjects').

omega_variable(
    consent_retroactivity_doctrine,
    'Does consent to real-data collection legally or ethically extend to synthetic replication, or does synthetic generation require new consent?',
    'Legal analysis across jurisdictions (GDPR, CCPA, emerging synthetic-data-specific regulations). Case law tracking consent doctrine under conditions of secondary use and transformation.',
    'If old consent extends: extraction mechanism is institutionally legitimate, constraint reclassifies toward Rope. If new consent required: extraction is uncompensated secondary use, constraint remains Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_retroactivity_doctrine, conceptual, 'Whether original consent covers synthetic replication').

omega_variable(
    model_provenance_disclosure_burden,
    'Who bears the burden and cost of proving synthetic data provenance and disclosure?',
    'Analysis of regulatory enforcement (who demands provenance, who supplies it, at what cost). Comparison of audit costs across industries (finance, healthcare, general ML).',
    'If burden on producers: creates enforcement mechanism and cost, reducing extraction incentive. If burden on regulators/operators: extraction persists because disclosure is too expensive to verify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_provenance_disclosure_burden, empirical, 'Burden allocation for synthetic data provenance verification').

omega_variable(
    federated_training_adoption_timeline,
    'At what point does federated/decentralized training become cost-competitive with centralized synthetic-data-based training?',
    'Infrastructure cost tracking, latency benchmarks, and adoption curves for federated learning frameworks. Comparison with synthetic data generation compute costs.',
    'If federated adoption is rapid (3-5 years): scaffold perspective validated, constraint has real sunset. If adoption stalls: centralized synthetic training persists as dominant paradigm, extraction mechanism remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federated_training_adoption_timeline, empirical, 'Timeline for federated training cost-competitiveness').

omega_variable(
    synthetic_data_quality_floor,
    'What is the minimum quality floor for synthetic data before model performance degradation becomes unacceptable?',
    'Empirical benchmarking of synthetic-trained models across task domains. Comparison of downstream performance on held-out real data when trained on synthetic vs real data.',
    'If quality floor is low: synthetic data solves the coordination problem efficiently, constraint reclassifies toward Rope. If quality floor is high: synthetic data requires enormous additional governance overhead, remaining Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(synthetic_data_quality_floor, empirical, 'Minimum acceptable quality for synthetic training data').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(synthetic_data_governance_gap, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(synth_tr_t0, synthetic_data_governance_gap, theater_ratio, 0, 0.45).
narrative_ontology:measurement(synth_tr_t3, synthetic_data_governance_gap, theater_ratio, 3, 0.58).
narrative_ontology:measurement(synth_tr_t6, synthetic_data_governance_gap, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(synth_be_t0, synthetic_data_governance_gap, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(synth_be_t3, synthetic_data_governance_gap, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(synth_be_t6, synthetic_data_governance_gap, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(synthetic_data_governance_gap, resource_allocation).
narrative_ontology:affects_constraint(synthetic_data_governance_gap, ai_model_training_bottleneck).
narrative_ontology:affects_constraint(synthetic_data_governance_gap, privacy_preserving_ml_constraints).
narrative_ontology:affects_constraint(synthetic_data_governance_gap, data_subject_consent_infrastructure).

% DUAL FORMULATION NOTE:
% Synthetic data governance gap is downstream of AI training infrastructure constraints but represents a distinct structural constraint on data governance coherence. The upstream training bottleneck has its own ε reflecting empirical difficulty of obtaining large labeled datasets; this constraint has ε reflecting governance failure in allocating consent and compensation for synthetic replication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
