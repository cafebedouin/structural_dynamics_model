% ============================================================================
% CONSTRAINT STORY: synthetic_data_authenticity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_synthetic_data_authenticity, []).

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
 *   constraint_id: synthetic_data_authenticity
 *   human_readable: Synthetic Data Authenticity Verification Constraint
 *   domain: data_science/machine_learning/AI_governance
 *
 * SUMMARY:
 *   The synthetic data authenticity constraint governs the structural
 *   relationship between producers claiming data is synthetic (generated, not
 *   sampled) and consumers relying on those claims for model training,
 *   deployment, and regulatory compliance. This constraint combines genuine
 *   coordination (trustworthy synthetic data enables data-sharing and
 *   privacy-preserving ML) with asymmetric extraction (producers benefit from
 *   information opacity; consumers bear verification cost and distributional
 *   drift risk). The constraint exhibits tangled rope structure across
 *   multiple perspectives: producers and model developers coordinate through
 *   synthetic data standards, but enforcement mechanisms create compliance
 *   burden asymmetry. The theater ratio (0.68) reflects that certification
 *   labor (data cards, documentation, attestation) is extensive but detection
 *   of authenticity violations is rare — the ritual persists through
 *   institutional convention rather than functional verification.
 *   Extractiveness has increased from 0.28 (early synthetic data adoption,
 *   high trust, minimal auditing) to 0.54 (mature market, discovery of
 *   distribution divergence cases, rising audit burden) as the gap between
 *   claimed and actual data fidelity became measurable.
 *
 * KEY AGENTS:
 *   - Synthetic Data Producers: Primary beneficiary (institutional/arbitrage) — capture trust premium and market first-mover advantage through authenticity claims; can shift production cost to consumers via audit burden
 *   - End-User Model Consumers: Primary victim (powerless/trapped) — cannot verify synthetic data authenticity; suffer silent model degradation when distribution drift occurs; no mechanism for accountability
 *   - Compliance Auditors: Secondary victim (moderate/constrained) — must verify authenticity but face resource asymmetry; auditing demands are effectively unlimited; can withdraw audits but face regulatory pressure
 *   - Model Developer Community: Secondary victim (organized/constrained) — benefits from synthetic data as coordination mechanism but faces unequal compliance burden distribution (startups bear higher proportional cost)
 *   - Regulatory Bodies: Institutional stakeholder (institutional/arbitrage) — ostensibly verify authenticity but often rely on producer attestations; maintain theater through certification requirements
 *   - Data Integrity Commons: Abstract victim (powerless/trapped) — field of ML/AI science accumulates models trained on misspecified synthetic data; systematic bias propagates; no mechanism to identify or correct
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(synthetic_data_authenticity, 0.54).
domain_priors:suppression_score(synthetic_data_authenticity, 0.62).
domain_priors:theater_ratio(synthetic_data_authenticity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(synthetic_data_authenticity, extractiveness, 0.54).
narrative_ontology:constraint_metric(synthetic_data_authenticity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(synthetic_data_authenticity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(synthetic_data_authenticity, tangled_rope).
narrative_ontology:human_readable(synthetic_data_authenticity, "Synthetic Data Authenticity Verification Constraint").
narrative_ontology:topic_domain(synthetic_data_authenticity, "data_science/machine_learning/AI_governance").

domain_priors:requires_active_enforcement(synthetic_data_authenticity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(synthetic_data_authenticity, synthetic_data_producers).
narrative_ontology:constraint_beneficiary(synthetic_data_authenticity, model_developers_using_synthetic_data).
narrative_ontology:constraint_victim(synthetic_data_authenticity, downstream_model_users).
narrative_ontology:constraint_victim(synthetic_data_authenticity, regulatory_compliance_bodies).
narrative_ontology:constraint_victim(synthetic_data_authenticity, data_integrity_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END-USER MODEL CONSUMER (SNARE) — Consumer of models trained on synthetic data has no capacity to verify authenticity or detect distribution shifts between claimed synthetic data characteristics and actual training data composition. Trapped by informational asymmetry and inability to audit training pipelines. Bears full extraction cost: models degrade silently in production; accountability flows nowhere.
constraint_indexing:constraint_classification(synthetic_data_authenticity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPLIANCE AUDITOR (TANGLED ROPE) — Regulatory body or internal audit function must verify synthetic data authenticity but faces high cost to conduct statistical audits. Genuine coordination function exists (auditors ensure data quality standards are maintained), but the enforcement mechanism itself becomes extractive: auditors can demand audits indefinitely, creating compliance burden asymmetry. High suppression through demanding resource-intensive verification procedures.
constraint_indexing:constraint_classification(synthetic_data_authenticity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYNTHETIC DATA PRODUCER (ROPE) — Primary beneficiary experiences the constraint as coordination mechanism: declaring synthetic data authenticity enables market exchange, builds trust, and creates value. Producer can arbitrage between markets (sell same synthetic data schema to multiple clients). Net beneficiary through information asymmetry and first-mover trust establishment.
constraint_indexing:constraint_classification(synthetic_data_authenticity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODEL DEVELOPER COMMUNITY (TANGLED ROPE) — Organized resistance (open-source verification frameworks, audit consortia) sees the constraint as solvable through collective action but faces resource constraints and competitive pressure to ship faster than audits can proceed. Gains coordination benefit (trustworthy synthetic data enables collaboration) but bears asymmetric extraction cost (compliance burden distributed unequally — startups vs enterprises).
constraint_indexing:constraint_classification(synthetic_data_authenticity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DATA PROVENANCE CERTIFICATION RITUAL (PITON) — Traditional approaches to synthetic data certification (data sheets, model cards, attestation documents) are largely performative. Producers fill templates; auditors check compliance with templates; nobody verifies the actual statistical properties of the synthetic data. The ritual persists through institutional convention (conferences, funding agencies, regulatory templates) despite low functional verification content. Theater ratio high because certification labor is extensive but detection of authenticity violations is rare.
constraint_indexing:constraint_classification(synthetic_data_authenticity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FUNDAMENTAL LIMIT VIEW (MOUNTAIN) — From a theoretical perspective, the authenticity constraint may appear immutable: synthetic data is, by definition, generated not sampled, so perfect fidelity to claimed distribution is mathematically infeasible under resource constraints. Compression loss is inherent to any finite synthetic dataset. This perspective risks naturalizing what is actually a tradeoff between fidelity cost and verification effort, not an immutable law.
constraint_indexing:constraint_classification(synthetic_data_authenticity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(synthetic_data_authenticity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(synthetic_data_authenticity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(synthetic_data_authenticity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(synthetic_data_authenticity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(synthetic_data_authenticity, TR),
    TR >= 0.70.

:- end_tests(synthetic_data_authenticity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. Producers systematically benefit from authenticity claims they cannot be forced to verify. The asymmetry emerges at scale — early adoption had high trust and low verification cost; mature market reveals distribution divergence cases where producers claimed fidelity they didn't achieve. The extraction is not maximal (0.70+) because open-source alternatives and audit consortia provide some producer discipline and some consumer protection. Suppression (0.62): Moderate-high. Multiple barriers prevent consumers from detecting or redressing authenticity violations: (1) statistical verification requires specialized expertise and expensive audits; (2) distribution drift manifests through model performance decay, not transparent signals; (3) attribution is nearly impossible — which training data source caused the degradation?; (4) legal/contractual frameworks for recourse are underdeveloped. Theater ratio (0.68): High. Data cards and model documentation are labor-intensive compliance artifacts, but empirical studies show they are often filled without reference to actual data characteristics. Certification bodies may not conduct statistical audits; they check documentation completeness. The performative content is high relative to the verification content. The trajectory from 0.42 to 0.68 reflects institutional response to early authenticity concerns: rather than developing robust verification methods, the field added documentation requirements, creating theater that simulates rigor without achieving it.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between producers (who see coordination and value creation) and consumers (who see asymmetric extraction). Secondary gap is between auditors (who see enforceable standards) and developers (who see compliance theater). Tertiary gap is between institutional observers (who see performative certification) and analytical observers (who risk naturalizing verification limits). The gaps are bridged by open-source verification frameworks and audit consortia, which enable organized developers to see a scaffold structure — the constraint appears temporarily solvable through collective action with a sunset as verification methods mature and become standard practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect each agent's structural position relative to the extraction flow. Producers with arbitrage options (can sell to multiple markets, shift audit costs) have low d (0.10-0.20) — they are net beneficiaries. End-users with no alternatives have high d (0.90+) — they are net targets. Auditors with constrained exit (regulatory pressure forces them to audit; cannot refuse) have high d (0.75-0.85). The derived chi values show why producers experience rope/low extraction: low d produces low f(d), dampening their experienced extractiveness. Consumers experience high chi due to high d and trapped exit. The perspectival gap widens as spatial scope increases: at local scope (single organization, closed dataset), verification is feasible and extraction is low; at global scope, verification becomes impossible and extraction approaches maximal. The scope modifier σ(S) scales extractiveness from σ(local)=0.8 to σ(global)=1.2, explaining why global synthetic data markets show higher extraction than local corporate uses.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the coordination function (trustworthy synthetic data enables data-sharing) is genuine but embedded in an asymmetric extraction mechanism (producers benefit from opacity, consumers bear verification cost). The tangled rope classification is not a mislabeling — it correctly captures that both functions exist. The piton perspective identifies institutional inertia: certification theater persists because stakeholders have not yet coordinated on robust statistical verification standards. The scaffold perspective identifies the exit path: as open-source verification tools mature and audit consortia establish baseline statistical thresholds, the theater ratio should decline and extraction should shift from snare toward rope for consumers. The false summit mountain perspective illustrates how analytical reasoning can naturalize institutional arrangements: 'synthetic data authenticity is fundamentally unverifiable' is a rationalization of current weak enforcement, not a mathematical law. Stronger enforcement regimes (mandatory third-party audits, legal recourse for distribution drift, standardized statistical thresholds) would shift the constraint toward rope. The mandatrophy is resolved by recognizing that the perceived immutability is institutional, not physical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statistical_indistinguishability_threshold,
    'What statistical distance (Wasserstein, KL divergence, MMD) between claimed and actual synthetic data distribution constitutes acceptable vs extractive?',
    'Empirical audits comparing claimed synthetic data characteristics to actual distributions; analysis of distribution shift impact on downstream model performance',
    'If threshold is strict (< 0.01): most synthetic data producers fail certification, market contracts; extraction mechanism evident. If threshold is loose (> 0.10): producers evade accountability, snare mechanism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_indistinguishability_threshold, empirical, 'Threshold for acceptable distribution divergence').

omega_variable(
    audit_cost_asymmetry,
    'Is the suppression mechanism structural (cost of auditing is genuinely high) or institutional (audits could be cheaper but certification standards are set to advantage incumbents)?',
    'Comparison of audit costs across organizations; analysis of whether audit burden correlates with firm size or with actual data complexity',
    'If structural: suppression reflects real technical limits; tangled rope classification stands. If institutional: suppression is rent-seeking; classification shifts toward snare for smaller actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_cost_asymmetry, empirical, 'Whether audit cost asymmetry is structural or institutional').

omega_variable(
    synthetic_data_mission_creep,
    'Are producers using ''synthetic data'' labels for data that is actually real but anonymized, or real data with minor perturbations, thereby conflating distinct authenticity problems?',
    'Audit of synthetic data provenance documentation; classification of datasets by generation method (fully synthetic vs anonymized real vs augmented real)',
    'If mission creep is common: authenticity verification breaks down entirely (impossibility of distinguishing categories); extraction mechanism succeeds through categorical ambiguity. If categories are clear: verification problem is tractable; tangled rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(synthetic_data_mission_creep, empirical, 'Label ambiguity between synthetic and anonymized/augmented data').

omega_variable(
    open_source_verification_viability,
    'Can open-source statistical verification frameworks (e.g., synthetic data validating libraries) actually detect distribution drift at production scale, or do they create false confidence through theater?',
    'Comparison of audit results from open-source tools vs specialized audit firms; longitudinal tracking of models trained on ''verified'' synthetic data for distribution shift incidents',
    'If viable: scaffold perspective emerges; distributed auditing could create sunset path for extraction. If theater: open-source tools become compliance theater, increasing suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_verification_viability, empirical, 'Effectiveness of open-source synthetic data verification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(synthetic_data_authenticity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(synth_auth_tr_t0, synthetic_data_authenticity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(synth_auth_tr_t3, synthetic_data_authenticity, theater_ratio, 3, 0.58).
narrative_ontology:measurement(synth_auth_tr_t6, synthetic_data_authenticity, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(synth_auth_be_t0, synthetic_data_authenticity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(synth_auth_be_t3, synthetic_data_authenticity, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(synth_auth_be_t6, synthetic_data_authenticity, base_extractiveness, 6, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(synthetic_data_authenticity, information_standard).
narrative_ontology:affects_constraint(synthetic_data_authenticity, model_training_data_provenance).
narrative_ontology:affects_constraint(synthetic_data_authenticity, ai_regulatory_compliance_audit).

% DUAL FORMULATION NOTE:
% The synthetic data authenticity constraint is upstream of model training data provenance (specific claims about data composition) and downstream of AI regulatory compliance frameworks (which mandate verification). This story addresses the intermediate verification bottleneck; upstream story (model_training_data_provenance) addresses the specific authenticity claims; downstream story (ai_regulatory_compliance_audit) addresses institutional enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(synthetic_data_authenticity, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
