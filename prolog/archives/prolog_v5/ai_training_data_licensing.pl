% ============================================================================
% CONSTRAINT STORY: ai_training_data_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_training_data_licensing, []).

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
 *   constraint_id: ai_training_data_licensing
 *   human_readable: AI Training Data Licensing: Coordination and Asymmetric Extraction
 *   domain: technology/intellectual_property/artificial_intelligence
 *
 * SUMMARY:
 *   AI training data licensing represents a structural clash between the
 *   legitimate coordination problem of data sourcing and asymmetric
 *   extraction favoring model developers. The constraint arises from the
 *   technical requirement that large language models require massive amounts
 *   of human-created text, images, and other content for training, combined
 *   with distributed copyright ownership across millions of individual
 *   creators and organizations. Current licensing regimes lack clear legal
 *   standards (fair use doctrine is unresolved for AI), enforcement
 *   mechanisms are weak (unauthorized scraping persists despite licensing
 *   attempts), and power asymmetries are extreme (millions of individual
 *   creators negotiate with a handful of model developers). The constraint's
 *   extractiveness has increased over the interval (0.35 → 0.58) as model
 *   developers have accumulated data without proportional creator
 *   compensation. Theater ratio has remained relatively stable and low
 *   (0.48–0.52) because licensing agreements are substantive, not purely
 *   performative—but the lopsided negotiating power makes the agreements
 *   themselves extractive instruments.
 *
 * KEY AGENTS:
 *   - Original Content Creators: Primary victims (powerless/trapped) — writers, artists, photographers whose work is used without consent or compensation; no exit options
 *   - Rights-Conscious Publishers: Secondary victims (moderate/constrained) — studios, publishers negotiating licensing terms; face coordination benefits but asymmetric extraction
 *   - AI Model Developers: Primary beneficiaries (institutional/arbitrage) — OpenAI, Google, Meta, etc.; capture value from data aggregation; experience licensing as coordination mechanism
 *   - Regulators / Governments: Institutional actors (institutional/constrained) — coordinate between innovation incentives and creator protection; subject to regulatory capture by tech lobbies
 *   - Copyright Legal System: Institutional mechanism (institutional/arbitrage) — fair use doctrine unresolved; licensing functions as de facto law
 *   - Open-Data Movement: Organized agents (organized/mobile) — creative commons, public domain repositories building alternative pathways with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees genuine coordination problem overlaid with systematic extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_training_data_licensing, 0.58).
domain_priors:suppression_score(ai_training_data_licensing, 0.65).
domain_priors:theater_ratio(ai_training_data_licensing, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_training_data_licensing, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_training_data_licensing, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_training_data_licensing, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_training_data_licensing, tangled_rope).
narrative_ontology:human_readable(ai_training_data_licensing, "AI Training Data Licensing: Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(ai_training_data_licensing, "technology/intellectual_property/artificial_intelligence").

domain_priors:requires_active_enforcement(ai_training_data_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_training_data_licensing, ai_developers).
narrative_ontology:constraint_beneficiary(ai_training_data_licensing, model_licensors).
narrative_ontology:constraint_victim(ai_training_data_licensing, content_creators).
narrative_ontology:constraint_victim(ai_training_data_licensing, original_rights_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL CONTENT CREATOR (SNARE) — Artists, writers, photographers whose work is scraped without consent or compensation cannot exit the constraint. Their creative output is extracted globally. No alternatives exist for non-participation; licensing is not offered. Maximum suppression through network effects: refusing to publish online is economic death. The extraction is pure — AI developers benefit from free training data; creators bear costs with no coordination benefit.
constraint_indexing:constraint_classification(ai_training_data_licensing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RIGHTS-CONSCIOUS PUBLISHER (TANGLED ROPE) — Publishers and studios that negotiate licensing terms with AI developers experience both coordination and extraction. They solve a real problem: preventing unauthorized model training on proprietary content. But asymmetric extraction persists — AI developers have aggregated market power; individual publishers are atomized. High suppression: costs of enforcement litigation are prohibitive; exit means market marginalization. Benefits exist (licensing revenue, negotiating position in distribution) alongside extraction.
constraint_indexing:constraint_classification(ai_training_data_licensing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: AI DEVELOPER / MODEL CREATOR (ROPE) — Experiences the licensing constraint as coordination mechanism. Licensing agreements establish data provenance, reduce legal exposure, and create clear property boundaries for models. From the developer's perspective, the constraint solves coordination problems: which data can be used, which cannot, liability assignments. Net beneficiary with arbitrage options — can license data, use synthetic data, license pre-trained models, or negotiate terms. Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(ai_training_data_licensing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATOR / GOVERNMENT (TANGLED ROPE) — States coordinate legitimate interests (innovation incentives, creator protection, public access to AI capabilities) while experiencing extraction through regulatory capture. Tech lobbies influence IP policy favoring low licensing costs. Suppression is institutional: path dependency on existing copyright frameworks, tension between innovation and creator rights, enforcement capacity constraints. Benefits exist (tax revenue, innovation cluster development) alongside extraction (regulatory capture by dominant firms).
constraint_indexing:constraint_classification(ai_training_data_licensing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COPYRIGHT LEGAL FRAMEWORK (PITON) — Fair use doctrine and existing copyright law are invoked but theatrically. Courts have not clearly ruled on whether training AI systems constitutes 'transformative use.' The legal framework persists through institutional inertia despite ambiguity. Theater ratio is moderate-to-low because litigation outcomes are still being established. As a piton, the constraint represents a degraded enforcement mechanism: the law exists but its application to AI training is unresolved, so licensing agreements function as de facto law.
constraint_indexing:constraint_classification(ai_training_data_licensing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN-DATA MOVEMENT (SCAFFOLD) — Creative commons licensing, public domain commitment, and consensual training data repositories represent a temporary alternative pathway. Organized creators can opt into open licensing with sunset logic: if compensation models for content creators mature (micropayments, data valuation markets), the extraction mechanism loses force. Current suppression is moderate because open-data advocates have collective agency and see exit paths. Sunset: 10-15 years for compensation frameworks to scale.
constraint_indexing:constraint_classification(ai_training_data_licensing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational vantage, AI training requires massive data aggregation (genuine coordination problem), but current licensing regimes systematically undercompensate original creators while centralizing value in model developers. The constraint exhibits real coordination function (establishing data provenance, liability, quality standards) alongside systematic extraction (power asymmetry favoring aggregators). Classification: tangled rope. The structure is not an immutable natural law but a contingent institutional arrangement where coordination mechanisms have been co-opted for extraction.
constraint_indexing:constraint_classification(ai_training_data_licensing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_training_data_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_training_data_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_training_data_licensing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_training_data_licensing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_training_data_licensing, TR),
    TR >= 0.70.

:- end_tests(ai_training_data_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not extreme. Model developers extract substantial value from unpaid or underpaid training data. The 0.58 value reflects that some creators do receive compensation (through licensing deals, though inadequate) and that the coordination problem is partially genuine—aggregating terabytes of data is a real technical challenge. However, the asymmetry means most value flows to developers. Suppression (0.65): High. Creators face multiple suppression mechanisms: (1) technical — scraping happens at scale and is difficult to detect; (2) economic — refusing to publish online is not viable for professional creators; (3) legal — fair use doctrine is ambiguous, so licensing appears optional even when creators object; (4) collective action — millions of individual creators cannot coordinate negotiating power. Theater ratio (0.48): Moderate-low. Licensing agreements are substantive (they establish terms, liability, compensation), not purely performative. But theater increases when licensing terms are offered at token rates (below creator opportunity cost) while framed as 'reasonable compensation.' The theater is in the *justification* that licensing is fair, not in the mechanism itself.
 *
 * PERSPECTIVAL GAP:
 *   The gap between developer and creator perspectives is maximal. Developers see licensing as a coordination mechanism that establishes data provenance and legal clarity (Rope perspective). Creators see the same constraint as pure extraction—their work is used without consent or fair compensation, and they have no exit option (Snare perspective). The regulator occupies an intermediate position: trying to preserve both innovation incentives (favoring loose licensing) and creator protection (favoring strict licensing), but subject to regulatory capture by dominant model developers. The analytical observer sees the genuine coordination problem (data aggregation is technically complex) but notes that the coordination mechanism has been captured: licensing terms systematically undercompensate creators relative to the value extracted.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for each institutional perspective: AI Developers (beneficiary + arbitrage) → d ≈ 0.15 → f(d) ≈ -0.01 → effective extraction runs toward them (negative from their perspective). Original Creators (victim + trapped) → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. Rights-conscious Publishers (victim + constrained; also partial beneficiary) → d ≈ 0.65 → f(d) ≈ 1.00 → moderate-high experienced extraction. Regulators (mixed beneficiary/victim + constrained + capture risk) → d ≈ 0.50 → f(d) ≈ 0.65 → moderate experienced extraction. The scope modifier σ(S) = 1.2 (global scope) amplifies all χ values, reflecting that AI training data is inherently global—creators in any jurisdiction are potential sources.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved through the tangled rope classification, which acknowledges that genuine coordination function coexists with systematic extraction. AI training data licensing is NOT pure coordination (Rope) because the power asymmetry is structural and licensing terms are coercive—creators have no meaningful negotiating power. It is NOT pure extraction (Snare) because coordination problems are real and some creators do benefit from licensing revenue. The constraint's function is to coordinate data sourcing while extracting value from powerless creators through a permission-based regime that appears consensual but operates under structural coercion. The mandatrophy dissolves when we recognize that both functions are present: the coordination legitimizes the extraction by framing it as a 'fair licensing mechanism' when in fact the licensing mechanism is the extraction apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_legal_threshold,
    'Do court rulings establish AI training as transformative fair use, or does licensing become required?',
    'Landmark litigation outcomes (US, EU, UK); legislative clarification of AI-specific copyright exceptions; appellate court decisions on training-use transformativeness',
    'If fair use is established: extraction mechanism dissolves; constraint reclassifies toward Rope for all institutional perspectives. If licensing required: extraction persists; snare classification holds for creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_legal_threshold, empirical, 'Legal determination of AI training as fair use vs licensing requirement').

omega_variable(
    creator_compensation_feasibility,
    'Can micropayment, revenue-sharing, or data valuation markets scale to compensate millions of creators proportionally?',
    'Technical feasibility analysis of blockchain-based micropayments, platform implementation of content-creator revenue sharing, cost-per-creator at scale',
    'If feasible: scaffold sunset becomes real; open-data pathways mature as viable alternatives. If infeasible: creators remain trapped; snare classification persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_compensation_feasibility, empirical, 'Feasibility of creator compensation mechanisms at scale').

omega_variable(
    licensing_enforcement_sufficiency,
    'Can licensing agreements prevent unauthorized data use, or does technical copying capability outpace enforcement?',
    'Empirical measurement of unlicensed vs licensed training in published models; litigation outcomes against violators; watermarking/provenance tracking effectiveness',
    'If enforcement effective: licensing becomes meaningful coordination mechanism; tangled rope classification holds. If ineffective: licensing is theater; extraction persists; snare classification for all non-developers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_enforcement_sufficiency, empirical, 'Whether licensing agreements can effectively prevent unauthorized data use').

omega_variable(
    power_asymmetry_structural,
    'Is the power imbalance between atomized creators and concentrated model developers structural or contingent on market consolidation?',
    'Market concentration analysis (Herfindahl index of model developers); entry barrier analysis for new developers; comparison of licensing negotiating power pre- and post-consolidation',
    'If structural: tangled rope is stable classification; extraction mechanism is baked into coordination logic. If contingent on consolidation: market fragmentation could shift toward rope; constraint type becomes politically variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_structural, conceptual, 'Whether power asymmetry is structural or contingent on market consolidation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_training_data_licensing, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aitdl_tr_t0, ai_training_data_licensing, theater_ratio, 0, 0.52).
narrative_ontology:measurement(aitdl_tr_t3, ai_training_data_licensing, theater_ratio, 3, 0.5).
narrative_ontology:measurement(aitdl_tr_t6, ai_training_data_licensing, theater_ratio, 6, 0.47).
narrative_ontology:measurement(aitdl_tr_t9, ai_training_data_licensing, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(aitdl_be_t0, ai_training_data_licensing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aitdl_be_t3, ai_training_data_licensing, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(aitdl_be_t6, ai_training_data_licensing, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(aitdl_be_t9, ai_training_data_licensing, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_training_data_licensing, resource_allocation).
narrative_ontology:affects_constraint(ai_training_data_licensing, copyright_legal_ambiguity).
narrative_ontology:affects_constraint(ai_training_data_licensing, creator_compensation_failure).
narrative_ontology:affects_constraint(ai_training_data_licensing, model_developer_market_concentration).

% DUAL FORMULATION NOTE:
% AI training data licensing decomposes into three related constraints: (1) copyright_legal_ambiguity (ε ≈ 0.42): whether AI training is fair use; (2) creator_compensation_failure (ε ≈ 0.68): whether compensation mechanisms can scale; (3) model_developer_market_concentration (ε ≈ 0.55): power asymmetry in negotiation. The training data licensing story represents the coordination-extraction hybrid that emerges from the interaction of all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_training_data_licensing, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
