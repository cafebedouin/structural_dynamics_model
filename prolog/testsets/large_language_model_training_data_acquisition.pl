% ============================================================================
% CONSTRAINT STORY: large_language_model_training_data_acquisition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_large_language_model_training_data_acquisition, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: large_language_model_training_data_acquisition
 *   human_readable: LLM Training Data Acquisition Asymmetry
 *   domain: machine_learning/digital_economy/intellectual_property
 *
 * SUMMARY:
 *   Large language models require massive text corpora to achieve
 *   state-of-the-art performance. The constraint structures an asymmetry
 *   between data provision and value capture: content creators publish freely
 *   on the open internet, and model developers freely scrape this content
 *   without compensation, licensing, or explicit consent. The extracted value
 *   is enormous — a single transformer model trained on billions of tokens
 *   can be worth billions of dollars in market capitalization or monopolistic
 *   advantage. The content creators receive zero compensation. This is not a
 *   coordination failure or a side effect; it is a structural extraction
 *   mechanism designed to maximize developer profit by externalizing training
 *   costs onto the creative commons. The constraint's suppression (0.68)
 *   reflects multiple barriers: technical (creators cannot detect or prevent
 *   scraping), legal (fair use doctrine remains contested), and economic
 *   (individual creators lack capital to litigate or negotiate). Theater
 *   (0.55) reflects the post-hoc rationalization: developers claim training
 *   data is 'freely available' (it is), that fair use justifies unconsented
 *   use (it is contested), and that compensation mechanisms are 'technically
 *   infeasible' (they are not, but are more expensive than free scraping).
 *   The constraint exhibits a clear snare signature from the perspective of
 *   individual creators and a rope signature from model developers — a
 *   perspectival gap revealing the asymmetry.
 *
 * KEY AGENTS:
 *   - Content Creators: Individual writers, bloggers, social media authors (powerless/trapped) — bear full extraction cost, have no exit or negotiation capacity
 *   - Creative Professionals: Freelance writers, illustrators, musicians (moderate/constrained) — face career penalty for withdrawal from visibility; can attempt legal action but at high cost
 *   - Copyright Holders: Publishers, media organizations, creative collectives (organized/constrained) — have legal standing to litigate but face fragmented litigation, uncertain outcomes, high costs
 *   - Model Developers: Companies building foundation models (institutional/arbitrage) — primary beneficiaries; experience constraint as efficient data procurement mechanism
 *   - Model Deployers: Platforms, API providers licensing trained models (institutional/arbitrage) — secondary beneficiaries; benefit from low-cost access to advanced capabilities
 *   - Regulatory Bodies: Government agencies, courts setting fair use precedent (institutional/analytical) — shape the constraint through legislative and judicial action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(large_language_model_training_data_acquisition, 0.62).
domain_priors:suppression_score(large_language_model_training_data_acquisition, 0.68).
domain_priors:theater_ratio(large_language_model_training_data_acquisition, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(large_language_model_training_data_acquisition, extractiveness, 0.62).
narrative_ontology:constraint_metric(large_language_model_training_data_acquisition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(large_language_model_training_data_acquisition, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(large_language_model_training_data_acquisition, snare).
narrative_ontology:human_readable(large_language_model_training_data_acquisition, "LLM Training Data Acquisition Asymmetry").
narrative_ontology:topic_domain(large_language_model_training_data_acquisition, "machine_learning/digital_economy/intellectual_property").

domain_priors:requires_active_enforcement(large_language_model_training_data_acquisition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(large_language_model_training_data_acquisition, '842df12f-41b8-457b-9afa-8c42bfa1fdcb').
narrative_ontology:cs_kernel_codification('842df12f-41b8-457b-9afa-8c42bfa1fdcb', distributed).
narrative_ontology:cs_authority_grounding('842df12f-41b8-457b-9afa-8c42bfa1fdcb', extraction).
narrative_ontology:cs_created_at('842df12f-41b8-457b-9afa-8c42bfa1fdcb', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(large_language_model_training_data_acquisition, model_developers).
narrative_ontology:constraint_beneficiary(large_language_model_training_data_acquisition, model_deployers).
narrative_ontology:constraint_beneficiary(large_language_model_training_data_acquisition, downstream_service_providers).
narrative_ontology:constraint_victim(large_language_model_training_data_acquisition, content_creators).
narrative_ontology:constraint_victim(large_language_model_training_data_acquisition, copyright_holders).
narrative_ontology:constraint_victim(large_language_model_training_data_acquisition, creative_professionals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CREATOR (SNARE) — Individual writers, artists, journalists lack capacity to prevent or negotiate data scraping. Exit is impossible: once published, content is scraped globally. No alternatives to participation in the digital commons without complete withdrawal. Maximum extraction, maximum suppression.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREATIVE PROFESSIONAL (SNARE) — Freelance writers, illustrators, musicians face career penalty for withdrawing from visibility (social media, portfolio sites). Can attempt contractual opt-out but face fragmented, legally uncertain enforcement. High cost to exit — remaining options (paywalls, closed platforms) reduce income and reach. Substantial extraction with some residual agency.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: COPYRIGHT HOLDER / PUBLISHER (TANGLED ROPE) — Publishers and media organizations have legal standing and enforcement capacity. Can sue (high cost, uncertain outcome), negotiate licensing (limited success), or lobby for regulation. Organized power constrains but doesn't eliminate extraction. Partial coordination benefit exists: some publishers negotiate data partnerships, creating licensing revenue and market differentiation. Mixed extraction-coordination dynamic.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MODEL DEVELOPER (ROPE) — Developers experience the constraint as a coordination mechanism: assembling a representative training corpus is a genuine technical problem, and open data (web crawls, public repositories) solve it efficiently. Can arbitrage between training-data procurement costs and model value. Net beneficiary experiencing minimal extraction cost.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MODEL DEPLOYER (ROPE) — Deployers (tech platforms, API providers) experience the constraint as enabling low-cost access to advanced capabilities. License trained models from developers and monetize through API access. Coordination mechanism: developers and deployers cooperate to extract value from the corpus without paying the upstream source.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a generational systemic view, LLM training represents a genuine coordination advance (enabling powerful language capabilities) layered atop systematic content appropriation without compensation. The coordination function is real (better models benefit downstream users); the extraction is structural (value flows to model developers, not to data sources). Classic Tangled Rope signature: both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(large_language_model_training_data_acquisition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(large_language_model_training_data_acquisition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(large_language_model_training_data_acquisition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High but not maximum. The constraint extracts significant value from creators (their intellectual property and labor are incorporated into models worth billions) but falls short of total expropriation because some creators retain legal claims, some licensing negotiations occur, and some developers make voluntary commitments to partial consent-based acquisition. The value of the extracted content is enormous relative to creator compensation (zero), but the structural mechanism is not as total as a chattel slavery snare would be. Suppression (0.68): High. Multiple barriers prevent exit or negotiation: (1) technical: creators cannot detect or prevent automated scraping; (2) legal: fair use remains contested, and individual creators cannot afford litigation; (3) economic: coordinated action requires solving a massive collective action problem (millions of creators); (4) informational: many creators are unaware their work is being scraped and monetized. Theater (0.55): Moderate. Post-hoc rationalization emphasizes 'open data' and 'fair use,' framing the extraction as natural or necessary. This theater is not as dense as a piton (which would have theater ≥ 0.70) because the extraction mechanism is still actively defended and justified, not merely maintained through inertia. Theater ratio increasing over time (0.30 → 0.55) reflects growing post-hoc justification as legal challenges mount.
 *
 * PERSPECTIVAL GAP:
 *   The snare/rope perspectival gap is the diagnostic signature of this constraint. Individual creators and copyright organizations perceive extraction (snare or tangled_rope). Model developers perceive coordination (rope) — they genuinely are solving a technical problem of data collection. The gap reveals that the extraction is structurally embedded in the coordination mechanism itself: the 'problem' being solved (obtaining training data) is defined to externalize costs onto creators. A different solution (licensing, consent, compensation) would solve the same technical problem at higher cost to developers. The analytical observer sees both: the coordination benefit (better models) and the systematic extraction (value flow direction). The constraint is snare-not-rope precisely because creators cannot negotiate alternative arrangements or exit; the rope experience of developers is parasitic on the snare experience of creators.
 *
 * DIRECTIONALITY LOGIC:
 *   Model developers and deployers are beneficiaries with arbitrage exit options (d ≈ 0.05–0.15), deriving negative or minimal effective extraction. They experience the constraint as enabling their business model — a coordination benefit. Content creators are victims with trapped or constrained exit options (d ≈ 0.85–0.95), deriving maximum effective extraction. Copyright organizations are organized victims with constrained options (d ≈ 0.60–0.70), deriving high extraction but with partial capacity for legal response. The analytical observer (d ≈ 0.72 canonical for observer position) sees the full structure: beneficiaries with optimal exit positions; victims with blocked exits. The directionality distribution is skewed toward extraction — the constraint's structural design favors beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLVED: The constraint exhibits the snare signature (ε > 0.46, χ > 0.66, suppression > 0.60) with no coordination function present. The rope experiences reported by developers are not coordination — they are one party's experience of extraction masked as efficiency. True coordination requires mutual benefit or at least mutual consent to the arrangement. Creators have provided neither. The constraint's entire function is to extract value from creators without paying them, and its existence relies entirely on suppressing (legal, technical, economic) alternatives to unconsented use. The false summit risk is minimal here — no one seriously claims training data acquisition is a natural law of physics. The mandatrophy resolves by showing that snare is the only defensible classification once you account for the suppression mechanisms and the beneficiary/victim asymmetry. The rope classification is a perspectival artifact of the beneficiary's subjective experience, not a structural property.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_doctrine_contestation,
    'Does transformer-based training constitute fair use under copyright law, or does it constitute unauthorized derivative work?',
    'Jurisdictional precedent via litigation (US, EU, UK courts); legislative clarification of fair use boundaries for AI training',
    'If fair use: extraction becomes legally justified, reclassifying constraint toward Rope or Scaffold (transitional). If not fair use: snare classification strengthens; extraction becomes legally extractive (tortious), enabling organized victim responses. Entire constraint type depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fair_use_doctrine_contestation, conceptual, 'Fair use status of transformer training on copyrighted text').

omega_variable(
    market_power_concentration_dynamics,
    'As model training data becomes scarcer and legally contested, does market power consolidate further toward largest developers (who have capital for licensing and litigation), or do alternative data sources (synthetic, consented, synthetic) reduce the asymmetry?',
    'Tracking of model training data provenance and licensing terms across developer cohorts; measurement of market share concentration in model development; emergence and adoption rates of synthetic/consented data frameworks',
    'If concentration increases: snare persists and deepens (high barriers to entry prevent new competitors). If alternatives emerge: potential transition to Scaffold (sunset as licensing norms mature) or Tangled Rope (mixed coordination-extraction with negotiation frameworks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_power_concentration_dynamics, empirical, 'Whether data scarcity increases or decreases market concentration').

omega_variable(
    consent_and_compensation_feasibility,
    'Is individual-level compensation for training data use technically and economically feasible, or does per-creator micro-payment create more overhead and extraction than the current regime?',
    'Cost-benefit analysis of distributed payment systems vs current free acquisition; pilot implementations of consent-based data acquisition; measurement of administrative overhead vs compensation per creator',
    'If feasible: institutional pressure toward Scaffold (temporary free scraping transits to licensed/consented data) or negotiated Tangled Rope. If infeasible: snare persists and becomes naturalized as economically necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_and_compensation_feasibility, empirical, 'Feasibility of per-creator compensation and consent mechanisms').

omega_variable(
    synthesized_data_sufficiency,
    'Can large language models trained primarily on synthetic or iteratively-filtered human-consented data match or exceed the performance of models trained on unconstrained web scraping?',
    'Comparative benchmarking of synthetic-data-trained models vs web-crawl models; measurement of performance gaps across standard evaluation suites; tracking of commercial viability of consent-based alternatives',
    'If synthetic data sufficient: snare constraint degrades toward Scaffold (sunset mechanisms emerge as alternatives become viable). If significant gaps remain: snare persists (extraction is ''necessary'' for capability). This is the empirical hinge for whether the constraint is structurally inevitable or structurally chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthesized_data_sufficiency, empirical, 'Whether synthetic data can substitute for unconstrained web scraping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(large_language_model_training_data_acquisition, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(llm_tda_tr_t0, large_language_model_training_data_acquisition, theater_ratio, 0, 0.3).
narrative_ontology:measurement(llm_tda_tr_t3, large_language_model_training_data_acquisition, theater_ratio, 3, 0.42).
narrative_ontology:measurement(llm_tda_tr_t6, large_language_model_training_data_acquisition, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(llm_tda_be_t0, large_language_model_training_data_acquisition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(llm_tda_be_t3, large_language_model_training_data_acquisition, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(llm_tda_be_t6, large_language_model_training_data_acquisition, base_extractiveness, 6, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(llm_tda_su_t0, large_language_model_training_data_acquisition, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(llm_tda_su_t3, large_language_model_training_data_acquisition, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(llm_tda_su_t6, large_language_model_training_data_acquisition, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(large_language_model_training_data_acquisition, resource_allocation).
narrative_ontology:affects_constraint(large_language_model_training_data_acquisition, copyright_enforcement_technological_asymmetry).
narrative_ontology:affects_constraint(large_language_model_training_data_acquisition, platform_attention_extraction).
narrative_ontology:affects_constraint(large_language_model_training_data_acquisition, creative_professional_income_precarity).

% DUAL FORMULATION NOTE:
% LLM training data acquisition is downstream of platform-driven content aggregation (Constraint: platform_attention_extraction) and upstream of copyright enforcement failures (Constraint: copyright_enforcement_technological_asymmetry). The three constraints form a family: platforms aggregate creator content for engagement metrics; model developers scrape aggregated content without compensation; copyright holders lack enforcement mechanisms against computational scraping. Each constraint has distinct ε values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(large_language_model_training_data_acquisition, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
