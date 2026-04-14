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
 *   Large language models require vast amounts of text data to train
 *   effectively. The constraint structures the asymmetry between data
 *   provision (unpaid, uncontracted, often non-consenting content creators)
 *   and value capture (concentrated among model developers and deployers).
 *   This creates a classic extraction mechanism: content creators'
 *   intellectual property and labor are incorporated into models without
 *   compensation, licensing, or contractual consent. The constraint appears
 *   as coordination to frontier labs (standardized training datasets,
 *   interoperable models) but as pure extraction to powerless creators with
 *   no exit option. Suppression is high: copyright law is ambiguous on
 *   training-data fair use, terms of service prohibit scraping opt-outs, and
 *   the scale of infringement makes individual enforcement impossible.
 *   Extractiveness has increased from 0.48 to 0.62 over the interval as model
 *   developers have scaled scraping and the economic value of trained models
 *   has grown while creator compensation has remained near zero. Theater
 *   ratio is moderate (0.45) because the constraint operates with relatively
 *   low performative overhead — scraping is functionally effective, not
 *   ritualistic. The emerging regulatory and collective-licensing
 *   alternatives (perspective 4: scaffold, perspective 6: regulatory
 *   coalition) represent genuine sunset pathways if they can scale and
 *   achieve cost-competitiveness with free scraping.
 *
 * KEY AGENTS:
 *   - Content Creators: Primary victims (powerless/trapped) — writers, artists, journalists, and knowledge workers whose creative output is scraped without consent or compensation
 *   - LLM Developers (Frontier Labs): Primary beneficiaries (institutional/arbitrage) — labs with resources to train frontier models; can afford licensing but benefit from free data; experience as coordination infrastructure
 *   - LLM Developers (Smaller Companies): Secondary beneficiary/victim (moderate/constrained) — depend on free scraping for viability; cannot afford licensing; experience mixed coordination and extraction
 *   - Platforms and Publishers: Secondary victim (institutional/constrained) — host the data; face difficult choices between litigation (costly, uncertain) and renegotiation (gives AI labs leverage)
 *   - Open Data Governance Movement: Organized alternative (organized/constrained) — building consent-based and licensed data pathways with sunset logic; currently nascent but structurally sound
 *   - Regulatory Coalition: Enforcement actors (organized/arbitrage) — EU, copyright offices, digital rights advocates setting consent and compensation rules; face global coordination challenges
 *   - Copyright System: Institutional mechanism (institutional/arbitrage) — persists through convention but functionally degraded for training-data enforcement (piton classification)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent choice to scrape without consent as an immutable property of knowledge creation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(large_language_model_training_data_acquisition, 0.62).
domain_priors:suppression_score(large_language_model_training_data_acquisition, 0.68).
domain_priors:theater_ratio(large_language_model_training_data_acquisition, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(large_language_model_training_data_acquisition, extractiveness, 0.62).
narrative_ontology:constraint_metric(large_language_model_training_data_acquisition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(large_language_model_training_data_acquisition, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(large_language_model_training_data_acquisition, tangled_rope).
narrative_ontology:human_readable(large_language_model_training_data_acquisition, "LLM Training Data Acquisition Asymmetry").
narrative_ontology:topic_domain(large_language_model_training_data_acquisition, "machine_learning/digital_economy/intellectual_property").

domain_priors:requires_active_enforcement(large_language_model_training_data_acquisition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(large_language_model_training_data_acquisition, llm_developers).
narrative_ontology:constraint_beneficiary(large_language_model_training_data_acquisition, model_deployers).
narrative_ontology:constraint_victim(large_language_model_training_data_acquisition, content_creators).
narrative_ontology:constraint_victim(large_language_model_training_data_acquisition, public_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CREATOR (SNARE) — Writers, artists, journalists, and knowledge workers cannot escape the constraint: their creative output is scraped without consent, compensation, or contractual recourse. No material exit option exists. Suppression is total: terms of service prohibit opt-out, legal ambiguity around copyright and fair use prevents enforcement, and the scale of scraping makes individual litigation impossible. Experiences pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALLER AI COMPANY (TANGLED ROPE) — Constrained by data acquisition costs and legal risk. Smaller firms depend on free scraping for training data; licensing would be prohibitively expensive. Exit option is available (use only licensed data) but carries high cost. Also experiences genuine coordination benefit: access to diverse data improves model quality, which benefits all downstream consumers. Mixed experience of extraction and coordination.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRONTIER AI LAB (ROPE) — Institutional beneficiary with arbitrage options. Can afford licensing, private data arrangements, or targeted scraping. Experiences the constraint as coordination mechanism: standardized data formats, common knowledge sources, and interoperable training datasets enable collaboration and model improvement. Net beneficiary with genuine exit — can switch to licensed data without prohibitive cost. Perceives the constraint as a coordination infrastructure.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN DATA GOVERNANCE MOVEMENT (SCAFFOLD) — Organized agents (licensing frameworks like CreativeCommons-compatible training data, synthetic data initiatives, federated learning pilots) are building alternative pathways for data access that include creator consent and compensation. These alternatives have built-in sunset logic: as licensing infrastructure matures and synthetic data quality improves, the free-scraping extraction mechanism loses competitive advantage. Current suppression is high (licensing frameworks still nascent), but organized agents see clear exit path.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM / DATA HOST (TANGLED ROPE) — Platforms and publishers (social media companies, news outlets, academic repositories) face constrained choices: they could sue scrapers (high cost, uncertain outcome due to fair-use ambiguity) or renegotiate terms with AI labs (costly, gives AI labs leverage). Also experience genuine coordination benefit from AI-powered features (content recommendation, search enhancement). Extraction flow: some of the AI-generated value flows back as platform integration contracts, but insufficient to offset data contribution.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY COALITION (TANGLED ROPE) — EU AI Act, digital copyright proposals, and consent-based data frameworks represent organized enforcement attempts. These actors have arbitrage: they can set rules and enforce them within jurisdiction. Also coordinate genuine benefit: establishing clear norms around data use reduces uncertainty for all parties. But enforcement faces global friction (US-based labs, non-compliance incentives) and must balance creator protection against innovation. High suppression comes from global coordination barriers, not from powerlessness.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: COPYRIGHT SYSTEM (PITON) — Traditional copyright enforcement for text/images used in training data is substantially performative: legal mechanisms exist but are unclear in application (fair use ambiguity), expensive to enforce (litigation costs), and difficult to scale (mass infringement). The copyright system persists through institutional inertia — it's the default intellectual property framework — despite low effectiveness in preventing or compensating for training data use. Theater ratio high because copyright notices and takedown requests are ritual-like: they persist despite acknowledged ineffectiveness. The system maintains itself through authority and convention rather than demonstrated function.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, data scarcity is an immutable feature of knowledge creation: to train models on human knowledge, some agents must provide data. The bottleneck between data holders and model builders is a fundamental structural property of knowledge production, not a contingent institutional arrangement. Statistical patterns cannot be learned without access to data patterns. However, the structural data contradicts this — scraping without consent is a contingent choice, not a law of nature. The 'inevitable extraction' framing naturalizes a policy choice.
constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(large_language_model_training_data_acquisition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(large_language_model_training_data_acquisition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(large_language_model_training_data_acquisition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(large_language_model_training_data_acquisition, TR),
    TR >= 0.70.

:- end_tests(large_language_model_training_data_acquisition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. Content creators experience significant extraction: their creative output generates economic value in trained models, and they receive zero direct compensation. However, the extractiveness is not maximal (0.72+) because: (1) some creators benefit indirectly through improved AI services, (2) some creators accept scraping as a norm or cost of digital publication, and (3) legal liability is still ambiguous, leaving escape routes theoretically open. The value of 0.62 reflects the actual asymmetry of current practice — systematic unpaid data extraction at scale — without assuming either total helplessness or total value capture. Suppression (0.68): High. Multiple barriers prevent content creator exit: (1) Copyright law ambiguity around fair use makes opt-out legally uncertain. (2) Terms of service for platforms typically prohibit scraping, but enforcement applies to scrapers, not creators. (3) Scale makes individual litigation infeasible; aggregate damages would be needed but are difficult to calculate. (4) Creator knowledge of scraping is asymmetric — most creators do not know their work is being used for training. (5) No practical consent mechanism exists — creators cannot choose to opt-in to specific models or opt-out of scraping generally. Theater ratio (0.45): Moderate. The constraint's mechanism is relatively direct and functional, not heavily ritualistic. Scraping works; it produces usable training data. However, theater is non-zero because: (1) Copyright frameworks (takedown notices, fair-use claims) are largely performative — they persist as a legal ritual but do not actually prevent training-data incorporation. (2) Terms-of-service enforcement against scrapers is theatrical — the industry knows enforcement is incomplete and widespread. (3) Claimed type (tangled_rope): The constraint exhibits both coordination (genuine improvement in data standardization and model interoperability) and asymmetric extraction (value flows disproportionately toward model developers). Both are present. Not pure extraction (snare) because the coordination function is real. Not pure coordination (rope) because suppression and extractiveness are both high.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Frontier labs (perspective 3: rope) experience the constraint as coordination infrastructure — access to standardized, diverse training data improves model quality for everyone. Content creators (perspective 1: snare) experience the constraint as pure extraction with no exit — their work is incorporated without consent or compensation. Smaller companies (perspective 2: tangled_rope) occupy the middle: they benefit from data access (coordination) but are also suppressed by inability to afford licensing (extraction). Platforms (perspective 5: tangled_rope, institutional) recognize genuine coordination (AI features improve their services) but also experience extraction (data is taken without renegotiated revenue share). Regulatory and governance actors (perspective 4: scaffold, perspective 6: tangled_rope) see the constraint as a temporary coordination problem with a solvable exit pathway — alternative licensing frameworks and synthetic data can replace free scraping as models mature. The copyright system (perspective 7: piton) sees its own degradation — the traditional intellectual property framework is inadequate for the data-extraction scale, but persists through convention. The analytical observer (perspective 8: mountain, false summit) risks naturalizing the contingent institutional choice (scrape without consent) as an immutable property of how knowledge works. This is a false summit: scarcity of training data is real, but the choice to acquire it via scraping without consent is a policy decision, not a law of nature. The gap between perspective 3 (rope, frontier labs, d ≈ 0.05, χ ≈ -0.12) and perspective 1 (snare, creators, d ≈ 0.95, χ ≈ 1.42) is diagnostic of asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural relationship: beneficiary or victim, power level, and exit options. Content creators (powerless/trapped) have d ≈ 0.95 (near 1.0 = full target). They cannot exit, receive no benefits, and bear full cost. LLM developers (institutional/arbitrage) have d ≈ 0.05 (near 0.0 = full beneficiary). They have exit options (can license data), receive full benefits (trained models), and bear minimal cost. Smaller companies (moderate/constrained) have d ≈ 0.65 (symmetric extraction-coordination mix). They experience both suppression (constrained exit) and benefit (access to diverse training data). Platforms (institutional/constrained) have d ≈ 0.55 because they host the data and lose direct value but gain secondary benefits through AI integration. The regulatory coalition (organized/arbitrage) has d ≈ 0.40 — they have enforcement capacity and arbitrage options, but face global coordination friction. The copyright system (institutional/arbitrage) has d ≈ 0.08 because it benefits from the current arrangement (maintains relevance as the framework) with minimal cost (enforcement failures are blamed on technology, not system design). The perspectival gap emerges: beneficiaries (frontier labs) see d ≈ 0.05, victims (creators) see d ≈ 0.95. This wide gap (90-point difference) is a signature of asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY ANALYSIS: The constraint resolves the mandatrophy by showing that the 'is this coordination or extraction?' question depends entirely on structural position. From the frontier lab's position, it genuinely is coordination — the constraint solves a real problem (how to obtain diverse training data efficiently) and all parties benefit from improved models, even if the benefit is asymmetric. From the creator's position, it is pure extraction — they bear all cost and receive no benefit. The resolution is NOT to force the constraint into a single type but to recognize that Tangled Rope is the correct classification from the civilizational analytical perspective because BOTH coordination function AND asymmetric extraction are structurally real. The coordination (perspective 3) is genuine. The extraction (perspective 1) is genuine. Both are simultaneously true. The Tangled Rope classification prevents the false choice between 'it's just coordination' (which would hide the asymmetry) and 'it's just extraction' (which would ignore the real coordination benefit). The mandatrophy resolves by acknowledging the constraint coordinates access to training data (real) while asymmetrically extracting value from creators (real). This is the definition of Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_copyright_boundary,
    'Does training LLMs on copyrighted text constitute fair use under copyright law, or does it constitute copyright infringement requiring compensation?',
    'Judicial precedent (US courts, EU CJEU) establishing bright-line test for training data incorporation. Test must differentiate: memorization (infringement) vs. statistical pattern learning (potentially fair use).',
    'If fair use: extraction mechanism is weak (legal framework permits scraping). If infringement: extraction mechanism is strong (legal liability gives content creators leverage). Classification shifts from Tangled Rope toward pure extraction Snare at creator perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_copyright_boundary, conceptual, 'Legal boundary between fair use and training data copyright infringement').

omega_variable(
    synthetic_data_quality_parity,
    'Can synthetic data (generated by existing models, simulated via physical/logical rules, or crowdsourced from labeled volunteers) achieve performance parity with scraped internet data at scale?',
    'Comparative benchmarking: models trained on synthetic vs. scraped data; cost-per-performance metrics; downstream task generalization across domains. Generational comparison (2025 models vs. 2027 models) to measure convergence.',
    'If parity achievable: scaffold sunset is real, free-scraping loses competitive advantage. If synthetic data persistently inferior: suppression of alternative pathways is structural, and constraint persists indefinitely as Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_data_quality_parity, empirical, 'Whether synthetic data can achieve performance parity with scraped data').

omega_variable(
    creator_organizing_potential,
    'Can content creators organize collectively (writers'' unions, artist collectives, digital rights organizations) to enforce licensing and compensation mechanisms faster than solo litigation?',
    'Rate of organizational adoption among creators; effectiveness of collective licensing frameworks (Creative Commons licensing adoption rate, rights collectives membership); revenue captured vs. litigation track record.',
    'If organizing succeeds: creator power shifts from powerless/trapped to moderate/constrained, classification changes from Snare toward Tangled Rope. If organizing fails: powerless agents remain trapped, extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_organizing_potential, empirical, 'Potential for creator collective action and licensing').

omega_variable(
    regulatory_enforcement_capacity,
    'Can global regulation (GDPR, AI Act, digital copyright frameworks) actually enforce data consent requirements against distributed and international AI development?',
    'Audit of non-compliance rates by jurisdiction; enforcement action frequency; costs to AI labs of compliance vs. penalties for violation; effectiveness of data provenance auditing at scale.',
    'If enforcement effective: suppression decreases, constraint moves toward Scaffold with real sunset. If enforcement weak: regulation is performative (piton-like), and constraint persists as Tangled Rope with theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_enforcement_capacity, empirical, 'Capacity of global regulation to enforce data consent').

omega_variable(
    emergent_model_value_capture,
    'What portion of economic value generated by trained models flows back to original content creators vs. remaining with model developers and deployers?',
    'Revenue accounting: aggregate value of model deployments (enterprise licensing, consumer products, API fees) vs. payments to creators for training data. Comparison of creator compensation mechanisms (none, flat-rate licensing, usage-based royalties).',
    'If minimal flow-back: asymmetry is severe, classification remains Snare/Tangled Rope. If significant flow-back emerges: extraction ratio decreases, constraint softens toward Rope or Scaffold. Resolves whether this is disguised value extraction or genuine coordination-with-friction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emergent_model_value_capture, empirical, 'Proportion of model value flowing back to content creators').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(large_language_model_training_data_acquisition, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(llm_train_data_tr_t0, large_language_model_training_data_acquisition, theater_ratio, 0, 0.38).
narrative_ontology:measurement(llm_train_data_tr_t2, large_language_model_training_data_acquisition, theater_ratio, 2, 0.42).
narrative_ontology:measurement(llm_train_data_tr_t4, large_language_model_training_data_acquisition, theater_ratio, 4, 0.45).

% Extraction over time
narrative_ontology:measurement(llm_train_data_be_t0, large_language_model_training_data_acquisition, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(llm_train_data_be_t2, large_language_model_training_data_acquisition, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(llm_train_data_be_t4, large_language_model_training_data_acquisition, base_extractiveness, 4, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(large_language_model_training_data_acquisition, resource_allocation).
narrative_ontology:affects_constraint(large_language_model_training_data_acquisition, copyright_fair_use_boundary).
narrative_ontology:affects_constraint(large_language_model_training_data_acquisition, creator_labor_compensation_asymmetry).
narrative_ontology:affects_constraint(large_language_model_training_data_acquisition, synthetic_data_alternative_viability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(large_language_model_training_data_acquisition, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
