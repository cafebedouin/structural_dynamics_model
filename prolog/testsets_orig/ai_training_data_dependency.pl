% ============================================================================
% CONSTRAINT STORY: ai_training_data_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_training_data_dependency, []).

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
 *   constraint_id: ai_training_data_dependency
 *   human_readable: AI Training Data Dependency
 *   domain: technological/artificial_intelligence
 *
 * SUMMARY:
 *   AI systems, particularly those targeting specialized domains like
 *   semiconductor design, have become structurally dependent on vast training
 *   datasets drawn from public sources, academic repositories, and
 *   proprietary engineering knowledge. This dependency creates a hybrid
 *   constraint: frontier AI laboratories (institutional beneficiaries) gain
 *   access to unlimited training data through web scraping and public dataset
 *   aggregation, solving a genuine coordination problem of data aggregation.
 *   Simultaneously, the constraint extracts value from data creators
 *   (researchers, engineers, domain experts) who contribute knowledge through
 *   publications, open-source repositories, and collaborative documentation
 *   without compensation, consent mechanisms, or attribution. The
 *   constraint's extractiveness (0.52) reflects that while the dependency
 *   solves real technical problems, it distributes benefits asymmetrically
 *   toward frontier labs and away from knowledge creators. Suppression (0.65)
 *   is high: data creators face barriers to restricting their contributions
 *   (publication incentives, career norms, open science expectations) while
 *   frontier labs face minimal constraints on data access. The theater ratio
 *   (0.48, increasing to 0.72 by year 10) indicates that institutional data
 *   governance frameworks (IRBs, ethics committees, data governance policies)
 *   are increasingly performative — they declare oversight while contributing
 *   to training corpora without meaningful control mechanisms. The constraint
 *   exhibits all six DR types from different perspectives: pure extraction
 *   (snare) for individual data creators; mixed extraction and coordination
 *   benefit (tangled rope) for domain expert communities and semiconductor
 *   firms; pure coordination (rope) for frontier labs; degraded institutional
 *   review (piton) for data ethics frameworks; temporary scaffolding
 *   (scaffold) for emerging data cooperatives and consent-preserving
 *   techniques; and potential false naturalization (mountain) when scaling
 *   law dependence is confused with mathematical inevitability.
 *
 * KEY AGENTS:
 *   - Frontier AI Laboratories: Primary beneficiary (institutional/arbitrage) — access unlimited training data through web scraping and public datasets; control terms of access; arbitrage between data sources
 *   - Data Source Creators: Primary victim (powerless/trapped) — researchers and engineers whose work is incorporated without consent; no exit mechanism; bear cost of knowledge extraction
 *   - Domain Expert Communities: Secondary victim (moderate/constrained) — benefit from AI tools trained on their knowledge while experiencing value extraction; constrained exit due to collaboration incentives
 *   - Semiconductor Design Industry: Secondary beneficiary/victim (powerful/mobile) — gain AI-augmented design tools but risk proprietary method disclosure; can invest in proprietary training (mobile exit)
 *   - Institutional Data Governance Bodies: Performative actors (institutional/constrained) — declare oversight while lacking enforcement capacity; theater ratio increases as data volumes exceed review capacity
 *   - Data Cooperatives and Open Governance Models: Organized agents building alternatives (organized/mobile) — developing consent-preserving training techniques and collective data trusts with sunset potential
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional data practices as mathematical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_training_data_dependency, 0.52).
domain_priors:suppression_score(ai_training_data_dependency, 0.65).
domain_priors:theater_ratio(ai_training_data_dependency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_training_data_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_training_data_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_training_data_dependency, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_training_data_dependency, tangled_rope).
narrative_ontology:human_readable(ai_training_data_dependency, "AI Training Data Dependency").
narrative_ontology:topic_domain(ai_training_data_dependency, "technological/artificial_intelligence").

domain_priors:requires_active_enforcement(ai_training_data_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_training_data_dependency, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_training_data_dependency, semiconductor_design_platforms).
narrative_ontology:constraint_victim(ai_training_data_dependency, data_source_creators).
narrative_ontology:constraint_victim(ai_training_data_dependency, domain_expertise_communities).
narrative_ontology:constraint_victim(ai_training_data_dependency, open_knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SOURCE CREATORS (SNARE) — Individual researchers, engineers, and domain experts who produced training data (papers, code repositories, design specifications, technical documentation) have no meaningful exit. Their work is scraped, indexed, and incorporated into AI training corpora without consent, compensation, or attribution mechanism. They bear the cost of value extraction while holding no leverage to negotiate terms. Complete suppression: cease producing data and forfeit career incentives; object and face professional isolation.
constraint_indexing:constraint_classification(ai_training_data_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMAIN EXPERT COMMUNITY (TANGLED ROPE) — Specialized communities (semiconductor engineers, materials scientists, medical researchers) benefit from AI tools trained on their collective knowledge while simultaneously experiencing extraction. They gain access to AI-augmented design and discovery tools, but those tools are priced as proprietary services controlled by frontier labs. Their own knowledge returns to them in value-extracted form. Constrained exit: communities can restrict data sharing, but this sacrifices collaboration benefits and risks being sidelined as AI systems trained on others' data surpass them.
constraint_indexing:constraint_classification(ai_training_data_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRONTIER AI LABORATORIES (ROPE) — Net beneficiary with full arbitrage capacity. Labs access unlimited training data through web scraping, public datasets, and partnerships. They experience the constraint as pure coordination: aggregating diverse data sources solves the genuine problem of producing capable models. No extraction is experienced because they control the extraction flow. Can exit by switching to proprietary data pipelines; can arbitrage between data sources; can dictate terms to data providers and downstream users.
constraint_indexing:constraint_classification(ai_training_data_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SEMICONDUCTOR DESIGN INDUSTRY (TANGLED_ROPE) — Major semiconductor firms benefit from AI-augmented design tools (faster layout optimization, yield prediction, fault detection) trained on decades of industry design data. However, their proprietary design methodologies are also extracted and embedded in general-purpose AI tools sold to competitors. Mobile exit: firms can invest in proprietary AI training (higher cost) or accept competitive exposure. Significant but not total extraction — they hold institutional power to negotiate data licensing terms, though most are not exercising it.
constraint_indexing:constraint_classification(ai_training_data_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL REVIEW BOARDS / ETHICS FRAMEWORKS (PITON) — University IRBs, data ethics committees, and institutional data governance processes are substantially performative: they were designed for small-scale human subjects research, not for comprehensive data provenance and consent tracking at scale. Institutions declare data governance while contributing to training corpora without meaningful oversight. Theater persists through institutional inertia and inability to scale review processes to AI-era data volumes. IRBs lack enforcement mechanisms and exit capacity.
constraint_indexing:constraint_classification(ai_training_data_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the dependency of neural scaling laws on data volume appears immutable: current AI architectures require vast training datasets to achieve capability — this is a mathematical fact of the loss landscape. No amount of regulation can change that larger datasets produce better models. However, this naturalizes a contingent institutional choice: the specific data sources chosen, the absence of consent mechanisms, and the distribution of benefits are not mathematical laws but policy decisions. The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(ai_training_data_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: DATA COOPERATIVES / OPEN GOVERNANCE (SCAFFOLD) — Emerging models (data cooperatives, collective data trusts, federated learning frameworks, differential privacy standards) represent temporary support structures with sunset potential. Organizations like data stewardship consortiums and community-governed datasets offer alternatives: data creators collectively control training corpus composition and benefit-sharing. Mobile exit: participants can shift to cooperative models. Sunset clause: as technical standards for consent-preserving AI training mature (federated learning, differential privacy, cryptographic commitment protocols), the current dependency on unrestricted data scraping becomes economically suboptimal rather than technically necessary.
constraint_indexing:constraint_classification(ai_training_data_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_training_data_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_training_data_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_training_data_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_training_data_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_training_data_dependency, TR),
    TR >= 0.70.

:- end_tests(ai_training_data_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant value from data creators (no compensation, no consent, no attribution) while providing genuine coordination benefits (enabling capable AI models). The extractiveness is not as severe as pure intellectual property theft because the value flows through legitimate technical systems. However, it is not low because the asymmetry is stark: frontier labs gain massive value; creators gain nothing directly. The metric has increased from 0.28 to 0.52 over the decade as AI capabilities have improved and reliance on training data has become more explicit. Suppression (0.65): High. Data creators face strong suppression: publishing expectations and career incentives favor open sharing; objecting risks professional isolation; restricting data sacrifices collaboration benefits. Frontier labs face minimal suppression — they control the data pipeline and can continue scraping regardless of creator objections. Theater ratio (0.48, increasing): Moderate and rising. Institutional data governance is partially performative — ethics committees declare oversight while lacking mechanisms to enforce consent or track data provenance at scale. However, some genuine technical work occurs (privacy impact assessments, data audit trails). The ratio increases as data volumes outpace institutional review capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival disagreement on both classification and experienced extraction. Data source creators (powerless/trapped) see a snare: their work is extracted without consent or exit option. Domain expert communities (moderate/constrained) see tangled rope: they gain AI tools but lose proprietary advantage. Semiconductor firms (powerful/mobile) see mixed costs: AI augments design but also incorporates competitors' data. Frontier labs (institutional/arbitrage) see rope: pure coordination of data sources. Data governance bodies see piton: their review processes are performative and degraded. Data cooperatives see scaffold: a temporary coordination failure being solved by emerging alternatives. The analytical observer risks seeing mountain: scaling laws appear to require massive data. The perspectival gaps indicate high structural complexity: no single classification captures how all agents experience the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier AI labs derive low directionality (d ≈ 0.15, beneficiary + arbitrage exit) — they control the extraction flow and experience the constraint as beneficial coordination. Data creators derive high directionality (d ≈ 0.95, victim + trapped exit) — they have no meaningful exit and bear full extraction cost. Domain expert communities derive moderate directionality (d ≈ 0.60, victim + constrained exit) — they can restrict data but at cost to collaboration. Semiconductor firms derive moderate directionality (d ≈ 0.55, mixed beneficiary/victim + mobile exit) — they benefit from AI tools but risk data exposure; they can switch to proprietary training. Data governance bodies derive constrained directionality (d ≈ 0.65, victim + constrained exit) — they declared oversight authority but lack enforcement capacity. These differences produce the perspectival gap: the same constraint structure produces wildly different experienced extraction depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by disambiguating coordination from extraction. The frontier labs genuinely solve a coordination problem — aggregating diverse data sources into trainable corpora enables new capabilities that benefit multiple stakeholders. However, the solution is asymmetrically distributed: labs gain access and control, while creators gain nothing. This is not pure coordination (rope) because the asymmetry is coercive — data creators cannot exit without sacrificing. It is not pure extraction (snare) because some genuine coordination benefit exists. The tangled rope classification correctly identifies both the coordination function and the asymmetric extraction. The mandatrophy is resolved by recognizing that hybrid constraints are legitimate: a constraint can simultaneously solve coordination problems and extract value. The challenge is whether the coordination benefit justifies the extraction cost — which depends on perspective. From data creators' viewpoint, no. From frontier labs' viewpoint, yes. From domain experts' viewpoint, mixed. The tangled rope classification holds across all perspectives with different experienced intensities (via χ values), which is the correct resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_quality_floor,
    'What is the minimum quantity and diversity of training data required to achieve human-competitive performance in specialized domains like semiconductor design? Is the current utilization actually using this minimum, or are frontier labs training on vastly larger corpora than necessary?',
    'Empirical scaling law studies isolating data volume vs model capability in specific domains; analysis of actual data utilization curves vs theoretical minimum; comparison of models trained on curated datasets vs indiscriminate scraping',
    'If minimum is 10% of current: dependency could be dramatically reduced through curation, resolving the snare into a rope. If minimum is 90%+ of current: the massive corpus is structurally necessary, and the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_quality_floor, empirical, 'Minimum data volume required for specialized domain performance').

omega_variable(
    consent_technical_feasibility,
    'Can consent-preserving training techniques (federated learning, differential privacy, synthetic data augmentation, cryptographic data provenance) scale to frontier-scale model training without unacceptable performance degradation?',
    'Technical benchmarks: models trained with consent constraints vs models trained on scraped data; cost analysis of federated approaches; privacy-utility tradeoff curves for differential privacy at large scale',
    'If feasible with <5% performance cost: data cooperatives become structurally viable, scaffold sunset becomes achievable. If cost >20%: consent mechanisms remain aspirational, and snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_technical_feasibility, empirical, 'Technical feasibility of consent-preserving training at scale').

omega_variable(
    semiconductor_competitive_parity,
    'Will firms that restrict proprietary design data from training corpora experience competitive disadvantage relative to firms whose data is incorporated into public AI models? What is the time scale for this divergence?',
    'Competitive analysis: design cycle time, yield improvement rates, innovation pace for restricted-data vs open-data firms; longitudinal tracking of market share and design capability metrics',
    'If parity maintained: firms retain genuine exit option (restrict data), tangled rope classification holds. If restriction leads to 2+ year delays: exit is illusory, classification degrades to snare for reluctant data providers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(semiconductor_competitive_parity, empirical, 'Competitive impact of restricting proprietary data from AI training').

omega_variable(
    alternative_architecture_feasibility,
    'Could alternative AI architectures (retrieval-augmented generation, modular reasoning systems, structured knowledge representations) achieve comparable performance without requiring massive indiscriminate training corpora?',
    'Research development and benchmarking of alternative approaches; performance comparisons on domain-specific tasks; scaling laws for non-transformer architectures',
    'If viable alternative exists: the mountain perspective dissolves, and dependency is revealed as institutional choice. If scaling laws strongly favor current approaches: mathematical necessity is more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_architecture_feasibility, conceptual, 'Alternative AI architectures reducing training data dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_training_data_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aitdd_tr_t0, ai_training_data_dependency, theater_ratio, 0, 0.32).
narrative_ontology:measurement(aitdd_tr_t5, ai_training_data_dependency, theater_ratio, 5, 0.4).
narrative_ontology:measurement(aitdd_tr_t10, ai_training_data_dependency, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(aitdd_be_t0, ai_training_data_dependency, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(aitdd_be_t5, ai_training_data_dependency, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(aitdd_be_t10, ai_training_data_dependency, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_training_data_dependency, resource_allocation).
narrative_ontology:affects_constraint(ai_training_data_dependency, intellectual_property_attribution).
narrative_ontology:affects_constraint(ai_training_data_dependency, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(ai_training_data_dependency, algorithmic_monoculture_risk).

% DUAL FORMULATION NOTE:
% AI training data dependency decomposes into at least two structurally distinct constraints: (1) Technical dependency on data volume for neural scaling (ε ≈ 0.15, approaching mountain as architecture approaches theoretical limits), and (2) Institutional dependency on uncompensated data extraction due to absence of consent mechanisms (ε ≈ 0.52, tangled rope). These are linked: technical scaling laws create incentive for frontier labs to maximize data collection, which intersects with institutional failures of attribution and consent. The story treats the hybrid institutional-technical constraint. Upstream decomposition into pure technical and pure institutional stories may reveal different resolution paths.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_training_data_dependency, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
