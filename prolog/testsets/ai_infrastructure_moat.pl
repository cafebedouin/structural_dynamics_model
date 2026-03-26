% ============================================================================
% CONSTRAINT STORY: ai_infrastructure_moat
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_infrastructure_moat, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_infrastructure_moat
 *   human_readable: AI Infrastructure Moat: Capital Concentration and Competitive Lock-In
 *   domain: technology/economics/governance
 *
 * SUMMARY:
 *   The AI infrastructure moat represents a structural barrier to competitive
 *   entry in advanced AI capability development. Large-scale GPU clusters,
 *   specialized semiconductors, proprietary training pipelines, and
 *   accumulated model weights create cumulative advantages that prevent new
 *   market entrants, smaller companies, academic researchers, and developing
 *   nations from achieving competitive capability parity. The constraint
 *   operates through both material barriers (capital requirements exceeding
 *   $1B, semiconductor supply controlled by few vendors) and structural
 *   suppression (network effects, data exclusivity, proprietary formats). The
 *   moat has intensified over the 2020-2026 interval as training compute
 *   requirements have doubled roughly every 18-24 months (Chinchilla scaling
 *   laws), pushing capital barriers higher and increasing consolidation.
 *   Unlike previous computing infrastructure monopolies (mainframes, cloud
 *   computing), the AI infrastructure moat exhibits qualitatively higher
 *   switching costs because the trained model weights, fine-tuned systems,
 *   and proprietary datasets are not interoperable. An organization switching
 *   between OpenAI, Google, and Anthropic loses all accumulated investment in
 *   model-specific optimization. This non-interoperability is partly economic
 *   (different optimization targets) and partly enforced through licensing
 *   restrictions. The constraint exhibits characteristics of pure extraction
 *   (Snare) from excluded agents' perspectives but includes genuine
 *   coordination functions (standardized APIs, reliable service, ecosystem
 *   stability) from the beneficiary's perspective.
 *
 * KEY AGENTS:
 *   - Large AI Developers (OpenAI, Google, Anthropic, Meta): Primary beneficiaries (institutional/arbitrage) — capture licensing revenue, investment capital concentration, exclusive data access, and first-mover advantage in capability development
 *   - Semiconductor Manufacturers (NVIDIA, TSMC, Intel): Secondary beneficiaries (institutional/arbitrage) — control GPU/TPU supply chains, maintain vendor lock-in, extract rents through scarcity pricing
 *   - Cloud Infrastructure Providers (AWS, Azure, GCP): Secondary beneficiaries (institutional/arbitrage) — operate training infrastructure monopolies, charge premium rates for GPU access
 *   - Smaller AI Companies (<$100M funding): Primary victims (powerless/trapped) — cannot access capital for independent training, cannot negotiate favorable terms for API access, cannot build proprietary models, systematically excluded from competitive capability development
 *   - Academic Researchers: Secondary victims (moderate/constrained) — restricted access through API rate limits and pricing, cannot afford training-from-scratch, dependent on proprietary model availability, face publication restrictions
 *   - Open-Source AI Communities: Tertiary victims (organized/constrained) — cannot access training data or compute at competitive scale, systematically disadvantaged against proprietary models, model weights and fine-tuning restricted by licensing
 *   - Developing Nation AI Initiatives: Structural victims (organized/constrained) — face geopolitical barriers (export controls on chips, sanctions), economic barriers (cannot afford training costs), capacity barriers (lack engineering talent due to brain drain to incumbents)
 *   - Global AI Capability Competition: Abstract victim (powerless/trapped) — concentrated capability reduces competitive dynamism, innovation concentrated in 3-5 firms, alternative approaches and diverse paradigms starved of resources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_infrastructure_moat, 0.68).
domain_priors:suppression_score(ai_infrastructure_moat, 0.72).
domain_priors:theater_ratio(ai_infrastructure_moat, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_infrastructure_moat, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_infrastructure_moat, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_infrastructure_moat, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_infrastructure_moat, snare).
narrative_ontology:human_readable(ai_infrastructure_moat, "AI Infrastructure Moat: Capital Concentration and Competitive Lock-In").
narrative_ontology:topic_domain(ai_infrastructure_moat, "technology/economics/governance").

domain_priors:requires_active_enforcement(ai_infrastructure_moat).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_infrastructure_moat, large_ai_developers).
narrative_ontology:constraint_beneficiary(ai_infrastructure_moat, semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(ai_infrastructure_moat, cloud_infrastructure_providers).
narrative_ontology:constraint_victim(ai_infrastructure_moat, smaller_ai_companies).
narrative_ontology:constraint_victim(ai_infrastructure_moat, academic_researchers).
narrative_ontology:constraint_victim(ai_infrastructure_moat, developing_nation_ai_development).
narrative_ontology:constraint_victim(ai_infrastructure_moat, open_source_ecosystem).
narrative_ontology:constraint_victim(ai_infrastructure_moat, market_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED COMPETITOR (SNARE) — Startups and smaller AI companies face insurmountable capital barriers (>$1B for competitive training infrastructure). No exit option exists; cannot enter the market without access to the very infrastructure controlled by incumbents. Full extraction with no alternatives.
constraint_indexing:constraint_classification(ai_infrastructure_moat, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC RESEARCHER (SNARE) — Must negotiate API access, face rate limits, pricing barriers, and proprietary model constraints. Cannot build independent computational capacity. High exit costs (years of retraining, loss of established work) create effective lock-in despite theoretical mobility.
constraint_indexing:constraint_classification(ai_infrastructure_moat, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LARGE AI DEVELOPER (SNARE) — Appears to have arbitrage (could theoretically build own infrastructure) but faces structural lock-in: proprietary compute architectures, supply chain control, and network effects create insurmountable switching costs. This perspective reveals the snare operates even at the powerful level through infrastructure coupling.
constraint_indexing:constraint_classification(ai_infrastructure_moat, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE COALITION (SNARE) — Organized but systematically disadvantaged. Model weights locked behind proprietary licensing, compute hardware controlled by commercial vendors, training data access restricted. Organization provides no exit path — collective action cannot overcome structural resource barriers.
constraint_indexing:constraint_classification(ai_infrastructure_moat, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPING NATION AI INITIATIVES (SNARE) — Geopolitical barriers compound infrastructure barriers. Cannot access cutting-edge chips (export controls), cannot afford training costs (economic dependency), cannot build independent ecosystems (network effects favor incumbents). Structured exclusion from competitive participation.
constraint_indexing:constraint_classification(ai_infrastructure_moat, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INCUMBENT AI PLATFORM (ROPE) — Experiences infrastructure as enabling coordination: APIs enable ecosystem development, standardized interfaces reduce fragmentation, integrated stacks solve real multi-party coordination problems. Extraction flows toward the incumbent through network lock-in and switching costs, but genuine coordination functions exist.
constraint_indexing:constraint_classification(ai_infrastructure_moat, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY BODIES (PITON) — Institutions designed to prevent monopolistic gatekeeping (antitrust authorities, competition regulators) are largely performative regarding AI infrastructure. Hearings and statements lack enforcement capacity or meaningful remedies. Theater ratio high because the regulatory mechanisms persist despite ineffectiveness.
constraint_indexing:constraint_classification(ai_infrastructure_moat, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED_ROPE) — From civilizational scope, infrastructure moat exhibits genuine coordination (standardization, reliability, ecosystem development) alongside asymmetric extraction (concentrated rents, barrier to entry, ecosystem control). The constraint is not pure extraction but hybrid: coordination without which modern AI systems cannot operate, extraction through market power enforcement. Classification accounts for chi formula scaling by scope and power.
constraint_indexing:constraint_classification(ai_infrastructure_moat, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_infrastructure_moat_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_infrastructure_moat, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_infrastructure_moat, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_infrastructure_moat, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_infrastructure_moat, TR),
    TR >= 0.70.

:- end_tests(ai_infrastructure_moat_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint extracts in multiple forms: (1) Licensing rents — proprietary model access costs, API pricing with no commodity alternatives. (2) Capital concentration — funding flows to incumbents at 10:1 ratios vs competitors. (3) Talent hoarding — top researchers concentrated at well-capitalized labs, reducing outside capability. (4) Data monopolization — training data access restricted or proprietary. The trajectory shows acceleration: extractiveness was 0.35 in 2020 (when multiple firms could field competitive models with $100M budgets), reached 0.52 by 2023 (Chinchilla scaling doubled capital requirements), and stands at 0.68 by 2026 (frontier models require $500M-$1B+ training budgets). Suppression (0.72): Very high. Multiple independent barriers compound: (a) Capital barrier is absolute — under $1B, cannot fund competitive training from scratch. (b) Semiconductor supply is controlled — cannot order sufficient GPUs without government approval or years-long waiting lists. (c) Talent is unavailable — top researchers command $500K-$2M+ compensation only afforded by well-capitalized firms. (d) Data is proprietary — web-scale training data hoarded or licensed at monopoly prices. (e) Regulatory barriers are high — export controls on chips, AI export restrictions. No single barrier is surmountable by individual excluded agents. Theater ratio (0.38): Moderate. Less performative than some constraints because the underlying extraction mechanism (infrastructure scarcity) is genuine and functional. The constraint does deliver real coordination (APIs work, models are stable, ecosystems develop) and real capability (frontier models at the moat are genuinely more capable). Theater arises primarily in regulatory/policy layers: antitrust authorities produce reports and statements with minimal enforcement; promises of 'open AI' persist despite increasing proprietary lock-in; public-private partnership rhetoric masks unidirectional extraction toward incumbents.
 *
 * PERSPECTIVAL GAP:
 *   The excluded competitor sees a pure snare — they are systematically locked out with no remedies. The large incumbent sees coordination and ecosystem development (rope perspective) — they built the infrastructure and see it enabling genuine multi-party innovation. The analytical observer at civilizational scope recognizes the constraint as hybrid: genuine coordination functions (standardized APIs, model stability, ecosystem reliability) coupled with asymmetric extraction (concentrated rents, barriers to entry, lock-in effects). The perspectival gap reveals the structural tension: the coordination functions are real and valuable, but they are being delivered in a way that concentrates benefits and blocks alternatives. A public compute utility delivering the same coordination with open access would exhibit lower extractiveness (0.25-0.35) and higher theater (more regulatory theatrics, but actual coordination without lock-in).
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality varies sharply by agent power and exit options. Smaller competitors occupy d≈0.98 (trapped, powerless, full victim status) — they face absolute barriers and no exit path. Academic researchers occupy d≈0.75 (constrained, moderate, high victim status) — they could theoretically exit academic AI research entirely but face high costs (retraining, career disruption). Incumbents appear to occupy low d values (arbitrage exit, institutional power, beneficiary status, d≈0.10-0.25) — they control the infrastructure and can extract rents. However, even incumbents show some upward pressure on d through competitive coupling: if competitor infrastructure improves (open-source, foreign alternatives, public compute utilities), incumbents face displacement risk. The analytical observer's perspective invokes scope effects: at local scope (single lab), the moat is a coordination mechanism (rope). At global scope, the moat is concentrated extraction (snare). The scope σ(S)=1.2 modifier amplifies extractiveness for constraints measured at global scale, which is appropriate here — the infrastructure moat's harm is precisely that it prevents global competitive alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the AI infrastructure moat is: 'Is this pure extraction or coordination-with-extraction?' The snare classification is accurate from excluded agents' perspectives (no exit, high extraction). The rope classification from incumbents' perspectives is also accurate (genuine coordination functions exist). The analytical observer's tangled-rope classification synthesizes: the constraint DOES coordinate (provides reliable infrastructure, enables ecosystem) AND DOES extract (concentrates rents, prevents entry, maintains lock-in). The constraint is not mislabeled — it genuinely has both functions. The mandatrophy is resolved by recognizing that 'coordination and extraction are not mutually exclusive.' An infrastructure can efficiently coordinate AND inefficiently extract. The question is not 'which is it?' but 'what proportion of the extracted value is justified as coordination cost vs. rent-seeking overhead?' The analytical framework suggests the answer: compare extractiveness under alternative governance structures. If a public compute utility can deliver the same coordination (reliability, standardization, ecosystem stability) at extractiveness 0.25, then the difference (0.68 - 0.25 = 0.43) is unjustified rent extraction, not necessary coordination cost. The current moat's 0.68 extractiveness includes genuine coordination overhead (~0.20-0.25) plus unjustified rent extraction (~0.40-0.45).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_capital_concentration,
    'Is the infrastructure moat an inevitable consequence of physical/economic laws (declining marginal cost of scale, network effects, R&D capital intensity) or a contingent institutional choice (IP protection regime, procurement policies, export controls)?',
    'Comparative analysis of alternative governance structures: public compute utilities, open-source foundation-governed infrastructure, international AI research commons. Empirical evidence that alternative models can sustain competitive capability.',
    'If inevitable (natural): classification remains Snare — no policy intervention can change structural extraction. If contingent (institutional): classification changes to Tangled Rope under policy intervention scenarios — extraction could be decoupled from coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_capital_concentration, empirical, 'Whether infrastructure concentration is natural law or institutional choice').

omega_variable(
    capability_floor_requirement,
    'What minimum infrastructure scale is genuinely necessary for competitive AI capability development? Is the $1B+ barrier economically justified or artificially enforced?',
    'Analysis of successful smaller-scale deployments: TinyML, federated learning, efficient model architectures. Historical precedent from other computing paradigms (cloud infrastructure consolidation, GPU markets). Cost analysis of minimum viable training runs.',
    'If floor is $500M+: barrier reflects genuine economic constraint, extraction is unavoidable, policy can only manage redistribution. If floor is $100M or lower: current barriers are rent-seeking overhead, moat is artificial, alternatives exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_floor_requirement, empirical, 'Minimum capital requirement for competitive AI capability').

omega_variable(
    geopolitical_fragmentation_risk,
    'Does the AI infrastructure moat increase or decrease risk of geopolitical fragmentation into incompatible AI ecosystems (US/China/EU)?',
    'Monitoring of alternative large-language model architectures, training data silos, compute hardware divergence. Analysis of whether monopolistic incumbents or fragmented competition produces compatibility layers.',
    'If moat reduces fragmentation risk (incumbents maintain interoperability): extraction is the cost of global coordination. If moat increases risk (lock-in forces regional isolation): extraction compounds geopolitical harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_fragmentation_risk, empirical, 'Impact of infrastructure moat on geopolitical fragmentation').

omega_variable(
    open_source_viability_threshold,
    'Can open-source AI development maintain competitive parity with closed proprietary systems if infrastructure barriers are removed? Or does the moat reflect fundamental advantages (data access, compute efficiency optimization, real-world feedback loops)?',
    'Longitudinal tracking of open-source model capabilities vs proprietary systems controlling for: (a) compute budget differences, (b) training data quality/quantity, (c) engineering optimization investment. Hypothesis test whether capability gaps are hardware-driven or algorithmic.',
    'If open-source can compete with equal resources: moat is rent-extraction, not quality advantage. If open-source systematically lags: moat reflects real technical superiority, extraction is payment for capability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_viability_threshold, empirical, 'Whether open-source AI can achieve parity with equal resources').

omega_variable(
    suppression_mechanism_durability,
    'Is the high suppression (0.72) primarily structural (genuinely difficult to overcome barriers) or internalized (market participants believe barriers are insurmountable when alternatives exist)?',
    'Post-intervention analysis: if policy reduces capital requirements (public compute funding, export control relaxation) or technology breakthrough (more efficient training), do excluded agents actually enter market? Or does suppression persist due to internalized belief that moat is unbreakable?',
    'If structural: suppression remains even after barriers are theoretically removed — policy interventions must include psychological reframing. If internalized: suppression drops rapidly once barriers are actually removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_durability, empirical, 'Whether suppression is structural or internalized belief').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_infrastructure_moat, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_moat_tr_t0, ai_infrastructure_moat, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_moat_tr_t3, ai_infrastructure_moat, theater_ratio, 3, 0.31).
narrative_ontology:measurement(ai_moat_tr_t6, ai_infrastructure_moat, theater_ratio, 6, 0.38).
narrative_ontology:measurement(ai_moat_tr_t9, ai_infrastructure_moat, theater_ratio, 9, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_moat_be_t0, ai_infrastructure_moat, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_moat_be_t3, ai_infrastructure_moat, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(ai_moat_be_t6, ai_infrastructure_moat, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(ai_moat_be_t9, ai_infrastructure_moat, base_extractiveness, 9, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_infrastructure_moat, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ai_infrastructure_moat, 0.22).
narrative_ontology:affects_constraint(ai_infrastructure_moat, ai_research_equity_access).
narrative_ontology:affects_constraint(ai_infrastructure_moat, semiconductor_supply_chain_bottleneck).
narrative_ontology:affects_constraint(ai_infrastructure_moat, model_weights_licensing_regime).
narrative_ontology:affects_constraint(ai_infrastructure_moat, ai_capability_concentration).

% DUAL FORMULATION NOTE:
% The AI infrastructure moat decomposes into four structurally distinct constraints: (1) ai_infrastructure_moat (this story, ε=0.68): the capital and hardware barriers to training-from-scratch. (2) ai_research_equity_access (ε=0.55): the disparity in API access, data access, and compute allocation between institutions. (3) semiconductor_supply_chain_bottleneck (ε=0.42): the NVIDIA/TSMC duopoly in GPU/AI-chip manufacturing. (4) model_weights_licensing_regime (ε=0.60): the proprietary licensing constraints on trained models and fine-tuning. These are linked: removing any single constraint reduces but does not eliminate the overall moat. Each story has different beneficiaries, victims, and resolution mechanisms. The moat as an integrated system is stronger than the sum of its parts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_infrastructure_moat, institutional, 0.18).
constraint_indexing:directionality_override(ai_infrastructure_moat, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
