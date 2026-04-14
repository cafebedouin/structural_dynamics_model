% ============================================================================
% CONSTRAINT STORY: ai_model_training_data_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_model_training_data_asymmetry, []).

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
 *   constraint_id: ai_model_training_data_asymmetry
 *   human_readable: AI Model Training Data Asymmetry
 *   domain: artificial_intelligence/data_governance/labor
 *
 * SUMMARY:
 *   The AI model training data asymmetry represents a structural extraction
 *   mechanism where the creators of training data (writers, artists,
 *   photographers, journalists, code contributors) surrender intellectual
 *   property and commercial rights to AI developers through terms of service,
 *   web scraping, and regulatory ambiguity, while AI companies capture
 *   disproportionate economic value from models trained on this data. The
 *   constraint exhibits both genuine coordination (aggregating diverse
 *   knowledge enables more capable models that benefit users globally) and
 *   genuine extraction (creators bear costs without compensation while
 *   developers and users capture benefits). Extractiveness has increased from
 *   0.35 to 0.62 over the interval as models became more valuable and
 *   training on unlicensed data normalized. Theater ratio (0.48) reflects
 *   mixed governance: some real enforcement mechanisms exist (lawsuits,
 *   licensing negotiations, copyright claims) but are consistently outpaced
 *   by technical capability to scrape, fine-tune, and redistribute models.
 *   The constraint is a tangled rope because it cannot be classified as pure
 *   extraction (genuine coordination function exists) or pure coordination
 *   (genuine asymmetric extraction exists). Active enforcement is required to
 *   balance the asymmetry.
 *
 * KEY AGENTS:
 *   - Data Creators (powerless/trapped): Writers, artists, photographers whose copyrighted work trains models without attribution or compensation. No exit options. Primary victims.
 *   - AI Companies (institutional/arbitrage): OpenAI, Anthropic, Meta, Google capturing economic value from trained models. Primary beneficiaries with institutional power and global arbitrage options.
 *   - Content Industries (moderate/constrained): Publishers, studios, music labels with some power to negotiate licensing and pursue litigation. Secondary victims with organized but constrained exit options.
 *   - Displaced Workers (organized/constrained): Labor advocates, unions representing creators and workers whose skills are devalued or jobs automated. Organized agents with constrained exit through collective action.
 *   - End Users (powerful/mobile): Consumers and downstream applications benefiting from capable models at low cost. Beneficiaries with exit options (choice of services) but currently enjoying consumer surplus.
 *   - Intellectual Property Institutions (institutional/arbitrage): Courts, legislatures, copyright offices nominally governing data licensing. Degraded function (piton perspective) — persist through inertia without effective capacity.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective recognizing both coordination function and extraction mechanism simultaneously.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_model_training_data_asymmetry, 0.58).
domain_priors:suppression_score(ai_model_training_data_asymmetry, 0.65).
domain_priors:theater_ratio(ai_model_training_data_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_model_training_data_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_model_training_data_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_model_training_data_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_model_training_data_asymmetry, tangled_rope).
narrative_ontology:human_readable(ai_model_training_data_asymmetry, "AI Model Training Data Asymmetry").
narrative_ontology:topic_domain(ai_model_training_data_asymmetry, "artificial_intelligence/data_governance/labor").

domain_priors:requires_active_enforcement(ai_model_training_data_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_model_training_data_asymmetry, model_developers).
narrative_ontology:constraint_beneficiary(ai_model_training_data_asymmetry, ai_companies).
narrative_ontology:constraint_beneficiary(ai_model_training_data_asymmetry, downstream_application_users).
narrative_ontology:constraint_victim(ai_model_training_data_asymmetry, data_creators).
narrative_ontology:constraint_victim(ai_model_training_data_asymmetry, training_data_sources).
narrative_ontology:constraint_victim(ai_model_training_data_asymmetry, displaced_workers).
narrative_ontology:constraint_victim(ai_model_training_data_asymmetry, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA CREATORS (SNARE) — Writers, artists, photographers whose work trains models without attribution or compensation. Trapped by platform terms of service, copyright asymmetry, and information scarcity about how data is used. High suppression: cannot opt out of web scraping, cannot negotiate data licensing, cannot learn what their work trained. Pure extraction — minimal coordination benefit, maximum coercion.
constraint_indexing:constraint_classification(ai_model_training_data_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATORS WITH INDUSTRY BACKING (TANGLED ROPE) — Publishers, studios, music labels experience genuine coordination benefits (broader distribution, new tools) alongside extraction (training data licensing fees, market cannibalization risks, devaluation of human-created content). Constrained by market power of AI developers but not fully trapped — can pursue licensing agreements, litigation, industry standards. Moderate extraction with real asymmetry.
constraint_indexing:constraint_classification(ai_model_training_data_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DISPLACED WORKERS & LABOR ADVOCATES (TANGLED ROPE) — Labor organizing groups recognize both coordination benefit (efficiency gains) and extraction (job displacement, skill obsolescence, wage suppression). Organized power (unions, advocacy coalitions) provides constrained exit options: collective bargaining, political mobilization. But coordination asymmetry is real — benefits accrue to capital, costs to labor. Requires active enforcement of worker protections.
constraint_indexing:constraint_classification(ai_model_training_data_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: AI COMPANIES (ROPE) — Primary beneficiaries experiencing the constraint as pure coordination: aggregating data sources, scaling models, capturing value. Institutional power and arbitrage options (can source data globally, can relocate operations, can set licensing terms) mean they experience low effective extraction. The constraint solves their core problem: scaling training data at low cost. Net beneficiary position.
constraint_indexing:constraint_classification(ai_model_training_data_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: END-USERS & DOWNSTREAM APPLICATIONS (ROPE) — Benefit from more capable models trained on broad datasets without direct coordination costs. Mobile exit options (can choose which AI services to use, can build alternatives) but currently enjoying significant consumer surplus from free or low-cost AI tools. Experiences the constraint as coordination mechanism enabling value delivery.
constraint_indexing:constraint_classification(ai_model_training_data_asymmetry, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT & IP INSTITUTIONS (PITON) — The legal regime governing training data licensing has substantially atrophied. Fair use doctrine (original function: enable transformative use) now largely enables uncompensated data extraction. DMCA takedown mechanisms (function: copyright protection) rarely enforce data removal from training pipelines. The IP framework persists through institutional inertia (courts, lawmakers) but has lost functional capacity to regulate training data flows. High theater: enforcement actions and policy discussions create appearance of governance without effective mechanism.
constraint_indexing:constraint_classification(ai_model_training_data_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits genuine coordination (aggregating global knowledge into capable models) that produces widespread value. But coordination is structurally asymmetric: creators bear cost (labor extraction, attribution loss), developers and users capture benefit (capability, economic value). Active enforcement asymmetry (data scrapers move faster than legal remedies) perpetuates extraction. Mandatrophy requires recognizing both genuine coordination function and genuine extraction mechanism.
constraint_indexing:constraint_classification(ai_model_training_data_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_model_training_data_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_model_training_data_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_model_training_data_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_model_training_data_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_model_training_data_asymmetry, TR),
    TR >= 0.70.

:- end_tests(ai_model_training_data_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. AI companies extract significant value from unlicensed training data — economic value of models trained on this data likely exceeds compensation paid to creators by orders of magnitude. However, genuine coordination value exists (models enable capabilities that would not emerge from isolated individual datasets). The 0.58 value reflects that extraction is substantial but not maximal — some legitimate developer value (algorithmic innovation, infrastructure, compute) justifies some asymmetry. The rising trajectory (0.35 → 0.62) shows that as model value increases and licensing remains ambiguous, effective extraction grows. Suppression (0.65): High. Barriers to creator exit and negotiation include: (1) Terms of service that make opt-out unilateral for companies, not creators. (2) Information asymmetry — most creators don't know their data was used. (3) Technical barriers — once training begins, data removal is difficult/impossible. (4) Regulatory gaps — fair use doctrine provides no clear protections for creators. (5) Power asymmetry — individual creators cannot negotiate with companies; collective action is nascent. Theater ratio (0.48): Moderate. Some genuine governance mechanisms exist (copyright lawsuits, licensing negotiations, policy discussions) but are consistently outpaced by technical capability. The appearance of enforcement (high-profile lawsuits, policy proposals) exceeds the functional capacity to regulate. Theater is lower than in pure pitons because enforcement is real, not purely theatrical, but consistent asymmetry in pacing (law moves slower than technology) produces theater effect.
 *
 * PERSPECTIVAL GAP:
 *   The foundational gap is between beneficiaries and victims. AI companies (institutional/arbitrage) see a coordination mechanism (Rope) — they are solving the legitimate problem of scaling training data globally. End users see coordination (Rope) — they benefit from capable models. Data creators (powerless/trapped) see pure extraction (Snare) — they cannot exit, receive no compensation, cannot learn how their data was used. Content industries (moderate/constrained) see mixed extraction with coordination (Tangled Rope) — they benefit from broader distribution but bear real costs from data devaluation. Workers (organized/constrained) see mixed extraction (Tangled Rope) — efficiency gains alongside job displacement risk. The piton perspective on IP institutions reveals that the constraint persists through institutional inertia: copyright mechanisms that were designed to protect creators now function to protect AI developers (fair use doctrine), and enforcement mechanisms are too slow to regulate the pace of AI capability growth. The analytical observer sees all these perspectives simultaneously and recognizes that the constraint is genuinely both coordination and extraction — not a disagreement about facts, but a structural asymmetry that makes the same mechanism appear beneficial from one position and extractive from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: Who benefits? Who bears costs? What exit options do they have? Data creators are victims (high d) with trapped exit options (cannot refuse participation once online) → highest experienced extraction. AI companies are beneficiaries (low d) with arbitrage exit options → low/negative experienced extraction (they experience this as pure coordination). Content industries are mixed victims with constrained exit (can negotiate but at cost) → moderate d. Organized workers have exit options through collective action (constrained, not trapped) → lower d than individual creators. End users are beneficiaries with mobile exit → low d. The analytical observer with analytical exit sees the full structure (medium d ≈ 0.72, produces poweful f(d) modifier). Suppression scaling is NOT applied to the directionality formula — suppression is structural and remains 0.65 regardless of perspective. Effective extractiveness chi = 0.58 × f(d) × σ(global=1.2) produces different chi values for each agent based on their f(d), but suppression provides the baseline coercive cost that all agents navigate.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint exhibits genuine coordination (aggregating diverse training data enables models with capabilities that wouldn't emerge from isolated datasets) and genuine extraction (creators bear costs, developers capture disproportionate benefits, suppression prevents creator exit and negotiation). The classification as Tangled Rope resolves the mandatrophy by recognizing that both functions are structurally necessary. The coordination function is not a cover story for extraction — removing the constraint would reduce model capability. The extraction is not a side effect of coordination — it persists because of active enforcement asymmetry (data scrapers move faster than legal remedies) and beneficiary incentive (companies benefit from low-cost data). Neither function can be eliminated without restructuring the constraint entirely. The Tangled Rope classification stands because: (1) Beneficiaries declared (AI companies, end users) with real coordination benefit. (2) Victims declared (data creators, displaced workers) with real extraction cost. (3) Active enforcement required (data licensing, copyright protection, labor standards) to balance asymmetry. If extractiveness exceeded 0.70, additional mandatrophy resolution (separation into extraction-only and coordination-only mechanisms) would be required. At 0.58, the tangled rope classification is stable — the constraint is genuinely hybrid, not misclassified coordination or disguised extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_scope_ambiguity,
    'Does training on unlicensed data constitute transformative fair use or derivative infringement?',
    'Test case law progression in major jurisdictions; correlation between legal outcomes and computational transformation metrics (token overlap, attribution precision); analysis of whether models memorize vs. transform training data',
    'If fair use applies: constraint is coordination (Rope) with residual extraction. If infringement applies: constraint is extraction (Snare/Tangled Rope) requiring compensation mechanisms. Classification shifts significantly based on legal resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fair_use_scope_ambiguity, conceptual, 'Fair use status of model training on unlicensed data').

omega_variable(
    model_capability_attribution,
    'What fraction of model performance improvement comes from training data scale vs. algorithmic innovation?',
    'Ablation studies comparing models trained on identical algorithms with varying data quantity and diversity; analysis of performance gains from scale vs. architecture changes; measurement of data attribution per test benchmark',
    'If data dominates (>70%): extraction framing strengthens — data creators should capture majority of value. If algorithms dominate (>60%): coordination framing strengthens — developer innovation deserves primary share. If mixed: both perspectives have empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_capability_attribution, empirical, 'Relative contribution of training data vs. algorithmic innovation to model performance').

omega_variable(
    worker_displacement_mechanism,
    'Is job displacement primarily from task automation or from devaluation of human-created training data?',
    'Longitudinal analysis of wage/employment trends in writing, art, music, code; correlation with AI model capabilities in each domain; survey data on employment impact attribution; measurement of output devaluation rates in creator markets',
    'If displacement mechanism is automation: technological substitution (structural). If devaluation mechanism is data reuse: extraction mechanism (policy-addressable). Different mechanisms suggest different remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_displacement_mechanism, empirical, 'Whether worker displacement is from automation or data value extraction').

omega_variable(
    epistemic_commons_degradation,
    'Does AI training on internet data degrade the epistemic commons through feedback loops (AI outputs train next models) or preserve it through amplification?',
    'Longitudinal study of information diversity metrics on the internet; measurement of AI-generated content prevalence in training datasets over time; analysis of feedback loop stability (does epistemic quality degrade monotonically or oscillate); comparison with pre-AI baseline information diversity',
    'If degradation occurs: epistemic commons is genuine victim, suppression is structural (victims cannot exit the information environment). If amplification occurs: constraint provides public good despite extraction. Classification of epistemic commons as victim vs. neutral beneficiary shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_commons_degradation, empirical, 'Whether training data feedback loops degrade or preserve epistemic commons').

omega_variable(
    global_data_sovereignty_asymmetry,
    'Does asymmetric access to training data (Global North AI companies accessing Global South web content) constitute structural extraction or legitimate knowledge sharing?',
    'Analysis of data source geographic distribution vs. model capability distribution; measurement of economic value capture by geography; comparison with historical knowledge extraction patterns (colonialism, IP regimes); interview analysis of researcher agency in data contribution decisions',
    'If structural extraction: constraint is neo-colonial (Snare from Global South perspective, Rope from Global North perspective). If knowledge sharing: constraint is cooperative (Rope globally). Geography-dependent classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_data_sovereignty_asymmetry, conceptual, 'Whether asymmetric training data access constitutes global extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_model_training_data_asymmetry, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aitda_tr_t0, ai_model_training_data_asymmetry, theater_ratio, 0, 0.32).
narrative_ontology:measurement(aitda_tr_t3, ai_model_training_data_asymmetry, theater_ratio, 3, 0.4).
narrative_ontology:measurement(aitda_tr_t6, ai_model_training_data_asymmetry, theater_ratio, 6, 0.48).
narrative_ontology:measurement(aitda_tr_t9, ai_model_training_data_asymmetry, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(aitda_be_t0, ai_model_training_data_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aitda_be_t3, ai_model_training_data_asymmetry, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aitda_be_t6, ai_model_training_data_asymmetry, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(aitda_be_t9, ai_model_training_data_asymmetry, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_model_training_data_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(ai_model_training_data_asymmetry, copyright_fair_use_doctrine).
narrative_ontology:affects_constraint(ai_model_training_data_asymmetry, labor_market_skill_devaluation).
narrative_ontology:affects_constraint(ai_model_training_data_asymmetry, content_creator_licensing_markets).
narrative_ontology:affects_constraint(ai_model_training_data_asymmetry, epistemically_reflective_ai_training).

% DUAL FORMULATION NOTE:
% Training data asymmetry decomposes into distinct constraints: (1) COPYRIGHT INFRINGEMENT vs FAIR USE TRANSFORMATION (ε≈0.42, contested, legal ambiguity). (2) LABOR MARKET DEVALUATION (ε≈0.65, clear extraction, skill obsolescence). (3) CONTENT LICENSING (ε≈0.48, mixed coordination and extraction, bilateral negotiation). (4) EPISTEMIC COMMONS DEGRADATION (ε≈uncertain, feedback loop effects). This story focuses on the asymmetry mechanism that connects these; see linked stories for domain-specific analyses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_model_training_data_asymmetry, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
