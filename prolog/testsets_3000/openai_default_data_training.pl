% ============================================================================
% CONSTRAINT STORY: openai_default_data_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openai_default_data_training, []).

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
 *   constraint_id: openai_default_data_training
 *   human_readable: Default Use of ChatGPT User Data for Model Training
 *   domain: technology/platform_governance
 *
 * SUMMARY:
 *   OpenAI's default-to-training policy for free tier and plus tier ChatGPT
 *   conversations represents a structural extraction mechanism disguised as a
 *   coordination necessity. Users implicitly consent (through disclosed but
 *   unread terms) to having their conversations incorporated into training
 *   data, improving the model at their expense. The policy is architecturally
 *   suppressive: opting out requires discovering and navigating settings that
 *   are not prominently advertised. The extraction is structurally
 *   asymmetric: OpenAI captures the training signal value while users receive
 *   a bounded service. From the free tier user perspective, this is a pure
 *   snare — no meaningful exit, no compensation, trapped by service
 *   dependency and information asymmetry. From OpenAI's perspective, it is
 *   coordination — solving the problem of obtaining diverse, real-world
 *   training data. From the paid user perspective, it is still snare because
 *   payment does not escape the training extraction. From regulators and
 *   privacy advocates, it is tangled rope: real coordination function
 *   (continuous improvement requires signal) but unjustified extraction
 *   (default bias toward contribution rather than explicit opt-in). The
 *   consent theater (Piton perspective) reflects that disclosure is
 *   performatively legitimate while functionally opaque.
 *
 * KEY AGENTS:
 *   - Free Tier Users: Primary victim (powerless/trapped) — no payment, complete service dependency, cannot exit without losing tool access
 *   - ChatGPT Plus Users: Secondary victim (moderate/constrained) — paid subscription but still defaults to training data extraction, switching costs prevent true arbitrage exit
 *   - OpenAI Corporation: Primary beneficiary (institutional/arbitrage) — captures training data value without explicit per-user negotiation, can change policy unilaterally
 *   - Privacy Advocates & Regulators: Organized agent (organized/constrained) — see coordination problem AND extraction, can impose costs (GDPR fines, legislation) but cannot force architectural change without innovation tradeoffs
 *   - Consent Documentation System: Institutional actor (institutional/arbitrage) — maintains disclosure ritual that satisfies legal requirements while functioning as theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choice (default opt-in) as inherent necessity of model improvement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_default_data_training, 0.58).
domain_priors:suppression_score(openai_default_data_training, 0.68).
domain_priors:theater_ratio(openai_default_data_training, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_default_data_training, extractiveness, 0.58).
narrative_ontology:constraint_metric(openai_default_data_training, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(openai_default_data_training, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_default_data_training, snare).
narrative_ontology:human_readable(openai_default_data_training, "Default Use of ChatGPT User Data for Model Training").
narrative_ontology:topic_domain(openai_default_data_training, "technology/platform_governance").

domain_priors:requires_active_enforcement(openai_default_data_training).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_default_data_training, openai_corporation).
narrative_ontology:constraint_victim(openai_default_data_training, chatgpt_free_tier_users).
narrative_ontology:constraint_victim(openai_default_data_training, chatgpt_plus_users).
narrative_ontology:constraint_victim(openai_default_data_training, user_data_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FREE TIER USER (SNARE) — Trapped in the constraint by service dependency; cannot meaningfully opt out without losing access to the tool. Data extraction is structural condition of service access. No viable alternatives at comparable quality/cost. Suppression through information asymmetry: default-to-training is the path-dependent norm, with opt-out requiring navigation of settings.
constraint_indexing:constraint_classification(openai_default_data_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PAID PLUS USER (SNARE) — Nominally pays for premium access but still defaults to training data contribution unless opt-out is discovered. Even with pricing paid, has constrained rather than arbitrage exit: switching costs (lost conversation history, workflow integration), lock-in through complementary services. Extraction persists despite payment because the default architecture assumes consent.
constraint_indexing:constraint_classification(openai_default_data_training, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: OPENAI CORPORATION (ROPE) — Experiences the constraint as coordination: user data enables continuous model improvement, which provides immediate feedback loop for safety and capability gains. Has full arbitrage options (can change policy unilaterally, can shift to other data sources, can monetize training data separately). Net beneficiary — extraction flows toward this agent. The constraint solves the problem of obtaining training signal without explicit per-user negotiation.
constraint_indexing:constraint_classification(openai_default_data_training, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVACY ADVOCATES & REGULATORS (TANGLED ROPE) — Organized agents (GDPR enforcers, privacy groups, civil society) see a coordination problem (need for training data) that is being solved via asymmetric extraction (default opt-in rather than explicit consent). They have constrained exit: can impose fines (EU) or threaten legislation, but cannot force architectural change without economic costs to innovation. This perspective sees both a real coordination function (continuous improvement requires signal) AND unjustified extraction (default bias toward consent rather than explicit opt-in).
constraint_indexing:constraint_classification(openai_default_data_training, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSENT THEATER (PITON) — The terms-of-service disclosure of data training, while technically transparent, functions as performative consent. Users do not read, do not understand implications of 'your data improves our models,' and the default-to-yes architecture makes the stated option ('you can disable') functionally invisible. The consent mechanism is degraded — it persists through institutional inertia (tech industry norm) rather than functional legitimacy. Theater ratio high because the disclosure ritual satisfies legal requirements while the actual consent decision is predetermined by defaults.
constraint_indexing:constraint_classification(openai_default_data_training, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, training data is inherent to capability improvement: any system that improves over time must ingest signal, and user interactions are a natural source of that signal. The bottleneck is immutable — no AI system can improve without data. However, the structural data contradicts mountain classification: the constraint is not about WHETHER to use data, but HOW — default vs explicit, opt-in vs opt-out, transparent vs hidden. These are contingent policy choices, not laws of nature. The false summit reveals that 'we need training data' naturalizes the specific extraction mechanism (default opt-in).
constraint_indexing:constraint_classification(openai_default_data_training, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_default_data_training_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openai_default_data_training, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_default_data_training, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openai_default_data_training, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(openai_default_data_training, TR),
    TR >= 0.70.

:- end_tests(openai_default_data_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. Free tier users receive a bounded service (access to model) in exchange for unlimited training data. The asymmetry is real — compute costs of serving free tier are lower than market value of training data obtained. However, extractiveness is not 0.8+ because users do receive genuine service value (access to a capable tool at zero cost). The value exchange is substantially imbalanced but not entirely one-way. Suppression (0.68): Moderate-high. Multiple suppressive mechanisms: information asymmetry (users do not read or understand terms), architectural default (training is opt-out not opt-in, requiring settings navigation), social pressure (network effects make defecting costly), and lack of alternatives at comparable quality/cost. Theater ratio (0.55): Moderate. The disclosure of data training in terms-of-service is technically transparent but functionally opaque — the language 'to improve our models' does not convey the scope or value extraction of training data ingestion. The opt-out mechanism exists but is not prominently advertised. The theater is real but not dominant — some users do discover and exercise the opt-out, indicating the mechanism is not purely performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. OpenAI sees coordination (Rope) — training data enables capability improvement that benefits all users through better models. Free tier users see extraction (Snare) — they are locked in, data is extracted without compensation or meaningful consent, they have no alternatives. Regulators see mixed extraction-coordination (Tangled Rope) — legitimate need for training data but unjustified default-to-yes architecture. The consent theater (Piton) is performatively legitimate but functionally inert — disclosure exists but is invisible to most users. The false summit (Mountain) naturalizes the policy choice as inherent necessity when the real necessity is only 'training data,' not 'default opt-in.' If OpenAI required explicit opt-in instead of opt-out, the coordination function (obtaining training signal) would remain, but the extraction mechanism would be reset to zero — the snare would collapse. This demonstrates that the extraction is architectural, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   The free tier user's directionality (d) is derived from powerless+trapped: they receive service value but have no genuine exit, cannot negotiate terms, bear extraction without compensation. This produces high d → high f(d) → high χ. The paid user's directionality is moderate+constrained: they have paid for premium access but are still trapped by switching costs and the same default training extraction. Slightly lower d than free tier because of payment, but still high because of constrained exit. OpenAI's directionality is institutional+arbitrage: they are beneficiary with full exit options (can change policy, can adopt alternative data sources), producing low d → negative f(d) → extraction flows toward them. The piton classification derives from theater_ratio ≥ 0.70 gate not firing (theater is 0.55); instead, piton arises from the consent ritual being performatively legitimate while functionally opaque — a degraded coordination mechanism maintained by institutional inertia rather than structural necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the coordination function (continuous model improvement via training data) is real and necessary, but the extraction mechanism (default opt-in, suppressed opt-out) is contingent policy. A hypothetical alternative system — explicit opt-in with per-user compensation, or separate training data markets — could achieve the same coordination benefit with lower extraction. The snare classification is therefore not 'we extract because we must improve models' but 'we extract via default architecture when alternatives exist.' The paid user perspective (also snare despite payment) shows that the extraction is not a service fee but a data extraction mechanism independent of pricing. The regulatory perspective (tangled rope) validates that there IS a coordination problem while rejecting that default extraction is the only solution. The false summit reveals the mandatrophy: the analytical observer's claim that 'training data is inherent to AI' is true but naturizes the policy choice of HOW to obtain it. The constraint is not about WHETHER to use data, but whether free tier users should be extracted from via default without explicit consent and without compensation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaningful_consent_standard,
    'What level of user understanding and deliberation constitutes meaningful informed consent for training data contribution?',
    'User comprehension studies: do users understand that ''improving our models'' means their conversations are ingested into training data? Follow-up: do users understand the downstream uses and cannot prevent them once data is accepted?',
    'If comprehension < 30%: current consent is spurious and should be reclassified from snare-with-disclosure to pure snare. If comprehension > 70%: consent mechanism is functionally legitimate and extraction classification drops to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_consent_standard, empirical, 'User comprehension threshold for informed consent').

omega_variable(
    alternative_training_sufficiency,
    'Can OpenAI achieve comparable model performance using only API and explicitly consenting data sources, without default-training-by-free-tier?',
    'Comparative capability analysis: model trained on opt-in data only vs current model; comparison of performance gaps in MMLU, reasoning, domain specialization; cost and time differential to equivalent capability',
    'If performance gap < 5% and cost increase < 20%: default training is extraction optimization, not necessity (Snare classification confirmed). If performance gap > 20%: default training is coordination necessity (Rope/Tangled Rope classification supported).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_training_sufficiency, empirical, 'Whether default training is necessary for capability').

omega_variable(
    opt_out_visibility_and_uptake,
    'What percentage of users are aware of, locate, and successfully exercise the opt-out mechanism? Is the opt-out placement designed to minimize discovery?',
    'Telemetry on opt-out clicks and setting changes; A/B testing on opt-out UI placement; user interviews on why they did/did not opt out',
    'If awareness < 15% and placement obscured: default-to-yes is architecturally suppressive (supports snare). If awareness > 50% and prominent placement: suppression is lower, classification shifts toward tangled_rope with legitimate coordination rationale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opt_out_visibility_and_uptake, empirical, 'Opt-out discovery and exercise rates').

omega_variable(
    data_value_asymmetry,
    'What is the market value of the training data extracted from free tier users, and does it proportionally exceed the value of free service they receive?',
    'Accounting for compute costs of model serving (free tier) vs market price of equivalent training data; comparison to data purchase prices from commercial sources; calculation of user value extraction ratio',
    'If asymmetry ratio > 3:1: extraction is substantial and snare classification is robust. If ratio < 1.5:1: users are receiving rough value equivalence and snare shifts toward tangled rope with reasonable coordination justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_value_asymmetry, empirical, 'Market value asymmetry of extracted training data').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_default_data_training, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(odt_tr_t0, openai_default_data_training, theater_ratio, 0, 0.48).
narrative_ontology:measurement(odt_tr_t2, openai_default_data_training, theater_ratio, 2, 0.52).
narrative_ontology:measurement(odt_tr_t4, openai_default_data_training, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(odt_be_t0, openai_default_data_training, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(odt_be_t2, openai_default_data_training, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(odt_be_t4, openai_default_data_training, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_default_data_training, resource_allocation).
narrative_ontology:affects_constraint(openai_default_data_training, large_language_model_training_data_externalities).
narrative_ontology:affects_constraint(openai_default_data_training, platform_user_consent_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the general problem of training data sourcing for large language models, but represents a specific institutional choice (default opt-in) rather than a structural necessity. The upstream constraint (LLM training data externalities) has different ε values depending on whether training uses purchased data, opt-in data, or default-extracted data. This story focuses on the default-extraction implementation and its snare properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
