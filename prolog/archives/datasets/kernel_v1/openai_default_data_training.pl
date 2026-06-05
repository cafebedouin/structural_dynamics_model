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
 *   OpenAI's default-to-training policy for ChatGPT's free and Plus tiers
 *   represents a structural extraction mechanism where user conversations are
 *   automatically incorporated into model training without prominent
 *   disclosure or compensation. Users implicitly consent through
 *   disclosed-but-unread terms of service, and opting out requires
 *   discovering and navigating settings that are architecturally suppressed —
 *   not presented at service entry, not highlighted as material, buried in
 *   account preferences several levels deep. The constraint exhibits all six
 *   DR types from different perspectives, with particular diagnostic value
 *   for false summit detection: regulators and analytical observers may frame
 *   the policy as an immutable technological necessity ('frontier LLMs
 *   require training data at unprecedented scale'), but the structural data
 *   reveals it as a governance choice. Alternative architectures exist:
 *   OpenAI could default to opt-in with prominent disclosure, offer tiered
 *   compensation, or implement transparent data tracking. The architectural
 *   choices — default inclusion, buried settings, obscured disclosure — are
 *   not technological requirements but governance decisions that maximize
 *   extraction by minimizing user agency.
 *
 * KEY AGENTS:
 *   - Free tier users: Primary victims (powerless/trapped) — receive service valued at zero cost; data extraction is architecturally suppressed through invisible opt-out
 *   - Plus tier users: Secondary victims (moderate/constrained) — paid subscribers but data still defaulted to training; some agency through sunk cost but suppression is real
 *   - OpenAI: Primary beneficiary (institutional/arbitrage) — captures training data at negligible cost; solves coordination problem of assembling training data at scale
 *   - Model training commons: Abstract victim (powerless/trapped) — epistemic collective good produced at hidden cost; zero compensation, zero visibility, zero agency
 *   - Data privacy regulators & advocates: Organized contestants (organized/constrained) — have leverage to demand change but also recognize training-at-scale coordination requirement
 *   - Platform terms of service: Institutional theater (institutional/arbitrage) — disclosure exists but is engineered to be invisible; provides legal cover while suppressing actual user choice
 *   - Analytical observer: Civilizational risk (analytical/analytical) — may naturalize contingent governance choice as technological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_default_data_training, 0.58).
domain_priors:suppression_score(openai_default_data_training, 0.72).
domain_priors:theater_ratio(openai_default_data_training, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_default_data_training, extractiveness, 0.58).
narrative_ontology:constraint_metric(openai_default_data_training, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(openai_default_data_training, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_default_data_training, snare).
narrative_ontology:human_readable(openai_default_data_training, "Default Use of ChatGPT User Data for Model Training").
narrative_ontology:topic_domain(openai_default_data_training, "technology/platform_governance").

domain_priors:requires_active_enforcement(openai_default_data_training).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_default_data_training, openai).
narrative_ontology:constraint_victim(openai_default_data_training, chatgpt_free_users).
narrative_ontology:constraint_victim(openai_default_data_training, chatgpt_plus_users).
narrative_ontology:constraint_victim(openai_default_data_training, model_training_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FREE TIER USER (SNARE) — Structurally trapped. User receives service (chatbot access) valued at zero monetarily; switching costs are near-zero materially but psychologically high (workflow disruption, relearning alternatives). The data extraction is architecturally suppressive: opt-out requires discovering settings buried in account preferences with no prominent disclosure at service entry. User perceives ChatGPT as 'free' but is paying in training data without meaningful choice architecture. Maximum extraction from this perspective — no exit capacity, no compensation, suppressed awareness.
constraint_indexing:constraint_classification(openai_default_data_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLUS TIER USER (TANGLED ROPE) — Paid subscriber ($20/month) but data is still defaulted to training pipeline. User has slightly higher agency than free tier (can opt out more easily after discovery, has sunk investment making exit costly). Benefits genuinely from improved model (default training makes model better). Extraction is real but tempered by coordination value — the user's contribution directly improves the service they use. Still suppressive (opt-out is buried) but not maximal — some agency, some genuine benefit, asymmetric extraction.
constraint_indexing:constraint_classification(openai_default_data_training, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: OPENAI (ROPE) — Benefits massively from default data pipeline. Data is essential coordination input for model improvement. The constraint solves real coordination problem: aggregating training data at scale. From OpenAI's perspective, this is governance necessity — distributing opt-out burden upstream (to users) rather than downstream (to data processors) reduces friction. Experiences the constraint as coordination: default inclusion enables high-quality training. Low experienced extraction from beneficiary — the constraint benefits this agent substantially.
constraint_indexing:constraint_classification(openai_default_data_training, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODEL TRAINING COMMONS (SNARE) — Abstract epistemic commons that cannot organize or exit. Training data provenance is systematically obscured (users do not know their data improved specific model versions or applications). No feedback loop to users about their contribution value or model performance. Extraction is maximum — zero compensation, zero visibility, zero agency. The collective good (better AI models) is produced at the hidden cost of individual data extraction.
constraint_indexing:constraint_classification(openai_default_data_training, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: DATA PRIVACY ADVOCATES / REGULATORS (TANGLED ROPE) — Organized agents (GDPR enforcement bodies, consumer protection groups, EU AI Act compliance officers) see this as extractive by design. But enforcement creates coordination function: regulatory pressure is forcing OpenAI to implement better opt-out mechanisms and transparency. Constraint exhibits both extraction (users cannot easily know or control data use) and enforcement (regulatory bodies can impose costly compliance). Experience is mixed — genuine leverage to demand change but also recognition that default-opt-out is how training at scale gets done. Extraction is significant but contestable.
constraint_indexing:constraint_classification(openai_default_data_training, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PLATFORM TERMS OF SERVICE THEATER (PITON) — The disclosed-but-unread ToS is largely performative. OpenAI discloses the data practice (legally defensible) but the disclosure is engineered to be invisible: buried in 15,000-word terms, not presented at account creation, not highlighted as a material consideration. The theater persists because it provides legal cover while suppressing actual user choice. Theater ratio (0.65) reflects that transparency exists but is architecturally obscured. The system is inertial — disclosure without salience is legally safe but functionally meaningless.
constraint_indexing:constraint_classification(openai_default_data_training, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, one might argue that large-scale model training requires data at unprecedented scale, and that user data is the only feasible source — making default inclusion an immutable necessity of the technology frontier. 'You cannot build frontier LLMs without human feedback at scale; therefore, user data must be incorporated; therefore, extraction is inherent.' However, the structural data contradicts this mountain classification: the suppressive choice architecture (buried settings, no prominent disclosure) is not a technological necessity — it is a governance decision. Alternative designs exist (opt-in with prominent disclosure, tiered compensation, transparent data tracking). The engine will classify this as a false summit, revealing that 'inevitable technological requirement' naturalizes a contingent business choice.
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
    constraint_indexing:constraint_classification(openai_default_data_training, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderately high. The value extracted from users (training data) is substantial and asymmetric. Users receive service access but do not receive compensation or even accurate valuation of their data contribution. The extraction is not maximal (snare-level ε ≥ 0.66) because there is genuine coordination value — user data does improve the model that users benefit from. However, the asymmetry is severe: OpenAI captures the vast majority of value (trained model, access monopoly, revenue from downstream API/Plus tiers) while users capture only service improvement they cannot quantify. The value disparity is the extraction signal. Suppression (0.72): High. The architectural suppression is systematic and deliberate. Opt-out requires: (1) awareness that the option exists (not discoverable at entry), (2) navigation to settings (buried 3–4 levels deep in account preferences), (3) interpretation of technical language (terms like 'conversation history' and 'model training' are not salient to typical users). The ToS disclosure is legally defensible but functionally meaningless — 15,000-word document not read at signup, not highlighted, not presented in plain language. Suppression is not total (opt-out IS technically available to those who search for it) but is engineered to minimize opt-out rates. Theater ratio (0.65): Moderate-high. The disclosure theater is substantial: OpenAI publishes detailed privacy policies, implements opt-out buttons, and claims transparency. But the theater is architecturally engineered to be performative. The discrepancy between disclosure completeness and disclosure salience is the theater signal. Rising trajectory (0.58→0.65 over 4 time periods) reflects that regulators are increasingly demanding transparency, forcing OpenAI to add more explicit disclosures, which increases the theater (performative compliance) without materially increasing user agency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Free tier users perceive pure extraction with no exit and no compensation (snare — χ ≈ 0.99). Plus tier users perceive mixed coordination and extraction with constrained exit and some genuine benefit (tangled rope — χ ≈ 0.52). OpenAI perceives coordination necessity with massive benefit and no experienced extraction (rope — negative χ from beneficiary perspective). Regulators perceive a constraint that is contestable and increasingly enforced (tangled rope with organized leverage). The abstract training commons perceives hidden extraction with zero agency (snare). The ToS theater persists because the architectural choice satisfies OpenAI (benefits from data without disclosure friction), satisfies regulatory letter-of-law (data use is disclosed), and suppresses user agency (most never opt out because most never discover the option). The perspectival divergence is not a failure of the framework — it is the diagnostic signal that the constraint IS extractive: it simultaneously looks like coordination to the beneficiary, and like pure extraction to the trapped users.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is computed from structural position: power level, exit options, beneficiary/victim status. Free tier users (powerless + trapped + victim) derive d ≈ 0.95, producing maximum f(d) ≈ 1.42 and high experienced extraction. Plus users (moderate + constrained + mixed beneficiary-victim) derive d ≈ 0.55, producing f(d) ≈ 0.75 and moderate extraction. OpenAI (institutional + arbitrage + beneficiary) derives d ≈ 0.05, producing f(d) ≈ -0.12 and negative/low extraction (they are extracting from users, not experiencing extraction). Scope modifier σ(global) = 1.2 amplifies extractiveness: χ = 0.58 × f(d) × 1.2. For the trapped free user: χ ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 (extreme effective extraction). For the constrained Plus user: χ ≈ 0.58 × 0.75 × 1.2 ≈ 0.52 (moderate-high). The perspectival gap is driven by these differential χ values: the same constraint appears as a snare to trapped users, tangled rope to Plus users, and rope to OpenAI.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that this constraint is structurally snare-like with a thin rope coating. OpenAI frames the policy as coordination necessity ('we need training data to improve your model'; 'this benefits users through better AI'). This is partially true — user data does improve the model users benefit from. But the framing obscures the asymmetric value capture: OpenAI's benefit (access to training data worth billions in alternative acquisition cost) vastly exceeds users' benefit (incremental model improvement they receive for free). The snare structure is confirmed by: (1) suppressive opt-out architecture (powerless users cannot easily exit), (2) zero compensation (users receive no payment despite value extraction), (3) invisible extraction (users do not know or cannot control data use), (4) no feedback loop (users never learn what value their data contributed or how it improved the model). The rope coating is thin: genuine model improvement does occur, and users do benefit. But the extraction asymmetry is severe. Classification as snare (not tangled rope) is justified when suppression (0.72) is weighted appropriately — it indicates that the constraint's primary function is data extraction, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    model_improvement_attribution,
    'How much of ChatGPT''s empirical improvements from v1 to v4 are attributable to user conversation data versus synthetic data, RLHF feedback, or reinforced learning from structured sources?',
    'Ablation studies comparing model performance with/without user conversation data; OpenAI technical documentation on training pipeline composition; comparison of model performance trajectories across user-data-heavy vs user-data-light training phases',
    'If user data contributes >30% of improvement: extraction is instrumentally justified as coordination (users benefit from better model). If user data contributes <10%: extraction is gratuitous, and constraint reclassifies toward pure snare (no coordination justification). If attribution is opaque: omega cannot resolve empirically, indicating structural obfuscation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(model_improvement_attribution, empirical, 'Empirical attribution of model improvements to user conversation data').

omega_variable(
    opt_out_salience_engineering,
    'Is the architectural choice to bury opt-out settings a technical necessity or a deliberate choice to minimize opt-out rates?',
    'A/B test: present prominent opt-out banner at login with clear explanation of data use, measure opt-out rate change; compare opt-out rates pre/post UI redesign highlighting settings; user survey on awareness of opt-out existence',
    'If opt-out rate stays <5% after prominent disclosure: suppression is structural (users genuinely prefer default inclusion). If opt-out rate jumps to 20%+ after disclosure: suppression is engineered (architectural choice to hide opt-out, not genuine preference). Constraint may reclassify from snare toward rope if user preference is revealed as genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opt_out_salience_engineering, empirical, 'Whether opt-out burial is technical necessity or deliberate suppression').

omega_variable(
    data_value_disparity,
    'What is the economic value of user-generated training data relative to the service value users receive (free model access or Plus subscription value)?',
    'Market valuation: compare OpenAI''s revenue growth aligned with user-data cohorts to growth from other revenue sources; estimate training data cost if outsourced (e.g., via DataBricks, Hugging Face, synthetic data providers); survey users'' willingness-to-accept for data contribution',
    'If data value >> service value: extraction is asymmetric and unjustified (snare). If data value ≈ service value: extraction is symmetric compensation (rope). Value disparity is the empirical measure of extraction severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_value_disparity, empirical, 'Economic value of user data relative to service received').

omega_variable(
    false_summit_natural_law_claim,
    'Is default data inclusion an immutable technological necessity or a contingent governance choice?',
    'Feasibility analysis: can frontier LLM training operate with opt-in-only data (potentially at slower improvement rate or higher cost)? Do closed-source competitors (Claude, Gemini, Grok) achieve comparable performance with different data governance? Can synthetic or licensed data replace user conversations at acceptable cost?',
    'If frontier training is possible without default inclusion: mountain classification is a false summit (natural law claim naturalizes contingent choice). If frontier training is genuinely constrained by data scarcity: mountain classification may be justified. This omega is the diagnostic signal for FSM engine evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether default data inclusion is technological necessity or governance choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_default_data_training, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(odt_tr_t0, openai_default_data_training, theater_ratio, 0, 0.58).
narrative_ontology:measurement(odt_tr_t2, openai_default_data_training, theater_ratio, 2, 0.62).
narrative_ontology:measurement(odt_tr_t4, openai_default_data_training, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(odt_be_t0, openai_default_data_training, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(odt_be_t2, openai_default_data_training, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(odt_be_t4, openai_default_data_training, base_extractiveness, 4, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(odt_su_t0, openai_default_data_training, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(odt_su_t2, openai_default_data_training, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(odt_su_t4, openai_default_data_training, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_default_data_training, resource_allocation).
narrative_ontology:affects_constraint(openai_default_data_training, training_data_provenance_opacity).
narrative_ontology:affects_constraint(openai_default_data_training, llm_model_alignment_incentives).

% DUAL FORMULATION NOTE:
% The default data training policy is downstream of OpenAI's architectural decisions about training efficiency and upstream of broader constraints about model incentive structures. Training data provenance opacity (separate story, ε higher) models the systematic obscuring of data attribution; model alignment incentives (separate story) models how training dynamics shape output behavior. All three stories are linked via the data pipeline's structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
