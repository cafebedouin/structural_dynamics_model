% ============================================================================
% CONSTRAINT STORY: openai_default_data_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: openai_default_data_training
 *   human_readable: Default Use of ChatGPT User Data for Model Training
 *   domain: technological/data_governance
 *
 * SUMMARY:
 *   OpenAI's default data training policy extracts user conversations from
 *   the free ChatGPT tier to improve model capabilities. This constraint
 *   represents a structural asymmetry between what users expect from a 'free
 *   service' (access without loss of control) and what they actually receive
 *   (access in exchange for intellectual property transfer). The constraint
 *   exhibits all six DR types across different observer positions. From the
 *   free user's perspective, it is a snare — they cannot access the service
 *   without surrendering data rights, and the cost is total (all
 *   conversations are training data). From OpenAI's perspective, it is
 *   coordination — the policy aggregates diverse language patterns to improve
 *   model robustness for all downstream users (API customers, future free
 *   users, enterprise clients). From a regulatory perspective, it is a
 *   temporary scaffold — GDPR consent requirements and emerging state privacy
 *   laws are building enforcement mechanisms and opt-out pathways that will
 *   shift the policy toward explicit opt-in within 3-5 years. The policy's
 *   theater ratio (0.52) reflects that while the terms of service do disclose
 *   the practice, the disclosure is performative: it occurs in lengthy TOS
 *   documents that users do not read and cannot meaningfully act upon. The
 *   constraint's extractiveness has increased over 12 months (0.32 → 0.58) as
 *   model training has intensified and regulatory pressure has made the issue
 *   more salient, causing users to recognize the asymmetry.
 *
 * KEY AGENTS:
 *   - Free ChatGPT Users (powerless/trapped): Primary victims — surrender all conversation data without meaningful consent or compensation
 *   - OpenAI Model Development (institutional/arbitrage): Primary beneficiary — captures value of user-generated training data at scale
 *   - Privacy-Conscious Users (moderate/constrained): Secondary victims — can opt out but face friction and unclear consequences
 *   - Privacy Regulators & Advocacy Organizations (organized/mobile): Enforcement coalition building regulatory pressure and alternative data pathways
 *   - Competing AI Companies (institutional/constrained): Secondary victims of regulatory asymmetry; also benefit from industry-wide data access norms
 *   - Terms-of-Service Ritual (institutional): Performative disclosure mechanism that persists despite non-function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_default_data_training, 0.58).
domain_priors:suppression_score(openai_default_data_training, 0.68).
domain_priors:theater_ratio(openai_default_data_training, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_default_data_training, extractiveness, 0.58).
narrative_ontology:constraint_metric(openai_default_data_training, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(openai_default_data_training, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_default_data_training, tangled_rope).
narrative_ontology:human_readable(openai_default_data_training, "Default Use of ChatGPT User Data for Model Training").
narrative_ontology:topic_domain(openai_default_data_training, "technological/data_governance").

domain_priors:requires_active_enforcement(openai_default_data_training).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_default_data_training, openai_model_development).
narrative_ontology:constraint_beneficiary(openai_default_data_training, future_api_users).
narrative_ontology:constraint_victim(openai_default_data_training, chatgpt_free_users).
narrative_ontology:constraint_victim(openai_default_data_training, user_privacy_expectations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FREE CHATGPT USER (SNARE) — Cannot exit without losing access to service; bears full cost of data extraction through intellectual property transfer and privacy loss. No meaningful choice architecture presented at signup. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(openai_default_data_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-CONSCIOUS USER (TANGLED ROPE) — Can opt out through settings menu, but friction is high (buried in settings, unclear consequences). Receives coordination benefit (improved model) but asymmetrically: benefits accrue slowly and universally while extraction is immediate and individual. d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.71.
constraint_indexing:constraint_classification(openai_default_data_training, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: OPENAI MODEL DEVELOPMENT (ROPE) — Experiences this as pure coordination mechanism: aggregating diverse conversation patterns improves model robustness and generalization. Can arbitrage by choosing which data to use, which cohorts to sample from. Sees users as cooperative partners in knowledge production. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.08. Negative extraction = net beneficiary with coordination function.
constraint_indexing:constraint_classification(openai_default_data_training, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — GDPR enforcement actions, state privacy laws (CCPA, PIPEDA), and organized advocacy (EFF, privacy nonprofits) are building exit pathways and cost-imposing mechanisms. The default-extraction model is under structural pressure to become explicit opt-in with regulatory deadlines. χ≈0.25 reflects that the enforcement mechanism is strong but still building. Sunset estimated 3-5 years as GDPR consent requirements and state laws mature.
constraint_indexing:constraint_classification(openai_default_data_training, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TOS RITUAL (PITON) — Terms of Service disclose data training by default, but at theater_ratio≈0.62: the disclosure exists, but users do not read it, do not understand it, and cannot meaningfully act on it. The ritual of 'informed consent through TOS' persists despite universal evidence of its non-function. Platform maintains it through institutional inertia (legal cover) rather than genuine consent mechanism.
constraint_indexing:constraint_classification(openai_default_data_training, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPETITOR INSTITUTIONS (TANGLED ROPE) — Face same regulatory pressure as OpenAI; also benefit from industry-wide data access (creates higher baseline for all models). But constrained by regulatory asymmetry (OpenAI's first-mover advantage in default extraction is now regulated). d≈0.55, f(d)≈0.73, σ=1.2 → χ≈0.51. Hybrid: benefit from coordination (large training datasets) but bear costs of regulatory catch-up and user backlash.
constraint_indexing:constraint_classification(openai_default_data_training, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

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
 *   Extractiveness (0.58): Moderate-high and rising. OpenAI captures substantial value from user-generated training data — conversations reflect diverse reasoning patterns, edge cases, and domain knowledge that would require expensive synthesis to replicate. The value to OpenAI is clear (improved model performance, competitive advantage, faster training). The value to users is indirect (marginally better future model access) and diluted across a billion-user base. Initial measurement (0.32) reflected pre-ChatGPT-4 era when policy was less salient. Current measurement (0.58) reflects intensified training + regulatory pressure making the asymmetry visible. Suppression (0.68): High. Multiple suppression mechanisms: (1) Technical: opt-out is buried in account settings; (2) Informational: TOS disclosure is unread and incomprehensible; (3) Structural: no alternative to free access except paid tier (which also uses data); (4) Cognitive: users don't understand that 'conversations used to improve the model' means their specific intellectual work is training data; (5) Institutional: regulatory frameworks for consent are still building. Theater ratio (0.52): Moderate. The constraint has genuine functional content (data really is used for training) but also significant performative content (the TOS disclosure ritual creates legal cover while not enabling actual consent). The theater_ratio reflects the mixed character: some institutional transparency exists, but it doesn't translate to user agency.
 *
 * PERSPECTIVAL GAP:
 *   The snare and scaffold perspectives reveal the core mandatrophy tension. From the free user's position (snare), this is pure extraction with no coordination benefit — they receive improved models but did not consent to become training data providers. From OpenAI's position (rope), this is coordination — data aggregation solves a genuine problem (improving generalization across diverse language patterns). From the regulatory position (scaffold), this is a temporary enforcement problem being solved — GDPR and state privacy laws are building mechanisms to shift to explicit opt-in. The gap is not about the facts (data is used for training; this is disclosed in TOS) but about structural position: does the user have meaningful choice, and does the benefit justify the cost? The snare and rope perspectives have irreconcilable assessments of whether the policy is extractive or coordinative. The scaffold perspective shows that the regulatory system is building a path to resolve this: explicit opt-in with transparency will become mandatory within regulatory jurisdictions, forcing OpenAI to offer a genuine choice architecture. The piton perspective reveals that the TOS ritual persists despite knowing it doesn't work — the legal fiction of 'informed consent via terms of service' continues because it provides institutional cover, not because it achieves actual consent.
 *
 * DIRECTIONALITY LOGIC:
 *   Free ChatGPT users: Victim + trapped → d≈0.92, f(d)≈1.40. Powerless to resist; extraction is total. Privacy-conscious users: Victim + constrained → d≈0.68, f(d)≈1.05. Can opt out but at cost; extraction is asymmetric. OpenAI (development): Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Full beneficiary; can choose which data to use, which cohorts to sample. Net beneficiary position drives rope classification. Regulatory coalition: Organized + mobile → d≈0.45, f(d)≈0.48. Has agency and exit mechanisms (enforcement, alternative regulations); effectiveness is building. Competing companies: Institutional + constrained → d≈0.55, f(d)≈0.73. Benefit from industry-wide data norms but constrained by regulatory catch-up. The directionality spread (0.08 to 0.92) is unusually wide, indicating high structural asymmetry. This asymmetry drives the mandatrophy resolution: the snare and rope perspectives are not measuring the same constraint from different angles — they have genuinely opposite classifications because the structural positions are opposite.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: The apparent mandatrophy (why is this both rope and snare?) resolves when we recognize that 'default data training' conflates two distinct structural claims: (1) The technical policy (OpenAI uses conversation data for model training) — this is factual and disclosed, supporting a rope classification if users actually consent; (2) The consent mechanism (TOS disclosure with buried opt-out) — this fails to produce meaningful choice, supporting a snare classification if users cannot actually refuse. The true constraint is #2: the consent mechanism. The technical policy could be rope (coordination with actual consent) or snare (extraction with faked consent). The current implementation is snare because suppression (buried opt-out, incomprehensible TOS) prevents meaningful exit. The scaffold resolution path is regulatory enforcement of explicit opt-in: forcing a choice architecture where users must affirmatively authorize data training. This would convert the snare to rope for those who consent (explicit choice) and eliminate it for those who don't (opt-out with no service loss). The regulation doesn't change the technical policy — it changes the suppression mechanism from high (TOS theater) to low (explicit choice). The mandatrophy is thus not a permanent ambiguity but a descriptor of the current institutional moment: the snare classification is correct now; the scaffold classification is correct for the near future (~3-5 years); the rope classification is correct only if users explicitly consent and understand the tradeoff.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_meaningfulness_threshold,
    'At what friction level does an opt-out mechanism constitute meaningful consent versus performative choice architecture?',
    'Empirical study of actual opt-out rates with different UI/UX treatments; behavioral economics analysis of choice architecture effects on data donation decisions',
    'If opt-out at <10% baseline → mechanism is performative theater (snare classification holds). If opt-out at >40% baseline → mechanism is genuinely meaningful (tangled_rope confirmed). Determines whether suppression should be 0.75+ (snare) or 0.55 (tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_meaningfulness_threshold, empirical, 'What friction level makes opt-out meaningful vs performative').

omega_variable(
    data_value_asymmetry_quantification,
    'What is the actual financial/capability value of user conversation data to model training versus the value of free model access to users?',
    'Comparative analysis: cost of equivalent synthetic data generation, model performance degradation studies with/without user conversation data, user pricing models that offer equivalent capabilities',
    'If data value >> access value: snare classification strengthened (pure extraction). If data value ≈ access value: tangled_rope confirmed (genuine coordination benefit traded for data). Affects beneficiary/victim asymmetry quantification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_value_asymmetry_quantification, empirical, 'Relative value of user data versus service access').

omega_variable(
    regulatory_enforcement_speed,
    'Will privacy regulations (GDPR consent requirements, CCPA, emerging state/national frameworks) force default-extraction models to explicit opt-in before or after the industry reaches saturation training data?',
    'Timeline analysis: regulatory enforcement velocity in digital markets, EU enforcement actions against tech platforms, state-by-state compliance deadlines, intersection with model saturation point',
    'If enforcement arrives before saturation: scaffold sunset is real (3-5 years). If enforcement arrives after saturation: default-extraction becomes de facto permanent institutional feature (piton classification). Determines whether this is temporary coordination friction or durable extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_speed, empirical, 'Will regulation force opt-in before training data saturation').

omega_variable(
    user_understanding_of_policy,
    'What percentage of free ChatGPT users accurately understand that their conversations are used for model training by default?',
    'Longitudinal user surveys; randomized disclosure experiments; analysis of support ticket patterns indicating confusion',
    'If understanding <20%: suppression 0.75+ (users trapped in ignorance). If understanding 40-60%: suppression 0.55-0.65 (mixed informed/uninformed cohorts). If understanding >75%: suppression 0.40 (genuine informed choice). Affects snare vs tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_understanding_of_policy, empirical, 'Actual user understanding of default data training policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_default_data_training, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oai_ddt_tr_t0, openai_default_data_training, theater_ratio, 0, 0.38).
narrative_ontology:measurement(oai_ddt_tr_t6, openai_default_data_training, theater_ratio, 6, 0.45).
narrative_ontology:measurement(oai_ddt_tr_t12, openai_default_data_training, theater_ratio, 12, 0.52).

% Extraction over time
narrative_ontology:measurement(oai_ddt_be_t0, openai_default_data_training, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(oai_ddt_be_t6, openai_default_data_training, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(oai_ddt_be_t12, openai_default_data_training, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_default_data_training, information_standard).
narrative_ontology:boltzmann_floor_override(openai_default_data_training, 0.35).
narrative_ontology:affects_constraint(openai_default_data_training, llm_training_data_sourcing).
narrative_ontology:affects_constraint(openai_default_data_training, ai_model_bias_amplification).
narrative_ontology:affects_constraint(openai_default_data_training, user_privacy_erosion_platforms).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the general problem of LLM training data sourcing but represents a distinct structural point: the default vs opt-in policy architecture. The upstream constraint (llm_training_data_sourcing) addresses the technical challenge of acquiring diverse training data; this constraint (openai_default_data_training) addresses the institutional implementation — whether users are asked or assumed to consent. A separate constraint story could address the data quality/bias consequences (ai_model_bias_amplification). All three are linked by information flow: default data policies drive sourcing practices, which drive bias propagation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openai_default_data_training, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
