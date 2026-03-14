% ============================================================================
% CONSTRAINT STORY: training_data_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_training_data_concentration, []).

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
 *   constraint_id: training_data_concentration
 *   human_readable: Training Data Concentration in Large Language Model Development
 *   domain: machine_learning/artificial_intelligence/data_governance
 *
 * SUMMARY:
 *   Training data concentration in large language model development
 *   represents a structural constraint where the technical requirements of
 *   building capable models (access to vast, diverse text datasets) have
 *   created an extractive arrangement extracting economic and intellectual
 *   property value from content creators and personal data from data
 *   subjects. The constraint exhibits the full spectrum of DR classifications
 *   depending on perspective: from pure extraction (Snare) for powerless
 *   content creators and data subjects, through mixed coordination-extraction
 *   (Tangled Rope) for downstream model users and data rights coalitions, to
 *   performative institutional inertia (Piton) for fair use legal doctrine,
 *   to naturalized immutability (Mountain) from the scaling-necessity
 *   perspective. The underlying extractiveness has increased from 0.35 to
 *   0.58 over the past 6 years as model capabilities have scaled, driven by
 *   the discovery that more data yields better performance and the
 *   regulatory/legal ambiguity that permits unconsented data use. The theater
 *   ratio (0.45) reflects that while data aggregation serves a genuine
 *   technical coordination function, much of the justification is
 *   performative — assertions of fair use legality, claims of public benefit,
 *   and appeals to open science progress that obscure the lack of creator
 *   consent and compensation mechanisms.
 *
 * KEY AGENTS:
 *   - Content Creators: Primary victims (powerless/trapped) — writers, artists, journalists, and researchers whose work is incorporated into training datasets without consent, compensation, or ability to prevent future use
 *   - Data Subjects: Primary victims (powerless/trapped) — individuals whose personal information, behavioral data, or biometric information appears in training datasets; no visibility or control over downstream model use
 *   - Model Developers/Aggregators: Primary beneficiaries (institutional/arbitrage) — OpenAI, Anthropic, Meta, Google, and other organizations that scrape, license, or purchase training data to build commercially valuable models
 *   - Downstream Model Users: Secondary victims and partial beneficiaries (moderate/constrained) — researchers, smaller organizations, and companies building on top of concentrated models; benefit from pre-training but inherit upstream data biases and choices
 *   - Data Governance Coalition: Organized agents (organized/constrained) — copyright advocates, artist collectives, data rights movements, privacy regulators, and legal actors attempting to establish consent and compensation mechanisms
 *   - Fair Use Legal Doctrine: Institutional actor (institutional/arbitrage) — copyright law framework that supposedly permits transformative use; currently performative and degraded in large-scale ML context
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (data aggregation enabled by regulatory ambiguity) as immutable scaling requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(training_data_concentration, 0.58).
domain_priors:suppression_score(training_data_concentration, 0.62).
domain_priors:theater_ratio(training_data_concentration, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(training_data_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(training_data_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(training_data_concentration, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(training_data_concentration, tangled_rope).
narrative_ontology:human_readable(training_data_concentration, "Training Data Concentration in Large Language Model Development").
narrative_ontology:topic_domain(training_data_concentration, "machine_learning/artificial_intelligence/data_governance").

domain_priors:requires_active_enforcement(training_data_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(training_data_concentration, model_developers).
narrative_ontology:constraint_beneficiary(training_data_concentration, data_aggregators).
narrative_ontology:constraint_victim(training_data_concentration, content_creators).
narrative_ontology:constraint_victim(training_data_concentration, data_subjects).
narrative_ontology:constraint_victim(training_data_concentration, downstream_model_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CREATOR (SNARE) — Writers, artists, and knowledge workers whose work is incorporated into training datasets without consent, compensation, or ability to opt out. No meaningful exit options. Work has been extracted into model weights with no recovery mechanism. The creator bears reputational and economic cost while receiving zero benefit from the derived models.
constraint_indexing:constraint_classification(training_data_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DATA SUBJECT (SNARE) — Individuals whose personal information, behavioral data, or biometric markers appear in training datasets. No consent mechanism. No visibility into which models trained on their data. No recourse if models make decisions affecting them. Suppression enforced through information asymmetry and technical opacity.
constraint_indexing:constraint_classification(training_data_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DOWNSTREAM MODEL USER / DEVELOPER (TANGLED ROPE) — Researchers and smaller organizations building on top of concentrated models (GPT, Claude, Llama). They benefit from access to pre-trained models that accelerate their work (coordination function). But they are also constrained by the data choices embedded in those models — biases, omissions, and errors in upstream training data are inherited and often invisible. Active enforcement through model licensing, API restrictions, and data exclusivity agreements.
constraint_indexing:constraint_classification(training_data_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MODEL DEVELOPER / DATA AGGREGATOR (ROPE) — Organizations (OpenAI, Anthropic, Meta, Google) that scrape, license, or purchase training data at scale. They experience the data concentration as a coordination mechanism solving the problem of how to build capable models from dispersed data sources. The constraint serves their interests: aggregating data enables model development. They have arbitrage options — they can shift between data sources, negotiate licenses, or develop proprietary datasets. Low effective extraction from their perspective.
constraint_indexing:constraint_classification(training_data_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DATA GOVERNANCE COALITION (TANGLED ROPE) — Organized agents (copyright advocates, data rights movements, privacy regulators, artist collectives) attempting to enforce consent and compensation mechanisms. They see the constraint as both coordination (establishing norms for fair data use) and extraction (the existing arrangements extract value without consent). Their exit option is constrained by regulatory complexity and enforcement challenges across jurisdictions. Active enforcement through litigation, regulation, and collective action.
constraint_indexing:constraint_classification(training_data_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FAIR USE DEFENSE (PITON) — Legal doctrine designed to permit transformative use of copyrighted material. Fair use was coherent in the context of individual researcher copying a page from a journal. In the context of training models on billions of copyrighted works, the fair use framework is substantially performative — courts are still applying 20th-century tests to 21st-century scale. The defense persists through institutional inertia (it's what copyright law says) despite functional degradation (the tests don't capture scale effects). Theater ratio reflects that legal compliance now consists largely of assertion of fair use status rather than substantive review.
constraint_indexing:constraint_classification(training_data_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SCALING NECESSITY VIEW (MOUNTAIN) — From a civilizational horizon, training capable language models requires access to vast amounts of text data. The data concentration appears immutable: you cannot build a 7B+ parameter model without enormous datasets. This perspective naturalizes the concentration as an inherent physical/computational constraint. However, the structural data contradicts this: the extraction, suppression, and active enforcement all indicate that the constraint is contingent institutional arrangement (favorable licensing, regulatory ambiguity, technical gatekeeping) rather than a natural law.
constraint_indexing:constraint_classification(training_data_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(training_data_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(training_data_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(training_data_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(training_data_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(training_data_concentration, TR),
    TR >= 0.70.

:- end_tests(training_data_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing. The model developer benefit from unconsented data use is significant — they acquire valuable training data at marginal cost despite the intellectual property and personal information contained within it. The extractiveness increased from 0.35 to 0.58 over 6 years as model scaling proved effective, creating stronger incentive to accumulate data. The value is not extracted uniformly — content creators lose intellectual property rights while data subjects lose privacy; model developers gain commercial advantage. Suppression (0.62): Moderate-high. Mechanisms include: technical opacity (creators cannot know which models trained on their work), regulatory ambiguity (fair use doctrine's applicability to ML training is unsettled), scale opacity (the sheer volume of data makes tracking individual contributions impossible), and information asymmetry (aggregators have full visibility; creators have none). But suppression is not total — litigation is now forcing disclosure, and regulatory efforts (EU AI Act, proposed US legislation) are increasing transparency. Theater ratio (0.45): Moderate. There is genuine coordination happening: aggregators must solve the technical problem of curating, cleaning, and managing training datasets at scale. But much of the justification is performative — appeals to open science and public benefit mask the lack of creator consent mechanisms. The fair use assertion is performative (unsettled in law but asserted confidently). The theater ratio is lower than in the verification_bottleneck example because the actual technical work is real, not purely ritual, but the legitimacy claims are thin.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon produces radically different classifications depending on agent position. Content creators (powerless/trapped) see a Snare — their work is extracted without consent. Data subjects (powerless/trapped) see a Snare — their information is extracted without visibility or recourse. Downstream model users (moderate/constrained) see Tangled Rope — they benefit from capable models but inherit upstream extraction and biases. Model developers (institutional/arbitrage) see Rope — they are solving the technical problem of data aggregation, and the resulting models are genuinely useful. The data governance coalition (organized/constrained) sees Tangled Rope with active enforcement — they recognize both the coordination problem (need for fair data governance norms) and the extraction (current unlicensed use), and they are attempting to shift the arrangement toward licensing and consent. The fair use doctrine (institutional/arbitrage) sees itself as Rope — it is meant to permit transformative use — but from the creator perspective, it is Piton (performative, degraded in the ML context). The scaling-necessity perspective (analytical/civilizational) risks seeing Mountain — training capable models requires data, therefore concentration is immutable — but the engine's false summit detector identifies this as naturalization of a contingent arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) derives from their position in the data flow and their exit options. Content creators have high d (they are targets of extraction) + trapped exit (they cannot prevent their work being used) → high f(d) → high χ ≈ 1.42. Model developers have low d (they are beneficiaries) + arbitrage exit (they can shift between data sources) → negative f(d) → low/negative χ ≈ -0.12. Downstream users have moderate-high d (they inherit upstream choices) + constrained exit (they can mitigate but at cost) → moderate f(d) → moderate χ ≈ 1.06. The scope modifier σ(S) scales these by global = 1.2, amplifying extractiveness across all perspectives. The formula χ = ε × f(d) × σ(S) = 0.58 × f(d) × 1.2 is applied separately for each agent, producing the perspectival gap where content creators experience χ ≈ 0.985 while model developers experience χ ≈ -0.084.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint operates simultaneously as a coordination mechanism (solving the technical problem of data aggregation) and an extraction mechanism (capturing creator value and data subject privacy without compensation or consent). The Tangled Rope classification is correct: the constraint has genuine coordination function (aggregating dispersed sources to enable model development) AND asymmetric extraction (beneficiaries capture value, targets bear cost). The false summit (mountain scaling-necessity view) is revealed by comparing this with the snare perspective — if concentration were truly immutable, it would appear immutable to all agents, not just to those with beneficiary status. The fact that powerless agents experience it as escapable (they could consent, be compensated, or exclude their work) while beneficiaries experience it as necessary reveals that the necessity is socially contingent, not natural law. The constraint's extractiveness has increased over 6 years (0.35 → 0.58) not because technical requirements changed, but because model developers discovered that more data yields better models and regulatory ambiguity permitted unconsented accumulation. This temporal drift from coordination-heavy (early years, lower ε) to extraction-heavy (recent years, higher ε) is the signature of a snare that falsely claimed coordination origins.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_scale_problem,
    'Is individual consent even computationally or administratively feasible at training data scale (billions of works)?',
    'Pilot implementation of consent-based data curation for 1B+ word datasets; comparison of resulting model capability vs non-consent baselines; cost analysis of consent infrastructure',
    'If infeasible: training data concentration becomes partially mountain (structural limit). If feasible but costly: concentration remains tangled_rope/snare (unjustified extraction). If feasible and cost-neutral: concentration is pure snare (extractive without structural excuse).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_scale_problem, empirical, 'Whether individual consent mechanisms can scale to training data volumes').

omega_variable(
    copyright_fair_use_boundary,
    'Does training a machine learning model constitute ''fair use'' of copyrighted works under current or future legal standards?',
    'Outcome of major copyright litigation (Authors Guild v OpenAI, Andersen v Stability AI, Getty Images v Stability AI); legislative clarification of fair use for ML training; international legal harmonization',
    'If fair use confirmed: the legal suppression mechanism (uncertainty/arbitrage) is resolved in model developers'' favor. If rejected: model developers face retroactive liability and must negotiate licenses (shifts bargaining power to creators). If ambiguous but clarified toward licensing: concert performance model where usage requires explicit rights agreements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyright_fair_use_boundary, empirical, 'Legal status of training data use under copyright and fair use doctrine').

omega_variable(
    model_capability_data_necessity,
    'What is the minimum viable training data volume for language models achieving specific capability thresholds (reasoning, coding, etc.)? How tightly coupled is capability to total data volume vs data quality and diversity?',
    'Comparative analysis of scaling laws across different data compositions, quality standards, and deduplication strategies; testing of capability floors with synthetic/curated datasets vs web-scale data',
    'If capability tightly coupled to scale: concentration appears more immutable (mountain-ish). If capability achieved at smaller volumes with curation: concentration is extractive artifact (snare), not necessity. If quality matters more than volume: alternative business models (paid content licensing, synthetic data) become competitive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(model_capability_data_necessity, empirical, 'Relationship between training data volume, quality, and model capability').

omega_variable(
    creator_organized_coalition_threshold,
    'What fraction of content creators must organize and enforce data rights before model developers face significant economic pressure to change licensing behavior? What is the critical mass threshold for coalition power?',
    'Historical analysis of similar media industries (music licensing, stock photography); simulation of coalition strength vs model developer compliance incentives; measurement of costs imposed by litigation and regulatory pressure',
    'If threshold low (10-20%): coalition can shift the constraint from snare→tangled_rope. If threshold high (70%+): individual creators remain powerless despite organization. Threshold variation reflects whether collective action is sufficient or requires state intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_organized_coalition_threshold, empirical, 'Critical mass threshold for creator coalition power in data licensing').

omega_variable(
    data_subject_privacy_irreducibility,
    'Is the privacy violation inherent to training on personal data, or can privacy-preserving techniques (differential privacy, federated learning, data anonymization) reduce extractiveness to acceptable levels?',
    'Testing of privacy-preserving training approaches at scale; measurement of utility-privacy tradeoffs; empirical assessment of re-identification risk under realistic attack scenarios',
    'If privacy-preserving is sufficient: data subject snare becomes tangled_rope (some extraction but some benefit from model capability). If privacy cannot be sufficiently preserved: data subject snare remains unchanged. If privacy-preserving is feasible but expensive: concentration reflects cost-shifting (developers avoid expense by not implementing it) rather than technical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_subject_privacy_irreducibility, empirical, 'Whether privacy-preserving techniques can reduce data subject extraction to acceptable levels').

omega_variable(
    downstream_bias_inheritance,
    'How much downstream model bias is directly attributable to upstream training data concentration vs other sources (architecture, fine-tuning, deployment context)? Can downstream users effectively mitigate inherited bias without retraining on alternative data?',
    'Causal analysis of bias propagation from pre-training through fine-tuning to deployment; comparison of mitigation costs (post-hoc debiasing vs alternative training data); measurement of residual bias after mitigation',
    'If upstream data is dominant source: downstream users are genuinely constrained (tangled_rope justified). If downstream sources dominate: concentration has lower impact on downstream extraction (rope or low-extraction tangled_rope). If mitigation is cheap: downstream users have exit options (mobile), not constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_bias_inheritance, empirical, 'Relative contribution of training data concentration to downstream model bias and mitigation costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(training_data_concentration, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tdc_tr_t0, training_data_concentration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tdc_tr_t3, training_data_concentration, theater_ratio, 3, 0.38).
narrative_ontology:measurement(tdc_tr_t6, training_data_concentration, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(tdc_be_t0, training_data_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tdc_be_t3, training_data_concentration, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(tdc_be_t6, training_data_concentration, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(training_data_concentration, resource_allocation).
narrative_ontology:affects_constraint(training_data_concentration, model_bias_inheritance).
narrative_ontology:affects_constraint(training_data_concentration, copyright_ai_licensing).
narrative_ontology:affects_constraint(training_data_concentration, privacy_preservation_tension).

% DUAL FORMULATION NOTE:
% Training data concentration decomposes into three structurally distinct constraints: (1) the intellectual property extraction from content creators (snare at creator level), (2) the privacy violation for data subjects (snare at individual level), and (3) the technical coordination of data aggregation (genuinely rope-like). Each has different ε and different victim groups. The current story addresses the aggregate constraint; decomposition into three stories would reveal that ε varies by victim type (higher for creators, even higher for privacy subjects). The three are linked: all resolved by the same licensing/consent mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(training_data_concentration, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
